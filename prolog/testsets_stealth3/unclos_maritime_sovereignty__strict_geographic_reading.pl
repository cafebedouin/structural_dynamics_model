% ============================================================================
% CONSTRAINT STORY: unclos_maritime_sovereignty__strict_geographic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
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
 *   constraint_id: unclos_maritime_sovereignty__strict_geographic_reading
 *   human_readable: UNCLOS Natural-Formation Rule for Maritime Entitlement Generation (Strict Geographic Reading)
 *   domain: international law / maritime governance / geopolitical strategy
 *
 * SUMMARY:
 *   The strict geographic reading holds that maritime entitlement flows only
 *   from features nature placed above water at high tide: artificial islands
 *   are installations, not territory, and construction cannot upgrade a rock
 *   or a low-tide elevation into an EEZ-generating island. This is one
 *   reading of the UNCLOS maritime-sovereignty kernel, operationalized by the
 *   2016 South China Sea arbitration (Philippines v. China), which held that
 *   low-tide elevations generate no zones and that dredging cannot change a
 *   feature's status. The claim/metric gap is deliberate and load-bearing
 *   here: the constraint is CLAIMED as tangled_rope — a genuine coordination
 *   standard wrapped around an asymmetric allocation — while the authored
 *   metrics describe its actual operation (substantial asymmetric stakes,
 *   high active enforcement, moderate theater, live resistance from the
 *   primary target). The engine measures that divergence per seat; the claim
 *   does not reconcile the metrics. Epsilon's referent is the standing
 *   arrangement under contest — the natural-formation entitlement standard as
 *   authoritatively applied — assessed by this reading's own lights; the
 *   readings this file rejects are separate constraints, not parameters of
 *   this one.
 *
 * KEY AGENTS:
 *   - law_of_sea_tribunals: Agenda setter (institutional/constrained) — administers and authoritatively interprets the rule; issued the 2016 award
 *   - naval_powers: Primary beneficiary (powerful/mobile) — collect transit freedom and anti-enclosure precedent; supply the enforcement weight
 *   - non_claimant_maritime_states: Beneficiary (organized/mobile) — collect a stable entitlement regime at near-zero cost
 *   - natural_feature_claimant_states: Beneficiary (moderate/constrained) — vindicated EEZ claims they cannot enforce alone
 *   - traditional_fishing_communities: Beneficiary (powerless/constrained) — restored access in law, intermittently in fact
 *   - expansionist_coastal_states: Primary target (powerful/identity_locked) — construction investments yield no sovereignty; exit fused with national identity
 *   - small_island_states: Excluded (organized/trapped) — sea-level rise makes them the reading's prospective victims; not in the room
 *   - law_of_sea_scholars: Analytical observer (analytical/analytical) — measure the gap between award text and archipelago concrete
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__strict_geographic_reading, 0.55).
domain_priors:suppression_score(unclos_maritime_sovereignty__strict_geographic_reading, 0.72).
domain_priors:theater_ratio(unclos_maritime_sovereignty__strict_geographic_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__strict_geographic_reading, tangled_rope).
narrative_ontology:human_readable(unclos_maritime_sovereignty__strict_geographic_reading, "UNCLOS Natural-Formation Rule for Maritime Entitlement Generation (Strict Geographic Reading)").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__strict_geographic_reading, "international law / maritime governance / geopolitical strategy").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__strict_geographic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__strict_geographic_reading, '9c2d81b9-37a7-4dbb-a707-ebadb4b26e7a').
narrative_ontology:cs_kernel_codification('9c2d81b9-37a7-4dbb-a707-ebadb4b26e7a', fixed_text).
narrative_ontology:cs_authority_grounding('9c2d81b9-37a7-4dbb-a707-ebadb4b26e7a', lineage).
narrative_ontology:cs_interpretation_layer_present('9c2d81b9-37a7-4dbb-a707-ebadb4b26e7a').
narrative_ontology:cs_reading_relation('9c2d81b9-37a7-4dbb-a707-ebadb4b26e7a', unclos_maritime_sovereignty__expansive_construction_reading, forecloses).
narrative_ontology:cs_reading_relation('9c2d81b9-37a7-4dbb-a707-ebadb4b26e7a', unclos_maritime_sovereignty__hybrid_effective_control_reading, forecloses).
narrative_ontology:cs_axiom('9c2d81b9-37a7-4dbb-a707-ebadb4b26e7a', foundational, artificial_construction_cannot_confer_maritime_entitlement).
narrative_ontology:cs_axiom_status(artificial_construction_cannot_confer_maritime_entitlement, holdable).
narrative_ontology:cs_axiom_grounding('9c2d81b9-37a7-4dbb-a707-ebadb4b26e7a', artificial_construction_cannot_confer_maritime_entitlement, deontological).
narrative_ontology:cs_axiom('9c2d81b9-37a7-4dbb-a707-ebadb4b26e7a', foundational, natural_formation_prerequisite_for_island_status).
narrative_ontology:cs_axiom_status(natural_formation_prerequisite_for_island_status, holdable).
narrative_ontology:cs_axiom_grounding('9c2d81b9-37a7-4dbb-a707-ebadb4b26e7a', natural_formation_prerequisite_for_island_status, conventional).
narrative_ontology:cs_reference_frame('9c2d81b9-37a7-4dbb-a707-ebadb4b26e7a', unclos_natural_geography_baseline).
narrative_ontology:cs_drift_state('9c2d81b9-37a7-4dbb-a707-ebadb4b26e7a', post_award_defiance_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9c2d81b9-37a7-4dbb-a707-ebadb4b26e7a', '').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__strict_geographic_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, naval_powers).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, non_claimant_maritime_states).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, natural_feature_claimant_states).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, traditional_fishing_communities).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__strict_geographic_reading, expansionist_coastal_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, law_of_sea_tribunals).
narrative_ontology:constraint_vindicates(unclos_maritime_sovereignty__strict_geographic_reading, unclos_artificial_island_non_status_doctrine).
narrative_ontology:constraint_vindicates(unclos_maritime_sovereignty__strict_geographic_reading, land_dominates_the_sea_principle).
narrative_ontology:constraint_vindicates(unclos_maritime_sovereignty__strict_geographic_reading, low_tide_elevation_no_zone_rule).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Annex VII arbitral tribunals and ITLOS chambers administer the treaty's dispute-settlement machinery and authoritatively interpret the natural-formation rule; the 2016 South China Sea award is the rule's operative enforcement event, holding that low-tide elevations generate no maritime zones and that dredging cannot upgrade a feature's status. They command no ships and collect no revenues; their rulings bind only insofar as states comply, and their reach is bounded by consent-based jurisdiction.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, law_of_sea_tribunals, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(unclos_maritime_sovereignty__strict_geographic_reading, law_of_sea_tribunals, beneficiary).

% Operate global navies that transit exclusive-economic-zone and contested waters under freedom-of-navigation doctrine. The natural-formation rule keeps artificial-island outposts from generating territorial seas or EEZs that would wall off sea lanes, so their transit and overflight rights rest on geography no rival can manufacture. They supply the enforcement weight — freedom-of-navigation sail-throughs, carrier presence, coalition statements — that keeps the rule operative against defying states, at the cost of recurring operational tempo.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, naval_powers, beneficiary,
    powerful, generational, mobile, global).

% States outside the immediate dispute — shipping nations, trading economies, G7 and EU members — rely on the precedent that sovereignty cannot be poured into existence. They gain a stable, predictable entitlement regime without spending on reclamation or enforcement; some hedge, affirming the rule diplomatically while avoiding operational commitments. Exit is cheap for them: the rule costs them little either way, which is why their affirmation is easy and their abandonment would be quiet.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, non_claimant_maritime_states, beneficiary,
    organized, generational, mobile, global).

% Philippines, Vietnam, Malaysia, Indonesia and neighbors hold claims that run through genuinely natural islands and coastlines. The strict reading vests their EEZs and continental shelves against construction-based rivals — the 2016 award vindicated the Philippine position on Scarborough Shoal and the Spratlys — but they cannot enforce it alone against a great-power non-complier, so they hedge between legal vindication and accommodation, and geography blocks their exit from the dispute.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, natural_feature_claimant_states, beneficiary,
    moderate, generational, constrained, regional).

% Filipino, Vietnamese and other coastal fishers lost access to traditional grounds when manufactured claims and coast-guard blockades closed them; the award's invalidation of the nine-dash historic-rights line restored their access in law. They capture little of the rule's value directly — enforcement at Scarborough remains intermittent — and they bear the immediate risks of any confrontation their beneficiaries' navies provoke.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, traditional_fishing_communities, beneficiary,
    powerless, biographical, constrained, regional).

% A great-power claimant has invested billions dredging and paving reefs into airstrips, harbors and garrisons across the Spratlys. Under the strict reading none of it moves the legal needle: the features remain rocks and low-tide elevations generating no EEZ, and the award stripped the historic-rights overlay as well. The claim is fused with the state's nationalist narrative of restoration and rejuvenation, so abandoning it is domestically unthinkable; the live paths are defiance (current practice) or a negotiated face-saving arrangement that still concedes the legal point.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, expansionist_coastal_states, payer,
    powerful, generational, identity_locked, regional).

% Pacific and Caribbean micro-states whose entire maritime entitlement rests on features that sea-level rise may erode below high tide. They are not parties to the South China Sea contest where this reading is enforced, yet the precedent — legal status fixed by natural formation at a moment in time — is exactly what would drown their zones. They push the opposite rule (maritime zones frozen once established) through the ILC and the Pacific Islands Forum, and are not in the room where the strict reading is operationalized.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, small_island_states, excluded,
    organized, civilizational, trapped, global).

% International-law academics and think-tank analysts map the reading's doctrinal basis, its drafting history, and its drift under state practice. They collect nothing and pay nothing; their seat is the analytical vantage from which the divergence between the award's text and the archipelago's concrete is measured.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, law_of_sea_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_maritime_sovereignty__strict_geographic_reading, diffuse).
narrative_ontology:fixing_cost_class(unclos_maritime_sovereignty__strict_geographic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides an ex ante, objectively verifiable standard for what generates maritime jurisdiction: entitlement attaches to natural geography (land dominates the sea), so states can delimit zones, plan shipping and resource development, and resolve disputes without trusting rivals' assertions or racing each other's dredgers. Without it, every coastal state could manufacture entitlement by construction, converting the commons into a reclamation arms race.
% TRANSFER_FUNCTION: Moves legal position and strategic space rather than money or labor: it allocates maritime entitlement — fisheries, hydrocarbons, seabed jurisdiction, transit control — away from states whose claims depend on artificial construction and toward states holding natural features and toward the open commons (naval transit freedom). The 2016 award alone re-vested potential claim over most South China Sea features from the constructing claimant to the natural-feature claimants and the international regime.
% ABSENT_VOICES: Small island developing states would object that a rigid natural-formation rule, generalized, drowns their own entitlements under sea-level rise; they advocate frozen maritime zones and are absent from the South China Sea forum where this reading is enforced. The original UNCLOS drafters' intent — to stop micro-feature imperialism — is historically documented, but the states now bearing the rule's costs were not represented in the coalition that operationalized it: the award's bench and the freedom-of-navigation coalition speak for the beneficiaries.
% DISAPPEARANCE_RATIONALE: If the natural-formation rule ceased to bind overnight, the expansive reading fills the vacuum: the constructing claimant's finished islands would generate claimed territorial seas and EEZs by occupation, other coastal states would resume reclamation to keep pace, sea-lane regimes would fragment into contested zones, and the 2016 award's framework — on which claimant-state EEZ claims, freedom-of-navigation legal positions, and regional resource arrangements rest — would lose its foundation. The maritime order rearranges around whoever builds fastest.
% FOUNDING_PROBLEM: The EEZ era's rock problem: decolonization and the 200-mile zone created the prospect of vast ocean entitlements hanging off uninhabitable specks, and later of sovereignty manufactured by dredging. UNCLOS's drafters wrote Articles 13, 60(8) and 121(3) to confine entitlement to features that nature put above water and that can sustain life, so that micro-features and artificial islands could not generate 200-mile zones.
% FOUNDING_PROBLEM_CORROBORATION: The 2016 tribunal corroborates the founding problem from outside the beneficiary set, resting on the treaty's object and purpose and on comparative state practice (the Rockall episode, ICJ jurisprudence) rather than on the parties' interests; the UNCLOS III negotiating record documents the drafters' intent independently of any current claimant. The primary target state rejects this attestation outright, which is itself signal: the problem the rule solved is the problem it still poses to construction-based strategy. No source outside the beneficiary set attests the expansive alternative as the founding intent.
narrative_ontology:disappearance_verdict(unclos_maritime_sovereignty__strict_geographic_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_maritime_sovereignty__strict_geographic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__strict_geographic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unclos_maritime_sovereignty__strict_geographic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_maritime_sovereignty__strict_geographic_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is 0.55: the asymmetric stakes are enormous (the award re-vested claim over most Spratly features and invalidated a historic-rights overlay), but the rule's core is a verifiable coordination standard, and the target's loss is non-grant relative to a rival reading rather than transfer of owned value. Suppression (0.72) is authored as a raw structural property — the engine, not the author, scales extractiveness by directionality and scope; suppression itself is unscaled. It is high because the rule's persistence against its primary target depends on active machinery: compulsory arbitration, freedom-of-navigation operations, coalition communiques — the suppression_requirement series (0.15 to 0.72) tracks that enforcement machinery being built, which is why it is authored on the shared grid. Theater (0.38) reflects that enforcement against a non-complier is partly ritual — sail-throughs assert rights more than restore them, and annual affirmations restate what no one disputes — while the rule operates genuinely across the oceans where it is uncontested. Accessibility collapse (0.45): understanding the rule does not close the expansive alternative; construction proceeds and the rival reading persists as a live political position — the rule prices the alternative rather than eliminating it. Resistance (0.75): open repudiation by a permanent Security Council member, construction in defiance, pressure on claimant governments, consensus-blocking inside ASEAN. The extractiveness rise across the interval tracks the dispute's hardening and the enforcement build-up, not layered rent-seeking. Coalition note: the framework's coalition question targets powerless victims; here the payer is itself a great power, so the coalition risk runs the other way — the target plus sympathetic states could shift custom, which is the maturation omega.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the tribunal seat the arrangement is the legal order working: text, jurisdiction, reasoned award. From the naval seat it is the freedom-of-navigation regime's load-bearing wall — geography no rival can manufacture. From the non-claimant seat it is a free good: stability without enforcement cost. From the natural-feature claimant seat it is vindication on paper and hostage-taking in fact — the award they won is the award that is not executed. From the expansionist seat the same structure is containment by cartography: a rule that freezes a hostile status quo and denies what effective occupation yielded in every earlier era of the law. The engine derives these per-seat types from power, exit, and role; the divergence between them is the measurement, not a defect to be averaged away.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations map to directionality as follows. Naval powers (beneficiary, mobile, global): the rule subsidizes them — d sits near the beneficiary end, since their transit rights cost them nothing beyond the tempo of asserting them. Non-claimant maritime states (beneficiary, mobile): similar, cheaper still. Natural-feature claimants (beneficiary, constrained): benefits are real but partially unrealized (an unexecuted award), and geography blocks exit — d low but not minimal. Fishing communities (beneficiary, powerless): low d with negligible capture. Expansionist coastal states (payer, identity_locked, regional): d near the full-target end — they bear the rule's entire asymmetric cost and cannot exit without abandoning a claim fused with national identity; identity-lock sits them nearer full target than a merely mobile target would sit. One override: the tribunals (institutional) would derive toward subsidy from their secondary beneficiary role (they collect authority when the reading prevails), but their true position is near-symmetric administration — authority gains offset by the legitimacy cost of unexecuted rulings — hence d 0.45.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy risk runs both ways. Read as pure rope, the rule's asymmetric allocation and enforcement dependency disappear — a neutral standard does not need carrier strike groups to hold. Read as pure snare, the genuine coordination function disappears — without a natural-status standard, a reclamation race encloses the commons against every state without dredging capacity, including eventually the expansionist itself, and the award's reasoning becomes unreadable as anything but power. Tangled rope holds both: real coordination (an ex ante, verifiable entitlement standard) wrapped around real asymmetry (entitlement flows to holders of natural features; construction-dependent strategy is sterilized), held by active enforcement. The founding problem — micro-feature and manufactured sovereignty in the EEZ era — is live, so no mandatrophy declaration. One caution the receipt surface raises: gain_flow is diffuse and fixing is prohibitive, the static profile of a piton. The temporal series answers the caution: enforcement intensity rose monotonically across the interval and the function is live wherever the rule is uncontested; the theater is concentrated in the contested theater rather than constitutive of the rule.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_resolution,
    'This constraint is the strict_geographic_reading of the kernel unclos_maritime_sovereignty; would the expansive_construction_reading or the hybrid_effective_control_reading better capture the treaty''s object and state practice — and where exactly is the disagreement located (whether construction can ever generate status)?',
    'Accumulated state practice, further annex VII or ITLOS jurisprudence, and any future consent-based adjudication involving the constructing claimant; a regional framework settlement would resolve it politically.',
    'If the expansive reading prevails, this constraint''s victim and beneficiary sets invert (construction states gain zones; naval powers face enclosure) and its classification migrates toward snare from the naval seat; if the hybrid reading prevails, this reading''s categorical rule degrades into a transitional position with scaffold-like dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_resolution, conceptual, 'Which reading of the maritime-sovereignty kernel the legal order settles on.').

omega_variable(
    unchallenged_control_maturation_risk,
    'Does prolonged unchallenged occupation of the constructed features erode the strict rule into the hybrid reading by accretion — the exact mechanism the hybrid sibling names?',
    'Track recognition behavior over 10-20 years: whether third states begin treating the constructed features as zone-generating (opinio juris shift) or continue citing the award; whether any future tribunal confronts the fait accompli.',
    'If maturation occurs, this reading''s persistence was transitional rather than steady-state and its enforcement was a delaying action; the corpus should expect reclassification pressure toward scaffold dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unchallenged_control_maturation_risk, empirical, 'Whether defiance plus time converts legal inertness into de facto status.').

omega_variable(
    sealevel_rise_scope_ambiguity,
    'Does the natural-formation criterion apply against features lost to climate-driven sea-level rise — that is, does a natural island that erodes below high tide lose its zone under this reading?',
    'ILC conclusions on sea-level rise, tribunal treatment of baselines and receding features, and uptake of the Pacific Islands Forum maritime-zone freeze declaration in state practice.',
    'Strict temporal application would extend the victim set to small island states and receding-coast states, sharply raising measured extraction and converting the rule from anti-expansionist to climate-punitive; a freeze exception would confine the reading''s costs to construction-based claims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sealevel_rise_scope_ambiguity, conceptual, 'Whether the rule''s natural-status logic is temporal or frozen at establishment.').

omega_variable(
    habitation_criterion_boundary,
    'This story''s rule states the natural-formation and high-tide criteria; Article 121(3) also withholds EEZs from rocks that cannot sustain human habitation or economic life of their own. Is the rock clause part of this constraint or a separate one?',
    'Decompose per the epsilon-invariance principle: author a sibling story for the habitation criterion — it has a different victim set (Japan''s Okinotorishima and marginal natural islands) and a different doctrinal basis — and link via network edges.',
    'Including the rock clause widens the victim set beyond construction-dependent states and raises epsilon; excluding it keeps this story''s epsilon referent cleanly on the artificial-construction question.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(habitation_criterion_boundary, conceptual, 'Scope boundary between the natural-formation rule and the rock clause.').

omega_variable(
    victim_baseline_symmetry,
    'Is the rule genuinely asymmetric (naval and non-claimant states gain at the construction-dependent claimant''s expense) or symmetric (all states gain from a stable standard, and the ''victim'' merely fails to receive a grant it never owned)?',
    'Counterfactual zone-allocation analysis under each reading plus enforcement-cost accounting: who pays for freedom-of-navigation operations and adjudication, and who would pay for a reclamation race.',
    'If the symmetric reading holds, the constraint moves toward rope (pure coordination); if asymmetric, tangled_rope holds and the payer seat''s high directionality stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_baseline_symmetry, conceptual, 'Whether the declared victim bears extraction or only non-grant.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__strict_geographic_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t0, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(uncl_tr_t0, observed).
narrative_ontology:measurement(uncl_tr_t5, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement_basis(uncl_tr_t5, observed).
narrative_ontology:measurement(uncl_tr_t10, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement_basis(uncl_tr_t10, observed).
narrative_ontology:measurement(uncl_tr_t15, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement_basis(uncl_tr_t15, observed).
narrative_ontology:measurement(uncl_tr_t18, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 18, 0.28).
narrative_ontology:measurement_basis(uncl_tr_t18, observed).
narrative_ontology:measurement(uncl_tr_t22, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 22, 0.35).
narrative_ontology:measurement_basis(uncl_tr_t22, observed).
narrative_ontology:measurement(uncl_tr_t26, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 26, 0.37).
narrative_ontology:measurement_basis(uncl_tr_t26, observed).
narrative_ontology:measurement(uncl_tr_t30, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement_basis(uncl_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(uncl_be_t0, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement_basis(uncl_be_t0, observed).
narrative_ontology:measurement(uncl_be_t5, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 5, 0.28).
narrative_ontology:measurement_basis(uncl_be_t5, observed).
narrative_ontology:measurement(uncl_be_t10, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement_basis(uncl_be_t10, observed).
narrative_ontology:measurement(uncl_be_t15, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 15, 0.4).
narrative_ontology:measurement_basis(uncl_be_t15, observed).
narrative_ontology:measurement(uncl_be_t18, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 18, 0.48).
narrative_ontology:measurement_basis(uncl_be_t18, observed).
narrative_ontology:measurement(uncl_be_t22, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 22, 0.58).
narrative_ontology:measurement_basis(uncl_be_t22, observed).
narrative_ontology:measurement(uncl_be_t26, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 26, 0.56).
narrative_ontology:measurement_basis(uncl_be_t26, observed).
narrative_ontology:measurement(uncl_be_t30, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement_basis(uncl_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t0, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(uncl_su_t0, observed).
narrative_ontology:measurement(uncl_su_t5, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 5, 0.18).
narrative_ontology:measurement_basis(uncl_su_t5, observed).
narrative_ontology:measurement(uncl_su_t10, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 10, 0.22).
narrative_ontology:measurement_basis(uncl_su_t10, observed).
narrative_ontology:measurement(uncl_su_t15, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 15, 0.35).
narrative_ontology:measurement_basis(uncl_su_t15, observed).
narrative_ontology:measurement(uncl_su_t18, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 18, 0.55).
narrative_ontology:measurement_basis(uncl_su_t18, observed).
narrative_ontology:measurement(uncl_su_t22, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 22, 0.68).
narrative_ontology:measurement_basis(uncl_su_t22, observed).
narrative_ontology:measurement(uncl_su_t26, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 26, 0.7).
narrative_ontology:measurement_basis(uncl_su_t26, observed).
narrative_ontology:measurement(uncl_su_t30, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(uncl_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__strict_geographic_reading, resource_allocation).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, unclos_maritime_sovereignty__expansive_construction_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, unclos_maritime_sovereignty__hybrid_effective_control_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, freedom_of_navigation_regime).

% DUAL FORMULATION NOTE:
% The colloquial label 'maritime sovereignty under UNCLOS' covers at least three structurally distinct claims about what generates entitlement. This file instantiates only the strict geographic reading: natural formation plus high tide, construction legally inert. Its epsilon reflects that reading's own beneficiary/victim structure (naval and non-claimant states benefit; construction-dependent claimants bear); the expansive reading's epsilon would invert the structure, and the hybrid reading's would split it. The siblings are separate stories linked here; epsilon is stable within this file because the referent — the natural-formation entitlement standard as authoritatively applied — does not shift with which observable a reviewer picks.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unclos_maritime_sovereignty__strict_geographic_reading, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
