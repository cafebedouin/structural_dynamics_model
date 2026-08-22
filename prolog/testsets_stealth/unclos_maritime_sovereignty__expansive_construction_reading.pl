% ============================================================================
% CONSTRAINT STORY: unclos_maritime_sovereignty__expansive_construction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_maritime_sovereignty__expansive_construction_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: unclos_maritime_sovereignty__expansive_construction_reading
 *   human_readable: Expansive Construction Reading of Maritime Sovereignty Generation
 *   domain: international law/maritime governance/geopolitical strategy
 *
 * SUMMARY:
 *   A state dredges submerged reefs and low-tide elevations into artificial
 *   islands — runways, harbors, garrisons, administrative stations — and
 *   asserts that the constructed perimeter, continuously policed and
 *   administered, generates a territorial sea and associated jurisdiction.
 *   This file instantiates the expansive_construction_reading of the
 *   unclos_maritime_sovereignty kernel: the claim that effective occupation
 *   and administrative control of built features confer de facto maritime
 *   sovereignty. The sibling readings are separate constraints, not parts of
 *   this one: the strict_geographic_reading denies that construction ever
 *   alters legal status, and the hybrid_effective_control_reading grants only
 *   500-meter safety zones pending unchallenged maturation. All three stories
 *   share a single referent — the standing arrangement of construction-backed
 *   control — with reading-indexed epsilon over that fixed referent: the
 *   strict sibling authors near-total burden (the arrangement is a legal
 *   nullity enforced against everyone else), the hybrid sibling authors a
 *   split burden (a tolerated safety function plus a contested growth path),
 *   and this reading authors a high-but-finite burden (0.72) reflecting a
 *   real allocation function with sharply asymmetric incidence. KEY AGENTS
 *   (by structural relationship): island_constructing_states — agenda-setter
 *   and sole concentrated beneficiary (powerful/identity_locked), creates and
 *   polices the facts the title rests on; neighboring_claimant_states —
 *   primary target (organized/constrained), loses waters adjacent to their
 *   own coasts; freedom_of_navigation_states — systemic target
 *   (institutional/mobile), corridors converted to foreign territorial seas;
 *   traditional_fishing_communities — diffuse target (powerless/trapped),
 *   absorbs daily enforcement; small_capacity_claimant_states — excluded
 *   voice (moderate/constrained), cannot convert claims into constructed
 *   facts; unclos_dispute_tribunals — analytical observer
 *   (institutional/analytical).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__expansive_construction_reading, 0.72).
domain_priors:suppression_score(unclos_maritime_sovereignty__expansive_construction_reading, 0.66).
domain_priors:theater_ratio(unclos_maritime_sovereignty__expansive_construction_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__expansive_construction_reading, tangled_rope).
narrative_ontology:human_readable(unclos_maritime_sovereignty__expansive_construction_reading, "Expansive Construction Reading of Maritime Sovereignty Generation").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__expansive_construction_reading, "international law/maritime governance/geopolitical strategy").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__expansive_construction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__expansive_construction_reading, '2d1a0cb7-2bdc-46b7-85b4-05a44130ae0a').
narrative_ontology:cs_kernel_codification('2d1a0cb7-2bdc-46b7-85b4-05a44130ae0a', fixed_text).
narrative_ontology:cs_authority_grounding('2d1a0cb7-2bdc-46b7-85b4-05a44130ae0a', practice).
narrative_ontology:cs_interpretation_layer_present('2d1a0cb7-2bdc-46b7-85b4-05a44130ae0a').
narrative_ontology:cs_reading_relation('2d1a0cb7-2bdc-46b7-85b4-05a44130ae0a', unclos_maritime_sovereignty__strict_geographic_reading, forecloses).
narrative_ontology:cs_reading_relation('2d1a0cb7-2bdc-46b7-85b4-05a44130ae0a', unclos_maritime_sovereignty__hybrid_effective_control_reading, influences).
narrative_ontology:cs_axiom('2d1a0cb7-2bdc-46b7-85b4-05a44130ae0a', foundational, artificial_construction_confers_title).
narrative_ontology:cs_axiom_status(artificial_construction_confers_title, holdable).
narrative_ontology:cs_axiom_grounding('2d1a0cb7-2bdc-46b7-85b4-05a44130ae0a', artificial_construction_confers_title, conventional).
narrative_ontology:cs_axiom('2d1a0cb7-2bdc-46b7-85b4-05a44130ae0a', secondary, administrative_control_satisfies_occupation).
narrative_ontology:cs_axiom_status(administrative_control_satisfies_occupation, holdable).
narrative_ontology:cs_axiom_grounding('2d1a0cb7-2bdc-46b7-85b4-05a44130ae0a', administrative_control_satisfies_occupation, instrumental).
narrative_ontology:cs_reference_frame('2d1a0cb7-2bdc-46b7-85b4-05a44130ae0a', effective_occupation_title_regime).
narrative_ontology:cs_drift_state('2d1a0cb7-2bdc-46b7-85b4-05a44130ae0a', post_arbitral_award_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2d1a0cb7-2bdc-46b7-85b4-05a44130ae0a', '').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__expansive_construction_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__expansive_construction_reading, island_constructing_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__expansive_construction_reading, neighboring_claimant_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__expansive_construction_reading, freedom_of_navigation_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__expansive_construction_reading, traditional_fishing_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Dredges submerged reefs and low-tide elevations into artificial islands with runways, harbors, garrisons, and administrative stations, then declares and polices the surrounding waters as national territory. Provides the patrols, services, and governance presence that the title claim rests on. Retreat would mean abandoning billions in sunk construction and a sovereignty narrative woven into domestic politics, so withdrawal is not a live option from where this seat stands.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, island_constructing_states, agenda_setter,
    powerful, generational, identity_locked, regional).

% Hold overlapping claims to the same features and waters and lose access to fishing grounds and hydrocarbon prospects inside the zones the constructed perimeters generate. Respond with diplomatic protests, joint statements, occasional construction of their own, and litigation. They cannot move away from their own coastlines, and the adjudicatory route is slow, consent-dependent, and unenforceable against the constructing power.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, neighboring_claimant_states, payer,
    organized, generational, constrained, regional).

% Depend on unimpeded transit through the region for commerce and military mobility. Each generated territorial sea converts what was open corridor into foreign waters subject to innocent-passage rules and prior-notification demands. They run freedom-of-navigation operations to contest the claims and can shift assets globally, but cannot be present everywhere at once, and ceding the norm anywhere weakens it everywhere.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, freedom_of_navigation_states, payer,
    institutional, generational, mobile, global).

% Have worked the grounds around the features for generations. Now encounter patrol vessels, expulsions, licensing demands, and seasonal bans inside the generated zones. Gear, home ports, and knowledge are tied to specific banks and reefs; relocating to distant grounds means losing the fishery they know. They hold no seat in any negotiation and absorb the day-to-day enforcement directly.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, traditional_fishing_communities, payer,
    powerless, biographical, trapped, local).

% Hold paper claims to features and waters they cannot convert into constructed facts, lacking the dredging fleets, logistics chains, and air cover the practice rewards. Their objections register as protests that an effectiveness-based standard discounts, and the longer the practice runs, the more their claims depreciate against rivals' concrete. They would choose natural geography, equity principles, or negotiated allocation as the decision rule instead.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, small_capacity_claimant_states, excluded,
    moderate, generational, constrained, regional).

% Adjudicate feature-status and entitlement questions through Annex VII arbitration and ITLOS proceedings. Have ruled that artificial islands possess no territorial sea and that certain occupied features are rocks without exclusive economic zones. Possess no enforcement machinery of their own; their output operates through consent, reputational pressure, and uptake by other states.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, unclos_dispute_tribunals, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_maritime_sovereignty__expansive_construction_reading, island_constructing_states).
narrative_ontology:fixing_cost_class(unclos_maritime_sovereignty__expansive_construction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts an indeterminate multi-claimant vacuum into a possession rule: whoever constructs and administers a feature governs the waters around it, giving each disputed feature a single managing authority for safety of navigation, search-and-rescue, fisheries order, and incident response.
% TRANSFER_FUNCTION: Moves maritime jurisdiction — twelve-nautical-mile territorial seas and the regulatory and resource rights attached to them — from adjacent coastal states, open-access navigation users, and traditional fishers to whichever state performs large-scale construction and sustains continuous administration.
% ABSENT_VOICES: Traditional fishing communities and small-capacity claimant states have no seat in the practice-based title process: the standard rewards physical construction capacity, so dissent registers only as protests that acquiescence doctrine progressively discounts. Present at the table, they would insist that natural geography, equitable principles, or multilateral allocation decide entitlement instead of unilateral fait accompli.
% DISAPPEARANCE_RATIONALE: If the construction-generates-title rule vanished overnight, the generated territorial seas would collapse back to rock and low-tide-elevation status, exclusion zones around built features would dissolve, fishing fleets would re-enter grounds now patrolled as internal waters, and the claim geometry of the region would reorganize around natural high-tide geography and negotiated lines rather than dredged perimeters.
% FOUNDING_PROBLEM: Competing claims over submerged features and low-tide elevations left the surrounding waters in an authority vacuum — unsafe navigation, unmanaged fisheries, recurring standoffs — and this reading was built to attach sovereignty to whoever actually bears the costs of occupation and administration, so that control and responsibility fall on the same actor.
% FOUNDING_PROBLEM_CORROBORATION: Non-constructing claimant states corroborate that the authority-vacuum problem is real — they litigate it and propose counter-rules rather than denying it — and Annex VII tribunal proceedings treated feature status as a genuine question requiring answer even while rejecting the expansive solution. No source outside the constructing states attests that the problem has been solved; the dispute over the answer is itself evidence the problem persists.
narrative_ontology:disappearance_verdict(unclos_maritime_sovereignty__expansive_construction_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_maritime_sovereignty__expansive_construction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__expansive_construction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unclos_maritime_sovereignty__expansive_construction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_maritime_sovereignty__expansive_construction_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_maritime_sovereignty__expansive_construction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_maritime_sovereignty__expansive_construction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_maritime_sovereignty__expansive_construction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claimed type is tangled_rope because the arrangement possesses BOTH a genuine coordination function and asymmetric extraction through the same structure. The coordination residue is real: a determinate possession rule replaces case-by-case armed confrontation over authority vacuums, and each feature gets one managing authority for safety, search-and-rescue, and order. The extraction is equally real: eligibility for the rule's benefits is keyed to construction capacity that only great-power actors possess, so the allocation systematically moves jurisdiction from those who cannot build to those who can. Enforcement is constitutive, not incidental — coast guard patrols, vessel expulsions, radar and command infrastructure, and licensing regimes are what the claim consists of — hence requires_active_enforcement. Suppression (0.66) is authored as a raw structural property and is NOT scaled by power or scope; only extractiveness is scaled by directionality and scope in the engine's computation. Accessibility collapse sits mid-range (0.50): litigation, protest coalitions, and freedom-of-navigation operations remain available, but each year of unchallenged administration forecloses reversal further. Resistance is high (0.68): arbitration, coordinated diplomatic protest, naval assertion, and fragmented counter-construction. The temporal series run on one shared grid (points 0, 6, 12, 18, 24, 30) so every tracked metric is authored at every examined point; extractiveness climbs as construction scales, and theater rises after the arbitral setback as ceremonial administration (commissionings, resettlement announcements, lighthouse dedications, feature-naming) substitutes for a weakening legal justification. Coalition note: the victim set spans organized states and powerless fishers; a claimant-state coalition is structurally possible but fragmented by divergent bilateral interests and by each member's temptation to cut its own deal, which is precisely the collective-action weakness the possession rule exploits.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently and the divergence is the finding. From the constructing state's seat the arrangement is lawful development plus responsible administration — it built, it governs, it delivers services, and effectiveness has always been how title works; that seat will compute rope-flavored. From the neighboring claimant's seat the same structure is annexation by dredging — jurisdiction stripped from waters touching their own shores by an act they could never match; that seat will compute snare-flavored. From the freedom-of-navigation seat it is creeping enclosure of a global commons, one generated territorial sea at a time. The engine derives these per-seat classifications from the power, exit, and directionality data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations map directly onto the directionality gradient. island_constructing_states sit near the beneficiary pole (d approaching 0): the arrangement subsidizes them, they collect the generated jurisdiction, and their identity-lock deepens rather than dampens the benefit. traditional_fishing_communities sit nearest the full-target pole (d approaching 1): trapped exit, local scope, and zero offsetting benefit mean the enforcement falls on them undiluted. neighboring_claimant_states carry high d (constrained exit, adjacent geography, no escape from the generated zones). freedom_of_navigation_states carry high but damped d — they are targets of the norm, but their global mobility and institutional weight mean each individual zone costs them less than it costs a trapped fisher or an adjacent claimant. small_capacity_claimant_states carry high d with no benefit offset whatsoever. No directionality overrides are authored: the derivation chain from beneficiary/victim declarations plus exit options produces the correct qualitative ordering without correction, and the override surface is reserved for cases the structural data cannot distinguish.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — authority vacuum over contested submerged features — is still live, so this is not a mandatrophy case: the mandate has not outlived its function, and mandatrophy_resolved is deliberately not declared. The classification discipline cuts both ways here. Labeling the arrangement a pure snare would erase the genuine coordination function (single managing authority, dispute-resolution rule, safety services) that even hostile seats implicitly rely on when they negotiate incident protocols. Labeling it a pure rope would erase the capacity gatekeeping that makes the allocation permanently asymmetric. The receipt surface records where the gains actually concentrate: gain_flow names island_constructing_states, the one seat that demonstrably accrues the generated jurisdiction, and fixing_cost is prohibitive — physically removing the constructed features or collectively compelling withdrawal exceeds any single challenger's capacity, which is exactly why the arrangement persists against majority opposition. On the R5 mismatch wiring, founding_problem_status=live paired with disappearance_verdict=world_rearranges is the consistent cell: no zombie flag, no piton signature — the gains are concentrated, not diffuse, and the administrator is also the beneficiary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is the expansive_construction_reading of kernel unclos_maritime_sovereignty; how would instantiating a sibling reading change the structural classification?',
    'Adopting strict_geographic_reading zeroes the generated zones entirely — constructed features become legally null for title purposes, pushing the same conduct toward pure enforced appropriation with no allocation function. Adopting hybrid_effective_control_reading caps the immediate burden at 500-meter safety zones and relocates the contest to the maturation condition (challenge versus acquiescence over time). The disagreement is located in the title-generation premise itself: whether artificial construction can ever alter maritime legal status.',
    'Under the strict sibling the arrangement computes as a snare-shaped structure (coordination cover gone, victims intact); under the hybrid sibling the measured burden splits between a tolerated safety component and a contested growth component; the tangled_rope classification authored here holds only within the expansive reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Reading-contest structure of the maritime sovereignty kernel').

omega_variable(
    acquiescence_crystallization,
    'Will sustained absence of effective challenge convert de facto control over the generated waters into customary title, as the reading''s occupation logic predicts?',
    'Longitudinal analysis of state practice and opinio juris: whether protests persist or lapse, whether third states begin treating the zones as foreign territorial seas in charts, insurance pricing, shipping routing, and bilateral agreements.',
    'Crystallization legalizes the transfer, collapsing resistance and enforcement costs and drifting the arrangement toward settled allocation; failure keeps it perpetually contested with high permanent enforcement overhead and preserves the victims'' legal position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(acquiescence_crystallization, empirical, 'Whether de facto control ripens into customary title').

omega_variable(
    construction_capacity_gatekeeping,
    'Is the rule''s benefit structure inherently gated to states with great-power dredging, logistics, and air-cover capacity, or could the practice diffuse widely enough to become a neutral allocation rule?',
    'Cost and capability analysis of large-scale land reclamation; tracking the diffusion of dredging fleets and engineering capacity to mid-sized states and observing whether second-tier states begin generating comparable facts.',
    'If capacity diffuses, the asymmetry softens and the arrangement migrates toward ordinary coordination with symmetric eligibility; if gated, the asymmetric burden is structural and permanent, and the coordination story functions as cover for a capacity monopoly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(construction_capacity_gatekeeping, empirical, 'Whether benefit eligibility is capacity-gated').

omega_variable(
    ecological_externality_accounting,
    'Does the measured burden account for irreversible reef destruction and fisheries degradation borne by parties outside the claimant-state structure?',
    'Environmental accounting of reclamation damage — reef loss, sediment plumes, lagoon burial, fishery collapse — attributed to the construction program and mapped onto the human populations dependent on the affected ecosystems.',
    'Including ecological casualties raises the effective burden above the inter-state transfer alone and strengthens the reading that the arrangement''s total costs exceed its allocation benefits; excluding them understates what the structure takes and from whom.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecological_externality_accounting, empirical, 'Third-party ecological costs omitted from inter-state accounting').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__expansive_construction_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unclos_expansive_reading_tr_t0, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(unclos_expansive_reading_tr_t0, observed).
narrative_ontology:measurement(unclos_expansive_reading_tr_t6, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 6, 0.18).
narrative_ontology:measurement_basis(unclos_expansive_reading_tr_t6, observed).
narrative_ontology:measurement(unclos_expansive_reading_tr_t12, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement_basis(unclos_expansive_reading_tr_t12, observed).
narrative_ontology:measurement(unclos_expansive_reading_tr_t18, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 18, 0.28).
narrative_ontology:measurement_basis(unclos_expansive_reading_tr_t18, observed).
narrative_ontology:measurement(unclos_expansive_reading_tr_t24, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 24, 0.34).
narrative_ontology:measurement_basis(unclos_expansive_reading_tr_t24, observed).
narrative_ontology:measurement(unclos_expansive_reading_tr_t30, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement_basis(unclos_expansive_reading_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(unclos_expansive_reading_be_t0, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(unclos_expansive_reading_be_t0, observed).
narrative_ontology:measurement(unclos_expansive_reading_be_t6, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 6, 0.42).
narrative_ontology:measurement_basis(unclos_expansive_reading_be_t6, observed).
narrative_ontology:measurement(unclos_expansive_reading_be_t12, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 12, 0.52).
narrative_ontology:measurement_basis(unclos_expansive_reading_be_t12, observed).
narrative_ontology:measurement(unclos_expansive_reading_be_t18, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 18, 0.61).
narrative_ontology:measurement_basis(unclos_expansive_reading_be_t18, observed).
narrative_ontology:measurement(unclos_expansive_reading_be_t24, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 24, 0.68).
narrative_ontology:measurement_basis(unclos_expansive_reading_be_t24, observed).
narrative_ontology:measurement(unclos_expansive_reading_be_t30, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 30, 0.72).
narrative_ontology:measurement_basis(unclos_expansive_reading_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(unclos_expansive_reading_su_t0, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(unclos_expansive_reading_su_t0, observed).
narrative_ontology:measurement(unclos_expansive_reading_su_t6, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 6, 0.47).
narrative_ontology:measurement_basis(unclos_expansive_reading_su_t6, observed).
narrative_ontology:measurement(unclos_expansive_reading_su_t12, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 12, 0.54).
narrative_ontology:measurement_basis(unclos_expansive_reading_su_t12, observed).
narrative_ontology:measurement(unclos_expansive_reading_su_t18, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 18, 0.6).
narrative_ontology:measurement_basis(unclos_expansive_reading_su_t18, observed).
narrative_ontology:measurement(unclos_expansive_reading_su_t24, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 24, 0.64).
narrative_ontology:measurement_basis(unclos_expansive_reading_su_t24, observed).
narrative_ontology:measurement(unclos_expansive_reading_su_t30, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 30, 0.66).
narrative_ontology:measurement_basis(unclos_expansive_reading_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__expansive_construction_reading, resource_allocation).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__expansive_construction_reading, unclos_maritime_sovereignty__strict_geographic_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__expansive_construction_reading, unclos_maritime_sovereignty__hybrid_effective_control_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'maritime sovereignty under UNCLOS' decomposes into three structurally distinct constraints — one per reading of the kernel — because measuring the standing arrangement through different title-generation premises yields different, internally stable epsilon values. This expansive file links to both siblings via affects_constraints; the upstream doctrinal tradition (occupation effectiveness) feeds all three, and each sibling story should carry reciprocal edges documenting the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
