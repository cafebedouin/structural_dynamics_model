% ============================================================================
% CONSTRAINT STORY: zionist_legitimacy_basis__settler_colonial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zionist_legitimacy_basis__settler_colonial_reading, []).

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
 *   constraint_id: zionist_legitimacy_basis__settler_colonial_reading
 *   human_readable: Zionist Legitimacy Basis — Settler-Colonial Reading: Ethno-State Constituted Through Indigenous Displacement
 *   domain: political history/nationalism/settler-colonial studies
 *
 * SUMMARY:
 *   This story instantiates ONE reading — the settler-colonial reading — of
 *   the contested kernel zionist_legitimacy_basis. The kernel label covers
 *   three structurally distinct claims (national liberation, religious
 *   restoration, settler colonial), and per the ε-invariance principle each
 *   is authored as its own constraint story with its own ε over the SAME
 *   referent: the standing arrangement, i.e. the Israeli state structure as
 *   founded through the 1948 displacement and maintained through occupation,
 *   settlement administration, and differential citizenship law. This story
 *   does not hedge across readings; the siblings are separate files linked by
 *   network edges. From this reading's seat, the standing arrangement is a
 *   structure that genuinely coordinates the Jewish national collective
 *   (membership, security, self-governance) while constitutively transferring
 *   land, sovereignty, and return rights away from the Palestinian indigenous
 *   population — the coordination and the transfer run through the same
 *   structure and require active enforcement (occupation administration,
 *   permit regimes, land law) to hold. Claim/metric independence:
 *   claimed_type (tangled_rope) is this reading's structural judgment; the
 *   metrics describe the arrangement's operation as this reading assesses it;
 *   the engine computes per-seat classifications independently of both. KEY
 *   AGENTS (by structural relationship): israeli_state_apparatus — agenda
 *   setter (institutional/identity_locked), administers and enforces;
 *   israeli_jewish_settler_society — primary beneficiary
 *   (powerful/identity_locked); diaspora_jewish_institutions — external
 *   beneficiary (organized/arbitrage); united_states_government — external
 *   beneficiary and enforcement shield (institutional/arbitrage);
 *   palestinian_displaced_refugees — primary target (powerless/trapped, also
 *   excluded from the conversation); west_bank_palestinians — primary target
 *   (powerless/trapped); palestinian_citizens_of_israel — secondary target
 *   (moderate/constrained); postcolonial_scholars — analytical observer, sees
 *   the full structure; international_legal_bodies — inter-institutional
 *   observer whose rulings move external legitimacy costs.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__settler_colonial_reading, 0.86).
domain_priors:suppression_score(zionist_legitimacy_basis__settler_colonial_reading, 0.85).
domain_priors:theater_ratio(zionist_legitimacy_basis__settler_colonial_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, extractiveness, 0.86).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__settler_colonial_reading, tangled_rope).
narrative_ontology:human_readable(zionist_legitimacy_basis__settler_colonial_reading, "Zionist Legitimacy Basis — Settler-Colonial Reading: Ethno-State Constituted Through Indigenous Displacement").
narrative_ontology:topic_domain(zionist_legitimacy_basis__settler_colonial_reading, "political history/nationalism/settler-colonial studies").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__settler_colonial_reading, '059940ed-b174-415c-8ea2-c081db93074e').
narrative_ontology:cs_kernel_codification('059940ed-b174-415c-8ea2-c081db93074e', distributed).
narrative_ontology:cs_authority_grounding('059940ed-b174-415c-8ea2-c081db93074e', expertise).
narrative_ontology:cs_interpretation_layer_present('059940ed-b174-415c-8ea2-c081db93074e').
narrative_ontology:cs_reading_relation('059940ed-b174-415c-8ea2-c081db93074e', zionist_legitimacy_basis__national_liberation_reading, influences).
narrative_ontology:cs_reading_relation('059940ed-b174-415c-8ea2-c081db93074e', zionist_legitimacy_basis__religious_restoration_reading, coexists_with).
narrative_ontology:cs_axiom('059940ed-b174-415c-8ea2-c081db93074e', foundational, colonial_structure_determines_legitimacy).
narrative_ontology:cs_axiom_status(colonial_structure_determines_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('059940ed-b174-415c-8ea2-c081db93074e', colonial_structure_determines_legitimacy, empirically_contingent).
narrative_ontology:cs_axiom('059940ed-b174-415c-8ea2-c081db93074e', foundational, displacement_constitutive_not_incidental).
narrative_ontology:cs_axiom_status(displacement_constitutive_not_incidental, holdable).
narrative_ontology:cs_axiom_grounding('059940ed-b174-415c-8ea2-c081db93074e', displacement_constitutive_not_incidental, empirically_contingent).
narrative_ontology:cs_reference_frame('059940ed-b174-415c-8ea2-c081db93074e', colonial_structure_legitimacy_basis).
narrative_ontology:cs_drift_state('059940ed-b174-415c-8ea2-c081db93074e', post_icj_advisory_opinion_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('059940ed-b174-415c-8ea2-c081db93074e', '').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__settler_colonial_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, israeli_jewish_settler_society).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, diaspora_jewish_institutions).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, united_states_government).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, palestinian_displaced_refugees).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, west_bank_palestinians).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, palestinian_citizens_of_israel).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the structure end to end: the land authority that holds and reallocates expropriated property, the settlement administration in the occupied territories, military rule and permit regimes, citizenship and return law. Sets the rules determining who may live, build, and return where. Its institutional self-concept — founding narrative, security doctrine, coalition politics — is fused with the structure it administers; revising the founding terms would mean renegotiating the state's own identity, not merely its policies.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, israeli_state_apparatus, agenda_setter,
    institutional, generational, identity_locked, regional).

% The collective that receives the arrangement's benefits: land allocation, citizenship priority, a security guarantee, and a national home constituted in basic law. Its national identity is bound to the state's founding story, so exit is not a realistic individual option — leaving means leaving the collective. It also carries real costs of the arrangement it benefits from: conscription, war casualties, and a mounting international legitimacy price, which places it slightly above the pure beneficiary pole.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, israeli_jewish_settler_society, beneficiary,
    powerful, generational, identity_locked, national).

% Federations, organizations, and religious bodies outside the territory that draw identity, fundraising energy, and political alignment from the state's existence. They bear none of its direct costs — no conscription, no exposure to the conflict's violence — and can modulate or suspend their association when costs spike, making their exit the cheapest of any beneficiary seat.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, diaspora_jewish_institutions, beneficiary,
    organized, generational, arbitrage, global).

% Provides military aid, diplomatic protection in international bodies, and vetoes that shield the arrangement from binding external enforcement. In exchange it receives strategic alignment in the region and domestic political alignment. Its commitment is revisable by its own political cycles; it is not structurally bound into the arrangement's survival.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, united_states_government, beneficiary,
    institutional, generational, arbitrage, global).

% Displaced in the 1948 war and again in 1967, now numbering millions across host countries and a global diaspora. Refugee status is inherited; most hold no citizenship of the state on whose land they hold claims, and several host states deny them full membership as well. Return is administered by the very authorities that displaced them. They were not parties to any of the decisions that determined their disposition and hold no seat in the conversations that maintain it.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, palestinian_displaced_refugees, payer,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__settler_colonial_reading, palestinian_displaced_refugees, excluded).

% Live under military administration: movement permits, checkpoint networks, settlement expropriation of land and water, house demolitions, and detention without the protections of either the governing state's civilian law or a sovereign of their own. They do not vote in the state that sets the terms of their lives. Exit means internal displacement or emigration; the territory itself is what is being transferred.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, west_bank_palestinians, payer,
    powerless, biographical, trapped, local).

% Hold citizenship and vote, but the state's basic law vests self-determination uniquely in the Jewish collective, and land, planning, budget, and language policy run through institutions that prioritize that collective. Their exit is emigration — individually possible, collectively a loss of presence on the land. Their formal status gives them standing that kin under occupation lack, at the price of membership in a polity constituted against their national claim.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, palestinian_citizens_of_israel, payer,
    moderate, biographical, constrained, national).

% Courts, UN bodies, and treaty mechanisms that adjudicate the arrangement's legality: advisory opinions on the occupation's lawfulness, investigations, resolutions. They command no army; their instrument is legitimacy itself, which makes them the institutional seat whose rulings most directly move the arrangement's external costs.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, international_legal_bodies, observer,
    institutional, generational, analytical, global).

% The historiographic and theoretical seat that produced this reading: archives of the displacement, land-registry reconstruction, comparative settler-colonial analysis. They hold no stake in the arrangement's operation, and their claims are adjudicated by peer review rather than by any party to the conflict — the analytical seat from which the full structure is visible.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, postcolonial_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zionist_legitimacy_basis__settler_colonial_reading, israeli_jewish_settler_society).
narrative_ontology:fixing_cost_class(zionist_legitimacy_basis__settler_colonial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the Jewish national collective: membership (Law of Return), citizenship, state institutions, security provision, and collective self-governance — a real coordination problem (stateless, persecuted diaspora) that the arrangement genuinely solves for its beneficiary seat.
% TRANSFER_FUNCTION: Moves land, sovereignty, citizenship priority, and return rights from the Palestinian indigenous population to the Jewish-Israeli collective; moves international resources (military aid, diplomatic cover) into the structure; moves refugee status and its costs onto host states and the displaced themselves.
% ABSENT_VOICES: The displaced refugees are structurally absent from every conversation that determined and maintains the arrangement — they were not parties to the founding decisions, are excluded from the state's sovereign conversation, and their return claim is administered by the structure that displaced them. Palestinians under occupation are governed without a vote in the governing state.
% DISAPPEARANCE_RATIONALE: If the structure vanished overnight, the refugee claim would convert from suspended to immediately actionable, regional alliances built on the arrangement (aid flows, alignment, normalization sequencing) would unwind, the beneficiary collective's identity frame would collapse into a crisis of membership, and host states would face immediate repatriation pressure — the regional order rearranges around the vacuum.
% FOUNDING_PROBLEM: The statelessness and persecution of European Jewry: a people with no sovereign refuge, culminating in the Holocaust, for which sovereignty over a territory was pursued as the remedy.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's historical reality is corroborated from outside the benefiting parties: Nazi-era archives, contemporaneous refugee-refusal records (the Évian Conference), and Holocaust scholarship. Its current status is disputed: antisemitism-monitoring bodies external to the beneficiary set attest that the danger persists (supporting 'live'), while land registries, UNRWA records, declassified state documents, and Palestinian testimony — all outside the beneficiary set — corroborate that the remedy's implementation was constitutively displacing, which is the element this reading holds determinative.
narrative_ontology:disappearance_verdict(zionist_legitimacy_basis__settler_colonial_reading, world_rearranges).
narrative_ontology:founding_problem_status(zionist_legitimacy_basis__settler_colonial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__settler_colonial_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(zionist_legitimacy_basis__settler_colonial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zionist_legitimacy_basis__settler_colonial_reading, 0.86, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zionist_legitimacy_basis__settler_colonial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(zionist_legitimacy_basis__settler_colonial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(zionist_legitimacy_basis__settler_colonial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.86 at interval end) because the transferred goods — land, sovereignty, return — are constitutive of the arrangement rather than incidental to it: the Nation-State Law vests self-determination solely in the Jewish collective, and land administration structurally prioritizes that collective. Suppression is high (0.85) and is authored as a raw structural property, unscaled by power or scope: the arrangement's persistence depends on occupation administration, permit regimes, blockade, and the denial of return, not on participant preference. Theater is moderate (0.32): the state's administrative and security functions are real, but a growing share of activity is legitimacy maintenance — the democracy claim against the structural record, negotiation processes that run alongside settlement expansion, and the international advocacy apparatus. Accessibility collapse is 0.55: alternatives are heavily constrained (the two-state framework persists formally while its territorial basis is progressively removed; one-state and binational proposals persist outside the structure's own logic) but are not fully collapsed. Resistance is 0.75: the arrangement meets sustained, multi-generational resistance — uprisings, armed and civil resistance, legal advocacy, boycott movements, and internal dissent — among the highest of any construct in this corpus's domain. All three tracked metrics run on ONE shared time grid (1948, 1967, 1980, 1993, 2000, 2007, 2025) so every metric is authored at every examined point; the series oscillates rather than drifts monotonically — the Oslo-era dip in all three series is a negotiation-phase effect, not a structural reversal, and the oscillation itself functions as intermittent reinforcement: each relaxation phase raised expectations while the underlying transfer continued, which is documented as the cyclical pattern rather than treated as noise.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute very differently. From the agenda_setter seat (state apparatus), the structure is self-preservation and security management — the same rules read as existential necessity. From the beneficiary seats, the structure is refuge, identity, and alignment: the settler society experiences it as home and conscription at once; the diaspora and the US experience it at near-zero direct cost. From the payer seats, the identical structure is dispossession, occupation, and second-class membership — the refugee seat inherits the loss across generations with no exit, the occupied seat is governed without a vote, the citizen seat holds formal equality inside a polity constituted against its national claim. The analytical seat sees all of it at once. The engine computes this divergence from the structural data (power, exit, role); the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real flows: the settler society receives land, citizenship priority, and security; the diaspora institutions and the US government receive identity and strategic returns at arbitrage-grade exit. Victim declarations map to the displaced (trapped across generations), the occupied (trapped in place), and the citizens (constrained exit). One directionality override is authored: power_atom 'powerful' at d=0.18. The derivation from beneficiary + identity_locked would place the settler society near the full-beneficiary pole (~0.05), but the seat demonstrably bears the arrangement's security costs — universal conscription, war casualties, and the legitimacy price — so its structural relationship sits slightly above the beneficiary pole. No override is authored for institutional seats: the US (declared beneficiary, arbitrage) derives low d correctly, and the state apparatus is left to the engine's agenda_setter handling, with its fused dual position documented in commentary rather than forced through a power-atom override that would also capture the US seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — stateless, persecuted European Jewry — was real and externally corroborated, and the refuge function it generated is arguably still live (external antisemitism monitoring attests persistent danger). But the arrangement's current form is not entailed by the founding problem: displacement was never a requirement of refuge, and the structure now persists through identity fusion and power rather than through the founding problem's demands. The tangled_rope classification is what prevents both mandatrophy failure modes: mislabeling the whole arrangement as pure extraction would deny the real, externally corroborated coordination function and misread the beneficiary seats (whose refuge need is documented outside their own testimony); mislabeling it as pure coordination would launder a constitutive transfer of land, sovereignty, and return as coordination cost. The reading holds both truths simultaneously — genuine coordination, constitutive transfer — and the classification preserves that structure rather than resolving it in either direction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is the settler_colonial_reading of kernel zionist_legitimacy_basis; which reading''s legitimacy-determining element — colonial structure, persecution-driven return, or divine promise — actually governs the arrangement''s classification?',
    'The readings are held by different parties and are not resolvable by fiat; resolution tracks which element the standing arrangement''s own operation vindicates — the archival, legal, and demographic record of how land, citizenship, and return are actually administered.',
    'Under the national_liberation reading, ε over the same referent drops sharply (refuge coordination dominates and displacement reads as tragic cost rather than constitutive structure); under the religious reading, ε indexes to a theological warrant this reading does not recognize; under this reading, displacement is constitutive and ε stays high. The classification of the kernel family is undetermined until the contest resolves.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: one of three live readings of the zionist_legitimacy_basis kernel; siblings are separate constraint files.').

omega_variable(
    displacement_constitutivity,
    'Is Palestinian displacement constitutive of the arrangement''s coordination function, or incidental to it — could the Jewish national coordination function have been, or now be, delivered without the transfer?',
    'Counterfactual institutional analysis: test against partition-era alternatives actually on the table (binational proposals, federal schemes) and against current one-state and confederation proposals; if a coordination-equivalent without the transfer is institutionally specifiable, the transfer is separable.',
    'If incidental, the type drifts toward rope (the transfer is removable overhead); if constitutive AND the coordination story is cover, the type drifts toward snare. This reading holds constitutive-but-real-coordination, which is the tangled_rope signature; the omega marks exactly what evidence would move it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displacement_constitutivity, conceptual, 'The reading''s central structural claim, held open as an omega rather than asserted as settled.').

omega_variable(
    beneficiary_internal_differentiation,
    'Does the beneficiary collective benefit uniformly, or do internal strata (Ashkenazi/Mizrahi/Haredi, settler-versus-coastal, state-institution-versus-private-capital) capture asymmetric shares of the transferred goods?',
    'Distributional data: land-allocation records, state budget and settlement-subsidy flows, planning-committee composition.',
    'If the transfer concentrates on a sub-elite, gain_flow refines from the collective seat to that sub-elite, the payer seats'' coalition geometry changes, and the beneficiary-seat directionality derivation splits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_internal_differentiation, empirical, 'Whether the beneficiary seat is a uniform collective or a differentiated capture structure.').

omega_variable(
    persistence_basis,
    'Does the arrangement persist by coercion (active suppression of alternatives) or by genuine preference of its beneficiary majority?',
    'Israeli public-opinion trajectories under counterfactual cost changes — US aid withdrawal, sanctions scenarios, referendum history — distinguishing preference for the arrangement from preference against its alternatives under threat.',
    'If preference-driven, measured suppression overstates the structural force and the type softens toward rope; if coercion-driven, suppression is constitutive and the snare-ward drift strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persistence_basis, empirical, 'Whether the arrangement''s persistence rests on enforcement machinery or beneficiary consent.').

omega_variable(
    refugee_return_feasibility,
    'Is the displaced population''s return right materially exercisable at scale — demographically, legally, and in land terms — or has the transfer become irreversible in practice?',
    'Demographic projection and land-registry reconstruction under negotiated-return scenarios; comparative analysis of implemented return programs elsewhere.',
    'If return is materially infeasible at scale, the transfer is irreversible-in-practice, fixing_cost stays prohibitive, and the arrangement''s persistence becomes a pure maintenance question; if feasible, persistence becomes a choice question and the snare-ward reading of continued denial strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(refugee_return_feasibility, empirical, 'Material reversibility of the founding transfer, governing the fixing-cost half of the receipt surface.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__settler_colonial_reading, 1948, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zion_tr_t1948, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1948, 0.2).
narrative_ontology:measurement(zion_tr_t1967, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1967, 0.18).
narrative_ontology:measurement(zion_tr_t1980, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1980, 0.23).
narrative_ontology:measurement(zion_tr_t1993, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1993, 0.31).
narrative_ontology:measurement(zion_tr_t2000, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 2000, 0.26).
narrative_ontology:measurement(zion_tr_t2007, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 2007, 0.33).
narrative_ontology:measurement(zion_tr_t2025, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 2025, 0.32).

% Extraction over time
narrative_ontology:measurement(zion_be_t1948, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1948, 0.78).
narrative_ontology:measurement(zion_be_t1967, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1967, 0.81).
narrative_ontology:measurement(zion_be_t1980, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1980, 0.8).
narrative_ontology:measurement(zion_be_t1993, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1993, 0.76).
narrative_ontology:measurement(zion_be_t2000, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 2000, 0.79).
narrative_ontology:measurement(zion_be_t2007, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 2007, 0.84).
narrative_ontology:measurement(zion_be_t2025, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 2025, 0.86).

% Suppression requirement over time
narrative_ontology:measurement(zion_su_t1948, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1948, 0.66).
narrative_ontology:measurement(zion_su_t1967, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1967, 0.73).
narrative_ontology:measurement(zion_su_t1980, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1980, 0.75).
narrative_ontology:measurement(zion_su_t1993, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1993, 0.69).
narrative_ontology:measurement(zion_su_t2000, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 2000, 0.78).
narrative_ontology:measurement(zion_su_t2007, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 2007, 0.82).
narrative_ontology:measurement(zion_su_t2025, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 2025, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__settler_colonial_reading, identity_coordination).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, zionist_legitimacy_basis__national_liberation_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, zionist_legitimacy_basis__religious_restoration_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the natural-language label 'the legitimacy basis of Zionism' covers three structurally distinct claims and decomposes into three stories per the ε-invariance principle. All three readings share the referent (the standing arrangement) and author different ε over it; they differ in beneficiary structure (persecuted-returnee collective vs. covenant community vs. settler society), victim structure, and the element held legitimacy-determining. This story is the settler-colonial reading; the national-liberation reading is the upstream claim most often cited against it (the refugee-refuge framing), and the religious-restoration reading operates on an orthogonal theological axis. Each story links the other two via network.affects_constraints so contamination analysis can track how evidence or institutional rulings move the family together.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(zionist_legitimacy_basis__settler_colonial_reading, powerful, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
