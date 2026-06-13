% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy_dual__two_state_coexistence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy_dual__two_state_coexistence_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: territorial_legitimacy_dual__two_state_coexistence_reading
 *   human_readable: Two-State Coexistence Framework with 1967 Boundaries
 *   domain: political/international
 *
 * SUMMARY:
 *   The two-state framework with 1967 boundaries represents a middle reading
 *   of the territorial legitimacy kernel: it grants mutual recognition to
 *   both Israeli and Palestinian peoples (accepting 1948 state establishment
 *   for Israel, and claiming 1967 territories for Palestine) and proposes
 *   partition as the compromise. This reading is distinct from the Zionist
 *   refuge reading (which privileges Israeli legitimacy through historical
 *   persecution and UN Partition acceptance, potentially rejecting
 *   Palestinian statehood) and the Palestinian autochthony reading (which
 *   privileges Palestinian legitimacy through continuous habitation and right
 *   of return, potentially rejecting Israel's existence). The two-state
 *   reading attempts to satisfy both by accepting 1948 for Israel AND 1967
 *   for Palestine, but this requires both sides to renounce claims that their
 *   identity narratives treat as foundational—Israel renounces settlements
 *   and territorial expansion; Palestine renounces return beyond state
 *   boundaries and territorial claims to 1948 lands.
 *
 * KEY AGENTS:
 *   - Two-state framework architects (international mediators, UN, negotiating delegations): set and enforce the compromise through diplomatic pressure and conditionality.
 *   - Israeli Jewish majority: beneficiary of international legitimacy but payer of settlement withdrawal and territorial constraint; identity-locked because rejecting 'Jewish state' is politically impossible.
 *   - Palestinian national movement: beneficiary of statehood recognition but payer of limited right of return and 1967-line acceptance; identity-locked by the displacement narrative they cannot abandon.
 *   - Palestinian refugees and diaspora (powerless): structurally excluded from return; no seat, no voice, no veto; bear the asymmetric cost of frozen displacement.
 *   - Israeli settler communities (organized, identity-locked): required to withdraw or be incorporated into Palestinian territory; their ideological commitment to territorial claim makes exit identity-annihilation.
 *   - Security cooperation advocates: benefit from shift to joint security logic; constrained by institutionalized doctrine.
 *   - Diaspora opposition blocs (excluded): have no seat in negotiations; their opposition is silenced by geographic privilege to territorial actors.
 *   - Third-state guarantors (institutional, analytical): monitor and enforce; can modify the constraint if will erodes.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__two_state_coexistence_reading, 0.58).
domain_priors:suppression_score(territorial_legitimacy_dual__two_state_coexistence_reading, 0.71).
domain_priors:theater_ratio(territorial_legitimacy_dual__two_state_coexistence_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__two_state_coexistence_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy_dual__two_state_coexistence_reading, "Two-State Coexistence Framework with 1967 Boundaries").
narrative_ontology:topic_domain(territorial_legitimacy_dual__two_state_coexistence_reading, "political/international").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__two_state_coexistence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__two_state_coexistence_reading, '29161ef5-c21f-453e-b217-7ab0cd1da3e6').
narrative_ontology:cs_kernel_codification('29161ef5-c21f-453e-b217-7ab0cd1da3e6', distributed).
narrative_ontology:cs_authority_grounding('29161ef5-c21f-453e-b217-7ab0cd1da3e6', distributed).
narrative_ontology:cs_reading_relation('29161ef5-c21f-453e-b217-7ab0cd1da3e6', territorial_legitimacy_dual__zionist_refuge_reading, influences).
narrative_ontology:cs_reading_relation('29161ef5-c21f-453e-b217-7ab0cd1da3e6', territorial_legitimacy_dual__palestinian_autochthony_reading, influences).
narrative_ontology:cs_axiom('29161ef5-c21f-453e-b217-7ab0cd1da3e6', foundational, dual_legitimacy_acceptance).
narrative_ontology:cs_axiom_status(dual_legitimacy_acceptance, holdable).
narrative_ontology:cs_axiom_grounding('29161ef5-c21f-453e-b217-7ab0cd1da3e6', dual_legitimacy_acceptance, conventional).
narrative_ontology:cs_axiom('29161ef5-c21f-453e-b217-7ab0cd1da3e6', foundational, id_1967_boundary_finality).
narrative_ontology:cs_axiom_status(id_1967_boundary_finality, holdable).
narrative_ontology:cs_axiom_grounding('29161ef5-c21f-453e-b217-7ab0cd1da3e6', id_1967_boundary_finality, instrumental).
narrative_ontology:cs_reference_frame('29161ef5-c21f-453e-b217-7ab0cd1da3e6', mutual_dual_legitimacy_1967_partition).
narrative_ontology:cs_drift_state('29161ef5-c21f-453e-b217-7ab0cd1da3e6', contemporary_2026, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('29161ef5-c21f-453e-b217-7ab0cd1da3e6', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__two_state_coexistence_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, both_peoples_mutual_recognition).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, international_order_stability).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_displaced_persons_exceeding_state_territory).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_settler_communities_east_of_1967_line).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, unresolved_refugees_from_1948).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__two_state_coexistence_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(territorial_legitimacy_dual__two_state_coexistence_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy_dual__two_state_coexistence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy_dual__two_state_coexistence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy_dual__two_state_coexistence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures how much the framework transfers legitimacy and territory asymmetrically: Palestinian refugees lose return rights (asymmetric extraction from them); Israeli settlers lose territorial claims (asymmetric extraction from them); both sides lose the option to pursue total victory (symmetric extraction). At 0.58 at interval end, extractiveness captures the real costs both sides bear—territory foregone, refugee claims frozen, security sovereignty shared. The rising trajectory from 1967 (0.35) to 2015 (0.64) reflects the accumulated weight of frozen claims over 50 years—each year of non-implementation, each generation of refugees without return, each new settlement built outside the 1967 line accumulates extraction pressure. The slight drop to 0.58 at 2026 reflects partial normalization (Abraham Accords logic, regional reorientation) that reduces zero-sum framing but does NOT resolve the underlying territorial asymmetries. Theater_ratio rises from 0.20 to 0.62, indicating that performative commitment to the framework (Peace Process summits, confidence-building announcements, security cooperation photo ops) increasingly dominates actual implementation. Suppression begins at 0.45 (early post-1967 military occupation) and rises to 0.78 at 2015 (peak enforcement machinery required to hold the framework against rising identity-fusion resistance) before declining to 0.71 (some de-escalation through normalization but still active suppression of return movements and settlement expansion).
 *
 * PERSPECTIVAL GAP:
 *   The framework appears as coordination to its architects and security-cooperation advocates (solved zero-sum logic, enabled mutual recognition). It appears as extraction + imposition to refugee advocates (lost return rights, frozen displacement) and settler communities (lost territorial claims). The engine computes these divergences from directionality: framework architects sit at d~0.5 (symmetric costs and benefits of brokering); refugees sit at d~0.9 (high extraction, trapped exit, no voice); settlers sit at d~0.85 (territorial loss, identity-locked); international guarantors sit at d~0.3 (analytical position, no direct loss). The two beneficiary groups (both_peoples_mutual_recognition, international_order_stability) both benefit genuinely from reduced zero-sum conflict; but both also pay the cost of frozen claims and require continuous suppression of opposition voices.
 *
 * DIRECTIONALITY LOGIC:
 *   Israeli Jewish majority: d~0.45. They gain legitimacy recognition (beneficiary effect) and sovereignty within secure boundaries, but lose settlement expansion rights and must accept Palestinian statehood (target effect). Identity-lock on 'Jewish state' means they cannot exit by rejecting Israel's existence, making their d higher than a mobile beneficiary would compute. Palestinian national movement: d~0.50. They gain statehood recognition and 1967 territories (beneficiary effect) but lose 1948 return rights and must accept Israel's existence (target effect). Identity-lock on Palestinian return makes their d higher than a mobile beneficiary would compute. Palestinian refugees: d~0.95. They bear the direct extraction (frozen displacement, no return), have trapped exit (cannot resettle elsewhere under international law), and have no seat in negotiations. Israeli settlers: d~0.88. They bear direct extraction (territorial loss, withdrawal requirement), have identity-locked exit (territorial claim fused with religious/ideological identity), and are excluded from the framework's beneficiary logic. Framework architects: d~0.5. They benefit from a 'solution' that appears balanced, but must continuously enforce suppression against both sides' opposition voices and renunciation movements.
 *
 * MANDATROPHY ANALYSIS:
 *   The two-state framework's founding mandate (replace zero-sum existential conflict with bounded coexistence) is structurally LIVE but increasingly UNIMPLEMENTED. The framework has not solved the problem it was built for—Israeli-Palestinian conflict remains endemic; settlement expansion continues; Palestinian refugee returns remain blocked; security cooperation remains intermittent. Yet the framework persists due to international institutional investment, Oslo architecture, and the identity-lock that prevents either side from rejecting statehood entirely (rejection would delegitimize both). This is NOT pure piton theater (the founding mandate is still claimed as operative). It is NOT pure snare (both sides genuinely benefit from parts of the framework—Israeli security coordination, Palestinian diplomatic standing). It is tangled rope at maximum strain: the coordination function (partition, mutual recognition) is real and genuinely cooperative for border management and security; the extraction function (frozen claims, frozen displacement) is equally real and asymmetrically concentrated on powerless groups. The rising theater ratio (0.62) indicates performative commitment is increasingly the only thing holding the framework together—Peace Process summits, normalization agreements, confidence-building measures that avoid addressing the core territorial asymmetries. Mandatrophy could resolve by: (a) actual implementation (settlements withdrawn, refugees resettled, borders demarcated), which would reduce extraction and convert to rope; or (b) framework collapse and reversion to zero-sum logic, which would increase extraction and convert to snare. Current trajectory is (c) frozen mandatrophy with rising theater—the constraint persists as international architecture and identity commitment, not as operative coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_sustainability,
    'Can identity-locked commitment to ''statehood'' (Palestinian) and ''Jewish state'' (Israeli) persist indefinitely without actual implementation of the framework, or does frozen mandatrophy eventually force identity reconstitution?',
    'Generational turnover: if younger Palestinians reject statehood-qua-framework and reassert return-demand, or younger Israelis reject Jewish-state identity in favor of secular or multi-ethnic nationalism, the identity lock breaks. Measurable via public opinion, political platform evolution, and youth activism patterns.',
    'If identity lock persists, framework persists as theater; if it breaks, the constraint either collapses to zero-sum logic (snare) or reconfigures around a different reading entirely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_sustainability, empirical, 'Whether generational identity commitments hold the framework or erode under 50+ years of non-implementation.').

omega_variable(
    asymmetric_extraction_concealment,
    'Is the framework''s persistence a genuine compromise where both sides accept real losses, or a mechanism where more powerful actors (Israel, international order) impose losses on the less powerful (Palestinian refugees, settlers) while calling it ''mutual recognition''?',
    'Analysis of who actually bears the enforcement costs: if suppression falls equally on both sides (equal police action, equal settlement dismantling, equal refugee compensation), framework is tangled rope; if suppression falls disproportionately on Palestinians (more police, fewer settlers evicted, fewer refugees compensated), framework is snare dressed as rope.',
    'Equal suppression supports computed classification as tangled rope; disproportionate suppression would shift computed type toward snare at the Palestinian-side seats. The measurement of suppression_requirement already captures this (0.71 does not distinguish level-wise burden).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(asymmetric_extraction_concealment, empirical, 'Whether ''mutual recognition'' masks asymmetric enforcement burden allocation.').

omega_variable(
    zionist_vs_autochthony_foreclosure,
    'Do the zionist_refuge_reading and palestinian_autochthony_reading logically foreclose each other (impossible to hold both in one framework), or do they merely coexist as different parties'' preferred readings of the same contested kernel?',
    'Test the core premises: Zionist readings assert ''Jewish historical claim + UN legitimacy + existential security''; autochthony readings assert ''Palestinian continuous habitation + displacement trauma + non-negotiable return''. Can a single legal framework hold both (e.g., UN acceptance of both Zionism AND Palestinian return)? If yes, coexist_with; if no, forecloses.',
    'Foreclosure would mean the two-state reading (which accepts both legitimacies) is internally incoherent; coexistence would mean it is a genuine middle reading. Affects the stability attribution of the kernel itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(zionist_vs_autochthony_foreclosure, conceptual, 'Whether Zionist and autochthony readings are logically incompatible or just politically contested.').

omega_variable(
    id_1967_boundary_naturalness,
    'Is the 1967 Green Line a natural or inevitable boundary (shaped by military realities, topography, security logic), or a constructed boundary (arbitrary ceasefire line chosen for expediency, now reified by international law)?',
    'Historical analysis of 1967 armistice negotiations: were boundaries chosen for topographic/security reasons (suggests naturalness) or for negotiating convenience (suggests construction)? Geographic analysis: do 1967 lines follow watershed, terrain, or appear arbitrary?',
    'Natural boundaries support treating 1967 as stable compromise; constructed boundaries undermine it—either side can claim the boundary is illegitimate and renegotiable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(id_1967_boundary_naturalness, empirical, 'Whether the 1967 boundary is natural or contingent, shaping its perceived legitimacy.').

omega_variable(
    right_of_return_limitation_mechanism,
    'Is limiting Palestinian right of return to the Palestinian state within 1967 boundaries a principled compromise (accepting UN 194 but territorializing it), or a mechanism that freezes displacement and converts Palestinian refugees into permanent stateless persons?',
    'Compare actual return numbers under the framework (where implemented) with refugee population; assess whether statelessness persists or is resolved through compensation or regional resettlement. Look for Palestinian refugees who remain unable to return or resettle after 50+ years.',
    'If returns occur and refugees are integrated, the limitation is a genuine compromise; if refugees remain stateless and return is blocked, the framework has frozen displacement—increasing its extractiveness from the refugee perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(right_of_return_limitation_mechanism, empirical, 'Whether right-of-return limitation is compromise or freezing mechanism for Palestinian displacement.').

omega_variable(
    reading_coexistence_instability,
    'Can the three kernel readings (Zionist refuge, Palestinian autochthony, two-state coexistence) remain coexistent indefinitely, or does one reading eventually dominate, foreclose others, or collapse back to zero-sum conflict?',
    'Track policy platform evolution in both polities: if either side''s leadership adopts exclusively one reading (e.g., Palestinian leadership explicitly rejects two-state in favor of autochthony-only, or Israeli leadership rejects two-state in favor of Zionist-refuge-plus-annexation), the coexistence is unstable and the kernel is moving toward foreclosure.',
    'Stability of coexistence means the two-state reading remains viable; foreclosure means this reading becomes untenable and the constraint dissolves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_coexistence_instability, empirical, 'Whether kernel readings can coexist or trend toward one reading''s dominance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__two_state_coexistence_reading, 1967, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1967, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 1967, 0.2).
narrative_ontology:measurement(terr_tr_t1978, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 1978, 0.35).
narrative_ontology:measurement(terr_tr_t1993, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 1993, 0.48).
narrative_ontology:measurement(terr_tr_t2005, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 2005, 0.58).
narrative_ontology:measurement(terr_tr_t2015, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 2015, 0.65).
narrative_ontology:measurement(terr_tr_t2026, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 2026, 0.62).

% Extraction over time
narrative_ontology:measurement(terr_be_t1967, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 1967, 0.35).
narrative_ontology:measurement(terr_be_t1978, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 1978, 0.42).
narrative_ontology:measurement(terr_be_t1993, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 1993, 0.55).
narrative_ontology:measurement(terr_be_t2005, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2005, 0.62).
narrative_ontology:measurement(terr_be_t2015, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2015, 0.64).
narrative_ontology:measurement(terr_be_t2026, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2026, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1967, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 1967, 0.45).
narrative_ontology:measurement(terr_su_t1978, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 1978, 0.58).
narrative_ontology:measurement(terr_su_t1993, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 1993, 0.68).
narrative_ontology:measurement(terr_su_t2005, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2005, 0.74).
narrative_ontology:measurement(terr_su_t2015, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2015, 0.78).
narrative_ontology:measurement(terr_su_t2026, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2026, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__two_state_coexistence_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(territorial_legitimacy_dual__two_state_coexistence_reading, 0.18).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__two_state_coexistence_reading, territorial_legitimacy_dual__zionist_refuge_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__two_state_coexistence_reading, territorial_legitimacy_dual__palestinian_autochthony_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_settlement_expansion_enforcement).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_refugee_resettlement_mechanism).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__two_state_coexistence_reading, security_cooperation_framework_bilateral).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the territorial_legitimacy_dual kernel. The Zionist refuge reading grounds Israeli legitimacy in historical persecution and UN Partition; the Palestinian autochthony reading grounds Palestinian legitimacy in continuous habitation and displacement. This two-state reading accepts both legitimacies and proposes 1967 partition as compromise. All three readings are live positions held by different parties; none forecloses the others within any single party's framework, though they INFLUENCE each other by shifting legitimacy conditions and territorial claims. The epsilon values differ substantially: Zionist and autochthony readings are primarily identity-constitution constraints with lower extractiveness (they are readings of legitimacy itself, not mechanisms of partition); the two-state reading is structurally an allocation mechanism with higher extractiveness (it transfers territory and freezes claims). All three must be authored to model the kernel contest accurately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_legitimacy_dual__two_state_coexistence_reading, organized, 0.52).
constraint_indexing:directionality_override(territorial_legitimacy_dual__two_state_coexistence_reading, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
