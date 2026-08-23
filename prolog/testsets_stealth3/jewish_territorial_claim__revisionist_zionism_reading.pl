% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__revisionist_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_territorial_claim__revisionist_zionism_reading, []).

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
 *   constraint_id: jewish_territorial_claim__revisionist_zionism_reading
 *   human_readable: Maximalist Territorial Claim with Iron Wall Compulsion (Revisionist Reading)
 *   domain: political history/settler colonialism/nationalism studies
 *
 * SUMMARY:
 *   This story instantiates the REVISIONIST READING of the contested kernel
 *   'jewish_territorial_claim': a territorial claim spanning BOTH banks of
 *   the Jordan River, held to be non-negotiable, paired with the doctrine
 *   (Jabotinsky's Iron Wall essays, 1923) that Arab consent is NOT a
 *   prerequisite for Jewish sovereignty — acceptance is to be produced by the
 *   demonstrated impossibility of defeating Jewish force. The interval
 *   (T=0..25, anchoring 1923-1948) runs from the doctrine's publication and
 *   the founding of Betar through settlement expansion, the Arab Revolt, the
 *   British White Paper reversal, the movement's own armed revolt against the
 *   Mandate, and the 1948 war that realized sovereignty on the west bank
 *   while the east bank hardened into the Kingdom of Jordan. EPSILON
 *   REFERENT: the standing arrangement under contest is the maximalist
 *   forced-sovereignty program itself as it operated across the interval —
 *   not any sibling reading's preferred alternative (partition, gradualism,
 *   spiritual center). FAMILY DECOMPOSITION: the colloquial label 'the Jewish
 *   territorial claim' decomposes into four structurally distinct constraints
 *   per the epsilon-invariance principle; this file authors only the
 *   revisionist instantiation and links its siblings via network edges.
 *
 * KEY AGENTS:
 *   - - revisionist_leadership: agenda-setting seat (organized/identity_locked) — authors the claim, defines loyalty monistically, raises diaspora funds
 *   - - revisionist_paramilitary_organizations: enforcement arm (organized/constrained) — converts doctrine into trained force and retaliation
 *   - - jewish_settlers_both_banks: principal on-the-ground beneficiary (moderate/constrained) — receives protected settlement; east-bank settlers await policy reversal
 *   - - european_jewish_diaspora: promised beneficiary (powerless/trapped) — supplies recruits and funds; the refuge promise addresses their closing exits in Europe
 *   - - palestinian_arab_communities: primary payer (moderate/trapped) — demographic majority west of the Jordan; consent never solicited pre-wall
 *   - - transjordanian_arab_communities: payer (moderate/trapped) — east-bank population living inside the claim's asserted footprint
 *   - - arab_national_leadership: organized payer representation (organized/constrained) — pays for resistance in exile, imprisonment, cadre losses
 *   - - british_mandate_administration: external agenda-setter (institutional/mobile) — administers the regime bounding the claim; detaches the east bank, caps immigration
 *   - - neighboring_arab_states: excluded parties (organized/mobile) — objectors kept outside the doctrine's planning until 1947-48
 *   - - league_of_nations_council: analytical observer (institutional/analytical) — confirms Mandate terms, records petitions, fixes no borders and no mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__revisionist_zionism_reading, 0.86).
domain_priors:suppression_score(jewish_territorial_claim__revisionist_zionism_reading, 0.84).
domain_priors:theater_ratio(jewish_territorial_claim__revisionist_zionism_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, extractiveness, 0.86).
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 0.84).
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__revisionist_zionism_reading, snare).
narrative_ontology:human_readable(jewish_territorial_claim__revisionist_zionism_reading, "Maximalist Territorial Claim with Iron Wall Compulsion (Revisionist Reading)").
narrative_ontology:topic_domain(jewish_territorial_claim__revisionist_zionism_reading, "political history/settler colonialism/nationalism studies").

domain_priors:requires_active_enforcement(jewish_territorial_claim__revisionist_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__revisionist_zionism_reading, '6970d811-2c2a-4061-ba8c-342795a1edfe').
narrative_ontology:cs_kernel_codification('6970d811-2c2a-4061-ba8c-342795a1edfe', fixed_text).
narrative_ontology:cs_authority_grounding('6970d811-2c2a-4061-ba8c-342795a1edfe', lineage).
narrative_ontology:cs_interpretation_layer_present('6970d811-2c2a-4061-ba8c-342795a1edfe').
narrative_ontology:cs_reading_relation('6970d811-2c2a-4061-ba8c-342795a1edfe', jewish_territorial_claim__political_zionism_reading, influences).
narrative_ontology:cs_reading_relation('6970d811-2c2a-4061-ba8c-342795a1edfe', jewish_territorial_claim__labor_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('6970d811-2c2a-4061-ba8c-342795a1edfe', jewish_territorial_claim__cultural_zionism_reading, coexists_with).
narrative_ontology:cs_axiom('6970d811-2c2a-4061-ba8c-342795a1edfe', foundational, arab_consent_not_prerequisite).
narrative_ontology:cs_axiom_status(arab_consent_not_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('6970d811-2c2a-4061-ba8c-342795a1edfe', arab_consent_not_prerequisite, instrumental).
narrative_ontology:cs_axiom('6970d811-2c2a-4061-ba8c-342795a1edfe', foundational, territorial_maximalism_non_negotiable).
narrative_ontology:cs_axiom_status(territorial_maximalism_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('6970d811-2c2a-4061-ba8c-342795a1edfe', territorial_maximalism_non_negotiable, deontological).
narrative_ontology:cs_reference_frame('6970d811-2c2a-4061-ba8c-342795a1edfe', integral_both_banks_national_home).
narrative_ontology:cs_drift_state('6970d811-2c2a-4061-ba8c-342795a1edfe', post_jordan_treaty_contemporary, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('6970d811-2c2a-4061-ba8c-342795a1edfe', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__revisionist_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__revisionist_zionism_reading, revisionist_leadership).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__revisionist_zionism_reading, revisionist_paramilitary_organizations).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__revisionist_zionism_reading, jewish_settlers_both_banks).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__revisionist_zionism_reading, european_jewish_diaspora).
narrative_ontology:constraint_victim(jewish_territorial_claim__revisionist_zionism_reading, palestinian_arab_communities).
narrative_ontology:constraint_victim(jewish_territorial_claim__revisionist_zionism_reading, transjordanian_arab_communities).
narrative_ontology:constraint_victim(jewish_territorial_claim__revisionist_zionism_reading, arab_national_leadership).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__revisionist_zionism_reading, iron_wall_deterrence_doctrine).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__revisionist_zionism_reading, integral_erez_israel_entitlement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writes and propagates the doctrine: the claim spanning both banks of the Jordan is declared non-negotiable, Arab consent is explicitly removed as a precondition, and a standing display of unbeatable force is named as the mechanism that will eventually bring Arab acceptance. Raises funds across diaspora communities, commands the youth movement, and defines loyalty in total terms — partition, staged autonomy, and negotiated truncation are all treated as betrayal of the whole. Stepping off the platform would mean dismantling the identity the movement is built around.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, revisionist_leadership, agenda_setter,
    organized, biographical, identity_locked, global).

% Trains and arms the movement's young members, guards settlements, conducts retaliatory raids, and after 1944 turns on the Mandatory police itself over immigration limits. The uncompromising character of the claim feeds them recruits, arms, and organizational purpose; their operational tempo rises whenever diplomacy threatens the maximal program. Demobilizing would dissolve the cadre structure their members' lives are organized around.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, revisionist_paramilitary_organizations, agenda_setter,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__revisionist_zionism_reading, revisionist_paramilitary_organizations, beneficiary).

% Farm, build, and hold land on both sides of the river where security permits, under the armed protection the doctrine prescribes. The promise their presence will never again be negotiable is the substance of what they receive. Those east of the river await a policy reversal that would reopen settlement; leaving in either direction means abandoning homes and lifework, and after the European catastrophe there is nowhere to return to.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, jewish_settlers_both_banks, beneficiary,
    moderate, biographical, constrained, regional).

% Forms the movement's recruiting and fundraising base and the intended recipient of statehood-as-refuge. Through the 1930s and 1940s the doors out of Europe close one by one and the promise of an unconditional territorial home becomes, for many, the only remaining exit anywhere. Their trapped position abroad is what gives the maximal pledge its moral force inside the movement; they pay dues, send sons to Betar, and consume the promise.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, european_jewish_diaspora, beneficiary,
    powerless, biographical, trapped, continental).

% Form the demographic majority west of the river. Under the doctrine their consent is not sought before the fact: land sales displace tenant families, retaliatory columns answer village resistance with collective penalties, and their towns are garrisoned during the revolt years. Their options are submission, flight, or uprising — each ruinous, and the doctrine is constructed so that none succeeds.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, palestinian_arab_communities, payer,
    moderate, generational, trapped, regional).

% Live east of the river under an emirate administration that a 1922 white paper detached from the National Home. The maximal claim asserts that separation is provisional and the east bank belongs to the same national patrimony; their villages, grazing lands, and emerging state sit inside a claim they were never asked about, and their ruler's survival depends on keeping the claim dormant.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, transjordanian_arab_communities, payer,
    moderate, generational, trapped, regional).

% Organizes petitions, general strikes, and the 1936-39 revolt; negotiates with London; boycotts proposals regarded as legitimizing the National Home. Pays for resistance with exile, imprisonment, and the decimation of its cadres through the suppression decade, and enters 1948 with its command structure badly degraded.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, arab_national_leadership, payer,
    organized, biographical, constrained, regional).

% Administers the Mandate regime: issues the 1922 white paper detaching the east bank, sets and caps immigration quotas, convenes the Peel partition inquiry, and suppresses the Arab Revolt with massive force. Holds the coercive instruments that decide what is implementable, and is courted by one movement wing and bombed by another. Its policy reversals are the main lever by which the claim's practical scope gets narrowed.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, british_mandate_administration, agenda_setter,
    institutional, generational, mobile, continental).

% Would object to a maximal Jewish state on both banks abutting their territories — and do object once finally convened in 1947-48. During the doctrine's formation decades they are outside the conversation entirely; its planning simply assumes their eventual acquiescence once force is demonstrated, without ever soliciting their terms.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, neighboring_arab_states, excluded,
    organized, generational, mobile, continental).

% Confirms the Mandate terms in 1922 and receives periodic reports through the Permanent Mandates Commission, hearing petitions from both communities and recording that the National Home text fixes neither borders nor mechanism. Evaluates legality and feasibility without administering anything; its findings are advisory inputs the contending parties each cite selectively.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, league_of_nations_council, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_territorial_claim__revisionist_zionism_reading, jewish_settlers_both_banks).
narrative_ontology:fixing_cost_class(jewish_territorial_claim__revisionist_zionism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves, for the movement, the collective-action problem of converting dispersed diaspora resources into a single credible deterrent: one doctrine aligns fundraising, youth training, settlement defense, and retaliation so that no faction free-rides on another's restraint and no adversary can probe the fragments separately.
% TRANSFER_FUNCTION: Moves land title, housing, and political sovereignty from the Arab residents of both banks to Jewish national institutions and settlers, without consent solicitation or equivalent compensation; separately moves diaspora money and manpower into the settlement and paramilitary apparatus.
% ABSENT_VOICES: The Arab inhabitants of both banks appear in the doctrine only as an obstacle to be deterred, never as negotiating principals — their consent is explicitly struck from the prerequisites. Neighboring Arab states are excluded from all planning until 1947-48. Inside the Yishuv, binational-state advocates (Brit Shalom) are marginalized as naive; within the movement itself, advocates of partition are purged rhetorically as traitors to the whole.
% DISAPPEARANCE_RATIONALE: Borders, the refugee map, and the region's conflict architecture all depend on the maximal program: without the forced track, partition-line sovereignty (the Peel-type lines) or consensual-limit arrangements were live paths; the east bank's separation would have gone formally uncontested; the 1948 war's scale and the displacement of roughly seven hundred thousand are downstream consequences of the uncompromised claim meeting regional intervention.
% FOUNDING_PROBLEM: How a stateless minority facing escalating extermination pressure in Europe establishes sovereign statehood on a territory whose resident majority refuses it, under a great-power patron whose commitments are conditional — the doctrine's answer: strike consent from the prerequisites and build unbeatable force first.
% FOUNDING_PROBLEM_CORROBORATION: The establishment problem's termination is attested from outside the beneficiary set by the Mandate's formal termination, the simultaneous United States and Soviet recognitions of May 1948, and UN admission in 1949. The underlying dual-community problem the doctrine addressed was independently attested by the Royal (Peel) Commission's 1937 report, a great-power body hearing both communities. No Arab-party source attests the doctrine's FRAMING of the solution — Arab testimony corroborates the problem (the refusal and its costs) strictly from the opposite seat.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__revisionist_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__revisionist_zionism_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__revisionist_zionism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_territorial_claim__revisionist_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__revisionist_zionism_reading, 0.86, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_territorial_claim__revisionist_zionism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_territorial_claim__revisionist_zionism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_territorial_claim__revisionist_zionism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is very high (0.86 at interval end) because the doctrine's defining move is to take land, sovereignty, and demographic control from the resident majority of both banks without soliciting or compensating their consent — displacement, tenancy eviction, and the foreclosure of Arab self-determination are not side effects but the doctrine's operating content. Suppression is correspondingly high (0.84) because the doctrine DECLARES force as the primary mechanism: the iron wall exists precisely so that Arab political agency is irrelevant until deterred into acquiescence. Suppression is authored as a RAW STRUCTURAL PROPERTY, unscaled; the engine scales only extractiveness by directionality and scope. Theater is moderate-low (0.32): the 'honorable peace after the wall' promissory rhetoric and the mobilization pageantry of the youth movement are performative layers over a substantively coercive program. Accessibility collapse is moderate (0.58): within the movement's decision space the doctrine eliminated compromise alternatives (partition rejected as amputation, gradualism as betrayal — Jabotinsky's monism), but the sibling readings and the Peel-track partition proposals remained live external alternatives throughout. Resistance is high (0.72): the 1936-39 Arab Revolt, urban riots, boycotts, intra-Yishuv opposition from labor and binationalist camps, and the British 1939 White Paper all pushed back. The temporal series run on ONE SHARED GRID (t=0,5,10,15,20,25) for all three metrics. Trajectories are monotone-rising with a single mid-interval dip (t=15, 1938: wartime resource strain and the movement's tactical pause against Britain temporarily lowered both enforcement tempo and rhetorical output); the oscillation is externally driven, not an intermittent-reinforcement cycle. The suppression_requirement series is authored deliberately: it tracks the MATURATION OF ENFORCEMENT CAPACITY (Betar 1923, Irgun's consolidation and 1937 split, Lehi 1940, integration into the 1948 war effort), not merely shifting extraction. COALITION CHECK: the payer seats' coalition potential (local revolt multiplied by regional states) is exactly what the doctrine's deterrence design was built to preempt; the 1948 intervention tested it and failed against the wall's accumulated facts. Receipt surface: land title and protected settlement accrue concretely to the settler seat; leadership and paramilitary collect organizational rents secondarily, so gain_flow names the settler seat.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the movement's seats compute radically different types from identical structure: to the Arab communities the arrangement is enforced dispossession with no coordination dividend flowing back to them; to the leadership and settlers it is national rescue and the only honest reading of the Mandate text. A third divergence sits INSIDE the beneficiary bloc: settlers wanted protection now, the leadership insisted the whole territory come as one package even at the price of slower settlement, and — decisively after 1942 — the diaspora rank-and-file wanted open doors more than intact borders (the 1944 revolt against Britain was fought over immigration limits, with maximalism riding along). The 1948 Altalena episode shows the force instrument momentarily turning inward between movement wings. None of this is adjudicated by the authored claim; the engine computes it per seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the movement-side seats to the subsidy end of directionality: settlers (protected presence), diaspora (promised refuge), leadership and paramilitary (organizational purpose and growth). Victim declarations drive the Arab seats to the target end: both community seats and the leadership pay in land, sovereignty, blood, and exile, with TRAPPED exits (there is nowhere the constraint permits them to be compensated elsewhere — the homeland is the point). One override: the european_jewish_diaspora seat is POWERLESS and TRAPPED, and the derivation would read trapped-exit as target-leaning — but their trap is external (closing European borders), and the constraint subsidizes them (the refuge promise is addressed to them), so d is overridden DOWN to 0.12. Institutional seats (Mandate administration, League Council) are left to the derivation chain and fallback; the Council is analytical and the Administration sits near-symmetric — it neither captures the gains nor primarily pays the transfer, bearing instead security costs and political exposure on both flanks.
 *
 * MANDATROPHY ANALYSIS:
 *   The doctrine's founding problem — how a stateless, hunted minority establishes sovereignty over refusing residents — TERMINATED in 1948: sovereignty was achieved. What persisted afterward was the maximal-territorial claim maintained as irredentist commitment (the east bank claim formally dormant, the west-bank maximal wing active). The classification apparatus guards against two opposite misreadings. First, mislabeling the arrangement as pure coordination: the national-liberation framing is genuine FOR THE MOVEMENT SEATS, but the same structure that coordinates the movement extracts from the region's majority with no reciprocal flow — per-seat computation keeps the payer seats visible no matter how flattering the liberation frame is to the beneficiary seats. Second, mislabeling the post-1948 persistence as mere inertial theater: the surviving claim remained enforcement-backed, not vestigial. The R5 mismatch signature (founding_problem_status=dead combined with disappearance_verdict=world_rearranges) correctly routes the post-interval continuation to the capture/zombie investigation rather than letting either the liberation romance or the piton decay-story stand unexamined.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_acceptance_status,
    'Does acceptance produced by demonstrated invincibility constitute consent, or is acceptance under an iron wall structurally identical to coerced submission?',
    'Longitudinal legitimation studies of post-deterrence arrangements and comparative cases: track whether succeeding generations treat the arrangement as self-authored or as an inherited imposition.',
    'If compelled acceptance never ripens into consent, the legitimacy deficit is permanent, effective extraction stays elevated, and the snare shape persists indefinitely; if genuine normalization occurs, long-run behavior drifts toward stable coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_acceptance_status, conceptual, 'Whether Jabotinsky''s promised ''honorable peace'' is a real terminus or rhetorical cover for permanent subordination.').

omega_variable(
    kernel_per_reading_epsilon,
    'This file instantiates ONE reading of the kernel jewish_territorial_claim; the colloquial kernel label covers four structurally distinct constraints with different epsilon values — is any kernel-level classification meaningful?',
    'Compile all four sibling stories (political, labor, cultural, revisionist readings) and compare per-seat classifications, epsilon values, and network edges across the family.',
    'Prevents false-summit naturalization of the kernel label (''historical destiny of the whole land'') that would launder four different extraction structures into one apparently natural law; all classification must remain per-reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_per_reading_epsilon, conceptual, 'Committer-structure omega: kernel label under-determines the constraint; epsilon is authored per reading only.').

omega_variable(
    east_bank_severance_bindingness,
    'Was the 1922 detachment of the east bank an administrative expedient whose reversal remained open, or a binding alteration of the National Home''s territory?',
    'Mandate-era archival record: Churchill White Paper drafting files, Permanent Mandates Commission minutes, and the legal status of the Transjordan memorandum.',
    'If severance was binding, the maximal claim''s east-bank component targeted an already-separate polity (revanchism against an existing population''s sovereignty); if reversible, the claim targeted administrable vacant-adjacent land. Changes the victim-set weighting and the effective extraction attributed to the maximalist element specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(east_bank_severance_bindingness, empirical, 'Legal status of the 1922 east-bank severance relative to the maximal claim.').

omega_variable(
    iron_wall_self_fulfilling_adversary,
    'Did the iron wall strategy manufacture the unified, implacable adversary it presupposed (a self-defeating prophecy), or did deterrence actually enable the later negotiated stabilization Jabotinsky claimed?',
    'Counterfactual comparison with the partition-track outcomes the Peel and Woodhead commissions sketched, plus post-1948 negotiation records showing what terms deterrence-facts made negotiable.',
    'Determines whether the suppression metric reflects a transient establishment cost (scaffold-like long-run behavior) or a self-perpetuating extraction infrastructure that regenerates its own opposition (snare-like long-run behavior).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(iron_wall_self_fulfilling_adversary, empirical, 'Whether the wall created the enemy it cited as justification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__revisionist_zionism_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(jewi_tr_t0, observed).
narrative_ontology:measurement(jewi_tr_t5, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 5, 0.27).
narrative_ontology:measurement_basis(jewi_tr_t5, observed).
narrative_ontology:measurement(jewi_tr_t10, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 10, 0.34).
narrative_ontology:measurement_basis(jewi_tr_t10, observed).
narrative_ontology:measurement(jewi_tr_t15, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 15, 0.29).
narrative_ontology:measurement_basis(jewi_tr_t15, observed).
narrative_ontology:measurement(jewi_tr_t20, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 20, 0.33).
narrative_ontology:measurement_basis(jewi_tr_t20, observed).
narrative_ontology:measurement(jewi_tr_t25, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 25, 0.32).
narrative_ontology:measurement_basis(jewi_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement_basis(jewi_be_t0, observed).
narrative_ontology:measurement(jewi_be_t5, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 5, 0.68).
narrative_ontology:measurement_basis(jewi_be_t5, observed).
narrative_ontology:measurement(jewi_be_t10, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 10, 0.74).
narrative_ontology:measurement_basis(jewi_be_t10, observed).
narrative_ontology:measurement(jewi_be_t15, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 15, 0.77).
narrative_ontology:measurement_basis(jewi_be_t15, observed).
narrative_ontology:measurement(jewi_be_t20, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 20, 0.82).
narrative_ontology:measurement_basis(jewi_be_t20, observed).
narrative_ontology:measurement(jewi_be_t25, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 25, 0.86).
narrative_ontology:measurement_basis(jewi_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement_basis(jewi_su_t0, observed).
narrative_ontology:measurement(jewi_su_t5, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 5, 0.73).
narrative_ontology:measurement_basis(jewi_su_t5, observed).
narrative_ontology:measurement(jewi_su_t10, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 10, 0.8).
narrative_ontology:measurement_basis(jewi_su_t10, observed).
narrative_ontology:measurement(jewi_su_t15, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 15, 0.76).
narrative_ontology:measurement_basis(jewi_su_t15, observed).
narrative_ontology:measurement(jewi_su_t20, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 20, 0.82).
narrative_ontology:measurement_basis(jewi_su_t20, observed).
narrative_ontology:measurement(jewi_su_t25, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 25, 0.84).
narrative_ontology:measurement_basis(jewi_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__revisionist_zionism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, jewish_territorial_claim__political_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, jewish_territorial_claim__labor_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, jewish_territorial_claim__cultural_zionism_reading).

% DUAL FORMULATION NOTE:
% Constraint family: 'the Jewish territorial claim' is one colloquial label covering four structurally distinct constraints (epsilon-invariance decomposition). The political_zionism_reading is the upstream member (highest empirical establishment, Weizmann-lineage diplomacy) from which the revisionist reading seceded organizationally in 1925 and rhetorically thereafter; this revisionist file creates downstream structural pressure on ALL three siblings — its force-first facts reset the legitimacy conditions under which partition proposals, gradualist settlement, and cultural-center schemes had to operate. Each sibling file must link back here. The epsilon spread across the family is wide: this reading's forced-maximalism sits far above the cultural reading's minimal-footprint center.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_territorial_claim__revisionist_zionism_reading, powerless, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
