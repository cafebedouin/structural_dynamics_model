% ============================================================================
% CONSTRAINT STORY: hebrew_continuity__bridge_pidginized
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_continuity__bridge_pidginized, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: hebrew_continuity__bridge_pidginized
 *   human_readable: Hebrew as Diaspora Contact/Bridge Language
 *   domain: sociolinguistics/religious/cultural
 *
 * SUMMARY:
 *   This story instantiates the bridge_pidginized reading of the
 *   hebrew_continuity kernel: Hebrew persists not as a fixed liturgical
 *   corpus and not as a fully native generative language, but as a functional
 *   contact register — high-register written correspondence and press
 *   alongside a simplified spoken/trade jargon — that lets dispersed,
 *   mutually unintelligible diaspora communities coordinate. This reading
 *   treats that instrumental persistence as continuity in its own right,
 *   structurally distinct from the liturgical_preservation reading (which
 *   locates continuity in preserved ritual recitation) and the
 *   native_generative reading (which requires daily generative native use).
 *   Both siblings, from their own premises, would deny this register counts
 *   as 'real' Hebrew — that denial is the expected structural delta and is
 *   documented here, not adjudicated by this story.
 *
 * KEY AGENTS:
 *   - diaspora_communal_organizations: primary beneficiary (organized/constrained) — coordinates via the bridge register
 *   - hebrew_press_and_correspondence_networks: beneficiary/agenda_setter (organized/constrained) — sets the de facto written standard
 *   - cross_community_traders_and_travelers: beneficiary/payer (moderate/mobile) — uses thin spoken jargon for trade
 *   - purist_grammarians: payer (moderate/identity_locked) — status cost as the pidginized form displaces classical standards
 *   - would_be_native_learners: payer (powerless/trapped) — starved of the dense native input full fluency would require
 *   - liturgical_authorities: excluded (institutional/analytical) — sacred-text framework has no seat here
 *   - native_revival_advocates: excluded (moderate/analytical) — ideological revival project sidelined by market-driven register
 *   - comparative_linguists: observer (analytical) — studies the phenomenon without adjudicating authenticity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__bridge_pidginized, 0.31).
domain_priors:suppression_score(hebrew_continuity__bridge_pidginized, 0.28).
domain_priors:theater_ratio(hebrew_continuity__bridge_pidginized, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, extractiveness, 0.31).
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__bridge_pidginized, rope).
narrative_ontology:human_readable(hebrew_continuity__bridge_pidginized, "Hebrew as Diaspora Contact/Bridge Language").
narrative_ontology:topic_domain(hebrew_continuity__bridge_pidginized, "sociolinguistics/religious/cultural").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__bridge_pidginized, '91d3d622-e297-4722-9572-ddbcd2a497fa').
narrative_ontology:cs_kernel_codification('91d3d622-e297-4722-9572-ddbcd2a497fa', distributed).
narrative_ontology:cs_authority_grounding('91d3d622-e297-4722-9572-ddbcd2a497fa', practice).
narrative_ontology:cs_interpretation_layer_present('91d3d622-e297-4722-9572-ddbcd2a497fa').
narrative_ontology:cs_reading_relation('91d3d622-e297-4722-9572-ddbcd2a497fa', hebrew_continuity__liturgical_preservation, coexists_with).
narrative_ontology:cs_reading_relation('91d3d622-e297-4722-9572-ddbcd2a497fa', hebrew_continuity__native_generative, influences).
narrative_ontology:cs_axiom('91d3d622-e297-4722-9572-ddbcd2a497fa', foundational, instrumental_use_constitutes_continuity).
narrative_ontology:cs_axiom_status(instrumental_use_constitutes_continuity, holdable).
narrative_ontology:cs_axiom_grounding('91d3d622-e297-4722-9572-ddbcd2a497fa', instrumental_use_constitutes_continuity, conventional).
narrative_ontology:cs_axiom('91d3d622-e297-4722-9572-ddbcd2a497fa', secondary, functional_register_need_not_be_generative_to_count).
narrative_ontology:cs_axiom_status(functional_register_need_not_be_generative_to_count, holdable).
narrative_ontology:cs_axiom_grounding('91d3d622-e297-4722-9572-ddbcd2a497fa', functional_register_need_not_be_generative_to_count, instrumental).
narrative_ontology:cs_reference_frame('91d3d622-e297-4722-9572-ddbcd2a497fa', diaspora_functional_bilingualism).
narrative_ontology:cs_drift_state('91d3d622-e297-4722-9572-ddbcd2a497fa', print_era_communal_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('91d3d622-e297-4722-9572-ddbcd2a497fa', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__bridge_pidginized, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__bridge_pidginized, diaspora_communal_organizations).
narrative_ontology:constraint_beneficiary(hebrew_continuity__bridge_pidginized, hebrew_press_and_correspondence_networks).
narrative_ontology:constraint_beneficiary(hebrew_continuity__bridge_pidginized, cross_community_traders_and_travelers).
narrative_ontology:constraint_victim(hebrew_continuity__bridge_pidginized, purist_grammarians).
narrative_ontology:constraint_victim(hebrew_continuity__bridge_pidginized, would_be_native_learners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(hebrew_continuity__bridge_pidginized, cross_community_traders_and_travelers).
narrative_ontology:constraint_vindicates(hebrew_continuity__bridge_pidginized, instrumental_continuity_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Uses Hebrew as the shared written and semi-spoken medium to coordinate between geographically dispersed, mutually unintelligible vernacular communities (Yiddish, Ladino, Judeo-Arabic speakers, etc.). Depends on a functional but simplified register that works across communities without requiring anyone to be a fluent native speaker.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, diaspora_communal_organizations, beneficiary,
    organized, generational, constrained, global).

% Produces newspapers, responsa, business correspondence, and communal records in high-register written Hebrew. Sets the de facto standard for what counts as usable Hebrew by what gets printed and circulated, favoring formulaic, learnable constructions over native idiomatic complexity.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, hebrew_press_and_correspondence_networks, beneficiary,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__bridge_pidginized, hebrew_press_and_correspondence_networks, agenda_setter).

% Uses a pidginized spoken Hebrew or Hebrew-inflected trade jargon to transact across linguistic boundaries where no other language is shared. Benefits from the low bar to functional competence but never achieves or needs full fluency; the register that serves them is thin by design.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, cross_community_traders_and_travelers, beneficiary,
    moderate, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__bridge_pidginized, cross_community_traders_and_travelers, payer).

% Scholars and maskilim invested in classical or Biblical Hebrew grammar watch the bridge register spread while regarding it as corrupted or degraded. They bear a status cost every time the pidginized form is what actually gets used, since their expertise in the 'correct' form has diminishing practical relevance.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, purist_grammarians, payer,
    moderate, civilizational, identity_locked, continental).

% Individuals or small circles attempting to raise Hebrew as a full native, generative language find the ambient linguistic environment offers only the thin bridge register as input — correspondence formulas and trade jargon, not the density of native discourse a child or adult learner would need to develop full generative competence.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, would_be_native_learners, payer,
    powerless, biographical, trapped, regional).

% Rabbinic and cantorial authorities who ground Hebrew's continuity in fixed liturgical recitation are not consulted by the bridge-register users, who treat Hebrew as an instrumental tool rather than a sacred textual inheritance. Their framework has no seat at the table where trade jargon and press Hebrew are negotiated.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, liturgical_authorities, excluded,
    institutional, civilizational, analytical, global).

% Advocates for full native generative Hebrew (the Ben-Yehuda-style project) regard the bridge/pidgin register as an obstacle to revival rather than a form of continuity, and are excluded from shaping it since it emerges from commercial and communal necessity rather than ideological planning.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, native_revival_advocates, excluded,
    moderate, generational, analytical, regional).

% Studies the bridge register as a linguistic phenomenon in its own right — a contact language with pidgin-like simplification features — without adjudicating whether it counts as 'real' Hebrew continuity.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, comparative_linguists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_continuity__bridge_pidginized, diffuse).
narrative_ontology:fixing_cost_class(hebrew_continuity__bridge_pidginized, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared written and minimally-spoken medium that lets mutually unintelligible diaspora Jewish communities (Yiddish, Ladino, Judeo-Arabic, and other vernacular speakers) correspond, trade, and coordinate communal affairs without any party needing to learn another's vernacular.
% TRANSFER_FUNCTION: Moves communicative access and communal coordination capacity toward organizations and networks that can operate the bridge register, while moving status and practical relevance away from purists who hold the classical/liturgical standard and away from individuals trying to build full native competence in an environment that only supplies thin, formulaic input.
% ABSENT_VOICES: Liturgical authorities and native-revival advocates are structurally absent from the negotiation of what this register looks like — it is shaped by commercial correspondence and marketplace necessity, not by either sacred-text custodians or revival ideologues, and both would object that what results is not 'really' Hebrew by their own standards.
% DISAPPEARANCE_RATIONALE: If the bridge register vanished, diaspora communities lacking a shared vernacular would lose a working medium for cross-community correspondence, trade, and communal administration; either another lingua franca (a colonial language, or renewed reliance on Yiddish/Ladino as intermediaries) would have to fill the gap, or cross-community coordination would fragment along vernacular lines.
% FOUNDING_PROBLEM: Geographically dispersed Jewish communities speaking mutually unintelligible vernaculars needed a common medium for trade, correspondence, and communal administration, and full native fluency in classical Hebrew was neither available nor necessary for that narrower purpose.
% FOUNDING_PROBLEM_CORROBORATION: Comparative linguists studying contact-language formation attest that pidginized bridge registers reliably emerge wherever this coordination problem exists, independent of any single community's self-description; purist grammarians and native-revival advocates, both benefiting from a different account of Hebrew continuity, dispute that this register constitutes genuine continuity at all rather than a functional workaround.
narrative_ontology:disappearance_verdict(hebrew_continuity__bridge_pidginized, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_continuity__bridge_pidginized, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__bridge_pidginized, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_continuity__bridge_pidginized, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_continuity__bridge_pidginized, 0.31, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_continuity__bridge_pidginized_tests).
:- end_tests(hebrew_continuity__bridge_pidginized_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate-low (0.31 at interval end) because the bridge register genuinely solves a coordination problem — dispersed communities need SOME shared medium, and the register provides one at low acquisition cost. It is not zero because the register's dominance imposes a real cost on purist grammarians (status/relevance erosion) and would-be native learners (input starvation) without those parties receiving compensating benefit. Suppression is low-moderate (0.28): nobody is coerced into using the bridge register; it wins by being the cheapest available solution, not by blocking alternatives. Theater ratio rises over the interval (0.25→0.42) as more institutional writing in the bridge register performs 'Hebrew competence' for communal legitimacy purposes even as actual generative depth stays thin — the press and correspondence networks increasingly produce Hebrew as a signal of continuity rather than as living discourse.
 *
 * PERSPECTIVAL GAP:
 *   From the diaspora organizations' and press networks' seats, this is straightforward rope: a low-cost, genuinely functional coordination solution nobody is forced into. From the would-be native learners' seat, the same arrangement looks closer to a trap — the only Hebrew environment available to them is too thin to build real fluency from, foreclosing generative revival without any single actor intending that outcome. The engine should register these as genuinely different computed experiences of one structure, not as a claim/metric error.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (communal organizations, press networks, traders) get low d because the register subsidizes their actual coordination needs at minimal cost. Victims (purist grammarians, would-be native learners) get higher d: grammarians lose relevance and status as a slow, diffuse cost; native learners are structurally trapped by an input environment that cannot support what they are trying to build, which is a harder, more identity-constitutive cost than the grammarians bear.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — cross-community coordination absent a shared vernacular — remains partially live wherever diaspora communities still lack a common language, but the register has also taken on independent symbolic weight (continuity theater) beyond its coordination function, which the rising theater_ratio is intended to capture. This prevents both over-reading the register as pure obsolete inertia and under-reading it as pure functional necessity: it is doing real coordination work AND increasingly performing 'Hebrew survives' for communities that need that narrative independent of the register's generative thinness.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bridge_register_as_genuine_continuity_or_erosion,
    'Does a functional, non-native, non-liturgical bridge register constitute genuine language continuity, or is it a symptom of Hebrew''s erosion as a living language dressed up as survival?',
    'Longitudinal tracking of whether communities using primarily the bridge register ever transition toward denser native or liturgical engagement, versus whether the bridge register is a terminal stable state that eventually loses even its instrumental function as other lingua francas (colonial languages, English) displace it.',
    'If the bridge register is a stable, self-sustaining continuity mechanism, this reading is a genuine rope alongside its siblings. If it is transitional erosion masking as continuity, the classification shifts toward piton — a vestigial function maintained mostly for symbolic/theatrical reasons as the rising theater_ratio suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bridge_register_as_genuine_continuity_or_erosion, conceptual, 'Whether instrumental bridge use is continuity or camouflaged decline.').

omega_variable(
    kernel_occupation_exclusivity,
    'Can more than one reading of the hebrew_continuity kernel be simultaneously true for the same historical population, or does adopting the bridge-register account require denying that liturgical recitation or native generative pockets are ALSO occurring concurrently?',
    'Ethnographic and historical mapping of specific diaspora communities to determine whether liturgical, bridge, and native-generative Hebrew use coexist in different social domains (synagogue vs. marketplace vs. isolated revival households) for the same individuals.',
    'If domains are cleanly separable (liturgical in synagogue, bridge in marketplace, native in rare households), all three readings coexist without contradiction, supporting coexists_with relations. If the bridge register''s marketplace dominance actively displaces the input needed for native-generative pockets to form, this reading exerts real downstream pressure on the native_generative reading''s viability, supporting an influences relation rather than mere coexistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_occupation_exclusivity, conceptual, 'Whether the three kernel readings describe separable domains or a zero-sum competition for the same linguistic space.').

omega_variable(
    purist_status_cost_measurement,
    'Is the status/relevance cost borne by purist grammarians a real structural extraction, or an artifact of comparing them to a counterfactual (full classical dominance) that never actually obtained in the diaspora?',
    'Historical analysis of whether purist grammarians ever held the practical relevance this story implies they are losing, or whether the bridge register has always coexisted alongside (rather than displaced) their classical domain.',
    'If purists never held the practical relevance being measured as lost, the victim classification for purist_grammarians should be weakened or removed, lowering measured extraction further toward a pure rope reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(purist_status_cost_measurement, empirical, 'Whether purist grammarians'' loss is a real cost or a manufactured counterfactual.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__bridge_pidginized, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_continuity__bridge_pidginized, theater_ratio, 0, 0.25).
narrative_ontology:measurement(hebr_tr_t8, hebrew_continuity__bridge_pidginized, theater_ratio, 8, 0.29).
narrative_ontology:measurement(hebr_tr_t16, hebrew_continuity__bridge_pidginized, theater_ratio, 16, 0.33).
narrative_ontology:measurement(hebr_tr_t24, hebrew_continuity__bridge_pidginized, theater_ratio, 24, 0.36).
narrative_ontology:measurement(hebr_tr_t32, hebrew_continuity__bridge_pidginized, theater_ratio, 32, 0.39).
narrative_ontology:measurement(hebr_tr_t40, hebrew_continuity__bridge_pidginized, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_continuity__bridge_pidginized, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(hebr_be_t8, hebrew_continuity__bridge_pidginized, base_extractiveness, 8, 0.24).
narrative_ontology:measurement(hebr_be_t16, hebrew_continuity__bridge_pidginized, base_extractiveness, 16, 0.27).
narrative_ontology:measurement(hebr_be_t24, hebrew_continuity__bridge_pidginized, base_extractiveness, 24, 0.29).
narrative_ontology:measurement(hebr_be_t32, hebrew_continuity__bridge_pidginized, base_extractiveness, 32, 0.3).
narrative_ontology:measurement(hebr_be_t40, hebrew_continuity__bridge_pidginized, base_extractiveness, 40, 0.31).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(hebrew_continuity__bridge_pidginized, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__bridge_pidginized, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_continuity__bridge_pidginized, 0.1).
narrative_ontology:affects_constraint(hebrew_continuity__bridge_pidginized, hebrew_continuity__liturgical_preservation).
narrative_ontology:affects_constraint(hebrew_continuity__bridge_pidginized, hebrew_continuity__native_generative).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the hebrew_continuity kernel. hebrew_continuity__liturgical_preservation grounds continuity in fixed ritual recitation and textual transmission (a codification-heavy, low-extraction mountain-adjacent reading). hebrew_continuity__native_generative grounds continuity in daily generative native use (a demanding, sparse-population reading with different victim structure — non-native speakers rather than purists). This reading (bridge_pidginized) grounds continuity in instrumental cross-community utility, with its own ε (0.31), its own beneficiary/victim set, and a distinct claimed type. Each story's ε is stable under its own reading and is NOT to be averaged or reconciled across the three; the kernel-level disagreement about what counts as 'real Hebrew' is carried by the reading_relations and axioms in each file's cs_structure, not by adjusting any single file's metrics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
