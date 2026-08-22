% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__revisionist_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: jewish_territorial_claim__revisionist_zionism_reading
 *   human_readable: Revisionist Zionist Maximalist Territorial Claim with Iron Wall Enforcement
 *   domain: political_history/settler_colonialism/nationalism_studies
 *
 * SUMMARY:
 *   The arrangement under contest is the revisionist Zionist program as a
 *   standing structure: an indivisible claim to both banks of the Jordan,
 *   held to be non-negotiable, to be realized immediately rather than
 *   incrementally, and to be secured by an 'Iron Wall' — overwhelming
 *   military force deployed in the expectation that Arab society, having
 *   failed to destroy it, will eventually accommodate itself to it. The
 *   claim's own doctrine openly declares Arab consent unnecessary and
 *   unobtainable, making the people whose sovereignty is transferred
 *   structurally external to the decision that disposes of it. Epsilon's
 *   referent is this maximalist claim-plus-enforcement arrangement itself,
 *   assessed by the reading's own lights: the doctrine does not deny the
 *   imposition on the Arab population — it asserts the imposition as tragic
 *   necessity, which keeps the referent stable and the epsilon
 *   reading-indexed rather than hedged across alternative programs.
 *
 * KEY AGENTS:
 *   - revisionist_zionist_movement: agenda-setting beneficiary (organized/identity_locked) — sets the claim's terms, builds its enforcement capacity, and collects its realization
 *   - palestinian_arab_communities: primary target (powerless/trapped) — bears displacement, repression, and denial of any veto over their governance
 *   - transjordanian_arab_society: secondary target (moderate/trapped) — territory claimed outright though settlement east of the river is blocked
 *   - british_mandate_administration: institutional administrator (institutional/arbitrage) — controls the legal terrain, bears enforcement costs, holds the exit of withdrawal
 *   - league_mandates_commission: analytical observer (institutional/analytical) — records petitions and protests, decides nothing
 *   - diaspora_jewish_communities: diffuse beneficiary (organized/mobile) — funds and populates the project, receives the refuge promise, support remains redirectable
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__revisionist_zionism_reading, 0.66).
domain_priors:suppression_score(jewish_territorial_claim__revisionist_zionism_reading, 0.82).
domain_priors:theater_ratio(jewish_territorial_claim__revisionist_zionism_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__revisionist_zionism_reading, tangled_rope).
narrative_ontology:human_readable(jewish_territorial_claim__revisionist_zionism_reading, "Revisionist Zionist Maximalist Territorial Claim with Iron Wall Enforcement").
narrative_ontology:topic_domain(jewish_territorial_claim__revisionist_zionism_reading, "political_history/settler_colonialism/nationalism_studies").

domain_priors:requires_active_enforcement(jewish_territorial_claim__revisionist_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__revisionist_zionism_reading, 'ed9715e2-5ff6-4d90-abd4-d237688ba52c').
narrative_ontology:cs_kernel_codification('ed9715e2-5ff6-4d90-abd4-d237688ba52c', formalized).
narrative_ontology:cs_authority_grounding('ed9715e2-5ff6-4d90-abd4-d237688ba52c', lineage).
narrative_ontology:cs_interpretation_layer_present('ed9715e2-5ff6-4d90-abd4-d237688ba52c').
narrative_ontology:cs_reading_relation('ed9715e2-5ff6-4d90-abd4-d237688ba52c', jewish_territorial_claim__political_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('ed9715e2-5ff6-4d90-abd4-d237688ba52c', jewish_territorial_claim__labor_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('ed9715e2-5ff6-4d90-abd4-d237688ba52c', jewish_territorial_claim__cultural_zionism_reading, forecloses).
narrative_ontology:cs_axiom('ed9715e2-5ff6-4d90-abd4-d237688ba52c', foundational, arab_consent_not_prerequisite).
narrative_ontology:cs_axiom_status(arab_consent_not_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('ed9715e2-5ff6-4d90-abd4-d237688ba52c', arab_consent_not_prerequisite, instrumental).
narrative_ontology:cs_axiom('ed9715e2-5ff6-4d90-abd4-d237688ba52c', foundational, territorial_wholeness_non_negotiable).
narrative_ontology:cs_axiom_status(territorial_wholeness_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('ed9715e2-5ff6-4d90-abd4-d237688ba52c', territorial_wholeness_non_negotiable, deontological).
narrative_ontology:cs_axiom('ed9715e2-5ff6-4d90-abd4-d237688ba52c', secondary, iron_wall_compels_acceptance).
narrative_ontology:cs_axiom_status(iron_wall_compels_acceptance, holdable).
narrative_ontology:cs_axiom_grounding('ed9715e2-5ff6-4d90-abd4-d237688ba52c', iron_wall_compels_acceptance, empirically_contingent).
narrative_ontology:cs_reference_frame('ed9715e2-5ff6-4d90-abd4-d237688ba52c', indivisible_both_banks_national_home).
narrative_ontology:cs_drift_state('ed9715e2-5ff6-4d90-abd4-d237688ba52c', post_1949_armistice_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ed9715e2-5ff6-4d90-abd4-d237688ba52c', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__revisionist_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__revisionist_zionism_reading, revisionist_zionist_movement).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__revisionist_zionism_reading, diaspora_jewish_communities).
narrative_ontology:constraint_victim(jewish_territorial_claim__revisionist_zionism_reading, palestinian_arab_communities).
narrative_ontology:constraint_victim(jewish_territorial_claim__revisionist_zionism_reading, transjordanian_arab_society).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__revisionist_zionism_reading, iron_wall_doctrine).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__revisionist_zionism_reading, integral_land_of_israel_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the doctrine: the whole of the historic territory on both banks of the Jordan under immediate Jewish sovereignty, won and kept by overwhelming force because Arab agreement will never come voluntarily. Builds the youth movement, the paramilitary cadres, and the political party that carry the doctrine. Collects organizational growth, prestige, and the prospect of the enlarged state. Abandoning the doctrine would dissolve the movement's reason to exist — its distinctness from the Zionist mainstream is the doctrine itself.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, revisionist_zionist_movement, agenda_setter,
    organized, generational, identity_locked, global).

% Live on the western portion of the claimed territory. The doctrine assigns their land and political future to another nation without consulting them; they bear the aftermath of riots, the suppression of the revolt, wartime flight, and the loss of any veto over their own governance. Leaving means abandoning villages and livelihoods for neighboring lands that do not want them.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, palestinian_arab_communities, payer,
    powerless, generational, trapped, regional).

% Lives east of the Jordan under the Emirate established when Britain separated that bank in 1921. The doctrine declares their state provisional and their territory promised to another nation. Jewish settlement east of the river is blocked for now, but the claim hangs permanently over their sovereignty, and a settled society cannot relocate itself elsewhere.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, transjordanian_arab_society, payer,
    moderate, generational, trapped, regional).

% Administers the Mandate and controls immigration, land sales, and the border between the two banks. Separated Transjordan in 1921 over the movement's protest, restricted immigration under the White Papers, and bore the military cost of suppressing the Arab Revolt. Its officials disagree internally and its policy oscillates; it retains the option of walking away entirely, which it exercises in 1948, handing the unresolved claim back to the parties themselves.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, british_mandate_administration, agenda_setter,
    institutional, biographical, arbitrage, global).

% Receives petitions and protests, including the movement's objections to the Transjordan separation and to the immigration restrictions. Records arguments, questions officials, and reports to the Council; it decides nothing and enforces nothing.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, league_mandates_commission, observer,
    institutional, biographical, analytical, continental).

% Fund the movement through dues and donations and supply its cadres and immigrants. They receive in return the promise of refuge and national dignity that the doctrine projects. Their support is real but redirectable — rival movements and private lives remain available — so their attachment is chosen rather than compelled.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, diaspora_jewish_communities, beneficiary,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_territorial_claim__revisionist_zionism_reading, revisionist_zionist_movement).
narrative_ontology:fixing_cost_class(jewish_territorial_claim__revisionist_zionism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates dispersed Jewish national effort around a single indivisible territorial objective and a unified theory of how it will be secured — strength rather than consent — resolving the movement's internal disputes about borders and method by doctrinal fiat.
% TRANSFER_FUNCTION: Moves sovereignty, land, and self-determination from the Arab inhabitants of both banks toward the Jewish national project; moves diaspora money and manpower into the movement; moves the cost of securing acceptance onto military organization and onto the Arab population's freedom of political action.
% ABSENT_VOICES: The Arab inhabitants whose sovereignty the claim transfers are structurally absent from the decision procedure — the reading's defining move is declaring their consent unnecessary and unobtainable, so the primary payers never sit at the table. Moderate Arab voices open to negotiated minority arrangements are equally excluded, since the doctrine holds that no Arab offer can ever be adequate.
% DISAPPEARANCE_RATIONALE: If the maximalist claim and its enforcement doctrine vanished overnight, the movement's institutional identity would collapse into the Zionist mainstream, settlement and military planning would lose their maximalist orientation, and the Arab societies of both banks would face a radically altered threat environment — the region's conflict architecture is arranged around this claim's presence.
% FOUNDING_PROBLEM: Jewish statelessness and lethal persecution in Europe, joined to the conviction that only sovereign territory can guarantee safety — and, specific to this reading, the further conviction that gradualism and consent-seeking would fail before demographic and diplomatic windows closed.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem itself is corroborated extensively from outside the benefiting parties: government records, contemporaneous journalism, and refugee documentation establish the reality of European persecution and statelessness. That THIS reading's solution — immediate maximal sovereignty compelled by force — was necessary is attested only by the movement itself; Arab testimony and British diplomatic correspondence attest the imposition's reality while denying its necessity. Corroboration exists for the problem, not for the reading's remedy.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__revisionist_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__revisionist_zionism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__revisionist_zionism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_territorial_claim__revisionist_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__revisionist_zionism_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness sits at 0.66 at interval end: the arrangement transfers land, sovereignty, and self-determination from one population to another without consent or compensation, and the doctrine itself concedes no adequate compensation is possible. Suppression is higher still at 0.82 — a raw, unscaled structural property — because persistence depends on the credible availability of overwhelming force against the governed population, not on participant preference; the suppression_requirement series traces the deliberate build-up of that enforcement machinery (youth corps, paramilitary cadres, then a standing army), which is precisely the dynamic this story tracks, hence its dedicated series. Theater rises from 0.15 to 0.48: early in the interval the both-banks claim was operationally serious, but after Transjordan's separation and independence the east-bank component increasingly survived as song, letterhead, and rally rhetoric while real activity concentrated west of the river — approaching but not crossing the 0.5 proxy-substitution line, because the west-bank core remained fully functional. Accessibility_collapse is 0.60: partition, consent-based gradualism, and minority-rights arrangements remained visibly on the table throughout the period, yet within the doctrine's own logic they collapse completely once its two premises (consent unobtainable, wholeness obligatory) are accepted. Resistance is 0.75 — riots, the 1936-39 revolt, British restriction, and sustained opposition inside the Zionist camp itself. Coalition potential among the powerless victims was real but repeatedly failed: communal fragmentation and elite deal-seeking broke coordinated Arab action at critical junctures, which is why trapped exit did not convert into effective class-level leverage. Identity-lock on the agenda-setting seat is ideological and institutional at once: maximalism is what distinguishes the movement from the mainstream, so softening the claim would not revise the movement but dissolve it; if that frame broke, the seat's exit options would shift from locked to constrained and the claim's non-negotiability would become a bargaining position rather than an identity. All three metric series run on one shared grid (points 0, 8, 16, 24, 32, 40, mapping roughly to 1923-1963) so no row borrows an end-state value from another series.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergently. From the movement's position the arrangement is tragic necessity — national rescue that regretfully cannot wait for agreement — and the Iron Wall is the price of existence, not plunder. From the two Arab seats the identical structure is dispossession administered by force, with the doctrine's candor about non-consent reading as confession rather than justification. The British seat experiences it as an unpayable administrative dilemma: enforcing one party's claim generates revolt, restricting it generates insurgency by the other. The engine derives these per-seat classifications from power, exit, and directional position; the authored claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   The movement sits near the beneficiary pole: it writes the rules and collects the claim's realization, and its identity-lock amplifies rather than dampens its investment. Diaspora communities also derive low directionality — genuine recipients of the refuge promise — but their mobile exit damps their exposure relative to the movement's. The two Arab populations sit near the full-target pole, and their trapped exit places them at the extreme end of effective extraction: they cannot arbitrage, relocate, or reframe their way out of the claim's reach. The British administration derives a mid-range directionality — it neither collects the claim's gains nor bears its deepest costs, administering the terrain while holding arbitrage-grade exit. No override was needed: beneficiary/victim declarations plus exit options reproduce these relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — statelessness amid lethal persecution — was and remains live, so the mismatch consumer finds status=live paired with verdict=world_rearranges: no dead-mandate zombie flag fires, correctly, because the arrangement still organizes real settlements and real conflict. But the story carries a partial mandatrophy inside it: the east-bank component's operational function died with Transjordan's separation and independence, while the component persists declaratively — visible as the rising theater trajectory rather than as a formal sunset. Classifying the whole as tangled_rope guards against both standard mislabelings: calling it a snare would erase the genuine coordination the arrangement performs for Jewish national life (a real collective-action solution to dispersion and statelessness that sibling readings pursue by other means), while calling it a rope would erase the coerced population whose consent the doctrine explicitly waives. The hybrid label is the honest one: someone is coordinated and someone pays, through the same structure, held together by active force.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is one reading of the kernel jewish_territorial_claim (instantiating revisionist_zionism_reading). Which structural features — rejection of Arab consent as prerequisite, force-first mechanism, both-banks maximalism — belong to this reading specifically rather than to the kernel itself?',
    'Side-by-side comparison with the sibling reading files (political_zionism_reading, labor_zionism_reading, cultural_zionism_reading): features present across all four are kernel-level; features unique to this file are reading-level deltas.',
    'If consent-rejection and force-first are reading-specific, the heavy suppression and extraction load attaches to this reading alone and the kernel can coordinate benignly under sibling readings; if they are kernel-level, every reading inherits the extraction profile.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Separating reading-specific structure from kernel-level structure in the territorial-claim family.').

omega_variable(
    iron_wall_conversion_hypothesis,
    'Does overwhelming military force actually convert Arab rejection into durable acceptance, as the Iron Wall doctrine predicts, or does it entrench permanent resistance?',
    'Longitudinal analysis of Arab political behavior following decisive displays of force versus periods of restraint across the interval.',
    'If force entrenches resistance, the arrangement''s own justification fails and its suppression becomes permanent maintenance cost, pushing the computed classification toward snare; if conversion occurs, part of the measured suppression is transitional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(iron_wall_conversion_hypothesis, empirical, 'Empirical testability of the Iron Wall''s core prediction about compelled acceptance.').

omega_variable(
    east_bank_claim_operationality,
    'After Transjordan''s separation from the Mandate (1921) and its independence (1946), is the east-bank component of the claim an operative objective receiving real settlement and military resources, or a declaratory inheritance maintained ritually?',
    'Audit of movement resource allocation, settlement attempts, and paramilitary planning directed east of the Jordan after 1946, versus purely ceremonial invocations.',
    'A high ritual share would date piton-drift onset for the east-bank component specifically and push the story''s effective theater ratio above 0.5 in later intervals.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(east_bank_claim_operationality, empirical, 'Operational versus performative status of the Transjordan claim after 1946.').

omega_variable(
    historic_title_naturalness,
    'Is the territorial claim experienced by its holders as restoration of an intrinsic historic title (felt as immovable regardless of who defends it), or as a modern nationalist construction among possible allocations of the territory?',
    'Comparative historiography of how the claim''s scope was argued across decades and across the four readings; whether the movement treated borders as negotiable under changed conditions.',
    'If constructed, the claim is fully negotiable and its non-negotiability is a strategic choice, supporting the tangled_rope/snare range; if held as intrinsic title, participants experience it as immovable, raising accessibility_collapse toward mountain-like profiles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historic_title_naturalness, conceptual, 'Constructed political claim versus experienced natural title.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__revisionist_zionism_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jtcrzr_tr_t0, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(jtcrzr_tr_t0, observed).
narrative_ontology:measurement(jtcrzr_tr_t8, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement_basis(jtcrzr_tr_t8, observed).
narrative_ontology:measurement(jtcrzr_tr_t16, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 16, 0.28).
narrative_ontology:measurement_basis(jtcrzr_tr_t16, observed).
narrative_ontology:measurement(jtcrzr_tr_t24, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement_basis(jtcrzr_tr_t24, observed).
narrative_ontology:measurement(jtcrzr_tr_t32, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 32, 0.42).
narrative_ontology:measurement_basis(jtcrzr_tr_t32, observed).
narrative_ontology:measurement(jtcrzr_tr_t40, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 40, 0.48).
narrative_ontology:measurement_basis(jtcrzr_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(jtcrzr_be_t0, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement_basis(jtcrzr_be_t0, observed).
narrative_ontology:measurement(jtcrzr_be_t8, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 8, 0.64).
narrative_ontology:measurement_basis(jtcrzr_be_t8, observed).
narrative_ontology:measurement(jtcrzr_be_t16, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 16, 0.7).
narrative_ontology:measurement_basis(jtcrzr_be_t16, observed).
narrative_ontology:measurement(jtcrzr_be_t24, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 24, 0.74).
narrative_ontology:measurement_basis(jtcrzr_be_t24, observed).
narrative_ontology:measurement(jtcrzr_be_t32, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 32, 0.7).
narrative_ontology:measurement_basis(jtcrzr_be_t32, observed).
narrative_ontology:measurement(jtcrzr_be_t40, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 40, 0.66).
narrative_ontology:measurement_basis(jtcrzr_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(jtcrzr_su_t0, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(jtcrzr_su_t0, observed).
narrative_ontology:measurement(jtcrzr_su_t8, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement_basis(jtcrzr_su_t8, observed).
narrative_ontology:measurement(jtcrzr_su_t16, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 16, 0.7).
narrative_ontology:measurement_basis(jtcrzr_su_t16, observed).
narrative_ontology:measurement(jtcrzr_su_t24, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 24, 0.78).
narrative_ontology:measurement_basis(jtcrzr_su_t24, observed).
narrative_ontology:measurement(jtcrzr_su_t32, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 32, 0.8).
narrative_ontology:measurement_basis(jtcrzr_su_t32, observed).
narrative_ontology:measurement(jtcrzr_su_t40, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 40, 0.82).
narrative_ontology:measurement_basis(jtcrzr_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__revisionist_zionism_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, political_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, labor_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, cultural_zionism_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the Jewish territorial claim' covers four structurally distinct arrangements that differ on consent, mechanism, scope, and timing. Per the epsilon-invariance principle each reading is authored as its own story with its own epsilon, beneficiary structure, and classification; this file carries the revisionist reading. The upstream readings (cultural, political) historically supplied the legitimacy vocabulary — historic title, national home — that this reading radicalizes into a non-negotiable maximal claim enforced by force, so contamination propagates downstream: erosion of the shared title premise degrades this reading faster than it degrades the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
