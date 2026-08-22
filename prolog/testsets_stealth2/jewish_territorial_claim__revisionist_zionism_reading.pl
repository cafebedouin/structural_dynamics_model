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
 *   human_readable: Revisionist Zionist Territorial Maximalism and the Iron Wall Doctrine
 *   domain: political_history/settler_colonialism/nationalism_studies
 *
 * SUMMARY:
 *   In 1923 Ze'ev Jabotinsky published the Iron Wall essays: Arab national
 *   feeling made voluntary consent to a Jewish state impossible, therefore
 *   only an unbreachable wall of Jewish military force, erected before any
 *   negotiation, could ever make the state possible. The doctrine hardened
 *   into a program — the 1935 Jerusalem Program of the New Zionist
 *   Organization demanded a Jewish state on both banks of the Jordan with
 *   immediate sovereignty — and into an apparatus: the uniformed Betar youth
 *   movement, the Irgun's 1937 break from the Haganah, and the armed
 *   confrontation of 1944-1948. The claim/metric gap is deliberate and
 *   load-bearing: the constraint is CLAIMED as tangled_rope (genuine
 *   coordination of a persecuted national movement riding on asymmetric
 *   extraction from the majority population), while the authored metrics
 *   describe heavily extractive, escalatingly coercive operation — the engine
 *   measures the divergence per seat; the claim is not reconciled to the
 *   metrics. Per the epsilon-referent rule for kernel readings, epsilon is
 *   authored for the standing arrangement under contest — the Revisionist
 *   program itself (claim plus Iron Wall) as it actually operated — assessed
 *   by this reading's own lights, never for the arrangements sibling readings
 *   endorse. KEY AGENTS (by structural relationship): -
 *   revisionist_zionist_leadership: agenda-setting seat
 *   (organized/identity_locked) — authors the claim, commands the movement -
 *   betar_rank_and_file: primary beneficiary (moderate/identity_locked) —
 *   receives purpose, discipline, promised statehood -
 *   palestinian_arab_residents: primary target (organized/trapped) — bears
 *   the transfer of land and self-determination -
 *   transjordanian_arab_communities: secondary target (powerless/trapped) —
 *   east-bank populations reattached to the claim without consultation -
 *   britain_mandate_administration: external authority
 *   (institutional/arbitrage) — administers the field, severs and restricts,
 *   exits in 1948 - arab_neighboring_states: excluded adversaries
 *   (institutional/constrained) - labor_zionist_leadership: excluded
 *   intra-movement rival (institutional/constrained) -
 *   binationalist_advocates: excluded dissenters (moderate/constrained) -
 *   historians_of_the_conflict: analytical observer (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__revisionist_zionism_reading, 0.86).
domain_priors:suppression_score(jewish_territorial_claim__revisionist_zionism_reading, 0.9).
domain_priors:theater_ratio(jewish_territorial_claim__revisionist_zionism_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, extractiveness, 0.86).
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__revisionist_zionism_reading, tangled_rope).
narrative_ontology:human_readable(jewish_territorial_claim__revisionist_zionism_reading, "Revisionist Zionist Territorial Maximalism and the Iron Wall Doctrine").
narrative_ontology:topic_domain(jewish_territorial_claim__revisionist_zionism_reading, "political_history/settler_colonialism/nationalism_studies").

domain_priors:requires_active_enforcement(jewish_territorial_claim__revisionist_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__revisionist_zionism_reading, '9d39a2c6-5cad-4bea-8fc3-b4e4648a56d6').
narrative_ontology:cs_kernel_codification('9d39a2c6-5cad-4bea-8fc3-b4e4648a56d6', formalized).
narrative_ontology:cs_authority_grounding('9d39a2c6-5cad-4bea-8fc3-b4e4648a56d6', lineage).
narrative_ontology:cs_interpretation_layer_present('9d39a2c6-5cad-4bea-8fc3-b4e4648a56d6').
narrative_ontology:cs_reading_relation('9d39a2c6-5cad-4bea-8fc3-b4e4648a56d6', jewish_territorial_claim__political_zionism_reading, influences).
narrative_ontology:cs_reading_relation('9d39a2c6-5cad-4bea-8fc3-b4e4648a56d6', jewish_territorial_claim__labor_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('9d39a2c6-5cad-4bea-8fc3-b4e4648a56d6', jewish_territorial_claim__cultural_zionism_reading, coexists_with).
narrative_ontology:cs_axiom('9d39a2c6-5cad-4bea-8fc3-b4e4648a56d6', foundational, arab_consent_not_prerequisite).
narrative_ontology:cs_axiom_status(arab_consent_not_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('9d39a2c6-5cad-4bea-8fc3-b4e4648a56d6', arab_consent_not_prerequisite, instrumental).
narrative_ontology:cs_axiom('9d39a2c6-5cad-4bea-8fc3-b4e4648a56d6', foundational, integral_mandate_borders_inviolable).
narrative_ontology:cs_axiom_status(integral_mandate_borders_inviolable, holdable).
narrative_ontology:cs_axiom_grounding('9d39a2c6-5cad-4bea-8fc3-b4e4648a56d6', integral_mandate_borders_inviolable, conventional).
narrative_ontology:cs_reference_frame('9d39a2c6-5cad-4bea-8fc3-b4e4648a56d6', integral_mandate_herzlian_statehood).
narrative_ontology:cs_drift_state('9d39a2c6-5cad-4bea-8fc3-b4e4648a56d6', end_of_mandate_1948, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9d39a2c6-5cad-4bea-8fc3-b4e4648a56d6', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__revisionist_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__revisionist_zionism_reading, revisionist_zionist_leadership).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__revisionist_zionism_reading, betar_rank_and_file).
narrative_ontology:constraint_victim(jewish_territorial_claim__revisionist_zionism_reading, palestinian_arab_residents).
narrative_ontology:constraint_victim(jewish_territorial_claim__revisionist_zionism_reading, transjordanian_arab_communities).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__revisionist_zionism_reading, iron_wall_doctrine).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__revisionist_zionism_reading, territorial_maximalism).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__revisionist_zionism_reading, jewish_monist_nationalism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Publishes and enforces the movement's fixed program: a Jewish state on both banks of the Jordan, sovereignty now, no Arab veto. Runs the party press, the New Zionist Organization, and the youth movement, and after 1937 sanctions the Irgun's separation from the Haganah. Its authority rests on presenting itself as heir to Herzl's original maximalism; abandoning either bank or the force-first sequence would dissolve the movement's distinct reason for existing.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, revisionist_zionist_leadership, agenda_setter,
    organized, generational, identity_locked, global).

% Tens of thousands of uniformed youth, concentrated in Poland, drilled in Hebrew, discipline, and the territorial program. They receive belonging, purpose, and the promise of citizenship in the coming state; their daily life, friendships, and self-concept are organized around the movement, so leaving means leaving a community and an identity, not just a policy position.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, betar_rank_and_file, beneficiary,
    moderate, biographical, identity_locked, continental).

% The majority population west of the Jordan. Their villages, orchards, and towns sit on the land the program transfers, and their political future is decided in forums they do not attend. They organize a general strike and armed revolt (1936-1939), suffer suppression and mass detention, and have nowhere to go: neighboring doors are largely closed and their lives are rooted in the contested ground.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, palestinian_arab_residents, payer,
    organized, generational, trapped, national).

% Inhabitants of the east bank, administratively separated from the Jewish national home provisions in 1922 and ruled through the Amman emirate. The program reattaches them to it without consulting them; they appear in the movement's literature as a border correction awaiting restoration, not as a population with a say.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, transjordanian_arab_communities, payer,
    powerless, generational, trapped, regional).

% Governments in Cairo, Baghdad, Damascus, Amman, and Riyadh whose publics overwhelmingly reject the program. The doctrine assigns them no consenting role and plans around fighting them if the wall is tested; they enter the story as adversaries or as parties to armistice, never as principals whose agreement is sought.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, arab_neighboring_states, excluded,
    institutional, generational, constrained, continental).

% Holds the League Mandate and writes the rules of the field: severs the east bank in 1922, proposes partition in 1937, caps immigration and land sales in 1939. The movement lobbies it to restore the integral borders and reads each restriction as betrayal; it finally withdraws in 1948, handing the contest to the parties on the ground.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, britain_mandate_administration, observer,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__revisionist_zionism_reading, britain_mandate_administration, agenda_setter).

% Leads the rival mainstream: builds the yishuv through kibbutzim, the Histadrut, and incremental institutions, and pursues British goodwill and eventually partition. Shares the goal of a Jewish commonwealth but rejects both the both-banks scope and the force-first sequence; within this program's frame they are capitulators to be outflanked, not partners to persuade.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, labor_zionist_leadership, excluded,
    institutional, generational, constrained, national).

% A small intellectual circle (Magnes, Buber, and associates) proposing a binational state with parity. Their proposal negates the program's premise that acceptance must be compelled rather than earned; they hold no institutions and no armed wing, and the movement dismisses them as unrealistic.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, binationalist_advocates, excluded,
    moderate, generational, constrained, local).

% Retrospective analytical seat working from the Iron Wall essays, congress proceedings, intelligence files, and the archival and oral record of 1948. Sees the whole structure at once — doctrine, mobilization, suppression, displacement — including features invisible to participants at any single moment.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, historians_of_the_conflict, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_territorial_claim__revisionist_zionism_reading, diffuse).
narrative_ontology:fixing_cost_class(jewish_territorial_claim__revisionist_zionism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies a dispersed, stateless national movement behind a single non-negotiable territorial demand and a single security doctrine, solving the collective-action problems of fragmentation (which lands, which method, when to compromise) by declaring the answers fixed: both banks, immediate sovereignty, force before negotiation.
% TRANSFER_FUNCTION: Moves land, water, and sovereign authority from the Arab inhabitants of both banks of the Jordan to the projected Jewish state; moves the security burden onto the movement's own military arm; moves the demographic and moral costs of dispossession onto the target population and, contingently, onto future generations of both peoples.
% ABSENT_VOICES: Palestinian Arabs — the population whose acceptance the doctrine declares unnecessary — are absent from the constraint's legitimacy calculus by design; so are binationalist advocates, labor-zionist diplomats, and neighboring Arab governments, each of whom would condition or refuse the claim. Their absence is not incidental: the reading defines itself by refusing their standing.
% DISAPPEARANCE_RATIONALE: If the maximalist claim and its Iron Wall vanished overnight, the Zionist movement's internal balance would shift toward the diplomatic and settlement-building readings; Arab consent would re-enter as a live variable; the paramilitary organizations would lose their organizing principle; and the 1948 war's maximalist objectives, with the displacement that accompanied them, would not be set in motion in the same form.
% FOUNDING_PROBLEM: European antisemitism and statelessness — pogroms, exclusionary nationality regimes — made sovereign refuge an existential need; Jabotinsky's specific diagnosis added that Arab national feeling made voluntary consent to a Jewish state impossible, so only an unbreachable wall of force, erected before negotiations, could ever make the state possible.
% FOUNDING_PROBLEM_CORROBORATION: The underlying insecurity is corroborated outside the benefiting parties: consular reporting on the pogroms, and the King-Crane Commission's 1919 documentation of near-unanimous Arab opposition to the Zionist program, which simultaneously corroborates the factual premise of the Iron Wall argument. The historiography (Shapira's Land and Power) attests both the reality of Jewish insecurity and the deliberate character of the choice of force over consent. No source outside the benefiting parties attests that both-banks maximalism specifically was necessary rather than chosen — that attribution rests on the movement's own testimony alone.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__revisionist_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__revisionist_zionism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__revisionist_zionism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.86 at interval end) because the claim transfers the totality of political self-determination from the majority population of both banks without their consent and without compensation — land, water, sovereignty, and demographic future move in one direction. Suppression is higher still (0.90) and is a RAW structural property, unscaled by power or scope: the Iron Wall is not an enforcement layer added to the claim, it IS the claim's mechanism — the doctrine defines force as the substitute for agreement, so the constraint's persistence depends on coercive capacity by construction. Theater ratio is low at end-state (0.15) and falls across the series: in 1923 the program was mostly rhetoric (uniformed parades, maximalist speeches, minimal capacity — theater-heavy), while by 1944-1948 the activity is overwhelmingly functional (revolt, insurgency, war, displacement). The declining theater series tracks function replacing performance, the inverse of Goodhart drift. Accessibility_collapse is 0.62: within the movement's frame alternatives collapse hard (partition equals treason; consent-seeking equals Herzl-betrayal), and for the target population no exit exists at all — but rival readings persisted institutionally inside Zionism throughout, so collapse is severe for targets and only partial for participants. Resistance is high (0.75): the 1936-1939 general strike and revolt, sustained Irgun-British conflict, and permanent intra-Zionist opposition. The measurement series run on one shared time grid (1923, 1929, 1935, 1936, 1939, 1944, 1948) with every tracked metric authored at every point; the trajectories show an escalation ratchet, not a cycle — each round of violence raised the enforcement baseline permanently rather than oscillating.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently and the structural data explains why. From the payer seats (palestinian_arab_residents, transjordanian_arab_communities) the arrangement operates as pure extraction under compulsion: no consent, no exit, no compensation — a snare-shaped experience. From the beneficiary seats (leadership, Betar rank-and-file) the same structure is genuine coordination: a stateless, recently massacred minority unifying behind one demand and one defense doctrine against documented violence (1920, 1921, 1929) — a rope-shaped experience. The agenda-setter seat experiences it as necessity doctrine: not preference but the only arithmetically available path. The engine computes this divergence from power, exit, and directional position; the authored claim does not adjudicate it. On coalition: the payer class repeatedly attempted coalition (Arab Higher Committee, pan-Arab intervention) and repeatedly fragmented — Hussein-Nashashibi clan rivalry, the British destruction of the revolt's leadership, and divided state interests — which is why a numerical majority with organized capacity nonetheless computed as tractable targets rather than as a blocking coalition.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive the derivation. revisionist_zionist_leadership and betar_rank_and_file sit at the beneficiary end (d near 0.0): the claim subsidizes them with purpose, cohesion, and promised statehood, and their identity_locked exit damps their effective extraction further. palestinian_arab_residents sit near the full-target end (d near 1.0): they bear the entire transfer, and their trapped exit amplifies effective extraction — there is no arbitrage, no second passport, no exit from the claim's consequences. transjordanian_arab_communities are likewise trapped and additionally powerless, placing them at the extreme target end. britain_mandate_administration holds an analytical-adjacent observer position with arbitrage-grade exit (it simply left in 1948), so it registers almost no extraction. The excluded seats (neighboring states, labor leadership, binationalists) carry no directional weight until seated — their exclusion is itself the enforcement object, since the doctrine's defining move is declaring their consent unnecessary.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling in both directions. A pure-rope reading would erase the victims: the coordination story (national unity, defense against real massacres) is genuine, but it rides on a structure that transfers everything from a non-consenting majority — calling that rope launders dispossession as cooperation. A pure-snare reading would erase the coordination: for a persecuted minority facing documented violence, unified territorial demand and military self-defense solved real collective-action problems that no other available reading solved as decisively. Tangled rope keeps both faces visible and forces the analytical question onto the seam: which part of the measured extraction is the price of coordination under siege, and which part is the maximalist premium (both banks, no consent) that no defensive need required? On obsolescence: the founding problem (existential insecurity) remains live, but the specific mandate (both banks, consent rejected) is contested even within Zionism — status=contested combined with verdict=world_rearranges is a coherent cell, not the dead-mandate zombie signature; the arrangement persists because the world rearranges around it, not because its founding problem died.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This story instantiates the revisionist_zionism_reading of the jewish_territorial_claim kernel; how would the classification shift if the sibling readings (political, labor, cultural zionism) were instantiated instead?',
    'Author each sibling as its own constraint story with its own scope, mechanism, and victim set; compare computed types and epsilon across the family.',
    'political_zionism_reading narrows scope to west of the Jordan and admits diplomacy (lower suppression, rope-leaning); labor_zionism_reading substitutes settlement facts for force (transfer mechanism changes, victims persist); cultural_zionism_reading drops the sovereignty requirement entirely (victim set shrinks or dissolves).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: this is one reading of a four-reading kernel; sibling readings are separate constraints, not hedges inside this one.').

omega_variable(
    iron_wall_necessity_or_self_fulfillment,
    'Was the Iron Wall empirically necessary given Arab rejection, or did the maximalist claim itself produce and harden the rejection it cited?',
    'Counterfactual historiography: Arab positions before the doctrine (Faisal-Weizmann correspondence 1919, Syrian Congress conditions), the King-Crane Commission record of near-unanimous Arab opposition, and whether binding minority-rights or consent mechanisms were ever seriously tabled by the movement.',
    'If necessity holds, part of the measured suppression is defensive coordination cost and the tangled-rope reading strengthens; if self-fulfilling, the suppression is amplified extraction and the profile slides toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(iron_wall_necessity_or_self_fulfillment, empirical, 'Whether the coercive mechanism was forced by circumstances or produced them.').

omega_variable(
    east_bank_claim_liveness,
    'After the 1922 Churchill White Paper severed Transjordan, was the both-banks limb of the claim a live operational objective or retained rhetoric?',
    'Compare movement resources and paramilitary operational geography devoted to the east bank against rhetorical invocations in congress platforms and the party press.',
    'If rhetorical, effective spatial scope is smaller than declared and scope-amplified extraction is overstated; if live, the claim''s scope is genuinely continental and the east-bank victim set is fully engaged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(east_bank_claim_liveness, empirical, 'Liveness of the transjordanian limb of the territorial claim.').

omega_variable(
    constituency_persistence_mechanism,
    'Does the Revisionist constituency''s persistence rest on strategic assessment (the wall will work) or on identity fusion formed through Betar socialization?',
    'Post-interval behavior: responses to territorial compromise offers (acceptance of armistice lines, later peace treaties) reveal whether the claim was held instrumentally or constitutively.',
    'If identity fusion dominates, exit_options for the beneficiary seat remain identity_locked and the claim outlives its strategic rationale; if strategic, the claim is revisable when costs exceed returns.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constituency_persistence_mechanism, conceptual, 'Strategic versus identity-fusion basis of constituency persistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__revisionist_zionism_reading, 1923, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1923, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1923, 0.32).
narrative_ontology:measurement_basis(jewi_tr_t1923, observed).
narrative_ontology:measurement(jewi_tr_t1929, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1929, 0.29).
narrative_ontology:measurement_basis(jewi_tr_t1929, observed).
narrative_ontology:measurement(jewi_tr_t1935, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1935, 0.26).
narrative_ontology:measurement_basis(jewi_tr_t1935, observed).
narrative_ontology:measurement(jewi_tr_t1936, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1936, 0.23).
narrative_ontology:measurement_basis(jewi_tr_t1936, observed).
narrative_ontology:measurement(jewi_tr_t1939, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1939, 0.2).
narrative_ontology:measurement_basis(jewi_tr_t1939, observed).
narrative_ontology:measurement(jewi_tr_t1944, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1944, 0.17).
narrative_ontology:measurement_basis(jewi_tr_t1944, observed).
narrative_ontology:measurement(jewi_tr_t1948, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1948, 0.15).
narrative_ontology:measurement_basis(jewi_tr_t1948, observed).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1923, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1923, 0.55).
narrative_ontology:measurement_basis(jewi_be_t1923, observed).
narrative_ontology:measurement(jewi_be_t1929, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1929, 0.61).
narrative_ontology:measurement_basis(jewi_be_t1929, observed).
narrative_ontology:measurement(jewi_be_t1935, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1935, 0.67).
narrative_ontology:measurement_basis(jewi_be_t1935, observed).
narrative_ontology:measurement(jewi_be_t1936, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1936, 0.73).
narrative_ontology:measurement_basis(jewi_be_t1936, observed).
narrative_ontology:measurement(jewi_be_t1939, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1939, 0.77).
narrative_ontology:measurement_basis(jewi_be_t1939, observed).
narrative_ontology:measurement(jewi_be_t1944, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1944, 0.83).
narrative_ontology:measurement_basis(jewi_be_t1944, observed).
narrative_ontology:measurement(jewi_be_t1948, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1948, 0.86).
narrative_ontology:measurement_basis(jewi_be_t1948, observed).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1923, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1923, 0.42).
narrative_ontology:measurement_basis(jewi_su_t1923, observed).
narrative_ontology:measurement(jewi_su_t1929, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1929, 0.52).
narrative_ontology:measurement_basis(jewi_su_t1929, observed).
narrative_ontology:measurement(jewi_su_t1935, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1935, 0.6).
narrative_ontology:measurement_basis(jewi_su_t1935, observed).
narrative_ontology:measurement(jewi_su_t1936, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1936, 0.71).
narrative_ontology:measurement_basis(jewi_su_t1936, observed).
narrative_ontology:measurement(jewi_su_t1939, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1939, 0.79).
narrative_ontology:measurement_basis(jewi_su_t1939, observed).
narrative_ontology:measurement(jewi_su_t1944, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1944, 0.86).
narrative_ontology:measurement_basis(jewi_su_t1944, observed).
narrative_ontology:measurement(jewi_su_t1948, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1948, 0.9).
narrative_ontology:measurement_basis(jewi_su_t1948, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__revisionist_zionism_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, political_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, labor_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, cultural_zionism_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the Jewish territorial claim' covers four structurally distinct commitments sharing one kernel. They differ on scope (both banks vs west of the Jordan vs no territory), mechanism (force vs charter diplomacy vs settlement facts vs spiritual center), and consent prerequisites (explicitly rejected vs sought vs irrelevant). Their epsilon values differ accordingly, so each is authored as its own story per the epsilon-invariance principle. This file is the revisionist reading; it links to all three siblings. Upstream/downstream: the political reading is upstream (the Revisionists claimed to be its true heir and lobbied for a restored charter), while this reading exerts downstream pressure on all siblings by militarizing the movement's operating environment and making compromise positions costlier to hold.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
