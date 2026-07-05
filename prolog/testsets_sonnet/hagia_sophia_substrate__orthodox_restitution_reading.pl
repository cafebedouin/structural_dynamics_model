% ============================================================================
% CONSTRAINT STORY: hagia_sophia_substrate__orthodox_restitution_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hagia_sophia_substrate__orthodox_restitution_reading, []).

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
 *   constraint_id: hagia_sophia_substrate__orthodox_restitution_reading
 *   human_readable: Orthodox Restitution Claim on Hagia Sophia (Ecclesiastical Return / Neutrality Reading)
 *   domain: cultural_heritage/religious_authority/geopolitical
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested Hagia Sophia
 *   sovereignty kernel: the claim that the site's founding as a Byzantine
 *   Orthodox cathedral generates a persisting normative claim, satisfied
 *   either by ecclesiastical restitution or by neutral/museum status honoring
 *   that origin. This reading has no enforcement mechanism and no realistic
 *   implementation pathway — Turkey exercises full administrative control and
 *   no international body adjudicates the claim as binding. What the reading
 *   generates is symbolic and diplomatic: it sustains Orthodox diaspora
 *   identity cohesion, gives the Greek state a durable rhetorical instrument
 *   in bilateral friction with Turkey, and periodically reframes the site's
 *   current Muslim worship community's continuous use as an open historical
 *   wrong. The sibling readings (islamic_sovereignty_reading, asserting
 *   Ottoman-conquest-plus-waqf legitimacy; universal_heritage_reading,
 *   asserting trans-religious shared heritage) are separate constraints with
 *   their own ε and stakeholder structures, not alternate measurements of
 *   this one.
 *
 * KEY AGENTS:
 *   - eastern_orthodox_diaspora: symbolic beneficiary (moderate/analytical) — draws identity cohesion, bears no cost
 *   - greek_state_diplomacy: beneficiary/agenda_setter (institutional/constrained) — uses claim as diplomatic leverage
 *   - ecumenical_patriarchate: cautious beneficiary (moderate/constrained) — benefits symbolically but avoids active pursuit given its own vulnerability
 *   - turkish_state_sovereignty: payer (institutional/mobile) — absorbs recurring diplomatic friction, faces no material threat
 *   - muslim_worship_congregation: payer (organized/trapped) — bears the implicit delegitimization of their continuous worship
 *   - unesco_world_heritage_committee: observer (institutional/analytical) — monitors heritage status without adjudicating sovereignty
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_substrate__orthodox_restitution_reading, 0.18).
domain_priors:suppression_score(hagia_sophia_substrate__orthodox_restitution_reading, 0.12).
domain_priors:theater_ratio(hagia_sophia_substrate__orthodox_restitution_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_substrate__orthodox_restitution_reading, piton).
narrative_ontology:human_readable(hagia_sophia_substrate__orthodox_restitution_reading, "Orthodox Restitution Claim on Hagia Sophia (Ecclesiastical Return / Neutrality Reading)").
narrative_ontology:topic_domain(hagia_sophia_substrate__orthodox_restitution_reading, "cultural_heritage/religious_authority/geopolitical").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hagia_sophia_substrate__orthodox_restitution_reading, '5e769a96-91bd-4766-b757-81e5ada95737').
narrative_ontology:cs_kernel_codification('5e769a96-91bd-4766-b757-81e5ada95737', distributed).
narrative_ontology:cs_authority_grounding('5e769a96-91bd-4766-b757-81e5ada95737', distributed).
narrative_ontology:cs_reading_relation('5e769a96-91bd-4766-b757-81e5ada95737', hagia_sophia_substrate__islamic_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('5e769a96-91bd-4766-b757-81e5ada95737', hagia_sophia_substrate__universal_heritage_reading, influences).
narrative_ontology:cs_axiom('5e769a96-91bd-4766-b757-81e5ada95737', foundational, founding_consecration_generates_unextinguished_claim).
narrative_ontology:cs_axiom_status(founding_consecration_generates_unextinguished_claim, holdable).
narrative_ontology:cs_axiom_grounding('5e769a96-91bd-4766-b757-81e5ada95737', founding_consecration_generates_unextinguished_claim, theological).
narrative_ontology:cs_axiom('5e769a96-91bd-4766-b757-81e5ada95737', secondary, conquest_and_subsequent_use_cannot_extinguish_prior_ecclesiastical_title).
narrative_ontology:cs_axiom_status(conquest_and_subsequent_use_cannot_extinguish_prior_ecclesiastical_title, holdable).
narrative_ontology:cs_axiom_grounding('5e769a96-91bd-4766-b757-81e5ada95737', conquest_and_subsequent_use_cannot_extinguish_prior_ecclesiastical_title, deontological).
narrative_ontology:cs_reference_frame('5e769a96-91bd-4766-b757-81e5ada95737', byzantine_ecclesiastical_founding).
narrative_ontology:cs_drift_state('5e769a96-91bd-4766-b757-81e5ada95737', post_2020_mosque_reconversion, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('5e769a96-91bd-4766-b757-81e5ada95737', '').
narrative_ontology:cs_kernel_id(hagia_sophia_substrate__orthodox_restitution_reading, hagia_sophia_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__orthodox_restitution_reading, eastern_orthodox_diaspora).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__orthodox_restitution_reading, greek_state_diplomacy).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__orthodox_restitution_reading, ecumenical_patriarchate).
narrative_ontology:constraint_victim(hagia_sophia_substrate__orthodox_restitution_reading, turkish_state_sovereignty).
narrative_ontology:constraint_victim(hagia_sophia_substrate__orthodox_restitution_reading, muslim_worship_congregation).
narrative_ontology:constraint_vindicates(hagia_sophia_substrate__orthodox_restitution_reading, byzantine_continuity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities dispersed from historically Orthodox lands who hold the cathedral's Byzantine founding as a live symbol of continuity and dispossession. They gain identity cohesion and a rallying point from the restitution claim without bearing any cost of pressing it; the claim circulates in diaspora institutions, liturgy commemorations, and advocacy statements rather than any legal proceeding they could lose.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, eastern_orthodox_diaspora, beneficiary,
    moderate, generational, analytical, global).

% Periodically invokes the restitution or neutrality framing in bilateral friction with Turkey — Aegean disputes, minority rights, EU accession leverage — without ever formally litigating the claim. The framing costs little to raise and nothing to withdraw; it functions as a durable rhetorical asset rather than a policy the state is committed to executing.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, greek_state_diplomacy, beneficiary,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(hagia_sophia_substrate__orthodox_restitution_reading, greek_state_diplomacy, agenda_setter).

% Based in Istanbul under conditions of its own precarious minority status, the Patriarchate benefits symbolically from the site's Christian founding narrative but has strong incentive not to press restitution actively, since doing so risks its own already-fragile standing with the Turkish state. It draws quiet legitimacy from the claim while distancing itself from its practical pursuit.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, ecumenical_patriarchate, beneficiary,
    moderate, civilizational, constrained, global).

% Holds full administrative and legal control of the site since 1934 (museum status) and again since 2020 (mosque status), and treats the Orthodox restitution claim as an external irritant with no binding force. It bears no material cost from the claim's existence but experiences repeated diplomatic and domestic-political friction each time the claim resurfaces in Greek or Western commentary; it has ample capacity to ignore it entirely.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, turkish_state_sovereignty, payer,
    institutional, generational, mobile, national).

% Worships at the site under its current mosque designation. Every renewed restitution or neutrality campaign implicitly casts their continuous use of the space as a historical wrong to be corrected, even though they have no role in the geopolitics generating the claim and no alternative site carries equivalent meaning. Their claim to uninterrupted worship is the thing this reading's remedy would necessarily disturb.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, muslim_worship_congregation, payer,
    organized, biographical, trapped, local).

% Monitors the site's World Heritage status and has commented on the 2020 status change without adjudicating any of the three sovereignty readings. It observes the contest from a heritage-preservation angle rather than a religious-authority angle, and its statements are cited by all three readings without endorsing any of them.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, unesco_world_heritage_committee, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hagia_sophia_substrate__orthodox_restitution_reading, diffuse).
narrative_ontology:fixing_cost_class(hagia_sophia_substrate__orthodox_restitution_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the Orthodox Christian world and the Greek state a stable, low-cost symbolic anchor for continuity claims and diplomatic leverage — it coordinates diaspora identity and periodic bilateral pressure without requiring any actual administrative capacity to run the site.
% TRANSFER_FUNCTION: Moves symbolic legitimacy and diplomatic leverage toward Greek state interests and Orthodox institutional standing, and moves reputational and psychological cost onto the Turkish state (repeated delegitimization pressure) and onto the site's current Muslim congregation (their continuous use is framed as an open wound rather than a settled fact).
% ABSENT_VOICES: The site's current worship community and the broader Turkish public are rarely party to the diaspora and diplomatic conversations that invoke this reading; they encounter the claim as an external pressure applied to their sovereignty and religious practice rather than as a claim negotiated with their participation.
% DISAPPEARANCE_RATIONALE: If the Orthodox restitution/neutrality claim vanished overnight, the site's physical administration, legal status, and daily worship pattern under Turkish state control would not change at all — the claim has no enforcement mechanism and no implementation pathway. What would change is a narrower set of diaspora identity discourse and periodic Greek-Turkish diplomatic rhetoric, which would lose one recurring symbolic reference point but would find others.
% FOUNDING_PROBLEM: The claim was built to preserve a narrative of continuity and redress for the 15th-century loss of Byzantine ecclesiastical control and the site's subsequent conversion to a mosque, later museum, then mosque again — asserting that the site's Christian founding carries an unextinguished normative claim.
% FOUNDING_PROBLEM_CORROBORATION: Orthodox diaspora institutions and the Greek Foreign Ministry attest the founding problem (unresolved Byzantine dispossession) remains live. Independent historians of Ottoman and Byzantine transition, along with Turkish legal scholars, corroborate that the 1453 conquest and subsequent five centuries of continuous Islamic use constitute settled fact under every applicable framework of international law and prescription — no international body outside the Greek state and Orthodox ecclesiastical bodies treats the restitution claim as a live legal question, only as a live rhetorical one.
narrative_ontology:disappearance_verdict(hagia_sophia_substrate__orthodox_restitution_reading, world_unchanged).
narrative_ontology:founding_problem_status(hagia_sophia_substrate__orthodox_restitution_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hagia_sophia_substrate__orthodox_restitution_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hagia_sophia_substrate__orthodox_restitution_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hagia_sophia_substrate__orthodox_restitution_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hagia_sophia_substrate__orthodox_restitution_reading_tests).
:- end_tests(hagia_sophia_substrate__orthodox_restitution_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.18 at interval end) because the reading has no material implementation pathway — it does not move money, territory, or administrative control from anyone to anyone in practice. Suppression is authored low (0.12) since no coercive apparatus enforces this reading; it persists by advocacy and rhetoric, not force. Theater ratio is authored moderately high and rising (0.35 to 0.55) because an increasing share of the reading's activity is performative — commemorative statements, diplomatic signaling timed to bilateral disputes — rather than any function moving toward resolution; this is a claim sustained more by ritual invocation than functional progress toward an achievable outcome. Resistance is authored high (0.72) reflecting the strong pushback the claim meets from Turkish state and legal-scholarly quarters whenever it resurfaces. Accessibility collapse is authored moderate (0.35): alternative framings (islamic_sovereignty_reading, universal_heritage_reading) remain fully live and contested, so this reading has not collapsed the field of live positions.
 *
 * PERSPECTIVAL GAP:
 *   From the Orthodox diaspora and Greek diplomatic seats, the claim functions as coordination — a shared, cost-free symbolic anchor requiring no institutional machinery to sustain. From the Turkish sovereignty and Muslim congregation seats, the same claim functions as a low-grade but recurring extraction of legitimacy and standing: their settled, centuries-continuous administrative and religious fact is repeatedly reopened as contestable by an external party bearing none of the cost of that contestation. The engine should register this asymmetry: low material ε paired with a genuine payer/beneficiary split.
 *
 * DIRECTIONALITY LOGIC:
 *   Eastern Orthodox diaspora and Greek state diplomacy sit near the beneficiary end: low or no cost, clear symbolic/diplomatic gain, and no exposure to the claim ever being tested and lost. The Ecumenical Patriarchate is beneficiary-leaning but structurally constrained — it profits from the narrative's mere existence while having strong reason to avoid activating it. Turkish state sovereignty and the Muslim worship congregation sit toward the target end: they bear the recurring cost of delegitimization pressure without deriving any benefit from the claim's persistence, even though the material stakes for the Turkish state remain low given its uncontested administrative control.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — unresolved 15th-century ecclesiastical dispossession — is contested rather than clearly dead: Orthodox and Greek state seats treat it as live; independent historical and legal corroboration outside the beneficiary set treats it as settled by five centuries of continuous Islamic use and administration. This divergence prevents a simple resolution: the reading cannot be dismissed as pure zombie mandate (there is a real historical grievance underlying it) nor certified as a live operative claim (it has no enforcement pathway and no adjudicating body treats it as binding). The classification as a low-ε, high-theater piton-leaning symbolic structure captures this: a constraint maintained for its rhetorical utility rather than because it is actively resolving anything.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    restitution_claim_enforceability,
    'Does the Orthodox restitution/neutrality reading carry any enforceable legal or institutional pathway, or is it purely a rhetorical/diplomatic instrument with no realistic implementation route?',
    'Examine whether any international legal body (ICJ, UNESCO dispute mechanisms, bilateral treaty frameworks) has ever accepted jurisdiction over the claim, versus whether it appears only in advocacy statements, diaspora commemorations, and diplomatic rhetoric.',
    'If no enforceable pathway exists (the current empirical picture), the reading''s ε remains low and its classification leans piton — symbolic persistence without functional mechanism. If an enforcement pathway emerged (e.g., a new international legal forum), the reading''s structure would shift toward an active tangled_rope or snare depending on how costs distributed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(restitution_claim_enforceability, empirical, 'Whether the restitution reading has any real implementation mechanism beyond rhetoric.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the Hagia Sophia sovereignty question best modeled as three coexisting readings of one kernel (as authored here), or does one reading''s growing symbolic/diplomatic use functionally foreclose the practical viability of another (e.g., does sustained Turkish 2020 mosque-status reassertion foreclose meaningful pursuit of the restitution reading, even if it does not foreclose its rhetorical life)?',
    'Track whether Greek state and Orthodox institutional invocations of the restitution claim measurably decline in frequency or specificity following major Turkish sovereignty assertions (e.g., the 2020 status change), which would indicate de facto (if not de jure) foreclosure pressure from the islamic_sovereignty_reading onto this one.',
    'If sustained empirical decline is observed, this reading''s relationship to islamic_sovereignty_reading should be re-examined for an ''influences'' or even ''forecloses'' relation rather than pure coexistence; if invocation frequency remains stable or cyclical, coexists_with remains the accurate structural characterization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether one reading''s practical dominance creates de facto foreclosure pressure on this reading despite formal coexistence.').

omega_variable(
    diaspora_beneficiary_naturality,
    'Is the diaspora/state benefit from this claim better modeled as a genuine coordination good (identity cohesion, cultural continuity) or as an extractive rhetorical asset maintained because it costs the beneficiaries nothing while imposing real cost on the Turkish state and Muslim congregation?',
    'Compare the claim''s persistence against counterfactual cases where similar historical-restitution claims were formally withdrawn or resolved by treaty (e.g., other post-imperial religious property disputes) to see whether resolution reduced diaspora cohesion or merely removed friction cost from the payer side.',
    'If resolution elsewhere shows diaspora cohesion persists via other symbols while payer friction disappears, this strengthens the extractive characterization; if diaspora cohesion measurably depends on the claim''s live status, it strengthens the genuine-coordination characterization.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(diaspora_beneficiary_naturality, conceptual, 'Whether the beneficiary side derives a genuine coordination good or a costless extractive rhetorical asset.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_substrate__orthodox_restitution_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hagi_tr_t0, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(hagi_tr_t8, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 8, 0.4).
narrative_ontology:measurement(hagi_tr_t16, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 16, 0.48).
narrative_ontology:measurement(hagi_tr_t24, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 24, 0.5).
narrative_ontology:measurement(hagi_tr_t32, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 32, 0.52).
narrative_ontology:measurement(hagi_tr_t40, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 40, 0.55).

% Extraction over time
narrative_ontology:measurement(hagi_be_t0, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(hagi_be_t8, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 8, 0.14).
narrative_ontology:measurement(hagi_be_t16, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 16, 0.16).
narrative_ontology:measurement(hagi_be_t24, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 24, 0.17).
narrative_ontology:measurement(hagi_be_t32, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 32, 0.17).
narrative_ontology:measurement(hagi_be_t40, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 40, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(hagia_sophia_substrate__orthodox_restitution_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hagia_sophia_substrate__orthodox_restitution_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hagia_sophia_substrate__orthodox_restitution_reading, 0.08).
narrative_ontology:affects_constraint(hagia_sophia_substrate__orthodox_restitution_reading, hagia_sophia_substrate__islamic_sovereignty_reading).
narrative_ontology:affects_constraint(hagia_sophia_substrate__orthodox_restitution_reading, hagia_sophia_substrate__universal_heritage_reading).

% DUAL FORMULATION NOTE:
% Part of the hagia_sophia_substrate kernel family (3 readings). This story (orthodox_restitution_reading) coexists with islamic_sovereignty_reading and universal_heritage_reading as parallel, non-merged constraints, each with independently authored ε and stakeholder structure. Orthodox restitution's low material ε and high theater_ratio contrast with islamic_sovereignty_reading's expected higher ε (it names an enforceable, currently-operative sovereignty claim with real administrative consequences) and with universal_heritage_reading's expected lower suppression and more genuinely coordinative profile (a heritage-preservation framing with broader, less partisan beneficiary base).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
