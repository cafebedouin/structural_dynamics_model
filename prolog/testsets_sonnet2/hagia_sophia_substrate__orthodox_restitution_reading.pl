% ============================================================================
% CONSTRAINT STORY: hagia_sophia_substrate__orthodox_restitution_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Orthodox Restitution Claim on Hagia Sophia's Founding Legitimacy
 *   domain: cultural_heritage/religious_authority/geopolitical_sovereignty
 *
 * SUMMARY:
 *   This story instantiates the orthodox_restitution_reading of the contested
 *   hagia_sophia_substrate kernel: the claim that Hagia Sophia's legitimacy
 *   derives from its founding as the cathedral of Byzantine Orthodox
 *   Christianity, and that its proper status is either return to Orthodox
 *   ecclesiastical control or preservation as a religiously neutral museum
 *   honoring that Byzantine origin. This is generated as a single,
 *   ε-invariant constraint on its own terms — it does not describe or average
 *   over the sibling readings (islamic_sovereignty_reading,
 *   universal_heritage_reading), which are separate constraint files with
 *   their own beneficiary/victim structures and their own ε values. The claim
 *   has essentially no realistic implementation pathway (Turkey exercises
 *   full, internationally recognized sovereignty and shows no institutional
 *   openness to ecclesiastical transfer), so material extraction from Turkish
 *   sovereignty is low; what the claim actually does is generate ongoing
 *   symbolic and diplomatic value for Orthodox institutions and the Greek
 *   state while imposing a diffuse reputational tax on Turkey and a quieter
 *   dignitary cost on the site's Muslim congregation.
 *
 * KEY AGENTS:
 *   - ecumenical_patriarchate: agenda_setter — restates and ritually maintains the claim (moderate/identity_locked)
 *   - greek_state: beneficiary — uses the claim as diplomatic leverage (institutional/constrained)
 *   - eastern_orthodox_diaspora: beneficiary — draws communal identity and cohesion from the narrative (organized/identity_locked)
 *   - turkish_state_sovereignty: payer — bears the standing external normative claim on its own territory (institutional/trapped)
 *   - muslim_worshippers_at_site: payer — their present worship is narratively cast as provisional (moderate/constrained)
 *   - unesco_and_heritage_bodies: observer — manages conservation status neutrally amid competing claims (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_substrate__orthodox_restitution_reading, 0.28).
domain_priors:suppression_score(hagia_sophia_substrate__orthodox_restitution_reading, 0.15).
domain_priors:theater_ratio(hagia_sophia_substrate__orthodox_restitution_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_substrate__orthodox_restitution_reading, tangled_rope).
narrative_ontology:human_readable(hagia_sophia_substrate__orthodox_restitution_reading, "Orthodox Restitution Claim on Hagia Sophia's Founding Legitimacy").
narrative_ontology:topic_domain(hagia_sophia_substrate__orthodox_restitution_reading, "cultural_heritage/religious_authority/geopolitical_sovereignty").

domain_priors:requires_active_enforcement(hagia_sophia_substrate__orthodox_restitution_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hagia_sophia_substrate__orthodox_restitution_reading, '0ac68d79-3a95-41a2-b6eb-f0b779c1d556').
narrative_ontology:cs_kernel_codification('0ac68d79-3a95-41a2-b6eb-f0b779c1d556', distributed).
narrative_ontology:cs_authority_grounding('0ac68d79-3a95-41a2-b6eb-f0b779c1d556', distributed).
narrative_ontology:cs_reading_relation('0ac68d79-3a95-41a2-b6eb-f0b779c1d556', hagia_sophia_substrate__islamic_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('0ac68d79-3a95-41a2-b6eb-f0b779c1d556', hagia_sophia_substrate__universal_heritage_reading, influences).
narrative_ontology:cs_axiom('0ac68d79-3a95-41a2-b6eb-f0b779c1d556', foundational, founding_consecration_establishes_perpetual_ecclesiastical_claim).
narrative_ontology:cs_axiom_status(founding_consecration_establishes_perpetual_ecclesiastical_claim, holdable).
narrative_ontology:cs_axiom_grounding('0ac68d79-3a95-41a2-b6eb-f0b779c1d556', founding_consecration_establishes_perpetual_ecclesiastical_claim, theological).
narrative_ontology:cs_axiom('0ac68d79-3a95-41a2-b6eb-f0b779c1d556', secondary, conquest_and_subsequent_administration_do_not_extinguish_prior_sacred_title).
narrative_ontology:cs_axiom_status(conquest_and_subsequent_administration_do_not_extinguish_prior_sacred_title, holdable).
narrative_ontology:cs_axiom_grounding('0ac68d79-3a95-41a2-b6eb-f0b779c1d556', conquest_and_subsequent_administration_do_not_extinguish_prior_sacred_title, deontological).
narrative_ontology:cs_reference_frame('0ac68d79-3a95-41a2-b6eb-f0b779c1d556', byzantine_cathedral_consecration_537ce).
narrative_ontology:cs_drift_state('0ac68d79-3a95-41a2-b6eb-f0b779c1d556', post_2020_remosqueification, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('0ac68d79-3a95-41a2-b6eb-f0b779c1d556', '').
narrative_ontology:cs_kernel_id(hagia_sophia_substrate__orthodox_restitution_reading, hagia_sophia_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__orthodox_restitution_reading, eastern_orthodox_diaspora).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__orthodox_restitution_reading, greek_state).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__orthodox_restitution_reading, ecumenical_patriarchate).
narrative_ontology:constraint_victim(hagia_sophia_substrate__orthodox_restitution_reading, turkish_state_sovereignty).
narrative_ontology:constraint_victim(hagia_sophia_substrate__orthodox_restitution_reading, muslim_worshippers_at_site).
narrative_ontology:constraint_vindicates(hagia_sophia_substrate__orthodox_restitution_reading, byzantine_founding_priority_doctrine).
narrative_ontology:constraint_vindicates(hagia_sophia_substrate__orthodox_restitution_reading, ecclesiastical_continuity_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains and periodically restates the claim that Hagia Sophia's founding as the seat of Byzantine Orthodox Christianity is the site's true legitimating origin, and that its 2020 reconversion to a mosque was a wound to Orthodoxy. Has no enforcement mechanism and does not seek forcible restitution, but keeps the claim alive liturgically and diplomatically. Cannot abandon the claim without undermining its own historical self-understanding as the successor see of Byzantium.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, ecumenical_patriarchate, agenda_setter,
    moderate, civilizational, identity_locked, global).

% Uses the Orthodox restitution framing as diplomatic leverage in disputes with Turkey over the Aegean, Cyprus, and minority rights for the Greek Orthodox community in Istanbul. Raises the issue at European and international forums when relations sour. Benefits from the claim's rhetorical power without needing it to succeed materially; the claim's persistence, not its resolution, is what serves Greek diplomatic interests.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, greek_state, beneficiary,
    institutional, generational, constrained, national).

% Diaspora communities across Europe, North America, and Australia treat the founding-cathedral narrative as a live symbol of continuity and historical grievance. Donations, commemorative events, and political lobbying flow from this narrative. Their attachment to the claim is constitutive of communal identity; exit from the narrative would mean exit from a shared historical self-understanding, not merely a policy position.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, eastern_orthodox_diaspora, beneficiary,
    organized, generational, identity_locked, global).

% Bears the external normative pressure of a foreign-origin claim on a structure standing on its own recognized territory, administered under its own law since 1453 (mosque, then museum, then mosque again in 2020). Cannot simply exit the dispute because the claim recurs in every diplomatic friction point with Greece and in Western media coverage of Turkish domestic religious policy. Has full physical and legal control of the site but must continually manage the reputational and diplomatic cost of the standing claim.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, turkish_state_sovereignty, payer,
    institutional, civilizational, trapped, national).

% Pray at the site as a functioning mosque under current Turkish administration. The restitution narrative implicitly frames their ongoing worship as an interruption of the 'true' Christian function of the building, symbolically delegitimizing their present use even though no material change to their access has ever resulted from the Orthodox claim. They have no voice in the international framing dispute and did not choose to become symbolic parties to it.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, muslim_worshippers_at_site, payer,
    moderate, biographical, constrained, national).

% Monitor the site's World Heritage status and issue statements when status changes (as in 2020) affect conservation access or the site's dual-heritage designation. Take no position on ecclesiastical restitution but their conservation mandate is invoked by all three kernel readings to support incompatible conclusions.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, unesco_and_heritage_bodies, observer,
    institutional, generational, analytical, global).

% The islamic_sovereignty_reading and universal_heritage_reading are not represented within this constraint's own frame — they are separate constraints entirely, but every public restatement of the Orthodox claim implicitly argues against them without engaging their proponents directly.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, rival_kernel_readings, excluded,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides Eastern Orthodox communities and the Greek state with a coherent historical-legitimacy narrative that coordinates diaspora identity, diplomatic positioning, and liturgical memory around a shared claim of dispossession and continuity — a genuine internal coordination function for a dispersed religious and national community.
% TRANSFER_FUNCTION: Moves symbolic capital and diplomatic leverage toward the Greek state and Orthodox institutional bodies (usable in negotiations, in EU/international forums, in diaspora fundraising and cohesion) while imposing a standing reputational and diplomatic cost on Turkish sovereignty and a quieter dignitary cost on the Muslim congregation whose present worship is narratively framed as provisional or illegitimate.
% ABSENT_VOICES: Muslim worshippers at the site are almost never quoted or centered in the international restitution debate; the debate is conducted between Greek/Orthodox advocates, Turkish state officials, and Western heritage commentators, with the people who actually use the building daily treated as background rather than party.
% DISAPPEARANCE_RATIONALE: If the Orthodox restitution claim vanished overnight, Turkish sovereignty over the site would be entirely unaffected in material terms — no enforcement mechanism exists to disappear. But Greek diplomatic leverage on a recurring friction point would lose a rhetorical instrument, and Orthodox diaspora communities would lose a piece of shared narrative identity that currently does real coordination work; whether that counts as 'the world rearranges' depends on whether symbolic/diplomatic capital is counted as part of 'the world.'
% FOUNDING_PROBLEM: The claim was built to preserve institutional and communal memory of the Byzantine Orthodox Church's dispossession after 1453, and to maintain a normative marker that the conversion was a conquest-imposed change rather than a legitimate succession — originally serving a genuine grief-and-memory function for a community that lost its principal cathedral.
% FOUNDING_PROBLEM_CORROBORATION: Independent historians of Byzantine and Ottoman history (writing outside both the Greek state and the Ecumenical Patriarchate) generally corroborate that the historical founding-as-cathedral fact is not in dispute, but many of the same historians and outside diplomatic analysts argue the restitution claim's present function is now primarily geopolitical leverage and diaspora cohesion rather than a live pursuit of actual ecclesiastical control — a status the Patriarchate and Greek state do not themselves concede.
narrative_ontology:disappearance_verdict(hagia_sophia_substrate__orthodox_restitution_reading, contested).
narrative_ontology:founding_problem_status(hagia_sophia_substrate__orthodox_restitution_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hagia_sophia_substrate__orthodox_restitution_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hagia_sophia_substrate__orthodox_restitution_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hagia_sophia_substrate__orthodox_restitution_reading, 0.28, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored moderate-low (0.28) because there is no enforcement mechanism moving any actual resource, territory, or worship right from Turkey to Orthodox institutions — the transfer is symbolic and diplomatic, not material. Suppression is low (0.15): no one is coerced into silence about the claim, and Turkish counter-narratives circulate freely; the constraint does not depend on suppressing alternatives to survive. Theater ratio is authored high and rising (0.30 at founding-era baseline to a 2020 peak of 0.68) because the claim's operative content has shifted heavily toward performative commemoration, diplomatic signaling, and diaspora-identity ritual, with genuine restitution advocacy now a small fraction of the claim's total activity — the 2020 reconversion spike reflects a burst of renewed rhetorical activity that settled back down by 2024 without any material change on the ground. Accessibility collapse is moderate (0.35): the historical fact of Byzantine founding is not contestable, but the normative conclusion (restitution or neutrality) remains one live option among several rather than a foreclosed near-certainty. Resistance is moderate-high (0.55), reflecting active Turkish state and domestic-Turkish pushback against the claim's framing.
 *
 * PERSPECTIVAL GAP:
 *   From the Ecumenical Patriarchate and Greek state's seats, this is a legitimate historical-memory and coordination function — keeping alive an accurate account of dispossession and cultural continuity. From Turkish sovereignty's seat, it is an ongoing, unenforceable but persistent external claim on internationally recognized territory that must be diplomatically managed at real cost. From the Muslim worshippers' seat, it is a background narrative that quietly frames their daily religious practice as provisional. The engine should compute divergent per-seat types from this same structural data — a beneficiary-side rope/coordination reading and a payer-side tangled_rope/extraction reading are both defensible from the authored structure without either being in error.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (Greek state, diaspora, Patriarchate) are coded near the beneficiary end of directionality because the claim's persistence — not its resolution — serves their diplomatic and identity interests; low material cost, high symbolic yield. Turkish state sovereignty is coded near the full-target end: trapped exit (cannot renounce sovereignty it already holds to make the claim disappear), institutional power, and a civilizational time horizon reflecting that the dispute recurs generation after generation. Muslim worshippers are coded as constrained payers: moderate power, no realistic exit from being caught in the symbolic crossfire, bearing a dignitary rather than material cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preserving memory of Byzantine dispossession) is genuinely old and its live/dead status is contested rather than settled either way — this prevents mislabeling the claim as pure cynical extraction (it retains real grief-memory and identity-coordination function for Orthodox communities) while also preventing a naive coordination-only reading that ignores the real diplomatic and dignitary costs it imposes on Turkish sovereignty and the site's Muslim congregation. Classifying as tangled_rope rather than snare or rope captures this: coordination function is real and named (diaspora and ecclesiastical identity), but so is asymmetric extraction (diplomatic leverage and reputational cost) requiring the Patriarchate's continued active liturgical/diplomatic maintenance to persist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the orthodox_restitution_reading a genuine claim of contested legitimacy, or is it better understood as a diplomatic and identity-coordination instrument that uses legitimacy language instrumentally, with restitution itself never seriously intended as an achievable outcome?',
    'Track whether Orthodox institutional actors (Patriarchate, Greek state) ever pursue restitution through actual legal or diplomatic channels with material commitment (formal claims before international courts, sustained multilateral diplomatic campaigns) versus confining the claim to commemorative statements and diaspora messaging.',
    'If restitution is never pursued through material channels, the claim''s coordination function (diaspora identity, diplomatic signaling) would be confirmed as dominant over any genuine legitimacy dispute, supporting the tangled_rope classification over a hypothetical high-extraction snare reading where actual dispossession of Turkish sovereignty were seriously attempted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether the restitution claim is a live legitimacy dispute or primarily an identity/diplomacy coordination instrument.').

omega_variable(
    sibling_reading_structural_delta,
    'Where exactly does the disagreement between this reading and the islamic_sovereignty_reading and universal_heritage_reading live — in the historical facts (which are largely uncontested: Byzantine founding, then 1453 conquest and endowment, then museum conversion, then 2020 remosqueification), or entirely in the normative weighting of which historical layer should determine present legitimacy?',
    'Compare the three kernel readings'' axiom sets directly: if all three readings agree on the sequence of historical facts but assign different normative priority to different layers (founding vs. conquest vs. universal heritage status), the disagreement is purely normative/conceptual, not empirical.',
    'If the disagreement is purely normative, no amount of additional historical evidence resolves the kernel contest — the three readings would remain permanently coexisting rather than one being correctable by better history. This affects how the cs_structure.reading_relations should be read across all three files.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Whether the kernel''s three readings disagree on history or only on normative weighting of undisputed history.').

omega_variable(
    diaspora_identity_versus_diplomatic_instrumentalization,
    'Is the primary beneficiary-side driver of this claim''s persistence genuine diaspora communal-identity need, or Greek state diplomatic instrumentalization of that need for leverage in unrelated bilateral disputes (Aegean territorial waters, Cyprus, minority rights)?',
    'Examine whether the claim''s public prominence correlates more tightly with the liturgical calendar and diaspora community events, or with spikes in unrelated Greek-Turkish diplomatic tension — a correlation with the latter would support the instrumentalization reading.',
    'If diplomatic instrumentalization dominates, the greek_state''s beneficiary directionality should be weighted more heavily than the diaspora''s, and the tangled_rope''s enforcement-requiring character (continual diplomatic maintenance) would be foregrounded over any pure identity-coordination story.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(diaspora_identity_versus_diplomatic_instrumentalization, empirical, 'Whether diaspora identity or state diplomatic leverage is the dominant driver of the claim''s persistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_substrate__orthodox_restitution_reading, 1453, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hagi_tr_t1453, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 1453, 0.3).
narrative_ontology:measurement_basis(hagi_tr_t1453, observed).
narrative_ontology:measurement(hagi_tr_t1923, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 1923, 0.4).
narrative_ontology:measurement_basis(hagi_tr_t1923, observed).
narrative_ontology:measurement(hagi_tr_t1934, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 1934, 0.45).
narrative_ontology:measurement_basis(hagi_tr_t1934, observed).
narrative_ontology:measurement(hagi_tr_t1974, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 1974, 0.5).
narrative_ontology:measurement_basis(hagi_tr_t1974, observed).
narrative_ontology:measurement(hagi_tr_t2000, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 2000, 0.55).
narrative_ontology:measurement_basis(hagi_tr_t2000, observed).
narrative_ontology:measurement(hagi_tr_t2020, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 2020, 0.68).
narrative_ontology:measurement_basis(hagi_tr_t2020, observed).
narrative_ontology:measurement(hagi_tr_t2024, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 2024, 0.62).
narrative_ontology:measurement_basis(hagi_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(hagi_be_t1453, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 1453, 0.05).
narrative_ontology:measurement_basis(hagi_be_t1453, observed).
narrative_ontology:measurement(hagi_be_t1923, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 1923, 0.08).
narrative_ontology:measurement_basis(hagi_be_t1923, observed).
narrative_ontology:measurement(hagi_be_t1934, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 1934, 0.1).
narrative_ontology:measurement_basis(hagi_be_t1934, observed).
narrative_ontology:measurement(hagi_be_t1974, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 1974, 0.18).
narrative_ontology:measurement_basis(hagi_be_t1974, observed).
narrative_ontology:measurement(hagi_be_t2000, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 2000, 0.2).
narrative_ontology:measurement_basis(hagi_be_t2000, observed).
narrative_ontology:measurement(hagi_be_t2020, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 2020, 0.32).
narrative_ontology:measurement_basis(hagi_be_t2020, observed).
narrative_ontology:measurement(hagi_be_t2024, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 2024, 0.28).
narrative_ontology:measurement_basis(hagi_be_t2024, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(hagia_sophia_substrate__orthodox_restitution_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hagia_sophia_substrate__orthodox_restitution_reading, identity_coordination).
narrative_ontology:affects_constraint(hagia_sophia_substrate__orthodox_restitution_reading, hagia_sophia_substrate__islamic_sovereignty_reading).
narrative_ontology:affects_constraint(hagia_sophia_substrate__orthodox_restitution_reading, hagia_sophia_substrate__universal_heritage_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the hagia_sophia_substrate kernel. islamic_sovereignty_reading grounds legitimacy in the 1453 conquest and continuous waqf endowment under actual functioning Turkish sovereign administration (expect a distinct, likely lower-extraction, rope-leaning structure from that reading's own beneficiaries' perspective, since it describes the status quo they administer). universal_heritage_reading grounds legitimacy in shared human cultural patrimony transcending any single national or religious claim (expect a coordination-heavy, low-extraction structure with UNESCO-type bodies as primary agenda setters). Each reading is authored with its own ε, its own beneficiary/victim structure, and its own claimed_type — none is derived from or averaged with the others; they are linked here only to preserve the kernel-contest network for downstream contamination and coupling analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
