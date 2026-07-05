% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__security_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy__security_necessity_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: territorial_legitimacy__security_necessity_reading
 *   human_readable: Security-Necessity Reading of Territorial Legitimacy (1967 Lines Plus Strategic Depth)
 *   domain: political theory / international law / territorial sovereignty
 *
 * SUMMARY:
 *   This story instantiates the security-necessity reading of the
 *   territorial-legitimacy kernel: the claim that Israeli control of the West
 *   Bank and Golan Heights beyond the 1948 armistice lines is legitimate
 *   because it constitutes indispensable strategic depth won defensively in
 *   1967, and that Palestinian sovereignty is properly conditional on
 *   demilitarization sufficient to neutralize that security concern, with
 *   settlement presence understood as a legitimate forward security posture
 *   rather than colonization. This is ONE of three structurally distinct
 *   constraints sharing a contested kernel over what legitimates territorial
 *   control in this dispute. The partition_reading (UN Resolution 181, 1948
 *   state recognition) and the indigenous_continuity_reading (continuous
 *   habitation, anti-colonial self-determination framing 1948 as Nakba) are
 *   separate constraints with their own ε, beneficiaries, and victims — they
 *   are not alternate measurements of this constraint, they are different
 *   constraints entirely, linked here only through the kernel network. In
 *   this reading specifically, the coordination function (a stable defense
 *   doctrine across changing governments) and the extraction function
 *   (structural subordination of Palestinian and Golan claimants'
 *   self-determination to a unilaterally administered security threshold) are
 *   both genuinely present, which is why this reading computes as
 *   tangled_rope rather than pure mountain or pure snare — it must be
 *   evaluated on its own terms, not blended with the sibling readings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__security_necessity_reading, 0.62).
domain_priors:suppression_score(territorial_legitimacy__security_necessity_reading, 0.71).
domain_priors:theater_ratio(territorial_legitimacy__security_necessity_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__security_necessity_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy__security_necessity_reading, "Security-Necessity Reading of Territorial Legitimacy (1967 Lines Plus Strategic Depth)").
narrative_ontology:topic_domain(territorial_legitimacy__security_necessity_reading, "political theory / international law / territorial sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy__security_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__security_necessity_reading, '3c05e1ef-b855-4216-b89f-18adbee158a6').
narrative_ontology:cs_kernel_codification('3c05e1ef-b855-4216-b89f-18adbee158a6', distributed).
narrative_ontology:cs_authority_grounding('3c05e1ef-b855-4216-b89f-18adbee158a6', distributed).
narrative_ontology:cs_reading_relation('3c05e1ef-b855-4216-b89f-18adbee158a6', territorial_legitimacy__partition_reading, influences).
narrative_ontology:cs_reading_relation('3c05e1ef-b855-4216-b89f-18adbee158a6', territorial_legitimacy__indigenous_continuity_reading, forecloses).
narrative_ontology:cs_axiom('3c05e1ef-b855-4216-b89f-18adbee158a6', foundational, defensible_borders_supersede_prior_armistice_lines).
narrative_ontology:cs_axiom_status(defensible_borders_supersede_prior_armistice_lines, holdable).
narrative_ontology:cs_axiom_grounding('3c05e1ef-b855-4216-b89f-18adbee158a6', defensible_borders_supersede_prior_armistice_lines, instrumental).
narrative_ontology:cs_axiom('3c05e1ef-b855-4216-b89f-18adbee158a6', foundational, sovereignty_conditional_on_demilitarization_verification).
narrative_ontology:cs_axiom_status(sovereignty_conditional_on_demilitarization_verification, holdable).
narrative_ontology:cs_axiom_grounding('3c05e1ef-b855-4216-b89f-18adbee158a6', sovereignty_conditional_on_demilitarization_verification, empirically_contingent).
narrative_ontology:cs_reference_frame('3c05e1ef-b855-4216-b89f-18adbee158a6', post_1967_defensive_war_settlement).
narrative_ontology:cs_drift_state('3c05e1ef-b855-4216-b89f-18adbee158a6', post_oslo_settlement_expansion_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3c05e1ef-b855-4216-b89f-18adbee158a6', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__security_necessity_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, israeli_state_security_establishment).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, settlement_population_west_bank).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, settlement_population_golan).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, west_bank_palestinian_residents).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, golan_druze_and_syrian_claimants).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, palestinian_statehood_movement).
narrative_ontology:constraint_vindicates(territorial_legitimacy__security_necessity_reading, defensible_borders_doctrine).
narrative_ontology:constraint_vindicates(territorial_legitimacy__security_necessity_reading, strategic_depth_necessity_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines and administers the security-necessity doctrine: designates which territories constitute indispensable strategic depth (Jordan Valley, Golan Heights escarpment, West Bank high ground overlooking the coastal plain), authorizes and protects settlement as a security presence, and sets the terms under which any Palestinian sovereignty would be conditioned on demilitarization and continued Israeli security control. Collects strategic and political benefit directly; can revise the doctrine's application but bears none of its territorial costs itself.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, israeli_state_security_establishment, agenda_setter,
    institutional, generational, arbitrage, national).

% Resides in West Bank settlements whose presence is legitimated under this reading as forward security infrastructure rather than colonization. Receives state subsidy, military protection, and infrastructure investment justified by the security-necessity framework. Exit from the settlement project would mean abandoning state-subsidized housing and the political-ideological project the doctrine underwrites.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, settlement_population_west_bank, beneficiary,
    organized, biographical, constrained, regional).

% Resides on the Golan Heights, annexed under a strategic-depth rationale (control of the escarpment overlooking the Galilee). Benefits from agricultural and residential development framed as consolidating a security buffer against Syria. Their tenure is structurally dependent on the security-necessity legitimation continuing to hold internationally and domestically.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, settlement_population_golan, beneficiary,
    organized, biographical, constrained, regional).

% Live under a military and administrative regime justified by the security-necessity doctrine: checkpoints, closed military zones, land expropriation for strategic buffers, and settlement expansion are all defended as necessary defensive measures. Any path to sovereignty is explicitly conditioned by this reading on demilitarization, meaning their political self-determination is structurally subordinated to an externally-defined security threshold they cannot themselves satisfy or negotiate away. No meaningful exit exists — residency and land are fixed facts under continuous administrative control.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, west_bank_palestinian_residents, payer,
    powerless, biographical, trapped, regional).

% Druze residents of the Golan and Syrian claimants to the territory bear the practical consequence of the strategic-depth rationale: annexation without their consent, restricted land use, and a permanent legal ambiguity over citizenship and property rights, all defended as a permanent security requirement rather than a negotiable political arrangement.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, golan_druze_and_syrian_claimants, payer,
    powerless, generational, trapped, regional).

% Palestinian political leadership and diaspora institutions seeking sovereignty find their claim structurally reframed under this reading: statehood becomes contingent on satisfying a security threshold set and adjudicated unilaterally by the other party to the dispute. They have no seat in defining what 'sufficient demilitarization' means, and their own competing legitimacy claims (partition-based or indigenous-continuity-based) are treated by this reading as subordinate to the security calculus.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, palestinian_statehood_movement, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__security_necessity_reading, palestinian_statehood_movement, excluded).

% UN bodies, the ICJ, and most international legal opinion hold that the security-necessity framing does not override the prohibition on acquiring territory by force or the illegality of settlement transfer under the Fourth Geneva Convention. Their rulings are advisory rather than enforceable against the doctrine's operation, so their voice is present in the discourse but structurally excluded from the enforcement mechanism itself.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, international_legal_bodies, excluded,
    institutional, generational, analytical, global).

% Assess whether specific territorial holdings (Jordan Valley presence, Golan escarpment control) provide militarily decisive advantage given modern missile and drone warfare, or whether the strategic-depth rationale has become increasingly disconnected from contemporary military reality since 1967.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, regional_security_analysts, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for coordinating Israeli state security planning: a shared doctrine among military planners, settlers, and political leadership about which territorial holdings are treated as non-negotiable for defense against conventional invasion and terrorism, allowing consistent policy across government changes.
% TRANSFER_FUNCTION: Moves land, water rights, freedom of movement, and political self-determination away from West Bank and Golan residents toward the Israeli state and settlement population, justified as the transfer price of defensible borders rather than as conquest or displacement.
% ABSENT_VOICES: Palestinian residents subject to the administrative regime and international legal bodies whose rulings on occupation and settlement illegality carry no enforcement weight within this reading's operative doctrine. Syrian claimants to the Golan are similarly outside the process that determines the territory's disposition.
% DISAPPEARANCE_RATIONALE: If the security-necessity legitimation collapsed as an accepted framework, the primary domestic and diplomatic justification for continued West Bank and Golan control would disappear, removing the rationale for settlement expansion, altering international diplomatic posture toward annexation, and forcing renegotiation of what territorial control Israel could defend on other grounds (historical, religious, or purely military-tactical claims lacking the doctrine's normative packaging).
% FOUNDING_PROBLEM: In 1967, Israel fought a war against neighboring states from within narrow, militarily vulnerable pre-war borders (the 9-mile-wide coastal waist, Syrian artillery positions overlooking Galilee farms, Jordanian control of West Bank high ground). The doctrine was built to prevent recurrence of that vulnerability by retaining territory that provides warning time and defensive terrain.
% FOUNDING_PROBLEM_CORROBORATION: Israeli military historians and security establishment figures attest the 1967 vulnerability was real and some strategic-depth logic remains militarily relevant. Independent military analysts (including some retired Israeli generals, e.g. the Council for Peace and Security) and international legal scholars outside the settlement movement corroborate that missile-age warfare has substantially eroded the tactical value of fixed territorial depth, and that the doctrine now functions primarily as political cover for settlement consolidation rather than as an operationally necessary defense posture.
narrative_ontology:disappearance_verdict(territorial_legitimacy__security_necessity_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy__security_necessity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__security_necessity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(territorial_legitimacy__security_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy__security_necessity_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy__security_necessity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy__security_necessity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy__security_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects substantial and rising transfer of land, water, and self-determination from West Bank/Golan populations to the Israeli state and settlement enterprise, justified through this doctrine specifically. Suppression (0.71) is high because administrative military control, checkpoints, and land expropriation are the active mechanisms by which the doctrine is operationalized against a population with no meaningful exit. Theater ratio (0.38) is moderate-rising: a genuine security-planning function persists (the doctrine is not pure pretext — some of its military logic was and to a lesser degree remains real), but an increasing share of settlement activity and territorial retention serves political/demographic consolidation rather than the original 1967 defensive rationale, especially as missile and drone warfare has eroded the tactical premise of fixed territorial depth. Accessibility collapse (0.48) is moderate rather than high: unlike a mountain, real alternative security arrangements (demilitarized Palestinian state with international guarantees, land swaps, multilateral security architecture) remain conceptually and diplomatically available, they are simply not adopted under this reading's operative framework. Resistance (0.78) is high, reflecting the doctrine's persistent international legal contestation, Palestinian political resistance, and internal Israeli debate over its continued military validity.
 *
 * PERSPECTIVAL GAP:
 *   From the israeli_state_security_establishment's seat, this reading computes as legitimate coordination around an existential defense requirement. From the west_bank_palestinian_residents' seat, the identical structure computes as extraction: enforced subordination of self-determination to an externally-imposed and unilaterally-defined threshold with no negotiated exit. The engine computes both from the same structural data — the divergence is the finding, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   The israeli_state_security_establishment sits at the agenda-setting/beneficiary pole: it defines the doctrine's application, bears none of the territorial cost, and derives strategic and political capital from its persistence. Settlement populations are direct material beneficiaries with constrained exit (their material stake is tied to the doctrine's continuation). West Bank Palestinian residents and Golan Druze/Syrian claimants sit at the full-target pole: trapped exit options, no voice in defining the security threshold that conditions their self-determination, and the direct payers of the land/water/mobility transfer. The palestinian_statehood_movement is a payer with a secondary excluded role — it has a diplomatic voice but no seat in adjudicating the security criteria this reading imposes on its own legitimacy claim.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1967 defensive vulnerability) was genuinely live at the doctrine's origin. Its status is now contested rather than clearly dead or clearly live: some strategic-depth logic persists, but corroboration from outside the beneficiary set (independent Israeli military analysts, international legal scholars) suggests missile-age warfare has substantially eroded the doctrine's original military justification while its territorial and demographic consequences have continued to accumulate and harden. This is the classic tangled-rope signature: a genuine coordination function at origin, increasingly serving as legitimating cover for continued extraction as the original necessity weakens — exactly the divergence the classification exists to surface, distinct from declaring the doctrine either purely defensive or purely pretextual.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    strategic_depth_military_validity_contemporary,
    'Does fixed territorial depth (West Bank high ground, Golan escarpment) still provide decisive defensive advantage given missile, drone, and precision-strike warfare, or has the doctrine''s core military premise been substantially overtaken by military-technological change since 1967?',
    'Independent military-technical assessment comparing the marginal defensive value of the retained territory against alternative security architectures (demilitarization verification regimes, early-warning technology, multilateral guarantees) under contemporary threat models.',
    'If the military premise has been substantially overtaken, the doctrine functions increasingly as political/demographic cover for settlement consolidation rather than a live security necessity, strengthening the classification toward snare; if the premise remains substantially valid, the coordination function is more genuinely load-bearing, strengthening the tangled_rope reading with a larger coordination component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_depth_military_validity_contemporary, empirical, 'Whether the 1967 strategic-depth military rationale remains operative under contemporary warfare.').

omega_variable(
    demilitarization_threshold_adjudication,
    'Is the demilitarization threshold that conditions Palestinian sovereignty under this reading a genuinely negotiable, objectively verifiable security standard, or is it a unilaterally-set and indefinitely-revisable bar that functions to permanently defer sovereignty?',
    'Historical review of negotiation records (Camp David 2000, Annapolis, subsequent rounds) to determine whether specific, satisfiable demilitarization criteria were ever formally offered and whether satisfying them was treated as sufficient for sovereignty recognition.',
    'If the threshold is genuinely negotiable and has been offered concretely, the conditionality is a legitimate security-coordination mechanism; if it has functioned as an indefinitely-revisable bar, the conditionality is better read as a structural mechanism for permanent extraction disguised as a security condition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demilitarization_threshold_adjudication, empirical, 'Whether Palestinian sovereignty conditionality under this reading is a genuine negotiable security standard or a permanently-deferring mechanism.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Given that the territorial_legitimacy kernel supports at least three structurally incompatible readings (security_necessity, partition, indigenous_continuity), what determines which reading a given international actor or domestic constituency adopts, and is that selection itself principled or interest-driven?',
    'Comparative analysis of which actors invoke which reading under which circumstances (e.g., whether security-necessity framing is invoked primarily when partition-based claims would be unfavorable, and vice versa), which would indicate motivated reading-selection rather than principled commitment to one legitimation framework.',
    'If reading-selection tracks strategic advantage rather than principled commitment, all three readings should be understood as competing legitimation resources deployed opportunistically rather than as genuinely held, stable normative frameworks — this would not change this story''s own ε but would contextualize why the kernel sustains three incompatible readings simultaneously.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether selection among the kernel''s competing readings is principled or strategically opportunistic.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__security_necessity_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1967, territorial_legitimacy__security_necessity_reading, theater_ratio, 1967, 0.15).
narrative_ontology:measurement(terr_tr_t1979, territorial_legitimacy__security_necessity_reading, theater_ratio, 1979, 0.2).
narrative_ontology:measurement(terr_tr_t1993, territorial_legitimacy__security_necessity_reading, theater_ratio, 1993, 0.25).
narrative_ontology:measurement(terr_tr_t2000, territorial_legitimacy__security_necessity_reading, theater_ratio, 2000, 0.29).
narrative_ontology:measurement(terr_tr_t2010, territorial_legitimacy__security_necessity_reading, theater_ratio, 2010, 0.34).
narrative_ontology:measurement(terr_tr_t2024, territorial_legitimacy__security_necessity_reading, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(terr_be_t1967, territorial_legitimacy__security_necessity_reading, base_extractiveness, 1967, 0.35).
narrative_ontology:measurement(terr_be_t1979, territorial_legitimacy__security_necessity_reading, base_extractiveness, 1979, 0.42).
narrative_ontology:measurement(terr_be_t1993, territorial_legitimacy__security_necessity_reading, base_extractiveness, 1993, 0.48).
narrative_ontology:measurement(terr_be_t2000, territorial_legitimacy__security_necessity_reading, base_extractiveness, 2000, 0.54).
narrative_ontology:measurement(terr_be_t2010, territorial_legitimacy__security_necessity_reading, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement(terr_be_t2024, territorial_legitimacy__security_necessity_reading, base_extractiveness, 2024, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1967, territorial_legitimacy__security_necessity_reading, suppression_requirement, 1967, 0.4).
narrative_ontology:measurement(terr_su_t1979, territorial_legitimacy__security_necessity_reading, suppression_requirement, 1979, 0.48).
narrative_ontology:measurement(terr_su_t1993, territorial_legitimacy__security_necessity_reading, suppression_requirement, 1993, 0.55).
narrative_ontology:measurement(terr_su_t2000, territorial_legitimacy__security_necessity_reading, suppression_requirement, 2000, 0.62).
narrative_ontology:measurement(terr_su_t2010, territorial_legitimacy__security_necessity_reading, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(terr_su_t2024, territorial_legitimacy__security_necessity_reading, suppression_requirement, 2024, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(territorial_legitimacy__security_necessity_reading, partition_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__security_necessity_reading, indigenous_continuity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints forming the territorial_legitimacy kernel family. security_necessity_reading, partition_reading, and indigenous_continuity_reading each instantiate a structurally distinct legitimation claim over the same disputed territory, with different beneficiaries, victims, and ε values. security_necessity_reading influences partition_reading because a security framework can reshape what territorial boundaries are treated as negotiable within a partition-based settlement (e.g., land-swap proposals grafted onto a partition logic). security_necessity_reading forecloses indigenous_continuity_reading at the level of a single legitimating framework: the security-necessity reading treats 1967 as the operative legitimating event and treats prior habitation claims as subordinate to post-1967 defensive necessity, while the indigenous_continuity_reading treats 1948 and pre-1948 continuous habitation as the sole legitimating ground and treats the 1967 conquest as itself illegitimate — no single coherent framework can hold both premises simultaneously as the primary legitimating basis, though different parties may hold either without holding both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
