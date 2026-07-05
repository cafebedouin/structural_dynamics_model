% ============================================================================
% CONSTRAINT STORY: npt_article_iv_vi_pairing__grand_bargain
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_article_iv_vi_pairing__grand_bargain, []).

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
 *   constraint_id: npt_article_iv_vi_pairing__grand_bargain
 *   human_readable: NPT Grand Bargain Reading: Article IV/VI as Reciprocal, Conditional Obligations
 *   domain: international_law/nuclear_governance/treaty_interpretation
 *
 * SUMMARY:
 *   This story instantiates the grand-bargain reading of the NPT kernel:
 *   Articles IV and VI are reciprocal, conditional obligations, such that
 *   non-weapon-state restraint is legally and morally contingent on
 *   weapon-state disarmament progress, and breach of Article VI erodes the
 *   legitimacy basis for continued Article IV/III enforcement. This is a
 *   distinct constraint from the nonproliferation_primary reading (which
 *   treats Article VI as non-justiciable and Article III verification as the
 *   operative gate) and the abolitionist reading (which treats Article IV
 *   itself as illegitimate under humanitarian-law framing). The three
 *   readings are not the same constraint measured differently — they have
 *   different beneficiary/victim structures, different enforceability claims,
 *   and different ε profiles, and are linked here only via network edges, per
 *   the ε-invariance principle.
 *
 * KEY AGENTS:
 *   - weapon_states_seeking_legitimacy: primary beneficiary/agenda_setter (institutional/arbitrage) — collects restraint, gates disarmament pace
 *   - non_weapon_states_bearing_restraint: primary payer (moderate/constrained) — bears restraint conditional on a promise not delivered
 *   - civil_nuclear_technology_recipients: secondary beneficiary/payer (moderate/constrained) — receives Article IV technology, dependent on weapon-state cooperation
 *   - disarmament_advocacy_coalitions: payer/excluded (organized/trapped) — presses the reciprocity claim, has no enforcement lever
 *   - iaea_and_verification_bodies: agenda_setter/observer (institutional/analytical) — verifies restraint half exhaustively, disarmament half not at all
 *   - review_conference_diplomatic_corps: observer (institutional/analytical) — the failed adjudication venue for the bargain's central claim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__grand_bargain, 0.58).
domain_priors:suppression_score(npt_article_iv_vi_pairing__grand_bargain, 0.52).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__grand_bargain, 0.47).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, extractiveness, 0.58).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 0.47).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__grand_bargain, tangled_rope).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__grand_bargain, "NPT Grand Bargain Reading: Article IV/VI as Reciprocal, Conditional Obligations").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__grand_bargain, "international_law/nuclear_governance/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__grand_bargain).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__grand_bargain, '80cd78f6-514d-40a2-b28a-8451042c19fc').
narrative_ontology:cs_kernel_codification('80cd78f6-514d-40a2-b28a-8451042c19fc', fixed_text).
narrative_ontology:cs_authority_grounding('80cd78f6-514d-40a2-b28a-8451042c19fc', lineage).
narrative_ontology:cs_interpretation_layer_present('80cd78f6-514d-40a2-b28a-8451042c19fc').
narrative_ontology:cs_reading_relation('80cd78f6-514d-40a2-b28a-8451042c19fc', npt_article_iv_vi_pairing__nonproliferation_primary, coexists_with).
narrative_ontology:cs_reading_relation('80cd78f6-514d-40a2-b28a-8451042c19fc', npt_article_iv_vi_pairing__abolitionist, influences).
narrative_ontology:cs_axiom('80cd78f6-514d-40a2-b28a-8451042c19fc', foundational, article_vi_conditionality_binds_article_iv).
narrative_ontology:cs_axiom_status(article_vi_conditionality_binds_article_iv, holdable).
narrative_ontology:cs_axiom_grounding('80cd78f6-514d-40a2-b28a-8451042c19fc', article_vi_conditionality_binds_article_iv, conventional).
narrative_ontology:cs_axiom('80cd78f6-514d-40a2-b28a-8451042c19fc', foundational, disarmament_breach_licenses_restraint_reconsideration).
narrative_ontology:cs_axiom_status(disarmament_breach_licenses_restraint_reconsideration, holdable).
narrative_ontology:cs_axiom_grounding('80cd78f6-514d-40a2-b28a-8451042c19fc', disarmament_breach_licenses_restraint_reconsideration, instrumental).
narrative_ontology:cs_reference_frame('80cd78f6-514d-40a2-b28a-8451042c19fc', conditional_reciprocal_bargain_1968).
narrative_ontology:cs_drift_state('80cd78f6-514d-40a2-b28a-8451042c19fc', post_2015_review_conference_failure, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('80cd78f6-514d-40a2-b28a-8451042c19fc', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__grand_bargain, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__grand_bargain, weapon_states_seeking_legitimacy).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__grand_bargain, civil_nuclear_technology_recipients).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__grand_bargain, non_weapon_states_bearing_restraint).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__grand_bargain, disarmament_advocacy_coalitions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__grand_bargain, civil_nuclear_technology_recipients).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__grand_bargain, reciprocal_bargain_doctrine).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__grand_bargain, conditional_legitimacy_of_nonproliferation_regime).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain nuclear arsenals while citing Article IV's promise of civil nuclear cooperation and their own selective disarmament gestures (New START-style bilateral reductions, rhetorical commitments at Review Conferences) as evidence of good-faith compliance with Article VI. They control the diplomatic machinery that decides what counts as 'progress' toward disarmament, and they administer export-control regimes that gate the civil nuclear technology transfers Article IV promises. Their exit from binding obligation is effectively permanent: no enforcement mechanism can compel disarmament against their will.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, weapon_states_seeking_legitimacy, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__grand_bargain, weapon_states_seeking_legitimacy, agenda_setter).

% Forgo weapons development in exchange for a promised reciprocal disarmament trajectory that has stalled for five decades. Under the grand-bargain reading they hold a live legal argument that continued restraint is conditional on weapon-state Article VI performance, but exercising that argument (formal withdrawal, or claiming Article IV expansion as compensation) risks sanctions, loss of civil nuclear cooperation, and diplomatic isolation. Their exit is constrained rather than trapped: NPT Article X permits withdrawal, but the political and economic cost is severe, and no state has withdrawn successfully citing Article VI failure without consequence.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, non_weapon_states_bearing_restraint, payer,
    moderate, generational, constrained, global).

% Receive nuclear energy technology and fuel-cycle assistance under Article IV's inalienable-right clause, contingent on IAEA safeguards compliance under Article III. They benefit from the bargain's technology-transfer half but bear the restraint half, and depend on continued weapon-state cooperation for reactor fuel, spare parts, and safeguards accommodation — a dependency that constrains any Article VI enforcement posture they might otherwise adopt.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, civil_nuclear_technology_recipients, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__grand_bargain, civil_nuclear_technology_recipients, payer).

% NGOs, non-weapon-state diplomatic blocs (New Agenda Coalition, Non-Aligned Movement), and civil-society campaigns press for Article VI justiciability and concrete disarmament timelines at Review Conferences. They bear the cost of the bargain's non-enforcement — decades of NPT Review Conference failures to reach consensus documents — but have no mechanism to compel weapon-state compliance beyond moral suasion and procedural obstruction. Their only leverage is withholding consensus at Review Conferences, a blunt instrument that damages the regime without extracting disarmament.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, disarmament_advocacy_coalitions, payer,
    organized, civilizational, trapped, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__grand_bargain, disarmament_advocacy_coalitions, excluded).

% Administer Article III safeguards verification for non-weapon states in exhaustive technical detail, but have no comparable mandate or resourcing to verify weapon-state Article VI disarmament progress. This verification asymmetry is structural under the grand-bargain reading: the reading demands 'verification reciprocity,' which does not currently exist and whose absence is itself evidence for the reading's central claim that the bargain is breached.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, iaea_and_verification_bodies, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__grand_bargain, iaea_and_verification_bodies, observer).

% Convene every five years to assess NPT implementation. Under the grand-bargain reading, their function is to adjudicate whether Article VI progress has been sufficient to sustain Article IV/III obligations — a function they have been structurally unable to perform, evidenced by repeated failure to adopt consensus final documents since 2005 and 2015.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, review_conference_diplomatic_corps, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine mutual-restraint problem: non-weapon states forgo the security and status benefits of nuclear weapons, and in exchange receive civil nuclear technology access and a credible trajectory toward a world with fewer weapons overall, reducing systemic proliferation and use risk for all parties.
% TRANSFER_FUNCTION: Moves restraint (weapons abstention, safeguards compliance, technology dependency) from non-weapon states to the regime, and is supposed to move disarmament (stockpile reduction, doctrine change) from weapon states to the regime in return; under the grand-bargain reading, the disarmament half of the transfer has been substantially withheld while the restraint half has been fully collected.
% ABSENT_VOICES: States that have withdrawn or never joined (DPRK, India, Pakistan, Israel) are structurally outside the bargain's adjudication entirely and are not heard in Review Conference proceedings, even though their existence is direct evidence bearing on whether the nonproliferation half of the bargain is working. Hibakusha and affected-community voices from weapons testing are present at TPNW forums but largely absent from NPT Review Conference formal proceedings.
% DISAPPEARANCE_RATIONALE: If the reciprocal-obligation reading of Articles IV/VI were formally abandoned or reinterpreted as fully severable (the nonproliferation_primary reading), the diplomatic leverage non-weapon states currently claim at Review Conferences would evaporate, weapon states would face no rhetorical cost for stalled disarmament, and the New Agenda Coalition's central legal argument would lose its textual anchor — the entire Review Conference conflict structure since 1995 would reorganize around a different set of claims.
% FOUNDING_PROBLEM: The 1968 negotiation needed to induce non-weapon states to forgo weapons acquisition permanently while weapon states retained theirs temporarily; the bargain structure (restraint now for disarmament later, sweetened by civil technology access) was the price of getting near-universal accession.
% FOUNDING_PROBLEM_CORROBORATION: Non-weapon-state diplomatic blocs (New Agenda Coalition working papers, Non-Aligned Movement statements) and independent arms-control scholarship (SIPRI yearbooks documenting continued warhead modernization) attest the disarmament half of the founding bargain remains substantially unmet decades after ratification. Weapon states' own Review Conference statements assert the bargain's disarmament component is 'aspirational' and progressing on its own terms — a claim made by the benefiting party about its own performance, which is why outside corroboration from arms-control monitoring organizations and non-weapon-state coalitions is load-bearing here rather than incidental.
narrative_ontology:disappearance_verdict(npt_article_iv_vi_pairing__grand_bargain, world_rearranges).
narrative_ontology:founding_problem_status(npt_article_iv_vi_pairing__grand_bargain, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__grand_bargain, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(npt_article_iv_vi_pairing__grand_bargain, 'none', 1).
narrative_ontology:epsilon_provenance(npt_article_iv_vi_pairing__grand_bargain, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_article_iv_vi_pairing__grand_bargain_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_article_iv_vi_pairing__grand_bargain, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_article_iv_vi_pairing__grand_bargain_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects that under this reading, non-weapon states have fully performed their half of the bargain (permanent weapons abstention, comprehensive safeguards) while weapon states have performed only selectively (bilateral arsenal reductions well short of elimination, continued modernization programs), yet the treaty structure continues to bind non-weapon states as if the bargain were current. Suppression (0.52) captures that exit via Article X withdrawal is nominally available but carries severe diplomatic and economic cost, and that no state has successfully exercised an Article VI-breach withdrawal claim without punitive consequence — the reciprocity this reading asserts is legally live but practically unenforceable. Theater ratio (0.47) reflects that Review Conferences increasingly perform disarmament dialogue (working groups, consensus-document drafting, high-level statements) without producing binding disarmament commitments — the ritual of assessing Article VI progress has grown even as substantive progress has plateaued. Accessibility collapse is moderate (0.4): non-weapon states retain a real, if costly, legal alternative (withdrawal, TPNW accession) that has not fully closed off. Resistance is high (0.68): disarmament coalitions actively contest the bargain's non-performance at every Review Conference, and consensus final documents have failed repeatedly (2005, 2015, partial 2022) precisely because of this contestation.
 *
 * DIRECTIONALITY LOGIC:
 *   Weapon states sit near the beneficiary end: they collect the nonproliferation benefit (reduced horizontal proliferation risk, preserved strategic advantage) while controlling the pace and definition of their own reciprocal obligation, and their exit option is effectively arbitrage — they can reframe compliance standards unilaterally. Non-weapon states sit near the target end: their restraint is collected in full and immediately, while the promised reciprocal benefit remains deferred indefinitely, and their exit (withdrawal) is constrained by asymmetric cost. Civil nuclear technology recipients occupy a mixed position — real technology benefit flows to them, but their exit from the restraint obligation is even more constrained because withdrawal risks losing fuel-cycle cooperation. Disarmament advocacy coalitions bear the coordination failure most acutely with no compensating benefit and no meaningful exit, which is why they are marked trapped rather than merely constrained.
 *
 * MANDATROPHY ANALYSIS:
 *   The grand-bargain reading is precisely the frame that prevents this constraint from being mislabeled as pure coordination (a rope) on one side or pure extraction (a snare) on the other. It preserves the genuine coordination function (mutual restraint reducing systemic proliferation risk really did produce a safer world than an unconstrained nuclear arms race) while naming the asymmetric extraction (non-weapon states' restraint has been fully collected while weapon states' reciprocal obligation has not been comparably enforced) as coexisting within the same structure. This is the tangled-rope signature: both a genuine coordination function AND asymmetric extraction requiring active enforcement (safeguards inspections, export-control regimes, diplomatic pressure on would-be withdrawers) to hold. Treating the pairing as purely aspirational (nonproliferation_primary) would erase the extraction; treating Article IV as illegitimate outright (abolitionist) would erase the coordination benefit both readings' proponents still rely on.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reciprocity_justiciability_ambiguity,
    'Is Article VI''s disarmament obligation legally enforceable as a condition on Article IV/III compliance, or is it a non-binding aspiration severable from the treaty''s operative nonproliferation machinery?',
    'An International Court of Justice advisory opinion or binding arbitration directly addressing Article VI justiciability (the 1996 ICJ advisory opinion touched disarmament obligation but did not resolve severability from Article IV); alternatively, a Review Conference consensus document explicitly linking the two articles'' enforcement.',
    'If Article VI is held justiciable and linked, the grand-bargain reading gains formal legal standing and non-weapon-state withdrawal-on-breach claims become credible, sharply raising suppression''s practical cost to weapon states. If held non-justiciable and severable, this reading collapses toward the nonproliferation_primary reading and the extraction becomes uncontestable within the regime''s own legal framework.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reciprocity_justiciability_ambiguity, conceptual, 'Whether the reciprocal-conditionality claim central to this reading has any binding legal force, or is purely a diplomatic and moral argument.').

omega_variable(
    verification_reciprocity_feasibility,
    'Is symmetric verification of weapon-state disarmament progress (comparable in rigor to IAEA Article III safeguards on non-weapon states) technically and politically feasible, or does classified weapons-design information make genuine reciprocal verification structurally impossible regardless of political will?',
    'Assessment of existing weapon-state transparency initiatives (US-Russia New START verification protocols, UK-Norway warhead dismantlement verification pilot) against the technical bar IAEA safeguards meet for non-weapon states.',
    'If reciprocal verification is technically feasible and merely politically withheld, the extraction reading strengthens (deliberate asymmetry). If it is structurally infeasible due to weapons-design secrecy requirements, part of the measured suppression reflects an irreducible technical constraint rather than pure extraction, which would lower the defensible ε.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_reciprocity_feasibility, empirical, 'Whether the verification asymmetry this reading treats as a breach indicator is a political choice or a technical floor.').

omega_variable(
    sibling_reading_selection_pressure,
    'Which reading of the kernel (grand_bargain, nonproliferation_primary, abolitionist) will prevail as the operative interpretation in future Review Conferences and state practice, and does the choice track legal merit or raw power distribution among weapon and non-weapon states?',
    'Track Review Conference outcome documents, ICJ jurisprudence trends, and TPNW accession patterns over successive five-year cycles; a shift toward TPNW-aligned state practice would signal abolitionist ascendance, while continued weapon-state control of consensus language without disarmament linkage would signal nonproliferation_primary ascendance.',
    'The kernel''s practical operative reading determines which constraint''s classification actually governs state behavior; this story''s tangled_rope classification is contingent on the grand_bargain reading remaining a live, contested frame rather than being definitively foreclosed by state practice in favor of one sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_selection_pressure, conceptual, 'Documents that this is one reading among three live contenders, and that the contest''s outcome is itself an open structural question this story does not resolve.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__grand_bargain, 1968, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1968, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 1968, 0.2).
narrative_ontology:measurement(npt__tr_t1985, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 1985, 0.28).
narrative_ontology:measurement(npt__tr_t2000, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(npt__tr_t2010, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(npt__tr_t2015, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 2015, 0.44).
narrative_ontology:measurement(npt__tr_t2025, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 2025, 0.47).

% Extraction over time
narrative_ontology:measurement(npt__be_t1968, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 1968, 0.32).
narrative_ontology:measurement(npt__be_t1985, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 1985, 0.4).
narrative_ontology:measurement(npt__be_t2000, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 2000, 0.46).
narrative_ontology:measurement(npt__be_t2010, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 2010, 0.52).
narrative_ontology:measurement(npt__be_t2015, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 2015, 0.55).
narrative_ontology:measurement(npt__be_t2025, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1968, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 1968, 0.3).
narrative_ontology:measurement(npt__su_t1985, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 1985, 0.36).
narrative_ontology:measurement(npt__su_t2000, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 2000, 0.42).
narrative_ontology:measurement(npt__su_t2010, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 2010, 0.47).
narrative_ontology:measurement(npt__su_t2015, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 2015, 0.5).
narrative_ontology:measurement(npt__su_t2025, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 2025, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__grand_bargain, npt_article_iv_vi_pairing__nonproliferation_primary).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__grand_bargain, npt_article_iv_vi_pairing__abolitionist).

% DUAL FORMULATION NOTE:
% This story is one of three linked constraints decomposing the natural-language label 'the NPT Article IV/VI relationship' per the ε-invariance principle. Each reading (grand_bargain, nonproliferation_primary, abolitionist) has a distinct ε, distinct beneficiary/victim structure, and distinct claimed type, because each reading answers 'what does the treaty text actually obligate' differently, and that answer changes who benefits and who pays. They are not the same constraint viewed from different angles — the abolitionist reading would likely classify as snare or tangled_rope with a different victim set (populations at risk from any nuclear weapons existing, rather than non-weapon states specifically), and the nonproliferation_primary reading would likely classify closer to rope or tangled_rope with substantially lower ε (no enforceable disarmament obligation means no unmet reciprocal claim to extract against). All three should be authored as separate files and linked via network edges, never merged into one story with a reading parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
