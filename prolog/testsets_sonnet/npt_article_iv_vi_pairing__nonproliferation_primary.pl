% ============================================================================
% CONSTRAINT STORY: npt_article_iv_vi_pairing__nonproliferation_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_article_iv_vi_pairing__nonproliferation_primary, []).

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
 *   constraint_id: npt_article_iv_vi_pairing__nonproliferation_primary
 *   human_readable: NPT Article IV/VI Pairing — Nonproliferation-Primary Reading
 *   domain: international_law/nuclear_governance
 *
 * SUMMARY:
 *   This story instantiates the nonproliferation-primary reading of the NPT
 *   Article IV/VI kernel: Article IV's civilian-technology promise is treated
 *   as conditional on Article III verification compliance, Article VI's
 *   disarmament language is treated as aspirational and non-justiciable, and
 *   the treaty's authority is grounded in weapon-state security interest in
 *   preventing horizontal proliferation. This is a distinct constraint from
 *   the grand_bargain reading (which treats Article IV and VI as reciprocal,
 *   binding obligations) and the abolitionist reading (which treats Article
 *   IV as illegitimate absent disarmament progress and grounds authority in
 *   humanitarian weapons-prohibition law). Each reading has its own ε, its
 *   own beneficiary/victim structure, and its own classification; they are
 *   linked as siblings in the same kernel contest, not merged into one story.
 *
 * KEY AGENTS:
 *   - nuclear_weapon_states: agenda_setter/beneficiary (institutional/arbitrage) — set verification architecture, exempt own arsenals
 *   - non_weapon_states_parties: payer (moderate/constrained) — bear compliance costs, no adjudicative forum for Article VI leverage
 *   - iaea_safeguards_apparatus: agenda_setter/beneficiary (institutional/arbitrage) — administers asymmetric verification mandate
 *   - nuclear_supplier_group_members: beneficiary (organized/arbitrage) — gatekeep scarce technology transfer
 *   - disarmament_advocacy_coalition: excluded (moderate/analytical) — objects at Review Conferences, no binding forum
 *   - treaty_law_scholars: observer (analytical/analytical) — assess textual and structural asymmetry
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__nonproliferation_primary, 0.68).
domain_priors:suppression_score(npt_article_iv_vi_pairing__nonproliferation_primary, 0.72).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__nonproliferation_primary, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__nonproliferation_primary, tangled_rope).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__nonproliferation_primary, "NPT Article IV/VI Pairing — Nonproliferation-Primary Reading").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__nonproliferation_primary, "international_law/nuclear_governance").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__nonproliferation_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__nonproliferation_primary, '5159034f-cd51-4384-989c-9fe2a3997d83').
narrative_ontology:cs_kernel_codification('5159034f-cd51-4384-989c-9fe2a3997d83', fixed_text).
narrative_ontology:cs_authority_grounding('5159034f-cd51-4384-989c-9fe2a3997d83', extraction).
narrative_ontology:cs_interpretation_layer_present('5159034f-cd51-4384-989c-9fe2a3997d83').
narrative_ontology:cs_reading_relation('5159034f-cd51-4384-989c-9fe2a3997d83', npt_article_iv_vi_pairing__grand_bargain, coexists_with).
narrative_ontology:cs_reading_relation('5159034f-cd51-4384-989c-9fe2a3997d83', npt_article_iv_vi_pairing__abolitionist, coexists_with).
narrative_ontology:cs_axiom('5159034f-cd51-4384-989c-9fe2a3997d83', foundational, security_interest_grounds_treaty_authority).
narrative_ontology:cs_axiom_status(security_interest_grounds_treaty_authority, holdable).
narrative_ontology:cs_axiom_grounding('5159034f-cd51-4384-989c-9fe2a3997d83', security_interest_grounds_treaty_authority, instrumental).
narrative_ontology:cs_axiom('5159034f-cd51-4384-989c-9fe2a3997d83', foundational, article_vi_is_non_binding_aspiration).
narrative_ontology:cs_axiom_status(article_vi_is_non_binding_aspiration, holdable).
narrative_ontology:cs_axiom_grounding('5159034f-cd51-4384-989c-9fe2a3997d83', article_vi_is_non_binding_aspiration, conventional).
narrative_ontology:cs_reference_frame('5159034f-cd51-4384-989c-9fe2a3997d83', cold_war_horizontal_proliferation_containment).
narrative_ontology:cs_drift_state('5159034f-cd51-4384-989c-9fe2a3997d83', post_cold_war_multipolar_proliferation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5159034f-cd51-4384-989c-9fe2a3997d83', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__nonproliferation_primary, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__nonproliferation_primary, iaea_safeguards_apparatus).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__nonproliferation_primary, nuclear_supplier_group_members).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__nonproliferation_primary, non_weapon_states_parties).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__nonproliferation_primary, civilian_nuclear_technology_seekers).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__nonproliferation_primary, horizontal_nonproliferation_as_primary_treaty_object).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The five NPT-recognized weapon states set and enforce the verification architecture that gates Article IV civilian-technology access, while their own arsenals sit outside any comparable treaty-enforced inspection regime. They frame Article VI's disarmament language as aspirational, non-binding, and non-justiciable — a political commitment rather than a legal obligation subject to remedy. Their security calculus (preventing horizontal proliferation among rivals) is the stated authority for the entire enforcement structure, and they retain full discretion over Nuclear Suppliers Group export decisions and IAEA Board dynamics.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, nuclear_weapon_states, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__nonproliferation_primary, nuclear_weapon_states, beneficiary).

% Non-weapon states accepted permanent renunciation of nuclear weapons and submit to intrusive IAEA safeguards as the price of accessing civilian nuclear technology under Article IV — technology transfer that in practice is further conditioned, delayed, or denied by supplier-state discretion. Their leverage instrument (linking their restraint to weapon-state disarmament progress under Article VI) has no adjudicative forum; treaty withdrawal is technically permitted under Article X but is treated as a hostile act carrying severe diplomatic and security costs, making exit largely theoretical.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, non_weapon_states_parties, payer,
    moderate, generational, constrained, global).

% Administers Article III verification, gates Article IV technology transfer on compliance findings, and derives its institutional mandate, budget, and relevance entirely from the nonproliferation-primary reading of the treaty. Has no verification mandate over weapon-state arsenals themselves, only over non-weapon-state civilian programs — a scope asymmetry it did not create but actively administers.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, iaea_safeguards_apparatus, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__nonproliferation_primary, iaea_safeguards_apparatus, beneficiary).

% Control export of enrichment, reprocessing, and reactor technology to Article IV claimants. Benefit commercially and strategically from being the gatekeepers of a scarce good conditioned on a compliance regime they help define; can tighten or loosen supply criteria unilaterally in ways the treaty text does not fully anticipate.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, nuclear_supplier_group_members, beneficiary,
    organized, generational, arbitrage, global).

% States and state agencies pursuing peaceful nuclear energy, medical isotopes, or research reactors under Article IV's promised 'inalienable right' find that right operationally contingent on supplier-state political judgment layered atop IAEA findings — a right that reads as unconditional in the text but is administered as conditional in practice. Alternative suppliers outside the NSG framework are limited and carry their own geopolitical costs.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, civilian_nuclear_technology_seekers, payer,
    moderate, biographical, trapped, national).

% Civil society coalitions and non-weapon-state diplomatic blocs (e.g. New Agenda Coalition) argue Article VI creates real, time-bound obligations and that its non-justiciability is a political choice, not a textual necessity. They are heard at NPT Review Conferences but have no forum with binding authority over weapon-state arsenal reduction; their objections are recorded, not remedied.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, disarmament_advocacy_coalition, excluded,
    moderate, generational, analytical, global).

% Analyze the structural asymmetry between the treaty's enforceable half (Article III/IV) and its unenforceable half (Article VI), and assess whether the nonproliferation-primary reading is a defensible textual interpretation or a power-driven asymmetry dressed as legal necessity.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, treaty_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_article_iv_vi_pairing__nonproliferation_primary, nuclear_weapon_states).
narrative_ontology:fixing_cost_class(npt_article_iv_vi_pairing__nonproliferation_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Genuinely solves a real collective-action problem: without some verification-gated technology-sharing arrangement, states pursuing civilian nuclear programs would have stronger incentives to develop indigenous enrichment/reprocessing capacity covertly, increasing weapons-proliferation risk for everyone, including non-weapon states' regional rivals.
% TRANSFER_FUNCTION: Moves permanent security guarantees and unmonitored strategic latitude to the weapon states, and moves compliance costs, sovereignty constraints, and technology-access uncertainty onto non-weapon states — in exchange for a promise of eventual disarmament that carries no enforcement mechanism.
% ABSENT_VOICES: The disarmament advocacy coalition and non-weapon-state blocs object to non-justiciability at every Review Conference but have no forum empowered to bind weapon-state conduct; their objections are procedurally noted and substantively unaddressed.
% DISAPPEARANCE_RATIONALE: If this specific reading collapsed — if Article VI became justiciable or Article IV access were decoupled from supplier discretion — the entire architecture of technology-transfer leverage, IAEA safeguards administration, and NSG gatekeeping would need to be renegotiated; weapon states would lose the framing that currently insulates their arsenals from treaty-based scrutiny.
% FOUNDING_PROBLEM: In 1968, the treaty was built to solve a specific security problem: prevent additional states from acquiring nuclear weapons during a Cold War period when rapid horizontal proliferation seemed likely, while offering non-weapon states a face-saving trade (civilian technology access plus a disarmament promise) in exchange for renouncing weapons programs.
% FOUNDING_PROBLEM_CORROBORATION: Weapon states and the IAEA apparatus attest the founding problem (horizontal proliferation risk) remains fully live and justifies continued asymmetric enforcement. Independent nuclear-policy scholarship (e.g. work cited at NPT Review Conferences by non-aligned blocs) and the 2017 TPNW negotiating history attest that the reciprocal half of the founding bargain — disarmament progress — has stalled for five decades, and that the nonproliferation-primary reading persists in that asymmetric form due to the leverage of the states it favors, not because the coordination problem it names has changed.
narrative_ontology:disappearance_verdict(npt_article_iv_vi_pairing__nonproliferation_primary, world_rearranges).
narrative_ontology:founding_problem_status(npt_article_iv_vi_pairing__nonproliferation_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__nonproliferation_primary, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(npt_article_iv_vi_pairing__nonproliferation_primary, 'none', 1).
narrative_ontology:epsilon_provenance(npt_article_iv_vi_pairing__nonproliferation_primary, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_article_iv_vi_pairing__nonproliferation_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_article_iv_vi_pairing__nonproliferation_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_article_iv_vi_pairing__nonproliferation_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.42 at treaty entry into force to 0.68 by 2026, reflecting the accumulation of asymmetric enforcement: verification and supply-gating machinery around Article IV matured and hardened while Article VI's disarmament promise generated no comparable enforcement infrastructure. Theater ratio rises correspondingly (0.20 to 0.45) as Review Conference proceedings increasingly perform disarmament dialogue without producing binding commitments — the 'strengthened review process' language substitutes for justiciable obligation. Suppression (0.72 by 2026) reflects the real structural cost of Article X withdrawal being treated as a hostile act, and the diplomatic isolation faced by states that challenge the nonproliferation-primary framing directly.
 *
 * PERSPECTIVAL GAP:
 *   From the weapon-state seat, the arrangement is a stable, functioning coordination mechanism that has prevented mass horizontal proliferation for over five decades — real coordination value, genuinely delivered. From the non-weapon-state seat, the same structure is an indefinitely renewed one-way transfer: permanent restraint obligations in exchange for a disarmament promise with no remedy when breached. The engine should register this as a structural asymmetry inherent in the nonproliferation-primary reading itself, not as a measurement artifact — the divergence is what this specific reading structurally produces, distinct from what the grand_bargain reading would produce from the same text.
 *
 * DIRECTIONALITY LOGIC:
 *   Weapon states and the IAEA apparatus sit near the beneficiary end: they administer the enforceable half of the treaty while remaining outside its scope, and derive institutional or strategic value from the current asymmetry (d low). Non-weapon states and civilian technology seekers sit near the target end: they bear the enforceable obligations (safeguards, renunciation) while receiving unenforceable reciprocal promises (d high). The disarmament advocacy coalition experiences maximum extraction with minimum recourse — excluded from any binding forum. Nuclear Suppliers Group members occupy a distinct beneficiary position: they benefit from gatekeeping scarce technology, a position not directly created by Article VI's non-justiciability but reinforced by it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (horizontal proliferation risk in a bipolar Cold War context) is genuinely still partially live — new proliferation risks exist (DPRK, Iran's program history, potential future breakout states) — so labeling this a pure zombie mandate would be wrong. But the nonproliferation-primary reading's specific claim that Article VI is non-justiciable is not required by the founding problem; it is a subsequent interpretive choice that stabilizes the asymmetry indefinitely rather than tying it to disarmament milestones. This is exactly why the classification sits at tangled_rope rather than snare or mountain: a genuine coordination function (preventing horizontal proliferation) is layered with asymmetric extraction (permanent restraint-bearing without reciprocal enforcement) that requires active enforcement (safeguards regime, supplier discretion, diplomatic costs of Article X exit) to persist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_justiciability_ambiguity,
    'Is Article VI''s non-justiciability a textually necessary reading of the treaty, or a power-driven interpretive choice sustained by weapon-state leverage over dispute-resolution forums?',
    'Comparative treaty-law analysis of similarly worded ''good faith negotiation'' clauses in other multilateral instruments that HAVE been treated as enforceable, plus examination of ICJ advisory opinions (1996) that characterized Article VI obligations as requiring good-faith pursuit of negotiations.',
    'If Article VI is textually justiciable and the non-justiciability is a political imposition, this reading is closer to a constructed asymmetry (supporting a higher-extraction, more snare-like classification) rather than a natural consequence of ambiguous treaty language.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_justiciability_ambiguity, conceptual, 'Whether non-justiciability is textually required or interpretively imposed by the dominant reading.').

omega_variable(
    kernel_reading_contest_location,
    'Where exactly does the disagreement among the three kernel readings (nonproliferation_primary, grand_bargain, abolitionist) live structurally?',
    'This is not resolvable by new data — it is a standing interpretive contest located in: (1) whether Article IV and Article VI are read as conditional-on-each-other or independent clauses, (2) whether ''good faith'' negotiation language in Article VI creates a legal obligation or a political aspiration, and (3) whether authority for the whole treaty derives from weapon-state security interest, mutual reciprocal bargain, or humanitarian law precedent external to the treaty. The nonproliferation_primary reading resolves all three toward independence, aspiration, and security-interest grounding respectively; the grand_bargain reading resolves toward conditionality and legal obligation; the abolitionist reading resolves toward external humanitarian-law override.',
    'Adopting the grand_bargain reading would reclassify non-weapon-state restraint as conditional rather than permanent, sharply reducing this reading''s extraction structure since the enforcement mechanism (unconditional restraint regardless of disarmament progress) would no longer exist. Adopting the abolitionist reading would treat the entire Article IV grant as illegitimate, changing which parties are beneficiaries versus victims entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Documents the precise structural location of the three-way kernel contest for cross-reading reference.').

omega_variable(
    weapon_state_security_interest_as_authority_ground,
    'Does grounding treaty authority in weapon-state security interest (rather than mutual consent or humanitarian precedent) itself constitute an admission that the arrangement is extractive rather than coordinative?',
    'Examine whether weapon states would have ratified an Article VI with binding disarmament timelines and enforcement mechanisms — historical negotiating record (1965-1968) suggests this was never on the table, indicating the security-interest grounding was primary from inception, not a later drift.',
    'If weapon-state security interest was always the primary authority ground (not a later capture of an originally reciprocal bargain), this reading is arguably the most historically faithful of the three, even though it produces the highest measured extraction of the sibling set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(weapon_state_security_interest_as_authority_ground, empirical, 'Whether the security-interest authority ground reflects original treaty design or later interpretive drift.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__nonproliferation_primary, 1968, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1968, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 1968, 0.2).
narrative_ontology:measurement(npt__tr_t1980, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 1980, 0.28).
narrative_ontology:measurement(npt__tr_t1995, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 1995, 0.35).
narrative_ontology:measurement(npt__tr_t2010, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(npt__tr_t2020, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 2020, 0.44).
narrative_ontology:measurement(npt__tr_t2026, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 2026, 0.45).

% Extraction over time
narrative_ontology:measurement(npt__be_t1968, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 1968, 0.42).
narrative_ontology:measurement(npt__be_t1980, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 1980, 0.5).
narrative_ontology:measurement(npt__be_t1995, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 1995, 0.58).
narrative_ontology:measurement(npt__be_t2010, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 2010, 0.64).
narrative_ontology:measurement(npt__be_t2020, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 2020, 0.67).
narrative_ontology:measurement(npt__be_t2026, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 2026, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1968, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 1968, 0.5).
narrative_ontology:measurement(npt__su_t1980, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 1980, 0.58).
narrative_ontology:measurement(npt__su_t1995, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 1995, 0.63).
narrative_ontology:measurement(npt__su_t2010, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(npt__su_t2020, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 2020, 0.71).
narrative_ontology:measurement(npt__su_t2026, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 2026, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__nonproliferation_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_article_iv_vi_pairing__nonproliferation_primary, 0.12).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_iv_vi_pairing__grand_bargain).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_iv_vi_pairing__abolitionist).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, iaea_safeguards_regime).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, nuclear_suppliers_group_export_controls).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the npt_article_iv_vi_pairing kernel, decomposed per the ε-invariance principle: nonproliferation_primary (this file, tangled_rope, ε≈0.68), grand_bargain (reciprocal-obligations reading, expected lower ε due to conditional restraint), and abolitionist (humanitarian-law-grounded reading, expected to treat Article IV itself as illegitimate). All three share the same treaty text but instantiate structurally distinct constraints with different beneficiary/victim sets and different authority-grounding claims. They are linked here and should reciprocally link back via network.affects_constraints in each sibling file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
