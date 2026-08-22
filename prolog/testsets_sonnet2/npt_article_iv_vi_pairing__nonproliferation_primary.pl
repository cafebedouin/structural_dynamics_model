% ============================================================================
% CONSTRAINT STORY: npt_article_iv_vi_pairing__nonproliferation_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   Article IV/VI kernel: Article IV civilian nuclear cooperation is read as
 *   conditional on Article III verification compliance, while Article VI's
 *   disarmament language is read as aspirational and non-justiciable — a
 *   hortatory commitment to 'pursue negotiations in good faith,' not an
 *   enforceable obligation running against weapon-state arsenals. Under this
 *   reading, the treaty's operative legal architecture is a one-way
 *   verification-for-access exchange whose authority derives from the weapon
 *   states' own security interest in preventing horizontal proliferation, not
 *   from a genuine bilateral bargain. The indefinite extension of the treaty
 *   in 1995 without a sunset or renegotiation trigger is, on this reading,
 *   the moment the asymmetry became structurally permanent rather than
 *   transitional. This is a distinct constraint from the grand_bargain
 *   reading (which treats Article VI as a live, breach-sensitive reciprocal
 *   obligation making Article IV legitimacy conditional on disarmament
 *   progress) and from the abolitionist reading (which treats any arrangement
 *   that perpetuates weapon-state arsenals as illegitimate regardless of
 *   verification compliance, and grounds authority in
 *   humanitarian/weapons-prohibition law rather than nonproliferation
 *   security interest). Each reading has its own ε, beneficiary/victim
 *   structure, and classification; they are linked here only through the
 *   kernel and the cs_structure reading_relations.
 *
 * KEY AGENTS:
 *   - recognized_weapon_states: agenda_setter/beneficiary — retain arsenals outside enforcement, administer export-control gatekeeping
 *   - iaea_verification_apparatus: beneficiary/agenda_setter — administers the asymmetric verification burden
 *   - non_weapon_state_parties: payer — bear permanent restraint and verification cost for conditional technology access
 *   - civilian_nuclear_energy_aspirants: payer — bear technology-transfer denial and delay under supplier gatekeeping
 *   - non_npt_nuclear_states: excluded — outside the bargain entirely, de facto weapon status without any treaty burden
 *   - disarmament_advocacy_states: excluded — present but structurally unable to force Article VI justiciability
 *   - icj_and_legal_scholars: observer — analytical seat on the 1996 Advisory Opinion's disputed legal weight
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__nonproliferation_primary, 0.68).
domain_priors:suppression_score(npt_article_iv_vi_pairing__nonproliferation_primary, 0.72).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__nonproliferation_primary, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__nonproliferation_primary, tangled_rope).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__nonproliferation_primary, "NPT Article IV/VI Pairing — Nonproliferation-Primary Reading").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__nonproliferation_primary, "international_law/nuclear_governance").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__nonproliferation_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__nonproliferation_primary, '4ee0ea34-b0b2-45fc-bfde-c7917b5b73f8').
narrative_ontology:cs_kernel_codification('4ee0ea34-b0b2-45fc-bfde-c7917b5b73f8', fixed_text).
narrative_ontology:cs_authority_grounding('4ee0ea34-b0b2-45fc-bfde-c7917b5b73f8', practice).
narrative_ontology:cs_interpretation_layer_present('4ee0ea34-b0b2-45fc-bfde-c7917b5b73f8').
narrative_ontology:cs_reading_relation('4ee0ea34-b0b2-45fc-bfde-c7917b5b73f8', npt_article_iv_vi_pairing__grand_bargain, coexists_with).
narrative_ontology:cs_reading_relation('4ee0ea34-b0b2-45fc-bfde-c7917b5b73f8', npt_article_iv_vi_pairing__abolitionist, coexists_with).
narrative_ontology:cs_axiom('4ee0ea34-b0b2-45fc-bfde-c7917b5b73f8', foundational, article_vi_non_justiciable_aspiration).
narrative_ontology:cs_axiom_status(article_vi_non_justiciable_aspiration, holdable).
narrative_ontology:cs_axiom_grounding('4ee0ea34-b0b2-45fc-bfde-c7917b5b73f8', article_vi_non_justiciable_aspiration, conventional).
narrative_ontology:cs_axiom('4ee0ea34-b0b2-45fc-bfde-c7917b5b73f8', foundational, nonproliferation_security_interest_as_sole_authority_ground).
narrative_ontology:cs_axiom_status(nonproliferation_security_interest_as_sole_authority_ground, holdable).
narrative_ontology:cs_axiom_grounding('4ee0ea34-b0b2-45fc-bfde-c7917b5b73f8', nonproliferation_security_interest_as_sole_authority_ground, instrumental).
narrative_ontology:cs_reference_frame('4ee0ea34-b0b2-45fc-bfde-c7917b5b73f8', verification_conditioned_nonproliferation_order).
narrative_ontology:cs_drift_state('4ee0ea34-b0b2-45fc-bfde-c7917b5b73f8', post_1995_indefinite_extension, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4ee0ea34-b0b2-45fc-bfde-c7917b5b73f8', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__nonproliferation_primary, recognized_weapon_states).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__nonproliferation_primary, iaea_verification_apparatus).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__nonproliferation_primary, non_weapon_state_parties).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__nonproliferation_primary, civilian_nuclear_energy_aspirants).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__nonproliferation_primary, horizontal_nonproliferation_as_primary_treaty_object).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__nonproliferation_primary, verification_conditionality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The five NPT-recognized nuclear weapon states retain their arsenals outside any treaty-based disarmament enforcement mechanism, while chairing or dominating the review conferences that interpret Article VI as a good-faith aspiration rather than a justiciable obligation. They administer the export-control regimes (NSG, Zangger Committee) that gate Article IV technology transfer and set the terms under which Article III verification is deemed sufficient.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, recognized_weapon_states, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__nonproliferation_primary, recognized_weapon_states, beneficiary).

% Administers safeguards inspections that function as the gatekeeping mechanism for Article IV civilian nuclear cooperation. Its institutional mandate and budget grow with verification complexity; it has no comparable role in disarmament verification of weapon-state arsenals, which structurally reinforces the asymmetry this reading names as the treaty's core operative logic.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, iaea_verification_apparatus, beneficiary,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__nonproliferation_primary, iaea_verification_apparatus, agenda_setter).

% Bear the permanent restraint obligation (forgo acquiring weapons) and submit to intrusive Article III verification as the price of Article IV civilian nuclear cooperation, while receiving no enforceable reciprocal claim on weapon-state disarmament under this reading. Their exit is constrained: withdrawal under Article X is legally available but carries severe diplomatic, security, and market costs (as DPRK's case illustrates), and most lack the industrial base to pursue nuclear energy outside the treaty framework at all.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, non_weapon_state_parties, payer,
    moderate, generational, constrained, global).

% Developing states seeking nuclear power for energy security must accept Article III verification burdens, technology-transfer gatekeeping by supplier-state cartels, and delay or denial of dual-use technology on proliferation-risk grounds, with essentially no alternative supplier network outside the weapon-state-dominated export control regimes.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, civilian_nuclear_energy_aspirants, payer,
    powerless, biographical, trapped, national).

% States that never joined or withdrew (India, Pakistan, Israel, North Korea) sit entirely outside this enforcement structure, benefiting from de facto weapon status without ever bearing Article III verification burdens or Article VI rhetorical constraint — their existence is the standing counterexample this reading must treat as an exogenous security problem rather than a treaty design flaw.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, non_npt_nuclear_states, excluded,
    powerful, civilizational, arbitrage, global).

% Non-weapon states and civil-society coalitions that argue Article VI creates a binding reciprocal obligation are procedurally present at review conferences but structurally unable to force justiciability — there is no tribunal, no consequence mechanism, and consensus rules let weapon states block any language that would harden Article VI into an enforceable duty.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, disarmament_advocacy_states, excluded,
    organized, generational, constrained, global).

% The 1996 ICJ Advisory Opinion held there exists an obligation to pursue negotiations in good faith toward disarmament, but this reading treats that language as precatory, not a justiciable duty enforceable against weapon states — legal scholarship remains split on whether the opinion supports this reading or the grand_bargain reading.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, icj_and_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_article_iv_vi_pairing__nonproliferation_primary, recognized_weapon_states).
narrative_ontology:fixing_cost_class(npt_article_iv_vi_pairing__nonproliferation_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents horizontal proliferation of nuclear weapons to additional states by exchanging civilian nuclear technology access (Article IV) for verified renunciation of weapons pursuit (Article III), stabilizing a predictable security order among existing weapon states and averting a multipolar nuclear arms race.
% TRANSFER_FUNCTION: Moves verification compliance costs, technology-access delay, and permanent strategic restraint from non-weapon states to weapon states' benefit; moves institutional authority and budget to the IAEA verification apparatus; does not move any binding disarmament obligation from weapon states to anyone.
% ABSENT_VOICES: Disarmament-advocacy states and TPNW signatories argue Article VI is a real, enforceable reciprocal obligation and that its non-justiciability under this reading is itself the extraction — they are present at review conferences but cannot force the interpretive question to a binding forum. Non-NPT nuclear states are entirely outside the room, never subject to the bargain at all.
% DISAPPEARANCE_RATIONALE: If this reading's enforcement structure vanished — i.e., if Article IV access stopped being conditioned on Article III verification and weapon-state arsenals became subject to comparable enforcement — the entire nonproliferation regime's incentive structure would need to be renegotiated; supplier cartels, safeguards budgets, and the diplomatic status quo among the five recognized weapon states all depend on the asymmetry persisting.
% FOUNDING_PROBLEM: In 1968, the founding problem was preventing a multiplication of nuclear-armed states beyond the five that had already tested, at a moment when several threshold states (West Germany, Japan, Sweden) were viewed as plausible near-term proliferators.
% FOUNDING_PROBLEM_CORROBORATION: Weapon states and the IAEA attest the horizontal-proliferation problem remains live, citing Iran, North Korea's withdrawal, and ongoing enrichment disputes. Independent arms-control scholars and the 1995/2000/2010 NPT Review Conference final documents (adopted by consensus including non-weapon states) attest that the founding bargain included a disarmament reciprocity component that this reading has treated as functionally dead since indefinite extension in 1995 — that gap is documented outside the weapon-state seats, in ICJ commentary and independent think-tank assessments (SIPRI, Arms Control Association).
narrative_ontology:disappearance_verdict(npt_article_iv_vi_pairing__nonproliferation_primary, world_rearranges).
narrative_ontology:founding_problem_status(npt_article_iv_vi_pairing__nonproliferation_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__nonproliferation_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.68) reflects a substantial but not extreme transfer: real coordination value exists (a credible nonproliferation norm reduces global security risk for everyone, including non-weapon states), but the verification-for-access exchange is structurally one-directional under this reading, and that one-directionality has hardened over time (1968: 0.45 -> 2026: 0.68) as indefinite extension foreclosed the original expectation of periodic renegotiation. Suppression (0.72) captures the coercive weight of export-control cartelization and the near-total foreclosure of a state's ability to pursue nuclear technology outside the treaty framework once it has joined — Article X withdrawal is nominally available but carries severe costs, so the suppression is real rather than merely bureaucratic. Theater ratio (0.4) reflects that Article VI review-conference language ('pursuit of good faith negotiations') has increasingly become a diplomatic performance disconnected from any operative disarmament mechanism, while the Article III/IV verification machinery remains functionally live and non-theatrical.
 *
 * DIRECTIONALITY LOGIC:
 *   Recognized weapon states sit at the beneficiary end: they administer the enforcement machinery, bear no comparable verification burden on their own arsenals, and retain arbitrage-grade exit (their compliance posture is self-adjudicated). The IAEA apparatus benefits institutionally from the asymmetric verification mandate even though it does not capture rents directly — it is listed as a structural beneficiary because its mandate, budget, and authority derive entirely from administering the one-directional verification function this reading identifies. Non-weapon-state parties and civilian nuclear aspirants sit at the target end: they carry the restraint obligation and the verification cost with constrained or trapped exit, since supplier-cartel dependency and diplomatic isolation make Article X withdrawal or independent technological development practically unavailable.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing 1960s-era horizontal proliferation among threshold states like West Germany, Japan, Sweden) is genuinely still partially live — Iran and historical North Korea cases show real proliferation risk persists. But this reading's classification as tangled_rope rather than pure snare depends on that genuine residual coordination function; if the founding problem were fully dead (no plausible new proliferator existed) while the enforcement asymmetry persisted, this would collapse toward snare. The mismatch check here is instructive: founding_problem_status is authored as contested rather than dead precisely because weapon states can point to live cases (corroborating their live-problem claim) while independent scholarship corroborates that the disarmament-reciprocity half of the original bargain is functionally dead — the classification holds tangled_rope specifically because one half of the coordination function survives while the extraction half has hardened.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_justiciability_ambiguity,
    'Is Article VI''s ''pursue negotiations in good faith'' language a genuine legal obligation whose breach has consequences, or a purely hortatory clause with no enforcement mechanism — and does the answer depend on which review-conference-era interpretive consensus is taken as authoritative?',
    'A binding international tribunal ruling (ICJ contentious jurisdiction, not merely advisory opinion) on a state''s Article VI compliance would resolve this; absent that, the question remains a matter of contested treaty interpretation among international law scholars.',
    'If Article VI is found genuinely justiciable, this reading''s core premise collapses into the grand_bargain reading, and the classification would likely shift toward a more clearly enforcement-imbalanced tangled_rope or even snare, since weapon-state non-compliance with a binding obligation would sharpen the victim structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_justiciability_ambiguity, conceptual, 'Whether Article VI''s disarmament language is legally binding or merely aspirational is the central interpretive fork this reading resolves one way.').

omega_variable(
    kernel_committer_structure,
    'This constraint is one reading (nonproliferation_primary) of the npt_article_iv_vi_pairing kernel. The sibling readings are grand_bargain (Article IV/VI as reciprocal obligations, weapon-state disarmament progress as a condition of non-weapon-state restraint legitimacy) and abolitionist (Article VI as a disarmament mandate rendering Article IV illegitimate if it perpetuates proliferation risk, authority grounded in humanitarian/TPNW law). Where is the disagreement located structurally?',
    'The disagreement is located specifically in (a) whether Article VI creates a binding reciprocal condition on Article IV legitimacy, and (b) what grounds treaty authority — nonproliferation security interest (this reading) vs. mutual bargain (grand_bargain) vs. humanitarian weapons-prohibition law (abolitionist). No further data resolves this; it is a standing interpretive dispute among treaty parties, not an empirical question with a discoverable answer.',
    'Adopting grand_bargain would make weapon-state disarmament stagnation a legitimacy-undermining breach with victim consequences flowing back onto weapon states themselves, changing the beneficiary/victim structure substantially. Adopting abolitionist would treat any Article-IV-preserving arrangement as illegitimate regardless of verification compliance, likely reclassifying the whole kernel as snare from that reading''s seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_committer_structure, conceptual, 'Committer-structure omega: names the kernel, this reading, the sibling readings, and the specific structural locus of disagreement (Article VI''s bindingness and the source of treaty authority).').

omega_variable(
    indefinite_extension_as_hardening_event,
    'Did the 1995 indefinite extension of the NPT (removing the original 25-year renewal/renegotiation trigger) structurally convert a time-limited scaffold arrangement into a permanent tangled_rope, or was the treaty already structurally permanent in practice before 1995?',
    'Historical analysis of review-conference negotiating records and the diplomatic conditions attached to the 1995 extension vote (which included promises of enhanced review process and a Middle East WMD-free-zone resolution, largely unfulfilled) would clarify whether 1995 was a genuine hardening event or a ratification of an already-existing asymmetry.',
    'If 1995 is the hardening event, the pre-1995 constraint may have genuinely functioned closer to a scaffold (with the sunset/renewal clause as the transitional mechanism); if the asymmetry predates 1995, the scaffold framing was always a false surface over a permanent tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indefinite_extension_as_hardening_event, empirical, 'Whether 1995 indefinite extension is the structural hardening point or merely confirmed a pre-existing permanence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__nonproliferation_primary, 1968, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1968, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 1968, 0.2).
narrative_ontology:measurement(npt__tr_t1980, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(npt__tr_t1995, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 1995, 0.35).
narrative_ontology:measurement(npt__tr_t2005, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 2005, 0.38).
narrative_ontology:measurement(npt__tr_t2015, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(npt__tr_t2026, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 2026, 0.4).

% Extraction over time
narrative_ontology:measurement(npt__be_t1968, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 1968, 0.45).
narrative_ontology:measurement(npt__be_t1980, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 1980, 0.5).
narrative_ontology:measurement(npt__be_t1995, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 1995, 0.58).
narrative_ontology:measurement(npt__be_t2005, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 2005, 0.62).
narrative_ontology:measurement(npt__be_t2015, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 2015, 0.65).
narrative_ontology:measurement(npt__be_t2026, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 2026, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1968, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 1968, 0.4).
narrative_ontology:measurement(npt__su_t1980, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 1980, 0.5).
narrative_ontology:measurement(npt__su_t1995, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 1995, 0.6).
narrative_ontology:measurement(npt__su_t2005, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 2005, 0.68).
narrative_ontology:measurement(npt__su_t2015, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 2015, 0.7).
narrative_ontology:measurement(npt__su_t2026, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 2026, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__nonproliferation_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_iv_vi_pairing__grand_bargain).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_iv_vi_pairing__abolitionist).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, iaea_safeguards_verification_regime).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, nuclear_suppliers_group_export_controls).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the npt_article_iv_vi_pairing kernel (nonproliferation_primary, grand_bargain, abolitionist), each authored as a separate constraint with its own ε per the ε-invariance principle. nonproliferation_primary reads Article VI as non-justiciable and grounds authority in weapon-state security interest (ε=0.68, tangled_rope); grand_bargain reads Article IV/VI as reciprocal with weapon-state disarmament stagnation as a legitimacy-undermining breach; abolitionist reads any Article-IV-preserving arrangement as illegitimate on humanitarian-law grounds. All three link to each other and to the downstream IAEA verification and export-control constraints they structurally condition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
