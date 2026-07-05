% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper_purpose__nakamoto_oracle_opacity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, []).

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
 *   constraint_id: bitcoin_whitepaper_purpose__nakamoto_oracle_opacity
 *   human_readable: The Founder's Silence as Contested Interpretive Substrate
 *   domain: distributed_systems/monetary_theory/technology_governance
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested
 *   'bitcoin_whitepaper_purpose' kernel: the reading that treats Nakamoto's
 *   2011 disappearance itself as the constitutive structural fact, rather
 *   than adjudicating between the electronic-cash and store-of-value
 *   content-readings of the text. Under this reading, the whitepaper is not
 *   ambiguous because its words are unclear in the abstract — it is ambiguous
 *   because the one party who could authoritatively fix its meaning is
 *   permanently unreachable, and every faction's claim to fidelity is equally
 *   unfalsifiable. This is structurally distinct from asking 'does the
 *   whitepaper mean cash or store of value' (those are the sibling readings,
 *   in separate constraint files) — this reading is about the absence of an
 *   oracle to settle that question at all, and the rent-generating industry
 *   that absence has produced.
 *
 * KEY AGENTS:
 *   - core_dev_maintainers: agenda_setter/institutional — control reference implementation and thereby de facto interpretive authority
 *   - competing_fork_promoters: beneficiary/organized — monetize legitimacy contests enabled by the vacuum
 *   - whitepaper_citation_industry: beneficiary/organized — builds careers on unfalsifiable exegesis
 *   - retail_holders_seeking_clarity: payer/powerless — bears confusion costs of an unstable referent
 *   - small_merchant_adopters: payer/powerless — absorbed integration costs for a use case later deprioritized without founder ruling
 *   - protocol_governance_participants: payer/moderate — repeatedly relitigates unresolvable interpretive disputes
 *   - satoshi_nakamoto: excluded/analytical — the one party whose clarification would resolve the dispute, permanently unavailable
 *   - protocol_historians: observer/analytical — documents evidence but cannot adjudicate underdetermined meaning
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 0.58).
domain_priors:suppression_score(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 0.35).
domain_priors:theater_ratio(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, extractiveness, 0.58).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, "The Founder's Silence as Contested Interpretive Substrate").
narrative_ontology:topic_domain(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, "distributed_systems/monetary_theory/technology_governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 'c914cd7d-da38-4f58-9d50-42ef5928a699').
narrative_ontology:cs_kernel_codification('c914cd7d-da38-4f58-9d50-42ef5928a699', fixed_text).
narrative_ontology:cs_authority_grounding('c914cd7d-da38-4f58-9d50-42ef5928a699', distributed).
narrative_ontology:cs_reading_relation('c914cd7d-da38-4f58-9d50-42ef5928a699', bitcoin_whitepaper_purpose__electronic_cash_reading, influences).
narrative_ontology:cs_reading_relation('c914cd7d-da38-4f58-9d50-42ef5928a699', bitcoin_whitepaper_purpose__store_of_value_reading, influences).
narrative_ontology:cs_axiom('c914cd7d-da38-4f58-9d50-42ef5928a699', foundational, founder_absence_is_structural_not_contingent).
narrative_ontology:cs_axiom_status(founder_absence_is_structural_not_contingent, holdable).
narrative_ontology:cs_axiom_grounding('c914cd7d-da38-4f58-9d50-42ef5928a699', founder_absence_is_structural_not_contingent, empirically_contingent).
narrative_ontology:cs_axiom('c914cd7d-da38-4f58-9d50-42ef5928a699', foundational, no_party_can_claim_verified_founder_ratification).
narrative_ontology:cs_axiom_status(no_party_can_claim_verified_founder_ratification, holdable).
narrative_ontology:cs_axiom_grounding('c914cd7d-da38-4f58-9d50-42ef5928a699', no_party_can_claim_verified_founder_ratification, conventional).
narrative_ontology:cs_reference_frame('c914cd7d-da38-4f58-9d50-42ef5928a699', pre_2011_active_founder_stewardship).
narrative_ontology:cs_drift_state('c914cd7d-da38-4f58-9d50-42ef5928a699', post_disappearance_multi_fork_era, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('c914cd7d-da38-4f58-9d50-42ef5928a699', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, bitcoin_whitepaper_purpose).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, core_dev_maintainers).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, competing_fork_promoters).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, whitepaper_citation_industry).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, retail_holders_seeking_clarity).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, small_merchant_adopters).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, protocol_governance_participants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control the reference implementation's commit access and set the terms of what counts as a 'legitimate' protocol change. In the absence of Nakamoto, their reading of the whitepaper's intent becomes de facto authoritative through code merges rather than through any conferred mandate. They can decline to implement rival interpretations without ever having to argue against Nakamoto directly, since Nakamoto cannot answer.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, core_dev_maintainers, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, core_dev_maintainers, beneficiary).

% Launch alternative chains and client implementations each claiming to be the 'true' continuation of the whitepaper's design. Each fork monetizes the interpretive vacuum: legitimacy contests generate press coverage, community formation, and token value independent of any resolved technical or philosophical question.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, competing_fork_promoters, beneficiary,
    organized, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, competing_fork_promoters, agenda_setter).

% Academics, consultants, podcasters, and self-styled 'Bitcoin maximalists' or 'Bitcoin Cash originalists' build careers and platforms on authoritative-sounding exegesis of a nine-page document whose author cannot be consulted. The absence of a living author to correct misreadings is the raw material of their output.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, whitepaper_citation_industry, beneficiary,
    organized, biographical, mobile, global).

% Buy into 'Bitcoin' expecting a stable referent — a digital cash system, a store of value, a hedge — and instead discover the referent is a live contest between factions each citing the same founding text. They bear the cost of confusion: buying the wrong chain, misunderstanding fee dynamics, being targeted by scams that exploit the ambiguity. They cannot appeal to Nakamoto to resolve which reading they bought into.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, retail_holders_seeking_clarity, payer,
    powerless, biographical, constrained, global).

% Attempted to adopt Bitcoin for point-of-sale transactions in the 2013-2017 window, following the whitepaper's 'peer-to-peer electronic cash' framing, then found fee volatility and block-size gridlock made this reading unworkable in practice as the store-of-value faction won the resource allocation fight. They absorbed integration costs for a use case the community itself later deprioritized without any founder ruling on whether that deprioritization was faithful to the design.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, small_merchant_adopters, payer,
    powerless, biographical, constrained, regional).

% Miners, node operators, and exchange operators who must pick a side in every contested upgrade (block size, SegWit, Taproot, various fork events) without any mechanism to ask the founding author which side the whitepaper actually endorses. Their labor and capital are spent relitigating the same interpretive question repeatedly because no ruling can ever be final.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, protocol_governance_participants, payer,
    moderate, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, protocol_governance_participants, excluded).

% Disappeared from public communication in April 2011 after handing off the project. Whether by choice, incapacity, or death, this absence is total and irreversible as a practical matter — no verified communication has occurred since. The absence itself is now a structural feature of the system: anyone claiming to speak for Nakamoto's intent is unfalsifiable and unaccountable to the actual author.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, satoshi_nakamoto, excluded,
    analytical, civilizational, trapped, global).

% Study the mailing list archives, the whitepaper text, and the early commit history as the only surviving evidence of original intent. They can document what was said but cannot adjudicate what was meant when the text underdetermines the answer and no living authority can clarify.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, protocol_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, diffuse).
narrative_ontology:fixing_cost_class(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The whitepaper functions as the closest thing to a founding constitutional text the protocol has; citing it lets factions coordinate around a shared symbolic anchor rather than fighting purely on raw power, which at least channels disputes through textual argument instead of naked force.
% TRANSFER_FUNCTION: Interpretive authority (and the legitimacy, community attention, and market capitalization that follow from being seen as the 'true' continuation) flows to whichever faction can most persuasively perform fidelity to the unreachable author, at the cost of retail holders' and merchants' ability to rely on a stable, resolvable meaning for what they bought into.
% ABSENT_VOICES: Satoshi Nakamoto themself is the paradigmatic absent voice — the one party whose actual clarification would settle the dispute is permanently unavailable, and every faction's confident citation of 'what Satoshi meant' proceeds without any possibility of correction from the source.
% DISAPPEARANCE_RATIONALE: If the interpretive vacuum were filled overnight (say, verified Nakamoto communication resolved intent), some factions would lose their legitimacy-generating engine entirely and the fork ecosystem's coordination costs would drop sharply; others would argue the protocol's meaning is properly determined by its actual technical trajectory and social consensus regardless of founder intent, and would contest that any single clarification should be binding at all. The verdict is genuinely disputed among the parties themselves.
% FOUNDING_PROBLEM: Bitcoin needed a founding document precise enough to bootstrap a leaderless, permissionless network without a central authority — Nakamoto wrote the whitepaper and then withdrew, apparently by design, to prevent the system from depending on a continuing human authority.
% FOUNDING_PROBLEM_CORROBORATION: Early mailing-list participants and protocol historians attest that withdrawal was consistent with Nakamoto's stated design philosophy (removing single points of failure, including social/political ones). Competing faction leaders, who are direct beneficiaries of the interpretive vacuum, each independently attest that the vacuum is either a feature (proving decentralization works without a leader) or a bug (preventing resolution of open design questions) depending on which reading benefits their position — no source outside the contesting factions themselves offers disinterested corroboration of which framing is correct.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, contested).
narrative_ontology:founding_problem_status(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects the substantial rent extracted by both fork-promotion and citation industries from an interpretive vacuum that a living, responsive founder could in principle close. Suppression (0.35) is moderate rather than high: no single faction can coercively prevent rival readings, but core-dev commit-access does function as soft suppression of unfavored implementations. Theater ratio (0.62) is high and rising through the 2017 block-size war and beyond — a large share of 'principled' interpretive argument is retrospectively legitimacy theater for economic positions already taken. Resistance (0.78) is high: every faction actively resists rival interpretive claims, which is exactly what an unresolvable contest produces. Accessibility collapse (0.4) is moderate-low: alternative framings (agnosticism about founder intent, appeal to present social consensus instead) remain genuinely available and are used by some participants.
 *
 * DIRECTIONALITY LOGIC:
 *   Core-dev maintainers and fork promoters sit near the beneficiary end: their institutional or organized position lets them convert interpretive ambiguity into commit-access power or market capitalization, and their exit options (arbitrage) let them pivot readings opportunistically. Retail holders and merchants sit near the target end: powerless, constrained exit, and directly bearing the cost of an unresolved referent they cannot even properly complain about since the 'proper' meaning is exactly what's contested. Protocol governance participants are intermediate — moderate power, but trapped in the sense that non-participation means losing all influence over outcomes that affect their capital.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (bootstrap a leaderless network without dependence on continuing human authority) is genuinely still partially live — decentralization from any single interpretive authority IS the design goal, so the vacuum is not purely dead weight. But the specific mandate now claimed by interpretive factions — 'we alone correctly channel Nakamoto's intent' — long ago detached from any verifiable connection to Nakamoto and became a legitimacy-generating engine in its own right. This is a tangled rope rather than a pure snare precisely because the coordination function (a shared textual anchor reduces naked power conflict to textual argument) is real, even as the enforcement of particular readings extracts real costs from powerless participants.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nakamoto_identity_and_status,
    'Is Nakamoto alive, capable of communication, and simply choosing silence, or deceased/incapacitated such that no future clarification is possible in principle?',
    'Verified cryptographic communication from a key demonstrably controlled by the original Nakamoto identity, or credible forensic/historical evidence of death or incapacity.',
    'If Nakamoto is alive and choosing silence, the opacity is a deliberate structural design choice (arguably itself part of the whitepaper''s decentralization telos) and the interpretive contest is a feature. If deceased, the vacuum is an accident of history with no designed justification, and factions claiming design intent behind the silence are fabricating a legitimacy narrative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(nakamoto_identity_and_status, empirical, 'Whether founder silence is deliberate design or historical accident.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the correct unit of analysis ''the whitepaper''s content-meaning'' (cash vs. store-of-value, the sibling readings) or ''the absence of an authority capable of fixing that meaning'' (this reading)? Both framings are coherent and produce structurally different constraints with different ε profiles.',
    'No empirical resolution exists; this is a framing choice about which structural feature is doing the causal work in observed fork proliferation and interpretive contest.',
    'Under the content-framing, the relevant constraint is a contest over two mutually exclusive technical/philosophical claims about optimal design. Under this reading''s framing, the relevant constraint is the persistence of a rent-generating vacuum regardless of which content-claim eventually prevails. The two framings support different remediation strategies: content-framing suggests technical resolution (e.g., protocol changes settling the block-size question); this reading''s framing suggests the rent will persist even after any technical resolution, because a new unresolvable question will simply substitute.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel is best modeled as content-contest or authority-vacuum; this story adopts the latter framing per its assigned reading_id.').

omega_variable(
    convergence_mechanism_absence,
    'Does any social or technical mechanism exist, or could one be constructed, that would substitute for founder clarification in resolving whitepaper-fidelity disputes?',
    'Track whether any single interpretive claim achieves durable, near-universal community acceptance over a multi-decade horizon without founder intervention — durable convergence would demonstrate a substitute mechanism exists.',
    'If no substitute mechanism ever emerges, the vacuum is permanent and the tangled-rope structure (coordination value + extraction cost) persists indefinitely. If a substitute emerges (e.g., a sufficiently authoritative multi-generational consensus), the constraint could decay toward a rope as legitimacy stabilizes without needing founder input.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(convergence_mechanism_absence, empirical, 'Whether social consensus can ever substitute for the missing founder as an interpretive authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 2011, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t2011, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 2011, 0.25).
narrative_ontology:measurement(bitc_tr_t2013, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 2013, 0.35).
narrative_ontology:measurement(bitc_tr_t2015, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 2015, 0.42).
narrative_ontology:measurement(bitc_tr_t2017, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 2017, 0.68).
narrative_ontology:measurement(bitc_tr_t2020, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 2020, 0.6).
narrative_ontology:measurement(bitc_tr_t2024, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 2024, 0.62).

% Extraction over time
narrative_ontology:measurement(bitc_be_t2011, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 2011, 0.22).
narrative_ontology:measurement(bitc_be_t2013, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 2013, 0.32).
narrative_ontology:measurement(bitc_be_t2015, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 2015, 0.4).
narrative_ontology:measurement(bitc_be_t2017, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 2017, 0.55).
narrative_ontology:measurement(bitc_be_t2020, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 2020, 0.52).
narrative_ontology:measurement(bitc_be_t2024, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 2024, 0.58).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, identity_coordination).
narrative_ontology:boltzmann_floor_override(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 0.1).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, electronic_cash_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, store_of_value_reading).

% DUAL FORMULATION NOTE:
% This constraint is the authority-vacuum reading of the bitcoin_whitepaper_purpose kernel; 'electronic_cash_reading' and 'store_of_value_reading' are the two content-contest readings that this vacuum keeps permanently open. All three share ε-invariance individually but are structurally coupled: resolving this constraint's central question (is there any path to authoritative clarification) would collapse the coordination-extraction structure of both siblings by removing the mechanism that keeps their contest perpetually live rather than resolved.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
