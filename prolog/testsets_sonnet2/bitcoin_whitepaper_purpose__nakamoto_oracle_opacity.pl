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
 *   human_readable: Nakamoto Oracle Opacity — Interpretive Vacuum in Whitepaper Authority
 *   domain: distributed_systems/monetary_theory/technology_governance
 *
 * SUMMARY:
 *   This story treats the bitcoin_whitepaper_purpose kernel not through
 *   either substantive reading of its telos (electronic cash vs. store of
 *   value) but through the structural fact that generates the contest itself:
 *   Satoshi Nakamoto's 2011 disappearance removed the only party capable of
 *   authoritatively settling what the whitepaper's design commitments
 *   actually require going forward. This reading does not adjudicate between
 *   the sibling readings' claims — it describes the governance vacuum both
 *   readings compete inside, and the parties who have come to administer that
 *   vacuum for their own benefit. The 2017 block-size-war peak in
 *   theater_ratio and suppression_requirement reflects the period when the
 *   interpretive contest became most actively coercive (UASF threats,
 *   hash-power signaling campaigns, exchange-driven ticker disputes) before
 *   settling into a lower-intensity but still-elevated steady state
 *   post-fork.
 *
 * KEY AGENTS:
 *   - core_dev_maintainer_faction: administers the reference implementation and thereby the practical meaning of the whitepaper absent Satoshi
 *   - large_scale_miners: arbitrage between competing readings via hashpower reallocation
 *   - exchange_and_custody_operators: adjudicate 'which chain is real Bitcoin' for their users
 *   - retail_holders_seeking_clarity: bear the volatility and narrative whiplash produced by the unresolved contest
 *   - satoshi_nakamoto: the excluded, absent authority whose ruling would end the dispute
 *   - protocol_researchers: analytical observers of the governance dynamic itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 0.58).
domain_priors:suppression_score(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 0.4).
domain_priors:theater_ratio(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, extractiveness, 0.58).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, "Nakamoto Oracle Opacity — Interpretive Vacuum in Whitepaper Authority").
narrative_ontology:topic_domain(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, "distributed_systems/monetary_theory/technology_governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 'e0e12dfc-d4da-4bcd-8e97-9a222dac380d').
narrative_ontology:cs_kernel_codification('e0e12dfc-d4da-4bcd-8e97-9a222dac380d', fixed_text).
narrative_ontology:cs_authority_grounding('e0e12dfc-d4da-4bcd-8e97-9a222dac380d', distributed).
narrative_ontology:cs_reading_relation('e0e12dfc-d4da-4bcd-8e97-9a222dac380d', bitcoin_whitepaper_purpose__electronic_cash_reading, influences).
narrative_ontology:cs_reading_relation('e0e12dfc-d4da-4bcd-8e97-9a222dac380d', bitcoin_whitepaper_purpose__store_of_value_reading, influences).
narrative_ontology:cs_axiom('e0e12dfc-d4da-4bcd-8e97-9a222dac380d', foundational, founder_absence_is_structurally_load_bearing).
narrative_ontology:cs_axiom_status(founder_absence_is_structurally_load_bearing, holdable).
narrative_ontology:cs_axiom_grounding('e0e12dfc-d4da-4bcd-8e97-9a222dac380d', founder_absence_is_structurally_load_bearing, empirically_contingent).
narrative_ontology:cs_axiom('e0e12dfc-d4da-4bcd-8e97-9a222dac380d', foundational, no_substitute_authority_can_legitimately_replace_founder_ruling).
narrative_ontology:cs_axiom_status(no_substitute_authority_can_legitimately_replace_founder_ruling, holdable).
narrative_ontology:cs_axiom_grounding('e0e12dfc-d4da-4bcd-8e97-9a222dac380d', no_substitute_authority_can_legitimately_replace_founder_ruling, conventional).
narrative_ontology:cs_reference_frame('e0e12dfc-d4da-4bcd-8e97-9a222dac380d', founder_present_interpretive_authority).
narrative_ontology:cs_drift_state('e0e12dfc-d4da-4bcd-8e97-9a222dac380d', post_2011_disappearance_contemporary, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('e0e12dfc-d4da-4bcd-8e97-9a222dac380d', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, bitcoin_whitepaper_purpose).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, core_dev_maintainer_faction).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, large_scale_miners).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, exchange_and_custody_operators).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, retail_holders_seeking_clarity).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, small_merchants_uncertain_of_roadmap).
narrative_ontology:constraint_victim(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, new_entrant_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the reference implementation's commit access and release cadence. In the absence of any founder ruling, this group's technical judgment functions as de facto interpretive authority over what 'the whitepaper means' for protocol changes. They can characterize any change as consistent with Satoshi's intent because no one can be contradicted by the only person who could settle it. They bear little personal cost from the ambiguity and considerable soft power from administering it.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, core_dev_maintainer_faction, agenda_setter,
    organized, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, core_dev_maintainer_faction, beneficiary).

% Capital-intensive operators who can redeploy hashpower across whichever fork or interpretation currently maximizes fee revenue and coin value. The interpretive vacuum lets them back whichever reading (small blocks vs. large blocks, cash vs. store-of-value) serves their amortization schedule, then retroactively claim whitepaper fidelity. Their mobility means the ambiguity is optionality, not risk, for them.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, large_scale_miners, beneficiary,
    powerful, biographical, mobile, global).

% List, delist, and rename forked chains according to their own interpretation of which chain is 'real Bitcoin,' effectively adjudicating the kernel dispute for their users without any founder check on that adjudication. Their listing decisions monetize the ambiguity — spread capture, custody fees, and fork-airdrop administration all depend on the contest remaining unresolved.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, exchange_and_custody_operators, beneficiary,
    institutional, generational, arbitrage, global).

% Bought or hold bitcoin believing it has a stable, singular purpose (money, savings vehicle, hedge). They have no standing to resolve which reading is correct and no founder to appeal to; they absorb whichever practical consequences follow from whichever faction currently prevails — fee spikes, chain splits, sudden narrative shifts from 'digital cash' to 'digital gold.' Exit means abandoning the asset, which many are financially or psychologically unwilling to do.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, retail_holders_seeking_clarity, payer,
    powerless, biographical, trapped, global).

% Businesses that adopted bitcoin payment infrastructure on the strength of the 'electronic cash' framing found the fee/confirmation-time tradeoff shift under them as the store-of-value reading gained institutional dominance, with no authoritative text to appeal to for redress. They can switch payment rails, but sunk integration costs and customer expectations make this costly.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, small_merchants_uncertain_of_roadmap, payer,
    powerless, biographical, constrained, regional).

% Attempting to build protocol-level tooling must guess which reading will prevail before committing engineering resources, since the two readings imply incompatible roadmaps (on-chain scaling vs. layered settlement). They bear the cost of the ambiguity through wasted development cycles and the risk of building for a losing faction, without the standing or capital to shape the outcome themselves.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, new_entrant_developers, payer,
    moderate, biographical, constrained, global).

% The absent author whose 2011 disappearance is the structural cause of the vacuum. Would be the only party capable of resolving the dispute by fiat, but is unreachable, presumed to have deliberately withdrawn, and cannot be substituted for by any subsequent authority without controversy over the substitution's own legitimacy.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, satoshi_nakamoto, excluded,
    analytical, civilizational, analytical, global).

% Academic and independent analysts who study the governance dynamics of the ambiguity itself — publishing on fork history, mailing-list archaeology, and the sociology of whitepaper citation — without a stake in which reading prevails.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, protocol_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, diffuse).
narrative_ontology:fixing_cost_class(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In principle, a settled reading of the whitepaper's purpose would let developers, miners, merchants, and holders coordinate around one roadmap instead of duplicating effort across incompatible forks. The founder's disappearance removed the one mechanism (authorial ruling) that could have supplied that settlement cheaply.
% TRANSFER_FUNCTION: The persistence of interpretive ambiguity transfers coordination costs from those positioned to exploit the ambiguity (developers who administer the reference implementation, miners who can arbitrage between forks, exchanges who adjudicate listings) onto those without secondary means of protecting themselves (retail holders, small merchants, junior developers) who bear fee volatility, roadmap whiplash, and wasted technical investment.
% ABSENT_VOICES: Satoshi Nakamoto is the structurally absent voice whose ruling would settle the matter; every faction invokes 'what Satoshi meant' while the only party who could confirm or deny any interpretation is unreachable. Retail holders and small merchants are functionally present but powerless — they can post in forums but cannot bind any faction's roadmap decisions.
% DISAPPEARANCE_RATIONALE: If the interpretive vacuum were filled overnight — say, cryptographic proof of Satoshi's authentic return with a definitive statement of intent — factions dispute whether the ecosystem would actually defer to it (many participants have built years of technical, financial, and identity investment in their own reading and might reject even an authenticated founder ruling that contradicted their position) or whether it would trigger genuine convergence. The uncertainty here is itself part of the constraint's structure.
% FOUNDING_PROBLEM: The whitepaper was written to solve double-spending without a trusted third party. It did not specify a permanent governance mechanism for resolving future interpretive disputes about the system's telos once its author became unavailable — an omission that was arguably not intended as a design choice but became one by circumstance.
% FOUNDING_PROBLEM_CORROBORATION: Independent protocol historians and academic researchers outside any faction (e.g., analysts of the 2015-2017 block size dispute correspondence) corroborate that no textual or cryptographic mechanism in the original design anticipated founder absence as a governance variable; core developers and miners each separately attest their own reading is the 'natural' extension of the founding intent, which is exactly the self-serving pattern the corroboration check is designed to flag.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, contested).
narrative_ontology:founding_problem_status(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.58) is authored moderate-high: the vacuum itself does not extract directly, but it creates a standing opportunity for factions with administrative or capital mobility (dev maintainers, miners, exchanges) to capture interpretive authority at the expense of parties who took the whitepaper's stated purpose at face value. Suppression (0.4) is lower than extractiveness because the constraint's coercive force is intermittent and social/reputational (accusations of 'not real Bitcoin,' community shunning of dissenting forks) rather than continuously enforced; it spikes sharply during active contest periods (2017) and settles lower otherwise. Theater ratio (0.62) is high: enormous discursive energy is spent invoking 'what Satoshi really meant,' whitepaper close-reading, and appeals to founder intent, when the actual function being performed is factional positioning for resource control — this is a textbook proxy-goal substitution (Goodhart drift) where citation of the founding text substitutes for the founder's actual, unavailable judgment. Accessibility collapse (0.35) is moderate-low: unlike a genuine mountain, alternative interpretive mechanisms (formal governance votes, BIP process reform, multi-implementation consensus) remain technically available even if none has achieved legitimacy — the collapse is social/political, not structural. Resistance (0.72) is high: every faction actively resists ceding interpretive ground, which is itself evidence this is not settled natural fact but a live power contest.
 *
 * DIRECTIONALITY LOGIC:
 *   Core developers, large miners, and exchanges are declared beneficiaries because their structural position (administrative control, capital mobility, listing authority) lets them convert the ambiguity into practical interpretive power and, downstream, into resource allocation favorable to themselves — this yields low d under the engine's derivation. Retail holders, small merchants, and new entrant developers are declared victims: they lack the mobility or standing to arbitrage between readings and instead absorb the second-order consequences (fee volatility, roadmap reversal, wasted development effort) of a dispute they did not create and cannot resolve — this yields high d. Satoshi Nakamoto is marked excluded/analytical rather than beneficiary or payer: the absence is definitionally not a position of benefit or cost within the ongoing system, but the causal precondition for the whole structure; no override is applied because the derivation correctly treats an absent, non-participating agent as structurally outside the flow.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (solving double-spending without a trusted third party) is substantially solved and remains live at the protocol level — this is NOT a case of an obsolete mandate persisting by inertia. What has drifted is a *secondary* function: informal reliance on founder intent as a governance backstop, which was never explicitly designed but emerged as a de facto Schelling point. That backstop's disappearance in 2011 is the founding problem's shadow — a governance gap that persists because filling it (via formal succession, foundation charter, or explicit multi-sig authority) has never achieved legitimacy, and administering the resulting ambiguity has become profitable for exactly the parties positioned to do so. Classifying this as tangled_rope rather than snare or piton is deliberate: there IS a genuine coordination function (a decentralized monetary network benefits from *some* shared interpretive floor, and the core-dev process does perform real technical maintenance), but it is bundled with asymmetric extraction (interpretive authority accrues disproportionately to administratively positioned actors) and requires active enforcement (community shunning, exchange delisting threats, hashpower signaling) to hold together — precisely the tangled_rope signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founder_absence_deliberate_or_incidental,
    'Was Satoshi''s 2011 withdrawal a deliberate design choice (ensuring no permanent authority could exist) or an incidental circumstance (illness, legal risk, personal reasons) that inadvertently created a permanent interpretive vacuum?',
    'No conclusive resolution mechanism exists absent Satoshi''s own testimony; historical forensic analysis of forum posts, code commits, and the timing/manner of departure provides only weak circumstantial evidence either way.',
    'If deliberate, the vacuum is arguably part of the system''s genuine design (decentralization requires no permanent human authority) and the extraction is a cost intentionally accepted for that benefit. If incidental, the vacuum is an unintended governance failure that the community has since retrofitted into a legitimating narrative (''this is how it should be'') to justify the extraction that has grown up around administering it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founder_absence_deliberate_or_incidental, conceptual, 'Whether founder absence was designed-in or accidental — bears directly on whether the resulting extraction is a legitimate design cost or an unaccounted governance failure.').

omega_variable(
    sibling_reading_convergence_mechanism,
    'Is there any mechanism by which the electronic_cash_reading and store_of_value_reading could converge or be adjudicated without an authoritative founder ruling, or is permanent bifurcation the structural default absent Satoshi?',
    'Track whether any future governance innovation (formal BIP ratification with supermajority binding force, a widely-recognized successor foundation, or cryptographic proof of continued founder involvement) achieves cross-faction legitimacy; absent that, observe whether the two readings simply continue as permanently coexisting chains/communities (as has arguably already happened via BCH/BSV forks).',
    'If a convergence mechanism emerges and gains legitimacy, this reading''s claimed tangled_rope status could shift toward scaffold (temporary interpretive gap awaiting resolution) rather than a persistent structural feature. If no mechanism ever gains legitimacy, the vacuum is better modeled as a permanent structural feature of the kernel — closer to piton (administrators persist without genuine coordination benefit growing) than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_convergence_mechanism, empirical, 'Whether the interpretive vacuum is a resolvable transitional gap or a permanent structural feature of the kernel.').

omega_variable(
    beneficiary_administrator_good_faith,
    'Do core developers and large miners genuinely believe their interpretation is the correct extension of Satoshi''s intent (good-faith interpretive disagreement), or do they knowingly exploit the ambiguity for resource capture while performing fidelity to the whitepaper as cover?',
    'Internal communications discovery (mailing list archaeology, leaked deliberations) comparing stated public rationale against private strategic reasoning around fork decisions and listing choices; absent such discovery, this remains an inference from behavior alone.',
    'If good faith, the extraction identified here is better understood as an emergent property of decentralized governance without malicious intent — still extractive in effect but not cynically designed. If bad faith is demonstrated, the tangled_rope classification strengthens toward snare, since the coordination story would be shown to function as deliberate cover for extraction rather than a genuine, if imperfect, coordination attempt.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_administrator_good_faith, empirical, 'Whether beneficiary factions act in good-faith interpretive disagreement or knowingly exploit the vacuum.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 2011, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t2011, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 2011, 0.2).
narrative_ontology:measurement(bitc_tr_t2013, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 2013, 0.3).
narrative_ontology:measurement(bitc_tr_t2015, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 2015, 0.42).
narrative_ontology:measurement(bitc_tr_t2017, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 2017, 0.68).
narrative_ontology:measurement(bitc_tr_t2019, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 2019, 0.55).
narrative_ontology:measurement(bitc_tr_t2021, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 2021, 0.6).
narrative_ontology:measurement(bitc_tr_t2024, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, theater_ratio, 2024, 0.62).

% Extraction over time
narrative_ontology:measurement(bitc_be_t2011, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 2011, 0.22).
narrative_ontology:measurement(bitc_be_t2013, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 2013, 0.3).
narrative_ontology:measurement(bitc_be_t2015, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 2015, 0.4).
narrative_ontology:measurement(bitc_be_t2017, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 2017, 0.55).
narrative_ontology:measurement(bitc_be_t2019, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 2019, 0.5).
narrative_ontology:measurement(bitc_be_t2021, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 2021, 0.56).
narrative_ontology:measurement(bitc_be_t2024, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t2011, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 2011, 0.15).
narrative_ontology:measurement(bitc_su_t2013, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 2013, 0.2).
narrative_ontology:measurement(bitc_su_t2015, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 2015, 0.3).
narrative_ontology:measurement(bitc_su_t2017, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 2017, 0.48).
narrative_ontology:measurement(bitc_su_t2019, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 2019, 0.38).
narrative_ontology:measurement(bitc_su_t2021, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 2021, 0.4).
narrative_ontology:measurement(bitc_su_t2024, bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, identity_coordination).
narrative_ontology:boltzmann_floor_override(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, 0.08).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, electronic_cash_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper_purpose__nakamoto_oracle_opacity, store_of_value_reading).

% DUAL FORMULATION NOTE:
% This story is the load-bearing structural member of the bitcoin_whitepaper_purpose kernel family: it does not claim a substantive telos (cash vs. store-of-value) but describes the governance vacuum both substantive readings operate inside. electronic_cash_reading and store_of_value_reading each author their own ε against their own claimed arrangement (fee-optimized transactional network vs. decentralization-optimized settlement layer, respectively); this story's ε (0.58) measures the extraction generated by the absence of any mechanism to adjudicate between them, which is analytically prior to and partly explains why those two readings have never converged. All three stories should be read as a triplet, not substitutes for one another.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
