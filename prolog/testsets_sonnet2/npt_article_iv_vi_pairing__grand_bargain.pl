% ============================================================================
% CONSTRAINT STORY: npt_article_iv_vi_pairing__grand_bargain
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: npt_article_iv_vi_pairing__grand_bargain
 *   human_readable: NPT Article IV/VI Reciprocal Bargain (Grand Bargain Reading)
 *   domain: international_law/nuclear_governance
 *
 * SUMMARY:
 *   This story instantiates the 'grand bargain' reading of the contested NPT
 *   kernel: Articles IV and VI are reciprocal, mutually enforceable
 *   obligations, such that non-weapon state (NNWS) restraint under Article
 *   III/IV is conditioned on weapon-state disarmament progress under Article
 *   VI, and persistent Article VI breach undermines the legitimacy of
 *   continued Article IV/III compliance demands on NNWS. This is one of three
 *   structurally distinct readings of the same treaty text —
 *   nonproliferation_primary treats Article VI as aspirational and
 *   non-justiciable with Article IV conditioned only on Article III
 *   verification; abolitionist treats Article IV itself as illegitimate given
 *   proliferation risk, deriving authority from humanitarian
 *   weapons-prohibition law (TPNW) rather than the NPT's internal bargain
 *   logic. Each reading is authored as its own constraint with its own
 *   epsilon; this file does not average across them.
 *
 * KEY AGENTS:
 *   - weapon_states: agenda_setter/beneficiary (institutional/arbitrage) — hold arsenals, control verification architecture, bear the nominal Article VI obligation with minimal enforceable consequence
 *   - non_weapon_states_restrained: payer (moderate/constrained) — bear the compliance cost of restraint conditioned on a promise not reliably performed
 *   - near_nuclear_threshold_states: payer (powerful/constrained) — cite weapon-state non-disarmament to justify hedging
 *   - civil_nuclear_supplier_states: beneficiary (organized/mobile) — profit from Article IV cooperation, weak incentive to press disarmament
 *   - iaea_verification_apparatus: agenda_setter/observer (institutional/constrained) — asymmetric verification mandate is itself the structural fault line this reading targets
 *   - review_conference_delegations & withdrawal_precedent_states: excluded — objections on record, structurally unable to bind outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__grand_bargain, 0.62).
domain_priors:suppression_score(npt_article_iv_vi_pairing__grand_bargain, 0.58).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__grand_bargain, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, extractiveness, 0.62).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__grand_bargain, tangled_rope).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__grand_bargain, "NPT Article IV/VI Reciprocal Bargain (Grand Bargain Reading)").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__grand_bargain, "international_law/nuclear_governance").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__grand_bargain).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__grand_bargain, 'a6d64edb-dd01-4222-867f-c9a16e5ab36b').
narrative_ontology:cs_kernel_codification('a6d64edb-dd01-4222-867f-c9a16e5ab36b', fixed_text).
narrative_ontology:cs_authority_grounding('a6d64edb-dd01-4222-867f-c9a16e5ab36b', distributed).
narrative_ontology:cs_reading_relation('a6d64edb-dd01-4222-867f-c9a16e5ab36b', npt_article_iv_vi_pairing__nonproliferation_primary, coexists_with).
narrative_ontology:cs_reading_relation('a6d64edb-dd01-4222-867f-c9a16e5ab36b', npt_article_iv_vi_pairing__abolitionist, influences).
narrative_ontology:cs_axiom('a6d64edb-dd01-4222-867f-c9a16e5ab36b', foundational, article_vi_creates_conditional_reciprocity).
narrative_ontology:cs_axiom_status(article_vi_creates_conditional_reciprocity, holdable).
narrative_ontology:cs_axiom_grounding('a6d64edb-dd01-4222-867f-c9a16e5ab36b', article_vi_creates_conditional_reciprocity, conventional).
narrative_ontology:cs_axiom('a6d64edb-dd01-4222-867f-c9a16e5ab36b', foundational, disarmament_breach_licenses_nnws_reconsideration).
narrative_ontology:cs_axiom_status(disarmament_breach_licenses_nnws_reconsideration, holdable).
narrative_ontology:cs_axiom_grounding('a6d64edb-dd01-4222-867f-c9a16e5ab36b', disarmament_breach_licenses_nnws_reconsideration, instrumental).
narrative_ontology:cs_reference_frame('a6d64edb-dd01-4222-867f-c9a16e5ab36b', id_1968_negotiated_bargain_equilibrium).
narrative_ontology:cs_drift_state('a6d64edb-dd01-4222-867f-c9a16e5ab36b', post_2015_review_conference_breakdown, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a6d64edb-dd01-4222-867f-c9a16e5ab36b', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__grand_bargain, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__grand_bargain, weapon_states).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__grand_bargain, civil_nuclear_supplier_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__grand_bargain, non_weapon_states_restrained).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__grand_bargain, near_nuclear_threshold_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain nuclear arsenals while nominally bound by Article VI's 'good faith' disarmament pursuit. Under this reading they are treaty-breach actors when disarmament stalls, but in practice they control verification regimes, Security Council leverage, and enforcement mechanisms, so the reciprocal obligation rarely translates into binding consequence against them. They collect the nonproliferation benefit (restrained rivals) without a matching enforceable cost.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, weapon_states, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__grand_bargain, weapon_states, beneficiary).

% Forgo weapons development and submit to IAEA safeguards in exchange for civil nuclear cooperation and a disarmament promise from weapon states. Under the grand bargain reading, their restraint is explicitly conditional on weapon-state progress, and stalled disarmament is grounds to contest their own continued compliance — but exiting the treaty carries severe diplomatic and economic cost, so the conditionality is more rhetorical leverage than practical exit option.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, non_weapon_states_restrained, payer,
    moderate, generational, constrained, global).

% States with the technical capacity to weaponize quickly (enrichment infrastructure, delivery systems) who remain treaty-compliant partly because Article IV grants them civil nuclear legitimacy. Under this reading, persistent Article VI breach by weapon states weakens the normative case against their own eventual proliferation, and they increasingly cite weapon-state non-disarmament as justification for hedging or reduced compliance.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, near_nuclear_threshold_states, payer,
    powerful, generational, constrained, regional).

% Export enrichment technology, reactors, and fuel-cycle services under Article IV's cooperation guarantee, generating substantial commercial revenue. Their trade depends on the treaty's continued legitimacy and on NNWS remaining inside the framework; they have strong incentive to preserve the appearance of reciprocity without pressing hard for weapon-state disarmament that could destabilize supplier relationships.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, civil_nuclear_supplier_states, beneficiary,
    organized, generational, mobile, global).

% Administers safeguards inspections against NNWS under Article III/IV but has no equivalent mandated verification role over weapon-state disarmament under Article VI. This asymmetric verification architecture is itself contested under the grand bargain reading, which would require reciprocal verification to make the bargain enforceable rather than one-directional.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, iaea_verification_apparatus, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__grand_bargain, iaea_verification_apparatus, observer).

% NNWS delegations at NPT Review Conferences repeatedly raise Article VI non-compliance as grounds for reconsidering their own obligations, but consensus procedural rules and weapon-state veto-adjacent leverage mean these objections rarely produce binding text. Their objection is on record but structurally unable to move the treaty's operative machinery.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, review_conference_delegations, excluded,
    moderate, biographical, constrained, global).

% States that have invoked or threatened Article X withdrawal citing security concerns tied to unaddressed disarmament failures. Under the grand bargain reading their argument is structurally licensed (breach of VI justifies reconsidering IV/III compliance), but in practice withdrawal triggers sanctions and isolation regardless of the legal argument's merit, so the reciprocity the reading grants them is largely unusable leverage.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, withdrawal_precedent_states, excluded,
    moderate, biographical, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine collective-action problem of preventing horizontal nuclear proliferation while allowing broad access to civilian nuclear energy, by trading NNWS restraint for a weapon-state commitment to eventually disarm — coordinating around a shared long-term goal (a world with fewer or no nuclear weapons) that no single state can reach unilaterally.
% TRANSFER_FUNCTION: Moves compliance costs (safeguards submission, forgone weapons capability, technology restrictions) from non-weapon states to the treaty regime, in exchange for civil nuclear cooperation and a promissory disarmament commitment from weapon states; the promissory side of that exchange remains largely unperformed, so real value transfers one direction while obligation is nominally bidirectional.
% ABSENT_VOICES: States that have exited or threatened exit over the reciprocity failure (citing security concerns), and civil society/humanitarian disarmament advocates who argue the reciprocity frame itself legitimizes indefinite retention, are present in Review Conference debate but structurally unable to bind outcomes given consensus rules weapon states can block.
% DISAPPEARANCE_RATIONALE: Weapon states would argue the nonproliferation architecture (safeguards, supplier controls) persists largely intact without the reciprocity frame, since Article III verification is the operative mechanism. NNWS and threshold states would argue the treaty's normative legitimacy — the reason 190+ states remain inside it rather than pursuing independent capability — depends on the disarmament promise; if the reciprocal framing were formally abandoned, several threshold states indicate they would reassess continued compliance.
% FOUNDING_PROBLEM: In 1968, the founding problem was to prevent a rapid proliferation cascade among dozens of states with emerging nuclear capability, while giving those states a face-saving, resource-positive reason (civil nuclear access) to accept permanent non-weapon status — and to make that non-weapon status politically sustainable by pairing it with a weapon-state commitment to eventually disarm rather than freeze the world's nuclear hierarchy permanently.
% FOUNDING_PROBLEM_CORROBORATION: Weapon states' own diplomatic statements at Review Conferences continue to affirm Article VI as a live obligation (not repudiated), which corroborates that the founding reciprocity problem is treated as unresolved rather than obsolete. Independent bodies outside the weapon-state bloc — the UN Secretary-General's disarmament reports and the International Court of Justice's 1996 advisory opinion (obligation to pursue negotiations in good faith to a conclusion) — corroborate that the disarmament half of the bargain remains legally live and substantively unperformed, supporting the NNWS reading over a claim that the problem has been solved.
narrative_ontology:disappearance_verdict(npt_article_iv_vi_pairing__grand_bargain, contested).
narrative_ontology:founding_problem_status(npt_article_iv_vi_pairing__grand_bargain, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__grand_bargain, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(npt_article_iv_vi_pairing__grand_bargain, 'none', 1).
narrative_ontology:epsilon_provenance(npt_article_iv_vi_pairing__grand_bargain, 0.62, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.62) reflects that under this reading, real costs (restraint, safeguards, forgone capability) flow from NNWS/threshold states while the reciprocal disarmament benefit remains substantially unperformed — an asymmetric transfer dressed as a symmetric bargain. Suppression (0.58) is moderate: NNWS are not coerced by force but by structural cost of exit (sanctions, isolation, loss of civil nuclear access) and by consensus procedural rules that block their objections from altering the treaty's operative terms. Theater ratio (0.44) captures that Review Conference proceedings increasingly perform reciprocity review (working groups, disarmament pledges, NPT action plans) without those processes producing binding disarmament steps — rising over the interval as the gap between disarmament rhetoric and disarmament fact widened post-Cold War.
 *
 * PERSPECTIVAL GAP:
 *   From the weapon-state agenda-setter seat, the arrangement reads as durable, functioning coordination they actively maintain and periodically reaffirm at Review Conferences. From the NNWS/threshold-state payer seat, the same structure reads as an increasingly one-directional extraction where the enforceable half of the bargain binds them and the aspirational half does not bind the other side. The engine computes this divergence from the structural data (beneficiary/victim declarations, exit options, enforcement asymmetry) rather than from either party's self-description.
 *
 * DIRECTIONALITY LOGIC:
 *   Weapon states derive low d (beneficiary end): they collect the nonproliferation benefit of a stable, restrained field of NNWS while bearing the disarmament obligation only nominally, protected by their own control of enforcement machinery. NNWS and threshold states derive high d (target end): the restraint is real and binding on them; the reciprocal benefit is deferred, contested, and largely unenforceable by them individually. Civil nuclear supplier states sit near the beneficiary end via commercial capture of the Article IV cooperation channel, with limited stake in disarmament progress one way or the other.
 *
 * MANDATROPHY ANALYSIS:
 *   The grand bargain reading is specifically the reading built to prevent the coordination function (mutual restraint enabling shared security and civil nuclear access) from being mislabeled as pure extraction OR pure natural inevitability. It holds both a genuine coordination story (the 1968 bargain solved a real collective-action problem) and an asymmetric extraction pattern (the bargain's second half chronically underperforms) simultaneously — which is exactly the tangled_rope signature. Classifying this as a pure snare would erase the genuine security value NNWS still receive from the nonproliferation regime; classifying it as a pure rope would erase the decades-long pattern of unperformed reciprocal obligation that threshold states cite as justification for their own hedging.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_justiciability,
    'Is Article VI''s ''good faith'' disarmament pursuit a legally binding, enforceable obligation whose breach has consequences for weapon states, or is it aspirational language that creates political but not legal pressure?',
    'The 1996 ICJ Advisory Opinion held there is an obligation to pursue negotiations in good faith to a conclusion; whether this constitutes a justiciable, sanctionable breach (versus a non-binding aspiration) remains contested among states parties and international law scholars. Resolution would require either a binding tribunal ruling on a specific weapon-state breach claim, or sustained state practice treating Article VI as enforceable.',
    'If Article VI is genuinely enforceable, the tangled_rope classification strengthens (the coordination/extraction asymmetry becomes a live legal claim rather than a rhetorical one). If Article VI is confirmed non-justiciable in practice, this reading''s core premise weakens and the constraint drifts toward the nonproliferation_primary reading''s structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_justiciability, conceptual, 'Whether Article VI creates an enforceable obligation or only aspirational pressure — the central structural question this reading depends on.').

omega_variable(
    reciprocity_kernel_framing_choice,
    'Is the grand_bargain reading''s framing of Article IV/VI as symmetric reciprocal obligations the correct structural read of the treaty''s drafting history and negotiating record, or does the historical record support the nonproliferation_primary reading (Article VI as a concession offered to secure NNWS signature, not a binding trade)?',
    'Comparative analysis of 1968 negotiating history (ENDC records, ratification debates) against post-1995 indefinite extension conference bargaining, which explicitly traded permanent extension for renewed disarmament commitments (the 13 Practical Steps, 2000 Review Conference).',
    'If the negotiating record supports genuine bilateral conditionality, this reading''s tangled_rope classification is well-grounded structurally, not merely rhetorically. If the record supports a weaker, non-binding concession framing, the grand_bargain reading is itself a retrospective reconstruction with less historical grounding than nonproliferation_primary, though it remains a live position held by many NNWS diplomats regardless of its historical pedigree.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_kernel_framing_choice, conceptual, 'Alternative framing consideration: whether the reciprocal-bargain premise is a historically grounded reading or a retrospectively constructed advocacy position — routed here per the CS-framing under-determination guidance rather than left implicit.').

omega_variable(
    withdrawal_leverage_realism,
    'Does the reciprocity this reading grants NNWS (licensing withdrawal or non-compliance reconsideration upon Article VI breach) function as real leverage, or is it structurally unusable given the severe sanctions and isolation costs that attach to any actual withdrawal attempt?',
    'Case study of the small number of actual or threatened NPT withdrawals (DPRK 2003 being the clearest case) and their consequences, to assess whether the legal license to withdraw translates into a credible, usable threat or remains theoretical.',
    'If withdrawal is structurally unusable regardless of the legal argument''s merit, the grand_bargain reading''s practical enforcement mechanism is largely theatrical, raising the effective theater_ratio and pushing the constraint''s lived operation closer to how the nonproliferation_primary reading already describes it (Article VI as non-binding in practice, whatever its formal status).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(withdrawal_leverage_realism, empirical, 'Whether the reciprocity-derived withdrawal license is usable leverage or a theoretical entitlement with no practical exercise path.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__grand_bargain, 1968, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1968, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 1968, 0.2).
narrative_ontology:measurement(npt__tr_t1980, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 1980, 0.28).
narrative_ontology:measurement(npt__tr_t1995, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 1995, 0.33).
narrative_ontology:measurement(npt__tr_t2005, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 2005, 0.38).
narrative_ontology:measurement(npt__tr_t2015, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 2015, 0.41).
narrative_ontology:measurement(npt__tr_t2026, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 2026, 0.44).

% Extraction over time
narrative_ontology:measurement(npt__be_t1968, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 1968, 0.35).
narrative_ontology:measurement(npt__be_t1980, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 1980, 0.42).
narrative_ontology:measurement(npt__be_t1995, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 1995, 0.48).
narrative_ontology:measurement(npt__be_t2005, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 2005, 0.55).
narrative_ontology:measurement(npt__be_t2015, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 2015, 0.59).
narrative_ontology:measurement(npt__be_t2026, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 2026, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1968, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 1968, 0.4).
narrative_ontology:measurement(npt__su_t1980, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 1980, 0.45).
narrative_ontology:measurement(npt__su_t1995, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 1995, 0.5).
narrative_ontology:measurement(npt__su_t2005, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 2005, 0.53).
narrative_ontology:measurement(npt__su_t2015, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 2015, 0.56).
narrative_ontology:measurement(npt__su_t2026, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 2026, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__grand_bargain, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_article_iv_vi_pairing__grand_bargain, 0.12).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__grand_bargain, npt_article_iv_vi_pairing__nonproliferation_primary).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__grand_bargain, npt_article_iv_vi_pairing__abolitionist).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the npt_article_iv_vi_pairing kernel. nonproliferation_primary treats Article VI as aspirational/non-justiciable and Article IV as conditioned solely on Article III verification (lower extractiveness claim, weapon-state-favorable framing). abolitionist treats Article IV itself as illegitimate given dual-use proliferation risk regardless of disarmament progress, grounding authority in TPNW/humanitarian law rather than NPT reciprocity logic (likely highest extractiveness claim, targeting the civil-nuclear-cooperation function itself as the harm). This grand_bargain reading occupies the middle position: it accepts the treaty's internal coordination logic as legitimate in principle but treats its asymmetric non-performance as the extractive element. Each reading carries its own epsilon per the epsilon-invariance principle; they are linked here, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
