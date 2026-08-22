% ============================================================================
% CONSTRAINT STORY: ost_article_ii_non_appropriation__international_regime
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ost_article_ii_non_appropriation__international_regime, []).

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
 *   constraint_id: ost_article_ii_non_appropriation__international_regime
 *   human_readable: Article II Deferral to Future International Space Resource Regime
 *   domain: international_space_law/treaty_interpretation/commons_governance
 *
 * SUMMARY:
 *   This story instantiates the 'international_regime' reading of the
 *   contested Outer Space Treaty Article II non-appropriation kernel: the
 *   claim that Article II deliberately defers the appropriation question to a
 *   future multilateral resource regime (analogous to Article XI of the Moon
 *   Agreement or the UNCLOS deep-seabed regime), such that neither
 *   extraction-permissive nor conservationist readings currently hold treaty
 *   authority. Structurally this is a scaffold that has failed to sunset —
 *   the deferral was meant to be transitional pending a follow-on agreement,
 *   but no such agreement has emerged in nearly six decades, and the interim
 *   vacuum has been filled unilaterally by domestic legislation (US 2015,
 *   Luxembourg 2017, UAE, Japan) asserting extraction rights the deferred
 *   multilateral process was supposed to adjudicate collectively. This is a
 *   distinct constraint from the extraction_permissive reading (which claims
 *   Article II already answers the private-ownership question in the
 *   affirmative) and the commons_conservation reading (which claims Article
 *   II already answers it in the negative) — this reading's defining claim is
 *   that NEITHER of those readings has authority yet, which is itself a
 *   structurally different assertion with a different beneficiary/victim map
 *   and a different epsilon.
 *
 * KEY AGENTS:
 *   - first_mover_extraction_firms: primary beneficiary of the ambiguity itself
 *   - spacefaring_states_with_domestic_licensing_regimes: agenda-setters who benefit from slow-walking the multilateral process while filling the gap domestically
 *   - non_spacefaring_states: formal stakeholders in any future common-heritage regime with no present leverage
 *   - future_multilateral_regime_negotiators: inherit a fait accompli
 *   - smaller_commercial_entrants: bear real legal-uncertainty cost without first-mover cover
 *   - treaty_drafters_1967: excluded, cannot attest to whether the deferral's intent survives 58 years of non-resolution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ost_article_ii_non_appropriation__international_regime, 0.42).
domain_priors:suppression_score(ost_article_ii_non_appropriation__international_regime, 0.28).
domain_priors:theater_ratio(ost_article_ii_non_appropriation__international_regime, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, extractiveness, 0.42).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__international_regime, scaffold).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__international_regime, "Article II Deferral to Future International Space Resource Regime").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__international_regime, "international_space_law/treaty_interpretation/commons_governance").

narrative_ontology:has_sunset_clause(ost_article_ii_non_appropriation__international_regime).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__international_regime, 'dead1864-1e64-4575-8be3-09f295fadeb7').
narrative_ontology:cs_kernel_codification('dead1864-1e64-4575-8be3-09f295fadeb7', fixed_text).
narrative_ontology:cs_authority_grounding('dead1864-1e64-4575-8be3-09f295fadeb7', distributed).
narrative_ontology:cs_reading_relation('dead1864-1e64-4575-8be3-09f295fadeb7', ost_article_ii_non_appropriation__extraction_permissive, coexists_with).
narrative_ontology:cs_reading_relation('dead1864-1e64-4575-8be3-09f295fadeb7', ost_article_ii_non_appropriation__commons_conservation, influences).
narrative_ontology:cs_axiom('dead1864-1e64-4575-8be3-09f295fadeb7', foundational, appropriation_question_requires_multilateral_settlement).
narrative_ontology:cs_axiom_status(appropriation_question_requires_multilateral_settlement, holdable).
narrative_ontology:cs_axiom_grounding('dead1864-1e64-4575-8be3-09f295fadeb7', appropriation_question_requires_multilateral_settlement, conventional).
narrative_ontology:cs_axiom('dead1864-1e64-4575-8be3-09f295fadeb7', foundational, unilateral_domestic_legislation_cannot_substitute_for_treaty_authority).
narrative_ontology:cs_axiom_status(unilateral_domestic_legislation_cannot_substitute_for_treaty_authority, holdable).
narrative_ontology:cs_axiom_grounding('dead1864-1e64-4575-8be3-09f295fadeb7', unilateral_domestic_legislation_cannot_substitute_for_treaty_authority, conventional).
narrative_ontology:cs_reference_frame('dead1864-1e64-4575-8be3-09f295fadeb7', id_1967_deferred_settlement_framework).
narrative_ontology:cs_drift_state('dead1864-1e64-4575-8be3-09f295fadeb7', contemporary_commercial_space_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('dead1864-1e64-4575-8be3-09f295fadeb7', '').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__international_regime, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__international_regime, first_mover_extraction_firms).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__international_regime, spacefaring_states_with_domestic_licensing_regimes).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__international_regime, non_spacefaring_states).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__international_regime, future_multilateral_regime_negotiators).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__international_regime, smaller_commercial_entrants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate in the regulatory grey zone the deferral creates, securing domestic licenses (US Commercial Space Launch Competitiveness Act, Luxembourg's space resources law) that assert extraction rights under national law while the treaty question stays open. Benefit precisely from the absence of an authoritative multilateral answer — every year of non-resolution is a year of unchallenged operating space to establish facts on the ground before any future regime could impose royalty, licensing, or benefit-sharing obligations retroactively.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, first_mover_extraction_firms, beneficiary,
    organized, biographical, arbitrage, global).

% Enact domestic space resource legislation that fills the interpretive vacuum in their own favor, while simultaneously participating in (and slow-walking) UN COPUOS working groups on a resource regime. They set the negotiating agenda's pace and can afford to wait — every year without a binding multilateral instrument is a year their domestic legal facts accumulate unchallenged.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, spacefaring_states_with_domestic_licensing_regimes, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(ost_article_ii_non_appropriation__international_regime, spacefaring_states_with_domestic_licensing_regimes, agenda_setter).

% Lack the technical or financial capacity to extract resources themselves but have a formal stake in any 'common heritage of mankind' benefit-sharing regime under Article XI language. Their leverage exists only in the negotiating room, and the negotiating room never closes — the deferral means their formal equity claim never converts into an operative benefit stream because there is no operative regime to claim it from.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, non_spacefaring_states, payer,
    powerless, generational, trapped, global).

% Whoever eventually convenes a binding international regime inherits a negotiating table where facts on the ground (established extraction operations, domestic legal precedents, sunk capital) have already shifted the bargaining leverage decisively toward the states and firms that moved first. They bear the cost of trying to retrofit multilateral legitimacy onto a fait accompli.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, future_multilateral_regime_negotiators, payer,
    institutional, civilizational, constrained, universal).

% Cannot access the same insurance, financing, or state-backed legal cover as first movers with sympathetic domestic licensing regimes. The legal uncertainty that large firms treat as arbitrage opportunity is, for smaller entrants, a genuine barrier — investors discount projects lacking clear property rights, and only well-capitalized incumbents can absorb the risk of an adverse future regime.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, smaller_commercial_entrants, payer,
    moderate, biographical, constrained, global).

% The original OST negotiators deferred the appropriation question deliberately, expecting an Article XI-style follow-on agreement (as eventually happened, contentiously, for the deep seabed and the Moon Agreement). They are not present to arbitrate whether five decades of non-resolution honors or betrays that deferral's intent.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, treaty_drafters_1967, excluded,
    institutional, civilizational, analytical, universal).

% Document the interpretive stalemate, publish competing readings, and are consulted by COPUOS working groups, but hold no binding authority. Their scholarship maps the grey zone without closing it.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, international_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ost_article_ii_non_appropriation__international_regime, first_mover_extraction_firms).
narrative_ontology:fixing_cost_class(ost_article_ii_non_appropriation__international_regime, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the possibility of a genuinely multilateral, negotiated resource regime by not prematurely foreclosing either extraction rights or conservation obligations — in principle, keeping the door open for a fairer outcome than either extreme reading would lock in unilaterally.
% TRANSFER_FUNCTION: Transfers de facto first-mover advantage from the deferral's temporal gap: states and firms that act now under permissive domestic law accumulate operational, financial, and precedential capital that a later multilateral regime will find costly or impossible to reverse — effectively moving future negotiating leverage from non-spacefaring and slower-moving states to whoever moves first.
% ABSENT_VOICES: The 1967 treaty drafters cannot testify to whether the deferral's fifty-year non-resolution matches their intent. Non-spacefaring states have UN General Assembly voice but no enforcement mechanism, and are structurally unable to compel treaty conclusion against the interest of states that benefit from continued ambiguity.
% DISAPPEARANCE_RATIONALE: If the deferral resolved overnight into either a binding extraction-permissive regime or a binding conservation regime, first-mover firms would either gain enforceable property rights (ending the current legal risk discount) or lose their asserted domestic-law claims entirely (ending the arbitrage). Either resolution would collapse the grey zone that currently lets multiple incompatible domestic legal theories coexist — capital allocation, insurance markets, and diplomatic posturing around lunar and asteroid resources would all reorganize around the new settled rule.
% FOUNDING_PROBLEM: In 1967, negotiators could not agree on how to treat commercial exploitation of celestial resources — technology was speculative, no state had done it, and forcing a substantive rule risked either killing the treaty (if extraction-restrictive) or ratifying an unwanted extraction free-for-all (if permissive). Deferring to a future Article XI-style regime let the treaty conclude without resolving the distributional question.
% FOUNDING_PROBLEM_CORROBORATION: COPUOS Legal Subcommittee working group discussions (ongoing since 2016) and independent international law scholarship (e.g., analyses from the Hague International Space Resources Governance Working Group, which includes participants outside both extraction-firm and dominant-state interests) attest that the distributional question remains genuinely unresolved and that domestic legislation has outpaced multilateral consensus — this is not merely asserted by the states or firms who benefit from the current gap.
narrative_ontology:disappearance_verdict(ost_article_ii_non_appropriation__international_regime, world_rearranges).
narrative_ontology:founding_problem_status(ost_article_ii_non_appropriation__international_regime, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ost_article_ii_non_appropriation__international_regime, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ost_article_ii_non_appropriation__international_regime, 'none', 1).
narrative_ontology:epsilon_provenance(ost_article_ii_non_appropriation__international_regime, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ost_article_ii_non_appropriation__international_regime_tests).
:- end_tests(ost_article_ii_non_appropriation__international_regime_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored moderate and rising (0.05 to 0.42) because the deferral itself is not extractive at origin — it was a genuine, good-faith punt on an unresolved distributional question at treaty conclusion. But the extraction climbs as the deferral calcifies: each year of non-resolution converts into differential advantage for whoever is positioned to act unilaterally in the interim, which compounds. Theater ratio rises sharply (0.2 to 0.58) because COPUOS working-group activity, symposia, and non-binding 'building blocks' documents have proliferated over the past decade without producing the treaty instrument the deferral was supposed to be transitional toward — this is the scaffold's sunset clause failing to trigger while institutional activity around it intensifies performatively. Suppression is authored moderate-low (0.28) because no one is coerced into accepting the ambiguity; the persistence is closer to negotiating deadlock than active enforcement. Accessibility collapse is moderate (0.35): a binding regime remains conceptually and legally reachable, it is simply not politically achievable given the zero-sum distributional stakes.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of a first-mover firm or a spacefaring state's space agency, the deferral looks like prudent, ongoing multilateral process — 'we are still working on it.' From the seat of a non-spacefaring state or a future regime negotiator, the same fifty-eight years of non-resolution looks like a structure that has stopped functioning as deferral and started functioning as de facto permission by attrition. The engine computes these as different seat-level classifications from the same structural data; this story does not adjudicate between the readings, only authors the international_regime reading's own structural claim.
 *
 * DIRECTIONALITY LOGIC:
 *   First-mover firms and their sponsoring states are beneficiaries because the absence of a binding rule is itself the valuable asset — every year without resolution is a year their domestic legal theory operates unchallenged and their sunk investment compounds into negotiating leverage. Non-spacefaring states and future negotiators are victims not because anyone extracts a payment from them directly, but because the deferral consumes the only resource that would have equalized their position: time before facts on the ground hardened. Smaller commercial entrants are victims of the same ambiguity that first movers profit from, because legal uncertainty is asymmetric — well-capitalized incumbents can absorb regulatory risk that smaller entrants cannot, so ambiguity itself functions as a barrier to entry for the less-resourced even though it is nominally neutral as to all parties.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (irreconcilable 1967 positions on commercial exploitation) is still live in the sense that the underlying distributional conflict between spacefaring and non-spacefaring interests has never been resolved — but the SCAFFOLD FUNCTION (temporary deferral pending prompt multilateral resolution) has plainly outlived its declared transitional character. This is the mandatrophy signature: a scaffold whose sunset clause was implicit in the promise of a 'future international regime' and whose failure to sunset has itself become the mechanism by which first-mover advantage accumulates. Classifying this as scaffold (not snare) matters because the coordination function is real and was genuinely intended — the framers were not extracting anything in 1967, they were managing an irreducible disagreement under uncertainty. But the metrics should honestly show the drift toward extraction as the non-resolution is exploited by parties who did not exist as such in 1967.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deferral_intent_vs_attrition,
    'Did the 1967 drafters intend the appropriation question to remain open indefinitely absent consensus, or did they anticipate a reasonably prompt follow-on agreement (as materialized, contentiously, in the 1979 Moon Agreement''s Article XI, which failed to achieve broad ratification)? If prompt resolution was expected and never delivered, is the current fifty-eight-year non-resolution a betrayal of the deferral''s design or a foreseeable consequence of the same distributional conflict the deferral was meant to manage?',
    'Diplomatic history research into the 1966-67 COPUOS travaux préparatoires and comparison with the near-contemporaneous UNCLOS deep-seabed ''common heritage'' negotiations, which faced a structurally similar deferral and took until 1994 (and a substantially renegotiated implementing agreement) to operationalize.',
    'If prompt resolution was clearly expected, the current non-resolution supports treating this as a mandatrophy-flagged failed scaffold with intensifying extraction. If indefinite openness was anticipated as a live possibility, the persistence is closer to the original design working as intended, and the extraction trend is better explained by later domestic legislative choices than by the deferral structure itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deferral_intent_vs_attrition, empirical, 'Whether the deferral''s open-endedness is a design failure or a foreseen possibility.').

omega_variable(
    which_reading_has_authority,
    'Given that all three readings of Article II (international_regime, extraction_permissive, commons_conservation) are held by different parties with no adjudicating international court having ruled on the question, is the international_regime reading itself authoritative, or is it merely the position of states and scholars who prefer inaction to either extreme?',
    'A ruling by the International Court of Justice or a binding arbitration on an actual extraction dispute would resolve which reading (if any) reflects customary international law; absent such a ruling, state practice and persistent objector doctrine analysis by international law scholarship is the best available proxy.',
    'If the international_regime reading is itself just one more contested position rather than a neutral ''no one has authority yet'' baseline, then this constraint''s claimed scaffold status is itself a further move in the same distributional contest the kernel is about, not a description above the contest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(which_reading_has_authority, conceptual, 'Whether the deferral reading is a neutral description or itself a partisan position in the same contest.').

omega_variable(
    sunset_trigger_ambiguity,
    'What would count as the ''future international regime'' arriving — is a non-binding COPUOS ''building blocks'' framework sufficient, or does the scaffold only sunset upon a binding, widely-ratified treaty instrument comparable to UNCLOS?',
    'Track whether any COPUOS output achieves binding treaty status with broad ratification (as opposed to non-binding guidelines), and whether major spacefaring states'' domestic legislation is amended to conform to it rather than the reverse.',
    'A low bar for sunset (any multilateral framework, binding or not) would mean the scaffold has partially triggered already via ongoing COPUOS process; a high bar (binding, ratified, enforceable) means the scaffold remains fully unfired and the extraction trend documented in the measurements is likely to continue.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sunset_trigger_ambiguity, conceptual, 'What counts as the deferred regime''s arrival, which determines whether the scaffold has already partially sunset.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__international_regime, 1967, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost__tr_t1967, ost_article_ii_non_appropriation__international_regime, theater_ratio, 1967, 0.2).
narrative_ontology:measurement(ost__tr_t1980, ost_article_ii_non_appropriation__international_regime, theater_ratio, 1980, 0.3).
narrative_ontology:measurement(ost__tr_t1995, ost_article_ii_non_appropriation__international_regime, theater_ratio, 1995, 0.4).
narrative_ontology:measurement(ost__tr_t2010, ost_article_ii_non_appropriation__international_regime, theater_ratio, 2010, 0.48).
narrative_ontology:measurement(ost__tr_t2015, ost_article_ii_non_appropriation__international_regime, theater_ratio, 2015, 0.52).
narrative_ontology:measurement(ost__tr_t2020, ost_article_ii_non_appropriation__international_regime, theater_ratio, 2020, 0.56).
narrative_ontology:measurement(ost__tr_t2025, ost_article_ii_non_appropriation__international_regime, theater_ratio, 2025, 0.58).

% Extraction over time
narrative_ontology:measurement(ost__be_t1967, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 1967, 0.05).
narrative_ontology:measurement(ost__be_t1980, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 1980, 0.08).
narrative_ontology:measurement(ost__be_t1995, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 1995, 0.12).
narrative_ontology:measurement(ost__be_t2010, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 2010, 0.18).
narrative_ontology:measurement(ost__be_t2015, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 2015, 0.28).
narrative_ontology:measurement(ost__be_t2020, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 2020, 0.37).
narrative_ontology:measurement(ost__be_t2025, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 2025, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(ost_article_ii_non_appropriation__international_regime, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ost_article_ii_non_appropriation__international_regime, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ost_article_ii_non_appropriation__international_regime, 0.12).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__international_regime, ost_article_ii_non_appropriation__extraction_permissive).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__international_regime, ost_article_ii_non_appropriation__commons_conservation).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the colloquial 'what does OST Article II mean for resource extraction' question, per the ε-invariance principle. The extraction_permissive reading claims Article II already authorizes private extraction (low ε from the firm's perspective, contested elsewhere); the commons_conservation reading claims Article II already prohibits de facto appropriation via extraction (high ε claimed against extracting firms, near-mountain claimed by conservation advocates); this international_regime reading claims neither is yet authoritative and that the deferral itself has drifted into extraction-enabling ambiguity. Each has a different ε, different beneficiary/victim structure, and different claimed type — they are linked via affects_constraints rather than merged into one variable-ε story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
