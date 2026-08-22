% ============================================================================
% CONSTRAINT STORY: dual_class_legitimacy__minority_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dual_class_legitimacy__minority_extraction, []).

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
 *   constraint_id: dual_class_legitimacy__minority_extraction
 *   human_readable: Dual-Class Share Structure — Minority Extraction Reading
 *   domain: corporate_governance/securities_law
 *
 * SUMMARY:
 *   This story authors the minority-extraction reading of the dual-class
 *   legitimacy kernel: dual-class voting structures and their accompanying
 *   controlled-company exemptions are read as a mechanism that transfers
 *   governance value and control-premium capture from the shareholders who
 *   bear the majority of capital and downside risk to founder-controllers who
 *   hold a minority economic stake but supermajority voting power. The
 *   extraction accumulates over the interval as companies age past the
 *   founding-execution phase (where the stewardship justification is
 *   strongest) into a steady state where the founder retains control
 *   indefinitely with no re-election of the underlying justification.
 *
 * KEY AGENTS:
 *   - founder_controllers: agenda-setter and beneficiary, institutional power, arbitrage exit — sets and benefits from the voting structure
 *   - class_a_public_shareholders: payer, powerless, trapped exit — bears capital risk without proportional voice
 *   - index_fund_beneficiaries: payer, powerless, trapped exit — involuntary indirect exposure via mandate-driven index holding
 *   - institutional_asset_managers: excluded, organized power but structurally capped voting influence
 *   - stock_exchanges: agenda-setter and beneficiary via listing competition
 *   - securities_regulators: observer, has deferred structural intervention to disclosure regime
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dual_class_legitimacy__minority_extraction, 0.71).
domain_priors:suppression_score(dual_class_legitimacy__minority_extraction, 0.68).
domain_priors:theater_ratio(dual_class_legitimacy__minority_extraction, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, extractiveness, 0.71).
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_class_legitimacy__minority_extraction, tangled_rope).
narrative_ontology:human_readable(dual_class_legitimacy__minority_extraction, "Dual-Class Share Structure — Minority Extraction Reading").
narrative_ontology:topic_domain(dual_class_legitimacy__minority_extraction, "corporate_governance/securities_law").

domain_priors:requires_active_enforcement(dual_class_legitimacy__minority_extraction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_class_legitimacy__minority_extraction, '93ddc2e4-9969-49c5-baf3-7b0befeb58d4').
narrative_ontology:cs_kernel_codification('93ddc2e4-9969-49c5-baf3-7b0befeb58d4', formalized).
narrative_ontology:cs_authority_grounding('93ddc2e4-9969-49c5-baf3-7b0befeb58d4', extraction).
narrative_ontology:cs_interpretation_layer_present('93ddc2e4-9969-49c5-baf3-7b0befeb58d4').
narrative_ontology:cs_reading_relation('93ddc2e4-9969-49c5-baf3-7b0befeb58d4', dual_class_legitimacy__founder_stewardship, coexists_with).
narrative_ontology:cs_reading_relation('93ddc2e4-9969-49c5-baf3-7b0befeb58d4', dual_class_legitimacy__disclosure_consent, coexists_with).
narrative_ontology:cs_axiom('93ddc2e4-9969-49c5-baf3-7b0befeb58d4', foundational, capital_at_risk_entitles_proportional_voice).
narrative_ontology:cs_axiom_status(capital_at_risk_entitles_proportional_voice, holdable).
narrative_ontology:cs_axiom_grounding('93ddc2e4-9969-49c5-baf3-7b0befeb58d4', capital_at_risk_entitles_proportional_voice, deontological).
narrative_ontology:cs_axiom('93ddc2e4-9969-49c5-baf3-7b0befeb58d4', secondary, control_without_proportional_capital_is_rent_extraction).
narrative_ontology:cs_axiom_status(control_without_proportional_capital_is_rent_extraction, holdable).
narrative_ontology:cs_axiom_grounding('93ddc2e4-9969-49c5-baf3-7b0befeb58d4', control_without_proportional_capital_is_rent_extraction, empirically_contingent).
narrative_ontology:cs_reference_frame('93ddc2e4-9969-49c5-baf3-7b0befeb58d4', one_share_one_vote_baseline).
narrative_ontology:cs_drift_state('93ddc2e4-9969-49c5-baf3-7b0befeb58d4', post_ipo_boom_controlled_company_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('93ddc2e4-9969-49c5-baf3-7b0befeb58d4', '').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__minority_extraction, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__minority_extraction, founder_controllers).
narrative_ontology:constraint_victim(dual_class_legitimacy__minority_extraction, class_a_public_shareholders).
narrative_ontology:constraint_victim(dual_class_legitimacy__minority_extraction, index_fund_beneficiaries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__minority_extraction, stock_exchanges).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__minority_extraction, capital_at_risk_entitles_governance_voice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds Class B shares with 10-to-1 or greater voting weight relative to capital contributed. Sets board composition, blocks shareholder proposals, and is structurally immune to proxy contests or hostile takeovers regardless of operating performance. Exchange controlled-company exemptions remove requirements for independent board majorities, independent compensation committees, and independent nominating committees. Can sell Class B shares at a control premium the public market never prices into Class A shares.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, founder_controllers, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__minority_extraction, founder_controllers, beneficiary).

% Purchased shares representing the substantial majority of capital at risk but hold negligible aggregate voting power. Bear full economic exposure to dilutive acquisitions, excessive founder compensation, and self-dealing transactions that a normal board would screen, but cannot elect directors who would object. Exit means selling the stock — which does not change the governance structure for remaining or future holders and typically occurs at a valuation discount attributable to the very structure being exited.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, class_a_public_shareholders, payer,
    powerless, biographical, trapped, national).

% Retail savers and pensioners whose retirement assets are allocated into dual-class companies via broad index inclusion, with no individual choice in the matter. They bear the same governance-voice asymmetry as direct Class A holders but lack even the option of a considered individual exit, since fund mandates require holding index constituents regardless of governance structure.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, index_fund_beneficiaries, payer,
    powerless, generational, trapped, national).

% Large asset managers vote proxies on behalf of index and active clients and have publicly opposed unequal voting structures, but their votes are structurally capped by the very share-class weighting they object to — their opposition is recorded but cannot translate into board seats or bylaw change while the controller retains majority voting power. Their formal objections appear in proxy voting guidelines but produce no binding effect.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, institutional_asset_managers, excluded,
    organized, biographical, constrained, national).

% Compete with each other for high-profile listings and have progressively relaxed listing standards to permit dual-class structures and controlled-company exemptions, collecting listing fees regardless of the governance structure adopted. Could tighten listing standards unilaterally but bear reputational and revenue risk from unilaterally doing so while competing exchanges do not.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, stock_exchanges, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__minority_extraction, stock_exchanges, beneficiary).

% Have authority over disclosure requirements but have historically deferred voting-structure design to exchange listing standards and corporate law, treating dual-class structures as a disclosed-and-consented-to feature rather than a governance defect requiring intervention. Periodically studies the issue without mandating sunset provisions.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__minority_extraction, securities_regulators, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In its stated form, the structure lets founders execute long-horizon strategy without being disciplined by short-term market pressure at each earnings cycle — a genuine coordination problem in principle (time-horizon mismatch between founders and diversified public shareholders).
% TRANSFER_FUNCTION: Moves governance control and its associated rents (excess compensation, self-dealing terms, entrenchment against value-maximizing sales) from the shareholders who supplied the majority of capital and bear the majority of downside risk to the founder-controller who supplied a minority of capital.
% ABSENT_VOICES: Class A shareholders as a class have no seat at the table that sets or revises the voting structure — that decision is made once, at IPO, by the founder and underwriters, before public capital is committed; subsequent objection has no mechanism to bind. Index fund beneficiaries are doubly absent: absent from the IPO-stage decision AND absent from any individual choice to hold the security at all.
% DISAPPEARANCE_RATIONALE: If dual-class structures and controlled-company exemptions vanished overnight, boards at affected companies would become removable by ordinary shareholder vote, self-dealing transactions would face independent committee review, and control premiums currently captured entirely by founders on any sale of control would be shared pro-rata with all shareholders under one-share-one-vote sale rules — a substantial reallocation of governance value and control-premium capture.
% FOUNDING_PROBLEM: Founders of high-growth companies argued that public markets impose short-termist discipline incompatible with executing long-horizon technology or mission-driven strategy, and that without insulation from quarterly pressure and hostile takeover threat, the company's distinctive value-creating strategy would be abandoned under market pressure.
% FOUNDING_PROBLEM_CORROBORATION: Founders and their underwriters attest the problem is live and the structure remains necessary. Independent evidence is mixed and comes from outside the beneficiary set: academic finance studies (Institutional Shareholder Services research, CII studies) find dual-class firms underperform single-class peers on long-run governance quality metrics after 7-10 years, and several dual-class founders have sold control-premium stakes privately while public Class A holders received no comparable premium — corroborating the extraction reading rather than the stewardship justification for the post-founding-decade period specifically.
narrative_ontology:disappearance_verdict(dual_class_legitimacy__minority_extraction, world_rearranges).
narrative_ontology:founding_problem_status(dual_class_legitimacy__minority_extraction, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__minority_extraction, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dual_class_legitimacy__minority_extraction, 'none', 1).
narrative_ontology:epsilon_provenance(dual_class_legitimacy__minority_extraction, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dual_class_legitimacy__minority_extraction_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dual_class_legitimacy__minority_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dual_class_legitimacy__minority_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.42 to 0.71 across the interval as the founding-era justification (genuine time-horizon mismatch, real coordination need) gives way to steady-state control retention with no functional re-justification mechanism — the theater_ratio climbs in parallel (0.20 to 0.45) as stewardship rhetoric persists while the underlying coordination need (protection from short-term IPO-era market pressure) diminishes with company maturity. Suppression is authored moderately-high and rising (0.55 to 0.68): the suppressive mechanism is not physical coercion but structural — no re-vote mechanism, no sunset, and proxy votes that cannot bind. Accessibility_collapse (0.62) reflects that once Class A shares are purchased, alternatives (voting reform, board change) are structurally foreclosed for that class of holder; resistance (0.58) reflects real organized opposition from asset managers and academic critics that nonetheless cannot translate into binding change.
 *
 * DIRECTIONALITY LOGIC:
 *   Founder_controllers sit at the full-beneficiary end: they collect governance value, block dilution of their control, and can realize control premiums unavailable to Class A holders. Class_a_public_shareholders and index_fund_beneficiaries sit near the full-target end: they supply the majority of capital, bear the downside risk of self-dealing or entrenchment, and have trapped exit (selling does not fix the structure, only relocates exposure to the next buyer). Institutional_asset_managers are excluded rather than positioned on the beneficiary/victim axis directly — their objection is real but non-binding, which is why they are marked excluded rather than payer despite bearing indirect fiduciary exposure on behalf of their own beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting long-horizon strategy from short-term market pressure) may have been genuinely live at IPO and for some years after. Declaring founding_problem_status as contested rather than flatly dead prevents mislabeling the entire structure as pure extraction from inception — there is likely a real window in which the coordination function operated as claimed. The tangled_rope classification (rather than snare) reflects this: a genuine coordination function existed and requires_active_enforcement + beneficiary + victim are all present, but the enforcement (exchange listing rules, absence of sunset, absence of re-vote mechanism) has calcified a temporary justification into a permanent extraction channel with no scheduled review — this is exactly the mandatrophy pattern the classification exists to catch, distinct from the founder_stewardship sibling reading which would read the same facts as ongoing legitimate coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disambiguation,
    'This story instantiates the minority_extraction reading of the dual_class_legitimacy kernel. The founder_stewardship reading (concentrated control legitimately serves all shareholders via long-horizon execution) and the disclosure_consent reading (legitimacy rests on informed disclosure and consent, not control parity) are structurally distinct constraints authored separately, sharing the same underlying dual-class arrangement but assigning it different ε, different beneficiary/victim structure, and different classification.',
    'Not empirically resolvable as a single fact — the three readings correspond to different normative premises about what governance entitlement rests on (capital-and-risk vs. stewardship-capacity vs. informed-consent). Comparative longitudinal study of post-IPO founder conduct and control-premium capture across dual-class firms could shift confidence in which reading better predicts outcomes, but would not eliminate the normative disagreement.',
    'If the founder_stewardship reading is adopted instead, the same facts would classify closer to rope or scaffold (a temporary or ongoing coordination benefit) rather than tangled_rope; if disclosure_consent is adopted, the beneficiary/victim structure dissolves entirely since informed purchasers are treated as full participants in a bargain rather than victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disambiguation, conceptual, 'This story is one of three sibling readings of the dual_class_legitimacy kernel; the readings differ on what grounds legitimate governance entitlement.').

omega_variable(
    founding_window_duration,
    'Even accepting the minority-extraction reading''s premise, how long does the genuine coordination function (insulation from short-term pressure during early execution) plausibly last before the structure becomes pure entrenchment with no offsetting coordination value?',
    'Empirical study correlating years-since-IPO with operating performance divergence between dual-class and single-class comparables, and with the rate of value-destroying self-dealing transactions, could identify an empirical inflection point.',
    'A short founding window (3-5 years) would support mandatory sunset provisions as the appropriate fix; a long or indefinite window would weaken the case that the current structure has drifted into pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_window_duration, empirical, 'How long the coordination justification plausibly persists before extraction dominates.').

omega_variable(
    exchange_competition_race_to_bottom,
    'Is the relaxation of listing standards permitting dual-class structures a response to genuine issuer preference reflecting efficient contracting, or a competitive race-to-the-bottom among exchanges competing for listing fee revenue regardless of governance quality?',
    'Comparative analysis of listing standard changes across competing exchanges and their timing relative to major listing wins/losses; issuer surveys on stated reasons for exchange choice.',
    'If race-to-the-bottom, exchanges function as co-beneficiaries actively degrading protective standards rather than neutral administrators, strengthening the tangled_rope/extraction reading; if efficient contracting, exchanges are more plausibly neutral administrators responding to legitimate demand.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exchange_competition_race_to_bottom, empirical, 'Whether exchange listing standard relaxation reflects competitive degradation or efficient market response.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_class_legitimacy__minority_extraction, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_tr_t0, dual_class_legitimacy__minority_extraction, theater_ratio, 0, 0.2).
narrative_ontology:measurement(dual_tr_t3, dual_class_legitimacy__minority_extraction, theater_ratio, 3, 0.26).
narrative_ontology:measurement(dual_tr_t6, dual_class_legitimacy__minority_extraction, theater_ratio, 6, 0.31).
narrative_ontology:measurement(dual_tr_t9, dual_class_legitimacy__minority_extraction, theater_ratio, 9, 0.37).
narrative_ontology:measurement(dual_tr_t12, dual_class_legitimacy__minority_extraction, theater_ratio, 12, 0.41).
narrative_ontology:measurement(dual_tr_t15, dual_class_legitimacy__minority_extraction, theater_ratio, 15, 0.45).

% Extraction over time
narrative_ontology:measurement(dual_be_t0, dual_class_legitimacy__minority_extraction, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(dual_be_t3, dual_class_legitimacy__minority_extraction, base_extractiveness, 3, 0.51).
narrative_ontology:measurement(dual_be_t6, dual_class_legitimacy__minority_extraction, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(dual_be_t9, dual_class_legitimacy__minority_extraction, base_extractiveness, 9, 0.63).
narrative_ontology:measurement(dual_be_t12, dual_class_legitimacy__minority_extraction, base_extractiveness, 12, 0.68).
narrative_ontology:measurement(dual_be_t15, dual_class_legitimacy__minority_extraction, base_extractiveness, 15, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(dual_su_t0, dual_class_legitimacy__minority_extraction, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(dual_su_t3, dual_class_legitimacy__minority_extraction, suppression_requirement, 3, 0.58).
narrative_ontology:measurement(dual_su_t6, dual_class_legitimacy__minority_extraction, suppression_requirement, 6, 0.61).
narrative_ontology:measurement(dual_su_t9, dual_class_legitimacy__minority_extraction, suppression_requirement, 9, 0.64).
narrative_ontology:measurement(dual_su_t12, dual_class_legitimacy__minority_extraction, suppression_requirement, 12, 0.66).
narrative_ontology:measurement(dual_su_t15, dual_class_legitimacy__minority_extraction, suppression_requirement, 15, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dual_class_legitimacy__minority_extraction, resource_allocation).
narrative_ontology:affects_constraint(dual_class_legitimacy__minority_extraction, dual_class_legitimacy__founder_stewardship).
narrative_ontology:affects_constraint(dual_class_legitimacy__minority_extraction, dual_class_legitimacy__disclosure_consent).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the dual_class_legitimacy kernel, each authored as a separate ε-invariant constraint per the decomposition principle: minority_extraction (this story, tangled_rope, ε=0.71) reads the arrangement as capital-and-risk-proportional entitlement violated by control concentration; founder_stewardship reads the same arrangement as legitimate long-horizon coordination (expected lower ε, rope or scaffold classification); disclosure_consent reads legitimacy as resting on informed consent rather than control parity (expected classification closer to rope, with disclosure adequacy as the operative variable rather than governance parity). All three share the same underlying dual-class share structure as their referent but assign it structurally different ε, beneficiary/victim sets, and classifications because they differ on what grounds legitimate governance entitlement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
