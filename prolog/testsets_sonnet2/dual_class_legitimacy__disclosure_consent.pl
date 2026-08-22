% ============================================================================
% CONSTRAINT STORY: dual_class_legitimacy__disclosure_consent
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dual_class_legitimacy__disclosure_consent, []).

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
 *   constraint_id: dual_class_legitimacy__disclosure_consent
 *   human_readable: Dual-Class Share Structure Legitimated by Disclosure-Based Informed Consent
 *   domain: corporate governance/securities law/organizational economics
 *
 * SUMMARY:
 *   A founder-controlled technology company completes an IPO with a
 *   dual-class share structure: Class A shares sold to the public carry one
 *   vote each, while Class B shares retained by founders carry ten votes
 *   each. The S-1 registration statement discloses the voting ratio, the
 *   absence of a sunset clause, and the risk factors associated with
 *   concentrated control in plain, required language. This story authors the
 *   reading under which that disclosure is the entire legitimating act:
 *   because the structure was named before any Class A share was purchased,
 *   and because Class A shares trade on a liquid public market where price
 *   can reflect the disclosed governance terms, legitimacy rests on informed
 *   consent, not on control parity. This is one of three readings of the
 *   dual_class_legitimacy kernel — see kernel_context.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dual_class_legitimacy__disclosure_consent, 0.28).
domain_priors:suppression_score(dual_class_legitimacy__disclosure_consent, 0.15).
domain_priors:theater_ratio(dual_class_legitimacy__disclosure_consent, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, extractiveness, 0.28).
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_class_legitimacy__disclosure_consent, rope).
narrative_ontology:human_readable(dual_class_legitimacy__disclosure_consent, "Dual-Class Share Structure Legitimated by Disclosure-Based Informed Consent").
narrative_ontology:topic_domain(dual_class_legitimacy__disclosure_consent, "corporate governance/securities law/organizational economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_class_legitimacy__disclosure_consent, '4efd361f-7dc0-40f3-aeb9-9baf72c29386').
narrative_ontology:cs_kernel_codification('4efd361f-7dc0-40f3-aeb9-9baf72c29386', formalized).
narrative_ontology:cs_authority_grounding('4efd361f-7dc0-40f3-aeb9-9baf72c29386', extraction).
narrative_ontology:cs_interpretation_layer_present('4efd361f-7dc0-40f3-aeb9-9baf72c29386').
narrative_ontology:cs_reading_relation('4efd361f-7dc0-40f3-aeb9-9baf72c29386', dual_class_legitimacy__founder_stewardship, coexists_with).
narrative_ontology:cs_reading_relation('4efd361f-7dc0-40f3-aeb9-9baf72c29386', dual_class_legitimacy__minority_extraction, influences).
narrative_ontology:cs_axiom('4efd361f-7dc0-40f3-aeb9-9baf72c29386', foundational, disclosure_satisfies_legitimation_regardless_of_duration).
narrative_ontology:cs_axiom_status(disclosure_satisfies_legitimation_regardless_of_duration, holdable).
narrative_ontology:cs_axiom_grounding('4efd361f-7dc0-40f3-aeb9-9baf72c29386', disclosure_satisfies_legitimation_regardless_of_duration, conventional).
narrative_ontology:cs_axiom('4efd361f-7dc0-40f3-aeb9-9baf72c29386', secondary, voluntary_pricing_of_control_rights_at_point_of_sale).
narrative_ontology:cs_axiom_status(voluntary_pricing_of_control_rights_at_point_of_sale, holdable).
narrative_ontology:cs_axiom_grounding('4efd361f-7dc0-40f3-aeb9-9baf72c29386', voluntary_pricing_of_control_rights_at_point_of_sale, empirically_contingent).
narrative_ontology:cs_reference_frame('4efd361f-7dc0-40f3-aeb9-9baf72c29386', disclosure_based_registration_regime).
narrative_ontology:cs_drift_state('4efd361f-7dc0-40f3-aeb9-9baf72c29386', post_2010s_sunset_provision_debate, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('4efd361f-7dc0-40f3-aeb9-9baf72c29386', '').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__disclosure_consent, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__disclosure_consent, founder_class_b_holders).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__disclosure_consent, class_a_public_investors).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__disclosure_consent, underwriting_banks).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dual_class_legitimacy__disclosure_consent, index_fund_managers).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__disclosure_consent, regulatory_disclosure_sufficiency_doctrine).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__disclosure_consent, efficient_pricing_of_control_rights).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain supervoting Class B shares carrying 10x or more the votes of Class A stock, preserving control of the board and major corporate decisions regardless of their declining economic stake. They authored the S-1 disclosure that named this structure explicitly before any public sale. Their exit options are effectively unconstrained; they can sell down economic exposure while control persists.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, founder_class_b_holders, beneficiary,
    institutional, generational, arbitrage, national).

% Choose, with the S-1 disclosure in hand at the time of purchase, to buy Class A shares knowing the voting disparity and its permanence absent a sunset clause. They set the agenda in the only sense that matters under disclosure logic: they are the ones whose consent the disclosure regime asks for, and they can decline to buy, sell at any time on a liquid exchange, or price the governance disparity into what they are willing to pay. Their exit is genuinely liquid — a public market with continuous quotes.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, class_a_public_investors, agenda_setter,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__disclosure_consent, class_a_public_investors, beneficiary).

% Structure and price the offering, draft the disclosure language describing the dual-class structure, and collect underwriting fees regardless of the governance structure's long-run performance. They face no continuing exposure to the structure once the offering closes.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, underwriting_banks, beneficiary,
    institutional, biographical, arbitrage, national).

% Administer the disclosure regime that requires dual-class structures, voting ratios, and sunset provisions (if any) to be stated plainly in the registration statement. They do not evaluate whether the structure is fair, only whether it is disclosed; their review confirms completeness of disclosure, not merits of governance design.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, securities_regulators, observer,
    institutional, generational, analytical, national).

% Hold Class A shares as passive index constituents on behalf of beneficiaries who never individually assessed the S-1 disclosure — inclusion in a benchmark index compels purchase regardless of governance terms. Their exit is nominally available (divest, engage in proxy contests) but constrained by fiduciary mandates to track the index and by the negligible practical effect of votes they cannot cast decisively.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, index_fund_managers, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__disclosure_consent, index_fund_managers, excluded).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dual_class_legitimacy__disclosure_consent, diffuse).
narrative_ontology:fixing_cost_class(dual_class_legitimacy__disclosure_consent, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables founders to raise public capital while preserving the control needed to execute a long-horizon strategy without the discipline of a hostile takeover or activist campaign disrupting execution; the disclosure regime coordinates by making the trade-off legible to all willing buyers before any money changes hands.
% TRANSFER_FUNCTION: No involuntary transfer occurs under this reading — buyers voluntarily forgo proportional voting power in exchange for economic participation, and that forbearance is priced into the offering price at the point of sale; any subsequent value effects are the realized outcome of a disclosed bargain, not an extraction.
% ABSENT_VOICES: Future minority shareholders who did not exist at the IPO and had no opportunity to price the structure themselves (successor generations of index fund beneficiaries, employees receiving Class A equity compensation) inherit the disparity without a fresh consent event; under this reading their absence is addressed by the disclosure having run once, at issuance, which the regime treats as binding on all subsequent holders of the same share class.
% DISAPPEARANCE_RATIONALE: If the disclosure-consent legitimation collapsed (e.g., courts or regulators ruled disclosure alone insufficient to legitimate permanent voting disparity), founders would face pressure toward sunset provisions or unification, and pricing of Class A shares would likely adjust upward to reflect regained governance rights — a real rearrangement. Whether this counts as the 'world rearranging' or merely 'a mispriced asset repricing' is itself contested between the disclosure-consent reading and the minority-extraction reading.
% FOUNDING_PROBLEM: Public capital markets needed a mechanism letting founders retain the control needed for long-horizon execution while accessing broad equity capital, and needed a governing legal doctrine (disclosure) that could legitimate whatever control terms parties freely negotiated without requiring merits review of the terms themselves.
% FOUNDING_PROBLEM_CORROBORATION: Securities regulators attest disclosure-based review remains the operative legal standard (no merits review in registration process); academic finance literature independent of both founders and public investors documents that dual-class share prices trade at discounts correlated with voting disparity, which the disclosure-consent reading interprets as the market successfully pricing the disclosed term — that pricing evidence is corroboration from outside both benefiting parties, though its interpretation (efficient pricing vs. persistent mispricing) remains contested in that same literature.
narrative_ontology:disappearance_verdict(dual_class_legitimacy__disclosure_consent, contested).
narrative_ontology:founding_problem_status(dual_class_legitimacy__disclosure_consent, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__disclosure_consent, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dual_class_legitimacy__disclosure_consent, 'none', 1).
narrative_ontology:epsilon_provenance(dual_class_legitimacy__disclosure_consent, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dual_class_legitimacy__disclosure_consent_tests).
:- end_tests(dual_class_legitimacy__disclosure_consent_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.28) because, under this reading's own lights, no involuntary transfer occurs: the governance disparity is a term of a voluntary bargain priced at the point of sale, not a mechanism siphoning value from an unconsenting party after the fact. It is not zero because secondary-market buyers (particularly through index inclusion) do not individually re-examine or re-price the disclosure, and because the underwriting banks that draft the disclosure language have an incentive to make risk factors legible rather than salient. Suppression is low (0.15) because no coercive apparatus prevents investors from declining to buy or from selling; accessibility_collapse is moderate (0.35), reflecting that once the IPO closes with a locked-in structure, no individual buyer can bargain for different terms — they can only accept or decline the terms as given, which is a real but disclosed constraint, not a suppressed one.
 *
 * PERSPECTIVAL GAP:
 *   From the founder/underwriter seat, the structure is pure coordination: capital was raised, control preserved, terms disclosed, bargain struck. From the index-fund seat, consent is nominal — no individual beneficiary read the S-1, no vote was meaningfully cast, and inclusion was compelled by benchmark methodology rather than governance assessment. This engine-computed divergence is exactly what the disclosure_consent reading claims does not undermine legitimacy (disclosure ran once, at issuance, and that is sufficient under this reading) and exactly what the minority_extraction sibling reading would treat as dispositive against it.
 *
 * DIRECTIONALITY LOGIC:
 *   Founders and underwriters are structural beneficiaries with high exit and low d. Class A public investors are declared BOTH agenda_setter (their consent is the legitimating act the regime asks for) and beneficiary (they receive whatever governance discount is priced in) because under this reading their purchase decision is itself the coordinating act, not a passive submission to extraction — this is why their directionality sits near symmetric rather than target. Index fund managers are the one seat carrying payer role: their fiduciary mandate compels holding regardless of individual assessment of the disclosed terms, so their consent is structurally attenuated even though the disclosure formally reached them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (letting founders raise capital while retaining execution control) remains live as long as founders continue to value long-horizon strategic latitude over full economic-control alignment; nothing here suggests the arrangement has outlived its function under this reading — the disclosure regime that legitimates it is doing exactly the job it was built to do (confirm terms are stated, not adjudicate their fairness), so no mandatrophy is declared.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    disclosure_sufficiency_vs_merits_review,
    'Does disclosure of a governance term, without any assessment of its fairness, constitute sufficient legitimation for permanent control disparity, or does legitimation require some baseline of proportionality regardless of disclosure?',
    'Courts or legislatures ruling on whether disclosure-only securities regimes can legitimate indefinite (non-sunsetted) voting disparities; comparative analysis of jurisdictions requiring sunset provisions versus those permitting perpetual dual-class structures.',
    'If disclosure is held sufficient regardless of duration, this reading is legally vindicated and the constraint properly classifies as coordination (rope). If courts or regulators move toward requiring sunset provisions as a condition of legitimacy, this reading''s premise weakens and the constraint drifts toward the minority_extraction sibling''s classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disclosure_sufficiency_vs_merits_review, conceptual, 'Whether disclosure alone, absent proportionality or sunset, can legitimate permanent control disparity.').

omega_variable(
    secondary_market_consent_attenuation,
    'Does the disclosure-based consent that legitimated the original IPO purchase extend meaningfully to secondary-market buyers, particularly passive index fund beneficiaries who never individually reviewed the S-1?',
    'Survey or behavioral evidence on secondary-market investor awareness of dual-class terms at time of purchase; analysis of whether index inclusion methodology treats voting structure as a screening criterion.',
    'If secondary consent is largely fictional for index-held shares, the disclosure_consent reading''s legitimating mechanism applies cleanly only to IPO-era direct purchasers, and the constraint''s effective extraction for the index-fund-manager seat would be higher than authored here — this would not change ε (an intrinsic property of the constraint) but would sharpen the case for treating index-held Class A shares as a structurally distinct sub-constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secondary_market_consent_attenuation, empirical, 'Whether disclosure-based consent survives the transition from IPO buyer to passive secondary holder.').

omega_variable(
    efficient_pricing_of_control_rights,
    'Does the observed trading discount on dual-class Class A shares relative to single-class comparables represent efficient pricing of the disclosed governance term, or a persistent market failure to price governance risk correctly?',
    'Long-horizon event studies comparing dual-class firm performance and valuation multiples against matched single-class peers, controlling for founder quality and industry.',
    'If the discount tracks governance risk efficiently, this reading''s claim that ''governance disparity is priced into valuation'' is empirically supported. If the discount is inconsistent or the disparity''s costs (e.g., entrenchment-driven value destruction) exceed the priced discount over time, the disclosure_consent reading''s central empirical premise weakens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(efficient_pricing_of_control_rights, empirical, 'Whether market pricing of dual-class discounts efficiently captures the disclosed control disparity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_class_legitimacy__disclosure_consent, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_tr_t0, dual_class_legitimacy__disclosure_consent, theater_ratio, 0, 0.08).
narrative_ontology:measurement(dual_tr_t4, dual_class_legitimacy__disclosure_consent, theater_ratio, 4, 0.09).
narrative_ontology:measurement(dual_tr_t8, dual_class_legitimacy__disclosure_consent, theater_ratio, 8, 0.1).
narrative_ontology:measurement(dual_tr_t12, dual_class_legitimacy__disclosure_consent, theater_ratio, 12, 0.1).
narrative_ontology:measurement(dual_tr_t16, dual_class_legitimacy__disclosure_consent, theater_ratio, 16, 0.11).
narrative_ontology:measurement(dual_tr_t20, dual_class_legitimacy__disclosure_consent, theater_ratio, 20, 0.12).

% Extraction over time
narrative_ontology:measurement(dual_be_t0, dual_class_legitimacy__disclosure_consent, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(dual_be_t4, dual_class_legitimacy__disclosure_consent, base_extractiveness, 4, 0.24).
narrative_ontology:measurement(dual_be_t8, dual_class_legitimacy__disclosure_consent, base_extractiveness, 8, 0.25).
narrative_ontology:measurement(dual_be_t12, dual_class_legitimacy__disclosure_consent, base_extractiveness, 12, 0.26).
narrative_ontology:measurement(dual_be_t16, dual_class_legitimacy__disclosure_consent, base_extractiveness, 16, 0.27).
narrative_ontology:measurement(dual_be_t20, dual_class_legitimacy__disclosure_consent, base_extractiveness, 20, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(dual_class_legitimacy__disclosure_consent, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dual_class_legitimacy__disclosure_consent, resource_allocation).
narrative_ontology:boltzmann_floor_override(dual_class_legitimacy__disclosure_consent, 0.15).
narrative_ontology:affects_constraint(dual_class_legitimacy__disclosure_consent, dual_class_legitimacy__founder_stewardship).
narrative_ontology:affects_constraint(dual_class_legitimacy__disclosure_consent, dual_class_legitimacy__minority_extraction).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the dual_class_legitimacy kernel. disclosure_consent (this file) authors ε=0.28, treating the disparity as a disclosed contractual term voluntarily priced by buyers. founder_stewardship authors a distinct ε grounded in mission-execution benefit to all shareholders. minority_extraction authors a substantially higher ε grounded in proportionality-to-risk as the legitimating baseline, with the same founders and Class A investors recast as agenda_setter/beneficiary and victim respectively. All three share the same underlying share-class structure but instantiate different constraints per the ε-invariance principle — each has its own beneficiary/victim structure and its own stable ε, linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
