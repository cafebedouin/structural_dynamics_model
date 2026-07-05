% ============================================================================
% CONSTRAINT STORY: dual_class_legitimacy__founder_stewardship
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dual_class_legitimacy__founder_stewardship, []).

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
 *   constraint_id: dual_class_legitimacy__founder_stewardship
 *   human_readable: Dual-Class Share Structure as Founder Stewardship Mechanism
 *   domain: corporate_governance/securities_law/organizational_economics
 *
 * SUMMARY:
 *   This story instantiates the founder-stewardship reading of the dual-class
 *   legitimacy kernel: a technology company's IPO retains a dual-class
 *   structure (Class B supervoting shares held by the founder-CEO, Class A
 *   ordinary shares sold to the public) on the theory that founder control is
 *   a coordination mechanism protecting long-horizon mission execution from
 *   short-term market pressure, and that Class A holders benefit indirectly
 *   through the superior returns that protected execution is claimed to
 *   generate. This is one of three sibling constraints sharing the same
 *   underlying dual-class kernel: the minority_extraction reading treats the
 *   same structural facts as capital-risk-proportional governance being
 *   illegitimately withheld, and the disclosure_consent reading locates
 *   legitimacy entirely in whether Class A holders had adequate notice at
 *   purchase, independent of control parity. Each reading is authored as its
 *   own ε-invariant constraint per the ε-invariance principle; they are not
 *   measurement variants of one constraint.
 *
 * KEY AGENTS:
 *   - founder_ceo: Primary agenda-setter and structural beneficiary (institutional/arbitrage) — controls the mechanism and collects its upside
 *   - class_a_shareholders: Indirect beneficiary and residual capital-risk bearer (moderate/mobile) — receives protected mission execution, bears price risk without proportional voice
 *   - long_horizon_employees: Secondary beneficiary (moderate/mobile) — equity-aligned with mission continuity
 *   - index_fund_administrators: Institutional observer with limited practical exit (institutional/analytical) — votes/advises within the structure rather than against it
 *   - public_market_short_term_traders: Excluded voice (organized/arbitrage) — structurally discounted by design under this reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dual_class_legitimacy__founder_stewardship, 0.38).
domain_priors:suppression_score(dual_class_legitimacy__founder_stewardship, 0.42).
domain_priors:theater_ratio(dual_class_legitimacy__founder_stewardship, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, extractiveness, 0.38).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_class_legitimacy__founder_stewardship, rope).
narrative_ontology:human_readable(dual_class_legitimacy__founder_stewardship, "Dual-Class Share Structure as Founder Stewardship Mechanism").
narrative_ontology:topic_domain(dual_class_legitimacy__founder_stewardship, "corporate_governance/securities_law/organizational_economics").

domain_priors:requires_active_enforcement(dual_class_legitimacy__founder_stewardship).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_class_legitimacy__founder_stewardship, '7bbcb254-9b2d-46de-8c34-cc44c2ac7982').
narrative_ontology:cs_kernel_codification('7bbcb254-9b2d-46de-8c34-cc44c2ac7982', formalized).
narrative_ontology:cs_authority_grounding('7bbcb254-9b2d-46de-8c34-cc44c2ac7982', lineage).
narrative_ontology:cs_interpretation_layer_present('7bbcb254-9b2d-46de-8c34-cc44c2ac7982').
narrative_ontology:cs_reading_relation('7bbcb254-9b2d-46de-8c34-cc44c2ac7982', dual_class_legitimacy__minority_extraction, coexists_with).
narrative_ontology:cs_reading_relation('7bbcb254-9b2d-46de-8c34-cc44c2ac7982', dual_class_legitimacy__disclosure_consent, influences).
narrative_ontology:cs_axiom('7bbcb254-9b2d-46de-8c34-cc44c2ac7982', foundational, founder_judgment_as_fiduciary_proxy_for_all_capital).
narrative_ontology:cs_axiom_status(founder_judgment_as_fiduciary_proxy_for_all_capital, holdable).
narrative_ontology:cs_axiom_grounding('7bbcb254-9b2d-46de-8c34-cc44c2ac7982', founder_judgment_as_fiduciary_proxy_for_all_capital, instrumental).
narrative_ontology:cs_axiom('7bbcb254-9b2d-46de-8c34-cc44c2ac7982', secondary, voting_control_properly_decoupled_from_capital_contribution_when_mission_aligned).
narrative_ontology:cs_axiom_status(voting_control_properly_decoupled_from_capital_contribution_when_mission_aligned, holdable).
narrative_ontology:cs_axiom_grounding('7bbcb254-9b2d-46de-8c34-cc44c2ac7982', voting_control_properly_decoupled_from_capital_contribution_when_mission_aligned, conventional).
narrative_ontology:cs_reference_frame('7bbcb254-9b2d-46de-8c34-cc44c2ac7982', founder_fiduciary_stewardship_at_ipo).
narrative_ontology:cs_drift_state('7bbcb254-9b2d-46de-8c34-cc44c2ac7982', post_maturity_activist_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7bbcb254-9b2d-46de-8c34-cc44c2ac7982', '').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__founder_stewardship, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, founder_ceo).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, class_a_shareholders).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, long_horizon_employees).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dual_class_legitimacy__founder_stewardship, class_a_shareholders).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__founder_stewardship, long_horizon_value_creation_thesis).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__founder_stewardship, mission_protection_from_market_myopia).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds supervoting Class B shares (typically 10:1 or 20:1) and controls board composition, strategic direction, and capital allocation decisions largely insulated from quarterly market pressure. Frames this control as a fiduciary trust exercised on behalf of all shareholders to pursue a long-horizon mission that public markets would otherwise punish for short-term underperformance. Personally wealthy from equity appreciation regardless of structure; the dual-class arrangement is what lets that wealth accrue without dilution of control through ordinary market discipline.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, founder_ceo, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__founder_stewardship, founder_ceo, beneficiary).

% Hold ordinary shares with full economic rights but attenuated or no voting power relative to their capital contribution. Under this reading, they benefit indirectly through superior long-run returns generated by protected strategic execution: freedom from activist pressure to cut R&D, freedom to make dilutive-but-mission-aligned acquisitions, freedom to run losses during buildout phases. Their exit option is real and liquid — they can sell at any time at a market-set price that already reflects the governance structure, meaning they entered (or remain) with the structure priced in.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, class_a_shareholders, beneficiary,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__founder_stewardship, class_a_shareholders, payer).

% Hold equity compensation and career capital tied to the firm's long-run trajectory. Benefit from strategic continuity and insulation from activist-driven layoffs, spin-offs, or short-term cost cutting that could disrupt multi-year product or research roadmaps. Can leave for competing employers if the mission execution stalls, but their vested equity value is tied to the same control structure they might otherwise criticize.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, long_horizon_employees, beneficiary,
    moderate, biographical, mobile, national).

% Hold Class A shares as passive index constituents on behalf of millions of end beneficiaries and cannot meaningfully exit without tracking-error consequences. Publish governance guidelines favoring one-share-one-vote but continue to hold and vote (or abstain) within the existing structure because exclusion from indices would harm their own fiduciary mandate to track the market. Their engagement is more advisory than coercive under this reading.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, index_fund_administrators, observer,
    institutional, generational, analytical, national).

% Would prefer full voting parity to pressure management toward quarterly earnings optimization, buybacks, or breakup value realization. Under the founder-stewardship reading, their preferences are treated as precisely the short-termism the structure is designed to filter out — their voice is structurally discounted, and they are not part of the legitimating conversation because their time horizon is treated as adverse to the firm's stated purpose.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, public_market_short_term_traders, excluded,
    organized, immediate, arbitrage, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates voting control in the founder to enable strategic decisions (R&D investment, multi-year product bets, resistance to hostile takeover or activist breakup) that require insulation from period-to-period market sentiment, on the theory that founder judgment aligned with a stated long-run mission produces better outcomes for all capital providers than diffuse shareholder democracy would.
% TRANSFER_FUNCTION: Moves voting control from capital-proportional allocation (one-share-one-vote) to founder-proportional allocation (supervoting shares), while economic rights (dividends, liquidation preference, share price appreciation) remain largely proportional to capital across share classes.
% ABSENT_VOICES: Short-term-oriented traders and activist investors who would otherwise push for capital discipline, buybacks, or strategic breakup are structurally excluded from influencing the outcome regardless of the size of their stake — under this reading their exclusion is treated as a feature (filtering myopic pressure) rather than a defect.
% DISAPPEARANCE_RATIONALE: If the dual-class structure were dissolved into one-share-one-vote overnight, the founder-stewardship reading holds that mission-critical long-horizon investments would be curtailed under activist pressure and firm value would fall for all shareholders; the sibling minority-extraction reading holds that governance would simply become proportional to capital risk and nothing of value would be lost. The world does rearrange in either case, but which rearrangement counts as loss versus correction is exactly the contested claim between readings.
% FOUNDING_PROBLEM: At IPO, the firm needed access to public capital markets without surrendering the strategic control the founding team believed was necessary to execute a distinctive, multi-year technology or product vision that a diffuse shareholder base would not have the specialized knowledge or patience to evaluate correctly.
% FOUNDING_PROBLEM_CORROBORATION: The founder and early venture backers attest the problem remains live — competitive and activist pressure toward short-termism has, if anything, intensified. Independent corporate governance scholars and some institutional investors (outside the founder's own circle) contest this, arguing the empirical record on dual-class long-run performance is mixed at best and that the structure persists well past any plausible 'building the moat' phase into indefinite entrenchment; this outside corroboration is genuinely divided, not unanimous in either direction.
narrative_ontology:disappearance_verdict(dual_class_legitimacy__founder_stewardship, contested).
narrative_ontology:founding_problem_status(dual_class_legitimacy__founder_stewardship, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__founder_stewardship, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dual_class_legitimacy__founder_stewardship, 'none', 1).
narrative_ontology:epsilon_provenance(dual_class_legitimacy__founder_stewardship, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dual_class_legitimacy__founder_stewardship_tests).
:- end_tests(dual_class_legitimacy__founder_stewardship_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate-low (0.38 at interval end) and rising gently, reflecting that under the founder-stewardship reading the structure's primary function is coordination (protecting mission execution) with some genuine but non-dominant extraction risk (control entrenchment beyond the point the coordination story justifies). Suppression (0.42) reflects that dissenting Class A votes are structurally discounted by design, but this is disclosed and priced at purchase rather than concealed. Theater ratio is low-to-moderate (0.22) and rising slowly — most stewardship activity is real strategic decision-making, though a growing minority of 'protecting the mission' framing is deployed defensively against legitimate governance challenges as the company matures past its founding buildout phase. Accessibility collapse is moderate (0.5): Class A holders can exit via sale at any time (this is not a trap), but cannot exit the voting asymmetry itself while holding the security — the alternative (not buying, or buying one-share-one-vote competitors) exists but is not equivalent. Resistance is moderate (0.35): activist campaigns, governance-ratings downgrades, and index-provider voting guideline changes represent real friction the structure must withstand, which is exactly what the coordination story predicts it should withstand if the story is true.
 *
 * DIRECTIONALITY LOGIC:
 *   Under this reading, the founder is coded as beneficiary/agenda-setter with the lowest directionality (control and economic upside both flow to this seat) but is explicitly NOT coded with victims — the reading's core claim is that no shareholder is a structural target, only that voting weight is decoupled from capital weight in service of a claimed common benefit. Class A shareholders and employees are coded as beneficiaries (indirect, via mission success) with secondary payer characteristics (foregone proportional voice, priced-in liquidity risk) rather than as victims — the reading holds this is a cost willingly borne in exchange for expected superior returns, disclosed at purchase. This is the load-bearing structural difference from the minority_extraction sibling reading, which would code the same Class A holders as victims of a coercive transfer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (insulating a specialized long-horizon strategy from market myopia during an early buildout phase) plausibly was live at IPO. Whether it remains live 15-20 years later, after the mission is largely executed and the moat largely built, is exactly the contested question the founding_problem_status captures. This reading resists treating dual-class control as automatically mandatrophic, but also does not certify it as permanently justified — the omega variables below carry the unresolved empirical question of whether the coordination function has degraded into pure entrenchment while the stewardship narrative persists unchanged.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stewardship_vs_entrenchment_temporal_boundary,
    'At what point, if any, does founder control transition from serving the coordination function it was created for (protecting a nascent long-horizon strategy) to pure entrenchment (protecting incumbent control after the strategic buildout phase is complete)?',
    'Longitudinal comparison of strategic decision quality and shareholder value creation in years 1-5 post-IPO (buildout phase, coordination story strongest) versus years 15+ (maturity phase, coordination story weakest), controlled against comparable single-class firms in the same sector.',
    'If the coordination benefit is concentrated early and decays, sunset provisions (time-based or event-based conversion to one-share-one-vote) would be structurally justified even within this reading''s own logic — a founder-stewardship claim without a sunset mechanism is harder to distinguish from the minority_extraction reading''s account of the same facts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stewardship_vs_entrenchment_temporal_boundary, empirical, 'Whether the coordination function this reading claims has a natural expiration the structure does not honor.').

omega_variable(
    kernel_framing_selection,
    'Is founder-stewardship the correct primary framing for this fact pattern, or does the disclosure_consent framing (legitimacy via informed consent, control parity irrelevant) better capture what actually legitimates the arrangement in practice?',
    'Examine whether Class A holders'' actual purchase decisions and pricing behavior track a stewardship-quality assessment (implying stewardship is the operative legitimating frame) or simply price in the disclosed voting structure as a known risk factor without regard to founder quality (implying disclosure_consent is doing the real legitimating work).',
    'If disclosure_consent is the framing actually operative in market pricing, the stewardship narrative may be largely rhetorical cover generated by the founder seat rather than a claim market participants rely on — this would not change this story''s own ε but would affect how much independent weight the stewardship reading should carry in aggregate kernel analysis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_selection, conceptual, 'Whether founder-stewardship or disclosure-consent is the framing doing the actual legitimating work in market behavior, independent of which framing founders assert.').

omega_variable(
    vindication_vs_beneficiary_boundary_for_mission_success,
    'Is ''mission success'' properly a vindicated proposition (the mission being achieved validates the arrangement) or does treating it this way obscure that specific parties — not an abstract mission — are the actual economic beneficiaries?',
    'Trace whether claimed ''mission success'' outcomes translate into measurable, broadly distributed shareholder returns versus concentrated founder/insider wealth events (e.g., disproportionate founder share sales, related-party transactions) that a ''mission succeeded'' narrative would obscure.',
    'If mission success systematically correlates with concentrated rather than distributed gains, the vindicated_propositions framing in this story is doing rhetorical work that should be re-examined against the actual beneficiary list.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vindication_vs_beneficiary_boundary_for_mission_success, conceptual, 'Whether ''mission success'' is a genuine vindicated proposition or a proxy label for founder-concentrated gains.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_class_legitimacy__founder_stewardship, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_tr_t0, dual_class_legitimacy__founder_stewardship, theater_ratio, 0, 0.1).
narrative_ontology:measurement(dual_tr_t4, dual_class_legitimacy__founder_stewardship, theater_ratio, 4, 0.13).
narrative_ontology:measurement(dual_tr_t8, dual_class_legitimacy__founder_stewardship, theater_ratio, 8, 0.16).
narrative_ontology:measurement(dual_tr_t12, dual_class_legitimacy__founder_stewardship, theater_ratio, 12, 0.18).
narrative_ontology:measurement(dual_tr_t16, dual_class_legitimacy__founder_stewardship, theater_ratio, 16, 0.2).
narrative_ontology:measurement(dual_tr_t20, dual_class_legitimacy__founder_stewardship, theater_ratio, 20, 0.22).

% Extraction over time
narrative_ontology:measurement(dual_be_t0, dual_class_legitimacy__founder_stewardship, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(dual_be_t4, dual_class_legitimacy__founder_stewardship, base_extractiveness, 4, 0.26).
narrative_ontology:measurement(dual_be_t8, dual_class_legitimacy__founder_stewardship, base_extractiveness, 8, 0.3).
narrative_ontology:measurement(dual_be_t12, dual_class_legitimacy__founder_stewardship, base_extractiveness, 12, 0.33).
narrative_ontology:measurement(dual_be_t16, dual_class_legitimacy__founder_stewardship, base_extractiveness, 16, 0.36).
narrative_ontology:measurement(dual_be_t20, dual_class_legitimacy__founder_stewardship, base_extractiveness, 20, 0.38).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(dual_class_legitimacy__founder_stewardship, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dual_class_legitimacy__founder_stewardship, resource_allocation).
narrative_ontology:boltzmann_floor_override(dual_class_legitimacy__founder_stewardship, 0.12).
narrative_ontology:affects_constraint(dual_class_legitimacy__founder_stewardship, dual_class_legitimacy__minority_extraction).
narrative_ontology:affects_constraint(dual_class_legitimacy__founder_stewardship, dual_class_legitimacy__disclosure_consent).

% DUAL FORMULATION NOTE:
% Three sibling constraints share the dual_class_legitimacy kernel and the identical underlying fact pattern (supervoting founder shares disclosed at IPO): founder_stewardship (this story, claimed rope — coordination-dominant, moderate rising extraction), minority_extraction (claimed tangled_rope or snare — same facts read as capital-proportional governance illegitimately withheld from at-risk Class A capital), and disclosure_consent (claimed rope or scaffold — legitimacy grounded entirely in adequacy of disclosure at time of purchase, bracketing the control-parity question). Each carries its own ε, beneficiary/victim structure, and claimed_type per the ε-invariance principle; they are linked here rather than merged because measuring 'is dual-class governance legitimate' one way (via mission outcomes) versus another way (via capital-risk proportionality) versus a third way (via informed consent) yields genuinely different ε values, not different views of one ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
