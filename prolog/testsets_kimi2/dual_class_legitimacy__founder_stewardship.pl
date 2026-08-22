% ============================================================================
% CONSTRAINT STORY: dual_class_legitimacy__founder_stewardship
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:affects_constraint/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Dual-Class Founder Stewardship Legitimacy
 *   domain: corporate governance/securities law/organizational economics
 *
 * SUMMARY:
 *   This constraint story instantiates the founder_stewardship reading of the
 *   dual_class_legitimacy kernel. The kernel is the dual-class corporate
 *   control structure in publicly traded companies. This reading frames
 *   concentrated founder control not as extraction from minority shareholders
 *   but as legitimate coordination: the founder acts as a fiduciary steward
 *   whose super-voting shares insulate long-horizon mission execution from
 *   short-term market pressures. Class A shareholders are understood to
 *   benefit indirectly through sustained innovation and value creation. The
 *   metrics are authored descriptively and independently of the claim; the
 *   engine computes per-seat classifications from the structural data.
 *
 * KEY AGENTS:
 *   - founder_stewards: Agenda-setter (powerful/identity_locked/global) â holds super-voting control, defines mission, cannot easily exit without abandoning identity.
 *   - class_a_shareholders: Beneficiary (moderate/mobile/global) â holds inferior voting rights, relies on founder stewardship, can exit via markets.
 *   - short_term_activists: Excluded (powerful/mobile/global) â would challenge strategy but are structurally barred by charter.
 *   - proxy_advisors: Observer (institutional/analytical/global) â analyze governance, recommend votes, document control asymmetry.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dual_class_legitimacy__founder_stewardship, 0.42).
domain_priors:suppression_score(dual_class_legitimacy__founder_stewardship, 0.38).
domain_priors:theater_ratio(dual_class_legitimacy__founder_stewardship, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, extractiveness, 0.42).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_class_legitimacy__founder_stewardship, rope).
narrative_ontology:human_readable(dual_class_legitimacy__founder_stewardship, "Dual-Class Founder Stewardship Legitimacy").
narrative_ontology:topic_domain(dual_class_legitimacy__founder_stewardship, "corporate governance/securities law/organizational economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_class_legitimacy__founder_stewardship, 'b6f030b0-3a71-4912-9738-481cf33b1b3d').
narrative_ontology:cs_kernel_codification('b6f030b0-3a71-4912-9738-481cf33b1b3d', formalized).
narrative_ontology:cs_authority_grounding('b6f030b0-3a71-4912-9738-481cf33b1b3d', lineage).
narrative_ontology:cs_interpretation_layer_present('b6f030b0-3a71-4912-9738-481cf33b1b3d').
narrative_ontology:cs_reading_relation('b6f030b0-3a71-4912-9738-481cf33b1b3d', dual_class_legitimacy__minority_extraction, coexists_with).
narrative_ontology:cs_reading_relation('b6f030b0-3a71-4912-9738-481cf33b1b3d', dual_class_legitimacy__disclosure_consent, coexists_with).
narrative_ontology:cs_axiom('b6f030b0-3a71-4912-9738-481cf33b1b3d', foundational, founder_fiduciary_supremacy).
narrative_ontology:cs_axiom_status(founder_fiduciary_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('b6f030b0-3a71-4912-9738-481cf33b1b3d', founder_fiduciary_supremacy, deontological).
narrative_ontology:cs_reference_frame('b6f030b0-3a71-4912-9738-481cf33b1b3d', founder_fiduciary_supremacy).
narrative_ontology:cs_drift_state('b6f030b0-3a71-4912-9738-481cf33b1b3d', contemporary_governance_reform_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('b6f030b0-3a71-4912-9738-481cf33b1b3d', '').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__founder_stewardship, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, founder_stewards).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, class_a_shareholders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds super-voting shares that confer board control and strategic decision-making authority. Defines the company's long-term mission and capital allocation. Exit would require surrendering control and likely departing the organization, which is bound to personal and professional identity.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, founder_stewards, agenda_setter,
    powerful, generational, identity_locked, global).

% Hold publicly traded common stock with limited or no voting rights. Invested on the premise that founder stewardship will generate long-term value exceeding that of short-term-optimized alternatives. Can liquidate holdings in public markets but cannot influence governance directly.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, class_a_shareholders, beneficiary,
    moderate, biographical, mobile, global).

% Control pools of capital that seek immediate returns through governance pressure, divestitures, or strategy changes. Are structurally prevented from acquiring board seats or influencing mergers by the super-voting control block.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, short_term_activists, excluded,
    powerful, immediate, mobile, global).

% Analyze corporate governance practices and issue voting recommendations to institutional investors. Document control asymmetries and benchmark them against market standards without direct power to alter the charter.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, proxy_advisors, observer,
    institutional, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Insulates strategic decision-making from short-term market pressures and activist interference, enabling sustained investment in projects with delayed returns and mission coherence.
% TRANSFER_FUNCTION: Transfers governance control from dispersed capital providers to a concentrated founder, in exchange for expected long-term value creation and mission stability.
% ABSENT_VOICES: Short-term activists and governance reformers who would advocate for one-share-one-vote structures or immediate returns are structurally excluded from the governance conversation by the charter.
% DISAPPEARANCE_RATIONALE: If the dual-class control structure disappeared overnight, founder authority would be subject to standard majority voting, activist investors could gain board influence, strategic horizons would likely compress to quarterly cycles, and the mission-centric governance model would rearrange into conventional corporate democracy.
% FOUNDING_PROBLEM: Public market short-termism and activist pressure drive founders to prioritize quarterly earnings over mission-critical long-term investments, causing underinvestment in innovation and long-term value destruction.
% FOUNDING_PROBLEM_CORROBORATION: Founder-stewards and mission-aligned directors attest to the problem from inside the structure. Independent academic research in corporate finance and governance (outside the benefiting parties) documents short-term pressure effects, though the specific magnitude for any given firm remains contested.
narrative_ontology:disappearance_verdict(dual_class_legitimacy__founder_stewardship, world_rearranges).
narrative_ontology:founding_problem_status(dual_class_legitimacy__founder_stewardship, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__founder_stewardship, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dual_class_legitimacy__founder_stewardship, 'none', 1).
narrative_ontology:epsilon_provenance(dual_class_legitimacy__founder_stewardship, 0.42, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is authored at moderate-low levels (0.42 at interval end) because even this reading acknowledges a governance cost to Class A shareholders, though it frames it as justified by coordination benefits. Suppression is moderate (0.38) because the control structure is legally entrenched and difficult to unwind without board action. Theater rises over time (0.32) as the stewardship narrative becomes more salient relative to measurable mission outcomes. Accessibility collapse is moderate-high (0.60) because recapitalization to a single-class structure is rare once dual-class is adopted. Resistance is moderate (0.50) due to ongoing governance reform pressure. The measurement series share a single time grid so every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   From the founder-steward seat, the arrangement is voluntary coordination that preserves mission integrity; from the Class A shareholder seat, it is a trade of governance rights for purported long-term benefits. The activist seat experiences the structure as exclusion. The engine computes these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Founder-stewards are declared beneficiaries with identity_locked exit, giving them low directionality (near the beneficiary end). Class A shareholders are also declared beneficiaries with mobile exit, giving them moderate directionality. Short-term activists are excluded and would experience high directionality if they were inside the constraint. No directionality overrides are needed because the structural derivation captures the relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â short-term market pressure undermining long-term investment â remains live in corporate governance debate. The constraint is actively defended by founders and contested by reformers; it has not outlived its function. The classification prevents mislabeling by requiring that a rope claim be matched by structural data; if the metrics diverge from the claim, the engine flags the gap.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fiduciary_stewardship_empirical,
    'Does the founder''s concentrated control produce superior long-term returns for Class A shareholders relative to comparable single-class firms, or does it enable private benefit extraction and entrenchment?',
    'Long-term event study comparing total shareholder returns of dual-class firms with founder control against single-class peers, controlling for sector and growth stage; paired with analysis of related-party transactions and compensation.',
    'If Class A returns are systematically lower or private benefits are detected, the coordination narrative weakens and the constraint shifts toward extraction in the classification; if returns are superior, the rope classification is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiduciary_stewardship_empirical, empirical, 'Whether founder stewardship produces measurable shareholder value or masks extraction').

omega_variable(
    control_asymmetry_cost,
    'Is the governance cost borne by Class A shareholders (loss of voting rights, inability to remove underperforming management) offset by mission execution benefits, or does the cost exceed the benefit?',
    'Comparative governance analysis measuring entrenchment discounts, premium-to-book ratios, and contingent governance events (e.g., founder failure, strategic missteps) where control asymmetry prevented corrective action.',
    'If the cost exceeds benefit, the reading''s classification of Class A shareholders as beneficiaries is undermined and they reclassify as payers; if benefit exceeds cost, the rope framing gains structural support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(control_asymmetry_cost, empirical, 'Cost-benefit balance of control asymmetry for non-controlling shareholders').

omega_variable(
    reading_foreclosure_boundary,
    'Does the founder-stewardship reading''s claim of fiduciary duty logically foreclose the minority-extraction reading''s claim of proportional governance rights, or can both readings coexist as live positions?',
    'Jurisprudential analysis of whether a single corporate charter can simultaneously uphold fiduciary stewardship and proportional governance; empirical observation of whether parties hold both frames simultaneously or treat them as mutually exclusive.',
    'If foreclosed, the kernel readings are in zero-sum contest; if coexistent, the kernel admits plural legitimate framings and classification depends on empirical seat data rather than logical elimination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_boundary, conceptual, 'Logical relationship between stewardship and proportional governance readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_class_legitimacy__founder_stewardship, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_class_fs_tr_t0, dual_class_legitimacy__founder_stewardship, theater_ratio, 0, 0.12).
narrative_ontology:measurement(dual_class_fs_tr_t4, dual_class_legitimacy__founder_stewardship, theater_ratio, 4, 0.16).
narrative_ontology:measurement(dual_class_fs_tr_t8, dual_class_legitimacy__founder_stewardship, theater_ratio, 8, 0.2).
narrative_ontology:measurement(dual_class_fs_tr_t12, dual_class_legitimacy__founder_stewardship, theater_ratio, 12, 0.24).
narrative_ontology:measurement(dual_class_fs_tr_t16, dual_class_legitimacy__founder_stewardship, theater_ratio, 16, 0.28).
narrative_ontology:measurement(dual_class_fs_tr_t20, dual_class_legitimacy__founder_stewardship, theater_ratio, 20, 0.32).

% Extraction over time
narrative_ontology:measurement(dual_class_fs_be_t0, dual_class_legitimacy__founder_stewardship, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(dual_class_fs_be_t4, dual_class_legitimacy__founder_stewardship, base_extractiveness, 4, 0.24).
narrative_ontology:measurement(dual_class_fs_be_t8, dual_class_legitimacy__founder_stewardship, base_extractiveness, 8, 0.28).
narrative_ontology:measurement(dual_class_fs_be_t12, dual_class_legitimacy__founder_stewardship, base_extractiveness, 12, 0.32).
narrative_ontology:measurement(dual_class_fs_be_t16, dual_class_legitimacy__founder_stewardship, base_extractiveness, 16, 0.38).
narrative_ontology:measurement(dual_class_fs_be_t20, dual_class_legitimacy__founder_stewardship, base_extractiveness, 20, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(dual_class_fs_su_t0, dual_class_legitimacy__founder_stewardship, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(dual_class_fs_su_t4, dual_class_legitimacy__founder_stewardship, suppression_requirement, 4, 0.22).
narrative_ontology:measurement(dual_class_fs_su_t8, dual_class_legitimacy__founder_stewardship, suppression_requirement, 8, 0.26).
narrative_ontology:measurement(dual_class_fs_su_t12, dual_class_legitimacy__founder_stewardship, suppression_requirement, 12, 0.3).
narrative_ontology:measurement(dual_class_fs_su_t16, dual_class_legitimacy__founder_stewardship, suppression_requirement, 16, 0.34).
narrative_ontology:measurement(dual_class_fs_su_t20, dual_class_legitimacy__founder_stewardship, suppression_requirement, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(dual_class_legitimacy__founder_stewardship, dual_class_legitimacy__minority_extraction).
narrative_ontology:affects_constraint(dual_class_legitimacy__founder_stewardship, dual_class_legitimacy__disclosure_consent).

% DUAL FORMULATION NOTE:
% This constraint is the founder_stewardship reading of the dual_class_legitimacy kernel. The kernel decomposes into three structurally distinct claims: founder_stewardship (coordination via fiduciary mission protection), minority_extraction (asymmetric extraction from non-controlling shareholders), and disclosure_consent (procedural legitimacy through informed consent rather than control parity). Each reading carries a distinct epsilon and beneficiary structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
