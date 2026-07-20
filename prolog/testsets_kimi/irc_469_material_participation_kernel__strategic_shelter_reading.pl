% ============================================================================
% CONSTRAINT STORY: irc_469_material_participation_kernel__strategic_shelter_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_irc_469_material_participation_kernel__strategic_shelter_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: irc_469_material_participation_kernel__strategic_shelter_reading
 *   human_readable: IRC Â§469 Material Participation â Strategic Shelter Reading
 *   domain: tax/regulatory/real_estate
 *
 * SUMMARY:
 *   This constraint story instantiates the strategic_shelter_reading of the
 *   IRC Â§469 material participation kernel. Under this reading, the
 *   statutory and regulatory material participation tests function as a
 *   permissive procedural safe harbor rather than a substantive economic
 *   gatekeeper. High-net-worth real estate investors, aided by tax advisors,
 *   systematically satisfy the 500-hour, 100-hour, or substantially-all tests
 *   through aggressive hour-counting, layered property-management structures,
 *   and strategic grouping elections under Reg. Â§1.469-9 and -4. The result
 *   is a broad qualifying population with low compliance friction and
 *   systematic passive-loss deduction that preserves wealth for those with
 *   access to sophisticated advice. The constraint coordinates the tax
 *   treatment of passive activities but extracts from the general tax base by
 *   making the shelter's accessibility contingent on advisory resources. This
 *   is a contested kernel: the sibling strict_gatekeeper_reading holds that
 *   material participation requires verifiable, substantial personal labor
 *   with a high documentation bar. The two readings coexist in tax practice
 *   and litigation; this reading influences the strict reading by creating
 *   the enforcement pressure that provokes it.
 *
 * KEY AGENTS:
 *   - high_net_worth_real_estate_investors: Primary beneficiary (powerful/mobile) â captures tax savings through mechanical compliance
 *   - tax_advisors_and_preparers: Secondary beneficiary (organized/mobile) â collects fees from shelter architecture
 *   - irs_and_treasury: Agenda-setter (institutional/constrained) â administers the rule but politically blocked from tightening
 *   - general_tax_base: Primary target (powerless/trapped) â bears diffuse revenue cost
 *   - non_sheltering_small_investors: Secondary target (moderate/constrained) â pays higher relative rates without shelter access
 *   - tax_courts_and_litigators: Analytical observer (institutional/analytical) â adjudicates boundary disputes
 *   - progressive_tax_reform_advocates: Excluded voice (moderate/constrained) â structurally absent from agenda-setting
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(irc_469_material_participation_kernel__strategic_shelter_reading, 0.72).
domain_priors:suppression_score(irc_469_material_participation_kernel__strategic_shelter_reading, 0.65).
domain_priors:theater_ratio(irc_469_material_participation_kernel__strategic_shelter_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(irc_469_material_participation_kernel__strategic_shelter_reading, tangled_rope).
narrative_ontology:human_readable(irc_469_material_participation_kernel__strategic_shelter_reading, "IRC Â§469 Material Participation â Strategic Shelter Reading").
narrative_ontology:topic_domain(irc_469_material_participation_kernel__strategic_shelter_reading, "tax/regulatory/real_estate").

domain_priors:requires_active_enforcement(irc_469_material_participation_kernel__strategic_shelter_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(irc_469_material_participation_kernel__strategic_shelter_reading, '46818954-eadb-4805-aec3-bacdc9201d5a').
narrative_ontology:cs_kernel_codification('46818954-eadb-4805-aec3-bacdc9201d5a', formalized).
narrative_ontology:cs_authority_grounding('46818954-eadb-4805-aec3-bacdc9201d5a', lineage).
narrative_ontology:cs_interpretation_layer_present('46818954-eadb-4805-aec3-bacdc9201d5a').
narrative_ontology:cs_reading_relation('46818954-eadb-4805-aec3-bacdc9201d5a', irc_469_material_participation_kernel__strict_gatekeeper_reading, coexists_with).
narrative_ontology:cs_axiom('46818954-eadb-4805-aec3-bacdc9201d5a', foundational, formal_hours_satisfy_material_participation).
narrative_ontology:cs_axiom_status(formal_hours_satisfy_material_participation, holdable).
narrative_ontology:cs_axiom_grounding('46818954-eadb-4805-aec3-bacdc9201d5a', formal_hours_satisfy_material_participation, conventional).
narrative_ontology:cs_axiom('46818954-eadb-4805-aec3-bacdc9201d5a', foundational, taxpayer_elected_grouping_defines_activity_scope).
narrative_ontology:cs_axiom_status(taxpayer_elected_grouping_defines_activity_scope, holdable).
narrative_ontology:cs_axiom_grounding('46818954-eadb-4805-aec3-bacdc9201d5a', taxpayer_elected_grouping_defines_activity_scope, conventional).
narrative_ontology:cs_reference_frame('46818954-eadb-4805-aec3-bacdc9201d5a', procedural_compliance_regime).
narrative_ontology:cs_drift_state('46818954-eadb-4805-aec3-bacdc9201d5a', contemporary_irs_enforcement, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('46818954-eadb-4805-aec3-bacdc9201d5a', '').
narrative_ontology:cs_kernel_id(irc_469_material_participation_kernel__strategic_shelter_reading, irc_469_material_participation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strategic_shelter_reading, high_net_worth_real_estate_investors).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strategic_shelter_reading, tax_advisors_and_preparers).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strategic_shelter_reading, general_tax_base).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strategic_shelter_reading, non_sheltering_small_investors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Structure real estate investments through syndications and LLCs, then use hour-tracking software, property-management overlays, and grouping elections under Reg. Â§1.469-9 to satisfy the 500-hour or 100-hour material participation tests. Deduct passive losses against active and portfolio income, generating tax savings that scale with investment size and hour-allocation aggressiveness.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, high_net_worth_real_estate_investors, beneficiary,
    powerful, biographical, mobile, national).

% Design multi-entity structures, prepare contemporaneous hour logs, file grouping elections, and defend hour counts in IRS audits and Tax Court. Collect fees proportional to the tax savings achieved and the complexity of the shelter architecture.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, tax_advisors_and_preparers, beneficiary,
    organized, biographical, mobile, national).

% Administer the material participation regulations, conduct audits, and litigate disputed hour counts. Politically and legally constrained by statutory text, regulatory procedure, and real-estate-industry lobbying; cannot unilaterally impose substantive economic reality tests without rulemaking or legislation that faces organized resistance.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, irs_and_treasury, agenda_setter,
    institutional, generational, constrained, national).

% Bears the diffuse revenue cost when high-income taxpayers reduce liability through passive-loss deductions. Cannot opt out of the tax system or directly challenge another taxpayer's grouping election; experiences the constraint as an invisible upward pressure on aggregate tax rates.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, general_tax_base, payer,
    powerless, generational, trapped, national).

% Own small real estate interests but lack resources to maintain contemporaneous hour logs or hire advisors to design grouping strategies. Pay tax on equivalent economic income at higher effective rates than large investors who access the shelter.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, non_sheltering_small_investors, payer,
    moderate, biographical, constrained, national).

% Adjudicate disputes over whether hour evidence is credible and whether grouping elections are valid. Incrementally define the boundary between permissible planning and abusive shelter, but rarely revisit the mechanical statutory thresholds themselves.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, tax_courts_and_litigators, observer,
    institutional, generational, analytical, national).

% Argue that mechanical tests enable systematic income sheltering and violate horizontal equity, but are structurally excluded from setting the interpretive agenda because tax legislation and regulatory priorities are dominated by industry lobbying and revenue-estimate constraints.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, progressive_tax_reform_advocates, excluded,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(irc_469_material_participation_kernel__strategic_shelter_reading, high_net_worth_real_estate_investors).
narrative_ontology:fixing_cost_class(irc_469_material_participation_kernel__strategic_shelter_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes the tax treatment of passive activities by establishing objective, administrable criteria (hours, participation tests) to distinguish passive from active income baskets, limiting loss offsets while preserving deductions for genuine real estate operators.
% TRANSFER_FUNCTION: Moves tax liability from high-income real estate investors (via deducted passive losses) to the general tax base and non-sheltering taxpayers; simultaneously moves advisory fees from investors to tax-preparation firms.
% ABSENT_VOICES: Progressive tax reform advocates who argue for substance-over-form standards and horizontal equity; small investors without access to hour-tracking infrastructure; career IRS revenue agents who favor economic reality tests but are overridden by resource and litigation constraints.
% DISAPPEARANCE_RATIONALE: Real estate syndication structures, leverage decisions, and after-tax return models are built around the passive-loss basket architecture; removing the material participation standard would eliminate the shelter mechanism, force restructuring of limited partnerships, and shift billions in annual tax liability.
% FOUNDING_PROBLEM: The 1986 Tax Reform Act sought to stop wealthy investors from using paper losses from tax shelters to offset wages and salaries, while preserving loss deductions for those genuinely active in real estate or business operations.
% FOUNDING_PROBLEM_CORROBORATION: Congressional record and Joint Committee on Taxation analysis from 1986 corroborate the original shelter-abuse problem. Independent tax-policy scholars, GAO reports, and career IRS officials outside the beneficiary set attest that the mechanical tests have mutated into a new shelter regime.
narrative_ontology:disappearance_verdict(irc_469_material_participation_kernel__strategic_shelter_reading, world_rearranges).
narrative_ontology:founding_problem_status(irc_469_material_participation_kernel__strategic_shelter_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(irc_469_material_participation_kernel__strategic_shelter_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(irc_469_material_participation_kernel__strategic_shelter_reading, 'none', 1).
narrative_ontology:epsilon_provenance(irc_469_material_participation_kernel__strategic_shelter_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(irc_469_material_participation_kernel__strategic_shelter_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(irc_469_material_participation_kernel__strategic_shelter_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(irc_469_material_participation_kernel__strategic_shelter_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72) because the constraint enables large-scale tax-base erosion through mechanically satisfied tests that do not track economic substance. Suppression (0.65) reflects the structural suppression of substance-over-form enforcement by procedural safe-harbor conventions and resource-asymmetric audit litigation. Theater ratio (0.55) is elevated: IRS audit campaigns and disclosure requirements create a theater of enforcement, yet the shelter persists because the tests are intrinsically gameable. Accessibility collapse (0.60) is moderate-to-high: once a taxpayer retains sophisticated advisors, alternative tax treatments (paying full liability) collapse as the shelter becomes the default strategy. Resistance (0.55) reflects ongoing IRS litigation, occasional congressional scrutiny, and administrative resistance that keeps the extraction contested rather than fully normalized. The measurement series use a single shared time grid (years since 1986 enactment) so temporal analysis is aligned.
 *
 * PERSPECTIVAL GAP:
 *   The high-net-worth investor seat experiences the constraint as a legitimate planning opportunity within statutory text; the IRS seat experiences it as a coordination mechanism that has been captured by advisory ingenuity; the general tax base experiences it as an invisible extraction that raises their relative tax burden. The engine will compute these seats differently: beneficiaries (investors, advisors) receive low directional extraction, while victims (general tax base, small investors) receive high directional extraction, producing divergent per-seat classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are high_net_worth_real_estate_investors (direct tax savings) and tax_advisors_and_preparers (fee revenue). Victims are general_tax_base (diffuse revenue cost) and non_sheltering_small_investors (relative tax penalty). The IRS is the agenda-setter administering the constraint; tax courts observe. Directionality is derived structurally: investors and advisors sit at the beneficiary end (d near 0.0â0.2); general tax base and small investors sit at the target end (d near 0.8â1.0). No override is needed because beneficiary/victim declarations capture the structural relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by separating the genuine coordination function (organizing passive/active loss baskets) from the extractive overlay (mechanical test gaming). A pure snare reading would ignore that the constraint does coordinate some legitimate activity classification; a pure rope reading would ignore that the mechanical thresholds are systematically exploited. The tangled_rope classification captures the hybrid: the coordination is real, but the same structure that coordinates also extracts, and the extraction requires the enforcement and interpretive maintenance of the IRS to persist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substance_vs_procedure_ambiguity,
    'Is material participation under IRC Â§469 a substantive economic standard or a purely procedural safe harbor?',
    'Systematic judicial review comparing hour evidence to actual economic involvement, or regulatory amendment requiring independent verification of hours against property cash flows.',
    'If substantive, the effective extractiveness of the strategic shelter reading collapses toward the strict gatekeeper reading; if procedural, the shelter reading is validated and the constraint remains tangled_rope or shifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substance_vs_procedure_ambiguity, conceptual, 'Whether the kernel''s material participation standard is substantive or procedural').

omega_variable(
    kernel_reading_contest,
    'Does the strategic shelter reading represent the dominant interpretive practice, or is it a deviant exploitation of a strict gatekeeper kernel?',
    'Empirical audit of IRS examination outcomes and Tax Court rulings measuring the rate at which aggressive hour counts are sustained versus rejected.',
    'If the strict reading dominates in enforcement, this story''s extractiveness is overstated; if the shelter reading dominates, the strict reading is largely theoretical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Which reading of the material participation kernel dominates in practice').

omega_variable(
    enforcement_capacity_cycle,
    'Does IRS enforcement of material participation follow a cyclical pattern of tightening and relaxation driven by budget and political cycles?',
    'Time-series analysis of IRS audit rates, hours-per-case, and litigation outcomes for passive activity issues across fiscal years.',
    'If cyclical, temporal measurements should oscillate and the constraint may exhibit intermittent reinforcement; if secular, the trend is monotonic.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_capacity_cycle, empirical, 'Whether enforcement capacity oscillates cyclically').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(irc_469_material_participation_kernel__strategic_shelter_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irc_469_strategic_shelter_tr_t0, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(irc_469_strategic_shelter_tr_t5, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(irc_469_strategic_shelter_tr_t10, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement(irc_469_strategic_shelter_tr_t15, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 15, 0.44).
narrative_ontology:measurement(irc_469_strategic_shelter_tr_t20, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 20, 0.49).
narrative_ontology:measurement(irc_469_strategic_shelter_tr_t25, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 25, 0.53).
narrative_ontology:measurement(irc_469_strategic_shelter_tr_t30, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 30, 0.55).

% Extraction over time
narrative_ontology:measurement(irc_469_strategic_shelter_be_t0, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(irc_469_strategic_shelter_be_t5, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(irc_469_strategic_shelter_be_t10, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(irc_469_strategic_shelter_be_t15, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(irc_469_strategic_shelter_be_t20, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(irc_469_strategic_shelter_be_t25, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 25, 0.71).
narrative_ontology:measurement(irc_469_strategic_shelter_be_t30, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 30, 0.72).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(irc_469_material_participation_kernel__strategic_shelter_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(irc_469_material_participation_kernel__strategic_shelter_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the IRC Â§469 material participation kernel. The sibling strict_gatekeeper_reading (strict_gatekeeper_reading) represents the same statutory text under a substantive-economic-test interpretation. The two stories are linked as a constraint family; they share a regulatory domain but exhibit different epsilon profiles and stakeholder directionalities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
