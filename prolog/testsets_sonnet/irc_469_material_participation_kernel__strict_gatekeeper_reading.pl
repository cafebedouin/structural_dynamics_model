% ============================================================================
% CONSTRAINT STORY: irc_469_material_participation_kernel__strict_gatekeeper_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_irc_469_material_participation_kernel__strict_gatekeeper_reading, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: irc_469_material_participation_kernel__strict_gatekeeper_reading
 *   human_readable: IRC §469 Material Participation Test — Strict Gatekeeper Reading
 *   domain: tax_law/real_estate_investment/regulatory_interpretation
 *
 * SUMMARY:
 *   The material participation test under IRC §469 was designed to separate
 *   active business losses from passive shelter losses so that high-income
 *   taxpayers could not use paper real estate losses to offset unrelated wage
 *   income. Under the strict gatekeeper reading, the seven tests
 *   (particularly the 500-hour test and the 'regular, continuous, and
 *   substantial' standard) are read and enforced as demanding a high,
 *   verifiable evidentiary bar — contemporaneous logs, calendars, and
 *   third-party corroboration — rather than after-the-fact reconstruction or
 *   aggressive hour aggregation. This reading narrows the qualifying
 *   population sharply: genuine part-time landlords and dual-career investors
 *   who really manage their properties but never anticipated needing a time
 *   diary are frequently reclassified as passive on audit, losing the ability
 *   to deduct real losses against ordinary income even where the underlying
 *   economic activity was substantial.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.58).
domain_priors:suppression_score(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.62).
domain_priors:theater_ratio(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(irc_469_material_participation_kernel__strict_gatekeeper_reading, tangled_rope).
narrative_ontology:human_readable(irc_469_material_participation_kernel__strict_gatekeeper_reading, "IRC §469 Material Participation Test — Strict Gatekeeper Reading").
narrative_ontology:topic_domain(irc_469_material_participation_kernel__strict_gatekeeper_reading, "tax_law/real_estate_investment/regulatory_interpretation").

domain_priors:requires_active_enforcement(irc_469_material_participation_kernel__strict_gatekeeper_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(irc_469_material_participation_kernel__strict_gatekeeper_reading, '027411ce-ca74-4493-b214-4a490c27141b').
narrative_ontology:cs_kernel_codification('027411ce-ca74-4493-b214-4a490c27141b', formalized).
narrative_ontology:cs_authority_grounding('027411ce-ca74-4493-b214-4a490c27141b', extraction).
narrative_ontology:cs_interpretation_layer_present('027411ce-ca74-4493-b214-4a490c27141b').
narrative_ontology:cs_reading_relation('027411ce-ca74-4493-b214-4a490c27141b', irc_469_material_participation_kernel__strategic_shelter_reading, coexists_with).
narrative_ontology:cs_axiom('027411ce-ca74-4493-b214-4a490c27141b', foundational, documentation_is_the_operative_test).
narrative_ontology:cs_axiom_status(documentation_is_the_operative_test, holdable).
narrative_ontology:cs_axiom_grounding('027411ce-ca74-4493-b214-4a490c27141b', documentation_is_the_operative_test, conventional).
narrative_ontology:cs_axiom('027411ce-ca74-4493-b214-4a490c27141b', secondary, economic_substance_insufficient_without_contemporaneous_proof).
narrative_ontology:cs_axiom_status(economic_substance_insufficient_without_contemporaneous_proof, holdable).
narrative_ontology:cs_axiom_grounding('027411ce-ca74-4493-b214-4a490c27141b', economic_substance_insufficient_without_contemporaneous_proof, instrumental).
narrative_ontology:cs_reference_frame('027411ce-ca74-4493-b214-4a490c27141b', pre_1986_shelter_abuse_baseline).
narrative_ontology:cs_drift_state('027411ce-ca74-4493-b214-4a490c27141b', contemporary_enforcement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('027411ce-ca74-4493-b214-4a490c27141b', '').
narrative_ontology:cs_kernel_id(irc_469_material_participation_kernel__strict_gatekeeper_reading, irc_469_material_participation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strict_gatekeeper_reading, irs_enforcement_division).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strict_gatekeeper_reading, high_hour_operating_owners).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strict_gatekeeper_reading, tax_compliance_industry).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strict_gatekeeper_reading, part_time_landlords).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strict_gatekeeper_reading, dual_career_real_estate_investors).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strict_gatekeeper_reading, small_syndication_limited_partners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the seven material-participation tests under Treas. Reg. §1.469-5T, audits contemporaneous log evidence, and disallows passive losses claimed against ordinary income when documentation fails to meet the substantiation bar. Sets the evidentiary standard through audit guidance and litigation posture, and collects nothing directly but enforces the boundary that determines who qualifies.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, irs_enforcement_division, agenda_setter,
    institutional, generational, analytical, national).

% Owners who work full-time in their real estate activity (property managers, full-time landlords, real estate professionals under §469(c)(7)) can meet the 500-hour or comparable tests with genuine contemporaneous records. For them the strict reading validates real operational involvement and lets them deduct real losses against ordinary income without the shelter stigma attached to passive investors.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, high_hour_operating_owners, beneficiary,
    moderate, biographical, mobile, national).

% CPAs, tax attorneys, and specialized advisors sell time-log systems, participation audits, and defense-in-depth documentation packages built specifically to survive the strict reading's evidentiary bar. Their revenue scales with the compliance friction the reading imposes; they have no incentive to see the bar lowered.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, tax_compliance_industry, beneficiary,
    organized, biographical, arbitrage, national).

% Own rental property alongside a demanding primary job. They perform real management work — screening tenants, coordinating repairs, handling finances — but rarely keep contemporaneous hour logs because the work doesn't feel like something to document. Under the strict reading their participation is presumptively passive regardless of actual involvement, and their losses are suspended rather than deductible. Exit means either quitting the primary job to hit the hour threshold (often infeasible) or accepting the passive-loss limitation.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, part_time_landlords, payer,
    powerless, biographical, constrained, national).

% Professionals — doctors, engineers, executives — who invest in rental property as a secondary activity. They often satisfy the economic substance of participation (decision-making, oversight, occasional hands-on work) but cannot satisfy the strict reading's contemporaneous documentation requirement because they did not anticipate needing to prove it. Audit exposure falls disproportionately on this group because their fact patterns are the most litigated.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, dual_career_real_estate_investors, payer,
    moderate, biographical, constrained, national).

% Invest capital into syndicated real estate deals with no operational role by structural design (limited partner status itself is treated as near-categorically passive under the regulations). They cannot meet the strict reading's tests even in principle because their investment vehicle forecloses the labor the test demands — the constraint doesn't ask whether they participate, it asks whether they can prove labor that their legal role structurally excludes.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, small_syndication_limited_partners, payer,
    powerless, biographical, trapped, national).

% Adjudicates disputed material-participation determinations case by case, weighing testimony, calendars, and reconstructed logs against the regulatory tests. Its opinions calibrate how strictly 'contemporaneous' and 'regular, continuous, and substantial' are actually enforced, and its precedent is the de facto content of the strict reading.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, tax_court, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(irc_469_material_participation_kernel__strict_gatekeeper_reading, diffuse).
narrative_ontology:fixing_cost_class(irc_469_material_participation_kernel__strict_gatekeeper_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Distinguishes active business losses (deductible against ordinary income) from passive investment losses (quarantined against passive income only), preventing high-income taxpayers from using paper losses on activities they do not actually run to shelter unrelated wage or portfolio income.
% TRANSFER_FUNCTION: Moves the burden of proof onto the taxpayer claiming a loss deduction: absent contemporaneous, substantial-labor documentation, otherwise-real economic losses are administratively reclassified as passive and their tax benefit is deferred or lost, effectively transferring value from the under-documented investor to the general tax base (and, secondarily, to the compliance-services industry that sells the documentation).
% ABSENT_VOICES: Part-time landlords and dual-career investors who genuinely manage their properties but lack contemporaneous logs are rarely represented in the rulemaking or audit-guidance process; their objection — that real participation is being denied for a paperwork failure, not an economic-substance failure — surfaces mainly in scattered Tax Court petitions rather than in policy input.
% DISAPPEARANCE_RATIONALE: If the strict documentation-bar reading vanished and only economic substance mattered, a large population of currently passive-classified investors would immediately become eligible to deduct real estate losses against ordinary income, materially changing after-tax returns on rental real estate, syndication structuring, and IRS audit yield from this category — the compliance-services market built around defending the bar would also contract sharply.
% FOUNDING_PROBLEM: In the 1980s, high-income taxpayers used real estate and other tax-shelter partnerships generating large paper losses (via depreciation and leverage) with essentially no personal involvement to eliminate tax on salary and investment income; §469 was enacted in TRA 1986 to stop losses from activities in which the taxpayer did not materially participate from offsetting unrelated income.
% FOUNDING_PROBLEM_CORROBORATION: Congressional Joint Committee on Taxation reports from the TRA 1986 era and subsequent GAO reviews attest the original shelter-abuse problem was real and substantially curtailed by the mid-1990s. Tax Court judges and academic tax scholars outside the IRS and outside the compliance industry have separately observed that the strict documentation standard now falls heaviest on genuine small-scale participants rather than the large syndicated shelters the statute targeted, suggesting the founding problem has partially receded while the enforcement apparatus calibrated against it has not.
narrative_ontology:disappearance_verdict(irc_469_material_participation_kernel__strict_gatekeeper_reading, world_rearranges).
narrative_ontology:founding_problem_status(irc_469_material_participation_kernel__strict_gatekeeper_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(irc_469_material_participation_kernel__strict_gatekeeper_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(irc_469_material_participation_kernel__strict_gatekeeper_reading, 'none', 1).
narrative_ontology:epsilon_provenance(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(irc_469_material_participation_kernel__strict_gatekeeper_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(irc_469_material_participation_kernel__strict_gatekeeper_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(irc_469_material_participation_kernel__strict_gatekeeper_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.58) because the reading transforms a genuine economic-substance question into a paperwork pass/fail: real losses from real activity are disallowed not because participation didn't happen but because it wasn't documented in the specific form the strict reading demands. Suppression (0.62) reflects the audit and litigation apparatus that actively enforces the documentation bar — this is not a passive default but a standard actively defended in Tax Court and audit guidance. Theater ratio (0.40) captures the growing share of compliance activity — elaborate contemporaneous log systems, retroactive activity logs, advisor-drafted participation memos — that exists to satisfy the documentation form rather than to reflect or improve actual operational involvement. Accessibility collapse is high (0.70): once a taxpayer understands the strict reading governs their audit exposure, the practical alternative (undocumented but genuine participation) collapses entirely, since audit risk cannot be retroactively cured. Resistance (0.55) reflects active taxpayer litigation and advisory-industry pushback against aggressive audit positions, but this resistance operates within the strict frame rather than against it.
 *
 * PERSPECTIVAL GAP:
 *   From the IRS enforcement seat, the strict reading is a necessary corrective closing a loophole exploited by 1980s-style tax shelters — genuine coordination protecting the integrity of the passive-loss regime. From the part-time landlord or dual-career investor seat, the identical rule operates as an arbitrary paperwork tax on real, substantial labor that happens not to have been logged in the specific contemporaneous format regulators demand. The engine computing these seats independently is expected to diverge — that divergence is the data point, not a contradiction to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   The IRS enforcement division sets and administers the bar without collecting extraction directly, functioning as agenda-setter. High-hour operating owners and the tax compliance industry are structural beneficiaries: the former because the strict reading validates and protects their real deductions against dilution by shelter abuse, the latter because compliance friction is their revenue base. Part-time landlords and dual-career investors are targets: they bear a documentation burden disproportionate to their actual economic participation, with constrained exit (changing employment to hit the hour threshold is rarely feasible). Small syndication limited partners are structurally trapped — their investment vehicle forecloses even the possibility of meeting the labor-based tests, making the strict reading's demand for provable labor a categorical exclusion rather than a contestable threshold.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (large-scale 1980s shelter abuse via depreciation-driven paper losses on activities with no real taxpayer involvement) is substantially resolved for the population the statute was aimed at — large syndicated shelters largely disappeared after TRA 1986 closed the loophole. But the strict gatekeeper reading's enforcement apparatus, calibrated against that original abuse, now falls hardest on small-scale genuine participants whose economic behavior looks nothing like the shelters the statute targeted. This is exactly the mislabeling the classification framework exists to catch: treating a documentation-bar enforcement regime as pure coordination (closing a real loophole) obscures that, at the margin, it now extracts from taxpayers who present none of the founding abuse pattern.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_material_participation,
    'Does IRC §469''s material participation kernel properly read as a strict evidentiary gatekeeper (this reading) or as a permissive threshold achievable through aggressive hour-counting and activity grouping (the strategic_shelter_reading sibling)?',
    'Comparative Tax Court outcome analysis across circuits and time periods: a pattern of courts consistently rejecting reconstructed, non-contemporaneous logs supports the strict reading; a pattern of courts accepting reasonable-approximation testimony and after-the-fact reconstruction supports the permissive sibling reading. IRS audit-guidance revisions and Treasury regulatory amendments would also shift the balance.',
    'If the permissive sibling reading is what actually governs enforcement in practice, this story''s narrow qualifying population and high compliance friction are overstated relative to the shelter-strategy population that treats the same tests as navigable; the two readings would then need recalibrated ε values reflecting which enforcement posture actually dominates outcomes for a given taxpayer profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_material_participation, conceptual, 'Which reading of the shared IRC §469 kernel actually governs enforcement outcomes, and where the disagreement is located structurally: the evidentiary weight given to contemporaneous versus reconstructed participation records.').

omega_variable(
    documentation_bar_as_proxy_for_shelter_intent,
    'Is the contemporaneous-documentation requirement a reasonable proxy for distinguishing genuine shelter-seeking behavior from real economic participation, or has it become disconnected from that original discriminating function?',
    'Empirical study comparing audit-disallowance rates against independently verified economic substance (property management records, contractor invoices, tenant communications) for taxpayers who fail only the contemporaneous-log requirement versus those who fail on economic substance grounds.',
    'If the documentation bar disallows a substantial share of taxpayers with genuine economic substance, the strict reading''s extraction is largely proxy-substitution (Goodhart drift) rather than legitimate shelter-abuse prevention, supporting a higher effective extractiveness score than currently authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(documentation_bar_as_proxy_for_shelter_intent, empirical, 'Whether the documentation proxy still tracks the abuse it was designed to detect.').

omega_variable(
    limited_partner_categorical_exclusion,
    'Is the near-categorical treatment of limited partnership interests as passive (regardless of actual involvement) a defensible bright-line rule or an unjustifiable foreclosure of a labor-based test for a population that could, in some cases, genuinely participate?',
    'Case-law and regulatory-history review of §1.469-5T(e) and its exceptions; comparison with jurisdictions or proposed reforms that allow limited partners to qualify via a facts-and-circumstances participation showing.',
    'If the bright-line exclusion is overbroad, small syndication limited partners are being denied not on a failure to document real participation but on a categorical rule that forecloses the possibility regardless of documentation — reclassifying part of this population''s situation from ''failed proof'' to ''structurally ineligible,'' which would sharpen the snare-like character of this segment of the constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(limited_partner_categorical_exclusion, conceptual, 'Whether limited-partner passive treatment is a legitimate bright line or an unjustified structural foreclosure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0, 38).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irc__tr_t0, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(irc__tr_t6, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 6, 0.26).
narrative_ontology:measurement(irc__tr_t12, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement(irc__tr_t19, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 19, 0.33).
narrative_ontology:measurement(irc__tr_t26, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 26, 0.37).
narrative_ontology:measurement(irc__tr_t32, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 32, 0.39).
narrative_ontology:measurement(irc__tr_t38, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 38, 0.4).

% Extraction over time
narrative_ontology:measurement(irc__be_t0, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(irc__be_t6, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 6, 0.4).
narrative_ontology:measurement(irc__be_t12, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 12, 0.46).
narrative_ontology:measurement(irc__be_t19, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 19, 0.5).
narrative_ontology:measurement(irc__be_t26, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 26, 0.54).
narrative_ontology:measurement(irc__be_t32, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(irc__be_t38, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 38, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(irc__su_t0, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(irc__su_t6, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 6, 0.46).
narrative_ontology:measurement(irc__su_t12, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 12, 0.51).
narrative_ontology:measurement(irc__su_t19, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 19, 0.55).
narrative_ontology:measurement(irc__su_t26, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 26, 0.58).
narrative_ontology:measurement(irc__su_t32, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 32, 0.6).
narrative_ontology:measurement(irc__su_t38, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 38, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(irc_469_material_participation_kernel__strict_gatekeeper_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(irc_469_material_participation_kernel__strict_gatekeeper_reading, irc_469_material_participation_kernel__strategic_shelter_reading).

% DUAL FORMULATION NOTE:
% This story and irc_469_material_participation_kernel__strategic_shelter_reading are two readings of the same statutory kernel (IRC §469 material participation). This file authors the strict gatekeeper reading: high documentation bar, narrow qualifying population, extraction falling on genuine but under-documented participants. The sibling authors the permissive shelter reading: the same statutory tests treated as navigable through aggressive hour-counting and grouping elections, with extraction running in the opposite direction (shelter-seekers extracting deductibility from the tax base rather than the tax base extracting compliance cost from genuine participants). Each carries its own ε and its own beneficiary/victim structure; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(irc_469_material_participation_kernel__strict_gatekeeper_reading, powerless, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
