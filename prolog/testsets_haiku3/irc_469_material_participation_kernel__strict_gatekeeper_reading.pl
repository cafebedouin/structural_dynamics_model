% ============================================================================
% CONSTRAINT STORY: irc_469_material_participation_kernel__strict_gatekeeper_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: irc_469_material_participation_kernel__strict_gatekeeper_reading
 *   human_readable: IRC §469 Material Participation Strict Gatekeeper (Passive Loss Limitation Regime)
 *   domain: tax_law/regulatory_interpretation
 *
 * SUMMARY:
 *   IRC §469 material participation is one reading of a contested kernel.
 *   This story instantiates the strict-gatekeeper reading: material
 *   participation requires verifiable, substantial personal labor with high
 *   documentation burden (contemporaneous time records, authority over
 *   decisions, specific hours logged). The sibling reading—strategic-shelter
 *   reading—interprets the same statute permissively, treating material
 *   participation as an achievable threshold through aggressive hour-counting
 *   and grouping elections. The two readings are held by competing
 *   institutional seats and cannot both be true operationally under a single
 *   unified interpretation, but they do coexist as competing hermeneutics
 *   defended by Treasury regulation, audit practice variance, and litigation
 *   outcomes. The claim/metric independence is deliberate: the constraint is
 *   CLAIMED as tangled-rope (genuine coordination function protecting tax
 *   base + asymmetric extraction of compliance burden), while the authored
 *   metrics describe high extraction and suppression—the engine measures
 *   whether the claimed coordination justifies the extraction or whether the
 *   suppression exceeds coordination necessity.
 *
 * KEY AGENTS:
 *   - IRS regulation authority: sets standard via Treasury Regulation §1.469-5 and Notice 2019-46; enforces via audit and penalty assessment.
 *   - Ordinary-income earners: protected beneficiaries whose tax base remains stable under strict standard.
 *   - Real estate syndicators: targeted payers who cannot generate sufficient verifiable-participation documentation.
 *   - Passive investors: targeted payers whose deal economics compress when losses are disallowed.
 *   - High-earning professionals: identity-locked payers whose professional income binds them to passive status.
 *   - Real estate tax professionals: excluded from regulatory drafting; voice audible only in notice-and-comment.
 *   - Small informal operators: excluded from conversation entirely; fail gatekeeping not from lack of participation but from lack of documentation apparatus.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.68).
domain_priors:suppression_score(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.71).
domain_priors:theater_ratio(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(irc_469_material_participation_kernel__strict_gatekeeper_reading, tangled_rope).
narrative_ontology:human_readable(irc_469_material_participation_kernel__strict_gatekeeper_reading, "IRC §469 Material Participation Strict Gatekeeper (Passive Loss Limitation Regime)").
narrative_ontology:topic_domain(irc_469_material_participation_kernel__strict_gatekeeper_reading, "tax_law/regulatory_interpretation").

domain_priors:requires_active_enforcement(irc_469_material_participation_kernel__strict_gatekeeper_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(irc_469_material_participation_kernel__strict_gatekeeper_reading, '5b2282ac-fa42-40f4-8760-ee120e69f21f').
narrative_ontology:cs_kernel_codification('5b2282ac-fa42-40f4-8760-ee120e69f21f', fixed_text).
narrative_ontology:cs_authority_grounding('5b2282ac-fa42-40f4-8760-ee120e69f21f', extraction).
narrative_ontology:cs_interpretation_layer_present('5b2282ac-fa42-40f4-8760-ee120e69f21f').
narrative_ontology:cs_reading_relation('5b2282ac-fa42-40f4-8760-ee120e69f21f', irc_469_material_participation_kernel__strategic_shelter_reading, coexists_with).
narrative_ontology:cs_axiom('5b2282ac-fa42-40f4-8760-ee120e69f21f', foundational, material_participation_requires_verifiable_substantial_labor).
narrative_ontology:cs_axiom_status(material_participation_requires_verifiable_substantial_labor, holdable).
narrative_ontology:cs_axiom_grounding('5b2282ac-fa42-40f4-8760-ee120e69f21f', material_participation_requires_verifiable_substantial_labor, deontological).
narrative_ontology:cs_axiom('5b2282ac-fa42-40f4-8760-ee120e69f21f', secondary, documentation_bar_gatekeeps_passive_loss_access).
narrative_ontology:cs_axiom_status(documentation_bar_gatekeeps_passive_loss_access, holdable).
narrative_ontology:cs_axiom_grounding('5b2282ac-fa42-40f4-8760-ee120e69f21f', documentation_bar_gatekeeps_passive_loss_access, empirically_contingent).
narrative_ontology:cs_reference_frame('5b2282ac-fa42-40f4-8760-ee120e69f21f', statutory_gatekeeping_mandate_1986).
narrative_ontology:cs_drift_state('5b2282ac-fa42-40f4-8760-ee120e69f21f', contemporary_2026, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5b2282ac-fa42-40f4-8760-ee120e69f21f', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(irc_469_material_participation_kernel__strict_gatekeeper_reading, irc_469_material_participation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strict_gatekeeper_reading, ordinary_income_earners).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strict_gatekeeper_reading, tax_authority).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strict_gatekeeper_reading, real_estate_syndicators).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strict_gatekeeper_reading, passive_investors).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strict_gatekeeper_reading, high_earning_professionals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues Treasury Regulation §1.469-5 defining material participation through facts-and-circumstances tests and safe harbors. Updates guidance via Notice, Revenue Procedures, and Chief Counsel Advice. Enforces through examination, penalty assessment, and litigation. Controls the interpretation of statutory ambiguity and the resource allocation to audit material-participation claims.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, irs_regulation_authority, agenda_setter,
    institutional, generational, analytical, national).

% Wage and salary earners whose ordinary income is shielded from offsetting passive losses. The constraint ensures their tax base remains predictable and high-income investors cannot compress their effective rates through syndicated real estate losses. They do not directly engage with material-participation rules but benefit from the constraint's gatekeeping.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, ordinary_income_earners, beneficiary,
    organized, generational, mobile, national).

% Federal revenue is higher under strict material-participation standards because fewer passive losses offset ordinary income and audit activity generates penalty revenue. The constraint produces direct fiscal benefit through increased tax collection and indirect benefit through reduced shelter-planning activity.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, tax_authority, beneficiary,
    institutional, generational, analytical, national).

% Promoters and general partners of real estate partnerships and funds who market passive investments to high-net-worth individuals. The strict-gatekeeper standard prevents pass-through of passive losses because verifiable material participation by the syndicate GP is rarely achievable at scale (time constraints, authority issues). They bear compliance costs (documentation, tax opinion letters, regulatory advice) and deal-economic compression (lower after-tax returns reduce attractiveness).
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, real_estate_syndicators, payer,
    powerful, biographical, constrained, national).

% High-net-worth individuals and institutions investing capital in syndicated real estate deals expecting passive loss flow-through for ordinary-income offset. The strict standard disallows losses where the syndicator cannot generate sufficient verifiable participation hours, compressing after-tax returns and making deals less attractive relative to competing investments. Exit requires liquidating at loss or holding to maturity.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, passive_investors, payer,
    powerful, biographical, constrained, national).

% Doctors, lawyers, executives, and consultants with substantial ordinary income from professional services. Their W-2 or 1099 status and full-time career commitments make material participation in real estate structurally impossible (cannot allocate 750+ hours to property management while maintaining professional practice). The constraint identity-locks them into passive-investor status and disallows losses, preventing ordinary-income offset that would otherwise be available if they had discretionary time or different career structure.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, high_earning_professionals, payer,
    powerful, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(irc_469_material_participation_kernel__strict_gatekeeper_reading, high_earning_professionals, excluded).

% CPAs, tax attorneys, and advisors designing real estate tax strategies and preparing material-participation documentation for clients. They would advocate for clearer safe harbors, broader grouping elections, and more generous hour-averaging provisions. Their voice is excluded from formal rulemaking; they participate only through notice-and-comment on proposed guidance, which rarely influences final Treasury Regulation.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, real_estate_tax_professionals, excluded,
    powerful, biographical, constrained, national).

% Individual landlords, small rental-property owners, and informal real estate operators who participate materially (managing properties, making decisions, spending time) but do not maintain contemporaneous time records, formal authority documentation, or professional-grade administrative systems. They fail the gatekeeping test not because they lack participation but because they lack the documentation apparatus. Cannot exit without abandoning properties and cannot access litigation resources to challenge disallowances.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, small_real_estate_operators, excluded,
    moderate, biographical, trapped, local).

% House Ways and Means and Senate Finance Committees who could rewrite §469 or direct Treasury to issue more permissive regulations. They monitor enforcement disparities but operate under revenue constraints and real-estate-industry lobbying pressure. Can override Treasury interpretation only through statutory amendment.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, legislative_tax_committees, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(irc_469_material_participation_kernel__strict_gatekeeper_reading, tax_authority).
narrative_ontology:fixing_cost_class(irc_469_material_participation_kernel__strict_gatekeeper_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents high-income earners from using passive real estate losses to offset their ordinary income without meaningful involvement in property operations, management decisions, or ownership responsibilities. Coordinates the integrity of the progressive tax system by ensuring that ordinary-income deductions require active participation rather than capital deployment alone. Solves the coordination problem of distinguishing genuine active investors from passive capitalists in a complex real estate market.
% TRANSFER_FUNCTION: Transfers the tax benefit of loss deductions away from passive investors and real estate syndicators (who cannot meet the documentation threshold) toward the IRS (as collected revenue) and indirectly toward ordinary-income earners (who retain their full tax base unshielded). Transfers compliance burden (audit exposure, documentation costs, penalty risk, tax advice) from the IRS to syndicators and real estate professionals.
% ABSENT_VOICES: Small informal real estate operators who materially participate but lack professional recordkeeping; real estate tax professionals and CPAs whose regulatory-clarity recommendations are excluded from formal rulemaking; academics and economists arguing that §469 shifted shelter tactics rather than prevented abuse; high-earning professionals in full-time careers who could argue identity-lock creates an unfair categorical exclusion; taxpayer-rights organizations advocating for clearer safe harbors and less aggressive audit practices.
% DISAPPEARANCE_RATIONALE: If the strict-gatekeeper standard vanished—whether through legislative repeal of §469, Treasury reinterpretation to the permissive standard, or court decision declaring the strict standard ultra vires—real estate syndication deal economics would immediately expand. Passive investors would claim ordinary-income losses; federal revenue would fall; real estate fund formation would accelerate; high-earning professionals would restructure investment vehicles to claim material participation through accounting technique; tax planning industry would reorganize around aggressive loss-passthrough strategies. The current deployment pattern (constrained real estate fundraising, compressed after-tax returns, limited passive loss deductions) exists because the strict standard forecloses an entire tax-planning population. Its disappearance would rearrange real estate capital flows and federal tax collection.
% FOUNDING_PROBLEM: TEFRA (1982) and OBRA (1986) identified explosive growth in tax shelters using real estate passive losses to offset ordinary income of high-earners, eroding the ordinary tax base and enabling wealthy individuals to reduce effective tax rates below wage earners. Shelters exploited ambiguities in the statutory definition of 'material participation' and used aggressive loss-passthrough structures to shield ordinary income. Congress authorized the IRS to establish gatekeeping standards to prevent abuse.
% FOUNDING_PROBLEM_CORROBORATION: Congressional record (House Ways and Means Committee reports on OBRA 1986) attests the founding problem clearly: shelter proliferation and revenue erosion. IRS audit statistics through the 1990s and 2000s document continued aggressive material-participation claims. However, academic research (starting in the 1990s) and CPA/tax attorney commentary attest that §469 shifted shelter tactics rather than prevented abuse—passive-loss limitations prompted alternative strategies (cost segregation, qualified opportunity zones, energy credits) that achieved similar tax outcomes outside §469. The problem evolved but the standard remained rigid. Current IRS officials attest the problem remains live based on audit findings; critics attest that audit findings reflect aggressive audit practice rather than genuine shelter proliferation.
narrative_ontology:disappearance_verdict(irc_469_material_participation_kernel__strict_gatekeeper_reading, world_rearranges).
narrative_ontology:founding_problem_status(irc_469_material_participation_kernel__strict_gatekeeper_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(irc_469_material_participation_kernel__strict_gatekeeper_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(irc_469_material_participation_kernel__strict_gatekeeper_reading, 'none', 1).
narrative_ontology:epsilon_provenance(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is 0.68 because the strict standard transfers tax benefits away from a large population (passive investors, real estate professionals) and that transfer is decoupled from genuine participation failure—many operators participate substantially but fail the documentation gate. Suppression is 0.71 because the constraint's persistence depends on active IRS audit targeting of material-participation claims and aggressive disallowance when documentation is inadequate; without enforcement, the population claiming loses would spike. Theater is 0.42 because a substantial share of enforcement activity is procedural gatekeeping (document review, hour verification, grouping-election denial) rather than substantive participation assessment. The measurement series runs one shared time grid across all three metrics and all time points: extractiveness rises from 0.45 (early regime, permissive court interpretations) to 0.68 (plateau from 2015 onward as audit practice hardened and taxpayer behavior stabilized). Suppression tracks closely, rising as enforcement infrastructure matured. Theater rises early (1987-2015) as gatekeeping became the dominant audit tactic, then plateaus (2015-2026) as the regime calcified and enforcement became routine.
 *
 * PERSPECTIVAL GAP:
 *   From the IRS and ordinary-income-earner seat, the constraint is genuine coordination preventing abuse and protecting the tax base—extractiveness appears lower (coordination value ~0.3, net extraction ~0.4 after subtracting coordination gain). From the passive-investor and syndicator seats, the same structure is pure extraction dressed in procedural language—extractiveness appears higher (extraction ~0.75, coordination value ~0.1, net extraction ~0.7). The engine computes per-seat directionality: IRS seat d ≈ 0.2 (beneficiary, low extraction felt), passive-investor seat d ≈ 0.85 (target, high extraction felt). The authored metrics represent the structural measurement (how much of the constraint's operation is actually gatekeeping vs. genuine participation verification), which sits between the two seats' perceived extractiveness.
 *
 * DIRECTIONALITY LOGIC:
 *   The IRS is the agenda-setter (controls interpretation, enforcement, updates guidance). Ordinary-income earners and the tax authority are beneficiaries: their tax base is protected and revenue is stable. Real estate syndicators, passive investors, and high-earning professionals are victims: they lose tax benefits and bear compliance burden. The directed asymmetry is structural: beneficiaries get protection without visible compliance cost; payers get compliance burden and loss disallowance. High-earning professionals are identity-locked: their professional income binds them to W-2/1099 status, which makes material-participation claims structurally impossible for full-time careers; unlike organized investors who can hire operators, they cannot reallocate time. Small informal operators are excluded—not because they lack participation but because they lack the documentation apparatus the gatekeeper requires.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1986 shelter abuse) was genuinely live and justified gatekeeping. Academic research from the 1990s-2010s established that §469 shifted shelter tactics rather than prevented abuse—the problem evolved but the standard remained rigid. The constraint meets the mandatrophy threshold: the founding mandate (prevent shelter abuse) has partially atrophied (abuse shifted to form-compliant strategies), but the standard persists and has actually intensified (theater and suppression rise from 1987-2015). The constraint is a tangled rope (coordination function—tax base stability—paired with asymmetric extraction—compliance burden on passive investors). It is NOT a piton because the IRS actively maintains and intensifies enforcement (not performance theater) and ordinary-income earners remain beneficiaries. The intensification suggests the constraint has captured an agenda beyond the original mandate: it now functions as a wealth-transfer mechanism (from high-net-worth real estate investors to ordinary-income earners) that operates orthogonal to shelter prevention.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gatekeeping_vs_abuse_prevention,
    'Does the strict-gatekeeper standard actually prevent tax-shelter abuse, or does it merely shift abuse tactics to form-compliant strategies while increasing compliance burden on good-faith participants?',
    'Empirical audit-outcome analysis comparing (1) shelter-abuse rates before §469 (pre-1987) vs. post-strict-interpretation (2010+), and (2) distribution of passive-loss disallowances by taxpayer type (professional syndicators vs. passive investors vs. small operators). Also, analysis of alternative shelter strategies (cost segregation, qualified opportunity zones, energy credits) adoption post-2010.',
    'If abuse rates stayed roughly constant and alternative shelters grew, the constraint is primarily extraction dressed as regulation; if abuse rates dropped and alternative shelters did not materially grow, the coordination function is genuine. This determines whether the constraint should remain classified as tangled_rope or reclassify toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeping_vs_abuse_prevention, empirical, 'Whether §469 gatekeeping prevents shelter abuse or shifts it.').

omega_variable(
    documentation_bar_as_class_sorting,
    'Is the high documentation bar (contemporaneous time records, specific hours, authority claims) neutral to participation, or does it systematically exclude informal and small-scale operators who participate materially but lack administrative apparatus?',
    'Survey of small real estate operators (rental property owners, small syndication GPs) identifying documentation practices and comparing disallowance rates by entity size and professionalization level. Analysis of audit outcomes: do small operators with equivalent participation hours receive different treatment than large operators with professional recordkeeping?',
    'If small operators are systematically excluded despite participation, the constraint operates as class sorting (extracting from working-class and small-business operators) rather than shelter prevention. This would support reclassification toward snare and identify a victim class currently obscured.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(documentation_bar_as_class_sorting, empirical, 'Whether documentation requirements are neutral or systematically exclude informal operators.').

omega_variable(
    regulatory_interpretation_vs_statutory_mandate,
    'Has Treasury regulatory interpretation of ''material participation'' drifted beyond the statutory mandate (prevent shelter abuse) toward a regulatory preference (maximize complexity, audit activity, and compliance cost)?',
    'Legislative history analysis of OBRA 1986 statutory language vs. Treasury Regulation §1.469-5 (issued 1988, updated multiple times). Interviews with legislative drafters, Treasury officials, and IRS practitioners. Comparison of Treasury regulatory updates to evidence of abuse trends—do updates respond to newly detected shelters or do they preemptively tighten standards regardless of observed abuse?',
    'If regulatory drift is documented, the constraint has partially decoupled from its founding mandate and operates under agenda-setter control rather than statute-defined purpose. This supports mandatrophy classification and suggests remediation through legislative override or regulatory reform is politically feasible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_interpretation_vs_statutory_mandate, conceptual, 'Whether regulatory interpretation has drifted beyond statutory mandate.').

omega_variable(
    reading_contest_materiality,
    'Are the strict-gatekeeper and strategic-shelter readings genuinely incompatible as unified operational standards, or can both coexist through taxpayer sorting (one reading applies to large operators, the other to small investors)?',
    'Audit-outcome analysis comparing IRS treatment of material-participation claims by taxpayer type, jurisdiction, and IRS district. Do auditors apply consistent standards or vary by taxpayer characteristic? Litigation outcomes on material-participation appeals: do courts adopt strict-gatekeeper reasoning or strategic-shelter reasoning, and does outcome correlate with taxpayer size/sophistication?',
    'If readings cannot coexist and one must dominate (strict-gatekeeper currently dominates), the framework is unstable and subject to reversal if political conditions shift or a Supreme Court decision clarifies statutory ambiguity. If readings coexist through sorting, the constraint operates as differentiated extraction and stability is higher.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_materiality, empirical, 'Whether the two readings of material participation can coexist or must be resolved.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(irc_469_material_participation_kernel__strict_gatekeeper_reading, 1987, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irc__tr_t1987, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 1987, 0.18).
narrative_ontology:measurement(irc__tr_t1995, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 1995, 0.24).
narrative_ontology:measurement(irc__tr_t2005, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 2005, 0.32).
narrative_ontology:measurement(irc__tr_t2015, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(irc__tr_t2020, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 2020, 0.42).
narrative_ontology:measurement(irc__tr_t2026, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 2026, 0.42).

% Extraction over time
narrative_ontology:measurement(irc__be_t1987, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 1987, 0.45).
narrative_ontology:measurement(irc__be_t1995, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 1995, 0.52).
narrative_ontology:measurement(irc__be_t2005, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 2005, 0.61).
narrative_ontology:measurement(irc__be_t2015, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 2015, 0.66).
narrative_ontology:measurement(irc__be_t2020, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 2020, 0.68).
narrative_ontology:measurement(irc__be_t2026, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 2026, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(irc__su_t1987, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 1987, 0.48).
narrative_ontology:measurement(irc__su_t1995, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 1995, 0.58).
narrative_ontology:measurement(irc__su_t2005, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 2005, 0.66).
narrative_ontology:measurement(irc__su_t2015, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 2015, 0.71).
narrative_ontology:measurement(irc__su_t2020, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 2020, 0.71).
narrative_ontology:measurement(irc__su_t2026, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 2026, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(irc_469_material_participation_kernel__strict_gatekeeper_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.14).
narrative_ontology:affects_constraint(irc_469_material_participation_kernel__strict_gatekeeper_reading, irc_469_material_participation_kernel__strategic_shelter_reading).
narrative_ontology:affects_constraint(irc_469_material_participation_kernel__strict_gatekeeper_reading, passive_activity_loss_limitation_regime).
narrative_ontology:affects_constraint(irc_469_material_participation_kernel__strict_gatekeeper_reading, real_estate_professional_status_gatekeeping).

% DUAL FORMULATION NOTE:
% The IRC §469 material-participation kernel decomposes into two structurally distinct constraints: the strict-gatekeeper reading (this story) instantiates material participation as a high-friction verification gate that excludes passive investors and informal operators. The strategic-shelter reading (sibling) instantiates the same statute as a permissive threshold achievable through accounting technique. Their ε values diverge sharply: strict-gatekeeper ε=0.68 (extraction through gatekeeping), strategic-shelter ε≈0.35 (minimal extraction, coordination function dominates). The readings are not two measurements of one constraint—they are two incompatible operational standards instantiated by competing regulatory and judicial authorities. They affect one another: if strict-gatekeeper dominates (current regime), passive real estate syndication becomes higher-risk and less attractive (strategic-shelter reading is foreclosed operationally). Link to upstream constraints: passive_activity_loss_limitation_regime (the parent statutory framework, ε≈0.5, pure coordination at §469 statutory level); real_estate_professional_status_gatekeeping (sibling constraint, defining occupational status tests that create the identity-lock dynamic for high-earning professionals).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(irc_469_material_participation_kernel__strict_gatekeeper_reading, institutional, 0.15).
constraint_indexing:directionality_override(irc_469_material_participation_kernel__strict_gatekeeper_reading, organized, 0.08).
constraint_indexing:directionality_override(irc_469_material_participation_kernel__strict_gatekeeper_reading, powerful, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
