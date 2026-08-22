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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: IRC 469 Material Participation Requirement (Strict Gatekeeper Reading)
 *   domain: tax_law/real_estate_investment
 *
 * SUMMARY:
 *   IRC § 469 Material Participation Requirement under the strict gatekeeper
 *   reading: The Internal Revenue Service, through Treasury regulation
 *   (Treas. Reg. § 1.469-5T) and examination practice, interprets 'material
 *   participation' narrowly — requiring verifiable, contemporaneous
 *   documentation of substantial personal labor (typically 100+ hours/year in
 *   real estate contexts). Under this reading, passive losses are rarely
 *   deductible against ordinary income, and the burden of proof on taxpayers
 *   is high. The strict reading emerged as the dominant IRS interpretation
 *   post-1986 and solidified through Tax Court precedent; its primary effect
 *   is to prevent high-income passive investors and business owners from
 *   sheltering ordinary income. The sibling reading
 *   (strategic_shelter_reading) interprets material participation
 *   permissively, permitting hour-counting aggregation and grouping elections
 *   that widen the qualifying population.
 *
 * KEY AGENTS:
 *   - Internal Revenue Service: Agenda-setter; maintains the documentation bar through regulatory interpretation and audit enforcement
 *   - Passive real estate investors and independent business owners: Payers; bear the extraction of disallowed passive losses and compliance friction
 *   - W2 wage earners and the tax authority: Beneficiaries; collect from the preserved tax base and reduced cross-income-class arbitrage
 *   - Tax-shelter promoters and permissive-reading advocates: Excluded; would argue for wider qualifying standards but have no voice in IRS regulation-writing
 *   - Congress: Observer; retains power to amend the statute and override the regulatory reading
 *   - Tax Court: Observer; legitimizes the strict interpretation through precedent and shifts litigation advantage to the IRS
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.68).
domain_priors:suppression_score(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.72).
domain_priors:theater_ratio(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, accessibility_collapse, 0.63).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(irc_469_material_participation_kernel__strict_gatekeeper_reading, tangled_rope).
narrative_ontology:human_readable(irc_469_material_participation_kernel__strict_gatekeeper_reading, "IRC 469 Material Participation Requirement (Strict Gatekeeper Reading)").
narrative_ontology:topic_domain(irc_469_material_participation_kernel__strict_gatekeeper_reading, "tax_law/real_estate_investment").

domain_priors:requires_active_enforcement(irc_469_material_participation_kernel__strict_gatekeeper_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(irc_469_material_participation_kernel__strict_gatekeeper_reading, 'ed2cc02b-7425-4c18-8638-4a1634899608').
narrative_ontology:cs_kernel_codification('ed2cc02b-7425-4c18-8638-4a1634899608', fixed_text).
narrative_ontology:cs_authority_grounding('ed2cc02b-7425-4c18-8638-4a1634899608', extraction).
narrative_ontology:cs_interpretation_layer_present('ed2cc02b-7425-4c18-8638-4a1634899608').
narrative_ontology:cs_reading_relation('ed2cc02b-7425-4c18-8638-4a1634899608', irc_469_material_participation_kernel__strategic_shelter_reading, coexists_with).
narrative_ontology:cs_axiom('ed2cc02b-7425-4c18-8638-4a1634899608', foundational, material_participation_requires_contemporaneous_documentation).
narrative_ontology:cs_axiom_status(material_participation_requires_contemporaneous_documentation, holdable).
narrative_ontology:cs_axiom_grounding('ed2cc02b-7425-4c18-8638-4a1634899608', material_participation_requires_contemporaneous_documentation, conventional).
narrative_ontology:cs_axiom('ed2cc02b-7425-4c18-8638-4a1634899608', foundational, passive_loss_deductibility_narrowly_constrained).
narrative_ontology:cs_axiom_status(passive_loss_deductibility_narrowly_constrained, holdable).
narrative_ontology:cs_axiom_grounding('ed2cc02b-7425-4c18-8638-4a1634899608', passive_loss_deductibility_narrowly_constrained, empirically_contingent).
narrative_ontology:cs_reference_frame('ed2cc02b-7425-4c18-8638-4a1634899608', tax_shelter_prevention_via_documentation_enforcement).
narrative_ontology:cs_drift_state('ed2cc02b-7425-4c18-8638-4a1634899608', contemporary_post_tax_litigation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ed2cc02b-7425-4c18-8638-4a1634899608', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(irc_469_material_participation_kernel__strict_gatekeeper_reading, irc_469_material_participation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strict_gatekeeper_reading, tax_authority).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strict_gatekeeper_reading, w2_wage_earners).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strict_gatekeeper_reading, passive_real_estate_investors).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strict_gatekeeper_reading, independent_business_owners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strict_gatekeeper_reading, real_estate_professional_designation_holders).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strict_gatekeeper_reading, real_estate_professional_designation_holders).
narrative_ontology:constraint_vindicates(irc_469_material_participation_kernel__strict_gatekeeper_reading, tax_shelter_prevention_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces the material participation standard through regulatory interpretation, examination protocols, and dispute resolution. Issues guidance, trains revenue agents, disallows passive losses failing documentation, and defends positions in litigation. Maintains gatekeeping authority through ongoing regulatory refinement and audit strictness.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, internal_revenue_service, agenda_setter,
    institutional, generational, analytical, national).

% Engage in real estate syndication, partnerships, and REITs without active management. Even with 60+ annual hours on partnership affairs, the IRS scrutinizes whether this qualifies as material participation under the narrow reading. Must maintain contemporaneous documentation; absent compliant records, cannot offset ordinary income with passive losses. Exit options: litigate (expensive, low win rate), reorganize to qualify as real estate professionals (requires restructuring), or accept the loss disallowance.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, passive_real_estate_investors, payer,
    moderate, biographical, constrained, national).

% Operate rental car fleets, laundry facilities, equipment leasing, and other passive businesses through pass-through entities where labor is genuine and substantial. Must maintain daily time logs, time-tracking systems, and contemporaneous written documentation of all management activities. Absent formalized records, IRS disallows passive loss claims even if hours and activities are real. The constraint creates compliance friction that smaller businesses absorb as administrative overhead.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, independent_business_owners, payer,
    moderate, biographical, constrained, national).

% Earn ordinary income from employment without ability to shelter via passive losses. The strict material participation requirement prevents higher-income neighbors from offsetting wage income with passive losses, preserving tax base on wages and reducing cross-income-class arbitrage. Benefit indirectly through lower effective tax rates on wage income.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, w2_wage_earners, beneficiary,
    powerful, biographical, mobile, national).

% Collects higher revenue through disallowed passive losses, translating to higher taxable income for affected taxpayers. Revenue from accuracy-related penalties and interest on underpaid taxes when disallowances are contested. The constraint's enforcement directly increases annual revenue collection.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, tax_authority, beneficiary,
    institutional, generational, analytical, national).

% Would design and sell aggressive passive loss strategies (grouping elections, hour-counting regimes, related-party partnerships) that exploit permissive material participation thresholds. The strict reading constrains their business model by raising qualifying bar. Have opposed the reading through litigation, technical commentary, and legislative advocacy but are excluded from IRS regulation-writing.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, tax_shelter_promoters, excluded,
    powerful, biographical, constrained, national).

% Real estate agents, developers, and property managers qualify as real estate professionals and can deduct passive losses against ordinary income via IRC 469(c)(7) exemption. The strict reading benefits them by making the base category narrow and defensible — the exemption becomes valuable precisely because the base rule is stringent. Pay in documentary compliance and organizational overhead to maintain professional status eligibility.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, real_estate_professional_designation_holders, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(irc_469_material_participation_kernel__strict_gatekeeper_reading, real_estate_professional_designation_holders, payer).

% Enacted IRC 469 with directive to Treasury to define material participation via regulation. Does not directly administer the constraint but retains power to amend the statute, overriding the regulatory reading. Legislative record shows intent to prevent tax-shelter abuse; both interpretive readings claim fidelity to that intent.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, congress, observer,
    institutional, generational, analytical, national).

% Review disputes between taxpayers and IRS over material participation determinations. The strict reading shifts litigation advantage toward the IRS — taxpayers must produce contemporaneous records to rebut disallowances; absent such records, burden disadvantages the taxpayer. Tax Court precedent has upheld strict documentation requirements and rejected post-hoc testimony as insufficient.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, tax_court_judges, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(irc_469_material_participation_kernel__strict_gatekeeper_reading, tax_authority).
narrative_ontology:fixing_cost_class(irc_469_material_participation_kernel__strict_gatekeeper_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents income-class arbitrage and tax-shelter abuse by imposing a uniform, high-friction standard for claiming passive losses: all taxpayers seeking to offset ordinary income with passive activity losses must meet the same documentary evidence standard, reducing distortions where sophisticated investors shelter income while wage-earners cannot.
% TRANSFER_FUNCTION: Moves deductibility rights (and hence after-tax income) from passive investors and business owners who cannot meet documentation requirements to the tax authority (via disallowed losses) and indirectly to wage-earners (via preserved tax base on ordinary income). The constraint transfers compliance cost (time, administrative overhead, documentation systems) from the IRS to taxpayers attempting to claim passive losses.
% ABSENT_VOICES: Tax-shelter promoters and permissive-reading advocates (e.g., real estate industry associations, passive investment syndicators) would argue for narrower documentation standards and easier grouping elections; they are excluded from IRS regulation-writing but participate through litigation and legislative advocacy channels. Small business owners and passive investors without professional accounting infrastructure are similarly excluded from the informal regulatory consensus-building that favors large, sophisticated operators.
% DISAPPEARANCE_RATIONALE: If the strict documentation bar disappeared and material participation reverted to a permissive interpretation, passive loss deductions would surge, tax revenue from high-income investors would fall sharply, and the tax system would face pressure to raise rates on ordinary wage income to maintain revenue neutrality. Real estate and passive business investment structures would proliferate; the architectural incentive to shelter income through passive losses would re-emerge; the IRS's enforcement discretion would widen.
% FOUNDING_PROBLEM: In the 1980s, sophisticated investors used passive loss strategies to shelter ordinary income through real estate and commodity partnerships, reducing the effective tax rate on high earners relative to wage-earners and eroding the tax base. The Tax Reform Act of 1986 enacted IRC 469 to prevent this arbitrage by limiting passive losses — the 'founding problem' is tax-shelter abuse that widens after-tax inequality.
% FOUNDING_PROBLEM_CORROBORATION: The IRS and Treasury affirm the founding problem is live: passive loss shelter strategies re-emerge whenever documentation standards relax (e.g., real estate professional designation expansions have repeatedly sparked IRS crackdowns). Independent tax policy analysts (e.g., Treasury research staff, academic tax economists) affirm that absent strict enforcement, passive loss arbitrage persists. Industry advocates (real estate syndicators, tax-shelter promoters) dispute that the problem persists at scale, citing modern market efficiency and IRS enforcement; however, their attestation is from the beneficiary side of the strict reading (they would benefit from a permissive reading). Neutral corroboration (legislative history, prior Treasury analyses, published Tax Court precedent trends) supports that the founding problem remains live.
narrative_ontology:disappearance_verdict(irc_469_material_participation_kernel__strict_gatekeeper_reading, world_rearranges).
narrative_ontology:founding_problem_status(irc_469_material_participation_kernel__strict_gatekeeper_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(irc_469_material_participation_kernel__strict_gatekeeper_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is measured at 0.68: the constraint transfers real economic value (deductibility, after-tax income) from passive investors to the tax authority and wage-earners. The transfer is substantial because passive loss disallowances can run to six figures in high-income scenarios. Suppression is higher (0.72) because the constraint's persistence depends on IRS enforcement discretion and the high documentation bar — it is not self-enforcing through market mechanism or taxpayer preference. Theater ratio is moderate (0.41) because roughly 40% of IRS examination activity around passive losses involves validating documentation compliance rather than substantive economic analysis — the form of compliance crowds out content. Accessibility_collapse is moderate (0.63): alternatives to the strict reading exist (permissive interpretation, legislative amendment, real estate professional exemption) but are high-friction to access and require either litigation risk or Congressional action. Resistance is moderate (0.58): tax-shelter promoters, real estate industry associations, and affected passive investors mount continuous resistance through litigation, technical commentary, and legislative advocacy, but lack the power to overturn the IRS interpretation unilaterally.
 *
 * PERSPECTIVAL GAP:
 *   From the IRS seat, the constraint is coordination and enforcement: a uniform standard preventing tax-shelter abuse, protecting the tax base, and preserving the progressivity of the income tax system. From the passive investor seat, the constraint is extractive: a high-friction documentation requirement that disallows legitimate business deductions and forces administrative overhead. From the wage-earner seat, it is protective: a constraint preventing their wealthier neighbors from sheltering income. The engine computes directionality from the structural data: IRS holds d near 0.0 (full beneficiary, sets the rules), passive investors near 1.0 (full target, constrained and documented), wage-earners near 0.3 (moderate beneficiary, benefit indirectly but carry no direct cost). The perspectival gap is the divergence between the IRS's coordination narrative and the payer seats' extraction experience.
 *
 * DIRECTIONALITY LOGIC:
 *   The IRS is the agenda-setter and beneficiary (d ≈ 0.1): it collects from disallowed losses and maintains regulatory authority. Passive investors and independent business owners are the primary targets (d ≈ 0.85): they bear the extraction (disallowed losses, compliance cost, uncertainty) and have constrained exit (cannot avoid passive participation structures without abandoning investment strategies, and cannot easily relocate to a permissive jurisdiction). W2 wage earners are secondary beneficiaries (d ≈ 0.25): they benefit from the preserved tax base but carry no direct cost and have mobile exit (they can shift income sources or seek deductions in other areas). Tax-shelter promoters are excluded (d not computed): they cannot participate in the constraint's operation, though they would shift to d ≈ 1.0 if the permissive reading prevailed.
 *
 * MANDATROPHY ANALYSIS:
 *   The strict material participation reading demonstrates genuine mandatrophy risk. The founding problem (1980s tax-shelter abuse through passive loss arbitrage) is contested: the tax authority and neutral analysts affirm it remains live, but real estate industry advocates dispute the scale and argue modern enforcement is sufficient. If the founding problem is deemed dead or substantially solved, the constraint's continued strict enforcement becomes inertial rather than justified-by-problem-solving — it would reclassify from tangled_rope (coordination + extraction) to piton (inert extraction maintained by theater and institutional path-dependency). The measurement series shows theater_ratio rising from 0.28 to 0.41 over the interval, consistent with a pattern of documentation compliance becoming a ritualistic end-in-itself rather than a means of substantive shelter prevention. If theater_ratio continues to rise above 0.5, it signals the constraint is increasingly maintained by form rather than function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_persistence_empirical,
    'Does tax-shelter abuse through passive loss arbitrage remain a live problem at economically significant scale, or has it been substantially controlled and the constraint now persists primarily by institutional inertia?',
    'Treasury analysis of Form 8582 (Passive Activity Loss Limitation) filings and IRS enforcement data: track the volume and magnitude of disallowed passive losses over time, and cross-reference litigation trends. If disallowances flatten while theater_ratio rises, the founding problem is dead.',
    'If dead: constraint reclassifies from tangled_rope (justifiable extraction) to piton (inert extraction maintained by theater). Mandatrophy is resolved in favor of the payers. If live: the strict reading remains justified as anti-abuse coordination, and no mandatrophy verdict applies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_persistence_empirical, empirical, 'Whether the 1980s founding problem (tax-shelter abuse) persists as a live threat or has been substantially contained.').

omega_variable(
    real_estate_professional_exemption_scope,
    'Does the real estate professional exemption (IRC 469(c)(7)) narrow the strict reading''s victim class to an economically small group, or does it function as a broad escape hatch for organized investors?',
    'IRS data on real estate professional status claims and Tax Court litigation: track the population qualifying for the exemption relative to passive investors, and examine whether the exemption has been subject to narrowing or broadening interpretations.',
    'If the exemption is narrow and tightly enforced, the strict reading''s payer class is small and self-selected (sole proprietors, unorganized investors lacking professional designation). If the exemption is broad, sophisticated investors can escape the constraint by organizing as real estate professionals, concentrating the extraction on less-organized passive investors — this would increase the snare-character of the constraint relative to its coordinative function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(real_estate_professional_exemption_scope, empirical, 'Whether the real estate professional exemption is a narrow carve-out or a broad escape hatch.').

omega_variable(
    regulatory_reading_vs_statutory_authority,
    'Does Treasury''s authority to define ''material participation'' via regulation (IRC 469(b)) permit the strict gatekeeper reading, or does the statutory language permit a wider range of defensible interpretations?',
    'Tax Court de novo review of Treasury Regulation § 1.469-5T under Administrative Procedure Act standards: would a reviewing court uphold the strict reading as within Treasury''s Chevron deference zone, or would it be overturned as an unreasonable interpretation of the statutory term?',
    'If de novo review would overturn the reading: the constraint is vulnerable to legal defeat and the committer framing is unstable (either reading would be equally defensible). If upheld: the strict reading has strong legal grounding and the committer choice (strict vs. permissive) is a Treasury discretionary judgment, not a statutory mandate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_reading_vs_statutory_authority, conceptual, 'Whether the strict reading is the only defensible interpretation or one of several within Treasury''s discretion.').

omega_variable(
    documentation_burden_internalization,
    'To what extent is the suppression (0.72) structural (external IRS enforcement) vs. internalized (taxpayers have accepted the compliance regime as legitimate and maintain documentation practices voluntarily)?',
    'Post-removal scenario: if the IRS announced abandonment of strict documentation requirements and switched to permissive interpretation, would passive investors continue maintaining contemporaneous records, or would documentation practices collapse? Real-world signal: observe whether voluntary compliance with documentation standards persists when enforcement slack is visible.',
    'If internalized: the suppression is lower than measured (it persists even without enforcement). If structural: the suppression reflects active enforcement and would drop sharply if enforcement ceased. The reclassification would affect whether the constraint is sustainable long-term.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(documentation_burden_internalization, empirical, 'Whether suppression is maintained by external enforcement or has become internalized norm.').

omega_variable(
    kernel_reading_contest_framing,
    'Is the contest between strict and permissive readings a genuine disagreement about the best means to prevent tax-shelter abuse (both readings claim fidelity to the same Congressional anti-shelter intent), or are the readings grounded in fundamentally opposed axioms about the relationship between government revenue and private income security?',
    'Textual analysis of Treasury regulatory guidance, IRS Chief Counsel memoranda, and Congressional record: do both readings cite the same anti-shelter legislative intent, or do they invoke different foundational premises about what counts as legitimate tax-benefit design?',
    'If the readings coexist within a shared frame (both anti-shelter, disagreeing on means): the committer structure is coexists_with, and regulatory negotiation or legislative compromise could produce a middle reading. If the readings rest on opposed axioms: the committer structure is forecloses (the readings cannot both be true in a single framework), and the contest is irreducible to technocratic compromise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_framing, conceptual, 'Whether the strict/permissive contest is a disagreement about means (within shared anti-shelter frame) or reflects opposed foundational axioms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irc__tr_t0, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(irc__tr_t0, observed).
narrative_ontology:measurement(irc__tr_t5, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 5, 0.31).
narrative_ontology:measurement_basis(irc__tr_t5, observed).
narrative_ontology:measurement(irc__tr_t10, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 10, 0.34).
narrative_ontology:measurement_basis(irc__tr_t10, observed).
narrative_ontology:measurement(irc__tr_t15, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement_basis(irc__tr_t15, observed).
narrative_ontology:measurement(irc__tr_t20, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement_basis(irc__tr_t20, observed).
narrative_ontology:measurement(irc__tr_t25, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(irc__tr_t25, observed).
narrative_ontology:measurement(irc__tr_t30, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(irc__tr_t30, observed).
narrative_ontology:measurement(irc__tr_t40, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(irc__tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(irc__be_t0, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(irc__be_t0, observed).
narrative_ontology:measurement(irc__be_t5, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 5, 0.61).
narrative_ontology:measurement_basis(irc__be_t5, observed).
narrative_ontology:measurement(irc__be_t10, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 10, 0.64).
narrative_ontology:measurement_basis(irc__be_t10, observed).
narrative_ontology:measurement(irc__be_t15, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement_basis(irc__be_t15, observed).
narrative_ontology:measurement(irc__be_t20, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(irc__be_t20, observed).
narrative_ontology:measurement(irc__be_t25, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(irc__be_t25, observed).
narrative_ontology:measurement(irc__be_t30, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(irc__be_t30, observed).
narrative_ontology:measurement(irc__be_t40, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(irc__be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(irc__su_t0, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(irc__su_t0, observed).
narrative_ontology:measurement(irc__su_t5, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement_basis(irc__su_t5, observed).
narrative_ontology:measurement(irc__su_t10, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 10, 0.67).
narrative_ontology:measurement_basis(irc__su_t10, observed).
narrative_ontology:measurement(irc__su_t15, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement_basis(irc__su_t15, observed).
narrative_ontology:measurement(irc__su_t20, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(irc__su_t20, observed).
narrative_ontology:measurement(irc__su_t25, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(irc__su_t25, observed).
narrative_ontology:measurement(irc__su_t30, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(irc__su_t30, observed).
narrative_ontology:measurement(irc__su_t40, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(irc__su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(irc_469_material_participation_kernel__strict_gatekeeper_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.12).
narrative_ontology:affects_constraint(irc_469_material_participation_kernel__strict_gatekeeper_reading, irc_469_material_participation_kernel__strategic_shelter_reading).

% DUAL FORMULATION NOTE:
% The IRC 469 material participation kernel decomposes into two structurally distinct constraint stories: strict_gatekeeper_reading (this story) and strategic_shelter_reading. Both instantiate the same statutory kernel (IRC 469(b) and Treasury Regulation § 1.469-5T) but operationalize 'material participation' differently, yielding different ε values, beneficiary/victim structures, and classifications. The strict reading extracts from passive investors through high documentation bars and narrow qualifying thresholds; the permissive reading extracts from the tax authority through aggressive loss-offsetting. The two readings coexist as live positions held by different parties (Treasury/IRS holding strict, passive investor advocates holding permissive); neither logically forecloses the other within a shared anti-shelter frame, though they compete for regulatory authority. The stories are linked via network.affects_constraints to enable constraint-family analysis and cross-reading comparison.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(irc_469_material_participation_kernel__strict_gatekeeper_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
