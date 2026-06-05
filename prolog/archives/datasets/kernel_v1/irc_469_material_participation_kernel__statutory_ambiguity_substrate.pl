% ============================================================================
% CONSTRAINT STORY: irc_469_material_participation_kernel__statutory_ambiguity_substrate
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_irc_469_material_participation_kernel__statutory_ambiguity_substrate, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: irc_469_material_participation_kernel__statutory_ambiguity_substrate
 *   human_readable: IRC §469 Material Participation Statutory Ambiguity
 *   domain: tax_law/real_estate_investment/regulatory_interpretation
 *
 * SUMMARY:
 *   IRC §469 (the passive activity loss limitation rule, enacted in 1986 as
 *   part of the Tax Reform Act) creates a structural regime that limits
 *   deductions for losses from 'passive activities' (generally rental real
 *   estate and other investments where the taxpayer does not materially
 *   participate in operations). The regime's core constraint is the statutory
 *   delegation to the Treasury to define 'material participation' through
 *   regulations. This delegation preserves strategic ambiguity: Congress
 *   could have codified a clear bright-line standard (e.g., '500 hours of
 *   participation, measured quarterly, documented through contemporaneous
 *   records'), but instead delegated to the Treasury, which issued
 *   regulations containing both safe harbors and a fact-and-circumstance test
 *   that preserves interpretive discretion. The result is a tangled_rope
 *   constraint that simultaneously coordinates legitimate anti-tax-shelter
 *   objectives AND extracts compliance and advisory costs from small
 *   investors. The ambiguity creates recurring billable hours for tax
 *   professionals, audit risk for taxpayers, and ongoing revenue for the
 *   Treasury (estimated $1-2B annually from passive loss disallowance). This
 *   constraint is ONE READING of the contested kernel: the statutory
 *   delegation itself and what counts as material participation. This reading
 *   instantiates the substrate-level statutory ambiguity as a constructed
 *   feature (not an inherent limitation), positioning it as a false-summit
 *   candidate when viewed from the analytical/civilizational perspective.
 *
 * KEY AGENTS:
 *   - Small Real Estate Investors: Primary victims (powerless/trapped) — bear passive loss disallowance during audit verification periods; cannot reliably predict participation status; face statute-of-limitations risk
 *   - Tax Professionals (CPAs, Tax Attorneys): Mixed beneficiaries/victims (moderate/constrained) — benefit from consulting demand created by ambiguity; constrained by malpractice liability and disciplinary exposure
 *   - Treasury/IRS Revenue System: Primary beneficiary (institutional/arbitrage) — derives $1-2B annually from passive loss limitation regime; controls audit enforcement and interpretive discretion
 *   - Real Estate Industry Coalition (NAREIT, investor advocacy groups): Organized actors (organized/constrained) — see the constraint as temporary (proposed Safe Harbor Amendment for aggregation rule); coordinating legislative remedy pathways
 *   - Tax Regulation Ritual (IRS Guidance, Revenue Procedures, Safe Harbors): Institutional performance mechanism (institutional/arbitrage) — maintains formal procedural legitimacy (safe harbor tests, 500-hour rule, significance test) while preserving discretionary enforcement through material participation fact-and-circumstance analysis
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the ambiguity as inherent to statutory delegation; false summit risk when analyzing whether the indeterminacy could have been reduced through clearer statutory language or regulatory bright-line rules
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(irc_469_material_participation_kernel__statutory_ambiguity_substrate, 0.58).
domain_priors:suppression_score(irc_469_material_participation_kernel__statutory_ambiguity_substrate, 0.65).
domain_priors:theater_ratio(irc_469_material_participation_kernel__statutory_ambiguity_substrate, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__statutory_ambiguity_substrate, extractiveness, 0.58).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__statutory_ambiguity_substrate, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__statutory_ambiguity_substrate, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(irc_469_material_participation_kernel__statutory_ambiguity_substrate, tangled_rope).
narrative_ontology:human_readable(irc_469_material_participation_kernel__statutory_ambiguity_substrate, "IRC §469 Material Participation Statutory Ambiguity").
narrative_ontology:topic_domain(irc_469_material_participation_kernel__statutory_ambiguity_substrate, "tax_law/real_estate_investment/regulatory_interpretation").

domain_priors:requires_active_enforcement(irc_469_material_participation_kernel__statutory_ambiguity_substrate).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(irc_469_material_participation_kernel__statutory_ambiguity_substrate, '6cbf4208-9bd3-4552-949e-233d69c48fe4').
narrative_ontology:cs_kernel_codification('6cbf4208-9bd3-4552-949e-233d69c48fe4', formalized).
narrative_ontology:cs_authority_grounding('6cbf4208-9bd3-4552-949e-233d69c48fe4', extraction).
narrative_ontology:cs_interpretation_layer_present('6cbf4208-9bd3-4552-949e-233d69c48fe4').
narrative_ontology:cs_axiom('6cbf4208-9bd3-4552-949e-233d69c48fe4', foundational, statutory_ambiguity_regulatory_choice).
narrative_ontology:cs_axiom_status(statutory_ambiguity_regulatory_choice, holdable).
narrative_ontology:cs_axiom_grounding('6cbf4208-9bd3-4552-949e-233d69c48fe4', statutory_ambiguity_regulatory_choice, empirically_contingent).
narrative_ontology:cs_axiom('6cbf4208-9bd3-4552-949e-233d69c48fe4', foundational, ambiguity_preserves_enforcement_discretion).
narrative_ontology:cs_axiom_status(ambiguity_preserves_enforcement_discretion, holdable).
narrative_ontology:cs_axiom_grounding('6cbf4208-9bd3-4552-949e-233d69c48fe4', ambiguity_preserves_enforcement_discretion, empirically_contingent).
narrative_ontology:cs_created_at('6cbf4208-9bd3-4552-949e-233d69c48fe4', '').
narrative_ontology:cs_kernel_id(irc_469_material_participation_kernel__statutory_ambiguity_substrate, irc_469_material_participation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__statutory_ambiguity_substrate, passive_loss_limitation_regime).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__statutory_ambiguity_substrate, treasury_tax_revenue_preservation).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__statutory_ambiguity_substrate, real_estate_investors).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__statutory_ambiguity_substrate, small_landlords).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__statutory_ambiguity_substrate, regulatory_interpretive_certainty).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL REAL ESTATE INVESTOR (SNARE) — Structurally trapped. Cannot exit the §469 regime (passive activity rules apply automatically to investment real estate), cannot reliably determine material participation status (standards are interpretive and subjective), and faces maximum suppression: IRS enforcement discretion, statute-of-limitations risk, and audit exposure create barriers to challenging adverse determinations. The investor bears full extraction cost during the verification period (often 3-7 years before audit, during which passive loss disallowance accumulates).
constraint_indexing:constraint_classification(irc_469_material_participation_kernel__statutory_ambiguity_substrate, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: TAX PROFESSIONAL (TANGLED ROPE) — Faces mixed coordination and extraction. Benefits from ambiguity through consulting fees (clients require expert interpretation); constrained by liability exposure (errors result in malpractice claims or disciplinary action). The constraint does coordinate: standardized tax reporting positions enable efficient filing and audit defense. But extraction exists: the ambiguity creates recurring billable hours and locks clients into professional dependency for compliance.
constraint_indexing:constraint_classification(irc_469_material_participation_kernel__statutory_ambiguity_substrate, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TREASURY/IRS REVENUE SYSTEM (ROPE) — Benefits substantially from the ambiguity. Passive loss limitation rules generate approximately $1-2B annually in net tax revenue relative to a world with clearer participation standards. The constraint coordinates legitimate anti-abuse objectives (preventing unlimited passive loss shelter strategies). IRS has high arbitrage: it can litigate interpretations, issue guidance, and modify enforcement posture without structural constraint.
constraint_indexing:constraint_classification(irc_469_material_participation_kernel__statutory_ambiguity_substrate, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REAL ESTATE INDUSTRY COALITION (SCAFFOLD) — Organized agents (NAREIT, real estate advocacy groups, investor coalitions) perceive the constraint as a temporary coordination failure with a sunset clause embedded in the tax legislative process. Proposed Safe Harbor Amendment (allowing investors to aggregate rental real estate under certain conditions) is a structural exit pathway. Low effective extraction for organized actors because they have agency and see legislative remedy pathways with estimated 5-10 year horizon.
constraint_indexing:constraint_classification(irc_469_material_participation_kernel__statutory_ambiguity_substrate, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: TAX REGULATION RITUAL (PITON) — The regulations (Treas. Reg. §1.469-5T) and IRS guidance (Rev. Proc. 2016-40, Notice 2016-18) are substantially performative: they codify safe harbors and tests (500-hour rule, significance test, participation fact-and-circumstance analysis) that provide procedural legitimacy while preserving discretionary enforcement through the 'material participation' standard's inherent flexibility. The ritual persists through institutional inertia and mutual institutional interest (IRS enforcement discretion, tax professional consulting demand), not because the safe harbors reliably predict audit outcomes. Theater ratio 0.68 reflects that formal guidance exists but does not resolve the core ambiguity.
constraint_indexing:constraint_classification(irc_469_material_participation_kernel__statutory_ambiguity_substrate, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal analytical perspective, some statutory ambiguity in tax law is inherent to legislation: complex economic behavior always outpaces statutory language, and verification of subjective activities (like 'material participation' in real estate management) is inherently difficult. This perspective sees the ambiguity as a structural feature of how tax regimes must function. However, the structural data contradicts this mountain classification — the engine will identify this as a false summit, revealing that the ambiguity is partially constructed: Congress could have chosen clearer standards or broader safe harbors, but chose strategic ambiguity to preserve administrative flexibility and revenue.
constraint_indexing:constraint_classification(irc_469_material_participation_kernel__statutory_ambiguity_substrate, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(irc_469_material_participation_kernel__statutory_ambiguity_substrate_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(irc_469_material_participation_kernel__statutory_ambiguity_substrate, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(irc_469_material_participation_kernel__statutory_ambiguity_substrate, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(irc_469_material_participation_kernel__statutory_ambiguity_substrate, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(irc_469_material_participation_kernel__statutory_ambiguity_substrate, TR),
    TR >= 0.70.

:- end_tests(irc_469_material_participation_kernel__statutory_ambiguity_substrate_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts compliance costs (professional advisory fees, audit preparation expenses, litigation risk), audit and statute-of-limitations costs (passive loss disallowances that persist for 3-7 year audit windows before resolution), and revenue leakage prevention (the regime preserves Treasury tax revenue that would be lost to passive loss sheltering strategies). The extraction is not maximal (like a pure snare) because legitimate anti-abuse purposes exist: preventing unlimited passive loss shelter strategies is a real coordination objective. Over the measurement interval (0-20 years, representing a multi-decade period from 1986 onwards), extractiveness has increased from 0.42 to 0.58, reflecting the accumulation of case law, regulatory guidance complexity, and IRS enforcement intensity that has made the ambiguity harder to navigate. Suppression (0.65): Moderate-high. Multiple suppression mechanisms operate: (1) IRS interpretive authority and audit discretion (taxpayers cannot easily challenge IRS positions without litigation, which is expensive); (2) statute-of-limitations asymmetry (taxpayers must defend positions during the audit period, often 3-7 years, during which passive loss disallowance accumulates); (3) information asymmetry (IRS has superior information about audit enforcement patterns and litigation success rates); (4) exit barriers (the §469 regime applies automatically to rental real estate; investors cannot opt out). Theater ratio (0.68): Moderate-high. The safe harbor tests (500-hour rule, significance test, participation fact-and-circumstance analysis) provide procedural legitimacy and formal guidance, but do not reliably predict audit outcomes. Taxpayers can satisfy the safe harbors and still face successful IRS audit challenges based on the fact-and-circumstance test. The guidance is performative: it signals compliance pathways while preserving enforcement discretion. Theater has increased over the interval as regulatory guidance has accumulated without substantially reducing audit risk.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a substantial perspectival gap. The Treasury/IRS sees a coordination mechanism (Rope) — the passive loss limitation rule prevents tax-shelter abuse and enables sensible revenue collection. The real estate industry coalition sees a temporary problem with a legislative exit (Scaffold) — the proposed Safe Harbor Amendment would clarify participation standards and enable realty investors to aggregate rental properties. The tax professional sees mixed coordination and extraction (Tangled Rope) — ambiguity creates consulting demand and professional dependency, but also enables legitimate audit defense and standardized compliance positions. The small investor sees pure extraction and suppression (Snare) — passive loss disallowance accumulates during audit periods with no reliable predictability, and the investor has minimal agency or exit options. The regulatory ritual (safe harbors, guidance documents, revenue procedures) sees its own performative character (Piton) — the formal structure persists through institutional inertia and mutual institutional interest, not because it reliably reduces participation determination disputes. The civilizational analytical observer risks naturalizing the ambiguity (Mountain) — 'all statutory delegations create interpretive ambiguity' — but the structural data reveals this as a false summit: Congress could have chosen clearer standards (mandatory bright-line rules, safe harbor expansion, or explicit materiality thresholds), but chose strategic ambiguity to preserve IRS enforcement flexibility and revenue.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from each agent's structural position relative to the constraint. The Treasury/IRS (beneficiary with arbitrage exit) derives low d (~0.15): they benefit from the ambiguity and have full interpretive discretion, so they experience negative effective extraction (f(d) ≈ -0.01). The small investor (victim with trapped exit) derives high d (~0.95): they bear maximum cost and have no meaningful exit options, so they experience maximum effective extraction (f(d) ≈ 1.42). The tax professional (mixed beneficiary/victim with constrained exit) derives moderate d (~0.60): they benefit from consulting demand but are constrained by liability exposure, so they experience moderate effective extraction (f(d) ≈ 0.80). The organized coalition (victim with constrained exit but collective agency) derives moderate-low d (~0.45): they have some organizational power and see an exit pathway (legislative amendment), so they experience lower effective extraction (f(d) ≈ 0.40). The analytical observer (observer position) derives d ≈ 0.72, producing f(d) ≈ 1.15, which reflects that the observer position sees the full structure of the constraint but is outside the immediate extraction flow.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by clarifying that different perspectives capture different structural aspects of the same phenomenon. The Treasury's Rope perspective is correct about the coordination function (the regime does prevent tax-shelter abuse). The investor's Snare perspective is correct about the extraction asymmetry (ambiguity creates audit risk and compliance costs borne disproportionately by small investors). The industry coalition's Scaffold perspective is correct about the legislative exit pathway (the Safe Harbor Amendment represents a real structural option). The regulatory ritual's Piton perspective is correct about the performative character (safe harbors do not reliably predict outcomes). The analytical observer's risk of a Mountain classification is a false summit: the ambiguity is not inherent to statutory delegation; it is a constructed regulatory choice. The tangled_rope classification at the main analytical level captures the hybrid structure: genuine coordination function (preventing abuses) + asymmetric extraction (audit risk and compliance costs disproportionately borne by small investors). The constraint is not purely extractive, but the extraction is real and embedded in the coordination mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    participation_standard_measurability,
    'Is ''material participation'' in real estate operations inherently unmeasurable (natural law), or is the unmeasurability a regulatory choice to preserve enforcement discretion?',
    'Comparative institutional analysis: examine tax regimes in other jurisdictions (UK, Canada, Australia) with clearer participation standards; assess whether IRS audit outcomes correlate with declared participation activities or with taxpayer characteristics (income level, professional status, audit selection criteria)',
    'If inherently unmeasurable: mountain classification confirmed; ambiguity is structural cost of verifying subjective participation. If regulatory choice: false summit confirmed; the ambiguity is constructed to preserve IRS flexibility and revenue.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(participation_standard_measurability, empirical, 'Whether participation standard unmeasurability is inherent or constructed').

omega_variable(
    treasury_revenue_dependency,
    'What proportion of the passive loss limitation regime''s estimated $1-2B annual revenue depends specifically on the ambiguity of material participation standards (rather than on the existence of the rule itself)?',
    'IRS audit data analysis: compare sustained audit adjustments in material participation cases with audit adjustments that would occur under a clearer bright-line standard; econometric modeling of taxpayer responses to hypothetical clarity',
    'If ambiguity accounts for >40% of revenue: Treasury has material incentive to preserve ambiguity (extraction mechanism confirmed). If <20%: ambiguity is incidental to the core revenue purpose (suggests natural law perspective has more merit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treasury_revenue_dependency, empirical, 'Treasury revenue dependency on participation standard ambiguity').

omega_variable(
    professional_consulting_dependency_lock,
    'Does the statutory ambiguity create genuine professional dependency (tax professionals are necessary for compliance) or artificial dependency (ambiguity is constructed to sustain consulting demand)?',
    'Comparative analysis: audit defense outcomes for taxpayers with professional representation vs. self-represented taxpayers; correlation between jurisdictions with clearer standards and professional tax compliance costs; historical analysis of guidance clarity before and after specific enforcement actions',
    'If genuine: tangled rope classification confirmed (coordination function + extraction lock both real). If artificial: snare classification more appropriate; the coordination function is ersatz.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(professional_consulting_dependency_lock, empirical, 'Whether professional dependency is genuine or constructed').

omega_variable(
    safe_harbor_effectiveness,
    'Do the existing safe harbors (500-hour rule, significance test, participation fact-and-circumstance framework) function as genuine brightline rules, or do they collapse into discretionary interpretation upon IRS audit?',
    'Audit outcome data: correlation between taxpayers meeting safe harbor criteria and audit defense success rates; IRS litigation positions on safe harbor interpretation; frequency of IRS challenges to taxpayers claiming safe harbor compliance',
    'If effective bright lines: current regulatory theater is functional (Piton is accurate). If collapse into discretion: safe harbors are performative cover for discretionary enforcement (theater is higher; snare classification more accurate).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(safe_harbor_effectiveness, empirical, 'Whether safe harbors provide genuine bright-line protection or collapse under audit').

omega_variable(
    legislative_amendment_feasibility,
    'Is the Real Estate Industry Coalition''s proposed Safe Harbor Amendment (aggregation rule for rental real estate) structurally feasible as a legislative fix, or are there entrenched institutional barriers to clarification?',
    'Legislative history analysis: track amendment proposals since 1986 (when §469 was enacted); identify political opposition (revenue loss concerns, complexity objections, special-interest alignment); assess whether amendment follows normal legislative pathways or encounters structural blocking coalitions',
    'If feasible: scaffold perspective is structural (sunset is real, organized actors have exit pathway). If infeasible: scaffold is aspirational; the constraint persists indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legislative_amendment_feasibility, conceptual, 'Legislative feasibility of Real Estate Industry Coalition proposed amendment').

omega_variable(
    audit_rate_correlation_with_ambiguity,
    'Do audit rates and audit outcomes in material participation cases correlate with IRS resource availability, or with taxpayer characteristics and interpretation strategies?',
    'Time-series analysis of IRS enforcement data: correlation between audit rates, IRS staffing levels, and material participation case outcomes; analysis of whether audit patterns follow revenue-maximization logic (targeting high-income taxpayers and syndicators) or interpretive-consistency logic (enforcing the statutory standard uniformly)',
    'If resource-driven: enforcement is opportunistic and suppression is situational (snare mechanism confirmed). If interpretation-driven: enforcement follows rule-of-law logic (mountain perspective more defensible).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(audit_rate_correlation_with_ambiguity, empirical, 'Correlation between audit patterns and enforcement drivers').

omega_variable(
    kernel_statutory_reading_indeterminacy,
    'Does the statutory text itself (IRC §469(c)(7)(B): ''material participation in any activity shall be determined under regulations prescribed by the Secretary'') inherently permit multiple readings of what constitutes material participation, or is the indeterminacy created by regulatory choice?',
    'Statutory exegesis: comparative reading of §469(c)(7)(B) against analogous statutory delegations in tax law (e.g., §162(a) ordinary and necessary business expense, §183 hobby loss rule); analysis of legislative history and conference committee reports on §469 intent; assessment of whether alternative regulatory frameworks (clear safe harbors vs. fact-and-circumstance tests) would be consistent with the statutory delegation',
    'If statutory text inherently indeterminate: natural law classification supported (ambiguity is an unavoidable feature of delegation). If regulatory choice drives indeterminacy: false summit confirmed (ambiguity is constructed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_statutory_reading_indeterminacy, conceptual, 'Whether statutory indeterminacy is inherent or constructed by regulatory choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(irc_469_material_participation_kernel__statutory_ambiguity_substrate, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irc469_amb_tr_t0, irc_469_material_participation_kernel__statutory_ambiguity_substrate, theater_ratio, 0, 0.55).
narrative_ontology:measurement(irc469_amb_tr_t10, irc_469_material_participation_kernel__statutory_ambiguity_substrate, theater_ratio, 10, 0.62).
narrative_ontology:measurement(irc469_amb_tr_t20, irc_469_material_participation_kernel__statutory_ambiguity_substrate, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(irc469_amb_be_t0, irc_469_material_participation_kernel__statutory_ambiguity_substrate, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(irc469_amb_be_t10, irc_469_material_participation_kernel__statutory_ambiguity_substrate, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(irc469_amb_be_t20, irc_469_material_participation_kernel__statutory_ambiguity_substrate, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(irc469_amb_su_t0, irc_469_material_participation_kernel__statutory_ambiguity_substrate, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(irc469_amb_su_t10, irc_469_material_participation_kernel__statutory_ambiguity_substrate, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(irc469_amb_su_t20, irc_469_material_participation_kernel__statutory_ambiguity_substrate, suppression_requirement, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(irc_469_material_participation_kernel__statutory_ambiguity_substrate, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% The IRC §469 material participation kernel is decomposed into multiple structurally distinct constraints based on the ε-invariance principle. The STATUTORY AMBIGUITY SUBSTRATE reading (this constraint) focuses on the constructed ambiguity in the regulatory framework (ε=0.58). Sibling readings would have different ε values reflecting their different structural claims: the STATUTORY CLARITY reading (arguing for bright-line standards) would have lower ε (more measurable), the ADMINISTRATIVE NECESSITY reading would have lower ε (necessity justifies lower extractiveness), and the ANTI-ABUSE COORDINATION reading would have lower ε (coordination function is primary). Each reading is a separate constraint story with its own ε, perspectives, and beneficiary/victim structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(irc_469_material_participation_kernel__statutory_ambiguity_substrate, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
