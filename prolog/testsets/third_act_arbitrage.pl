% ============================================================================
% CONSTRAINT STORY: third_act_arbitrage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_third_act_arbitrage, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: third_act_arbitrage
 *   human_readable: Gray Divorce as Third-Act Self-Actualization Exit
 *   domain: family_sociology/demography/gender_studies
 *
 * SUMMARY:
 *   Gray divorce—dissolution of marriages among couples 50 and older—has
 *   risen sharply since 1990, even as overall divorce rates declined. The
 *   phenomenon is framed in popular discourse as empowered
 *   self-actualization: individuals reclaiming their 'third act' after
 *   decades of compromise. Increased longevity creates a 20-40 year horizon
 *   that makes starting over seem viable. Women's financial autonomy provides
 *   material exit capacity. But the coordination function (escape from
 *   genuinely harmful marriages) coexists with substantial extraction:
 *   household wealth optimized for one unit is split into two less-efficient
 *   households, often leaving both parties financially precarious. The
 *   lower-earning spouse, typically the wife who reduced career investment
 *   for caregiving, bears disproportionate costs. Adult children lose the
 *   stable family home as a psychological anchor. The narrative
 *   apparatus—divorce professionals, cultural commentators, self-help
 *   literature—benefits from volume and frames exit as courage rather than
 *   examining whether the marriage could be renegotiated. The constraint is
 *   claimed as tangled_rope because it genuinely coordinates exit for trapped
 *   individuals while extracting from financial security and relationship
 *   continuity for those whose marriages were functional but culturally
 *   delegitimized.
 *
 * KEY AGENTS:
 *   - divorcing_individuals_seeking_fulfillment: Primary beneficiaries (moderate/mobile) — exit long-term marriages for self-actualization, bear financial costs but prioritize psychological liberation
 *   - lower_earning_spouse: Primary victim (powerless/trapped) — faces asset division that does not compensate for lost earning potential, re-enters workforce with resume gap into precarious work
 *   - financial_security_of_both_parties: Structural victim (non-agent) — household wealth split, retirement savings fund two households, economies of scale vanish
 *   - adult_children_navigating_split: Secondary victims (moderate/constrained) — lose stable family home, navigate dual obligations and parental dating lives during their own life-building years
 *   - divorce_industry_professionals: Secondary beneficiaries (organized/mobile) — attorneys, mediators, therapists, financial planners serving gray divorce market
 *   - cultural_commentators: Agenda setters (organized/mobile) — produce 'never too late' narrative, amplify reinvention stories while underreporting precarity
 *   - family_researchers: Analytical observers (analytical/analytical) — document both identity reconstruction and financial damage, attempt to separate coordination from extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(third_act_arbitrage, 0.58).
domain_priors:suppression_score(third_act_arbitrage, 0.42).
domain_priors:theater_ratio(third_act_arbitrage, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(third_act_arbitrage, extractiveness, 0.58).
narrative_ontology:constraint_metric(third_act_arbitrage, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(third_act_arbitrage, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(third_act_arbitrage, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(third_act_arbitrage, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(third_act_arbitrage, tangled_rope).
narrative_ontology:human_readable(third_act_arbitrage, "Gray Divorce as Third-Act Self-Actualization Exit").
narrative_ontology:topic_domain(third_act_arbitrage, "family_sociology/demography/gender_studies").

domain_priors:requires_active_enforcement(third_act_arbitrage).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(third_act_arbitrage, divorcing_individuals_seeking_fulfillment).
narrative_ontology:constraint_victim(third_act_arbitrage, financial_security_of_both_parties).
narrative_ontology:constraint_victim(third_act_arbitrage, adult_children_navigating_split).
narrative_ontology:constraint_victim(third_act_arbitrage, lower_earning_spouse).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(third_act_arbitrage, divorce_industry_professionals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exit long-term marriages after children are grown, citing decades of accumulated dissatisfaction and the prospect of 20-40 remaining years. Frame the divorce as reclaiming autonomy and pursuing authentic self-expression. Benefit from increased longevity creating a viable 'third act' timeframe and from reduced social stigma around late-life divorce. Bear financial costs but prioritize psychological liberation over economic optimization.
narrative_ontology:constraint_stakeholder(third_act_arbitrage, divorcing_individuals_seeking_fulfillment, beneficiary,
    moderate, biographical, mobile, national).

% Household wealth accumulated over decades is split, often unevenly. Retirement savings designed for one household now fund two. Neither party achieves the standard of living the joint household would have sustained. Housing costs double, healthcare premiums rise, economies of scale vanish. The financial damage is structural and irreversible for the remaining lifespan.
narrative_ontology:constraint_stakeholder(third_act_arbitrage, financial_security_of_both_parties, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_non_agent(third_act_arbitrage, financial_security_of_both_parties).

% Typically the wife in heterosexual marriages, having reduced career investment for caregiving. Faces asset division that does not compensate for lost earning potential. Alimony is time-limited or absent in many jurisdictions. Re-entering the workforce at 55-65 with a resume gap yields low-wage, precarious work. The 'freedom' narrative does not map to her material reality.
narrative_ontology:constraint_stakeholder(third_act_arbitrage, lower_earning_spouse, payer,
    powerless, biographical, trapped, local).

% Lose the stable family home as a psychological anchor. Navigate dual holiday obligations, parental dating lives, and requests for emotional support during their own life-building years. Often pressured to take sides or mediate. The parental self-actualization project externalizes its emotional costs onto them.
narrative_ontology:constraint_stakeholder(third_act_arbitrage, adult_children_navigating_split, payer,
    moderate, biographical, constrained, local).

% Attorneys, mediators, therapists, financial planners, and life coaches who serve the gray divorce market. Benefit from the volume of late-life dissolutions and the complexity of dividing decades of assets. Provide real coordination services but also have a financial interest in framing divorce as empowerment rather than examining whether the marriage could be renegotiated.
narrative_ontology:constraint_stakeholder(third_act_arbitrage, divorce_industry_professionals, beneficiary,
    organized, biographical, mobile, national).

% Journalists, authors, and influencers who produce the 'never too late to start over' narrative. Amplify stories of post-divorce reinvention while underreporting financial precarity and regret. Shape the cultural script that makes gray divorce legible as self-actualization rather than failure. Benefit from audience engagement with aspirational content.
narrative_ontology:constraint_stakeholder(third_act_arbitrage, cultural_commentators, agenda_setter,
    organized, biographical, mobile, national).

% Study gray divorce rates, motivations, and outcomes through longitudinal surveys and qualitative interviews. Document both the identity reconstruction narratives and the financial damage. Attempt to separate the coordination function (exit from genuinely harmful marriages) from the extraction (exit from salvageable marriages driven by cultural permission and sunk-cost aversion).
narrative_ontology:constraint_stakeholder(third_act_arbitrage, family_researchers, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a socially legitimate exit path from marriages that have become psychologically untenable or abusive, particularly for women who lacked financial autonomy in earlier decades. Solves the problem of being trapped in a relationship that no longer serves either party's wellbeing.
% TRANSFER_FUNCTION: Moves accumulated household wealth from a joint optimized structure into two separate, less efficient households. Transfers emotional labor and mediation costs to adult children. Moves social permission and narrative framing from cultural commentators and divorce professionals to divorcing individuals, who pay in financial security and relationship continuity.
% ABSENT_VOICES: The lower-earning spouse's material reality is structurally underrepresented in the empowerment narrative. Adult children's costs are dismissed as temporary adjustment rather than lasting loss. Couples who successfully renegotiated long marriages are absent from the discourse, which treats divorce as the only path to authenticity.
% DISAPPEARANCE_RATIONALE: If the cultural permission structure and professional apparatus vanished, many borderline-decision divorces would not occur. Couples would face the sunk-cost question without the 'third act' framing. Divorce rates among 50+ would drop, household wealth would remain consolidated, and the industry serving gray divorce would contract. The world rearranges because the constraint actively shapes decisions, not merely reflects them.
% FOUNDING_PROBLEM: Mid-20th century marriage norms trapped individuals, especially women, in lifelong unions regardless of compatibility or abuse. Divorce carried severe social stigma and economic ruin. No-fault divorce laws and women's workforce participation created the structural conditions for exit, but cultural narratives lagged.
% FOUNDING_PROBLEM_CORROBORATION: Family historians and feminist scholars attest that the founding problem (entrapment in harmful marriages) was real and severe. However, contemporary family researchers and financial planners document that the current gray divorce pattern includes many exits from functional marriages driven by cultural permission rather than abuse or incompatibility. The coordination function persists for genuinely trapped individuals, but the extraction function (financial damage from exits that serve narrative self-actualization more than material wellbeing) has layered on top.
narrative_ontology:disappearance_verdict(third_act_arbitrage, world_rearranges).
narrative_ontology:founding_problem_status(third_act_arbitrage, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(third_act_arbitrage, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-06-24',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(third_act_arbitrage, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(third_act_arbitrage_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(third_act_arbitrage, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(third_act_arbitrage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate-high (0.58) because the financial damage is structural and irreversible: splitting decades of accumulated wealth into two households destroys economies of scale and leaves both parties worse off than the joint household would have been. The extraction is higher for the lower-earning spouse, who cannot recover lost career investment. Suppression is moderate (0.42) and declining: early in the interval, social stigma and economic dependency kept unhappy couples together; by interval end, cultural permission and women's workforce participation have reduced the suppressive force, but financial constraints still bind many. Theater ratio is low-moderate (0.28): the 'empowerment' narrative is partly performative (underreporting regret and precarity), but the identity reconstruction is real for many. The measurement series shows extraction rising as the cultural apparatus matures and suppression falling as exit becomes more accessible. Accessibility collapse is moderate (0.48): alternatives to divorce (renegotiating the marriage, accepting dissatisfaction, therapy) remain conceptually available but are culturally delegitimized by the 'third act' framing. Resistance is moderate (0.52): financial advisors, some family therapists, and adult children push back against the empowerment narrative, but the cultural momentum is strong.
 *
 * PERSPECTIVAL GAP:
 *   From the divorcing individual's seat, the constraint operates as coordination: it provides cultural permission and professional support to exit an unsatisfying marriage, solving the sunk-cost trap. From the lower-earning spouse's seat, the same structure operates as extraction: the 'empowerment' narrative does not map to her material reality of financial precarity and lost security. From the adult children's seat, it is extraction of emotional labor and loss of family stability. From the divorce industry's seat, it is a market they serve and benefit from. The engine should compute these seats differently: the divorcing individual as rope-leaning (coordination with moderate cost), the lower-earning spouse as snare (trapped, extracted from), adult children as tangled_rope (coordinated into a role they did not choose, bearing real costs), and the industry as beneficiary of the overall structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Divorcing individuals seeking fulfillment are beneficiaries (d near 0.2-0.3): they gain psychological liberation and narrative validation, though they pay financially. Their mobile exit options and moderate power mean they experience the constraint as enabling rather than extractive. The lower-earning spouse is a full target (d near 0.9): trapped by lost career investment and precarious re-entry prospects, she bears the extraction with minimal agency. Financial security (non-agent) is structurally targeted. Adult children are secondary targets (d near 0.6-0.7): constrained exit (cannot refuse to navigate the split), moderate power, bearing emotional costs they did not choose. Divorce industry professionals are beneficiaries (d near 0.1-0.2): organized power, mobile exit, they collect from the volume. Cultural commentators are agenda setters with beneficiary directionality (d near 0.2): they shape the narrative and benefit from engagement. Family researchers are analytical observers (d = 0.5): symmetric, studying the structure without being extracted from or subsidized by it.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (provide exit from genuinely harmful marriages) has not outlived its function—abusive and incompatible marriages still exist. But the extraction function (financial damage from exits driven by cultural permission rather than material harm) has layered on top. The coordination and extraction are inseparable in practice: the same legal and cultural apparatus that enables escape from abuse also enables exits from functional marriages. This is the tangled_rope signature: you cannot remove the extraction without also removing the coordination. The mandate is live but the structure has accumulated extractive overhead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    salvageability_threshold,
    'What proportion of gray divorces exit genuinely unsalvageable marriages versus marriages that could have been renegotiated with therapeutic or cultural support?',
    'Longitudinal studies tracking marital satisfaction, therapy engagement, and post-divorce regret. Comparison of divorce rates in cohorts with strong vs. weak cultural permission for late-life exit.',
    'If most gray divorces exit salvageable marriages, the extraction function dominates and the constraint is closer to snare. If most exit unsalvageable marriages, the coordination function dominates and the constraint is closer to rope. The tangled_rope classification assumes both functions are substantial.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(salvageability_threshold, empirical, 'Whether gray divorces primarily exit unsalvageable or salvageable marriages.').

omega_variable(
    narrative_vs_material_wellbeing,
    'Do post-divorce identity reconstruction narratives reflect genuine psychological improvement, or are they performative adaptations to justify a costly decision?',
    'Comparison of self-reported wellbeing in qualitative interviews versus objective measures (mental health diagnoses, substance use, social isolation, financial stress). Longitudinal tracking of narrative stability over 5-10 years post-divorce.',
    'If the narratives are performative, the theater ratio is higher and the extraction is more severe. If they reflect genuine improvement, the coordination function is stronger and the beneficiary experience is real rather than compensatory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(narrative_vs_material_wellbeing, empirical, 'Whether post-divorce reinvention narratives reflect genuine or performative wellbeing.').

omega_variable(
    gendered_extraction_asymmetry,
    'Does the financial extraction fall disproportionately on women due to career interruption, or has women''s workforce participation equalized the damage?',
    'Stratified analysis of post-divorce financial outcomes by gender, controlling for pre-divorce earning patterns and asset division. Comparison across cohorts with different female labor force participation rates.',
    'If extraction is still gendered, the lower-earning spouse victim category is primarily women and the constraint reproduces gender inequality. If extraction is equalized, both parties are victims symmetrically and the constraint is less structurally gendered.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gendered_extraction_asymmetry, empirical, 'Whether financial extraction from gray divorce is gendered or symmetric.').

omega_variable(
    longevity_horizon_counterfactual,
    'Would gray divorce rates be substantially lower if life expectancy were 10-15 years shorter, removing the ''third act'' timeframe?',
    'Historical comparison of late-life divorce rates across cohorts with different life expectancies. Cross-national comparison of gray divorce rates in countries with different longevity profiles, controlling for legal and cultural factors.',
    'If the longevity horizon is causally necessary, the constraint is downstream of the longevity_mismatch mountain and the extraction is partly an artifact of extended lifespan. If gray divorce rates are insensitive to longevity, the cultural permission structure is the primary driver.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(longevity_horizon_counterfactual, empirical, 'Whether increased longevity is causally necessary for gray divorce rates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(third_act_arbitrage, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(thir_tr_t0, third_act_arbitrage, theater_ratio, 0, 0.15).
narrative_ontology:measurement(thir_tr_t5, third_act_arbitrage, theater_ratio, 5, 0.18).
narrative_ontology:measurement(thir_tr_t10, third_act_arbitrage, theater_ratio, 10, 0.21).
narrative_ontology:measurement(thir_tr_t15, third_act_arbitrage, theater_ratio, 15, 0.23).
narrative_ontology:measurement(thir_tr_t20, third_act_arbitrage, theater_ratio, 20, 0.25).
narrative_ontology:measurement(thir_tr_t25, third_act_arbitrage, theater_ratio, 25, 0.27).
narrative_ontology:measurement(thir_tr_t30, third_act_arbitrage, theater_ratio, 30, 0.28).

% Extraction over time
narrative_ontology:measurement(thir_be_t0, third_act_arbitrage, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(thir_be_t5, third_act_arbitrage, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(thir_be_t10, third_act_arbitrage, base_extractiveness, 10, 0.47).
narrative_ontology:measurement(thir_be_t15, third_act_arbitrage, base_extractiveness, 15, 0.51).
narrative_ontology:measurement(thir_be_t20, third_act_arbitrage, base_extractiveness, 20, 0.54).
narrative_ontology:measurement(thir_be_t25, third_act_arbitrage, base_extractiveness, 25, 0.56).
narrative_ontology:measurement(thir_be_t30, third_act_arbitrage, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(thir_su_t0, third_act_arbitrage, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(thir_su_t5, third_act_arbitrage, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(thir_su_t10, third_act_arbitrage, suppression_requirement, 10, 0.49).
narrative_ontology:measurement(thir_su_t15, third_act_arbitrage, suppression_requirement, 15, 0.46).
narrative_ontology:measurement(thir_su_t20, third_act_arbitrage, suppression_requirement, 20, 0.44).
narrative_ontology:measurement(thir_su_t25, third_act_arbitrage, suppression_requirement, 25, 0.43).
narrative_ontology:measurement(thir_su_t30, third_act_arbitrage, suppression_requirement, 30, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(third_act_arbitrage, identity_coordination).
narrative_ontology:boltzmann_floor_override(third_act_arbitrage, 0.12).

% DUAL FORMULATION NOTE:
% This constraint is downstream of three others: longevity_mismatch (mountain) provides the extended timeframe that makes 'starting over' viable; empty_shell_tolerance (rope) is the prior coordination equilibrium this constraint disrupts; womens_financial_autonomy (rope) provides the material exit capacity. The constraint family models how a genuine coordination function (exit from harmful marriages) accumulates extractive overhead (financial damage from exits driven by cultural permission) as the cultural apparatus matures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(third_act_arbitrage, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
