% ============================================================================
% CONSTRAINT STORY: nafta_jurisdictional_boundary__capital_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nafta_jurisdictional_boundary__capital_supremacy_reading, []).

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
 *   constraint_id: nafta_jurisdictional_boundary__capital_supremacy_reading
 *   human_readable: Trade Agreement Text as Supreme Law Overriding Domestic Regulatory Standards (Capital Supremacy Reading)
 *   domain: International Trade Law / Political Economy / Regulatory Federalism
 *
 * SUMMARY:
 *   This story instantiates the capital supremacy reading of the NAFTA
 *   jurisdictional boundary kernel: the treaty text is read as supreme law
 *   that overrides domestic regulatory standards whenever they touch investor
 *   expectations, with capital mobility and regulatory harmonization treated
 *   as mandatory treaty obligations rather than negotiated policy space.
 *   Under this reading, domestic labor and environmental regulators lose
 *   effective jurisdictional authority whenever their rules threaten foreign
 *   capital's expected returns, and the resulting extraction flows upward to
 *   capital mobility beneficiaries — multinational investors, cross-border
 *   manufacturers, and the arbitration bar that adjudicates disputes under
 *   the investment chapters. This is a distinct constraint from the
 *   embedded_liberalism_reading (which holds trade obligations compatible
 *   with legitimate domestic policy space) and the
 *   sovereignty_primacy_reading (which holds domestic law supreme over treaty
 *   text) — those are separate constraints with their own ε values, not
 *   alternative measurements of this one.
 *
 * KEY AGENTS:
 *   - multinational_capital_investors: primary beneficiary (institutional/arbitrage) — collects relocation leverage and arbitration protection
 *   - cross_border_manufacturing_firms: beneficiary (institutional/arbitrage) — exploits harmonization for regulatory arbitrage
 *   - domestic_environmental_regulators: primary target (moderate/constrained) — chilled from tightening standards
 *   - unionized_manufacturing_workers: primary target (powerless/trapped) — bears relocation-threat wage suppression with no standing to claim
 *   - national_trade_negotiators: agenda-setter (institutional/analytical) — administers and could renegotiate the treaty
 *   - trade_policy_scholars: analytical observer — assesses dispute settlement record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.79).
domain_priors:suppression_score(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.72).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, extractiveness, 0.79).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__capital_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__capital_supremacy_reading, "Trade Agreement Text as Supreme Law Overriding Domestic Regulatory Standards (Capital Supremacy Reading)").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__capital_supremacy_reading, "International Trade Law / Political Economy / Regulatory Federalism").

domain_priors:requires_active_enforcement(nafta_jurisdictional_boundary__capital_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__capital_supremacy_reading, '73863bfb-f768-42d4-8fc3-98410cc02c5c').
narrative_ontology:cs_kernel_codification('73863bfb-f768-42d4-8fc3-98410cc02c5c', fixed_text).
narrative_ontology:cs_authority_grounding('73863bfb-f768-42d4-8fc3-98410cc02c5c', extraction).
narrative_ontology:cs_interpretation_layer_present('73863bfb-f768-42d4-8fc3-98410cc02c5c').
narrative_ontology:cs_reading_relation('73863bfb-f768-42d4-8fc3-98410cc02c5c', nafta_jurisdictional_boundary__embedded_liberalism_reading, coexists_with).
narrative_ontology:cs_reading_relation('73863bfb-f768-42d4-8fc3-98410cc02c5c', nafta_jurisdictional_boundary__sovereignty_primacy_reading, forecloses).
narrative_ontology:cs_axiom('73863bfb-f768-42d4-8fc3-98410cc02c5c', foundational, capital_mobility_is_treaty_guaranteed_right).
narrative_ontology:cs_axiom_status(capital_mobility_is_treaty_guaranteed_right, holdable).
narrative_ontology:cs_axiom_grounding('73863bfb-f768-42d4-8fc3-98410cc02c5c', capital_mobility_is_treaty_guaranteed_right, conventional).
narrative_ontology:cs_axiom('73863bfb-f768-42d4-8fc3-98410cc02c5c', foundational, domestic_regulatory_authority_subordinate_to_investment_protection).
narrative_ontology:cs_axiom_status(domestic_regulatory_authority_subordinate_to_investment_protection, holdable).
narrative_ontology:cs_axiom_grounding('73863bfb-f768-42d4-8fc3-98410cc02c5c', domestic_regulatory_authority_subordinate_to_investment_protection, instrumental).
narrative_ontology:cs_reference_frame('73863bfb-f768-42d4-8fc3-98410cc02c5c', post_expropriation_era_investment_protection).
narrative_ontology:cs_drift_state('73863bfb-f768-42d4-8fc3-98410cc02c5c', contemporary_regulatory_chill_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('73863bfb-f768-42d4-8fc3-98410cc02c5c', '').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__capital_supremacy_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__capital_supremacy_reading, multinational_capital_investors).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__capital_supremacy_reading, cross_border_manufacturing_firms).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__capital_supremacy_reading, investor_state_arbitration_bar).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_environmental_regulators).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_labor_standards_agencies).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, unionized_manufacturing_workers).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, local_governments_facing_preemption).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Can relocate production, invoke investor-state dispute settlement against domestic regulations that reduce expected profits, and treat the treaty text as a floor beneath which no signatory government's standards may reach without triggering compensation claims. Capital mobility across borders is treated as a treaty-guaranteed right, not a policy choice any single government can withdraw.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, multinational_capital_investors, beneficiary,
    institutional, generational, arbitrage, continental).

% Structure supply chains to exploit the harmonization requirement, locating production where domestic regulatory standards are lowest while retaining guaranteed tariff-free access to all signatory markets. The treaty text is read as making regulatory arbitrage a right rather than a byproduct.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, cross_border_manufacturing_firms, beneficiary,
    institutional, generational, arbitrage, continental).

% Litigates and adjudicates investor-state dispute claims under the treaty's investment chapters, drawing fees and precedent-setting authority from the volume and scope of claims brought against domestic regulatory changes. Has an institutional interest in reading treaty supremacy broadly.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, investor_state_arbitration_bar, beneficiary,
    organized, generational, arbitrage, continental).

% Draft and would enforce standards on emissions, chemical use, and land management, but face the credible threat that any new or tightened rule affecting a foreign investor's expected returns can be challenged as a treaty violation. Regulatory action is chilled before it is attempted; existing rules are vulnerable to being read down or compensated around.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_environmental_regulators, payer,
    moderate, biographical, constrained, national).

% Set wage floors, workplace safety rules, and organizing protections, but under the capital supremacy reading these are subordinate to the treaty's harmonization and market-access guarantees; enforcement against firms threatening relocation is politically and legally constrained.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_labor_standards_agencies, payer,
    moderate, biographical, constrained, national).

% Bear the direct cost of the capital mobility guarantee: plant relocation, wage suppression under credible relocation threats, and the loss of leverage in bargaining because employers can point to treaty-protected exit as a fallback. Cannot relocate themselves and cannot bring a treaty claim of their own — only states and investors have standing under the investment chapters.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, unionized_manufacturing_workers, payer,
    powerless, biographical, trapped, national).

% Attempt to pass procurement preferences, zoning restrictions, or local environmental ordinances, only to find such measures preempted or chilled by treaty obligations negotiated at the national level, over which they had no direct voice.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, local_governments_facing_preemption, payer,
    moderate, biographical, constrained, regional).

% Negotiate and defend the treaty text, administer accession and amendment processes, and represent their governments in dispute settlement. They set the terms under which capital mobility and harmonization obligations bind domestic regulatory authority, and could in principle renegotiate the investment chapters, but bear none of the diffuse enforcement costs themselves.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, national_trade_negotiators, agenda_setter,
    institutional, generational, analytical, continental).

% Study dispute settlement outcomes, regulatory chill effects, and the empirical record of investor-state claims to assess whether the treaty operates as a genuine market-access coordination mechanism or as an extraction channel favoring capital over domestic democratic authority.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, trade_policy_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nafta_jurisdictional_boundary__capital_supremacy_reading, multinational_capital_investors).
narrative_ontology:fixing_cost_class(nafta_jurisdictional_boundary__capital_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reduces tariff and non-tariff barriers across signatory markets and provides investors a predictable, harmonized regulatory environment so that cross-border production and investment decisions do not have to be re-evaluated against each government's unilateral policy risk.
% TRANSFER_FUNCTION: Moves regulatory authority and bargaining leverage from domestic legislatures, regulatory agencies, and organized labor to mobile capital and the investor-state arbitration apparatus; moves compliance costs and wage/standards suppression onto workers and communities whose governments cannot act without triggering treaty exposure.
% ABSENT_VOICES: Workers, environmental communities, and local governments were not parties to treaty negotiation or to investor-state arbitration; they have no standing to bring claims under the investment chapters and are represented, if at all, only indirectly through national negotiators who answer to broader constituencies.
% DISAPPEARANCE_RATIONALE: If the treaty's supremacy-over-domestic-standards reading were abandoned, domestic regulators could tighten environmental and labor rules without compensation exposure, capital's relocation threat would lose its treaty-guaranteed backing, and bargaining leverage would shift measurably back toward workers and regulators — production location decisions and enforcement postures would change within a single legislative cycle.
% FOUNDING_PROBLEM: Cross-border trade and investment faced unpredictable, unilaterally revisable regulatory environments; firms needed assurance that market access and investment conditions would not be arbitrarily withdrawn after capital was committed.
% FOUNDING_PROBLEM_CORROBORATION: Investors and trade negotiators attest the problem remains live — regulatory unpredictability still threatens committed capital. Independent economic analysis from labor economists and domestic regulatory agencies (outside the beneficiary set) attests that the mechanism has shifted from protecting against arbitrary expropriation toward routinely constraining ordinary, non-discriminatory regulatory updates, evidenced by the post-1994 rise in investor-state claims targeting environmental and public-health measures documented in independent tribunal-record studies.
narrative_ontology:disappearance_verdict(nafta_jurisdictional_boundary__capital_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(nafta_jurisdictional_boundary__capital_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__capital_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nafta_jurisdictional_boundary__capital_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.79, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nafta_jurisdictional_boundary__capital_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nafta_jurisdictional_boundary__capital_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nafta_jurisdictional_boundary__capital_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction rises from 0.48 to 0.79 across the interval, tracking the documented growth in investor-state dispute claims targeting non-discriminatory environmental and public health regulation since the mid-1990s — a rent-seeking accumulation layered onto a genuine market-access coordination function. Suppression (0.72) is high because the constraint's persistence depends on regulators' credible fear of arbitration exposure, not on voluntary compliance; this is a raw structural property, not scaled by scope. Theater ratio remains comparatively low (0.28) because the enforcement mechanism — actual arbitration panels issuing binding awards — is functionally real, not merely performative, even as its scope of application widens beyond the treaty's stated purpose.
 *
 * PERSPECTIVAL GAP:
 *   From the capital-side beneficiary seats, the treaty functions as pure coordination: predictable market access enabling efficient cross-border investment. From the regulatory-agency and worker seats, the identical treaty text functions as an extraction mechanism that transfers policy authority and bargaining leverage upward while foreclosing corrective domestic action. The tangled_rope classification captures both faces simultaneously — the coordination function (market access, investment predictability) is real, and the asymmetric extraction (regulatory chill, wage suppression) is real, riding on the same enforcement machinery.
 *
 * DIRECTIONALITY LOGIC:
 *   Capital-side beneficiaries hold institutional power with arbitrage-grade exit — they can relocate production or bring treaty claims, placing them near the full-beneficiary end of directionality. Workers hold powerless status with trapped exit and no standing under the investment chapters at all, placing them near the full-target end. Domestic regulators and local governments sit at moderate power with constrained exit: they retain formal authority but face credible deterrence, which the engine's directionality derivation should register as substantial (not maximal) target exposure given their residual formal jurisdiction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — protecting committed capital against arbitrary unilateral expropriation — was real at treaty formation, when the risk of unpredictable nationalization or regulatory reversal was material. Under the capital supremacy reading, that founding problem is treated as still fully live and used to justify a scope of protection that now reaches ordinary non-discriminatory regulatory updates, which independent tribunal-record analysis suggests was never the original target. The mismatch between founding_problem_status (contested) and disappearance_verdict (world_rearranges) is exactly the signal this classification is built to surface: the mandate has plausibly outrun its original function, but is not yet formally acknowledged as such by the agenda-setting negotiators who administer it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy,
    'Does the treaty text itself compel the capital supremacy reading, or is this reading one contestable interpretation among the embedded_liberalism_reading and sovereignty_primacy_reading, sustained by which tribunals and negotiators currently hold interpretive authority?',
    'Comparative analysis of dispute settlement panel composition and precedent over time: if panels drawn from the investment arbitration bar systematically favor capital-supremacy readings of ambiguous treaty language relative to panels with broader composition, the reading is partly an artifact of who adjudicates rather than of the text itself.',
    'If the reading is substantially artifact-of-adjudication rather than textually compelled, the extraction attributed to this reading is better understood as a capture of interpretive authority — reinforcing the tangled_rope classification''s enforcement dependency rather than any inherent textual determinacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether capital supremacy is textually compelled or interpretively constructed by adjudicator composition.').

omega_variable(
    regulatory_chill_measurement_gap,
    'How much domestic regulatory action is deterred before ever being proposed (invisible chill) versus overturned after being challenged (visible extraction)?',
    'Survey of regulatory agency internal deliberation records and legislative drafting history for abandoned or narrowed proposals citing treaty exposure as a reason, compared against the visible record of actual investor-state claims filed.',
    'If invisible chill substantially exceeds visible claims, the authored extractiveness value (0.79) understates the reading''s true effect, since only the visible portion is captured in dispute-settlement-based measurement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_chill_measurement_gap, empirical, 'The gap between measured dispute claims and unmeasured anticipatory regulatory chill.').

omega_variable(
    sibling_reading_convergence_pressure,
    'Does the growing volume of investor-state claims under the capital supremacy reading create structural pressure toward the embedded_liberalism_reading (via negotiated carve-outs and reform) or toward entrenchment of capital supremacy as the operative default?',
    'Track amendment and side-letter activity following major disputes: carve-outs for public health/environmental measures moving the operative reading toward embedded_liberalism would be observable in subsequent treaty text revisions (e.g., USMCA relative to NAFTA).',
    'Determines whether this reading''s extraction trajectory is self-limiting (triggering reform) or self-reinforcing (entrenching via precedent) — relevant to whether the rising extraction trend in the temporal measurements continues or reverses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_convergence_pressure, empirical, 'Whether accumulating claims pressure the kernel toward the embedded_liberalism_reading or entrench capital supremacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__capital_supremacy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naft_tr_t0, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(naft_tr_t6, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 6, 0.14).
narrative_ontology:measurement(naft_tr_t12, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 12, 0.18).
narrative_ontology:measurement(naft_tr_t18, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 18, 0.21).
narrative_ontology:measurement(naft_tr_t24, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 24, 0.25).
narrative_ontology:measurement(naft_tr_t30, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 30, 0.28).

% Extraction over time
narrative_ontology:measurement(naft_be_t0, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(naft_be_t6, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 6, 0.56).
narrative_ontology:measurement(naft_be_t12, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 12, 0.63).
narrative_ontology:measurement(naft_be_t18, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 18, 0.7).
narrative_ontology:measurement(naft_be_t24, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 24, 0.75).
narrative_ontology:measurement(naft_be_t30, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 30, 0.79).

% Suppression requirement over time
narrative_ontology:measurement(naft_su_t0, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(naft_su_t6, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 6, 0.57).
narrative_ontology:measurement(naft_su_t12, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 12, 0.62).
narrative_ontology:measurement(naft_su_t18, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 18, 0.66).
narrative_ontology:measurement(naft_su_t24, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 24, 0.69).
narrative_ontology:measurement(naft_su_t30, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nafta_jurisdictional_boundary__capital_supremacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.1).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, embedded_liberalism_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, sovereignty_primacy_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, investor_state_dispute_settlement_mechanism).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the natural-language label 'the NAFTA jurisdictional boundary' per the ε-invariance principle. capital_supremacy_reading (this story, tangled_rope, ε=0.79), embedded_liberalism_reading (compatible-policy-space reading, expected lower ε and rope-adjacent classification), and sovereignty_primacy_reading (domestic-law-supreme reading, expected minimal extraction, rope or mountain-adjacent from the sovereignty seat) share the same treaty text but instantiate structurally distinct constraints with different beneficiary/victim sets and different ε. They are linked here rather than merged because measuring 'the treaty' one way (capital mobility guarantee) versus another way (domestic policy space preservation) yields incompatible ε values — the signature of two constraints, not one constraint with an ambiguous observable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
