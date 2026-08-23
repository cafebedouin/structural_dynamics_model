% ============================================================================
% CONSTRAINT STORY: ecb_mandate_article_127__climate_incorporation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ecb_mandate_article_127__climate_incorporation, []).

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
 *   constraint_id: ecb_mandate_article_127__climate_incorporation
 *   human_readable: Climate-Incorporation Reading of the ECB Mandate: Collateral Haircuts and Purchase Tilts under Article 11 TFEU
 *   domain: economic/legal/institutional
 *
 * SUMMARY:
 *   Within the contested kernel of the ECB's Article 127 TFEU mandate, this
 *   story instantiates the climate_incorporation reading: the Governing
 *   Council treats the mandate as requiring active integration of
 *   climate-transition risk into corporate asset purchases and collateral
 *   haircuts, with the Article 11 TFEU environmental-integration clause
 *   supplying the treaty-level support obligation. The referent for epsilon,
 *   assessed by this reading's own lights, is the standing
 *   climate-integration arrangement itself - disclosure conditions on
 *   eligible collateral, climate-scored haircut schedules, and
 *   decarbonization-path purchase tilts - never the arrangements the sibling
 *   readings would install. Structurally the arrangement coordinates
 *   euro-area financing around a legislated transition pathway while imposing
 *   asymmetric financing costs on carbon-intensive collateral posters and
 *   issuers through the same framework; hence the claimed_type tangled_rope,
 *   authored independently of the metric values. Extraction arrives by
 *   repricing rather than confiscation - the expected structural delta for
 *   this reading. The sibling readings (orthodox_price_stability,
 *   expansive_secondary_objectives) are separate constraints in separate
 *   files; the reading contest is carried entirely by the omegas below. KEY
 *   AGENTS (by structural relationship): - ecb_governing_council: Agenda
 *   setter (institutional/constrained) - administers haircuts, eligibility,
 *   and tilts; defends the legal basis - green_bond_issuers: Primary
 *   financing beneficiary (moderate/mobile) - eu_climate_policy_institutions:
 *   Policy-level beneficiary (institutional/constrained) - supported without
 *   operating - climate_transition_industries: Beneficiary (organized/mobile)
 *   - fossil_fuel_sector_firms: Primary target (powerful/identity_locked) -
 *   bears haircut and tilt effects - heavy_emitting_manufacturers: Target
 *   (organized/constrained) - euro_area_credit_institutions: Dual
 *   intermediary (institutional/constrained) - bears compliance costs,
 *   collects green-origination gains - climate_advocacy_organizations:
 *   Excluded voice (organized/no route into deliberation) -
 *   academic_monetary_economists: Analytical observer (analytical/analytical)
 *
 * KEY AGENTS:
 *   - ecb_governing_council: agenda setter (institutional/constrained)
 *   - green_bond_issuers: primary financing beneficiary (moderate/mobile)
 *   - eu_climate_policy_institutions: policy-level beneficiary (institutional/constrained)
 *   - climate_transition_industries: beneficiary (organized/mobile)
 *   - fossil_fuel_sector_firms: primary target (powerful/identity_locked)
 *   - heavy_emitting_manufacturers: target (organized/constrained)
 *   - euro_area_credit_institutions: dual intermediary, payer and beneficiary (institutional/constrained)
 *   - climate_advocacy_organizations: excluded voice (organized/trapped outside deliberation)
 *   - academic_monetary_economists: analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__climate_incorporation, 0.47).
domain_priors:suppression_score(ecb_mandate_article_127__climate_incorporation, 0.42).
domain_priors:theater_ratio(ecb_mandate_article_127__climate_incorporation, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, extractiveness, 0.47).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__climate_incorporation, tangled_rope).
narrative_ontology:human_readable(ecb_mandate_article_127__climate_incorporation, "Climate-Incorporation Reading of the ECB Mandate: Collateral Haircuts and Purchase Tilts under Article 11 TFEU").
narrative_ontology:topic_domain(ecb_mandate_article_127__climate_incorporation, "economic/legal/institutional").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__climate_incorporation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__climate_incorporation, 'a6f8230b-c796-4e6f-9d16-63606009965e').
narrative_ontology:cs_kernel_codification('a6f8230b-c796-4e6f-9d16-63606009965e', fixed_text).
narrative_ontology:cs_authority_grounding('a6f8230b-c796-4e6f-9d16-63606009965e', lineage).
narrative_ontology:cs_interpretation_layer_present('a6f8230b-c796-4e6f-9d16-63606009965e').
narrative_ontology:cs_reading_relation('a6f8230b-c796-4e6f-9d16-63606009965e', ecb_mandate_article_127__orthodox_price_stability, forecloses).
narrative_ontology:cs_reading_relation('a6f8230b-c796-4e6f-9d16-63606009965e', ecb_mandate_article_127__expansive_secondary_objectives, coexists_with).
narrative_ontology:cs_axiom('a6f8230b-c796-4e6f-9d16-63606009965e', foundational, article_11_environmental_integration_is_operative_duty).
narrative_ontology:cs_axiom_status(article_11_environmental_integration_is_operative_duty, holdable).
narrative_ontology:cs_axiom_grounding('a6f8230b-c796-4e6f-9d16-63606009965e', article_11_environmental_integration_is_operative_duty, conventional).
narrative_ontology:cs_axiom('a6f8230b-c796-4e6f-9d16-63606009965e', secondary, transition_risk_correction_serves_price_stability).
narrative_ontology:cs_axiom_status(transition_risk_correction_serves_price_stability, holdable).
narrative_ontology:cs_axiom_grounding('a6f8230b-c796-4e6f-9d16-63606009965e', transition_risk_correction_serves_price_stability, instrumental).
narrative_ontology:cs_reference_frame('a6f8230b-c796-4e6f-9d16-63606009965e', treaty_environmental_integration_baseline).
narrative_ontology:cs_drift_state('a6f8230b-c796-4e6f-9d16-63606009965e', contemporary_litigation_and_rollout_period, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a6f8230b-c796-4e6f-9d16-63606009965e', '').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__climate_incorporation, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, green_bond_issuers).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, eu_climate_policy_institutions).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, climate_transition_industries).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, fossil_fuel_sector_firms).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, heavy_emitting_manufacturers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, euro_area_credit_institutions).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, euro_area_credit_institutions).
narrative_ontology:constraint_vindicates(ecb_mandate_article_127__climate_incorporation, article_11_tfeu_integration_principle).
narrative_ontology:constraint_vindicates(ecb_mandate_article_127__climate_incorporation, climate_transition_risk_materiality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the haircut schedules, collateral eligibility criteria, climate scores, and corporate-purchase tilts that operationalize climate consideration in Eurosystem balance-sheet management. Defends the legal basis by tying the measures to the price-stability objective (transition risk as a stability threat) and to the Article 11 TFEU duty to support Union environmental policy. Faces private litigation, member-state criticism voiced through national central bank governors seated on the Council itself, and parliamentary scrutiny. It can revise the framework at will, but wholesale reversal would carry treaty-consistency and credibility exposure.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, ecb_governing_council, agenda_setter,
    institutional, generational, constrained, continental).

% Issue climate-labelled and transition-aligned debt. Gain eligibility advantages in purchase programs and benefit from demand reallocation that compresses their yields as portfolio tilts steer purchases toward better climate performers. Can issue across multiple markets and currencies, so their position improves with tilt intensity and deteriorates only if the framework reverts to climate-blind baselines.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, green_bond_issuers, beneficiary,
    moderate, biographical, mobile, continental).

% The Commission and Parliament legislate the EU climate framework (Fit-for-55, taxonomy regulation) and publicly press for monetary support of it. They receive amplification of their policy's financing conditions without running any monetary operation themselves and cannot compel the ECB directly due to independence guarantees; their leverage runs through legislation, resolutions, and appointments.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, eu_climate_policy_institutions, beneficiary,
    institutional, generational, constrained, continental).

% Renewable-energy, grid, and clean-technology firms obtain comparatively cheaper euro funding as collateral and purchase frameworks channel capital toward their activities. Organized trade associations lobby for faster and deeper tilts.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, climate_transition_industries, beneficiary,
    organized, biographical, mobile, continental).

% Oil and gas producers whose bonds face wider haircuts when posted as Eurosystem collateral, reduced purchase-program access, and mounting disclosure burdens. Global capital markets remain open to them, but the firm's asset base, infrastructure, workforce, and strategic identity are built around hydrocarbon production: partial diversification is feasible, wholesale transformation dissolves the enterprise as currently constituted. Funds litigation and political resistance against the measures.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, fossil_fuel_sector_firms, payer,
    powerful, generational, identity_locked, global).

% Steel, cement, and chemical producers that post large collateral pools and borrow at scale. Face stepwise financing-cost penalties as haircut schedules incorporate climate performance, plus eligibility scrutiny tied to disclosure quality. Relocation is expensive because energy infrastructure and customer networks are regionally fixed; decarbonization pathways are capital-intensive. Caught between paying the differential and financing a transformation, with some carbon-leakage pressure toward jurisdictions outside the framework.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, heavy_emitting_manufacturers, payer,
    organized, generational, constrained, continental).

% Banks pledge collateral in Eurosystem operations and intermediate corporate funding. They bear widened haircuts on carbon-heavy pools, compliance and disclosure costs, and repricing of legacy exposures; they simultaneously earn origination and advisory fees from the green financing boom and operate in a macro environment framed as stabilized against transition shocks. They cannot abandon euro-area repo funding and adjust mainly by reweighting client books.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, euro_area_credit_institutions, payer,
    institutional, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(ecb_mandate_article_127__climate_incorporation, euro_area_credit_institutions, beneficiary).

% NGOs and policy institutes pressing for faster, deeper tilts and eventual full exclusion of fossil assets from Eurosystem operations. They stand outside Governing Council deliberation with no route into the decision room; their influence travels through publications, litigation support, and European Parliament hearings rather than votes.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, climate_advocacy_organizations, excluded,
    organized, generational, trapped, continental).

% Publish on mandate scope, the market-neutrality critique, and the measurable effects of tilts and haircuts. Supply the theoretical ammunition used by every other seat; hold no operational stake in the framework either way.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, academic_monetary_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ecb_mandate_article_127__climate_incorporation, green_bond_issuers).
narrative_ontology:fixing_cost_class(ecb_mandate_article_127__climate_incorporation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns Eurosystem collateral valuation and corporate asset purchases with the EU's legislated climate pathway (Article 11 TFEU environmental-integration obligation applied to the ESCB), correcting the systematic underpricing of transition risk that climate-blind, ratings-only frameworks embedded in secured-funding and corporate-bond markets across the whole currency union at once.
% TRANSFER_FUNCTION: Moves financing conditions along the carbon gradient: raises funding costs for carbon-intensive collateral posters and bond issuers through haircut widening, eligibility conditions, and purchase tilting; lowers relative funding costs for climate-aligned issuers; and shifts the Eurosystem balance sheet's exposure composition away from stranded-asset concentration.
% ABSENT_VOICES: Carbon-intensive firms participate only as market counterparties, never as policy interlocutors, yet bear the sharpest financing-cost effects; climate advocacy organizations demanding stronger measures are outside the Governing Council room; euro-area savers and taxpayers carrying diffuse indirect effects have no seat at all. The unanimity of official support for the reading is partly explained by these absences.
% DISAPPEARANCE_RATIONALE: If the climate-integration layer vanished overnight, collateral haircuts revert to ratings-only schedules, green funding premia compress, carbon-intensive funding costs fall, banks rebuild brown collateral pools, and the Eurosystem re-accumulates concentrated transition-risk exposure under market-neutral acquisition rules. Euro-area financing conditions reorganize around the pre-2021 climate-blind baseline, and EU climate-policy transmission loses a non-fiscal lever.
% FOUNDING_PROBLEM: Three stacked problems circa 2020-2021: (a) a legal gap - Article 11 TFEU obliges every Union institution including the ESCB to integrate environmental protection into policy implementation, and the Eurosystem's market operations had ignored this; (b) a financial-stability gap - climate-blind purchase and collateral rules accumulated concentrated transition-risk exposure on the Eurosystem balance sheet; (c) a market-failure gap - carbon externality and transition risk were systematically unpriced in collateral valuation.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: independent academic legal scholarship attests the existence and disputed scope of the Article 11 obligation; insurer catastrophe modeling and rating-agency climate methodologies (commercial parties with no stake in the mandate debate) attest that transition and physical risk were underpriced in fixed-income valuation; the existence of active private legal challenges to the measures attests that whether the operative-duty reading is settled remains genuinely contested. No source outside the benefiting set attests that the full operative-duty reading itself is established law.
narrative_ontology:disappearance_verdict(ecb_mandate_article_127__climate_incorporation, world_rearranges).
narrative_ontology:founding_problem_status(ecb_mandate_article_127__climate_incorporation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__climate_incorporation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ecb_mandate_article_127__climate_incorporation, 'none', 1).
narrative_ontology:epsilon_provenance(ecb_mandate_article_127__climate_incorporation, 0.47, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ecb_mandate_article_127__climate_incorporation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ecb_mandate_article_127__climate_incorporation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ecb_mandate_article_127__climate_incorporation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.47: the haircut differentials, eligibility conditions, and tilts transfer real financing costs to carbon-intensive actors, but the magnitudes are bounded and much of the effect works through signaling and demand reallocation; the rising series reflects deliberate ratchet design (disclosure conditions 2022, scorecard haircuts 2023 onward, progressive tightening), not oscillation. Suppression 0.42 is authored as the raw structural property and is deliberately NOT scaled here - the engine owns directionality and scope scaling; it captures rule-driven repricing plus the anticipatory de-risking banks perform ahead of the written rules (see the suppression-mechanism omega). Theater_ratio 0.33 with a declining series: the 2021 phase was announcement-heavy relative to binding action (high theatrical share), maturing into operative machinery as haircuts and tilts acquired force. Accessibility_collapse 0.38: alternatives are channeled rather than eliminated - decarbonize, pay the differential, or leave euro funding; leaving is hard for regionally fixed manufacturers but softened for globally mobile majors. Resistance 0.58: private litigation, member-state opposition, industry campaigns, and academic critique arriving from both directions (too slow, too far). Receipt surface: the relative financing gains demonstrably accrue to green_bond_issuers, so gain_flow names that seat rather than asserting diffuse; fixing_cost is prohibitive because unwinding would carry treaty-consistency accusations, credibility loss with EU legislative partners, and forfeiture of the balance-sheet-protection rationale - costs exceeding whatever relief the Council would gain. All three series share one six-point grid (2021-2030), with observed/provenance-marked endpoints through 2025 and projected points thereafter.
 *
 * PERSPECTIVAL GAP:
 *   Per-seat classifications should diverge sharply. From the payer seats the arrangement operates as enforced cost imposition: fossil majors sit near the full-target end because identity lock amplifies their directionality (capital is globally mobile but the enterprise's asset base, workforce, and self-conception are fused to hydrocarbon production - exit means ceasing to be the firm), while heavy manufacturers sit slightly less extreme with constrained but real relocation and transformation exits. From the agenda-setter seat the same structure is mandate fulfillment and prudential housekeeping. Same-nominal-standing differentiation: fossil majors (powerful, global capital access) and manufacturers (organized, regionally fixed) hold comparable political weight but very different exit grades - asset fixity and identity fusion, not global rank, set the difference. Inter-institutionally, the Commission and Parliament collect policy amplification without operating anything, while member-state preferences enter the Council internally through national central bank governors, making enforcement intensity a negotiated outcome. Coalition potential among the two payer groups is limited by divergent interests: manufacturers primarily want subsidized transition, fossil firms primarily want rollback. The excluded advocacy seat supplies commentary-grade absence signal only - it never drives classification corrections.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low directionality: green_bond_issuers (mobile, arbitrage-grade outside options in global capital markets) sit nearest the beneficiary end; climate_transition_industries similar; eu_climate_policy_institutions collect policy-level rather than financing-level gains. Victim declarations map to high directionality: identity_locked fossil majors nearest the full-target end, constrained manufacturers somewhat less. euro_area_credit_institutions derive mixed directionality from their dual payer/beneficiary roles. Spatial scopes are predominantly continental, which the engine treats with the corresponding verification-difficulty scaling; the globally scoped fossil seat is the exception. No explicit overrides are authored: the beneficiary/victim declarations plus exit grades already produce the correct ordering, so the derivation chain suffices.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the kernel decomposition, the colloquial label 'the ECB mandate' collapses three structurally distinct constraints into one story. A pure-coordination reading hides the fossil-sector cost imposition behind treaty language; a pure-extraction reading (the democratic-deficit critique) erases the genuine coordination function - climate-blind market-neutral rules verifiably underpriced transition risk and accumulated concentrated exposure. The tangled_rope claim keeps both faces visible: real coordination solved once centrally (treaty alignment plus risk correction) AND asymmetric payment through the identical haircut and tilt machinery. On the R5 interview, the founding problem is live (the obligation stands, underpricing persists, balance-sheet exposure is real), and the status-live x world_rearranges combination yields no dead-mandate mismatch flag - mandatrophy is not resolved and is not declared.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_article_127,
    'This constraint is one reading of kernel ecb_mandate_article_127: do Article 127''s support clause and Article 11 TFEU impose an operative duty of climate integration, or are secondary objectives merely permissible (expansive reading) or non-operational outright (orthodox reading)?',
    'European Court of Justice adjudication of a concrete legal challenge to climate-conditioned collateral or purchase rules. Until such adjudication, each reading stands as a separately classified constraint in its own story.',
    'If the orthodox reading prevails, this constraint''s enforcement machinery loses its legal foundation and the arrangement collapses toward inert ceremonial maintenance; if this reading prevails decisively, the orthodox sibling''s core premise becomes untenable within a single framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_article_127, empirical, 'Which reading of the Article 127 kernel governs the Eurosystem''s operational room.').

omega_variable(
    haircut_risk_calibration_gap,
    'Do the collateral haircut differentials and purchase tilts track measured transition-risk differentials, or do they embed a punitive component exceeding actuarially supportable risk pricing?',
    'Compare the Eurosystem haircut schedule deltas against third-party climate-value-at-risk models and rating-agency methodologies on matched collateral pools; audit the climate-score methodology against realized default and downgrade experience.',
    'A punitive excess converts the coordination reading into materially asymmetric cost imposition and raises effective extraction at the payer seats, pushing computed types toward the snare end; pure risk tracking supports the rope-like half of the hybrid claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(haircut_risk_calibration_gap, empirical, 'Whether haircut calibration prices risk or punishes emissions.').

omega_variable(
    article_11_operative_binding_status,
    'Is Article 11 TFEU''s environmental-integration obligation on the ESCB an operative legal duty that constrains operational choices, or a programmatic aspiration without enforcement force?',
    'Legal-doctrinal analysis combined with how courts actually treat the clause when invoked in litigation; the framing choice between a positivist enacted-rule reading and an aspirational-declaration reading is itself part of the resolution.',
    'Operative binding makes the constraint''s coordination claim treaty-grounded and strengthens the rope half; aspirational status reduces the measure to discretionary institutional preference and weakens the coordination gate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_11_operative_binding_status, conceptual, 'Operative versus aspirational legal status of the integration clause.').

omega_variable(
    portfolio_repricing_suppression_mechanism,
    'Is the financing suppression borne by carbon-intensive borrowers structural (written haircut, eligibility, and tilt rules) or anticipatory and internalized (banks de-risking beyond what the written rules require)?',
    'Post-relaxation trajectory test: if brown-asset funding spreads normalize after rules are eased, the suppression was rule-driven; if spreads persist after relaxation, the de-risking expectation has been internalized by lenders.',
    'An internalized component means effective suppression exceeds the structural measure and persists even after exit from the rules, changing both the suppression reading and any assessment of reversibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(portfolio_repricing_suppression_mechanism, empirical, 'Structural versus anticipatory mechanism of the financing suppression.').

omega_variable(
    transition_pathway_policy_dependence,
    'Does the constraint''s coordination function depend on continuation of the current EU climate-policy ambition (Fit-for-55 pathway), such that weakening the legislated targets would remove the coordination justification while the machinery persisted?',
    'Counterfactual analysis of framework behavior under revised EU targets: observe whether haircut schedules and tilt calibrations recalibrate downward with target revisions or persist unchanged.',
    'Policy-dependent coordination makes the arrangement transitional and contingent; pathway-independent risk correction sustains a steady-state hybrid classification regardless of political fortune.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transition_pathway_policy_dependence, conceptual, 'Dependence of the coordination justification on the prevailing EU policy pathway.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__climate_incorporation, 2021, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb__tr_t2021, ecb_mandate_article_127__climate_incorporation, theater_ratio, 2021, 0.55).
narrative_ontology:measurement_basis(ecb__tr_t2021, observed).
narrative_ontology:measurement(ecb__tr_t2023, ecb_mandate_article_127__climate_incorporation, theater_ratio, 2023, 0.45).
narrative_ontology:measurement_basis(ecb__tr_t2023, observed).
narrative_ontology:measurement(ecb__tr_t2025, ecb_mandate_article_127__climate_incorporation, theater_ratio, 2025, 0.38).
narrative_ontology:measurement_basis(ecb__tr_t2025, observed).
narrative_ontology:measurement(ecb__tr_t2027, ecb_mandate_article_127__climate_incorporation, theater_ratio, 2027, 0.36).
narrative_ontology:measurement_basis(ecb__tr_t2027, projected).
narrative_ontology:measurement(ecb__tr_t2029, ecb_mandate_article_127__climate_incorporation, theater_ratio, 2029, 0.34).
narrative_ontology:measurement_basis(ecb__tr_t2029, projected).
narrative_ontology:measurement(ecb__tr_t2030, ecb_mandate_article_127__climate_incorporation, theater_ratio, 2030, 0.33).
narrative_ontology:measurement_basis(ecb__tr_t2030, projected).

% Extraction over time
narrative_ontology:measurement(ecb__be_t2021, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 2021, 0.22).
narrative_ontology:measurement_basis(ecb__be_t2021, observed).
narrative_ontology:measurement(ecb__be_t2023, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 2023, 0.28).
narrative_ontology:measurement_basis(ecb__be_t2023, observed).
narrative_ontology:measurement(ecb__be_t2025, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 2025, 0.34).
narrative_ontology:measurement_basis(ecb__be_t2025, observed).
narrative_ontology:measurement(ecb__be_t2027, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 2027, 0.39).
narrative_ontology:measurement_basis(ecb__be_t2027, projected).
narrative_ontology:measurement(ecb__be_t2029, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 2029, 0.44).
narrative_ontology:measurement_basis(ecb__be_t2029, projected).
narrative_ontology:measurement(ecb__be_t2030, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 2030, 0.47).
narrative_ontology:measurement_basis(ecb__be_t2030, projected).

% Suppression requirement over time
narrative_ontology:measurement(ecb__su_t2021, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 2021, 0.2).
narrative_ontology:measurement_basis(ecb__su_t2021, observed).
narrative_ontology:measurement(ecb__su_t2023, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 2023, 0.27).
narrative_ontology:measurement_basis(ecb__su_t2023, observed).
narrative_ontology:measurement(ecb__su_t2025, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 2025, 0.33).
narrative_ontology:measurement_basis(ecb__su_t2025, observed).
narrative_ontology:measurement(ecb__su_t2027, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 2027, 0.37).
narrative_ontology:measurement_basis(ecb__su_t2027, projected).
narrative_ontology:measurement(ecb__su_t2029, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 2029, 0.4).
narrative_ontology:measurement_basis(ecb__su_t2029, projected).
narrative_ontology:measurement(ecb__su_t2030, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 2030, 0.42).
narrative_ontology:measurement_basis(ecb__su_t2030, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_mandate_article_127__climate_incorporation, resource_allocation).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, ecb_mandate_article_127__orthodox_price_stability).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, ecb_mandate_article_127__expansive_secondary_objectives).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, eu_taxonomy_regulation).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'the ECB mandate' into three kernel readings, per the epsilon-invariance principle: the single natural-language concept covers structurally distinct claims with different epsilon referents, different victim sets, and different empirical status. Genealogy runs upstream-to-downstream: orthodox_price_stability (the founding, historically most established reading) -> expansive_secondary_objectives (the interpretive broadening) -> climate_incorporation (this file, the newest reading, citing Article 11 TFEU and taxonomy-alignment evidence upstream of it). Each story links the others through affects_constraints; the taxonomy-regulation edge records the real institutional coupling whereby EU classification rules feed the ECB's climate-scoring inputs. This file's epsilon is authored for the standing climate-integration arrangement alone; the sibling files author theirs independently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
