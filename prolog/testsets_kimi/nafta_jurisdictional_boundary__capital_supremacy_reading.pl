% ============================================================================
% CONSTRAINT STORY: nafta_jurisdictional_boundary__capital_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   human_readable: NAFTA-Style Capital Supremacy Reading (Treaty Text as Supreme Law Over Domestic Regulation)
 *   domain: international_trade_law/political_economy/regulatory_federalism
 *
 * SUMMARY:
 *   This constraint instantiates the capital_supremacy_reading of the
 *   nafta_jurisdictional_boundary kernel. Under this reading, trade agreement
 *   text (exemplified by NAFTA Chapter 11 and successor ISDS provisions)
 *   functions as supreme law that overrides domestic regulatory standards.
 *   Capital mobility and regulatory harmonization are treated as mandatory
 *   treaty obligations enforced through investor-state dispute settlement
 *   (ISDS), which transfers jurisdictional authority from domestic
 *   legislatures and regulatory agencies to transnational arbitration panels.
 *   The coordination functionâcross-border trade and investment
 *   predictabilityâis real, but the structural arrangement asymmetrically
 *   extracts regulatory autonomy from domestic public-interest institutions
 *   and transfers it upward to multinational enterprises and mobile capital.
 *   The authored claim is tangled_rope to capture both the genuine
 *   coordination and the asymmetric extraction; the metrics are authored
 *   independently to describe the actual operation.
 *
 * KEY AGENTS:
 *   - multinational_enterprises: Primary beneficiary (powerful/global/mobile) â extract regulatory flexibility and ISDS access
 *   - investor_state_arbitrators: Agenda-setter (institutional/global/analytical) â interpret treaty text as supreme and enforce capital mobility
 *   - domestic_regulatory_agencies: Primary target (institutional/national/constrained) â lose jurisdictional authority to treaty override
 *   - domestic_legislatures: Target (institutional/national/constrained) â democratic lawmaking subordinated to harmonization obligations
 *   - labor_environmental_standards_bodies: Target (organized/national/constrained) â standards chilled or invalidated by supremacy clauses
 *   - public_interest_advocates: Excluded voice (moderate/national/constrained) â structurally absent from ISDS proceedings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.72).
domain_priors:suppression_score(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.68).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__capital_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__capital_supremacy_reading, "NAFTA-Style Capital Supremacy Reading (Treaty Text as Supreme Law Over Domestic Regulation)").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__capital_supremacy_reading, "international_trade_law/political_economy/regulatory_federalism").

domain_priors:requires_active_enforcement(nafta_jurisdictional_boundary__capital_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__capital_supremacy_reading, 'f042bd30-d2a1-4423-9949-bb40f287d43c').
narrative_ontology:cs_kernel_codification('f042bd30-d2a1-4423-9949-bb40f287d43c', formalized).
narrative_ontology:cs_authority_grounding('f042bd30-d2a1-4423-9949-bb40f287d43c', lineage).
narrative_ontology:cs_interpretation_layer_present('f042bd30-d2a1-4423-9949-bb40f287d43c').
narrative_ontology:cs_reading_relation('f042bd30-d2a1-4423-9949-bb40f287d43c', nafta_jurisdictional_boundary__embedded_liberalism_reading, influences).
narrative_ontology:cs_reading_relation('f042bd30-d2a1-4423-9949-bb40f287d43c', nafta_jurisdictional_boundary__sovereignty_primacy_reading, forecloses).
narrative_ontology:cs_axiom('f042bd30-d2a1-4423-9949-bb40f287d43c', foundational, treaty_text_supreme_over_domestic_law).
narrative_ontology:cs_axiom_status(treaty_text_supreme_over_domestic_law, holdable).
narrative_ontology:cs_axiom_grounding('f042bd30-d2a1-4423-9949-bb40f287d43c', treaty_text_supreme_over_domestic_law, conventional).
narrative_ontology:cs_axiom('f042bd30-d2a1-4423-9949-bb40f287d43c', foundational, capital_mobility_requires_mandatory_harmonization).
narrative_ontology:cs_axiom_status(capital_mobility_requires_mandatory_harmonization, holdable).
narrative_ontology:cs_axiom_grounding('f042bd30-d2a1-4423-9949-bb40f287d43c', capital_mobility_requires_mandatory_harmonization, instrumental).
narrative_ontology:cs_reference_frame('f042bd30-d2a1-4423-9949-bb40f287d43c', capital_supremacy_framework).
narrative_ontology:cs_drift_state('f042bd30-d2a1-4423-9949-bb40f287d43c', contemporary_trade_skepticism_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f042bd30-d2a1-4423-9949-bb40f287d43c', '').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__capital_supremacy_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__capital_supremacy_reading, multinational_enterprises).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_regulatory_agencies).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_legislatures).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, labor_environmental_standards_bodies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Deploy capital across borders and rely on treaty-enforced regulatory harmonization to reduce compliance fragmentation. Can access ISDS to challenge domestic regulations that affect expected returns. Exit from the constraint is mobileâcapital can relocateâbut the legal framework is designed to follow and protect it.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, multinational_enterprises, beneficiary,
    powerful, generational, mobile, global).

% Appointed to adjudicate claims that domestic measures violate treaty obligations. Their interpretive work establishes whether regulatory standards constitute indirect expropriation or unfair treatment. They operate within the treaty framework and their authority derives from the parties' consent to arbitration.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, investor_state_arbitrators, agenda_setter,
    institutional, generational, analytical, global).

% Draft and enforce environmental, labor, and health standards within their national jurisdiction. Under this constraint, their regulations risk challenge by foreign investors and may be chilled or overridden by treaty interpretation. Exit is constrainedâthey cannot unilaterally opt out of treaty obligations.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_regulatory_agencies, payer,
    institutional, biographical, constrained, national).

% Enact public-welfare legislation on behalf of constituents. Treaty supremacy and harmonization requirements narrow the policy space, potentially rendering new statutes non-compliant. Amendments or exits require renegotiation with treaty partners.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_legislatures, payer,
    institutional, biographical, constrained, national).

% Develop and maintain protective standards for workers and ecosystems. Their mandates conflict with downward harmonization pressures and regulatory chill generated by ISDS risk. They lack direct standing in treaty dispute proceedings.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, labor_environmental_standards_bodies, payer,
    organized, biographical, constrained, national).

% Represent constituencies harmed by weakened standards. They are structurally excluded from ISDS proceedings, which only permit investor claims, and their domestic advocacy is often overridden by treaty supremacy arguments.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, public_interest_advocates, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nafta_jurisdictional_boundary__capital_supremacy_reading, multinational_enterprises).
narrative_ontology:fixing_cost_class(nafta_jurisdictional_boundary__capital_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables cross-border capital mobility and trade by establishing a single predictable legal framework that harmonizes divergent national regulatory regimes through mandatory treaty obligations.
% TRANSFER_FUNCTION: Moves regulatory authority and democratic policy space from domestic legislatures and agencies to international treaty obligations and investor-state dispute mechanisms, transferring wealth and legal flexibility upward to transnational capital.
% ABSENT_VOICES: Domestic constituencies dependent on strong labor and environmental standards are structurally excluded from ISDS proceedings; only investors can initiate claims. Developing countries seeking policy flexibility are marginalized in treaty drafting and revision.
% DISAPPEARANCE_RATIONALE: If the supremacy clause and mandatory harmonization vanished, domestic regulatory agencies would regain full jurisdiction, capital would face differentiated national standards, and the architecture of ISDS would lose its overriding forceâthe global investment regime would fragment into sovereign regulatory patches.
% FOUNDING_PROBLEM: Post-war fragmented international investment law created uncertainty for cross-border capital; conflicting national regulations raised transaction costs and political risk for investors.
% FOUNDING_PROBLEM_CORROBORATION: International economic institutions and capital-exporting states attest the problem remains live. Labor unions, environmental NGOs, and some Global South governments attest the founding problem has mutated into a mechanism for regulatory chill and sovereignty erosion; independent legal scholarship and critical political economy analysis from outside the benefiting parties support the extraction reading.
narrative_ontology:disappearance_verdict(nafta_jurisdictional_boundary__capital_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(nafta_jurisdictional_boundary__capital_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__capital_supremacy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nafta_jurisdictional_boundary__capital_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.72, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.72) is high because the constraint systematically moves regulatory authority from domestic public institutions to capital mobility beneficiaries, evidenced by ISDS awards and regulatory chill. Suppression (0.68) reflects the active enforcement infrastructure of ISDS and treaty retaliation mechanisms that prevent exit from harmonization obligations. Theater ratio (0.25) is relatively low because the enforcement is functional and materialâreal money changes hands, real regulations are withdrawnâthough some performative legalization rhetoric exists. Accessibility collapse (0.45) is moderate: alternatives (sovereign exit, renegotiation) exist but are costly and politically difficult. Resistance (0.55) captures the sustained opposition from labor, environmental, and some state actors against ISDS expansion. The temporal series show extraction intensifying as the ISDS jurisprudence matured and the scope of 'indirect expropriation' expanded.
 *
 * PERSPECTIVAL GAP:
 *   The multinational enterprise seat experiences the constraint as a coordination mechanism that reduces political risk and creates predictable rules; the domestic regulatory agency and legislature seats experience the same text as an extraction mechanism that hollows democratic lawmaking. The engine computes this divergence from the structural data: identical treaty text produces opposed directionality depending on whether the agent collects ISDS awards or bears regulatory chill. The arbitrators occupy an analytical-enforcement seat with near-symmetric directionalityâtheir authority depends on maintaining the supremacy reading without themselves being the primary beneficiaries of the extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (multinational_enterprises) are positioned with mobile exit and global scope, deriving low directionality (d near 0.0 â subsidized by the constraint's enforcement). Victims (domestic_regulatory_agencies, legislatures, standards bodies) are institutional but nationally constrained, with no exit from treaty obligations they did not design, yielding high directionality (d near 1.0 â targeted extraction). The arbitrators sit between: they enforce but do not capture the gains, producing mid-range d.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the arrangement as pure coordination (rope would ignore the domestic victim set) or pure extraction (snare would deny the genuine trade-and-investment coordination function). The R5 genealogy interview is critical: the founding problemâfragmented investment law and political riskâwas real, but its status is contested. If the problem is dead (solved by mature markets) and the constraint persists, the mandatrophy path would trend toward piton or snare. The authored metrics (rising extractiveness, moderate theater) keep the classification in tangled_rope territory: coordination and extraction are structurally coupled, not sequentially replaced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capital_supremacy_covers_extraction,
    'Is treaty supremacy over domestic regulatory standards a necessary legal principle for international economic coordination, or a constructed mechanism for transferring democratic authority to mobile capital?',
    'Comparative analysis of trade regimes that operate without ISDS supremacy clauses (e.g., post-reform USMCA sectoral exemptions, Brazilian model investment agreements) to determine whether capital mobility persists without hierarchical override.',
    'If capital mobility persists without supremacy, the extraction component is separable from the coordination function and the constraint tilts toward snare; if supremacy is structurally necessary, the tangled_rope classification is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_supremacy_covers_extraction, conceptual, 'Whether treaty supremacy is necessary coordination or constructed extraction').

omega_variable(
    regulatory_chill_empirical_magnitude,
    'Does the constraint produce measurable regulatory chill, or is the chilling effect theoretically hypothesized but empirically unsubstantiated at scale?',
    'Quantitative studies comparing regulatory adoption rates in ISDS-exposed sectors versus non-exposed sectors within signatory states, controlling for income and political orientation.',
    'Documented chill would raise extractiveness and support the victim structure; absence of chill would suggest the extraction is latent or discursive rather than operational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_chill_empirical_magnitude, empirical, 'Empirical magnitude of regulatory chill from treaty supremacy').

omega_variable(
    isds_legitimacy_crisis_structure,
    'Is the contemporary backlash against ISDS a cyclical political fluctuation, or does it represent structural delegitimation of the capital supremacy framework?',
    'Track treaty renegotiation patterns, state withdrawals from ICSID, and replacement of ISDS with state-state mechanisms over a ten-year window.',
    'If structural, the drift_state direction shifts toward repudiation_pressure and the constraint may undergo type transition; if cyclical, the authority_erosion is temporary and the tangled_rope persists.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(isds_legitimacy_crisis_structure, empirical, 'Whether ISDS backlash is cyclical or structural delegitimation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__capital_supremacy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nafta_capital_supremacy_tr_t0, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(nafta_capital_supremacy_tr_t6, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 6, 0.13).
narrative_ontology:measurement(nafta_capital_supremacy_tr_t12, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 12, 0.16).
narrative_ontology:measurement(nafta_capital_supremacy_tr_t18, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 18, 0.2).
narrative_ontology:measurement(nafta_capital_supremacy_tr_t24, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 24, 0.23).
narrative_ontology:measurement(nafta_capital_supremacy_tr_t30, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 30, 0.25).

% Extraction over time
narrative_ontology:measurement(nafta_capital_supremacy_be_t0, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(nafta_capital_supremacy_be_t6, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(nafta_capital_supremacy_be_t12, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 12, 0.6).
narrative_ontology:measurement(nafta_capital_supremacy_be_t18, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 18, 0.66).
narrative_ontology:measurement(nafta_capital_supremacy_be_t24, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 24, 0.7).
narrative_ontology:measurement(nafta_capital_supremacy_be_t30, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 30, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(nafta_capital_supremacy_su_t0, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(nafta_capital_supremacy_su_t6, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 6, 0.52).
narrative_ontology:measurement(nafta_capital_supremacy_su_t12, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 12, 0.58).
narrative_ontology:measurement(nafta_capital_supremacy_su_t18, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 18, 0.63).
narrative_ontology:measurement(nafta_capital_supremacy_su_t24, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 24, 0.66).
narrative_ontology:measurement(nafta_capital_supremacy_su_t30, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nafta_jurisdictional_boundary__capital_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, nafta_jurisdictional_boundary__embedded_liberalism_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, nafta_jurisdictional_boundary__sovereignty_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is the capital_supremacy_reading of the nafta_jurisdictional_boundary kernel, decomposed from the colloquial label 'trade agreement supremacy' per the Îµ-invariance principle. Sibling readings instantiate structurally distinct constraints with different Îµ values and victim/beneficiary structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
