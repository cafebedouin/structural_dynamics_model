% ============================================================================
% CONSTRAINT STORY: substance_control_authority__legalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_authority__legalization_reading, []).

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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: substance_control_authority__legalization_reading
 *   human_readable: State Authority to Regulate Drug Markets as Legal Commerce
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   State authority to regulate psychoactive substances as legal commercial
 *   products, with quality controls, taxation, and licensed distribution,
 *   replacing criminal prohibition with a market-regulation framework. This
 *   is the legalization reading of the substance_control_authority kernel. It
 *   asserts that the state's proper role is not to criminalize use but to
 *   manage supply through commercial regulation, protecting consumers via
 *   quality standards and third parties via taxation and access limits. The
 *   constraint coordinates a legal market while extracting tax revenue and
 *   enforcing a sharp boundary between licensed and unlicensed commerce.
 *
 * KEY AGENTS:
 *   - state_regulatory_authority: Agenda-setter (institutional/generational/constrained) â designs and enforces the legal market framework, collects tax revenue, and expands regulatory jurisdiction.
 *   - licensed_vendors: Beneficiary (moderate/biographical/constrained) â gain protected legal market position in exchange for compliance and tax burden.
 *   - regulated_market_consumers: Beneficiary (organized/biographical/constrained) â receive quality-assured supply but pay tax premiums and accept access surveillance.
 *   - unlicensed_suppliers: Payer (powerless/immediate/trapped) â criminalized by the regulatory boundary, targeted by enforcement that protects licensed incumbents.
 *   - affected_communities: Payer (organized/generational/constrained) â bear uncompensated externalities from any use-volume increase and spatial concentration of access points.
 *   - public_health_systems: Beneficiary (institutional/generational/constrained) â receive tax funding and reduced overdose burden, but must manage volume-related chronic care.
 *   - prohibitionist_institutions: Excluded (institutional/generational/constrained) â lose mandate and budget priority under the new regulatory frame, structurally sidelined in design.
 *   - comparative_policy_analysts: Observer (analytical/civilizational/analytical) â comparative evaluators without direct stake in the arrangement.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__legalization_reading, 0.55).
domain_priors:suppression_score(substance_control_authority__legalization_reading, 0.6).
domain_priors:theater_ratio(substance_control_authority__legalization_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__legalization_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_authority__legalization_reading, "State Authority to Regulate Drug Markets as Legal Commerce").
narrative_ontology:topic_domain(substance_control_authority__legalization_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_authority__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__legalization_reading, '82864d14-9fde-44df-9179-f693d786173d').
narrative_ontology:cs_kernel_codification('82864d14-9fde-44df-9179-f693d786173d', formalized).
narrative_ontology:cs_authority_grounding('82864d14-9fde-44df-9179-f693d786173d', lineage).
narrative_ontology:cs_interpretation_layer_present('82864d14-9fde-44df-9179-f693d786173d').
narrative_ontology:cs_reading_relation('82864d14-9fde-44df-9179-f693d786173d', substance_control_authority__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('82864d14-9fde-44df-9179-f693d786173d', substance_control_authority__harm_reduction_reading, coexists_with).
narrative_ontology:cs_axiom('82864d14-9fde-44df-9179-f693d786173d', foundational, state_commercial_regulation_authority).
narrative_ontology:cs_axiom_status(state_commercial_regulation_authority, holdable).
narrative_ontology:cs_axiom_grounding('82864d14-9fde-44df-9179-f693d786173d', state_commercial_regulation_authority, conventional).
narrative_ontology:cs_axiom('82864d14-9fde-44df-9179-f693d786173d', foundational, consumer_quality_protection_mandate).
narrative_ontology:cs_axiom_status(consumer_quality_protection_mandate, holdable).
narrative_ontology:cs_axiom_grounding('82864d14-9fde-44df-9179-f693d786173d', consumer_quality_protection_mandate, instrumental).
narrative_ontology:cs_reference_frame('82864d14-9fde-44df-9179-f693d786173d', regulated_commerce_public_health).
narrative_ontology:cs_drift_state('82864d14-9fde-44df-9179-f693d786173d', post_legalization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('82864d14-9fde-44df-9179-f693d786173d', '').
narrative_ontology:cs_kernel_id(substance_control_authority__legalization_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, licensed_vendors).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, regulated_market_consumers).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, public_health_systems).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, state_regulatory_authority).
narrative_ontology:constraint_victim(substance_control_authority__legalization_reading, unlicensed_suppliers).
narrative_ontology:constraint_victim(substance_control_authority__legalization_reading, affected_communities).
narrative_ontology:constraint_vindicates(substance_control_authority__legalization_reading, consumer_protection_through_regulation).
narrative_ontology:constraint_vindicates(substance_control_authority__legalization_reading, taxed_legal_market_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets licensing criteria, product quality standards, taxation rates, and access controls for psychoactive substances. Enforces the regulatory boundary through inspections, product testing, and policing of unlicensed supply. Collects tax revenue and expands administrative jurisdiction over a previously criminalized domain.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, state_regulatory_authority, agenda_setter,
    institutional, generational, constrained, national).

% Operate legally under state-issued licenses to produce and sell regulated substances. Benefit from displaced criminal competition and consumer confidence in legal product safety. Pay taxes and compliance costs; their market position depends entirely on maintaining regulatory favor and license renewal.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, licensed_vendors, beneficiary,
    moderate, biographical, constrained, national).

% Purchase psychoactive substances through licensed channels, receiving quality-assured products with labeled potency and safety information. Pay a tax premium over production cost and face access restrictions such as age limits, purchase caps, and zoning. Their practical alternative is returning to unregulated supply if the legal market is inaccessible or too expensive.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, regulated_market_consumers, beneficiary,
    organized, biographical, constrained, national).

% Previously operated in black markets; under legalization they are criminalized as unlicensed competitors and targeted by enforcement that protects licensed vendors. Face asset seizure, imprisonment, and market exclusion. Their expertise and distribution networks have few legitimate outlets due to licensing barriers and capital requirements.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, unlicensed_suppliers, payer,
    powerless, immediate, trapped, national).

% Bear externalities from any increase in substance use volume resulting from normalized legal access, including public consumption, impaired driving incidents, and localized health costs. Receive some mitigation via tax-funded services but do not control the regulatory setting or licensing density. Cannot easily exit the jurisdiction to avoid spillovers.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, affected_communities, payer,
    organized, generational, constrained, regional).

% Receive dedicated tax revenue from regulated sales to fund treatment and prevention programs. Experience reduced acute burden from adulterant overdoses as supply quality stabilizes. Must still manage any increase in overall use volume and associated chronic health conditions within fixed budgets.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, public_health_systems, beneficiary,
    institutional, generational, constrained, national).

% Law enforcement agencies and drug-control bureaucracies whose mission and budgets were organized around criminalization. Under legalization they lose enforcement mandate, interdiction funding, and institutional priority. They would argue for maintaining prohibition but are structurally sidelined in the regulatory design process, though they may resist politically.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, prohibitionist_institutions, excluded,
    institutional, generational, constrained, national).

% Evaluate the regulatory framework against outcomes in other jurisdictions, tracking use-volume trends, overdose rates, black-market persistence, and fiscal flows. Do not collect from or pay into the arrangement; their seat is analytical.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, comparative_policy_analysts, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a legal supply chain for psychoactive substances by substituting state-licensed commerce for unregulated criminal markets, establishing product quality standards, and channeling consumption through regulated access points subject to known rules.
% TRANSFER_FUNCTION: Moves revenue from consumers and licensed vendors to the state via taxation and licensing fees; moves regulatory compliance costs from vendors to state oversight apparatus; moves criminal suppliers from protected black-market position to excluded and enforced-against status.
% ABSENT_VOICES: Unlicensed suppliers and criminal network participants are excluded from regulatory design; additionally, future generations bearing long-term public health effects from increased normalization have no seat at the founding decision.
% DISAPPEARANCE_RATIONALE: If the state authority to regulate legal drug commerce vanished overnight, the licensed market would collapse, tax revenues would disappear, quality assurances would evaporate, and criminal supply networks would rapidly reconstitute to meet persistent demand â the arrangement organizes substantial economic and social activity around it.
% FOUNDING_PROBLEM: Unregulated drug supply causes preventable overdose deaths, funds organized crime, and exposes consumers to unknown potency and contaminants; prohibitionist frameworks generate secondary harms including mass incarceration, criminal records, and violent illicit markets without eliminating demand.
% FOUNDING_PROBLEM_CORROBORATION: Independent epidemiologists and criminal justice researchers outside the state regulatory and licensed-vendor beneficiary set corroborate the mortality and fiscal costs of unregulated supply and prohibition. However, international drug control treaty bodies and domestic law enforcement institutions contest that legalization is the appropriate response, providing external corroboration that the problem framing itself is politically disputed.
narrative_ontology:disappearance_verdict(substance_control_authority__legalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_authority__legalization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_authority__legalization_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(substance_control_authority__legalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_authority__legalization_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_authority__legalization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_authority__legalization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_authority__legalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.55) because taxation and licensing create real transfers from consumers and vendors to the state, and regulatory barriers generate rents for licensed incumbents. Suppression (0.60) is active: the legal market requires continuous enforcement against unlicensed supply to maintain price and quality differentials. Theater ratio is moderate-low (0.25): most regulatory activity is functional, but some enforcement is theatrical (high-profile raids on unlicensed sellers to demonstrate regulatory seriousness). Accessibility collapse is moderate (0.50): the unregulated market does not fully disappear because tax and access restrictions preserve demand for illegal alternatives. Resistance (0.55) reflects ongoing opposition from criminal networks, prohibitionist institutions, and communities concerned about use-volume externalities. Measurements share a single time grid to prevent misaligned substitution.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (state regulatory authority) experiences the constraint as a successful coordination mechanism that generates revenue and public health benefits. The payer seats (unlicensed suppliers, affected communities) experience it as structural exclusion and externality burden. The beneficiary seats (licensed vendors, consumers, public health systems) experience mixed effects: vendors gain protected market position but lose autonomy to regulators; consumers gain safety but pay tax premiums and accept access surveillance; public health gains funding but absorbs volume-related costs. The engine computes these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   State regulatory authority is a concentrated beneficiary (low d) through tax capture and jurisdictional expansion. Licensed vendors are moderate beneficiaries (low-moderate d) through barrier-protected rents. Public health systems are diffuse beneficiaries. Regulated consumers sit near symmetric (moderate d): they receive genuine quality coordination but pay extraction embedded in price. Unlicensed suppliers are full targets (high d): the constraint's existence criminalizes their activity and directs enforcement at them. Affected communities are targets (high d) via uncompensated externalities. No directionality overrides are needed because beneficiary/victim declarations and exit options capture the structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents misreading the constraint as either pure coordination (rope) or pure extraction (snare). The genuine coordination function â quality assurance, known potency, displacement of violent criminal markets â is real and benefits identifiable agents. However, the asymmetric extraction through taxation, licensing scarcity, and the deliberate suppression of unlicensed alternatives means the coordination story is not cover for extraction, nor is extraction incidental to coordination. Both are structurally necessary to the constraint's operation: without enforcement against unlicensed supply, the legal market cannot sustain its tax and quality premium. The mandatrophy risk would be labeling this a scaffold (it has no sunset clause) or a piton (it has clear beneficiaries and functional purpose). The type is tangled_rope because both coordination and extraction are present and coupled through the same enforcement structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    black_market_resilience,
    'Does the regulated market eliminate illegal supply, or does tax-driven price differentiation preserve a resilient black market?',
    'Comparative analysis of jurisdictions with varying tax rates; measurement of unlicensed supply share post-legalization.',
    'If black markets persist substantially, the constraint''s suppression is higher than apparent and its coordination function is weaker; if eliminated, the extraction is primarily the tax premium itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(black_market_resilience, empirical, 'Whether legalization displaces or merely reshapes illegal supply.').

omega_variable(
    use_volume_externality,
    'Does legal commercial availability increase total use volume, and if so, do the resulting externalities constitute an extractive transfer to affected communities?',
    'Longitudinal epidemiological tracking of use prevalence and attributable harms in jurisdictions pre- and post-legalization, compared against tax revenue allocated to harm mitigation.',
    'If volume rises and externalities exceed tax-funded mitigation, the constraint is more extractive than the coordination story suggests; if volume is stable or mitigation is sufficient, the beneficiary structure is more symmetric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(use_volume_externality, empirical, 'Whether increased use volume creates uncompensated community externalities.').

omega_variable(
    kernel_reading_boundary,
    'Is the legalization reading of substance control authority structurally separable from the harm_reduction reading, or do they collapse into a single policy framework in practice?',
    'Analysis of whether jurisdictions adopting legalization simultaneously adopt harm-reduction infrastructure (safe supply, consumption sites) or treat commercialization as a substitute for public health intervention.',
    'If inseparable, the constraint''s coordination function is partly attributable to harm-reduction mechanisms outside the commercial frame; if separable, the legalization reading stands as an independent constraint with distinct epsilon.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether legalization and harm reduction readings merge in implementation.').

omega_variable(
    regulatory_capture_risk,
    'Does the licensing framework capture regulatory benefits for a small number of large commercial producers, converting public health coordination into concentrated extraction?',
    'Market concentration analysis of licensed vendors; comparison of licensing barriers to entry against public health necessity.',
    'High capture would shift the constraint toward snare-like concentration; low capture supports the tangled_rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_risk, empirical, 'Whether licensing creates oligopoly rents versus open coordination.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__legalization_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_authority__legalization_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(subs_tr_t4, substance_control_authority__legalization_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement(subs_tr_t8, substance_control_authority__legalization_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(subs_tr_t12, substance_control_authority__legalization_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(subs_tr_t16, substance_control_authority__legalization_reading, theater_ratio, 16, 0.25).
narrative_ontology:measurement(subs_tr_t20, substance_control_authority__legalization_reading, theater_ratio, 20, 0.25).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_authority__legalization_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(subs_be_t4, substance_control_authority__legalization_reading, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(subs_be_t8, substance_control_authority__legalization_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(subs_be_t12, substance_control_authority__legalization_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(subs_be_t16, substance_control_authority__legalization_reading, base_extractiveness, 16, 0.56).
narrative_ontology:measurement(subs_be_t20, substance_control_authority__legalization_reading, base_extractiveness, 20, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_authority__legalization_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(subs_su_t4, substance_control_authority__legalization_reading, suppression_requirement, 4, 0.66).
narrative_ontology:measurement(subs_su_t8, substance_control_authority__legalization_reading, suppression_requirement, 8, 0.63).
narrative_ontology:measurement(subs_su_t12, substance_control_authority__legalization_reading, suppression_requirement, 12, 0.6).
narrative_ontology:measurement(subs_su_t16, substance_control_authority__legalization_reading, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(subs_su_t20, substance_control_authority__legalization_reading, suppression_requirement, 20, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_authority__legalization_reading, resource_allocation).
narrative_ontology:affects_constraint(substance_control_authority__legalization_reading, substance_control_authority__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_authority__legalization_reading, substance_control_authority__harm_reduction_reading).

% DUAL FORMULATION NOTE:
% This constraint is the legalization reading of the substance_control_authority kernel, decomposing the state's authority over psychoactive substances into three structurally distinct claims: criminalization (prohibition), public health management (harm reduction), and regulated commerce (legalization). Each reading carries a distinct epsilon, beneficiary/victim structure, and empirical foundation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
