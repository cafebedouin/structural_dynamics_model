% ============================================================================
% CONSTRAINT STORY: ai_governance_legitimacy__market_libertarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_governance_legitimacy__market_libertarian_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: ai_governance_legitimacy__market_libertarian_reading
 *   human_readable: Market Libertarian Reading of AI Governance Legitimacy
 *   domain: theological ethics / technology governance / political theology
 *
 * SUMMARY:
 *   This constraint story instantiates the market libertarian reading of the
 *   ai_governance_legitimacy kernel. It treats property rights and voluntary
 *   exchange as pre-political foundations for AI governance, framing
 *   collective mandatesâincluding the encyclical's solidarity demandsâas
 *   illegitimate coercion. The reading claims mountain status (natural law)
 *   but carries beneficiaries and victims that trigger false-summit
 *   evaluation. The authored metrics (low extraction, low suppression, high
 *   accessibility collapse) are descriptively true of the reading's own
 *   self-understanding, not tuned to match the claimed type. The engine will
 *   compute whether the mountain claim survives contact with the structural
 *   data.
 *
 * KEY AGENTS:
 *   - ai_entrepreneurs: Primary beneficiary (powerful/mobile) â gains autonomy and reduced regulatory burden under property-rights framing
 *   - venture_investors: Primary beneficiary (institutional/arbitrage) â secures governance environment favoring capital returns
 *   - high_autonomy_individuals: Secondary beneficiary (moderate/mobile) â benefits from low-friction market choice
 *   - low_market_power_actors: Primary target (powerless/trapped) â bears externalized coordination costs without voice
 *   - coordination_failure_communities: Primary target (powerless/trapped) â denied collective-governance solutions for public goods
 *   - monopsony_workers: Primary target (powerless/constrained) â faces asymmetric labor market power under freedom-of-contract framing
 *   - solidarity_advocates: Excluded voice (organized/constrained) â defined out of legitimate discourse
 *   - political_theologians: Analytical observer (analytical/analytical) â maps the kernel's competing readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_legitimacy__market_libertarian_reading, 0.25).
domain_priors:suppression_score(ai_governance_legitimacy__market_libertarian_reading, 0.22).
domain_priors:theater_ratio(ai_governance_legitimacy__market_libertarian_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__market_libertarian_reading, mountain).
narrative_ontology:human_readable(ai_governance_legitimacy__market_libertarian_reading, "Market Libertarian Reading of AI Governance Legitimacy").
narrative_ontology:topic_domain(ai_governance_legitimacy__market_libertarian_reading, "theological ethics / technology governance / political theology").

domain_priors:emerges_naturally(ai_governance_legitimacy__market_libertarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__market_libertarian_reading, '2d9df3c8-cfb7-4c0f-9d27-34fa5ee704ff').
narrative_ontology:cs_kernel_codification('2d9df3c8-cfb7-4c0f-9d27-34fa5ee704ff', implicit).
narrative_ontology:cs_authority_grounding('2d9df3c8-cfb7-4c0f-9d27-34fa5ee704ff', self_enforcing).
narrative_ontology:cs_reading_relation('2d9df3c8-cfb7-4c0f-9d27-34fa5ee704ff', ai_governance_legitimacy__magisterial_subsidiarity_reading, forecloses).
narrative_ontology:cs_reading_relation('2d9df3c8-cfb7-4c0f-9d27-34fa5ee704ff', ai_governance_legitimacy__technocratic_optimization_reading, coexists_with).
narrative_ontology:cs_reading_relation('2d9df3c8-cfb7-4c0f-9d27-34fa5ee704ff', ai_governance_legitimacy__democratic_pluralist_reading, forecloses).
narrative_ontology:cs_axiom('2d9df3c8-cfb7-4c0f-9d27-34fa5ee704ff', foundational, property_rights_pre_political).
narrative_ontology:cs_axiom_status(property_rights_pre_political, holdable).
narrative_ontology:cs_axiom_grounding('2d9df3c8-cfb7-4c0f-9d27-34fa5ee704ff', property_rights_pre_political, deontological).
narrative_ontology:cs_axiom('2d9df3c8-cfb7-4c0f-9d27-34fa5ee704ff', foundational, collective_mandates_illegitimate).
narrative_ontology:cs_axiom_status(collective_mandates_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('2d9df3c8-cfb7-4c0f-9d27-34fa5ee704ff', collective_mandates_illegitimate, deontological).
narrative_ontology:cs_reference_frame('2d9df3c8-cfb7-4c0f-9d27-34fa5ee704ff', pre_political_property_order).
narrative_ontology:cs_drift_state('2d9df3c8-cfb7-4c0f-9d27-34fa5ee704ff', post_encyclical_ai_governance, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('2d9df3c8-cfb7-4c0f-9d27-34fa5ee704ff', '').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, ai_entrepreneurs).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, venture_investors).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, high_autonomy_individuals).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, low_market_power_actors).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, coordination_failure_communities).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, monopsony_workers).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__market_libertarian_reading, voluntary_exchange_doctrine).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__market_libertarian_reading, pre_political_property_rights).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and deploy AI systems under a governance framework that privileges contractual autonomy and resists licensing or safety mandates. They retain equity and decision rights without collective oversight.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, ai_entrepreneurs, beneficiary,
    powerful, biographical, mobile, global).

% Fund AI ventures under property-rights frameworks that maximize return on capital and limit fiduciary exposure to redistributive or regulatory claims. Exit through portfolio diversification and jurisdictional shopping.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, venture_investors, beneficiary,
    institutional, generational, arbitrage, global).

% Use AI tools and platforms with minimal identity verification or behavioral reporting. Benefit from competitive choice among services and low switching costs where markets are thick.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, high_autonomy_individuals, beneficiary,
    moderate, biographical, mobile, national).

% Rely on AI-mediated services but lack capital or information to negotiate terms. Subject to unilateral platform changes and data extraction without reciprocal voice in governance.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, low_market_power_actors, payer,
    powerless, immediate, trapped, local).

% Face collective-action problems such as environmental monitoring or public-health surveillance that markets underprovide. Cannot fund or coordinate AI solutions without either state action or charitable intervention, both of which the framework delegitimizes.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, coordination_failure_communities, payer,
    powerless, generational, trapped, regional).

% Supply labor to AI-intensive firms with few alternative employers. Their bargaining power is diluted by non-compete clauses and algorithmic management justified under freedom of contract.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, monopsony_workers, payer,
    powerless, biographical, constrained, national).

% Advance claims for common-good oversight and redistribution of AI-derived surplus. Their arguments are classified as illegitimate coercion under the reading's axioms and are structurally absent from legitimating discourse.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, solidarity_advocates, excluded,
    organized, generational, constrained, global).

% Analyze the contest between pre-political property claims and solidarity-based governance. They map how each reading of the kernel allocates dignity, authority, and vulnerability differently.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, political_theologians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Decentralizes AI governance by replacing collective political mandates with voluntary contracting and property-rights enforcement, coordinating dispersed innovation without centralized deliberation.
% TRANSFER_FUNCTION: Transfers governance authority from democratic and religious institutions to private contract parties and market mechanisms, concentrating decision rights with capital holders and high-autonomy users while externalizing coordination costs to low-power actors.
% ABSENT_VOICES: Workers in monopsony labor markets, communities facing coordination failures, and advocates of solidarity-based redistribution are excluded; their demands are reframed as coercive rather than legitimate governance inputs.
% DISAPPEARANCE_RATIONALE: Libertarian proponents would argue that removing this reading collapses the protective architecture of property rights and invites tyrannical oversight; critics would argue that AI governance would simply reorganize around democratic or technocratic legitimacy with no loss of function. The kernel's other readings ensure continuity, but this specific seat would experience rearrangement.
% FOUNDING_PROBLEM: How to legitimate AI governance without subordinating economic freedom and innovation to collective political or religious authority.
% FOUNDING_PROBLEM_CORROBORATION: Libertarian legal scholars and public-choice economists attest to the problem from within the tradition. No corroboration exists from democratic-pluralist, technocratic, or magisterial seats, who dispute that collective authority constitutes a problem at all.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__market_libertarian_reading, contested).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__market_libertarian_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__market_libertarian_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_governance_legitimacy__market_libertarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_governance_legitimacy__market_libertarian_reading, 0.25, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_governance_legitimacy__market_libertarian_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, ExtMetricName, E),
    domain_priors:suppression_score(ai_governance_legitimacy__market_libertarian_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(ai_governance_legitimacy__market_libertarian_reading),
    narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(ai_governance_legitimacy__market_libertarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.25 because the constraint extracts primarily through opportunity denial (foregone collective governance) rather than direct rent. Suppression is 0.22 because enforcement is decentralized through contract and reputation rather than centralized coercion. Accessibility collapse is high (0.75) because once property rights are accepted as pre-political, collective alternatives become cognitively unavailable. Resistance is low (0.25) because the framing presents itself as natural law, deflecting active opposition into apparently quixotic campaigns against gravity. Theater ratio is modest (0.20) and rising: as political and religious authorities assert AI oversight, libertarian defenses become more performative.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (entrepreneurs, investors, high-autonomy individuals) experience the constraint as protective mountain or benign coordination. The payer seats (low-market-power actors, coordination-failure communities, monopsony workers) experience the same structure as invisible extractionâgovernance legitimacy that systematically ignores their vulnerability. The excluded solidarity advocates experience it as an epistemic snare that defines their concerns out of existence.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries derive low directionality because the constraint subsidizes their autonomy and capital accumulation. Victims derive high directionality because the constraint extracts from them through opportunity denial and asymmetric bargaining power. The observer seat sits at analytical distance with neutral directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling by requiring active enforcement for tangled_rope and victims for snare. This reading claims mountain status, which would normally preclude victims; their presence forces the engine to test false-summit detection. If the founding problem (resisting collective authority) is live but the constraint has aged into defending entrenched platform power, the mandatrophy mismatch (dead problem plus world_rearranges) would flag piton drift. Currently authored as live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_or_constructed,
    'Is the pre-political status of property rights in AI governance a genuine natural law, or a constructed constraint that benefits market-active agents?',
    'Comparative institutional analysis and anthropological evidence on the political construction of property regimes; examination of whether AI property rights emerge spontaneously or require state enforcement.',
    'If constructed, the mountain claim fails and FSM reclassifies toward tangled_rope or snare, raising effective extraction for victim seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_or_constructed, conceptual, 'Whether property rights in AI governance are natural or constructed').

omega_variable(
    sibling_reading_structural_delta,
    'How would the classification change if the magisterial subsidiarity, technocratic optimization, or democratic pluralist reading of this kernel were adopted instead?',
    'Comparative analysis of sibling constraint stories in the ai_governance_legitimacy family.',
    'Each reading shifts epsilon, beneficiary/victim structures, and authority grounding substantially; the market libertarian reading''s low extraction and mountain claim are unique to this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Structural differences across kernel readings').

omega_variable(
    exit_efficacy_for_low_power,
    'Do competitive markets and exit options actually protect dignity for actors with low market power, or do informational and capital barriers render exit illusory?',
    'Empirical measurement of actual exit costs in AI labor markets and platform ecosystems; monopsony concentration studies.',
    'If exit is illusory for substantial populations, victim-seat extraction is higher than authored and the mountain claim weakens further.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_efficacy_for_low_power, empirical, 'Whether market exit is real or illusory for low-power actors').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__market_libertarian_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aglmr_tr_t0, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(aglmr_tr_t10, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 10, 0.08).
narrative_ontology:measurement(aglmr_tr_t20, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 20, 0.11).
narrative_ontology:measurement(aglmr_tr_t30, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 30, 0.14).
narrative_ontology:measurement(aglmr_tr_t40, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 40, 0.17).
narrative_ontology:measurement(aglmr_tr_t50, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(aglmr_be_t0, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(aglmr_be_t10, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 10, 0.15).
narrative_ontology:measurement(aglmr_be_t20, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 20, 0.18).
narrative_ontology:measurement(aglmr_be_t30, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 30, 0.2).
narrative_ontology:measurement(aglmr_be_t40, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 40, 0.23).
narrative_ontology:measurement(aglmr_be_t50, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 50, 0.25).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(ai_governance_legitimacy__market_libertarian_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_legitimacy__market_libertarian_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_governance_legitimacy__market_libertarian_reading, magisterial_subsidiarity_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__market_libertarian_reading, technocratic_optimization_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__market_libertarian_reading, democratic_pluralist_reading).

% DUAL FORMULATION NOTE:
% The ai_governance_legitimacy kernel decomposes into four structurally distinct readings. Each reading emits a different constraint with distinct epsilon, beneficiary/victim structure, and authority grounding. This story instantiates the market libertarian reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
