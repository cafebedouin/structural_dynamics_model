% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__commons_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_control_legitimacy__commons_reading, []).

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
 *   constraint_id: software_control_legitimacy__commons_reading
 *   human_readable: Software as Digital Commons Governance
 *   domain: software_engineering/political_economy_of_technology/intellectual_property
 *
 * SUMMARY:
 *   This constraint represents the 'commons reading' of software control,
 *   asserting that software and digital infrastructure are shared resources
 *   requiring negotiated collective management, rather than absolute freedom
 *   or absolute property. This reading posits that both absolutist positions
 *   (unfettered freedom and exclusive property rights) are 'victims' in the
 *   sense that their preferred modes of operation are constrained by the
 *   collective governance framework. The constraint functions as a Tangled
 *   Rope, providing genuine coordination for sustainable digital commons
 *   while extracting compliance from those who resist its collective rules.
 *
 * KEY AGENTS:
 *   - digital_commons_stewards: Agenda setter (institutional/constrained) — manages the commons.
 *   - stakeholder_communities: Beneficiary (organized/constrained) — participates in governance, benefits from shared resources.
 *   - digital_commons_users: Beneficiary (moderate/mobile) — benefits from access to shared infrastructure.
 *   - absolute_freedom_advocates: Payer (powerful/identity_locked) — resists governance, views rules as illegitimate.
 *   - absolute_property_advocates: Payer (institutional/arbitrage) — resists collective management, seeks enclosure.
 *   - policy_makers: Observer (institutional/analytical) — considers regulatory frameworks.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__commons_reading, 0.45).
domain_priors:suppression_score(software_control_legitimacy__commons_reading, 0.6).
domain_priors:theater_ratio(software_control_legitimacy__commons_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__commons_reading, tangled_rope).
narrative_ontology:human_readable(software_control_legitimacy__commons_reading, "Software as Digital Commons Governance").
narrative_ontology:topic_domain(software_control_legitimacy__commons_reading, "software_engineering/political_economy_of_technology/intellectual_property").

domain_priors:requires_active_enforcement(software_control_legitimacy__commons_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__commons_reading, '4818a0c5-89cd-4e00-bb7b-3cef3de2707d').
narrative_ontology:cs_kernel_codification('4818a0c5-89cd-4e00-bb7b-3cef3de2707d', formalized).
narrative_ontology:cs_authority_grounding('4818a0c5-89cd-4e00-bb7b-3cef3de2707d', practice).
narrative_ontology:cs_interpretation_layer_present('4818a0c5-89cd-4e00-bb7b-3cef3de2707d').
narrative_ontology:cs_reading_relation('4818a0c5-89cd-4e00-bb7b-3cef3de2707d', software_control_legitimacy__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('4818a0c5-89cd-4e00-bb7b-3cef3de2707d', software_control_legitimacy__pragmatic_openness_reading, coexists_with).
narrative_ontology:cs_reading_relation('4818a0c5-89cd-4e00-bb7b-3cef3de2707d', software_control_legitimacy__property_rights_reading, coexists_with).
narrative_ontology:cs_axiom('4818a0c5-89cd-4e00-bb7b-3cef3de2707d', foundational, software_is_shared_resource).
narrative_ontology:cs_axiom_status(software_is_shared_resource, holdable).
narrative_ontology:cs_axiom_grounding('4818a0c5-89cd-4e00-bb7b-3cef3de2707d', software_is_shared_resource, conventional).
narrative_ontology:cs_axiom('4818a0c5-89cd-4e00-bb7b-3cef3de2707d', foundational, collective_governance_is_legitimate).
narrative_ontology:cs_axiom_status(collective_governance_is_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('4818a0c5-89cd-4e00-bb7b-3cef3de2707d', collective_governance_is_legitimate, deontological).
narrative_ontology:cs_reference_frame('4818a0c5-89cd-4e00-bb7b-3cef3de2707d', sustainable_collective_stewardship).
narrative_ontology:cs_drift_state('4818a0c5-89cd-4e00-bb7b-3cef3de2707d', contemporary_digital_economy, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4818a0c5-89cd-4e00-bb7b-3cef3de2707d', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__commons_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__commons_reading, stakeholder_communities).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__commons_reading, digital_commons_users).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, absolute_freedom_advocates).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, absolute_property_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organizations and individuals dedicated to establishing and maintaining governance structures for digital commons. They define rules, mediate disputes, and enforce community norms to ensure sustainability and prevent enclosure or tragedy of the commons.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, digital_commons_stewards, agenda_setter,
    institutional, generational, constrained, global).

% Groups of developers, users, and contributors who actively participate in the governance and development of specific digital commons projects. They benefit from shared resources and collective decision-making but must adhere to community rules.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, stakeholder_communities, beneficiary,
    organized, biographical, constrained, global).

% Individuals who utilize software and digital infrastructure managed as a commons. They benefit from access to shared, often free or low-cost, resources and the stability provided by collective governance, but have limited direct influence on rules.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, digital_commons_users, beneficiary,
    moderate, immediate, mobile, global).

% Individuals and groups who believe software should be entirely free from any control or governance, including collective management. They view any rules or restrictions, even those for commons sustainability, as illegitimate constraints on user freedom, bearing the 'cost' of having their preferred mode of operation suppressed.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, absolute_freedom_advocates, payer,
    powerful, generational, identity_locked, global).

% Corporations and legal entities that assert strong, exclusive intellectual property rights over software. They view collective management as an infringement on their right to control and profit from their creations, bearing the 'cost' of having their enclosure efforts resisted by the commons framework.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, absolute_property_advocates, payer,
    institutional, generational, arbitrage, global).

% Government bodies and international organizations that consider legal and regulatory frameworks for software and digital infrastructure. They observe the tensions between different control paradigms and may intervene to support or constrain specific models.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, policy_makers, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To enable sustainable, equitable, and innovative development and use of shared digital infrastructure by balancing individual contributions with collective needs, preventing both enclosure by private interests and degradation from unfettered use.
% TRANSFER_FUNCTION: Transfers governance authority from individual developers/corporations to collective bodies, and distributes the costs and benefits of maintaining the commons among participants. It also transfers legitimacy from absolute property/freedom claims to a collective management paradigm.
% ABSENT_VOICES: Advocates for absolute software freedom (e.g., those who reject any licensing or governance) and proponents of absolute proprietary control (e.g., those who seek to enclose all digital assets) are structurally excluded from the core governance conversation, as their foundational premises are rejected by the commons framework.
% DISAPPEARANCE_RATIONALE: If the framework for software as a digital commons vanished, digital infrastructure would likely revert to either proprietary enclosure (dominated by large corporations) or chaotic, unsustainable 'free-for-all' models, losing the benefits of managed collective resources. Innovation, access, and equity would be significantly impacted.
% FOUNDING_PROBLEM: The historical tension between proprietary enclosure stifling innovation and open-source models struggling with sustainability, funding, and governance, leading to a search for a third way that balances individual rights with collective responsibility for shared digital resources.
% FOUNDING_PROBLEM_CORROBORATION: Independent legal scholars, digital rights organizations, and some open-source foundations attest to the ongoing challenges of balancing these interests, citing persistent debates over licensing, platform control, and the funding of public digital goods. This corroboration comes from outside the direct beneficiaries of the commons model.
narrative_ontology:disappearance_verdict(software_control_legitimacy__commons_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__commons_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__commons_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(software_control_legitimacy__commons_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__commons_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_control_legitimacy__commons_reading_tests).
:- end_tests(software_control_legitimacy__commons_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) reflecting the ongoing costs of collective management and the 'extraction' of compliance from those who prefer absolutist models. Suppression is moderate (0.60) as active enforcement is required to maintain commons rules against pressures for both enclosure and unregulated use. Theater ratio is low (0.20) because the governance work is largely functional, though community-building and consensus-seeking have performative elements. Resistance is high (0.70) due to persistent challenges from both absolutist camps. The metrics show slight fluctuations over time, reflecting the dynamic and often contested nature of commons governance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of digital commons stewards and stakeholder communities, this framework is a necessary and beneficial coordination mechanism for sustainable digital infrastructure. However, from the perspective of absolute freedom or property advocates, it is an illegitimate imposition that extracts their preferred mode of control. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Digital commons stewards and stakeholder communities are beneficiaries, as they gain from the stability and shared resources provided by collective management. Digital commons users also benefit from access. Absolute freedom advocates and absolute property advocates are victims, as their preferred (and often mutually exclusive) modes of software control are actively suppressed by the commons governance model. Their 'costs' are the limitations placed on their desired actions.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope prevents mislabeling genuine collective management as pure extraction, while acknowledging the costs imposed on those who prefer other models. The coordination function (sustainable digital infrastructure) is real, but the enforcement against absolutist positions creates an extractive dynamic for those who do not align with the commons principles. The 'live' status of the founding problem indicates that the constraint's mandate is still relevant, though its implementation is contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commons_effectiveness_vs_capture,
    'How effective is the actual governance of specific digital commons in preventing capture by powerful interests or degradation by free-riders, relative to its stated goals?',
    'Empirical studies of governance outcomes, resource sustainability, and power distribution within established digital commons projects over time.',
    'If governance is consistently captured or ineffective, the constraint''s effective extractiveness would be higher, potentially reclassifying it closer to a Snare for those excluded from decision-making. If highly effective and equitable, it would lean more towards a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commons_effectiveness_vs_capture, empirical, 'Measures the gap between ideal and actual commons governance outcomes.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of absolutist positions structural (rules of the commons) or internalized (community norms and ideological pressure)?',
    'Post-exit behavior analysis: if individuals or groups continue to self-limit their actions after leaving a specific commons, it suggests internalized suppression. If they immediately revert to absolutist behaviors, it''s primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the ''cost'' of non-compliance is carried by the individual. If purely structural, removing the commons rules would immediately eliminate the suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for non-compliant actors.').

omega_variable(
    kernel_reading_identity,
    'This constraint is the ''commons_reading'' of the ''software_control_legitimacy'' kernel. What would be the structural changes if a sibling reading, such as the ''property_rights_reading'' or ''freedom_imperative_reading'', were adopted as the dominant framework?',
    'Conceptual analysis of the logical implications of each reading''s foundational axioms on software development, distribution, and use, and comparison with historical outcomes under different legal regimes.',
    'Adopting the ''property_rights_reading'' would likely increase extractiveness for users and developers (through licensing fees and restrictions) and suppression of open-source models. Adopting the ''freedom_imperative_reading'' would likely decrease extractiveness for users but could lead to sustainability challenges for developers and potential ''tragedy of the commons'' scenarios for shared infrastructure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Impact of alternative kernel readings on constraint structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__commons_reading, 2000, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(soft_be_t2000, software_control_legitimacy__commons_reading, base_extractiveness, 2000, 0.4).
narrative_ontology:measurement(soft_be_t2005, software_control_legitimacy__commons_reading, base_extractiveness, 2005, 0.42).
narrative_ontology:measurement(soft_be_t2010, software_control_legitimacy__commons_reading, base_extractiveness, 2010, 0.45).
narrative_ontology:measurement(soft_be_t2015, software_control_legitimacy__commons_reading, base_extractiveness, 2015, 0.47).
narrative_ontology:measurement(soft_be_t2020, software_control_legitimacy__commons_reading, base_extractiveness, 2020, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t2000, software_control_legitimacy__commons_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(soft_su_t2005, software_control_legitimacy__commons_reading, suppression_requirement, 2005, 0.58).
narrative_ontology:measurement(soft_su_t2010, software_control_legitimacy__commons_reading, suppression_requirement, 2010, 0.6).
narrative_ontology:measurement(soft_su_t2015, software_control_legitimacy__commons_reading, suppression_requirement, 2015, 0.62).
narrative_ontology:measurement(soft_su_t2020, software_control_legitimacy__commons_reading, suppression_requirement, 2020, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__commons_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, digital_rights_legislation).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, intellectual_property_law).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, open_source_licensing_regimes).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
