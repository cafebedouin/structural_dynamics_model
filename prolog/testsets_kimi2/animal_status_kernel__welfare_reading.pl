% ============================================================================
% CONSTRAINT STORY: animal_status_kernel__welfare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status_kernel__welfare_reading, []).

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
 *   constraint_id: animal_status_kernel__welfare_reading
 *   human_readable: Regulated Property with Welfare Constraints (Welfare Reading)
 *   domain: moral_philosophy/animal_ethics/legal_theory
 *
 * SUMMARY:
 *   This constraint instantiates the welfare_reading of the
 *   animal_status_kernel. It treats animals as sentient beings whose
 *   suffering is morally relevant, retains their legal status as property,
 *   and permits continued human use provided it is regulated to minimize
 *   pain. The reading generates the 'new welfarism' critique from
 *   abolitionists, who argue that welfare reforms make the public comfortable
 *   with 'happy meat' and thereby entrench exploitation. Structurally, the
 *   constraint coordinates ongoing animal use while extracting residual
 *   suffering and bodily autonomy from farmed animals.
 *
 * KEY AGENTS:
 *   - Regulatory agencies: set and enforce welfare standards within property frameworks (institutional/constrained)
 *   - Animal industry operators: primary beneficiaries of continued legal legitimacy (powerful/constrained)
 *   - Farmed animals: primary targets of residual suffering and confinement (powerless/trapped)
 *   - Consumers of animal products: secondary beneficiaries of supply continuity and moral comfort (organized/constrained)
 *   - Abolitionist advocates: excluded voices arguing against property status itself (moderate/constrained)
 *   - Veterinary welfare scientists: analytical observers providing empirical legitimacy (moderate/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status_kernel__welfare_reading, 0.45).
domain_priors:suppression_score(animal_status_kernel__welfare_reading, 0.55).
domain_priors:theater_ratio(animal_status_kernel__welfare_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status_kernel__welfare_reading, tangled_rope).
narrative_ontology:human_readable(animal_status_kernel__welfare_reading, "Regulated Property with Welfare Constraints (Welfare Reading)").
narrative_ontology:topic_domain(animal_status_kernel__welfare_reading, "moral_philosophy/animal_ethics/legal_theory").

domain_priors:requires_active_enforcement(animal_status_kernel__welfare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status_kernel__welfare_reading, '23eff442-8551-4467-95ec-515110635f40').
narrative_ontology:cs_kernel_codification('23eff442-8551-4467-95ec-515110635f40', formalized).
narrative_ontology:cs_authority_grounding('23eff442-8551-4467-95ec-515110635f40', lineage).
narrative_ontology:cs_interpretation_layer_present('23eff442-8551-4467-95ec-515110635f40').
narrative_ontology:cs_reading_relation('23eff442-8551-4467-95ec-515110635f40', animal_status_kernel__property_reading, influences).
narrative_ontology:cs_reading_relation('23eff442-8551-4467-95ec-515110635f40', animal_status_kernel__abolitionist_reading, coexists_with).
narrative_ontology:cs_axiom('23eff442-8551-4467-95ec-515110635f40', foundational, sentience_generates_welfare_duty).
narrative_ontology:cs_axiom_status(sentience_generates_welfare_duty, holdable).
narrative_ontology:cs_axiom_grounding('23eff442-8551-4467-95ec-515110635f40', sentience_generates_welfare_duty, deontological).
narrative_ontology:cs_axiom('23eff442-8551-4467-95ec-515110635f40', foundational, regulated_use_minimizes_net_harm).
narrative_ontology:cs_axiom_status(regulated_use_minimizes_net_harm, holdable).
narrative_ontology:cs_axiom_grounding('23eff442-8551-4467-95ec-515110635f40', regulated_use_minimizes_net_harm, instrumental).
narrative_ontology:cs_reference_frame('23eff442-8551-4467-95ec-515110635f40', regulated_property_normality).
narrative_ontology:cs_drift_state('23eff442-8551-4467-95ec-515110635f40', contemporary_abolitionist_challenge, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('23eff442-8551-4467-95ec-515110635f40', '').
narrative_ontology:cs_kernel_id(animal_status_kernel__welfare_reading, animal_status_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status_kernel__welfare_reading, animal_industry_operators).
narrative_ontology:constraint_beneficiary(animal_status_kernel__welfare_reading, consumers_of_animal_products).
narrative_ontology:constraint_victim(animal_status_kernel__welfare_reading, farmed_animals).
narrative_ontology:constraint_vindicates(animal_status_kernel__welfare_reading, regulated_property_doctrine).
narrative_ontology:constraint_vindicates(animal_status_kernel__welfare_reading, welfare_science_paradigm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and enforce animal welfare standards within property law frameworks. They balance industry viability with public concern for suffering, operating under legislative mandates that assume continued animal use.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, regulatory_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Own and use animals as property for food, research, and entertainment. They bear compliance costs for welfare standards but receive legal legitimacy and social license to continue operations that would otherwise face heavier public opposition.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, animal_industry_operators, beneficiary,
    powerful, biographical, constrained, national).

% Sentient beings subjected to confinement, mutilation, and slaughter under welfare constraints that mitigate but do not eliminate suffering. Their interests are represented by proxies; they cannot exit or advocate.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, farmed_animals, payer,
    powerless, immediate, trapped, local).

% Purchase and consume animal products made available by the continued property-use framework. They benefit from stable supply and lower prices subsidized by externalized animal suffering, and from moral comfort provided by welfare assurances.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, consumers_of_animal_products, beneficiary,
    organized, biographical, constrained, national).

% Argue that property status itself is unjust and welfare reform perpetuates exploitation. They are structurally marginalized in policy processes that frame the question as how to regulate use rather than whether to use at all.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, abolitionist_advocates, excluded,
    moderate, generational, constrained, national).

% Produce the empirical knowledge base about animal suffering that informs welfare regulations. They occupy an analytical seat that legitimates the framework without directly capturing its economic gains or bearing its bodily costs.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, veterinary_welfare_scientists, observer,
    moderate, biographical, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stabilizes human use of animals by establishing minimum suffering thresholds, preventing worse cruelty and reducing social conflict over animal exploitation without ending the underlying practice.
% TRANSFER_FUNCTION: Transfers moral legitimacy, legal continuity, and economic subsidy to animal-using industries and consumers; transfers compliance costs and residual bodily harm to farmed animals.
% ABSENT_VOICES: Abolitionist advocates who reject use entirely are excluded from regulatory design; the direct interests of farmed animals are mediated through industry and scientific proxies rather than self-represented.
% DISAPPEARANCE_RATIONALE: If the welfare-regulated property framework vanished overnight, animal industries would face a legitimacy crisis, consumer moral comfort would collapse, and policy would polarize between pure deregulation and abolition â the current settlement depends on the constraint.
% FOUNDING_PROBLEM: Unregulated animal use caused visible cruelty and social disturbance; societies sought to prevent gratuitous suffering without dismantling animal agriculture, research, or other established property uses.
% FOUNDING_PROBLEM_CORROBORATION: Historians of animal law attest the founding cruelty problem was real; abolitionist scholars and critical animal theorists outside the benefiting parties attest that the chosen solution (regulated property) perpetuates the deeper problem of commodification.
narrative_ontology:disappearance_verdict(animal_status_kernel__welfare_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status_kernel__welfare_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status_kernel__welfare_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(animal_status_kernel__welfare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status_kernel__welfare_reading, 0.45, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status_kernel__welfare_reading_tests).
:- end_tests(animal_status_kernel__welfare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate because welfare regulations impose real costs on industry and mitigate some suffering, but they do not eliminate the underlying extraction of bodies and lives. Suppression (0.55) reflects the constraint's active suppression of abolitionist alternatives by framing the question as regulation rather than elimination. Theater ratio (0.40) captures the 'humane washing' dynamic: an increasing share of welfare activity performs care for consumers rather than functionally reducing harm. Accessibility collapse (0.60) indicates that once the welfare frame is accepted, abolition appears extreme. Resistance (0.50) captures sustained abolitionist and rights-based challenge.
 *
 * PERSPECTIVAL GAP:
 *   The animal industry operator seat should compute as coordination: the constraint secures their legal license and social stability. The farmed animal seat should compute as extraction: they bear the uncompensated costs of confinement and slaughter. The abolitionist observer sees a legitimizing snare, while the regulatory agency sees a pragmatic compromise. The engine computes this divergence from the structural data rather than adjudicating it.
 *
 * DIRECTIONALITY LOGIC:
 *   Animal industry operators and consumers are beneficiaries (d near 0.0): the constraint subsidizes their continued operation and consumption. Farmed animals are full targets (d near 1.0): they bear the bodily extraction and have no exit. Regulatory agencies sit near symmetric: they administer costs and benefits without primarily capturing either. Abolitionist advocates are excluded rather than targeted, but their exclusion is structurally necessary for the constraint's stability.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as tangled rope prevents the mislabeling of welfare frameworks as pure coordination (rope) â animals are not voluntary participants and pay asymmetrically â while also preventing mislabeling as pure extraction (snare) â there is a genuine coordination function in reducing worse suffering and stabilizing social expectations. It is not a scaffold because it carries no sunset clause and is not framed as transitional. It is not a piton because its primary function has not atrophied; enforcement and contestation are both active.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    new_welfarism_legitimacy,
    'Does the welfare reading genuinely reduce net animal suffering, or does it primarily legitimize continued exploitation by making consumers comfortable with ''humane'' use?',
    'Comparative analysis of total animal numbers and suffering rates across jurisdictions with stronger versus weaker welfare regimes, controlling for demand elasticity and substitution effects.',
    'If welfare regimes increase total demand while marginally reducing per-animal suffering, the constraint is more extractive than its coordination framing suggests; if they reduce net suffering, the coordination function is genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(new_welfarism_legitimacy, empirical, 'Whether welfare regulation reduces net suffering or legitimizes use').

omega_variable(
    property_welfare_compatibility,
    'Can welfare obligations be structurally honored within a property framework, or does property status inherently subordinate welfare interests to economic ones?',
    'Jurisprudential analysis of cases where welfare and economic interests conflict; measure outcome bias toward property holders.',
    'If property systematically overrides welfare, the constraint''s coordination story is cover for extraction; if welfare can genuinely constrain property, the tangled rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_welfare_compatibility, conceptual, 'Whether property and welfare are structurally compatible or contradictory').

omega_variable(
    industry_capture_of_standards,
    'To what extent are welfare standards captured by animal industry operators through regulatory lobbying and revolving doors?',
    'Lobbying expenditure analysis, regulatory capture metrics, and comparison of enacted standards against independent scientific recommendations.',
    'High capture would shift the constraint toward snare by showing the coordination story is administered for the beneficiary''s benefit; low capture supports the genuine hybrid classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(industry_capture_of_standards, empirical, 'Degree of regulatory capture in welfare standard-setting').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status_kernel__welfare_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status_kernel__welfare_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(anim_tr_t40, animal_status_kernel__welfare_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement(anim_tr_t80, animal_status_kernel__welfare_reading, theater_ratio, 80, 0.2).
narrative_ontology:measurement(anim_tr_t120, animal_status_kernel__welfare_reading, theater_ratio, 120, 0.3).
narrative_ontology:measurement(anim_tr_t160, animal_status_kernel__welfare_reading, theater_ratio, 160, 0.35).
narrative_ontology:measurement(anim_tr_t200, animal_status_kernel__welfare_reading, theater_ratio, 200, 0.4).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status_kernel__welfare_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(anim_be_t40, animal_status_kernel__welfare_reading, base_extractiveness, 40, 0.25).
narrative_ontology:measurement(anim_be_t80, animal_status_kernel__welfare_reading, base_extractiveness, 80, 0.3).
narrative_ontology:measurement(anim_be_t120, animal_status_kernel__welfare_reading, base_extractiveness, 120, 0.38).
narrative_ontology:measurement(anim_be_t160, animal_status_kernel__welfare_reading, base_extractiveness, 160, 0.42).
narrative_ontology:measurement(anim_be_t200, animal_status_kernel__welfare_reading, base_extractiveness, 200, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status_kernel__welfare_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(anim_su_t40, animal_status_kernel__welfare_reading, suppression_requirement, 40, 0.35).
narrative_ontology:measurement(anim_su_t80, animal_status_kernel__welfare_reading, suppression_requirement, 80, 0.4).
narrative_ontology:measurement(anim_su_t120, animal_status_kernel__welfare_reading, suppression_requirement, 120, 0.5).
narrative_ontology:measurement(anim_su_t160, animal_status_kernel__welfare_reading, suppression_requirement, 160, 0.52).
narrative_ontology:measurement(anim_su_t200, animal_status_kernel__welfare_reading, suppression_requirement, 200, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status_kernel__welfare_reading, resource_allocation).
narrative_ontology:affects_constraint(animal_status_kernel__welfare_reading, animal_status_kernel__property_reading).
narrative_ontology:affects_constraint(animal_status_kernel__welfare_reading, animal_status_kernel__abolitionist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the animal_status_kernel, decomposed per the epsilon-invariance principle because the kernel's natural-language label conflates structurally distinct claims about moral status, property rights, and welfare obligations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
