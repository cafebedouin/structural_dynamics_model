% ============================================================================
% CONSTRAINT STORY: flexible_employment_legitimacy__precarity_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_flexible_employment_legitimacy__precarity_extraction_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: flexible_employment_legitimacy__precarity_extraction_reading
 *   human_readable: Flexible Employment as Structural Precarity Enabling Platform Extraction
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'precarity_extraction_reading' of
 *   the contested kernel 'flexible_employment_legitimacy'. The kernel is the
 *   claim that flexible employment arrangements (gig work, zero-hours
 *   contracts, algorithmic management) are a legitimate feature of modern
 *   labor markets. Three readings contend: (1) market_efficiency_reading —
 *   flexibility clears markets and empowers workers; (2)
 *   developmental_state_reading — flexibility is a transitional form the
 *   state should manage toward formalization; (3)
 *   precarity_extraction_reading (this story) — flexibility is engineered
 *   precarity that enables platforms to externalize risk and extract surplus
 *   value via algorithmic control. This story authors ONLY the third reading
 *   as a clean, ε-invariant constraint. The other readings are separate
 *   constraints (other files).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(flexible_employment_legitimacy__precarity_extraction_reading, 0.72).
domain_priors:suppression_score(flexible_employment_legitimacy__precarity_extraction_reading, 0.68).
domain_priors:theater_ratio(flexible_employment_legitimacy__precarity_extraction_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(flexible_employment_legitimacy__precarity_extraction_reading, tangled_rope).
narrative_ontology:human_readable(flexible_employment_legitimacy__precarity_extraction_reading, "Flexible Employment as Structural Precarity Enabling Platform Extraction").
narrative_ontology:topic_domain(flexible_employment_legitimacy__precarity_extraction_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(flexible_employment_legitimacy__precarity_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(flexible_employment_legitimacy__precarity_extraction_reading, '84bbcb28-74d0-4bca-bb23-b5520c9af8b4').
narrative_ontology:cs_kernel_codification('84bbcb28-74d0-4bca-bb23-b5520c9af8b4', distributed).
narrative_ontology:cs_authority_grounding('84bbcb28-74d0-4bca-bb23-b5520c9af8b4', extraction).
narrative_ontology:cs_interpretation_layer_present('84bbcb28-74d0-4bca-bb23-b5520c9af8b4').
narrative_ontology:cs_reading_relation('84bbcb28-74d0-4bca-bb23-b5520c9af8b4', flexible_employment_legitimacy__market_efficiency_reading, coexists_with).
narrative_ontology:cs_reading_relation('84bbcb28-74d0-4bca-bb23-b5520c9af8b4', flexible_employment_legitimacy__developmental_state_reading, influences).
narrative_ontology:cs_axiom('84bbcb28-74d0-4bca-bb23-b5520c9af8b4', foundational, flexibility_is_engineered_precarity).
narrative_ontology:cs_axiom_status(flexibility_is_engineered_precarity, holdable).
narrative_ontology:cs_axiom_grounding('84bbcb28-74d0-4bca-bb23-b5520c9af8b4', flexibility_is_engineered_precarity, empirically_contingent).
narrative_ontology:cs_axiom('84bbcb28-74d0-4bca-bb23-b5520c9af8b4', foundational, algorithmic_control_externalizes_risk_to_extract_surplus).
narrative_ontology:cs_axiom_status(algorithmic_control_externalizes_risk_to_extract_surplus, holdable).
narrative_ontology:cs_axiom_grounding('84bbcb28-74d0-4bca-bb23-b5520c9af8b4', algorithmic_control_externalizes_risk_to_extract_surplus, instrumental).
narrative_ontology:cs_reference_frame('84bbcb28-74d0-4bca-bb23-b5520c9af8b4', post_crisis_labor_market_exclusion).
narrative_ontology:cs_drift_state('84bbcb28-74d0-4bca-bb23-b5520c9af8b4', platform_hegemony_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('84bbcb28-74d0-4bca-bb23-b5520c9af8b4', '').
narrative_ontology:cs_kernel_id(flexible_employment_legitimacy__precarity_extraction_reading, flexible_employment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__precarity_extraction_reading, platform_operators).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__precarity_extraction_reading, capital_intermediaries).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__precarity_extraction_reading, algorithmic_management_systems).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__precarity_extraction_reading, gig_workers).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__precarity_extraction_reading, precarious_contract_workers).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__precarity_extraction_reading, algorithmic_labor_subjects).
narrative_ontology:constraint_vindicates(flexible_employment_legitimacy__precarity_extraction_reading, flexibility_as_discipline).
narrative_ontology:constraint_vindicates(flexible_employment_legitimacy__precarity_extraction_reading, risk_externalization_as_profit_mechanism).
narrative_ontology:constraint_vindicates(flexible_employment_legitimacy__precarity_extraction_reading, algorithmic_control_as_extraction_infrastructure).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own and operate the algorithmic infrastructure that mediates labor markets. Set terms of engagement, commission rates, deactivation policies, and performance metrics. Collect platform fees from every transaction. Justify the model as enabling entrepreneurship and flexible income. Can relocate legal entities, shift jurisdiction, or restructure fee models to avoid regulation.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, platform_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Venture capital, private equity, and financial investors who fund platform growth and extract returns through equity appreciation, dividends, and exit events. Benefit from labor cost externalization and regulatory arbitrage. Can reallocate capital across platforms, sectors, or geographies with low switching costs.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, capital_intermediaries, beneficiary,
    organized, biographical, mobile, global).

% The socio-technical apparatus of algorithmic dispatch, rating, ranking, and discipline that coordinates labor at scale. Not a human agent but a structural beneficiary: its persistence and expansion depend on the precarity it manages. Collects behavioral surplus as training data and operational control. No exit — it is the infrastructure.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, algorithmic_management_systems, beneficiary,
    institutional, civilizational, analytical, universal).

% Depend on platform income for survival; face algorithmic deactivation without recourse; bear all costs of equipment, insurance, downtime, and injury; cannot negotiate terms; exit means income loss with no safety net. Organize informally but face coordinated suppression (deactivation, shadow-banning, legal retaliation).
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, gig_workers, payer,
    powerless, immediate, trapped, local).

% Zero-hours, temporary, or misclassified contractors in logistics, care, delivery, and creative sectors. Face similar risk externalization and algorithmic scheduling but retain fragmentary labor protections. Exit options exist but are costly: retraining, relocation, or accepting worse conditions. Some unionize; many cycle in and out.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, precarious_contract_workers, payer,
    moderate, biographical, constrained, regional).

% Workers whose professional identity, certification, and career progression are mediated by algorithmic reputation systems (e.g., Upwork ratings, Uber Pro tiers, TopCoder rankings). Exit means abandoning accumulated reputation capital that cannot be ported. Identity is fused to the platform's evaluation metric — leaving is professional death.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, algorithmic_labor_subjects, payer,
    organized, generational, identity_locked, national).

% Public insurance, pension, and welfare systems that absorb the costs of platform externalization (unemployment, injury, old-age poverty) without representation in platform governance. Would object to cost-shifting if structurally empowered; currently reactive rather than preventive.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, social_security_systems, excluded,
    institutional, generational, analytical, national).

% Enforce employment classification, minimum wage, and safety standards. Investigate misclassification and algorithmic discrimination. Constrained by jurisdictional boundaries, lobbying, and the pace of platform innovation. Their rulings reshape the constraint's enforcement but rarely its core logic.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, labor_regulators, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Matches labor supply to demand in real time across fragmented tasks; provides income access to workers excluded from formal employment; standardizes trust and payment in peer-to-peer markets.
% TRANSFER_FUNCTION: Moves risk (income volatility, injury, retirement, equipment costs) from platforms and capital to workers; moves surplus value (commission, data, behavioral control) from workers to platform operators and investors; moves regulatory compliance costs to the state.
% ABSENT_VOICES: Workers in the Global South who perform platform labor without any jurisdictional protection; undocumented workers excluded from even minimal labor rights; future cohorts who will inherit the normalized precarity model. They are absent because they cannot access the regulatory or discursive arena.
% DISAPPEARANCE_RATIONALE: If algorithmic precarity vanished overnight, platforms would need to internalize labor costs (benefits, insurance, wage floors), restructure fee models, or exit markets. Workers would gain bargaining power but lose immediate income access. Capital would reallocate. The state would face a sudden expansion of formal employment obligations. The entire gig/platform labor market would reorganize around employer responsibilities.
% FOUNDING_PROBLEM: Post-2008 labor market exclusion: large populations unable to access stable employment; technology enabled low-friction matching of idle labor to micro-tasks; platforms promised 'be your own boss' as a solution to structural unemployment.
% FOUNDING_PROBLEM_CORROBORATION: Platform operators and investors attest the founding problem persists — flexible income remains a lifeline for excluded workers (corroborated by worker surveys citing income necessity). Labor economists, unions, and regulatory bodies attest the problem has mutated: the solution became a new structure of exploitation, with algorithmic control replacing managerial authority while externalizing its costs (corroborated by ILO reports, EU directive proceedings, and academic literature on platform work).
narrative_ontology:disappearance_verdict(flexible_employment_legitimacy__precarity_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(flexible_employment_legitimacy__precarity_extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(flexible_employment_legitimacy__precarity_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(flexible_employment_legitimacy__precarity_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(flexible_employment_legitimacy__precarity_extraction_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(flexible_employment_legitimacy__precarity_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(flexible_employment_legitimacy__precarity_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(flexible_employment_legitimacy__precarity_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.72) reflects the gap between platform revenue per transaction and the marginal cost of algorithmic mediation — commissions of 20-35% plus data extraction vs. near-zero marginal dispatch cost. Suppression (0.68) captures algorithmic deactivation, rating lock-in, juridical misclassification defenses, and the structural absence of collective bargaining. Theater ratio (0.42) measures the growing gap between 'entrepreneurship' rhetoric and the reality of algorithmic discipline: safety nets marketed as 'benefits' that few qualify for, 'flexibility' that is uni-directional (worker bears all variance). Accessibility collapse (0.58) — alternatives exist (formal employment, cooperatives) but are structurally disadvantaged by capital intensity and network effects. Resistance (0.62) — strikes, lawsuits, unionization drives, and regulatory campaigns are real but fragmented across jurisdictions and platform architectures.
 *
 * PERSPECTIVAL GAP:
 *   From the platform operator seat, the constraint appears as coordination (rope-like): they built a market that didn't exist, enabled income for millions, and innovate on safety nets voluntarily. From the gig worker seat, it is extraction (snare-like): algorithmic control without accountability, risk without reward, voice without power. The engine computes this divergence from the declared power/exit/role structure — the claimed_type (tangled_rope) asserts the coordination function is real but asymmetrically extractive, which is exactly the structural claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform operators and capital intermediaries are structural beneficiaries (d near 0.0): they collect rents, control infrastructure, and exit via jurisdictional arbitrage. Algorithmic management systems are institutional beneficiaries with no exit (d = 0.0, analytical seat). Gig workers are trapped targets (d near 1.0): income dependence, no portability, deactivation risk. Precarious contract workers are constrained payers (d ~0.7): some labor rights, some mobility, but same risk externalization. Algorithmic labor subjects are identity-locked (d ~0.85): reputation capital is non-portable and career-defining. Social security systems are excluded bearers of externalized costs. Labor regulators are analytical observers with enforcement power but structural lag.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-2008 exclusion) was real. The arrangement solved it — then metastasized. The coordination function (matching) persists but is now subordinated to the extraction function (commission + data + control). Mandatrophy is unresolved: the arrangement's mandate ('flexible income for the excluded') has been captured by its own enforcement infrastructure (algorithmic discipline). The constraint persists because the beneficiaries (platforms, capital) are powerful enough to block formalization but not motivated to improve conditions; the victims are too fragmented to force transition; the state is caught between fiscal exposure and regulatory capture. This is not a scaffold (no sunset) and not a piton (extraction is active, not inertial). It is a tangled_rope whose coordination limb is being consumed by its extraction limb.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_separability,
    'Can the algorithmic matching function be separated from the extraction function (commission, data, control) without collapsing the market?',
    'Natural experiment from platforms that adopt open protocols, worker cooperatives, or regulated fee caps: if matching persists at lower extraction, the functions are separable; if matching collapses, extraction is the price of coordination.',
    'If separable, the constraint is a tangled_rope where extraction is layered on coordination; if inseparable, it may be a snare where coordination is the cover story. Affects whether regulatory remedies (fee caps, data portability, algorithmic transparency) can preserve coordination while reducing extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the constraint''s coordination and extraction components are structurally separable or co-constitutive.').

omega_variable(
    identity_lock_mechanism,
    'Is the identity_locked exit condition for algorithmic_labor_subjects primarily professional (reputation capital), psychological (internalized platform metrics as self-worth), or structural (no alternative evaluation infrastructure exists)?',
    'Longitudinal studies of workers who exit platforms: track re-employment, income recovery, and identity reconstruction. Compare platforms with vs. without portable reputation systems.',
    'If professional, portability mandates could unlock exit. If psychological, the lock persists after structural removal — suppression is internalized. If structural, the lock requires building alternative evaluation infrastructures. Changes the intervention logic for this stakeholder seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Mechanism of identity lock for workers whose career capital is algorithmically mediated.').

omega_variable(
    kernel_framing_underdetermination,
    'Does the kernel ''flexible_employment_legitimacy'' refer to the same empirical phenomenon across all three readings, or do the readings carve the phenomenon at different joints (e.g., market_efficiency focuses on matching efficiency, precarity_extraction focuses on risk externalization, developmental_state focuses on formalization trajectories)?',
    'Map each reading''s empirical referent: what specific arrangements, metrics, and counterfactuals does each reading treat as central? Compare the extension of ''flexible employment'' in each reading''s evidence base.',
    'If readings refer to different empirical extensions, the kernel is a linguistic conflation, not a genuine contest — decomposition into separate constraints is warranted (which this story already does). If they share an extension but diverge on causal attribution, the contest is interpretive and the omega documents the under-determination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel''s contested readings share an empirical referent or constitute a family resemblance concept.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.68) primarily structural (algorithmic deactivation, legal misclassification, jurisdictional evasion) or internalized (workers self-censor, accept precarity as normal, identify with ''entrepreneur'' framing)?',
    'Post-exit suppression trajectory: track workers who leave platforms — does suppression persist (internalized) or dissipate (structural)? Survey experiments on framing effects: does ''entrepreneur'' vs. ''worker'' framing change tolerance for precarity?',
    'If internalized, effective suppression is higher than structural measure suggests — the constraint travels with the worker after exit. If structural, exit genuinely reduces suppression. Changes the classification of the identity_locked seat and the remedy logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in algorithmic labor control.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(flexible_employment_legitimacy__precarity_extraction_reading, 2009, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(flex_tr_t2009, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 2009, 0.15).
narrative_ontology:measurement(flex_tr_t2012, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 2012, 0.22).
narrative_ontology:measurement(flex_tr_t2015, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 2015, 0.28).
narrative_ontology:measurement(flex_tr_t2018, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 2018, 0.35).
narrative_ontology:measurement(flex_tr_t2021, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 2021, 0.39).
narrative_ontology:measurement(flex_tr_t2025, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(flex_be_t2009, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 2009, 0.35).
narrative_ontology:measurement(flex_be_t2012, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 2012, 0.42).
narrative_ontology:measurement(flex_be_t2015, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 2015, 0.51).
narrative_ontology:measurement(flex_be_t2018, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 2018, 0.6).
narrative_ontology:measurement(flex_be_t2021, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 2021, 0.68).
narrative_ontology:measurement(flex_be_t2025, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 2025, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(flex_su_t2009, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 2009, 0.25).
narrative_ontology:measurement(flex_su_t2012, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 2012, 0.35).
narrative_ontology:measurement(flex_su_t2015, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 2015, 0.45).
narrative_ontology:measurement(flex_su_t2018, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 2018, 0.55).
narrative_ontology:measurement(flex_su_t2021, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 2021, 0.62).
narrative_ontology:measurement(flex_su_t2025, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 2025, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(flexible_employment_legitimacy__precarity_extraction_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(flexible_employment_legitimacy__precarity_extraction_reading, 0.18).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__precarity_extraction_reading, algorithmic_management_infrastructure).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__precarity_extraction_reading, labor_classification_regime).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__precarity_extraction_reading, social_protection_coverage_gaps).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__precarity_extraction_reading, platform_data_extraction_architecture).

% DUAL FORMULATION NOTE:
% This constraint is one member of the 'flexible_employment_legitimacy' constraint family. The market_efficiency_reading treats the coordination function as dominant (claimed_type: rope, extractiveness ~0.35). The developmental_state_reading treats the arrangement as a transient form (claimed_type: scaffold, has_sunset_clause: true). This reading (precarity_extraction) treats coordination as real but subordinated to extraction (claimed_type: tangled_rope, extractiveness ~0.72). All three share the kernel but author different ε, different beneficiary/victim structures, and different temporal trajectories. They are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(flexible_employment_legitimacy__precarity_extraction_reading, institutional, 0.05).
constraint_indexing:directionality_override(flexible_employment_legitimacy__precarity_extraction_reading, powerless, 0.95).
constraint_indexing:directionality_override(flexible_employment_legitimacy__precarity_extraction_reading, moderate, 0.7).
constraint_indexing:directionality_override(flexible_employment_legitimacy__precarity_extraction_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
