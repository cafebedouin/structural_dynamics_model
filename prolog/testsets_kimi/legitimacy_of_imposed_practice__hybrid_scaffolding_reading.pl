% ============================================================================
% CONSTRAINT STORY: legitimacy_of_imposed_practice__hybrid_scaffolding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_imposed_practice__hybrid_scaffolding_reading, []).

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
 *   constraint_id: legitimacy_of_imposed_practice__hybrid_scaffolding_reading
 *   human_readable: Hybrid Scaffolding of Cultural Practice Imposition
 *   domain: political/state_formation/cultural_imposition
 *
 * SUMMARY:
 *   This constraint instantiates the hybrid_scaffolding_reading of the
 *   legitimacy_of_imposed_practice kernel in political history and state
 *   formation. The kernel asks how state-imposed cultural practices acquire
 *   legitimacy sufficient to displace entrenched tradition. This reading
 *   claims that top-down mandates succeed when reinforced by ideological
 *   messaging and elite modeling that generate quasi-endogenous pull,
 *   producing partial displacement. The canonical contrast is between
 *   calendar reform (pure decree, no scaffolding, failed) and dress reform
 *   (elite modeling plus ideological framing, partial success with hybrid
 *   practices). Urban elites adopting Western identity markers benefit from
 *   the scaffolding infrastructure, while rural populations excluded from
 *   that infrastructure bear the costs of displacement and stigma.
 *
 * KEY AGENTS:
 *   - modernizing_state: Agenda setter (institutional/arbitrage) â imposes mandate and builds ideological apparatus
 *   - urban_elites: Primary beneficiary (powerful/constrained) â receive scaffolding and state favor
 *   - rural_populations: Primary target (powerless/trapped) â bear costs without access to legitimating infrastructure
 *   - traditional_leaders: Excluded voice (moderate/constrained) â would defend prior practices but are sidelined from legitimating discourse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.65).
domain_priors:suppression_score(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.72).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, "Hybrid Scaffolding of Cultural Practice Imposition").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, "political/state_formation/cultural_imposition").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__hybrid_scaffolding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, '0c1c6f00-302f-49a3-9368-8c662527ff1c').
narrative_ontology:cs_kernel_codification('0c1c6f00-302f-49a3-9368-8c662527ff1c', distributed).
narrative_ontology:cs_authority_grounding('0c1c6f00-302f-49a3-9368-8c662527ff1c', expertise).
narrative_ontology:cs_interpretation_layer_present('0c1c6f00-302f-49a3-9368-8c662527ff1c').
narrative_ontology:cs_reading_relation('0c1c6f00-302f-49a3-9368-8c662527ff1c', legitimacy_of_imposed_practice__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('0c1c6f00-302f-49a3-9368-8c662527ff1c', legitimacy_of_imposed_practice__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_axiom('0c1c6f00-302f-49a3-9368-8c662527ff1c', foundational, scaffolding_generates_authentic_pull).
narrative_ontology:cs_axiom_status(scaffolding_generates_authentic_pull, holdable).
narrative_ontology:cs_axiom_grounding('0c1c6f00-302f-49a3-9368-8c662527ff1c', scaffolding_generates_authentic_pull, empirically_contingent).
narrative_ontology:cs_axiom('0c1c6f00-302f-49a3-9368-8c662527ff1c', foundational, elite_adoption_cascades_to_mass_legitimacy).
narrative_ontology:cs_axiom_status(elite_adoption_cascades_to_mass_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('0c1c6f00-302f-49a3-9368-8c662527ff1c', elite_adoption_cascades_to_mass_legitimacy, empirically_contingent).
narrative_ontology:cs_reference_frame('0c1c6f00-302f-49a3-9368-8c662527ff1c', hybrid_legitimation_equilibrium).
narrative_ontology:cs_drift_state('0c1c6f00-302f-49a3-9368-8c662527ff1c', postcolonial_critique_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0c1c6f00-302f-49a3-9368-8c662527ff1c', '').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, urban_elites).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, rural_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Imposes top-down mandates for practice displacement and constructs ideological messaging apparatus to frame adoption as modernity and progress. Can pivot to alternative state-building strategies if cultural imposition fails, but bears reputational costs of reversal.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, modernizing_state, agenda_setter,
    institutional, generational, arbitrage, national).

% Receive scaffolding infrastructure including elite modeling networks, educational institutions, and ideological framing that make Western identity markers legible and prestigious. Their adoption signals loyalty and modernity, unlocking state favor and social capital. Maintaining elite status requires continued visible compliance.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, urban_elites, beneficiary,
    powerful, biographical, constrained, regional).

% Bear the costs of cultural displacement without access to the scaffolding that makes new practices meaningful. Stigmatized as backward when maintaining traditional practices and excluded from state favor and elite networks. Geographically and economically trapped outside the infrastructure of ideological transmission.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, rural_populations, payer,
    powerless, generational, trapped, regional).

% Would articulate defense of prior practices and organize resistance, but are structurally excluded from the legitimating discourse. Their authority is undermined by state ideological framing that casts tradition as obstruction.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, traditional_leaders, excluded,
    moderate, generational, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, urban_elites).
narrative_ontology:fixing_cost_class(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Synchronizes elite behavior around new practice standards through visible modeling and ideological framing, creating the appearance of endogenous cultural momentum that reduces enforcement costs for state-mandated reforms.
% TRANSFER_FUNCTION: Moves cultural legitimacy and state favor from traditional practice-keepers to urban adopters of Western identity markers; moves compliance costs, stigma, and exclusion to rural populations trapped outside the scaffolding infrastructure.
% ABSENT_VOICES: Rural populations are structurally excluded from the ideological framing apparatus and elite modeling networks; traditional religious and community leaders who would defend prior practices are sidelined from the legitimating discourse that frames the new practices as modern and progressive.
% DISAPPEARANCE_RATIONALE: If the scaffolding of elite modeling and ideological messaging vanished, the quasi-endogenous pull would collapse; urban elite adoption would revert to performative compliance or resistance, rural populations would revert to traditional practices, and the state would face pure decree costs or abandon the reform.
% FOUNDING_PROBLEM: How to displace entrenched traditional practices when pure state decree generates resistance and passive non-compliance, while waiting for organic generational change is too slow for state-building timelines.
% FOUNDING_PROBLEM_CORROBORATION: Urban elites attest the problem is solved through their own adoption success. Rural populations and traditional leaders attest the displacement is incomplete and coerced. Post-colonial historians and sociologists from outside the benefiting parties corroborate that partial displacement persists with hybrid practices, but debate whether this constitutes success or persistent domination.
narrative_ontology:disappearance_verdict(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_imposed_practice__hybrid_scaffolding_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_imposed_practice__hybrid_scaffolding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is substantial (0.65) because the constraint moves cultural legitimacy and material state favor from traditional practice-keepers to urban adopters while imposing costs on excluded rural populations. Suppression is higher (0.72) because the arrangement requires active enforcement of the mandate plus continuous ideological production to maintain the quasi-endogenous pull. Theater is moderate (0.45) because elite adoption is partially performativeâvisible compliance outpaces genuine internalizationâand the state must maintain the ideological spectacle of modernity. Accessibility collapse is moderate (0.55) because traditional alternatives persist in rural zones despite stigma, while resistance is moderate (0.58) from rural populations and traditional leaders. The temporal series show extraction and theater peaking mid-interval as scaffolding intensifies, then modestly declining as hybrid practices partially normalize.
 *
 * PERSPECTIVAL GAP:
 *   The urban elite seat and the rural population seat should compute differently: from the urban elite position the arrangement is a coordination mechanism that provides status, legitimation, and state favor through comprehensible pathways; from the rural population position the same structure operates as enforced extraction that stigmatizes their existing practices while denying them the infrastructure to adopt new ones meaningfully. The modernizing state experiences the constraint as a policy instrument with manageable enforcement costs due to elite buy-in.
 *
 * DIRECTIONALITY LOGIC:
 *   Urban elites are declared beneficiaries with constrained exitâlow directionality, damped effective extraction. Rural populations are declared victims with trapped exitâhigh directionality, amplified effective extraction. The modernizing state sits near the administrative center with arbitrage exit; it is neither beneficiary nor victim in the base properties, deriving structural advantage through state consolidation rather than direct rent collection.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as tangled rope prevents the false dichotomy of labeling it pure coordination (which would ignore the asymmetric exclusion of rural populations) or pure extraction (which would ignore the genuine coordination function performed for urban elites and the partial legitimacy the scaffolding generates). The mandate has not atrophied into a piton because identifiable beneficiaries still materially profit from the arrangement, and the enforcement apparatus is still functional rather than merely theatrical.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_hybrid_reading_contest,
    'Is the hybrid scaffolding reading the correct structural account of imposed practice legitimacy, or does the exogenous override or endogenous climb reading better capture the mechanism?',
    'Comparative historical analysis across multiple reform episodes measuring adoption rates under pure decree, pure endogenous conditions, and hybrid scaffolding.',
    'If hybrid scaffolding is not the operative mechanism, this constraint dissolves into either a snare or a rope, collapsing the tangled rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_hybrid_reading_contest, conceptual, 'Whether this constraint is a genuine hybrid mechanism or a misattribution of success to scaffolding.').

omega_variable(
    endogenization_veracity,
    'Does the ideological messaging actually produce endogenous pull among urban elites, or does compliance remain instrumental and performative?',
    'Ethnographic and archival study of elite discourse and private practice to determine whether elites maintain traditional practices in private while performing new ones publicly.',
    'If purely performative, theater_ratio is higher than measured and the constraint is more extractive; if genuinely internalized, effective extraction from elites is lower and the constraint approaches rope for that seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(endogenization_veracity, empirical, 'Whether elite adoption is genuine internalization or performance.').

omega_variable(
    rural_exclusion_intentionality,
    'Is rural exclusion from scaffolding infrastructure a designed feature of the constraint or a resource-constrained side effect?',
    'Policy archaeology examining whether state documents explicitly target rural populations for exclusion or attribute infrastructure gaps to logistical limits.',
    'If designed, the victim structure is intentional and the constraint leans snare; if incidental, the extraction is a byproduct of coordination prioritized on elites.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rural_exclusion_intentionality, empirical, 'Whether rural exclusion is intentional or incidental.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hybrid_scaffold_tr_t0, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(hybrid_scaffold_tr_t8, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 8, 0.38).
narrative_ontology:measurement(hybrid_scaffold_tr_t16, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 16, 0.52).
narrative_ontology:measurement(hybrid_scaffold_tr_t24, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 24, 0.48).
narrative_ontology:measurement(hybrid_scaffold_tr_t32, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 32, 0.42).
narrative_ontology:measurement(hybrid_scaffold_tr_t40, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 40, 0.35).

% Extraction over time
narrative_ontology:measurement(hybrid_scaffold_be_t0, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(hybrid_scaffold_be_t8, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(hybrid_scaffold_be_t16, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 16, 0.68).
narrative_ontology:measurement(hybrid_scaffold_be_t24, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 24, 0.65).
narrative_ontology:measurement(hybrid_scaffold_be_t32, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 32, 0.62).
narrative_ontology:measurement(hybrid_scaffold_be_t40, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 40, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(hybrid_scaffold_su_t0, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(hybrid_scaffold_su_t8, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 8, 0.7).
narrative_ontology:measurement(hybrid_scaffold_su_t16, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 16, 0.78).
narrative_ontology:measurement(hybrid_scaffold_su_t24, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 24, 0.72).
narrative_ontology:measurement(hybrid_scaffold_su_t32, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 32, 0.65).
narrative_ontology:measurement(hybrid_scaffold_su_t40, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 40, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, identity_coordination).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, legitimacy_of_imposed_practice__exogenous_override_reading).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, legitimacy_of_imposed_practice__endogenous_climb_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the legitimacy_of_imposed_practice kernel. The hybrid_scaffolding_reading models the scaffolded imposition mechanism; the exogenous_override_reading models pure decree; the endogenous_climb_reading models organic adoption. They share empirical territory (state-mandated cultural reform) but differ on the necessary and sufficient conditions for successful practice displacement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
