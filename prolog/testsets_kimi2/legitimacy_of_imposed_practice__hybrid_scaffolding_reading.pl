% ============================================================================
% CONSTRAINT STORY: legitimacy_of_imposed_practice__hybrid_scaffolding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-06
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
 *   human_readable: Scaffolded Cultural Imposition via Elite Modeling and Ideological Framing
 *   domain: political_history/state_formation/cultural_imposition
 *
 * SUMMARY:
 *   This constraint instantiates the hybrid_scaffolding_reading of the
 *   contested kernel legitimacy_of_imposed_practice, examining how
 *   state-mandated cultural reform achieves partial practice displacement
 *   only when top-down decree is reinforced by elite modeling and ideological
 *   messaging that generates quasi-endogenous pull. Pure decree (calendar
 *   reform) fails because it lacks scaffolding; pure endogenous climb is too
 *   slow; scaffolded imposition (dress reform) produces hybrid practices and
 *   asymmetrically benefits urban elites while excluding rural populations
 *   from the legitimizing infrastructure.
 *
 * KEY AGENTS:
 *   - state_reform_authority (institutional/constrained): sets mandates and builds ideological scaffolding
 *   - urban_elites (powerful/constrained): beneficiaries who adopt Western markers and model compliance
 *   - rural_populations (powerless/trapped): victims excluded from scaffolding, bear displacement costs
 *   - traditional_practitioners (powerless/trapped): excluded from reform design, maintain stigmatized practices
 *   - critical_historians (analytical/analytical): observer tracking differential reform success
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.58).
domain_priors:suppression_score(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.65).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, "Scaffolded Cultural Imposition via Elite Modeling and Ideological Framing").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, "political_history/state_formation/cultural_imposition").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__hybrid_scaffolding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, '21babf76-8cab-4f96-9d3c-ef042d3aa50f').
narrative_ontology:cs_kernel_codification('21babf76-8cab-4f96-9d3c-ef042d3aa50f', distributed).
narrative_ontology:cs_authority_grounding('21babf76-8cab-4f96-9d3c-ef042d3aa50f', distributed).
narrative_ontology:cs_reading_relation('21babf76-8cab-4f96-9d3c-ef042d3aa50f', legitimacy_of_imposed_practice__exogenous_override_reading, forecloses).
narrative_ontology:cs_reading_relation('21babf76-8cab-4f96-9d3c-ef042d3aa50f', legitimacy_of_imposed_practice__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_axiom('21babf76-8cab-4f96-9d3c-ef042d3aa50f', foundational, scaffolding_necessary_for_rapid_displacement).
narrative_ontology:cs_axiom_status(scaffolding_necessary_for_rapid_displacement, holdable).
narrative_ontology:cs_axiom_grounding('21babf76-8cab-4f96-9d3c-ef042d3aa50f', scaffolding_necessary_for_rapid_displacement, empirically_contingent).
narrative_ontology:cs_axiom('21babf76-8cab-4f96-9d3c-ef042d3aa50f', foundational, pure_decree_insufficient).
narrative_ontology:cs_axiom_status(pure_decree_insufficient, holdable).
narrative_ontology:cs_axiom_grounding('21babf76-8cab-4f96-9d3c-ef042d3aa50f', pure_decree_insufficient, empirically_contingent).
narrative_ontology:cs_reference_frame('21babf76-8cab-4f96-9d3c-ef042d3aa50f', scaffolded_modernization_as_legitimate).
narrative_ontology:cs_drift_state('21babf76-8cab-4f96-9d3c-ef042d3aa50f', contemporary_postcolonial_reappraisal, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('21babf76-8cab-4f96-9d3c-ef042d3aa50f', '').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, urban_elites).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, rural_populations).
narrative_ontology:constraint_vindicates(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, modernization_through_elite_emulation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues mandates for practice reform and constructs ideological scaffolding through education, propaganda, and elite recruitment. Bears enforcement costs and political risks of reform failure. Can reverse course but faces severe legitimacy costs if the modernizing project collapses.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, state_reform_authority, agenda_setter,
    institutional, generational, constrained, national).

% Adopt Western identity markers as visible status signals and channels to state favor. Positioned as the modeling vanguard for broader society. Receive differentiated access to education, administration, and cosmopolitan networks; their compliance is celebrated and rewarded.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, urban_elites, beneficiary,
    powerful, biographical, constrained, national).

% Bear the costs of practice displacement without receiving the legitimizing ideological scaffolding or elite modeling available in urban centers. Traditional practices are stigmatized as backward; access to modernizing institutions is blocked by geography and poverty. Compliance is extracted through penalty or exclusion rather than aspirational pull.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, rural_populations, payer,
    powerless, biographical, trapped, local).

% Maintain pre-reform practices and would contest the legitimacy of imposed alternatives if admitted to reform design. Are structurally absent from state and elite deliberations; their knowledge is erased from the official narrative.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, traditional_practitioners, excluded,
    powerless, biographical, trapped, local).

% Analyze differential reform success across domains to assess whether scaffolding or decree drives displacement. Occupied an analytical seat outside the historical beneficiary structure, tracking the divergence between elite and rural experience.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, critical_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, urban_elites).
narrative_ontology:fixing_cost_class(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reduces the friction of pure coercion in cultural modernization by creating aspirational pathways through elite emulation and ideological framing, achieving partial unification of practice across fragmented populations.
% TRANSFER_FUNCTION: Moves status, legitimacy, and institutional access from rural traditional practitioners to urban elites who adopt imposed markers, while extracting compliance labor and cultural deference from excluded rural populations.
% ABSENT_VOICES: Rural populations and traditional practitioners are excluded from the rooms where scaffolding is designed; the reform conversation is held between state modernizers and urban elites, muting dissent.
% DISAPPEARANCE_RATIONALE: If the scaffolded imposition vanished overnight, hybrid dress practices would revert toward prior norms; urban elites would lose the differentiated status marker that anchors their social position; the state's modernizing project would lose its social vanguard and visible proof of progress.
% FOUNDING_PROBLEM: The need to displace entrenched traditional practices and align society with a modernizing standard without relying solely on ineffective pure decree or waiting for the slow generational pace of bottom-up adoption.
% FOUNDING_PROBLEM_CORROBORATION: State modernizers and urban elites attest the problem of backwardness requiring rapid alignment. Critical historians and postcolonial scholars attest the problem was exaggerated or the cure was worse than the disease; there is no corroborated consensus outside the beneficiary set.
narrative_ontology:disappearance_verdict(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.58, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.58) reflects substantial but partial displacement: urban elites capture status and access while rural populations lose traditional legitimacy without gaining the new. Suppression (0.65) is moderated by quasi-endogenous pull, but remains significant because traditional alternatives are stigmatized and structurally excluded. Theater ratio (0.40) captures the performative dimension of elite modeling, which is real but not purely theatricalâhybrid practices do stabilize. Accessibility collapse (0.50) is incomplete: alternatives survive in rural and hybrid forms. Resistance (0.55) is persistent among excluded populations but managed by scaffolding. Measurements trace an enforcement lifecycle: initial high suppression gives way to ideologically maintained compliance, while theater peaks during the performative phase of elite modeling and then settles.
 *
 * PERSPECTIVAL GAP:
 *   The urban elite seat experiences the constraint as aspirational coordination (access to modernity, status gain), computing toward rope-like or beneficiary-biased tangled rope. The rural population seat experiences the same constraint as extractive imposition with no compensating scaffolding, computing toward snare-like high extraction. The state seat sees a modernizing project whose partial success justifies the cost; the analytical seat sees the divergence between these computed types as the core historical pattern. The engine derives this divergence from the same structural data: beneficiary versus victim declarations, differential exit options (constrained vs trapped), and spatial scope (national vs local).
 *
 * DIRECTIONALITY LOGIC:
 *   Urban elites are declared beneficiaries (low d): the constraint subsidizes their status and access. Rural populations are declared victims (high d): the constraint extracts compliance and stigma costs. The state reform authority is the agenda setter; its directionality is structurally near the beneficiary end because the modernizing project subsidizes its legitimacy. No override is needed because the structural derivation matches the historical relationship: low d for elites, high d for rural populations.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by requiring both coordination and extraction. A purely exogenous reading would classify calendar reform as a rope or snare; the hybrid reading shows that the same state structure produces different outcomes depending on scaffolding presence. The mandate is not yet atrophied because the coordination function (modernizing displacement) is still partially active and the beneficiary structure (urban elites) remains invested. If the scaffolding eroded while the mandate persisted, the constraint would drift toward pure snare or piton as the quasi-endogenous pull decayed into mere theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_sibling_divergence,
    'The exogenous_override_reading treats state decree as sufficient, while this reading treats decree as necessary but insufficient without scaffolding; does historical evidence from calendar reform (failed decree) versus dress reform (partial success) adjudicate this, or are the cases too heterogeneous?',
    'Comparative historical analysis controlling for domain (temporal vs sartorial), regional variation, and state capacity.',
    'If cases are adjudicable, one reading gains empirical support over the other; if not, the kernel remains underdetermined and both readings persist as distinct constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_sibling_divergence, empirical, 'Whether historical cases adjudicate between decree-sufficiency and scaffolding-necessity').

omega_variable(
    internalization_depth,
    'Is the elite adoption of Western markers genuine identity transformation or strategic performance under observable surveillance?',
    'Examine behavior of elites when the scaffolding is removed or when they emigrate to contexts without the imposed practice.',
    'If compliance vanishes when scaffolding is removed, the suppression was structural or performative; if it persists, genuine internalization occurred, raising effective coordination and lowering extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalization_depth, empirical, 'Whether elite compliance is internalized belief or performative strategy').

omega_variable(
    rural_exclusion_intentionality,
    'Are rural populations excluded from the scaffolding infrastructure because of geographic marginalization, or because the scaffolding was designed as an urban-elite signaling mechanism from the outset?',
    'Archival analysis of reform design documents versus implementation records.',
    'If designed for urban elites, the extraction is intentional; if geographic, it is a side effect of coordination cost, changing the directionality logic.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rural_exclusion_intentionality, empirical, 'Whether rural exclusion was by design or by implementation constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(legi_tr_t4, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 4, 0.42).
narrative_ontology:measurement(legi_tr_t8, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 8, 0.48).
narrative_ontology:measurement(legi_tr_t12, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 12, 0.5).
narrative_ontology:measurement(legi_tr_t16, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 16, 0.48).
narrative_ontology:measurement(legi_tr_t20, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 20, 0.44).
narrative_ontology:measurement(legi_tr_t24, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(legi_be_t4, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 4, 0.38).
narrative_ontology:measurement(legi_be_t8, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 8, 0.45).
narrative_ontology:measurement(legi_be_t12, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(legi_be_t16, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 16, 0.54).
narrative_ontology:measurement(legi_be_t20, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(legi_be_t24, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(legi_su_t4, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 4, 0.76).
narrative_ontology:measurement(legi_su_t8, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 8, 0.74).
narrative_ontology:measurement(legi_su_t12, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 12, 0.7).
narrative_ontology:measurement(legi_su_t16, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 16, 0.67).
narrative_ontology:measurement(legi_su_t20, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(legi_su_t24, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 24, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, exogenous_override_reading).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, endogenous_climb_reading).

% DUAL FORMULATION NOTE:
% The natural-language kernel legitimacy_of_imposed_practice decomposes into three structurally distinct readings. The exogenous_override_reading treats decree as sufficient; the endogenous_climb_reading treats only bottom-up adoption as viable; this reading treats top-down scaffolding as necessary. Each reading has a different epsilon, beneficiary structure, and empirical referent. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
