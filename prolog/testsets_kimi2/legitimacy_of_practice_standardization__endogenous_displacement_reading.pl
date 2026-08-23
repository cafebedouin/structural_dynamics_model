% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__endogenous_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_practice_standardization__endogenous_displacement_reading, []).

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
 *   constraint_id: legitimacy_of_practice_standardization__endogenous_displacement_reading
 *   human_readable: Endogenous Displacement Legitimacy Standard
 *   domain: political/historical/social
 *
 * SUMMARY:
 *   This constraint story instantiates the endogenous_displacement_reading of
 *   the contested kernel legitimacy_of_practice_standardization. The kernel
 *   asks what confers legitimacy on practice change. This reading answers
 *   that legitimacy emerges from voluntary adoption driven by perceived
 *   utility or cultural evolution â as seen in calendar and dress reforms
 *   where change shows gradual adoption curves, regional variation, and
 *   elite-to-mass diffusion. The constraint coordinates modernization by
 *   delegitimizing imposed decree and traditional stasis alike, but it
 *   extracts from traditional authorities and rural communities by denying
 *   their practices legitimacy regardless of their preferences.
 *
 * KEY AGENTS:
 *   - modernizing_elites (agenda_setter, powerful/mobile) â define the legitimacy standard and capture authority from displaced traditional institutions
 *   - urban_merchants (beneficiary, moderate/mobile) â profit from standardized practices without setting the agenda
 *   - traditional_religious_authorities (payer, organized/identity_locked) â bear legitimacy loss and ritual discontinuity
 *   - rural_agriculturalists (payer, powerless/constrained) â bear cultural costs of adoption under economic and social pressure
 *   - state_modernizers (excluded, institutional/constrained) â pushed out of the legitimacy framework by the voluntary-adoption requirement
 *   - comparative_historians (observer, analytical/analytical) â evaluate the fit between narrative and archival evidence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.48).
domain_priors:suppression_score(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.42).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__endogenous_displacement_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__endogenous_displacement_reading, "Endogenous Displacement Legitimacy Standard").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__endogenous_displacement_reading, "political/historical/social").

domain_priors:requires_active_enforcement(legitimacy_of_practice_standardization__endogenous_displacement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__endogenous_displacement_reading, '803e9733-8369-413e-b5f2-2c2b263d66c3').
narrative_ontology:cs_kernel_codification('803e9733-8369-413e-b5f2-2c2b263d66c3', implicit).
narrative_ontology:cs_authority_grounding('803e9733-8369-413e-b5f2-2c2b263d66c3', practice).
narrative_ontology:cs_interpretation_layer_present('803e9733-8369-413e-b5f2-2c2b263d66c3').
narrative_ontology:cs_reading_relation('803e9733-8369-413e-b5f2-2c2b263d66c3', legitimacy_of_practice_standardization__exogenous_override_reading, forecloses).
narrative_ontology:cs_reading_relation('803e9733-8369-413e-b5f2-2c2b263d66c3', legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, coexists_with).
narrative_ontology:cs_axiom('803e9733-8369-413e-b5f2-2c2b263d66c3', foundational, voluntary_adoption_confers_legitimacy).
narrative_ontology:cs_axiom_status(voluntary_adoption_confers_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('803e9733-8369-413e-b5f2-2c2b263d66c3', voluntary_adoption_confers_legitimacy, conventional).
narrative_ontology:cs_axiom('803e9733-8369-413e-b5f2-2c2b263d66c3', secondary, state_decrees_lack_practice_legitimacy).
narrative_ontology:cs_axiom_status(state_decrees_lack_practice_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('803e9733-8369-413e-b5f2-2c2b263d66c3', state_decrees_lack_practice_legitimacy, conventional).
narrative_ontology:cs_reference_frame('803e9733-8369-413e-b5f2-2c2b263d66c3', endogenous_legitimacy_framework).
narrative_ontology:cs_drift_state('803e9733-8369-413e-b5f2-2c2b263d66c3', post_colonial_critique_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('803e9733-8369-413e-b5f2-2c2b263d66c3', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, modernizing_elites).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, urban_merchants).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__endogenous_displacement_reading, traditional_religious_authorities).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__endogenous_displacement_reading, rural_agriculturalists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Shape the discourse of utility and cultural evolution; define what counts as legitimate voluntary adoption. Benefit from the delegitimization of traditional authorities and the opening of economic and cultural institutions to modern practices.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, modernizing_elites, agenda_setter,
    powerful, generational, mobile, national).

% Benefit from standardized calendars and dress codes that reduce transaction costs and facilitate trade across regions. They do not set the agenda but profit from the coordinated shift to modern practices.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, urban_merchants, beneficiary,
    moderate, biographical, mobile, regional).

% Lose legitimacy and social standing as their practices are reclassified as obsolete rather than voluntarily abandoned. Their authority depended on preserving ritual continuity; the new legitimacy standard treats their resistance as temporary friction.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, traditional_religious_authorities, payer,
    organized, generational, identity_locked, regional).

% Adopt new calendars and dress under social and economic pressure to access markets, education, and legal recognition. Bear the cultural and cognitive costs of abandoning traditional agrarian rhythms and markers, even when the adoption is framed as voluntary.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, rural_agriculturalists, payer,
    powerless, biographical, constrained, local).

% Would prefer to impose practice changes by state decree for rapid modernization. Are excluded from the legitimacy conversation by this reading, which delegitimizes their primary instrument of exogenous override.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, state_modernizers, excluded,
    institutional, generational, constrained, national).

% Analyze whether practice changes in cases like Meiji Japan or Ottoman Tanzimat were genuinely endogenous or masked state imposition. They assess the fit between the legitimacy narrative and archival evidence.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, comparative_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_practice_standardization__endogenous_displacement_reading, modernizing_elites).
narrative_ontology:fixing_cost_class(legitimacy_of_practice_standardization__endogenous_displacement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the transition from traditional to modern practices by providing a legitimacy standard that reduces violent conflict and allows gradual adaptation; solves the collective-action problem of when to abandon old practices without centralized decree.
% TRANSFER_FUNCTION: Moves social legitimacy and institutional access from traditional authorities and agrarian communities to modernizing elites and urban commercial actors, under the cover of voluntary utility-driven adoption.
% ABSENT_VOICES: State modernizers who prefer exogenous decree are structurally excluded by this legitimacy framework; subaltern populations whose adoption is coerced by economic necessity are framed as voluntary agents and thus silenced.
% DISAPPEARANCE_RATIONALE: If the standard vanished, the social justification for calendar and dress reforms would collapse. Exogenous state decree would rush in to impose changes, or traditional practices would reassert in public life. The modernizing social order depends on this legitimacy narrative to avoid appearing as raw imposition.
% FOUNDING_PROBLEM: How to modernize practices (calendars, dress, weights and measures) without constant violent suppression of traditional populations, and how to justify new practices as legitimate rather than externally imposed.
% FOUNDING_PROBLEM_CORROBORATION: Modernizing elites and urban merchants attest that voluntary adoption solved violent resistance. Post-colonial historians and subaltern studies scholars outside the benefiting parties attest that the founding problem was 'solved' only by redefining structural coercion as voluntariness; traditional religious authorities corroborate that their resistance was persistent and structural, not temporary friction.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__endogenous_displacement_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__endogenous_displacement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__endogenous_displacement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__endogenous_displacement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_practice_standardization__endogenous_displacement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_practice_standardization__endogenous_displacement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_practice_standardization__endogenous_displacement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48) is moderate because the voluntary-adoption frame coordinates genuine modernization but asymmetrically benefits urban merchants and modernizing elites while imposing cultural costs on traditional authorities and rural communities. Suppression (0.42) reflects social and institutional gatekeeping rather than direct violence: schools, markets, and administrative offices require modern dress and calendar adherence, making non-adoption costly. Theater ratio (0.38) captures the performative discourse of 'civilization' and 'utility' that frames elite-steered change as organic evolution. Accessibility collapse (0.45) is partial: traditional practices persist in private but lose public legitimacy. Resistance (0.52) comes from traditional religious authorities whose identity is fused with the old practices. The measurement series share one time grid (every 10 units) to prevent misalignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   The modernizing elite seat experiences the constraint as a rope-like coordination mechanism that legitimizes change and reduces enforcement costs. The traditional authority and rural agriculturalist seats experience it as extractive delegitimization of their lifeworld, where 'voluntary' adoption is the only path to institutional access. The engine will compute divergent per-seat types from this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Modernizing elites are structural beneficiaries (low d): they define the standard and collect authority. Urban merchants are beneficiaries (low-moderate d): they profit from coordination. Traditional religious authorities and rural agriculturalists are targets (high d): they bear the cultural and economic costs of displacement. State modernizers are excluded (anomalous d): this constraint delegitimizes their preferred instrument.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the R5 genealogy, this could be misread as a simple rope (voluntary coordination) or a snare (elite imposition). The tangled_rope classification is warranted because there is a genuine coordination function â standardized calendars and dress reduce transaction costs â but also asymmetric extraction: elites capture the authority to define 'utility' and 'voluntary,' while traditionalists pay through identity loss. The founding problem (how to modernize without constant violence) is contested because the 'voluntary' solution itself becomes a vehicle for extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntary_adoption_authenticity,
    'Is the ''voluntary adoption'' described by this reading genuinely spontaneous, or does it mask structural coercion via economic necessity, elite propaganda, and institutional gatekeeping?',
    'Archival analysis of adoption petitions and ethnographic records; measuring the correlation between adoption and economic dependency or access to state services.',
    'If coerced, extraction is higher than the reading''s own framework admits, and the constraint functions more as a snare or tangled rope with high suppression; if genuine, it moves toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_adoption_authenticity, empirical, 'Authenticity of voluntariness in endogenous practice change').

omega_variable(
    elite_steering_vs_bottom_up,
    'Does cultural evolution in this framework emerge bottom-up from dispersed utility maximization, or is it steered by modernizing elites who control education, print media, and administrative access?',
    'Network analysis of elite influence on adoption curves; measuring whether regional variation correlates with local utility conditions or with proximity to elite institutions.',
    'If elite-steered, the coordination story is cover for asymmetric extraction and the beneficiary structure concentrates upward; if bottom-up, the extraction is more diffuse and the rope characterization strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_steering_vs_bottom_up, empirical, 'Top-down steering versus bottom-up emergence in cultural evolution').

omega_variable(
    kernel_reading_location,
    'This constraint is the endogenous_displacement_reading of kernel legitimacy_of_practice_standardization. Would adopting the exogenous_override_reading or dual_practice_equilibrium_reading change the beneficiary/victim structure and the extracted seat?',
    'Comparative historical analysis of cases where practice change was attributed to different legitimacy sources; measure which reading predicts actual extraction patterns.',
    'Exogenous_override would identify state bureaucrats as agenda-setters and citizens as victims of decree; dual_practice would split beneficiaries and victims by public/private domain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Committer omega for kernel legitimacy_of_practice_standardization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(legi_tr_t10, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(legi_tr_t20, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(legi_tr_t30, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 30, 0.34).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 40, 0.36).
narrative_ontology:measurement(legi_tr_t50, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 50, 0.38).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(legi_be_t10, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(legi_be_t20, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 20, 0.44).
narrative_ontology:measurement(legi_be_t30, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 30, 0.46).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 40, 0.47).
narrative_ontology:measurement(legi_be_t50, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 50, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(legi_su_t10, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 10, 0.35).
narrative_ontology:measurement(legi_su_t20, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement(legi_su_t30, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 30, 0.4).
narrative_ontology:measurement(legi_su_t40, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 40, 0.41).
narrative_ontology:measurement(legi_su_t50, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 50, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__endogenous_displacement_reading, identity_coordination).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization__exogenous_override_reading).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading).

% DUAL FORMULATION NOTE:
% The natural-language concept 'legitimacy of practice standardization' decomposes into three structurally distinct constraints (three readings of a single kernel). This story addresses endogenous voluntary-adoption legitimacy; siblings address state-imposition legitimacy and domain-partitioned legitimacy. Each has distinct beneficiary/victim structures and epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
