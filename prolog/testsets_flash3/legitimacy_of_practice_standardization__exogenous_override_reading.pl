% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_practice_standardization__exogenous_override_reading, []).

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
 *   constraint_id: legitimacy_of_practice_standardization__exogenous_override_reading
 *   human_readable: Legitimacy of Practice Standardization via Exogenous State Decree
 *   domain: political_history/modernization_studies/institutional_change
 *
 * SUMMARY:
 *   This constraint describes the legitimacy of practice change when it is
 *   decreed by state authority for perceived collective benefit (e.g.,
 *   modernization, fiscal stability, international alignment). This
 *   'exogenous override' reading emphasizes abrupt legal imposition, active
 *   enforcement, and surface-level compliance that often masks persistent
 *   underground traditional practices. The constraint is framed as a Tangled
 *   Rope because it attempts to coordinate national practices while
 *   extracting conformity and suppressing traditional alternatives through
 *   coercive state power. The 'double life' of populations maintaining both
 *   official and traditional practices is a stable equilibrium, not a
 *   transitional phase.
 *
 * KEY AGENTS:
 *   - state_modernization_agenda_setters: Primary agenda setter (institutional/mobile) — decrees and enforces practice changes.
 *   - urban_elites_aligned_with_state: Primary beneficiary (powerful/mobile) — benefits from conformity and alignment with state power.
 *   - rural_traditional_communities: Primary target/payer (powerless/identity_locked) — bears the costs of forced change, maintains underground practices.
 *   - cultural_conservatives: Secondary target/payer (moderate/constrained) — resists changes, bears costs, works to preserve traditions.
 *   - international_observers: Analytical observer (analytical/analytical) — monitors and often supports state modernization narratives.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__exogenous_override_reading, 0.65).
domain_priors:suppression_score(legitimacy_of_practice_standardization__exogenous_override_reading, 0.78).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__exogenous_override_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__exogenous_override_reading, "Legitimacy of Practice Standardization via Exogenous State Decree").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__exogenous_override_reading, "political_history/modernization_studies/institutional_change").

domain_priors:requires_active_enforcement(legitimacy_of_practice_standardization__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__exogenous_override_reading, '12d69e06-381d-4767-ae2c-72cb77bfe6d7').
narrative_ontology:cs_kernel_codification('12d69e06-381d-4767-ae2c-72cb77bfe6d7', formalized).
narrative_ontology:cs_authority_grounding('12d69e06-381d-4767-ae2c-72cb77bfe6d7', extraction).
narrative_ontology:cs_interpretation_layer_present('12d69e06-381d-4767-ae2c-72cb77bfe6d7').
narrative_ontology:cs_reading_relation('12d69e06-381d-4767-ae2c-72cb77bfe6d7', legitimacy_of_practice_standardization__endogenous_displacement_reading, influences).
narrative_ontology:cs_reading_relation('12d69e06-381d-4767-ae2c-72cb77bfe6d7', legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, coexists_with).
narrative_ontology:cs_axiom('12d69e06-381d-4767-ae2c-72cb77bfe6d7', foundational, state_sovereignty_over_social_practice).
narrative_ontology:cs_axiom_status(state_sovereignty_over_social_practice, holdable).
narrative_ontology:cs_axiom_grounding('12d69e06-381d-4767-ae2c-72cb77bfe6d7', state_sovereignty_over_social_practice, conventional).
narrative_ontology:cs_axiom('12d69e06-381d-4767-ae2c-72cb77bfe6d7', foundational, collective_benefit_justifies_coercion).
narrative_ontology:cs_axiom_status(collective_benefit_justifies_coercion, holdable).
narrative_ontology:cs_axiom_grounding('12d69e06-381d-4767-ae2c-72cb77bfe6d7', collective_benefit_justifies_coercion, instrumental).
narrative_ontology:cs_reference_frame('12d69e06-381d-4767-ae2c-72cb77bfe6d7', modernizing_state_supremacy).
narrative_ontology:cs_drift_state('12d69e06-381d-4767-ae2c-72cb77bfe6d7', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('12d69e06-381d-4767-ae2c-72cb77bfe6d7', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__exogenous_override_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, state_modernization_agenda_setters).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, urban_elites_aligned_with_state).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__exogenous_override_reading, rural_traditional_communities).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__exogenous_override_reading, cultural_conservatives).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Initiates and enforces decrees for practice standardization (e.g., calendar reform, dress codes) to align the nation with international norms, improve fiscal administration, or project an image of modernity. Benefits from perceived progress and increased state control.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, state_modernization_agenda_setters, agenda_setter,
    institutional, generational, mobile, national).

% Adopts new practices readily, often benefiting from their alignment with state power and international systems. They gain social capital and economic opportunities by conforming to the decreed standards.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, urban_elites_aligned_with_state, beneficiary,
    powerful, biographical, mobile, national).

% Are compelled to adopt new practices that often conflict with deeply ingrained cultural and religious traditions. They face penalties for non-compliance but often maintain traditional practices underground, leading a 'double life'. Their identity is tied to ancestral practices.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, rural_traditional_communities, payer,
    powerless, generational, identity_locked, local).

% Resist state-imposed changes to traditional practices on ideological or religious grounds. They bear the costs of non-compliance (fines, social marginalization) but actively work to preserve traditional ways, often through quiet defiance or organized opposition.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, cultural_conservatives, payer,
    moderate, generational, constrained, national).

% Monitor the state's modernization efforts, often providing aid or diplomatic support based on perceived progress. They may overlook the coercive aspects of standardization in favor of the stated goals of development and alignment.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, international_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to standardize diverse local practices (e.g., multiple calendars, varied dress codes) into a single national system, facilitating administrative efficiency, national unity, and international integration.
% TRANSFER_FUNCTION: Transfers legitimacy and authority from traditional local institutions to the central state, and imposes the social and economic costs of forced adaptation onto traditional communities, while conferring benefits (e.g., perceived modernity, administrative ease) onto the state and its aligned elites.
% ABSENT_VOICES: Traditional religious leaders and local customary authorities, whose legitimacy is directly challenged by state decrees, are often excluded from the decision-making process. Their voices would emphasize the cultural disruption and loss of identity caused by forced standardization.
% DISAPPEARANCE_RATIONALE: If the state's authority to decree practice changes vanished, many traditional communities would revert to or openly practice their ancestral customs. The 'double life' would cease, and the state's administrative and symbolic control over daily life would diminish, leading to a re-emergence of diverse local practices.
% FOUNDING_PROBLEM: The state perceived a lack of national cohesion, administrative inefficiency, and an image of 'backwardness' due to diverse, often localized, traditional practices (e.g., multiple calendars hindering national planning, traditional dress seen as an impediment to modernity).
% FOUNDING_PROBLEM_CORROBORATION: The state and its aligned urban elites continue to attest that the problem of 'modernization' and 'national unity' is live, citing ongoing needs for international alignment and administrative efficiency. However, rural communities and cultural conservatives argue that the original problem was misdiagnosed or has been superseded by the imposition of a new, more fundamental problem of cultural suppression. International observers often corroborate the state's framing of the problem, focusing on development metrics.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__exogenous_override_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__exogenous_override_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_practice_standardization__exogenous_override_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_practice_standardization__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_practice_standardization__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_practice_standardization__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is substantial because traditional communities are forced to abandon or hide practices central to their identity, incurring social and economic costs. Suppression (0.78) is high due to active state enforcement (fines, legal sanctions, social pressure) against non-compliance. The theater ratio (0.55) is significant because surface compliance (e.g., using the official calendar for public administration) often coexists with continued private adherence to traditional practices (e.g., lunar calendar for rituals), creating a performative aspect to the state's 'success'. The claimed type is Tangled Rope because the state genuinely seeks to coordinate national practices for perceived collective benefit (e.g., administrative efficiency, international alignment), but this coordination is achieved through asymmetric extraction and suppression of alternatives.
 *
 * PERSPECTIVAL GAP:
 *   The state agenda-setters and aligned urban elites perceive this as a legitimate and necessary coordination mechanism for national progress, experiencing it as a Rope or even a Mountain (natural law of progress). In contrast, rural traditional communities and cultural conservatives experience it as a Snare, a purely extractive mechanism that suppresses their cultural identity. The engine's classification as Tangled Rope reflects the hybrid nature of this constraint, acknowledging both the coordination function and the asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   State modernization agenda-setters are full beneficiaries (d=0.0) as they gain control and legitimacy. Urban elites aligned with the state are also beneficiaries (d=0.15) as they benefit from the new order. Rural traditional communities are full targets (d=1.0) due to identity-locked exit and direct imposition. Cultural conservatives are also targets (d=0.8) with constrained exit. International observers are analytical (d=0.5) and do not directly benefit or pay.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure Rope (as the state claims) by highlighting the substantial extraction and suppression. It also prevents mislabeling it as a pure Snare by acknowledging the genuine, albeit coercively achieved, coordination function (e.g., a single national calendar for administrative purposes). The 'double life' phenomenon, where official and traditional practices coexist, is a key indicator of the theatricality and the ongoing, active enforcement required to maintain the state's desired 'modernity' on the surface.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_source_ambiguity,
    'Is the legitimacy of practice change derived from state decree (exogenous) or from voluntary adoption and cultural evolution (endogenous)?',
    'Longitudinal ethnographic studies tracking the actual drivers of practice change in communities, distinguishing between coerced compliance and genuine internal shifts in belief or utility.',
    'If legitimacy is primarily endogenous, this constraint would be reclassified closer to a Rope (if beneficial) or Piton (if inertially maintained). If exogenous, the Tangled Rope classification is reinforced, highlighting the coercive aspect.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_source_ambiguity, conceptual, 'Ambiguity regarding the ultimate source of legitimacy for practice change.').

omega_variable(
    double_life_stability,
    'Is the ''double life'' (surface compliance, underground tradition) a stable equilibrium or a temporary phase leading to full assimilation or open resistance?',
    'Decades-long observation of communities under such decrees: if the dual practice persists across generations without significant shifts, it''s stable. If one practice displaces the other, it''s transitional.',
    'If stable, the theater_ratio and suppression metrics are accurate for a long-term state. If transitional, the current metrics might overstate long-term theatricality or understate eventual assimilation/resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(double_life_stability, empirical, 'Whether dual practice is a stable state or a phase.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal penalties, economic disincentives) or internalized (social pressure, belief in state authority)?',
    'Post-decree enforcement trajectory: if suppression persists after the overt enforcement mechanisms are removed, reclassify as partially internalized. If it collapses, it''s structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit. This would push the classification closer to a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in practice standardization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__exogenous_override_reading, 1920, 1980).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t1920, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 1920, 0.4).
narrative_ontology:measurement(legi_tr_t1930, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 1930, 0.48).
narrative_ontology:measurement(legi_tr_t1940, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 1940, 0.55).
narrative_ontology:measurement(legi_tr_t1950, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 1950, 0.6).
narrative_ontology:measurement(legi_tr_t1960, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 1960, 0.58).
narrative_ontology:measurement(legi_tr_t1970, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 1970, 0.56).
narrative_ontology:measurement(legi_tr_t1980, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 1980, 0.55).

% Extraction over time
narrative_ontology:measurement(legi_be_t1920, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 1920, 0.55).
narrative_ontology:measurement(legi_be_t1930, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 1930, 0.6).
narrative_ontology:measurement(legi_be_t1940, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 1940, 0.65).
narrative_ontology:measurement(legi_be_t1950, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 1950, 0.68).
narrative_ontology:measurement(legi_be_t1960, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 1960, 0.67).
narrative_ontology:measurement(legi_be_t1970, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 1970, 0.66).
narrative_ontology:measurement(legi_be_t1980, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 1980, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t1920, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 1920, 0.7).
narrative_ontology:measurement(legi_su_t1930, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 1930, 0.75).
narrative_ontology:measurement(legi_su_t1940, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 1940, 0.78).
narrative_ontology:measurement(legi_su_t1950, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 1950, 0.77).
narrative_ontology:measurement(legi_su_t1960, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 1960, 0.76).
narrative_ontology:measurement(legi_su_t1970, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 1970, 0.77).
narrative_ontology:measurement(legi_su_t1980, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 1980, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__exogenous_override_reading, legitimacy_of_practice_standardization__endogenous_displacement_reading).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__exogenous_override_reading, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'legitimacy of practice standardization' kernel. This 'exogenous override' reading emphasizes state-decreed change, contrasting with the 'endogenous displacement' (voluntary adoption) and 'dual practice equilibrium' (domain-partitioned legitimacy) readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
