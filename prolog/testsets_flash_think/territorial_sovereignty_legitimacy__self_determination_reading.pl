% ============================================================================
% CONSTRAINT STORY: territorial_sovereignty_legitimacy__self_determination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_sovereignty_legitimacy__self_determination_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: territorial_sovereignty_legitimacy__self_determination_reading
 *   human_readable: Territorial Sovereignty Legitimacy (Self-Determination Reading)
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint story instantiates the 'self_determination_reading' of
 *   the 'territorial_sovereignty_legitimacy' kernel. It describes the
 *   existing arrangement in the territory as a Snare, where the modern
 *   principle of self-determination, applied to the Arab population with
 *   demographic majority and continuous residence during the 19th-20th
 *   centuries, is systematically denied. The constraint is the active
 *   enforcement of this denial, which is viewed as highly extractive and
 *   suppressive from the perspective of the Arab population.
 *
 * KEY AGENTS:
 *   - arab_population_in_territory: Primary target (powerless/identity_locked) — bears extraction and suppression.
 *   - palestinian_diaspora: Secondary target/excluded (organized/identity_locked) — bears costs of displacement and exclusion.
 *   - israeli_state: Primary agenda_setter/beneficiary (institutional/constrained) — benefits from the existing territorial arrangement.
 *   - international_community: Observer/secondary agenda_setter (institutional/analytical) — formally upholds principles but often fails to enforce them.
 *   - international_powers_maintaining_status_quo: Beneficiary (institutional/arbitrage) — benefits from geopolitical stability at the expense of self-determination.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__self_determination_reading, 0.88).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__self_determination_reading, 0.92).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__self_determination_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__self_determination_reading, snare).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__self_determination_reading, "Territorial Sovereignty Legitimacy (Self-Determination Reading)").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__self_determination_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__self_determination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__self_determination_reading, '7a6fb7bf-532a-49df-9605-928a29f4e332').
narrative_ontology:cs_kernel_codification('7a6fb7bf-532a-49df-9605-928a29f4e332', formalized).
narrative_ontology:cs_authority_grounding('7a6fb7bf-532a-49df-9605-928a29f4e332', lineage).
narrative_ontology:cs_interpretation_layer_present('7a6fb7bf-532a-49df-9605-928a29f4e332').
narrative_ontology:cs_reading_relation('7a6fb7bf-532a-49df-9605-928a29f4e332', territorial_sovereignty_legitimacy__covenant_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('7a6fb7bf-532a-49df-9605-928a29f4e332', territorial_sovereignty_legitimacy__existential_matrix_reading, coexists_with).
narrative_ontology:cs_axiom('7a6fb7bf-532a-49df-9605-928a29f4e332', foundational, self_determination_is_universal_right).
narrative_ontology:cs_axiom_status(self_determination_is_universal_right, holdable).
narrative_ontology:cs_axiom_grounding('7a6fb7bf-532a-49df-9605-928a29f4e332', self_determination_is_universal_right, deontological).
narrative_ontology:cs_axiom('7a6fb7bf-532a-49df-9605-928a29f4e332', foundational, colonial_settlement_is_illegitimate).
narrative_ontology:cs_axiom_status(colonial_settlement_is_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('7a6fb7bf-532a-49df-9605-928a29f4e332', colonial_settlement_is_illegitimate, conventional).
narrative_ontology:cs_reference_frame('7a6fb7bf-532a-49df-9605-928a29f4e332', post_wwi_self_determination_era).
narrative_ontology:cs_drift_state('7a6fb7bf-532a-49df-9605-928a29f4e332', contemporary_geopolitical_realities, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7a6fb7bf-532a-49df-9605-928a29f4e332', '').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__self_determination_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__self_determination_reading, israeli_state).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__self_determination_reading, international_powers_maintaining_status_quo).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__self_determination_reading, arab_population_in_territory).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_diaspora).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The indigenous population whose right to self-determination and continuous residence forms the basis of this reading's claim to sovereignty. They bear the costs of occupation, displacement, and denial of political agency. Exit means abandoning their ancestral lands and identity.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, arab_population_in_territory, payer,
    powerless, generational, identity_locked, regional).

% Descendants of those displaced, who assert a right of return and self-determination based on historical presence and modern principles. They are excluded from direct political participation in the territory but maintain organized resistance and advocacy globally. Their identity is deeply tied to the land.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_diaspora, payer,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_diaspora, excluded).

% The entity exercising de facto sovereignty over the territory. From this reading's perspective, its legitimacy is challenged, and it benefits from the denial of self-determination to the Arab population. It actively enforces the existing territorial and demographic status quo.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, israeli_state, agenda_setter,
    institutional, generational, constrained, national).

% Comprises states and international organizations that formally endorse the principle of self-determination but often fail to enforce it consistently in this context. They observe, mediate, and sometimes impose sanctions, but their actions are often insufficient to alter the fundamental power dynamics.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, international_community, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__self_determination_reading, international_community, agenda_setter).

% Major global powers whose geopolitical interests align with maintaining the existing state of affairs, even if it contradicts the self-determination principle. They benefit from regional stability (as they define it) and strategic alliances, often at the expense of enforcing universal principles.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, international_powers_maintaining_status_quo, beneficiary,
    institutional, generational, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_sovereignty_legitimacy__self_determination_reading, israeli_state).
narrative_ontology:fixing_cost_class(territorial_sovereignty_legitimacy__self_determination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From this reading's perspective, the existing arrangement coordinates the suppression of the Arab population's self-determination and the maintenance of a territorial status quo that benefits the Israeli state and its allies, rather than solving a genuine collective action problem for all parties.
% TRANSFER_FUNCTION: Transfers land, resources, political control, and the right to self-determination from the Arab population to the Israeli state, enforced by military and legal mechanisms.
% ABSENT_VOICES: The full, unconstrained voice of the Arab population, particularly those displaced or living under occupation, is largely absent from the international forums where the territory's future is debated. Their claims for self-determination and right of return are often marginalized or reframed.
% DISAPPEARANCE_RATIONALE: If the constraint (the denial of self-determination based on modern principles) vanished, the entire political and demographic structure of the territory would be fundamentally challenged. The Israeli state's legitimacy would be questioned, the Arab population would assert its rights, and the geopolitical landscape would undergo a radical transformation.
% FOUNDING_PROBLEM: The founding problem, from this reading's perspective, was the imposition of a colonial-settler project and the denial of the indigenous Arab population's right to self-determination and continuous residence in their homeland during the modern period.
% FOUNDING_PROBLEM_CORROBORATION: Numerous international legal scholars, human rights organizations, UN resolutions, and historical records from independent sources corroborate the ongoing denial of self-determination and the persistence of the founding problem, contradicting claims by the Israeli state and its allies that the conflict is purely about security or ancient claims.
narrative_ontology:disappearance_verdict(territorial_sovereignty_legitimacy__self_determination_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_sovereignty_legitimacy__self_determination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__self_determination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__self_determination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_sovereignty_legitimacy__self_determination_reading, 0.88, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_sovereignty_legitimacy__self_determination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__self_determination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_sovereignty_legitimacy__self_determination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.88) because the core political agency and territorial rights of the Arab population are denied. Suppression is extremely high (0.92) due to ongoing military occupation, legal restrictions, and control over movement and resources, actively preventing the realization of self-determination. Theater ratio is moderate (0.45) as international rhetoric often pays lip service to self-determination while practical enforcement is weak or contradictory. Accessibility collapse is high (0.85) as viable alternatives for self-determination are systematically dismantled or prevented. Resistance is very high (0.90) reflecting the continuous struggle by the Arab population against the imposed status quo. The claimed type is Snare because the coordination story (e.g., 'security' or 'historical rights' from other readings) is seen as cover for a fundamentally extractive and coercive structure.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Arab population, the constraint is a clear Snare, actively denying their fundamental rights. From the Israeli state's perspective, the arrangement might be framed as a necessary Rope for security or a Mountain based on historical claims. The international community often views it as a Tangled Rope, acknowledging both coordination and extraction, but this reading emphasizes the overwhelming extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The Israeli state is a clear beneficiary, as the constraint secures its territorial control and demographic policies. International powers that prioritize regional stability over the enforcement of self-determination also benefit. The Arab population in the territory and the Palestinian diaspora are the primary targets, bearing the full weight of denied self-determination, displacement, and ongoing suppression. The international community, while formally upholding self-determination, often acts as an observer whose actions are insufficient to shift the fundamental directionality for the victims.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the denial of self-determination as a legitimate coordination mechanism. By identifying it as a Snare, it highlights that the 'coordination' function (e.g., maintaining 'order' or 'security') is primarily a cover for extraction and suppression, rather than a genuine collective benefit. The high resistance and suppression metrics further underscore that this is not a consensual arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_modern_period,
    'What specific historical period constitutes the ''modern period'' for establishing demographic majority and continuous residence, and how does this definition impact the legitimacy claim?',
    'Historical and demographic studies establishing consensus on the relevant timeframe for ''modern period'' in international law and self-determination discourse.',
    'A narrower or broader definition of the ''modern period'' could alter the demographic baseline, potentially strengthening or weakening the self-determination claim based on continuous residence and majority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_of_modern_period, conceptual, 'Ambiguity in the temporal scope of ''modern period'' for self-determination claims.').

omega_variable(
    continuous_residence_threshold,
    'What constitutes ''continuous residence'' in the context of forced displacement and diaspora, and how does this affect the claim of self-determination?',
    'Legal scholarship and international precedents on the rights of displaced populations and refugees to maintain claims to ancestral lands despite forced absence.',
    'A strict interpretation of ''continuous residence'' could weaken the claims of the diaspora, while a more expansive interpretation would strengthen them, impacting the victim set and the scope of the injustice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuous_residence_threshold, conceptual, 'Ambiguity in the definition of ''continuous residence'' for self-determination claims.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (external barriers) or internalized (cognitive patterns that persist after barrier removal)?',
    'Post-exit suppression trajectory: if suppression persists after the extractive mechanism is removed (e.g., in diaspora communities), reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making the path to self-determination even harder.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the context of occupation and displacement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__self_determination_reading, 1948, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1948, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1948, 0.3).
narrative_ontology:measurement(terr_tr_t1967, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1967, 0.35).
narrative_ontology:measurement(terr_tr_t1987, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1987, 0.4).
narrative_ontology:measurement(terr_tr_t2000, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 2000, 0.43).
narrative_ontology:measurement(terr_tr_t2010, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 2010, 0.44).
narrative_ontology:measurement(terr_tr_t2023, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 2023, 0.45).

% Extraction over time
narrative_ontology:measurement(terr_be_t1948, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1948, 0.75).
narrative_ontology:measurement(terr_be_t1967, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1967, 0.82).
narrative_ontology:measurement(terr_be_t1987, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1987, 0.85).
narrative_ontology:measurement(terr_be_t2000, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 2000, 0.87).
narrative_ontology:measurement(terr_be_t2010, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 2010, 0.88).
narrative_ontology:measurement(terr_be_t2023, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 2023, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1948, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1948, 0.78).
narrative_ontology:measurement(terr_su_t1967, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1967, 0.85).
narrative_ontology:measurement(terr_su_t1987, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1987, 0.89).
narrative_ontology:measurement(terr_su_t2000, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 2000, 0.91).
narrative_ontology:measurement(terr_su_t2010, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 2010, 0.92).
narrative_ontology:measurement(terr_su_t2023, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 2023, 0.92).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1948, tn=2023
narrative_ontology:measurement(terr_grid_01, territorial_sovereignty_legitimacy__self_determination_reading, accessibility_collapse(class), 1948, 0.8).
narrative_ontology:measurement(terr_grid_02, territorial_sovereignty_legitimacy__self_determination_reading, accessibility_collapse(class), 2023, 0.92).
narrative_ontology:measurement(terr_grid_03, territorial_sovereignty_legitimacy__self_determination_reading, accessibility_collapse(individual), 1948, 0.6).
narrative_ontology:measurement(terr_grid_04, territorial_sovereignty_legitimacy__self_determination_reading, accessibility_collapse(individual), 2023, 0.85).
narrative_ontology:measurement(terr_grid_05, territorial_sovereignty_legitimacy__self_determination_reading, accessibility_collapse(organizational), 1948, 0.7).
narrative_ontology:measurement(terr_grid_06, territorial_sovereignty_legitimacy__self_determination_reading, accessibility_collapse(organizational), 2023, 0.9).
narrative_ontology:measurement(terr_grid_07, territorial_sovereignty_legitimacy__self_determination_reading, accessibility_collapse(structural), 1948, 0.85).
narrative_ontology:measurement(terr_grid_08, territorial_sovereignty_legitimacy__self_determination_reading, accessibility_collapse(structural), 2023, 0.95).
narrative_ontology:measurement(terr_grid_09, territorial_sovereignty_legitimacy__self_determination_reading, resistance(class), 1948, 0.9).
narrative_ontology:measurement(terr_grid_10, territorial_sovereignty_legitimacy__self_determination_reading, resistance(class), 2023, 0.92).
narrative_ontology:measurement(terr_grid_11, territorial_sovereignty_legitimacy__self_determination_reading, resistance(individual), 1948, 0.8).
narrative_ontology:measurement(terr_grid_12, territorial_sovereignty_legitimacy__self_determination_reading, resistance(individual), 2023, 0.85).
narrative_ontology:measurement(terr_grid_13, territorial_sovereignty_legitimacy__self_determination_reading, resistance(organizational), 1948, 0.85).
narrative_ontology:measurement(terr_grid_14, territorial_sovereignty_legitimacy__self_determination_reading, resistance(organizational), 2023, 0.9).
narrative_ontology:measurement(terr_grid_15, territorial_sovereignty_legitimacy__self_determination_reading, resistance(structural), 1948, 0.7).
narrative_ontology:measurement(terr_grid_16, territorial_sovereignty_legitimacy__self_determination_reading, resistance(structural), 2023, 0.8).
narrative_ontology:measurement(terr_grid_17, territorial_sovereignty_legitimacy__self_determination_reading, stakes_inflation(class), 1948, 0.85).
narrative_ontology:measurement(terr_grid_18, territorial_sovereignty_legitimacy__self_determination_reading, stakes_inflation(class), 2023, 0.95).
narrative_ontology:measurement(terr_grid_19, territorial_sovereignty_legitimacy__self_determination_reading, stakes_inflation(individual), 1948, 0.65).
narrative_ontology:measurement(terr_grid_20, territorial_sovereignty_legitimacy__self_determination_reading, stakes_inflation(individual), 2023, 0.9).
narrative_ontology:measurement(terr_grid_21, territorial_sovereignty_legitimacy__self_determination_reading, stakes_inflation(organizational), 1948, 0.75).
narrative_ontology:measurement(terr_grid_22, territorial_sovereignty_legitimacy__self_determination_reading, stakes_inflation(organizational), 2023, 0.92).
narrative_ontology:measurement(terr_grid_23, territorial_sovereignty_legitimacy__self_determination_reading, stakes_inflation(structural), 1948, 0.9).
narrative_ontology:measurement(terr_grid_24, territorial_sovereignty_legitimacy__self_determination_reading, stakes_inflation(structural), 2023, 0.98).
narrative_ontology:measurement(terr_grid_25, territorial_sovereignty_legitimacy__self_determination_reading, suppression(class), 1948, 0.8).
narrative_ontology:measurement(terr_grid_26, territorial_sovereignty_legitimacy__self_determination_reading, suppression(class), 2023, 0.95).
narrative_ontology:measurement(terr_grid_27, territorial_sovereignty_legitimacy__self_determination_reading, suppression(individual), 1948, 0.7).
narrative_ontology:measurement(terr_grid_28, territorial_sovereignty_legitimacy__self_determination_reading, suppression(individual), 2023, 0.9).
narrative_ontology:measurement(terr_grid_29, territorial_sovereignty_legitimacy__self_determination_reading, suppression(organizational), 1948, 0.75).
narrative_ontology:measurement(terr_grid_30, territorial_sovereignty_legitimacy__self_determination_reading, suppression(organizational), 2023, 0.92).
narrative_ontology:measurement(terr_grid_31, territorial_sovereignty_legitimacy__self_determination_reading, suppression(structural), 1948, 0.85).
narrative_ontology:measurement(terr_grid_32, territorial_sovereignty_legitimacy__self_determination_reading, suppression(structural), 2023, 0.98).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__self_determination_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__self_determination_reading, territorial_sovereignty_legitimacy__covenant_continuity_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__self_determination_reading, territorial_sovereignty_legitimacy__existential_matrix_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'territorial_sovereignty_legitimacy' kernel, each representing a distinct structural claim about the basis of sovereignty in the contested territory. This reading focuses on modern self-determination principles for the Arab population.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
