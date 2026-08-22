% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__temporal_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eternal_marriage_covenant__temporal_accommodation_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: eternal_marriage_covenant__temporal_accommodation_reading
 *   human_readable: Eternal Marriage Covenant (Temporal Accommodation Reading)
 *   domain: religious_law/political_theology/commitment_system_dynamics
 *
 * SUMMARY:
 *   This constraint story represents the 'temporal accommodation' reading of
 *   the eternal marriage covenant, specifically concerning the practice of
 *   polygamy within a religious tradition. This reading posits that while the
 *   doctrine of plural marriage remains an eternal principle, its practice is
 *   suspended due to external pressures (e.g., federal law). The suspension
 *   is viewed as a temporary measure, with the doctrine remaining valid but
 *   dormant, awaiting a future restoration when political constraints lift.
 *   This framing allows the church to maintain doctrinal consistency while
 *   complying with the law of the land.
 *
 * KEY AGENTS:
 *   - church_leadership: Agenda setter (institutional/arbitrage) — administers the suspension, interprets doctrine.
 *   - church_members: Beneficiary/Payer (organized/constrained) — benefit from church standing, bear costs of compliance.
 *   - polygamous_families_in_transition: Victim (powerless/trapped) — bear the direct costs of abandoning practice.
 *   - federal_government: Agenda setter (institutional/analytical) — enforces anti-polygamy laws.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__temporal_accommodation_reading, 0.3).
domain_priors:suppression_score(eternal_marriage_covenant__temporal_accommodation_reading, 0.6).
domain_priors:theater_ratio(eternal_marriage_covenant__temporal_accommodation_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__temporal_accommodation_reading, scaffold).
narrative_ontology:human_readable(eternal_marriage_covenant__temporal_accommodation_reading, "Eternal Marriage Covenant (Temporal Accommodation Reading)").
narrative_ontology:topic_domain(eternal_marriage_covenant__temporal_accommodation_reading, "religious_law/political_theology/commitment_system_dynamics").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__temporal_accommodation_reading).
narrative_ontology:has_sunset_clause(eternal_marriage_covenant__temporal_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__temporal_accommodation_reading, '2223811c-17d2-43fc-9454-8f71217db947').
narrative_ontology:cs_kernel_codification('2223811c-17d2-43fc-9454-8f71217db947', fixed_text).
narrative_ontology:cs_authority_grounding('2223811c-17d2-43fc-9454-8f71217db947', lineage).
narrative_ontology:cs_interpretation_layer_present('2223811c-17d2-43fc-9454-8f71217db947').
narrative_ontology:cs_reading_relation('2223811c-17d2-43fc-9454-8f71217db947', eternal_marriage_covenant__immutable_commandment_reading, coexists_with).
narrative_ontology:cs_reading_relation('2223811c-17d2-43fc-9454-8f71217db947', eternal_marriage_covenant__prophetic_override_reading, coexists_with).
narrative_ontology:cs_axiom('2223811c-17d2-43fc-9454-8f71217db947', foundational, divine_law_is_eternal_but_practice_can_be_suspended).
narrative_ontology:cs_axiom_status(divine_law_is_eternal_but_practice_can_be_suspended, holdable).
narrative_ontology:cs_axiom_grounding('2223811c-17d2-43fc-9454-8f71217db947', divine_law_is_eternal_but_practice_can_be_suspended, theological).
narrative_ontology:cs_axiom('2223811c-17d2-43fc-9454-8f71217db947', secondary, obedience_to_law_of_land_is_a_divine_mandate).
narrative_ontology:cs_axiom_status(obedience_to_law_of_land_is_a_divine_mandate, holdable).
narrative_ontology:cs_axiom_grounding('2223811c-17d2-43fc-9454-8f71217db947', obedience_to_law_of_land_is_a_divine_mandate, theological).
narrative_ontology:cs_reference_frame('2223811c-17d2-43fc-9454-8f71217db947', eternal_doctrine_with_temporal_flexibility).
narrative_ontology:cs_drift_state('2223811c-17d2-43fc-9454-8f71217db947', contemporary_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2223811c-17d2-43fc-9454-8f71217db947', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__temporal_accommodation_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__temporal_accommodation_reading, church_leadership).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__temporal_accommodation_reading, church_members).
narrative_ontology:constraint_victim(eternal_marriage_covenant__temporal_accommodation_reading, polygamous_families_in_transition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(eternal_marriage_covenant__temporal_accommodation_reading, church_members).
narrative_ontology:constraint_vindicates(eternal_marriage_covenant__temporal_accommodation_reading, obedience_to_law_of_land).
narrative_ontology:constraint_vindicates(eternal_marriage_covenant__temporal_accommodation_reading, divine_law_is_eternal).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the suspension of polygamous practice, interprets doctrine, and navigates the church's relationship with secular law. Benefits from maintaining the church's legal standing and public image, but bears the internal tension of doctrinal consistency.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, church_leadership, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from continued membership in the church and its community, but bear the cost of conforming to the suspended practice, which may conflict with personal beliefs or family history. Their identity is deeply tied to the church's teachings.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, church_members, beneficiary,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(eternal_marriage_covenant__temporal_accommodation_reading, church_members, payer).

% Directly impacted by the suspension, facing legal and social pressure to abandon polygamous practices. They bear significant personal, familial, and economic costs, with limited options for recourse or exit without losing their community or facing legal repercussions.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, polygamous_families_in_transition, payer,
    powerless, immediate, trapped, local).

% Enforces anti-polygamy laws, creating the external pressure that led to the church's accommodation. Its role is to uphold secular law, and it views the church's suspension of practice as compliance.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, federal_government, agenda_setter,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the church's adherence to secular law while preserving the integrity of its eternal doctrines, allowing the institution to maintain its legal status and social acceptance.
% TRANSFER_FUNCTION: Transfers the burden of compliance from the church as an institution to individual members, particularly those who previously practiced or believed in polygamy, in exchange for the church's continued legal and social standing.
% ABSENT_VOICES: Fundamentalist groups who continue to practice polygamy, viewing the Manifesto as a betrayal of divine law. They are excluded from the mainstream church's discourse and face legal persecution.
% DISAPPEARANCE_RATIONALE: If this accommodation vanished overnight, the church would face immediate legal challenges and social condemnation, potentially losing its tax-exempt status and institutional legitimacy. The relationship between the church and the federal government would be severely strained, and internal doctrinal conflicts would resurface, forcing a re-evaluation of core tenets.
% FOUNDING_PROBLEM: The church faced existential threats from the federal government due to its practice of polygamy, including confiscation of property, disenfranchisement of members, and imprisonment of leaders.
% FOUNDING_PROBLEM_CORROBORATION: Historical records, legal documents from the late 19th century, and contemporary academic analyses corroborate the severe federal pressure and existential threat faced by the church. These sources, external to the church's direct beneficiaries, confirm the problem's historical reality and its ongoing relevance in shaping church policy.
narrative_ontology:disappearance_verdict(eternal_marriage_covenant__temporal_accommodation_reading, world_rearranges).
narrative_ontology:founding_problem_status(eternal_marriage_covenant__temporal_accommodation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__temporal_accommodation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(eternal_marriage_covenant__temporal_accommodation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eternal_marriage_covenant__temporal_accommodation_reading, 0.3, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eternal_marriage_covenant__temporal_accommodation_reading_tests).
:- end_tests(eternal_marriage_covenant__temporal_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Scaffold because it represents a temporary support structure for the church's legal standing and social acceptance, intended to be transitional until external conditions change. Extractiveness (0.3) is moderate, reflecting the internal tension and the cost of abandoning a core practice, but lower than a Snare because the doctrine itself is not renounced. Suppression (0.6) is significant due to both federal law and internal church enforcement. Theater ratio (0.4) is present as the church publicly maintains the doctrine's validity while not practicing it, creating a performative aspect to its adherence. The measurements show a decrease in extractiveness and suppression over time as the accommodation becomes more established, and a rise in theater ratio as the gap between doctrine and practice widens.
 *
 * PERSPECTIVAL GAP:
 *   Church leadership experiences this as a necessary, temporary adaptation that preserves the institution and its core doctrines. Members who previously practiced polygamy, or who believe in its eternal validity, experience it as a significant personal sacrifice and a suppression of divine command. The federal government views its anti-polygamy laws as a fixed mountain, with the church's accommodation as a necessary compliance.
 *
 * DIRECTIONALITY LOGIC:
 *   Church leadership is a beneficiary (d=0.1) as the accommodation preserves the church's legal status and avoids conflict with the federal government. Church members are both beneficiaries (d=0.4) of the church's stability and payers of the social and personal cost of compliance. Polygamous families in transition are victims (d=0.9) as they bear the direct and immediate costs of abandoning their lifestyle and family structure. The federal government, while an external enforcer, is not a direct stakeholder in the internal constraint's operation, but its laws create the external pressure.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the accommodation as a permanent doctrinal shift (which would be a Rope or even a Mountain from a 'prophetic override' perspective). By identifying it as a Scaffold, the framework highlights its transitional nature and the underlying tension between doctrine and practice, which is crucial for understanding its potential for future change or reversal. It also avoids mislabeling it as a pure Snare, acknowledging the genuine coordination function of maintaining the church's legal standing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the suspension of polygamous practice a temporary accommodation to external pressure, or a permanent doctrinal shift?',
    'Future prophetic pronouncements or changes in church policy regarding the practice if political constraints were removed.',
    'If permanent, the ''immutable commandment'' reading is foreclosed; if temporary, this ''temporal accommodation'' reading remains valid, and the ''immutable commandment'' reading is merely dormant.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'This constraint is the ''temporal accommodation'' reading of the ''eternal_marriage_covenant'' kernel. Sibling readings include ''immutable_commandment_reading'' and ''prophetic_override_reading''. The core disagreement is whether the suspension is temporary or permanent.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of polygamous practice structural (federal law) or internalized (church teaching and social pressure)?',
    'Post-exit suppression trajectory: if individuals continue to avoid polygamous practice even after leaving the church or moving to jurisdictions where it is legal, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, even if federal law changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for polygamous practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__temporal_accommodation_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eter_tr_t0, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(eter_tr_t10, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(eter_tr_t20, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(eter_tr_t30, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(eter_be_t0, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 0, 0.8).
narrative_ontology:measurement(eter_be_t10, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(eter_be_t20, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(eter_be_t30, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 30, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(eter_su_t0, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 0, 0.9).
narrative_ontology:measurement(eter_su_t10, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 10, 0.8).
narrative_ontology:measurement(eter_su_t20, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(eter_su_t30, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 30, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__temporal_accommodation_reading, identity_coordination).
narrative_ontology:affects_constraint(eternal_marriage_covenant__temporal_accommodation_reading, immutable_commandment_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__temporal_accommodation_reading, prophetic_override_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'eternal_marriage_covenant' kernel. It is linked to 'immutable_commandment_reading' and 'prophetic_override_reading' as sibling interpretations of the same core doctrine, each with different implications for practice and authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
