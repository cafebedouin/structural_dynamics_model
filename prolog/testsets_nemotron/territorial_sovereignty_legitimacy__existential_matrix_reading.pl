% ============================================================================
% CONSTRAINT STORY: territorial_sovereignty_legitimacy__existential_matrix_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_sovereignty_legitimacy__existential_matrix_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: territorial_sovereignty_legitimacy__existential_matrix_reading
 *   human_readable: Existential Matrix Reading of Territorial Sovereignty Legitimacy
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint story instantiates the existential_matrix_reading of the
 *   contested kernel 'territorial_sovereignty_legitimacy'. The reading
 *   asserts that sovereignty legitimacy derives not from law, covenant, or
 *   self-determination principles, but from the existential requirement that
 *   each people must control territory as a precondition for collective
 *   survival and identity expression. This makes the conflict fundamentally
 *   zero-sum: territorial compromise is structurally unstable because neither
 *   side can accept the vulnerability inherent in sharing or partitioning the
 *   land. The beneficiary is whichever side achieves demographic and military
 *   dominance; the victims are those rendered vulnerable by that dominance.
 *   The reading treats juridical claims (covenant, international law,
 *   self-determination) as epiphenomenal — post-hoc rationalizations for the
 *   existential drive. The kernel includes two sibling readings:
 *   covenant_continuity_reading (legitimacy from divine promise + continuous
 *   presence + international recognition) and self_determination_reading
 *   (legitimacy from modern self-determination applied to the Arab population
 *   with demographic majority and continuous residence). This story generates
 *   ONLY the existential_matrix_reading as a clean, ε-invariant constraint
 *   per Rule 1.
 *
 * KEY AGENTS:
 *   - dominant_military_power: Primary beneficiary (institutional/arbitrage) — controls territory, sets rules, extracts security
 *   - demographic_majority_group: Secondary beneficiary (organized/constrained) — gains existential security through demographic control
 *   - minority_national_group: Primary victim (powerless/trapped) — existential vulnerability under other's control
 *   - displaced_population: Victim (powerless/trapped) — removed from territorial control, identity expression blocked
 *   - civilian_populations_under_occupation: Victim (moderate/constrained) — daily life structured by the dominant power's existential security requirements
 *   - international_legal_order: Observer (institutional/analytical) — provides juridical frameworks the reading treats as epiphenomenal
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.88).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.92).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__existential_matrix_reading, tangled_rope).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__existential_matrix_reading, "Existential Matrix Reading of Territorial Sovereignty Legitimacy").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__existential_matrix_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__existential_matrix_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__existential_matrix_reading, '7a801912-49d6-4bd7-bcd2-3dd53e9c2a89').
narrative_ontology:cs_kernel_codification('7a801912-49d6-4bd7-bcd2-3dd53e9c2a89', distributed).
narrative_ontology:cs_authority_grounding('7a801912-49d6-4bd7-bcd2-3dd53e9c2a89', extraction).
narrative_ontology:cs_interpretation_layer_present('7a801912-49d6-4bd7-bcd2-3dd53e9c2a89').
narrative_ontology:cs_reading_relation('7a801912-49d6-4bd7-bcd2-3dd53e9c2a89', territorial_sovereignty_legitimacy__covenant_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('7a801912-49d6-4bd7-bcd2-3dd53e9c2a89', territorial_sovereignty_legitimacy__self_determination_reading, coexists_with).
narrative_ontology:cs_axiom('7a801912-49d6-4bd7-bcd2-3dd53e9c2a89', foundational, existential_fear_primary_driver).
narrative_ontology:cs_axiom_status(existential_fear_primary_driver, holdable).
narrative_ontology:cs_axiom_grounding('7a801912-49d6-4bd7-bcd2-3dd53e9c2a89', existential_fear_primary_driver, instrumental).
narrative_ontology:cs_axiom('7a801912-49d6-4bd7-bcd2-3dd53e9c2a89', foundational, juridical_claims_epiphenomenal).
narrative_ontology:cs_axiom_status(juridical_claims_epiphenomenal, holdable).
narrative_ontology:cs_axiom_grounding('7a801912-49d6-4bd7-bcd2-3dd53e9c2a89', juridical_claims_epiphenomenal, deontological).
narrative_ontology:cs_axiom('7a801912-49d6-4bd7-bcd2-3dd53e9c2a89', foundational, zero_sum_territorial_logic).
narrative_ontology:cs_axiom_status(zero_sum_territorial_logic, holdable).
narrative_ontology:cs_axiom_grounding('7a801912-49d6-4bd7-bcd2-3dd53e9c2a89', zero_sum_territorial_logic, instrumental).
narrative_ontology:cs_axiom('7a801912-49d6-4bd7-bcd2-3dd53e9c2a89', secondary, demographic_dominance_as_security).
narrative_ontology:cs_axiom_status(demographic_dominance_as_security, holdable).
narrative_ontology:cs_axiom_grounding('7a801912-49d6-4bd7-bcd2-3dd53e9c2a89', demographic_dominance_as_security, instrumental).
narrative_ontology:cs_reference_frame('7a801912-49d6-4bd7-bcd2-3dd53e9c2a89', post_holocaust_statelessness).
narrative_ontology:cs_drift_state('7a801912-49d6-4bd7-bcd2-3dd53e9c2a89', contemporary_occupation_reality, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7a801912-49d6-4bd7-bcd2-3dd53e9c2a89', '').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__existential_matrix_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__existential_matrix_reading, dominant_military_power).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__existential_matrix_reading, demographic_majority_group).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, minority_national_group).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, displaced_population).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, civilian_populations_under_occupation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, demographic_majority_group).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__existential_matrix_reading, existential_security_precedes_legal_order).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__existential_matrix_reading, zero_sum_territorial_conflict).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__existential_matrix_reading, demographic_military_dominance_as_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the territory through military force and legal architecture. Sets the rules of movement, residency, and resource allocation. Justifies control as existential security requirement. Collects the security benefits and demographic control directly. Can project power beyond the territory and has international alliances that provide exit options unavailable to other parties.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, dominant_military_power, agenda_setter,
    institutional, generational, arbitrage, regional).

% Gains existential security and collective identity expression through territorial control. Experiences the arrangement as coordination for survival. Also bears costs: permanent militarization of society, demographic anxiety, international isolation, and moral injury from occupation. Exit is constrained — emigration is possible but means abandoning the collective project; internal dissent is suppressed.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, demographic_majority_group, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__existential_matrix_reading, demographic_majority_group, payer).

% Exists under the dominant power's control with no territorial sovereignty. Collective identity expression is suppressed or channeled into resistance. Daily life structured by permits, checkpoints, and land expropriation. No meaningful exit: cannot leave without abandoning homeland, cannot achieve sovereignty without the dominant power's consent. Bears the full existential vulnerability the constraint's logic assigns to the non-dominant party.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, minority_national_group, payer,
    powerless, generational, trapped, local).

% Removed from the territory entirely — refugees and their descendants in camps or diaspora. The constraint's logic (territorial control as existential precondition) renders their return structurally impossible because it would create vulnerability for the dominant group. No exit from displacement: host states deny integration, return is blocked, third-country resettlement is minimal. Identity is fused with the lost territory, making the constraint identity-locked even in exile.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, displaced_population, payer,
    powerless, generational, trapped, regional).

% Daily life governed by the dominant power's security architecture: movement permits, building restrictions, resource allocation, legal systems they cannot influence. Some agency remains — commercial activity, cultural preservation, limited political organization — but all within the envelope set by the dominant power's existential security requirements. Exit is constrained: can leave but loses connection to land and community; staying means accepting structural subordination.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, civilian_populations_under_occupation, payer,
    moderate, biographical, constrained, local).

% Provides the juridical frameworks (international humanitarian law, human rights law, UN resolutions) that the existential reading treats as epiphenomenal. Issues opinions, resolutions, and legal findings that the dominant power ignores or reinterprets. Has no enforcement capacity independent of member states. Its situation is analytical: it observes the constraint's operation and records the gap between legal norms and existential facts on the ground.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, international_legal_order, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine coordination problem of existential security for a people who experienced genocide and statelessness: provides territorial control as the substrate for collective survival and identity expression. The coordination is real — without territorial control, the group faces existential vulnerability.
% TRANSFER_FUNCTION: Moves total territorial control and existential security from the minority population to the dominant power. The minority loses sovereignty, land, movement, and collective future; the dominant power gains security, demographic dominance, and ideological fulfillment. The transfer is enforced through military occupation, legal architecture, and demographic engineering.
% ABSENT_VOICES: The minority national group's legitimate leadership (as distinct from factions co-opted by the dominant power) is structurally excluded from the conversation about the territory's future. The displaced population has no seat at any table. International voices proposing non-zero-sum frameworks (confederation, shared sovereignty, internationalized regimes) are excluded because the existential framing treats them as category errors.
% DISAPPEARANCE_RATIONALE: If the existential matrix constraint vanished overnight, the territorial arrangement would reorganize: the dominant power would face genuine existential insecurity without the occupation architecture; the minority population would demand immediate sovereignty; the international legal order would impose a settlement based on self-determination and borders. The world rearranges because the constraint IS the current territorial order — its disappearance is the conflict's resolution or transformation.
% FOUNDING_PROBLEM: The existential survival of a persecuted people requiring territorial control as the only reliable guarantee against genocide and statelessness, following the Holocaust and centuries of pogroms and expulsions.
% FOUNDING_PROBLEM_CORROBORATION: The dominant power attests the founding problem remains live, citing ongoing antisemitism and regional hostility. Historians and international lawyers outside the benefiting parties attest the founding problem (statelessness, genocide vulnerability) was substantially solved by 1948 statehood and international recognition, and the current arrangement persists as demographic dominance and ideological expansion rather than existential survival. The displaced population and minority national group attest the founding problem has been displaced onto them — they now face the existential vulnerability the arrangement was built to prevent.
narrative_ontology:disappearance_verdict(territorial_sovereignty_legitimacy__existential_matrix_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_sovereignty_legitimacy__existential_matrix_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__existential_matrix_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__existential_matrix_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.88, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_sovereignty_legitimacy__existential_matrix_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__existential_matrix_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_sovereignty_legitimacy__existential_matrix_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.88) is very high because the existential framing treats the entire territorial domain as a winner-take-all survival asset — the dominant power extracts total control, the minority extracts nothing. Suppression (0.92) is near-maximum because the zero-sum logic requires active prevention of any territorial compromise that would create vulnerability; the enforcement machinery (military occupation, settlement expansion, movement restrictions, legal architecture) is the constraint's operating substrate. Theater ratio (0.25) is moderate-low: the existential security function is real (the dominant population genuinely experiences existential fear), but a growing share of enforcement activity serves demographic engineering rather than immediate security. The measurement series shows extractiveness and suppression rising over 60 time units as the existential framing hardens into irreversible facts on the ground, while theater slowly increases as juridical justifications accumulate over the raw existential drive. Accessibility collapse (0.78) is high but not total — alternatives (confederation, shared sovereignty, internationalized regimes) exist conceptually but collapse when the existential frame is activated. Resistance (0.71) is high — the minority population and international actors continuously contest the arrangement through diplomatic, legal, and armed resistance.
 *
 * PERSPECTIVAL GAP:
 *   From the dominant_military_power's seat, the constraint appears as necessary coordination for survival — a genuine rope-like arrangement securing existential needs. From the minority_national_group's seat, the same structure is pure extraction enforced by overwhelming power — a snare. The demographic_majority_group experiences a hybrid: coordination benefit (identity expression) with extraction cost (permanent militarization, demographic anxiety). The engine computes this seat divergence from the structural data; the authored claim (tangled_rope) captures the coordination/extraction hybrid at the system level.
 *
 * DIRECTIONALITY LOGIC:
 *   The dominant_military_power is the structural beneficiary: it controls the territory, writes the legal architecture, and extracts existential security (d near 0.0 — full beneficiary). The demographic_majority_group is a secondary beneficiary: it gains collective identity expression and demographic security through the same structure (d ~ 0.2). The minority_national_group and displaced_population are full targets: they bear the existential vulnerability, have no territorial control, and face trapped exit options (d near 1.0). Civilian_populations_under_occupation are constrained targets: they experience daily extraction of autonomy but retain some agency (d ~ 0.7). The international_legal_order sits at the analytical seat (d = 0.5) — it neither benefits nor pays but provides the juridical vocabulary the reading treats as epiphenomenal.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (existential survival of a persecuted people requiring territorial control) was live at the reading's inception but has mutated: the dominant power now enjoys existential security while the constraint persists and intensifies. The arrangement no longer solves the founding problem for the original beneficiaries — it now serves demographic dominance and ideological expansion. The mandatrophy is unresolved: the constraint's mandate has outlived its function for the original beneficiary but has been captured by a new beneficiary set. This prevents mislabeling the current arrangement as pure coordination (it is not) or pure extraction (it once was coordination).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    existential_vs_juridical_primacy,
    'Is the existential driver genuinely primary, or does the existential framing serve as a legitimating cover for a juridical claim that cannot be openly defended?',
    'Counterfactual analysis: if existential security were achieved without juridical recognition, would the claimant accept it? If not, juridical claim is primary.',
    'If juridical claim is primary, the constraint''s ε drops (coordination function rises); if existential driver is primary, high ε and tangled_rope classification are confirmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(existential_vs_juridical_primacy, conceptual, 'Whether existential framing is the real driver or a cover story').

omega_variable(
    zero_sum_structural_inevitability,
    'Is the zero-sum structure an invariant of the conflict, or a product of the existential framing itself?',
    'Historical comparison: identify cases where existential territorial conflicts were resolved through non-zero-sum frameworks (confederation, shared sovereignty, internationalized regimes).',
    'If zero-sum is framing-dependent, the constraint''s claimed_type shifts toward scaffold or rope; if structural, tangled_rope/snare holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(zero_sum_structural_inevitability, empirical, 'Whether the zero-sum character is structural or constructed').

omega_variable(
    kernel_reading_committer_structure,
    'How does this reading''s existential framing structurally relate to the covenant_continuity_reading and self_determination_reading within the contested kernel of territorial sovereignty legitimacy?',
    'Comparative structural analysis of all three readings'' beneficiary/victim sets, exit options, and claimed coordination functions across the same territorial domain.',
    'If readings foreclose each other, the kernel is a genuine commitment system with irreconcilable frames; if they coexist, the conflict is a contest of interpretations over a shared kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Commitment system structure: this reading''s relationship to sibling readings within the territorial_sovereignty_legitimacy kernel').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (military occupation, legal barriers, economic blockade) or internalized (populations accepting vulnerability as inevitable, identity fused with the conflict)?',
    'Post-ceasefire suppression trajectory analysis: if suppression persists after military enforcement is removed, reclassify as partially internalized.',
    'If internalized, effective suppression is higher than structural measure suggests — the constraint''s extraction survives formal exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in existential territorial conflict').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__existential_matrix_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(territorial_sovereignty_legitimacy__existential_matrix_reading_tr_t0, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(territorial_sovereignty_legitimacy__existential_matrix_reading_tr_t0, observed).
narrative_ontology:measurement(territorial_sovereignty_legitimacy__existential_matrix_reading_tr_t15, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement_basis(territorial_sovereignty_legitimacy__existential_matrix_reading_tr_t15, observed).
narrative_ontology:measurement(territorial_sovereignty_legitimacy__existential_matrix_reading_tr_t30, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement_basis(territorial_sovereignty_legitimacy__existential_matrix_reading_tr_t30, observed).
narrative_ontology:measurement(territorial_sovereignty_legitimacy__existential_matrix_reading_tr_t45, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 45, 0.24).
narrative_ontology:measurement_basis(territorial_sovereignty_legitimacy__existential_matrix_reading_tr_t45, observed).
narrative_ontology:measurement(territorial_sovereignty_legitimacy__existential_matrix_reading_tr_t60, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 60, 0.25).
narrative_ontology:measurement_basis(territorial_sovereignty_legitimacy__existential_matrix_reading_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(territorial_sovereignty_legitimacy__existential_matrix_reading_be_t0, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement_basis(territorial_sovereignty_legitimacy__existential_matrix_reading_be_t0, observed).
narrative_ontology:measurement(territorial_sovereignty_legitimacy__existential_matrix_reading_be_t15, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 15, 0.79).
narrative_ontology:measurement_basis(territorial_sovereignty_legitimacy__existential_matrix_reading_be_t15, observed).
narrative_ontology:measurement(territorial_sovereignty_legitimacy__existential_matrix_reading_be_t30, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 30, 0.84).
narrative_ontology:measurement_basis(territorial_sovereignty_legitimacy__existential_matrix_reading_be_t30, observed).
narrative_ontology:measurement(territorial_sovereignty_legitimacy__existential_matrix_reading_be_t45, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 45, 0.87).
narrative_ontology:measurement_basis(territorial_sovereignty_legitimacy__existential_matrix_reading_be_t45, observed).
narrative_ontology:measurement(territorial_sovereignty_legitimacy__existential_matrix_reading_be_t60, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 60, 0.88).
narrative_ontology:measurement_basis(territorial_sovereignty_legitimacy__existential_matrix_reading_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(territorial_sovereignty_legitimacy__existential_matrix_reading_su_t0, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 0, 0.78).
narrative_ontology:measurement_basis(territorial_sovereignty_legitimacy__existential_matrix_reading_su_t0, observed).
narrative_ontology:measurement(territorial_sovereignty_legitimacy__existential_matrix_reading_su_t15, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 15, 0.83).
narrative_ontology:measurement_basis(territorial_sovereignty_legitimacy__existential_matrix_reading_su_t15, observed).
narrative_ontology:measurement(territorial_sovereignty_legitimacy__existential_matrix_reading_su_t30, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 30, 0.87).
narrative_ontology:measurement_basis(territorial_sovereignty_legitimacy__existential_matrix_reading_su_t30, observed).
narrative_ontology:measurement(territorial_sovereignty_legitimacy__existential_matrix_reading_su_t45, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 45, 0.9).
narrative_ontology:measurement_basis(territorial_sovereignty_legitimacy__existential_matrix_reading_su_t45, observed).
narrative_ontology:measurement(territorial_sovereignty_legitimacy__existential_matrix_reading_su_t60, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 60, 0.92).
narrative_ontology:measurement_basis(territorial_sovereignty_legitimacy__existential_matrix_reading_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__existential_matrix_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.08).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__existential_matrix_reading, territorial_sovereignty_legitimacy__covenant_continuity_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__existential_matrix_reading, territorial_sovereignty_legitimacy__self_determination_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__existential_matrix_reading, settlement_expansion_enforcement).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__existential_matrix_reading, movement_restriction_regime).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__existential_matrix_reading, demographic_engineering_policy).

% DUAL FORMULATION NOTE:
% The territorial_sovereignty_legitimacy kernel decomposes into three constraint stories with distinct ε values and beneficiary/victim structures. This reading (existential_matrix) has ε=0.88 and identifies dominant_military_power as primary beneficiary. The covenant_continuity_reading would have lower ε (juridical coordination function) and identify the covenant community as beneficiary. The self_determination_reading would have different victim set (Arab population under occupation) and different coordination claim. All three are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_sovereignty_legitimacy__existential_matrix_reading, institutional, 0.05).
constraint_indexing:directionality_override(territorial_sovereignty_legitimacy__existential_matrix_reading, powerless, 0.95).
constraint_indexing:directionality_override(territorial_sovereignty_legitimacy__existential_matrix_reading, moderate, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
