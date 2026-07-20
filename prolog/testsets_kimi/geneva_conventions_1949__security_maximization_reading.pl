% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__security_maximization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_1949__security_maximization_reading, []).

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
 *   constraint_id: geneva_conventions_1949__security_maximization_reading
 *   human_readable: Geneva Conventions 1949 â Security Maximization Reading
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint story models the Geneva Conventions of 1949 under the
 *   security-maximization reading: the interpretive framework that treats
 *   international humanitarian law as peacetime aspirations subordinate to
 *   operational necessity, particularly in asymmetric conflicts against
 *   irregular forces. Under this reading, the conventions minimally constrain
 *   state violence; categories like 'unlawful combatant' are expanded to deny
 *   POW status and habeas corpus, civilian immunity is degraded through
 *   'human shields' doctrines and elevated collateral damage acceptance, and
 *   coercive interrogation is normalized as non-torture. This is one reading
 *   of a contested kernel; sibling readings instantiate structurally distinct
 *   constraints and are linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - security_maximizing_states: Agenda-setter (institutional/global/arbitrage) â interprets and enforces the reading, benefits from operational latitude and intelligence extraction.
 *   - detainees_unlawful_combatants: Primary target (powerless/national/trapped) â denied Geneva protections, habeas corpus, and due process; bear direct extraction.
 *   - civilians_in_conflict_zones: Secondary target (powerless/regional/trapped) â immunity degraded by targeting doctrines that shift risk to them.
 *   - irregular_combatants: Secondary target (powerless/regional/trapped) â denied combatant privileges and exposed to indefinite detention or targeted killing.
 *   - humanitarian_organizations: Excluded voice (organized/global/constrained) â structurally marginalized from policy and access.
 *   - international_courts_and_tribunals: Observer (institutional/global/constrained) â issues rulings but lacks enforcement cooperation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__security_maximization_reading, 0.82).
domain_priors:suppression_score(geneva_conventions_1949__security_maximization_reading, 0.78).
domain_priors:theater_ratio(geneva_conventions_1949__security_maximization_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__security_maximization_reading, snare).
narrative_ontology:human_readable(geneva_conventions_1949__security_maximization_reading, "Geneva Conventions 1949 â Security Maximization Reading").
narrative_ontology:topic_domain(geneva_conventions_1949__security_maximization_reading, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(geneva_conventions_1949__security_maximization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__security_maximization_reading, '074f4c9d-1aa8-4ff5-966b-d65ddb06e521').
narrative_ontology:cs_kernel_codification('074f4c9d-1aa8-4ff5-966b-d65ddb06e521', fixed_text).
narrative_ontology:cs_authority_grounding('074f4c9d-1aa8-4ff5-966b-d65ddb06e521', lineage).
narrative_ontology:cs_interpretation_layer_present('074f4c9d-1aa8-4ff5-966b-d65ddb06e521').
narrative_ontology:cs_reading_relation('074f4c9d-1aa8-4ff5-966b-d65ddb06e521', geneva_conventions_1949__humanitarian_ceiling_reading, forecloses).
narrative_ontology:cs_reading_relation('074f4c9d-1aa8-4ff5-966b-d65ddb06e521', geneva_conventions_1949__conditional_reciprocity_reading, influences).
narrative_ontology:cs_axiom('074f4c9d-1aa8-4ff5-966b-d65ddb06e521', foundational, operational_necessity_overrides_humanitarian_restraint).
narrative_ontology:cs_axiom_status(operational_necessity_overrides_humanitarian_restraint, holdable).
narrative_ontology:cs_axiom_grounding('074f4c9d-1aa8-4ff5-966b-d65ddb06e521', operational_necessity_overrides_humanitarian_restraint, instrumental).
narrative_ontology:cs_axiom('074f4c9d-1aa8-4ff5-966b-d65ddb06e521', foundational, unlawful_combatant_exclusion_from_privileged_status).
narrative_ontology:cs_axiom_status(unlawful_combatant_exclusion_from_privileged_status, holdable).
narrative_ontology:cs_axiom_grounding('074f4c9d-1aa8-4ff5-966b-d65ddb06e521', unlawful_combatant_exclusion_from_privileged_status, conventional).
narrative_ontology:cs_reference_frame('074f4c9d-1aa8-4ff5-966b-d65ddb06e521', military_necessity_supremacy).
narrative_ontology:cs_drift_state('074f4c9d-1aa8-4ff5-966b-d65ddb06e521', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('074f4c9d-1aa8-4ff5-966b-d65ddb06e521', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__security_maximization_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__security_maximization_reading, security_maximizing_states).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, detainees_unlawful_combatants).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, civilians_in_conflict_zones).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, irregular_combatants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adopts and enforces an interpretive framework that subordinates Geneva Convention protections to operational necessity in asymmetric conflict. Expands the unlawful-combatant category, degrades civilian immunity via human-shields doctrines, and normalizes coercive interrogation and indefinite detention without trial. Benefits from unchecked operational latitude and intelligence extraction while maintaining formal adherence to the convention text.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, security_maximizing_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Held incommunicado in national or offshore detention facilities, denied POW status, habeas corpus, and due process. Subjected to indefinite detention and coercive interrogation justified by the security-maximization reading. No viable legal or physical exit from the extraction.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, detainees_unlawful_combatants, payer,
    powerless, biographical, trapped, national).

% Civilian immunity degraded by targeting doctrines that accept elevated collateral damage and shift legal risk to civilians through human-shields allegations. Cannot exit the conflict zone due to resource constraints, border closures, or active hostilities.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, civilians_in_conflict_zones, payer,
    powerless, immediate, trapped, regional).

% Denied lawful-combatant privileges under expanded unlawful-combatant categories. Exposed to indefinite detention, targeted killing, and interrogation without the protections reserved for lawful combatants. Trapped by the legal status assigned to them under this reading.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, irregular_combatants, payer,
    powerless, biographical, trapped, regional).

% Humanitarian and human-rights organizations such as the ICRC seek access to detainees and conflict zones to monitor compliance. Their access is conditional, delayed, or denied by states invoking operational necessity and classification, structurally excluding them from meaningful oversight.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, humanitarian_organizations, excluded,
    organized, generational, constrained, global).

% Issue rulings and legal opinions asserting that humanitarian minimums apply regardless of conflict type, but face state non-cooperation, jurisdictional challenges, and lack of enforcement mechanisms. Their exit is constrained by the structural power imbalance between international adjudication and state sovereignty.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, international_courts_and_tribunals, observer,
    institutional, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_1949__security_maximization_reading, security_maximizing_states).
narrative_ontology:fixing_cost_class(geneva_conventions_1949__security_maximization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Nominally coordinates interstate and non-state conduct in armed conflict by establishing rules that distinguish combatants from civilians and set minimum protections for the wounded, shipwrecked, prisoners of war, and civilians.
% TRANSFER_FUNCTION: Moves legal protections, due-process guarantees, and bodily integrity from detainees, civilians, and irregular combatants to security-maximizing states, converting humanitarian-law categories into instruments of strategic advantage and intelligence extraction.
% ABSENT_VOICES: Detainees are held incommunicado without counsel or habeas review. Humanitarian organizations are granted conditional or denied access. International criminal courts are blocked by state non-cooperation and jurisdictional claims.
% DISAPPEARANCE_RATIONALE: If this interpretive constraint vanished, states would lose the legal architecture sustaining indefinite detention, coercive interrogation, and degraded civilian immunity. Detention regimes, targeting protocols, and interrogation rules would have to reorganize around stricter humanitarian ceilings or reciprocal frameworks.
% FOUNDING_PROBLEM: The conventions were built to regularize interstate armed conflict and limit unnecessary suffering. Under this reading, the arrangement addresses asymmetric conflict where non-state adversaries do not reciprocate compliance, creating a perceived need to suspend protections to maximize state security.
% FOUNDING_PROBLEM_CORROBORATION: No independent corroboration exists. Security-maximizing states assert the problem requires this solution. The ICRC, UN special rapporteurs, international human rights courts, and independent legal scholars contest that asymmetric conflict justifies suspending humanitarian minimums, pointing to the text's non-derogable articles and state obligations under customary law.
narrative_ontology:disappearance_verdict(geneva_conventions_1949__security_maximization_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_1949__security_maximization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__security_maximization_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(geneva_conventions_1949__security_maximization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_1949__security_maximization_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__security_maximization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_1949__security_maximization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_1949__security_maximization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the reading systematically strips legal protections from defined categories of persons and transfers operational latitude to states. Suppression is high (0.78) because the constraint depends on state secrecy, classification, territorial exclusion of courts, and non-cooperation with international tribunals to persist. Theater_ratio is moderate-high (0.55) because the state maintains elaborate legal performanceâOLC memos, military commissions, and status-review tribunalsâthat creates a veneer of process while the underlying function is extraction. Accessibility_collapse is substantial (0.68) because once the reading is accepted in a given jurisdiction, legal alternatives for victims largely close off; resistance remains moderate (0.60) because international courts and domestic litigation continue to challenge the reading, though with limited enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the state agenda-setter seat, the constraint is a necessary adaptation of law to asymmetric threats; from the detainee and civilian payer seats, the same structure operates as legally rationalized violence and dispossession. The engine computes this divergence from beneficiary/victim declarations and exit asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Security_maximizing_states are declared beneficiaries with arbitrage-grade exit (can adopt or abandon the reading), yielding low directionality. Detainees, civilians, and irregular combatants are declared victims with trapped exit, yielding high directionality. Humanitarian organizations and international courts sit in excluded and observer roles with constrained exit, experiencing high directionality without being the primary extraction targets.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâregularizing interstate armed conflictâhas either deadened or transformed. The security-maximization reading claims to solve asymmetric conflict, but this claim is contested and self-asserted by the benefiting parties. The persistence of the arrangement after its original problem shifted indicates mandatrophy risk; the claimed coordination (humanitarian protection) has atrophied into a cover story for security extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_naturalness,
    'Is the security-maximization reading an inherent feature of the Geneva Conventions text, or a constructed extraction using the text as cover?',
    'Historical-textual analysis of the travaux prÃ©paratoires and subsequent state practice against the security-maximization claims.',
    'If constructed, the constraint is a snare using legal lineage as cover; if inherent, it is closer to a tangled rope with genuine textual coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_naturalness, conceptual, 'Whether the reading is textually inherent or constructed extraction').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (state secrecy, classification, territorial exclusion of courts) or internalized (global acceptance of the security narrative)?',
    'Track legal-challenge success rates and public-opinion trajectories on torture and detention across jurisdictions.',
    'If internalized, the constraint''s effective suppression exceeds the structural measure and the target population carries the suppression with them even after physical exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').

omega_variable(
    theater_vs_genuine_adjudication,
    'Do the military commissions and OLC memos retain genuine adjudicative function, or are they primarily performative legitimization?',
    'Empirical study of acquittal rates, procedural-fairness indicators, and subsequent legal reversals.',
    'If purely theater, theater_ratio should be higher and the constraint approaches pure snare; if genuine, some residual coordination function persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_vs_genuine_adjudication, empirical, 'Whether legal processes are performative or functional').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__security_maximization_reading, 0, 23).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gva_secmax_tr_t0, geneva_conventions_1949__security_maximization_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(gva_secmax_tr_t5, geneva_conventions_1949__security_maximization_reading, theater_ratio, 5, 0.5).
narrative_ontology:measurement(gva_secmax_tr_t10, geneva_conventions_1949__security_maximization_reading, theater_ratio, 10, 0.55).
narrative_ontology:measurement(gva_secmax_tr_t15, geneva_conventions_1949__security_maximization_reading, theater_ratio, 15, 0.58).
narrative_ontology:measurement(gva_secmax_tr_t20, geneva_conventions_1949__security_maximization_reading, theater_ratio, 20, 0.55).
narrative_ontology:measurement(gva_secmax_tr_t23, geneva_conventions_1949__security_maximization_reading, theater_ratio, 23, 0.55).

% Extraction over time
narrative_ontology:measurement(gva_secmax_be_t0, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(gva_secmax_be_t5, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 5, 0.72).
narrative_ontology:measurement(gva_secmax_be_t10, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 10, 0.8).
narrative_ontology:measurement(gva_secmax_be_t15, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 15, 0.78).
narrative_ontology:measurement(gva_secmax_be_t20, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 20, 0.82).
narrative_ontology:measurement(gva_secmax_be_t23, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 23, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(gva_secmax_su_t0, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(gva_secmax_su_t5, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 5, 0.78).
narrative_ontology:measurement(gva_secmax_su_t10, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 10, 0.8).
narrative_ontology:measurement(gva_secmax_su_t15, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 15, 0.72).
narrative_ontology:measurement(gva_secmax_su_t20, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(gva_secmax_su_t23, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 23, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__security_maximization_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, geneva_conventions_1949__humanitarian_ceiling_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, geneva_conventions_1949__conditional_reciprocity_reading).

% DUAL FORMULATION NOTE:
% The Geneva Conventions 1949 kernel decomposes into three constraint stories because the colloquial label 'Geneva Conventions' covers multiple structurally distinct claims. The security-maximization reading produces high extraction and a snare classification; the humanitarian-ceiling reading would produce negligible extraction; the conditional-reciprocity reading would produce extraction conditional on adversary conduct. They have different empirical status, different victim sets, and different failure modes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
