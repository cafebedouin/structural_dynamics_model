% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__coercion_visibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_marriage_command__coercion_visibility_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: divine_marriage_command__coercion_visibility_reading
 *   human_readable: Divine Marriage Command (Coercion Visibility Reading)
 *   domain: religious/political_theology
 *
 * SUMMARY:
 *   This constraint represents the 'coercion visibility' reading of the
 *   divine marriage command, where the church's Manifesto abandoning polygamy
 *   is explicitly acknowledged as a response to federal coercion, and its
 *   theological legitimacy is derived from the necessity of institutional
 *   survival. This reading closes the M-set gap by admitting exogenous
 *   pressure as a valid input for doctrinal shift, but potentially creates a
 *   legitimacy crisis if divine command is seen as mutable by external force.
 *   The constraint is claimed as a Rope by the institutional church
 *   (coordination for survival) but operates as a Tangled Rope for its
 *   members (coordination for the institution, extraction from individuals).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__coercion_visibility_reading, 0.45).
domain_priors:suppression_score(divine_marriage_command__coercion_visibility_reading, 0.6).
domain_priors:theater_ratio(divine_marriage_command__coercion_visibility_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__coercion_visibility_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__coercion_visibility_reading, "Divine Marriage Command (Coercion Visibility Reading)").
narrative_ontology:topic_domain(divine_marriage_command__coercion_visibility_reading, "religious/political_theology").

domain_priors:requires_active_enforcement(divine_marriage_command__coercion_visibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__coercion_visibility_reading, '8430ff61-8015-47d6-93a1-a70a74d97f5f').
narrative_ontology:cs_kernel_codification('8430ff61-8015-47d6-93a1-a70a74d97f5f', formalized).
narrative_ontology:cs_authority_grounding('8430ff61-8015-47d6-93a1-a70a74d97f5f', lineage).
narrative_ontology:cs_interpretation_layer_present('8430ff61-8015-47d6-93a1-a70a74d97f5f').
narrative_ontology:cs_reading_relation('8430ff61-8015-47d6-93a1-a70a74d97f5f', divine_marriage_command__continuationist_reading, coexists_with).
narrative_ontology:cs_reading_relation('8430ff61-8015-47d6-93a1-a70a74d97f5f', divine_marriage_command__substitutionist_reading, coexists_with).
narrative_ontology:cs_axiom('8430ff61-8015-47d6-93a1-a70a74d97f5f', foundational, institutional_survival_theological_imperative).
narrative_ontology:cs_axiom_status(institutional_survival_theological_imperative, holdable).
narrative_ontology:cs_axiom_grounding('8430ff61-8015-47d6-93a1-a70a74d97f5f', institutional_survival_theological_imperative, instrumental).
narrative_ontology:cs_axiom('8430ff61-8015-47d6-93a1-a70a74d97f5f', foundational, exogenous_pressure_valid_doctrinal_input).
narrative_ontology:cs_axiom_status(exogenous_pressure_valid_doctrinal_input, holdable).
narrative_ontology:cs_axiom_grounding('8430ff61-8015-47d6-93a1-a70a74d97f5f', exogenous_pressure_valid_doctrinal_input, conventional).
narrative_ontology:cs_reference_frame('8430ff61-8015-47d6-93a1-a70a74d97f5f', institutional_survival_under_duress).
narrative_ontology:cs_drift_state('8430ff61-8015-47d6-93a1-a70a74d97f5f', contemporary_theological_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('8430ff61-8015-47d6-93a1-a70a74d97f5f', '').
narrative_ontology:cs_kernel_id(divine_marriage_command__coercion_visibility_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__coercion_visibility_reading, institutional_church).
narrative_ontology:constraint_beneficiary(divine_marriage_command__coercion_visibility_reading, monogamous_members).
narrative_ontology:constraint_victim(divine_marriage_command__coercion_visibility_reading, polygamous_members).
narrative_ontology:constraint_victim(divine_marriage_command__coercion_visibility_reading, dissident_factions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The central authority that issued the Manifesto, acknowledging it as a response to federal coercion. Benefits from institutional survival and legal recognition, but bears the cost of doctrinal ambiguity and internal dissent. Enforces the shift to monogamy through ecclesiastical courts and social pressure.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, institutional_church, agenda_setter,
    institutional, generational, constrained, global).

% Members who had entered into or believed in polygamous marriages prior to the Manifesto. They face social ostracization, excommunication, and legal penalties if they continue the practice. Their identity is deeply tied to the prior doctrine, making exit from the church extremely costly.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, polygamous_members, payer,
    powerless, biographical, identity_locked, local).

% Members who either already practiced monogamy or readily adopted it. They benefit from the church's legal recognition and social acceptance, and face fewer internal conflicts regarding the doctrinal shift. Their adherence reinforces the new norm.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, monogamous_members, beneficiary,
    moderate, biographical, mobile, local).

% Exerted legal and coercive pressure (disenfranchisement, property confiscation, imprisonment) to compel the church to abandon polygamy. Benefits from enforcing its legal norms and asserting its sovereignty over religious practice. Its pressure is the acknowledged exogenous force driving the Manifesto.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Groups that rejected the Manifesto and continued to practice polygamy, often forming splinter communities. They are excluded from the mainstream church and face ongoing legal persecution, but maintain their interpretation of divine command.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, dissident_factions, excluded,
    powerless, generational, trapped, regional).

% Academics who analyze the historical context and theological implications of the Manifesto, including the role of coercion in doctrinal development. They provide external corroboration and alternative interpretations of the event.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, theologians_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the church's legal status and social integration within the broader federal system by aligning its marriage practices with civil law, preventing institutional dissolution.
% TRANSFER_FUNCTION: Transfers the right to practice polygamy from individual members to the institutional imperative of survival, shifting the burden of compliance onto adherents while securing the church's legal existence.
% ABSENT_VOICES: The voices of those who believed polygamy was a divine command and were forced to abandon it, or who left the church to continue it, are largely absent from the official narrative, which emphasizes institutional unity and divine guidance. Dissident factions continue to articulate these views outside the mainstream.
% DISAPPEARANCE_RATIONALE: If the Manifesto and its enforcement vanished, the institutional church would face an immediate crisis of legitimacy regarding its historical actions and current doctrine. Many members would question the basis of monogamy, and dissident polygamous groups might seek reintegration or greater recognition, fundamentally altering the church's structure and social contract.
% FOUNDING_PROBLEM: The institutional church faced existential threat from the federal government due to its practice of polygamy, including legal dissolution, confiscation of assets, and imprisonment of leaders.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (federal persecution of polygamy) is widely acknowledged by both church historians and external academics as having been resolved by the Manifesto. The church's current legal status and social acceptance corroborate that the existential threat has passed. However, the theological implications of that resolution remain contested internally.
narrative_ontology:disappearance_verdict(divine_marriage_command__coercion_visibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_marriage_command__coercion_visibility_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__coercion_visibility_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(divine_marriage_command__coercion_visibility_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_marriage_command__coercion_visibility_reading_tests).
:- end_tests(divine_marriage_command__coercion_visibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate: while individuals lost the right to practice polygamy, the institution survived. Suppression (0.6) is substantial, reflecting the internal and external pressures to conform. Theater ratio (0.2) is low, as the shift was a genuine, albeit coerced, change in practice, not merely performance. The initial rise in extractiveness and suppression reflects the period of active enforcement and internal realignment following the Manifesto.
 *
 * PERSPECTIVAL GAP:
 *   The institutional church (agenda_setter) experiences this as a necessary coordination for survival, a 'Rope' that saved the institution. Polygamous members (payer) experience it as a 'Snare' or 'Tangled Rope' that extracted their religious practice and identity under duress. Monogamous members (beneficiary) experience it as a 'Rope' that brought stability and social acceptance. The federal government (agenda_setter) views it as successful enforcement of civil law.
 *
 * DIRECTIONALITY LOGIC:
 *   The institutional church is a beneficiary (d=0.15) as it achieved survival and legal recognition. Polygamous members are targets (d=0.9) due to loss of practice and identity-locked exit. Monogamous members are beneficiaries (d=0.3) due to stability. The federal government is a beneficiary (d=0.05) as its laws were enforced. Dissident factions are targets (d=0.95) due to complete exclusion and persecution.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (institutional survival) was achieved, but the theological justification for the means (doctrinal shift under coercion) remains contested. This reading acknowledges the coercion, preventing a false 'Mountain' claim of purely divine revelation, but opens the question of whether a coerced doctrinal shift can retain full theological authority. The 'dead' status of the founding problem combined with 'world_rearranges' verdict signals a potential zombie constraint, where the original problem is gone but the structure persists, now extracting from those who question its legitimacy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coercion_as_revelation,
    'Can external coercion be a legitimate mechanism for divine revelation or doctrinal change within this theological framework?',
    'Further theological development or a new authoritative statement from the institutional church clarifying the nature of revelation in times of duress.',
    'If coercion is deemed a valid input, the legitimacy crisis is mitigated, and the constraint leans more towards a ''Rope'' for the institution. If not, the constraint''s theological foundation is weakened, increasing its ''Snare'' characteristics for those who lost their practice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coercion_as_revelation, conceptual, 'Theological implications of coercion-driven doctrinal shift.').

omega_variable(
    legitimacy_of_survival_theology,
    'Is institutional survival a sufficient theological justification for altering a divine command?',
    'Internal theological debate and consensus building within the church, or a formal re-evaluation of the hierarchy of theological values.',
    'If survival is a paramount theological value, the constraint''s ''Rope'' aspect is strengthened. If not, the ''Tangled Rope'' aspect (extraction from individuals for institutional benefit) becomes more pronounced, potentially leading to a ''Snare'' classification for those who prioritize individual divine commands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_survival_theology, preference, 'Theological weight of institutional survival versus divine command.').

omega_variable(
    m_set_gap_closure_validity,
    'Does acknowledging exogenous pressure as a valid input for doctrinal shift genuinely close the M-set gap, or does it merely shift the locus of contestation?',
    'Analysis of subsequent doctrinal developments and internal dissent: if new doctrinal shifts are consistently attributed to external pressures without internal theological justification, the gap may have merely shifted.',
    'If the gap is genuinely closed, the constraint''s internal coherence is higher. If it merely shifted, the constraint remains structurally unstable, prone to further legitimacy crises, and more extractive for those who bear the cost of this instability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(m_set_gap_closure_validity, empirical, 'Effectiveness of acknowledging coercion in resolving doctrinal ambiguity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__coercion_visibility_reading, 1890, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t1890, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1890, 0.1).
narrative_ontology:measurement(divi_tr_t1900, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1900, 0.15).
narrative_ontology:measurement(divi_tr_t1910, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1910, 0.2).
narrative_ontology:measurement(divi_tr_t1920, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1920, 0.2).
narrative_ontology:measurement(divi_tr_t1930, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1930, 0.2).
narrative_ontology:measurement(divi_tr_t1940, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1940, 0.2).
narrative_ontology:measurement(divi_tr_t1950, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1950, 0.2).

% Extraction over time
narrative_ontology:measurement(divi_be_t1890, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1890, 0.3).
narrative_ontology:measurement(divi_be_t1900, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1900, 0.4).
narrative_ontology:measurement(divi_be_t1910, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1910, 0.45).
narrative_ontology:measurement(divi_be_t1920, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1920, 0.45).
narrative_ontology:measurement(divi_be_t1930, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1930, 0.45).
narrative_ontology:measurement(divi_be_t1940, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1940, 0.45).
narrative_ontology:measurement(divi_be_t1950, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1950, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t1890, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1890, 0.5).
narrative_ontology:measurement(divi_su_t1900, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1900, 0.6).
narrative_ontology:measurement(divi_su_t1910, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1910, 0.6).
narrative_ontology:measurement(divi_su_t1920, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1920, 0.6).
narrative_ontology:measurement(divi_su_t1930, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1930, 0.6).
narrative_ontology:measurement(divi_su_t1940, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1940, 0.6).
narrative_ontology:measurement(divi_su_t1950, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1950, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__coercion_visibility_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'divine_marriage_command' kernel, alongside 'continuationist_reading' and 'substitutionist_reading'. Each reading offers a distinct interpretation of the Manifesto's theological and historical significance, leading to different structural properties and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
