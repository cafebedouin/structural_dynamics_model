% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_commitment__incoherence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_commitment__incoherence_reading, []).

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
 *   constraint_id: shinbutsu_ontological_commitment__incoherence_reading
 *   human_readable: Shinbutsu-shugo as Institutionally Tolerated Ontological Incoherence
 *   domain: religious_studies/japanese_history
 *
 * SUMMARY:
 *   This constraint story instantiates the incoherence reading of the
 *   shinbutsu-shugo kernel: the claim that Edo-period Japanese religious
 *   institutions operated under no stable ontological commitment, and that
 *   this incoherence was institutionally tolerated because it served
 *   political coordination and economic functions. The arrangement
 *   coordinated a mixed shrine-temple ritual economy while extracting
 *   doctrinal clarity from purists and setting the stage for the Meiji state
 *   to exploit the ease of separation. The story covers the interval from the
 *   consolidation of the Tokugawa religious order (c. 1600) through the Meiji
 *   Restoration (1868), tracing the slow accretion of theatrical performance
 *   and the eventual collapse of syncretic institutions when the constraint
 *   was overturned.
 *
 * KEY AGENTS:
 *   - tokugawa_state: Agenda-setter (institutional/analytical) â tolerated and managed incoherence for political stability.
 *   - syncretic_religious_institutions: Beneficiary (organized/constrained) â collected patronage within the blurred ritual economy.
 *   - kokugaku_scholars: Payer (moderate/constrained) â bore the cost of marginalized doctrinal clarity.
 *   - common_parishioners: Payer (powerless/trapped) â enrolled in a syncretic parish system with no doctrinal alternative.
 *   - meiji_state_builders: Downstream beneficiary (institutional/analytical) â exploited the constraint's instability to build State Shinto.
 *   - doctrinal_buddhist_reformers: Excluded (moderate/constrained) â absent voice demanding Buddhist orthodoxy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_commitment__incoherence_reading, 0.85).
domain_priors:suppression_score(shinbutsu_ontological_commitment__incoherence_reading, 0.3).
domain_priors:theater_ratio(shinbutsu_ontological_commitment__incoherence_reading, 0.8).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 0.8).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_commitment__incoherence_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_ontological_commitment__incoherence_reading, "Shinbutsu-shugo as Institutionally Tolerated Ontological Incoherence").
narrative_ontology:topic_domain(shinbutsu_ontological_commitment__incoherence_reading, "religious_studies/japanese_history").

domain_priors:requires_active_enforcement(shinbutsu_ontological_commitment__incoherence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_commitment__incoherence_reading, 'c5d1975a-fac2-4129-88c8-5950e341c609').
narrative_ontology:cs_kernel_codification('c5d1975a-fac2-4129-88c8-5950e341c609', implicit).
narrative_ontology:cs_authority_grounding('c5d1975a-fac2-4129-88c8-5950e341c609', practice).
narrative_ontology:cs_interpretation_layer_present('c5d1975a-fac2-4129-88c8-5950e341c609').
narrative_ontology:cs_reading_relation('c5d1975a-fac2-4129-88c8-5950e341c609', shinbutsu_ontological_commitment__syncretic_reading, forecloses).
narrative_ontology:cs_reading_relation('c5d1975a-fac2-4129-88c8-5950e341c609', shinbutsu_ontological_commitment__partition_reading, coexists_with).
narrative_ontology:cs_axiom('c5d1975a-fac2-4129-88c8-5950e341c609', foundational, no_stable_ontological_commitment).
narrative_ontology:cs_axiom_status(no_stable_ontological_commitment, holdable).
narrative_ontology:cs_axiom_grounding('c5d1975a-fac2-4129-88c8-5950e341c609', no_stable_ontological_commitment, empirically_contingent).
narrative_ontology:cs_axiom('c5d1975a-fac2-4129-88c8-5950e341c609', foundational, institutional_tolerance_over_doctrinal_clarity).
narrative_ontology:cs_axiom_status(institutional_tolerance_over_doctrinal_clarity, holdable).
narrative_ontology:cs_axiom_grounding('c5d1975a-fac2-4129-88c8-5950e341c609', institutional_tolerance_over_doctrinal_clarity, conventional).
narrative_ontology:cs_reference_frame('c5d1975a-fac2-4129-88c8-5950e341c609', syncretic_practice_imperative).
narrative_ontology:cs_drift_state('c5d1975a-fac2-4129-88c8-5950e341c609', meiji_restoration, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c5d1975a-fac2-4129-88c8-5950e341c609', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_commitment__incoherence_reading, shinbutsu_ontological_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, tokugawa_state).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, syncretic_religious_institutions).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, meiji_state_builders).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__incoherence_reading, kokugaku_scholars).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__incoherence_reading, common_parishioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, common_parishioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administered the temple registration system and religious regulation; tolerated doctrinal incoherence to prevent sectarian mobilization against political authority; could have enforced doctrinal clarity but chose managerial convenience and political stability.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, tokugawa_state, agenda_setter,
    institutional, generational, analytical, national).

% Operated combined shrine-temple complexes and performed mixed rituals for both kami and buddhas; collected parishioner fees and state patronage; benefited from the absence of doctrinal policing that would have forced a choice between Buddhist and Shinto identity.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, syncretic_religious_institutions, beneficiary,
    organized, biographical, constrained, regional).

% Advocated for the purification of Shinto from Buddhist accretions; were marginalized and lacked institutional support under the Tokugawa order; their alternative vision of Japanese religion was structurally excluded from official discourse.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, kokugaku_scholars, payer,
    moderate, generational, constrained, national).

% Registered with Buddhist parishes and participated in mixed shrine-temple rituals; lacked access to doctrinally pure alternatives because the local religious economy was organized around syncretic institutions; bore the cost of ontological confusion and later the disruption of the Meiji separation.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, common_parishioners, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__incoherence_reading, common_parishioners, beneficiary).

% Inherited the Tokugawa religious landscape and exploited its ontological incoherence to effect a rapid separation of Shinto and Buddhism; converted the prior institutional blur into State Shinto as a sharply defined national ideology.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, meiji_state_builders, beneficiary,
    institutional, generational, analytical, national).

% Sought to restore Buddhist sectarian orthodoxy and resist the blending of buddhas with local kami; were subordinated to the parish registration system which prioritized population control over doctrinal purity; their voices were absent from the dominant discourse on religious practice.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, doctrinal_buddhist_reformers, excluded,
    moderate, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_commitment__incoherence_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinated the coexistence of Shinto and Buddhist institutions in a shared ritual economy without requiring doctrinal resolution, solving the collective-action problem of potential sectarian conflict under the Tokugawa political order.
% TRANSFER_FUNCTION: Moved ritual legitimacy and economic patronage across shrine-temple complexes; moved the costs of ontological confusion and later institutional collapse onto purists, parishioners, and syncretic institutions themselves.
% ABSENT_VOICES: Kokugaku nativists and Buddhist orthodox reformers who demanded doctrinal clarity were marginalized; Meiji-era nationalist historians who would retrospectively impose a separation narrative were not yet present.
% DISAPPEARANCE_RATIONALE: Removing the toleration of incoherence would have forced a reorganization of the parish system, shrine-temple complexes, and state-religion relations; the Meiji separation demonstrates that the world rearranged when the constraint was finally overturned.
% FOUNDING_PROBLEM: Managing two major religious traditions in a single polity without sectarian conflict or challenge to centralized political authority.
% FOUNDING_PROBLEM_CORROBORATION: Tokugawa legal codes and temple registration policies attest to the state's prioritization of political order over theological clarity; modern historians such as Kuroda Toshio and Herman Ooms, writing from outside the Edo religious establishment, corroborate that the arrangement served political coordination rather than doctrinal integration.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_commitment__incoherence_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_commitment__incoherence_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_commitment__incoherence_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(shinbutsu_ontological_commitment__incoherence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_commitment__incoherence_reading, 0.85, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_commitment__incoherence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_ontological_commitment__incoherence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_ontological_commitment__incoherence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.30 to 0.85 because the constraint evolved from a pragmatic coordination solution into an increasingly hollow performance that ultimately transferred all institutional value to the Meiji separation. Theater_ratio tracks this hollowing: early honji-suijaku metaphysics gave way to Edo-period formulaic practice. Suppression rises then falls: dissent was increasingly marginalized through the Edo period, but the constraint's own enforcement capacity collapsed with the Tokugawa state in 1868. Accessibility_collapse (0.65) reflects that pure Shinto or pure Buddhism were practically unavailable to ordinary parishioners. Resistance (0.40) captures kokugaku and reformist challenge that remained sub-threshold until the political rupture.
 *
 * PERSPECTIVAL GAP:
 *   From the Tokugawa state's seat, the arrangement was a rope: it solved the coordination problem of religious governance at low cost. From the kokugaku scholar's seat, it was a snare: the coordination story was cover for the suppression of Shinto autonomy. From the common parishioner's seat, it was largely invisible infrastructure until the Meiji separation forcibly redefined their ritual obligations. The engine computes these divergences from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The Tokugawa state, syncretic institutions, and Meiji state builders occupy the beneficiary side of the directionality derivation: they collect stability, patronage, and nation-building utility from the constraint. Kokugaku scholars and common parishioners occupy the target side: they pay in suppressed doctrinal alternatives and limited ritual choice. The Meiji state's position is temporally displaced â it benefits from the constraint's dissolution, but structurally it is a downstream beneficiary of the instability the constraint created.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â preventing sectarian conflict â was dead by the late Edo period, yet the arrangement persisted because its beneficiaries (the Tokugawa state and religious institutions) had no incentive to dissolve it, and the cost of fixing it (imposing doctrinal clarity) was politically prohibitive under the old regime. The Meiji state solved this by changing the payoff structure, revealing the constraint as a zombie arrangement whose mandate had outlived its function. The R5 genealogy interview (founding_problem_status = dead + disappearance_verdict = world_rearranges) flags this as mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Does the historical record of Edo-period practice definitively support the incoherence reading over the syncretic and partition readings, or is the evidence equally compatible with all three?',
    'Comparative analysis of parish-temple doctrinal texts versus actual ritual performance records; if texts show systematic honji-suijaku metaphysics while practice shows arbitrary mixing, the readings describe different levels and the kernel is underdetermined.',
    'If underdetermined, the constraint''s classification as tangled_rope versus rope versus snare is reading-dependent rather than structurally fixed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Whether the historical evidence resolves the kernel in favor of one reading.').

omega_variable(
    meiji_benefit_contingency,
    'Did the Meiji state''s nation-building project benefit causally from the prior ontological incoherence, or would the separation have been equally achievable from a state of deep syncretic integration?',
    'Comparative historical analysis of other syncretic societies undergoing modernization; if separation ease correlates with shallow integration, the Meiji benefit is structurally tied to incoherence.',
    'Confirms whether the constraint''s instability was an inherent feature that transferred extraction to the Meiji state or merely a neutral background condition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_benefit_contingency, empirical, 'Causal link between prior incoherence and Meiji state-building.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Was the suppression of doctrinal dissent under shinbutsu-shugo primarily structural (state enforcement of the parish system) or internalized (priests and parishioners naturalizing syncretic mixture)?',
    'Post-Meiji trajectory of religious professionals: immediate embrace of separation suggests structural suppression; continued private syncretic practice suggests internalized suppression.',
    'If internalized, effective suppression is higher than structural measures indicate, pushing the constraint toward snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_commitment__incoherence_reading, 1600, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t1600, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1600, 0.25).
narrative_ontology:measurement(shin_tr_t1650, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1650, 0.35).
narrative_ontology:measurement(shin_tr_t1700, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1700, 0.42).
narrative_ontology:measurement(shin_tr_t1750, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1750, 0.5).
narrative_ontology:measurement(shin_tr_t1800, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1800, 0.58).
narrative_ontology:measurement(shin_tr_t1850, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1850, 0.65).
narrative_ontology:measurement(shin_tr_t1868, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1868, 0.8).

% Extraction over time
narrative_ontology:measurement(shin_be_t1600, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1600, 0.3).
narrative_ontology:measurement(shin_be_t1650, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1650, 0.4).
narrative_ontology:measurement(shin_be_t1700, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1700, 0.48).
narrative_ontology:measurement(shin_be_t1750, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1750, 0.55).
narrative_ontology:measurement(shin_be_t1800, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1800, 0.62).
narrative_ontology:measurement(shin_be_t1850, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1850, 0.7).
narrative_ontology:measurement(shin_be_t1868, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1868, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t1600, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1600, 0.2).
narrative_ontology:measurement(shin_su_t1650, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1650, 0.25).
narrative_ontology:measurement(shin_su_t1700, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1700, 0.3).
narrative_ontology:measurement(shin_su_t1750, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1750, 0.35).
narrative_ontology:measurement(shin_su_t1800, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1800, 0.42).
narrative_ontology:measurement(shin_su_t1850, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1850, 0.5).
narrative_ontology:measurement(shin_su_t1868, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1868, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_commitment__incoherence_reading, identity_coordination).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__incoherence_reading, shinbutsu_ontological_commitment__syncretic_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__incoherence_reading, shinbutsu_ontological_commitment__partition_reading).

% DUAL FORMULATION NOTE:
% This constraint decomposes the colloquial label shinbutsu-shugo into three structurally distinct readings: the syncretic reading (unified cosmology), the partition reading (separate functional domains), and the incoherence reading (no stable ontology). Each reading instantiates a different constraint with different beneficiaries, victims, and epsilon values. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
