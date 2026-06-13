% ============================================================================
% CONSTRAINT STORY: july_charter_sovereign_legitimacy__guided_nationalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_july_charter_sovereign_legitimacy__guided_nationalism_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: july_charter_sovereign_legitimacy__guided_nationalism_reading
 *   human_readable: Islamic-Nationalist Sovereign Legitimacy Framework (Guided Nationalism Reading)
 *   domain: constitutional_law/political_transitions
 *
 * SUMMARY:
 *   A post-revolutionary charter grounds state legitimacy in
 *   Islamic-nationalist identity and religious constitutional principles.
 *   This story instantiates the GUIDED NATIONALISM READING: the charter's
 *   primary function is to establish Islamic-nationalist as the sovereign
 *   legitimacy basis, with religious law or norms gaining constitutional
 *   status and secular institutions constrained. The religious-nationalist
 *   coalition and constitutional guardian council benefit from concentrated
 *   institutional authority; secular civil society, religious minorities, and
 *   secular legal professions bear extraction through institutional
 *   marginalization and subordinate legal status. The charter is CLAIMED as
 *   tangled_rope (genuine coordination on national legitimacy + asymmetric
 *   enforcement) while the metrics describe rising extractiveness and
 *   suppression — the divergence captures the contestation over whether this
 *   is legitimate state sovereignty assertion or institutional capture
 *   dressed as legitimacy. The temporal series shows enforcement
 *   intensification: extractiveness and suppression both rise as the reading
 *   is operationalized, suggesting institutional entrenchment rather than
 *   stable coordination.
 *
 * KEY AGENTS:
 *   - religious_nationalist_coalition: Benefits from constitutional authority and interpretive monopoly; sets the legitimacy frame; identity-locked to Islamic-nationalist ideology
 *   - constitutional_guardian_council: Institutional beneficiary and agenda-setter; wields veto authority over legislation and appointments; carries both institutional power and guardianship authority
 *   - secular_civil_society: Payer; marginalized from constitutional voice; constrained institutional alternatives; moderate power but limited exit
 *   - religious_minorities: Payer; trapped exit; bears subordinate legal status and constrained civic space; powerless institutional position
 *   - secular_legal_professionals: Payer; identity-locked (career built on secular jurisprudence); retraining requirements; demotion of professional authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.68).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.71).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, accessibility_collapse, 0.76).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__guided_nationalism_reading, tangled_rope).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__guided_nationalism_reading, "Islamic-Nationalist Sovereign Legitimacy Framework (Guided Nationalism Reading)").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__guided_nationalism_reading, "constitutional_law/political_transitions").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__guided_nationalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__guided_nationalism_reading, '5988d73c-f0fe-44b1-9649-7b60d9f8cc2a').
narrative_ontology:cs_kernel_codification('5988d73c-f0fe-44b1-9649-7b60d9f8cc2a', formalized).
narrative_ontology:cs_authority_grounding('5988d73c-f0fe-44b1-9649-7b60d9f8cc2a', lineage).
narrative_ontology:cs_interpretation_layer_present('5988d73c-f0fe-44b1-9649-7b60d9f8cc2a').
narrative_ontology:cs_reading_relation('5988d73c-f0fe-44b1-9649-7b60d9f8cc2a', july_charter_sovereign_legitimacy__secular_democratic_reading, forecloses).
narrative_ontology:cs_reading_relation('5988d73c-f0fe-44b1-9649-7b60d9f8cc2a', july_charter_sovereign_legitimacy__military_custodian_reading, coexists_with).
narrative_ontology:cs_axiom('5988d73c-f0fe-44b1-9649-7b60d9f8cc2a', foundational, islamic_identity_as_state_legitimacy).
narrative_ontology:cs_axiom_status(islamic_identity_as_state_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('5988d73c-f0fe-44b1-9649-7b60d9f8cc2a', islamic_identity_as_state_legitimacy, deontological).
narrative_ontology:cs_axiom('5988d73c-f0fe-44b1-9649-7b60d9f8cc2a', secondary, secular_institutional_subordination_to_religious_constitutional_ground).
narrative_ontology:cs_axiom_status(secular_institutional_subordination_to_religious_constitutional_ground, holdable).
narrative_ontology:cs_axiom_grounding('5988d73c-f0fe-44b1-9649-7b60d9f8cc2a', secular_institutional_subordination_to_religious_constitutional_ground, conventional).
narrative_ontology:cs_reference_frame('5988d73c-f0fe-44b1-9649-7b60d9f8cc2a', islamic_nationalist_post_revolutionary_legitimacy).
narrative_ontology:cs_drift_state('5988d73c-f0fe-44b1-9649-7b60d9f8cc2a', contemporary_institutionalization, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5988d73c-f0fe-44b1-9649-7b60d9f8cc2a', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__guided_nationalism_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_nationalist_coalition).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__guided_nationalism_reading, constitutional_guardian_council).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, secular_civil_society).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_minorities).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, secular_legal_professionals).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__guided_nationalism_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__guided_nationalism_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(july_charter_sovereign_legitimacy__guided_nationalism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__guided_nationalism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(july_charter_sovereign_legitimacy__guided_nationalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness begins at 0.42 (projection of post-charter institutional establishment) and rises to 0.68 by interval end, indicating increasing institutional consolidation and power concentration in the coalition and guardian council. Suppression requirement rises from 0.48 to 0.71, showing that maintaining the reading requires intensifying enforcement mechanisms — vetoing legislation, blocking secular-oriented candidates, removing secular judges. Theater ratio rises from 0.28 to 0.42, suggesting the constraint's legitimacy-coordination function is gradually being replaced by enforcement performance (justifying suppression as protecting state legitimacy). The gap between claimed_type (tangled_rope) and measured extraction reflects the core contest: does the charter coordinate genuine post-revolutionary legitimacy, or does it operationalize institutional capture by the coalition? The temporal data show extraction accumulating, which supports the institutional-capture reading.
 *
 * PERSPECTIVAL GAP:
 *   From the religious-nationalist coalition and guardian council seat, the charter solves a genuine coordination problem: post-revolutionary state needs a legitimacy foundation; Islamic nationalism bridges religious popular sentiment and state authority; the constraint coordinates national identity and prevents the state from fragmenting into secular/military/foreign-influenced factions. From the secular civil society and legal professional seats, the same structure operates as institutional exclusion and power grab: the coalition weaponizes legitimacy language to concentrate institutional authority; the coordination story is cover for extraction; exit options are closed by enforcement, not by genuine preference for coordination. The engine computes per-seat classification from power, exit, and beneficiary/victim structure; this divergence should emerge as the secular seats computing snare while the coalition seats compute rope. If all seats converge on snare, the institutional-capture reading is validated; if seats diverge by role, the tangled-rope claim is partially vindicated.
 *
 * DIRECTIONALITY LOGIC:
 *   The religious-nationalist coalition holds institutional power (power=institutional), identity-locked to the reading (exit=identity_locked), and appears in beneficiaries — directionality near 0.0 (beneficiary end). The guardian council holds institutional power and arbitrage exit (can interpret the charter flexibly or enforce it rigidly) but also appears in beneficiaries — directionality also near the beneficiary end, with some upward modulation from institutional enforcement role. Secular civil society holds moderate power, constrained exit (institutional alternatives closed, emigration difficult), appears in victims — directionality near 0.7 (target end). Religious minorities hold powerless status, trapped exit, victims — directionality near 1.0 (full target end). Secular legal professionals hold moderate power, identity_locked exit (career depends on secular jurisprudence; staying requires accepting demotion), victims — directionality near 0.75-0.80. These directionality values feed the engine's computation of effective extraction per seat; the result should show snare-like dynamics for the victim seats despite the tangled_rope claim at the story level.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is contested: the coalition attests the post-revolutionary legitimacy deficit persists and requires Islamic-nationalist grounding; secular actors attest the founding problem is solved (state exists, has institutional structure) and the constraint now functions as power maintenance. The disappearance_verdict is world_rearranges: if the charter reading vanished, secular democratic or military-custodian readings would reorganize institutions. The mismatch (status=contested + verdict=world_rearranges) does NOT trigger mandatrophy_resolved — the founding problem is genuinely live within the coalition's frame, just contested. Mandatrophy would resolve only if founding_problem_status converged to dead (all parties attest the legitimacy problem is solved, yet enforcement persists) — which the evidence does not show. The constraint remains functionally valid, though operationally contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    religious_identity_vs_constructed_nationalism,
    'Is the Islamic-nationalist reading a reflection of genuine popular legitimacy grounded in religious identity, or a constructed institutional frame imposed by a coalition that benefits from the religious-constitutional arrangement?',
    'Historical analysis of pre-charter popular sentiment, empirical study of how the charter frame was institutionalized and enforced, survey evidence of whether populations outside the coalition genuinely prefer Islamic-grounded legitimacy or accept it under constraint, exit-intention analysis among secular professionals and minorities.',
    'If genuinely popular and organic, the constraint operates as coordination on shared identity; if constructed and maintained through institutional control, it is pure extraction riding on the appearance of legitimacy. Classification would shift from tangled_rope toward snare; suppression mechanisms become the primary constraint function rather than coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(religious_identity_vs_constructed_nationalism, empirical, 'Whether Islamic-nationalist framing expresses popular consensus or institutional imposition.').

omega_variable(
    religious_minorities_agency_and_exit,
    'Are religious minorities'' constrained exit options and subordinate legal status structurally necessary features of the Islamic-nationalist reading, or contingent institutional choices within that reading?',
    'Comparative constitutional analysis: do other Islamic-nationalist states protect religious minority rights while maintaining Islamic legitimacy grounding? Survey evidence: do religious minorities accept subordinate status as necessary cost of Islamic legitimacy, or view it as discretionary extraction?',
    'If necessary, the constraint''s victim set is inherent to the reading; if contingent, the reading could coordinate on Islamic nationalism while protecting minorities, shifting the extraction measurement and potential remedies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(religious_minorities_agency_and_exit, conceptual, 'Whether minority subordination is integral or contingent to the reading.').

omega_variable(
    secular_legal_system_incompatibility,
    'Are secular law and Islamic-nationalist legitimacy genuinely incompatible at the constitutional level, or can they coexist with Islamic-grounded legitimacy while preserving secular legal institutions for civil/commercial law and religious minorities'' personal autonomy?',
    'Constitutional design analysis from comparable systems; empirical study of whether the charter actively suppresses secular law or merely subordinates it; examination of whether suppression intensity correlates with securing Islamic legitimacy or with concentrating power in the guardian council.',
    'If genuinely incompatible, the constraint''s design is structurally necessary; if coexistence is possible, suppression of secular law becomes excess extraction not needed for the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_legal_system_incompatibility, conceptual, 'Whether secular and Islamic-nationalist institutions can coexist at constitutional level.').

omega_variable(
    guardian_council_independence_vs_coalition_capture,
    'Is the constitutional guardian council acting as an independent interpreter of Islamic principles, or has it become institutionally captured by the religious-nationalist coalition, serving as an enforcement arm for coalition preferences rather than a neutral arbiter?',
    'Analysis of guardian council veto patterns: are vetoes tied to consistency with constitutional religious principles, or to coalition political interests? Study of appointment processes and council members'' institutional affiliations; comparison to similar guardianship structures in other systems.',
    'If genuinely independent, the council distributes enforcement authority across an institutional separation; if captured, all enforcement power concentrates in the coalition, intensifying the extraction mechanism and narrowing the beneficiary base.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(guardian_council_independence_vs_coalition_capture, empirical, 'Whether guardian council operates independently or as coalition instrument.').

omega_variable(
    reading_kernel_contest_framing,
    'This constraint is ONE READING of a contested constitutional kernel. The sibling readings—secular_democratic_reading and military_custodian_reading—offer structurally different legitimacy bases and institutional arrangements. What structural facts would resolve which reading the charter actually instantiates?',
    'Comparative analysis of the charter''s text, institutional enforcement patterns, and political history: (1) If the charter''s primary function is protecting democratic participation and civil rights, it instantiates the secular_democratic_reading. (2) If the primary function is guaranteeing military institutional supremacy and stability, it instantiates the military_custodian_reading. (3) If the primary function is grounding state legitimacy in Islamic-nationalist identity and constraining secular institutions, it instantiates this reading (guided_nationalism_reading). The enforcement patterns and beneficiary structure reveal which reading governs in practice.',
    'Each reading produces a different constraint structure: different beneficiary sets, different victim sets, different directionality distributions. The true reading determines which institutional actors hold power and which bear costs. Misidentifying the reading produces fundamental misclassification of the constraint''s type and function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_contest_framing, conceptual, 'Which sibling reading the charter actually instantiates determines constraint structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(july_tr_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(july_tr_t5, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(july_tr_t10, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement(july_tr_t15, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement(july_tr_t20, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(july_tr_t25, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 25, 0.42).

% Extraction over time
narrative_ontology:measurement(july_be_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(july_be_t5, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 5, 0.51).
narrative_ontology:measurement(july_be_t10, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(july_be_t15, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(july_be_t20, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(july_be_t25, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(july_su_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(july_su_t5, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 5, 0.56).
narrative_ontology:measurement(july_su_t10, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(july_su_t15, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 15, 0.67).
narrative_ontology:measurement(july_su_t20, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(july_su_t25, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 25, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__guided_nationalism_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.12).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__guided_nationalism_reading, july_charter_sovereign_legitimacy__military_custodian_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__guided_nationalism_reading, july_charter_sovereign_legitimacy__secular_democratic_reading).

% DUAL FORMULATION NOTE:
% This story is one reading of a contested constitutional kernel. The kernel is the post-revolutionary charter; three distinct constraints emerge from three readings: (1) guided_nationalism_reading (this file) — Islamic-nationalist legitimacy, religious-constitutional status, secular institutions constrained; (2) military_custodian_reading — military institutional guardianship, stability as legitimacy ground, secular civilians subordinate; (3) secular_democratic_reading — democratic consent and secular law as legitimacy bases, military subordination to civilian authority. Each reading instantiates a different constraint with different beneficiary/victim structures, different ε values, and different types. The readings are NOT observations of the same constraint from different angles — they are structurally distinct constraints that share a common kernel text. The epsilon values diverge: guided_nationalism extracts from secular civil society and minorities (high ε); military_custodian extracts from democratic institutions and civilians (different ε and victim set); secular_democratic extracts from religious nationalists and military elites (inverse victim set). This is the ε-invariance principle in action: changing the observable (which reading governs in practice) changes ε, so you have multiple constraints, not one constraint viewed differently. All three stories must be authored independently and linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(july_charter_sovereign_legitimacy__guided_nationalism_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
