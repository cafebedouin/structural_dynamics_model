% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_commitment__incoherence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   human_readable: Shinbutsu-shūgō as Institutionally Tolerated Ontological Incoherence
 *   domain: religious_studies/japanese_history/ontology_of_practice
 *
 * SUMMARY:
 *   This constraint describes the state of shinbutsu-shūgō (the syncretic
 *   fusion of Shinto and Buddhism in Japan) as institutionally tolerated
 *   ontological incoherence, rather than a coherent syncretic system or a
 *   clear partition. This reading posits that no stable, unified ontological
 *   commitment existed, allowing for diverse local practices but leaving the
 *   system vulnerable to later state-imposed separation. The low
 *   extractiveness and suppression reflect the nature of 'incoherence'
 *   itself, which did not actively coerce or extract, but rather represented
 *   an atrophied, ambiguous state. The high theater ratio reflects the
 *   continuation of rituals and institutional forms despite the underlying
 *   lack of a unified philosophical grounding.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_commitment__incoherence_reading, 0.15).
domain_priors:suppression_score(shinbutsu_ontological_commitment__incoherence_reading, 0.05).
domain_priors:theater_ratio(shinbutsu_ontological_commitment__incoherence_reading, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, accessibility_collapse, 0.1).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_commitment__incoherence_reading, piton).
narrative_ontology:human_readable(shinbutsu_ontological_commitment__incoherence_reading, "Shinbutsu-shūgō as Institutionally Tolerated Ontological Incoherence").
narrative_ontology:topic_domain(shinbutsu_ontological_commitment__incoherence_reading, "religious_studies/japanese_history/ontology_of_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_commitment__incoherence_reading, '29f1e194-431b-4519-92ac-787dcce338b6').
narrative_ontology:cs_kernel_codification('29f1e194-431b-4519-92ac-787dcce338b6', implicit).
narrative_ontology:cs_authority_grounding('29f1e194-431b-4519-92ac-787dcce338b6', practice).
narrative_ontology:cs_reading_relation('29f1e194-431b-4519-92ac-787dcce338b6', shinbutsu_ontological_commitment__syncretic_reading, forecloses).
narrative_ontology:cs_reading_relation('29f1e194-431b-4519-92ac-787dcce338b6', shinbutsu_ontological_commitment__partition_reading, coexists_with).
narrative_ontology:cs_axiom('29f1e194-431b-4519-92ac-787dcce338b6', foundational, ontological_ambiguity_prevailed).
narrative_ontology:cs_axiom_status(ontological_ambiguity_prevailed, holdable).
narrative_ontology:cs_axiom_grounding('29f1e194-431b-4519-92ac-787dcce338b6', ontological_ambiguity_prevailed, conventional).
narrative_ontology:cs_axiom('29f1e194-431b-4519-92ac-787dcce338b6', foundational, institutional_flexibility_over_doctrine).
narrative_ontology:cs_axiom_status(institutional_flexibility_over_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('29f1e194-431b-4519-92ac-787dcce338b6', institutional_flexibility_over_doctrine, conventional).
narrative_ontology:cs_reference_frame('29f1e194-431b-4519-92ac-787dcce338b6', pre_meiji_institutional_ambiguity).
narrative_ontology:cs_drift_state('29f1e194-431b-4519-92ac-787dcce338b6', meiji_restoration_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('29f1e194-431b-4519-92ac-787dcce338b6', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_commitment__incoherence_reading, shinbutsu_ontological_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, local_religious_institutions).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, meiji_state_builders).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__incoherence_reading, common_worshippers).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__incoherence_reading, intellectual_elites_seeking_coherence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefited from the flexibility and lack of strict doctrinal oversight that ontological incoherence allowed, enabling them to adapt practices and beliefs to local needs without needing to conform to a unified, centralized system. This made them resilient to internal theological disputes but vulnerable to external, coherent state policies.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, local_religious_institutions, beneficiary,
    moderate, generational, constrained, local).

% Later, as the Meiji state sought to establish a unified national identity, they acted as agenda-setters for the separation of Shinto and Buddhism. They benefited from the prior institutional tolerance of ontological incoherence, as it made the implementation of their separation policies structurally easier and less costly than if a deeply unified system had existed.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, meiji_state_builders, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__incoherence_reading, meiji_state_builders, beneficiary).

% Experienced a diffuse cost of ontological ambiguity, potentially lacking a clear, unified spiritual framework for understanding their world. Later, they bore the direct costs of forced separation, being compelled to choose between Shinto and Buddhist affiliations for their local institutions and practices, often disrupting long-standing community traditions.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, common_worshippers, payer,
    powerless, biographical, trapped, local).

% Debated the nature of shinbutsu-shūgō, with some seeking a coherent theological or philosophical framework, and others acknowledging or even advocating for the practical ambiguity. They were not direct payers or beneficiaries of the institutional incoherence itself, but rather engaged with its conceptual implications, often frustrated by the lack of a clear system.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, intellectual_elites_seeking_coherence, observer,
    moderate, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_commitment__incoherence_reading, diffuse).
narrative_ontology:fixing_cost_class(shinbutsu_ontological_commitment__incoherence_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allowed diverse local religious practices and beliefs, integrating indigenous kami worship with imported Buddhism, to coexist under a broad, ambiguous institutional umbrella without requiring strict doctrinal or ontological consistency.
% TRANSFER_FUNCTION: Maintained a diffuse flow of cultural, spiritual, and social capital within local communities, supporting a variety of religious institutions and practices without a clear, centralized extraction mechanism or unified theological framework.
% ABSENT_VOICES: Those who sought clear theological or ontological consistency, or those who would later be dispossessed by forced separation, were not effectively represented in the 'tolerated incoherence.' Their calls for clarity or resistance to later separation were not part of the prevailing institutional ambiguity.
% DISAPPEARANCE_RATIONALE: If the institutional tolerance of incoherence had vanished overnight (e.g., replaced by a strong, unified theological system), the existing religious landscape would have been fundamentally reshaped. The ease with which the Meiji state later imposed separation demonstrates the prior structural significance of this ambiguous arrangement.
% FOUNDING_PROBLEM: The need to integrate or reconcile indigenous kami worship with imported Buddhism, leading to a practical, rather than strictly doctrinal, modus vivendi that avoided explicit ontological commitments.
% FOUNDING_PROBLEM_CORROBORATION: Historical records of widespread syncretic practices, the absence of unified theological treatises on shinbutsu-shūgō, and later Meiji-era policies that explicitly targeted this ambiguity, are corroborated by historians and religious studies scholars outside the directly benefiting parties.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_commitment__incoherence_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_commitment__incoherence_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_commitment__incoherence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(shinbutsu_ontological_commitment__incoherence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_commitment__incoherence_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_commitment__incoherence_reading_tests).
:- end_tests(shinbutsu_ontological_commitment__incoherence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The classification as a Piton reflects the view that the 'coherent' function of shinbutsu-shūgō (as a unified religious system) had atrophied, leaving behind a set of practices and institutions maintained largely by inertia and local custom, rather than a strong, unified ontological commitment. No single party actively maintained this incoherence for concentrated benefit, nor was any party sufficiently harmed by the incoherence itself to force its resolution. The Meiji state later found it 'cheap' to dismantle precisely because of this prior structural ambiguity.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of local institutions, the incoherence offered practical flexibility. From the perspective of the Meiji state, it was a weakness to be exploited. From the perspective of common worshippers, it was a diffuse ambiguity that later became a source of disruption. The engine's per-seat classification will capture these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Local religious institutions were beneficiaries of the flexibility offered by incoherence, allowing them to operate without strict doctrinal adherence. The Meiji state builders were beneficiaries in that the prior incoherence made their later separation policies easier to implement. Common worshippers bore diffuse costs of ambiguity and later direct costs of forced separation. Intellectual elites were observers, engaging with the conceptual problem rather than being directly extracted from or benefiting from the institutional arrangement.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_status_of_shinbutsu_shugo,
    'Was shinbutsu-shūgō truly ontologically incoherent, or did it represent a different, non-Western form of coherent syncretism or functional partition?',
    'Deep historical and philosophical analysis of primary religious texts and practices, focusing on indigenous categories of understanding rather than imposing Western ontological frameworks. Comparative studies with other syncretic traditions.',
    'If a coherent syncretism or partition is established, the constraint would reclassify, likely as a Rope (for coordination) or even a Mountain (if seen as a natural outcome of cultural interaction), with different beneficiaries and victims. If confirmed as incoherence, the Piton classification is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ontological_status_of_shinbutsu_shugo, conceptual, 'Ambiguity regarding the fundamental ontological nature of shinbutsu-shūgō.').

omega_variable(
    impact_of_incoherence_on_worshippers,
    'What was the actual lived experience and impact of this ontological incoherence on common worshippers and their spiritual lives?',
    'Archaeological evidence, ethnographic studies of surviving local traditions, and analysis of popular religious literature and art from the period to gauge the practical implications of ambiguity.',
    'If evidence suggests significant spiritual distress or confusion, the ''payer'' role for common worshippers would be strengthened, potentially increasing the effective extraction. If it suggests practical adaptability and resilience, their ''payer'' role might be attenuated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_of_incoherence_on_worshippers, empirical, 'Empirical impact of ontological ambiguity on religious practitioners.').

omega_variable(
    toleration_as_control_mechanism,
    'Was the ''institutional toleration'' of incoherence a genuine lack of centralized concern for doctrinal purity, or a subtle form of control that maintained local autonomy while preventing the emergence of a unified, potentially challenging, religious authority?',
    'Analysis of state policies and religious administration during the Edo period, examining instances where attempts at doctrinal unification or challenges to ambiguity were suppressed or co-opted.',
    'If toleration is found to be a subtle control mechanism, the ''suppression'' metric would be higher, and the constraint might lean more towards a Tangled Rope, with the central authorities as beneficiaries of this ''managed'' incoherence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(toleration_as_control_mechanism, conceptual, 'Nature of ''toleration'' – genuine ambiguity vs. subtle control.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_commitment__incoherence_reading, 1600, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t1600, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1600, 0.65).
narrative_ontology:measurement(shin_tr_t1650, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1650, 0.68).
narrative_ontology:measurement(shin_tr_t1700, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1700, 0.7).
narrative_ontology:measurement(shin_tr_t1750, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1750, 0.72).
narrative_ontology:measurement(shin_tr_t1800, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1800, 0.73).
narrative_ontology:measurement(shin_tr_t1868, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1868, 0.7).

% Extraction over time
narrative_ontology:measurement(shin_be_t1600, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1600, 0.12).
narrative_ontology:measurement(shin_be_t1650, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1650, 0.13).
narrative_ontology:measurement(shin_be_t1700, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1700, 0.14).
narrative_ontology:measurement(shin_be_t1750, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1750, 0.14).
narrative_ontology:measurement(shin_be_t1800, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1800, 0.15).
narrative_ontology:measurement(shin_be_t1868, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1868, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t1600, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1600, 0.05).
narrative_ontology:measurement(shin_su_t1650, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1650, 0.05).
narrative_ontology:measurement(shin_su_t1700, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1700, 0.05).
narrative_ontology:measurement(shin_su_t1750, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1750, 0.05).
narrative_ontology:measurement(shin_su_t1800, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1800, 0.05).
narrative_ontology:measurement(shin_su_t1868, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 1868, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
