% ============================================================================
% CONSTRAINT STORY: public_health_mandate_authority__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-25
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_health_mandate_authority__proportionality_reading, []).

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
 *   constraint_id: public_health_mandate_authority__proportionality_reading
 *   human_readable: Proportionality-Based Public Health Mandate Authority
 *   domain: public_health_law/constitutional_rights/bioethics
 *
 * SUMMARY:
 *   This constraint story captures the proportionality reading of public
 *   health mandate authority — the constitutional and bioethical position
 *   that mandate legitimacy is not categorical but depends on a sliding scale
 *   assessment of four variables: severity of threat, availability of
 *   alternatives, magnitude of coercion, and duration of imposition. Unlike
 *   the bodily_autonomy_primary reading (never justified) or the
 *   public_health_primary reading (obligatory when vulnerable populations are
 *   at risk), this reading instantiates a dynamic constraint whose
 *   extractiveness and victim boundary shift with epidemiological conditions.
 *   At T=10 (peak pandemic emergency), extractiveness and suppression spike
 *   as mandates broaden; as threat recedes, both decline but leave residual
 *   institutional capacity. The constraint is a tangled_rope: it performs
 *   genuine coordination (protecting the immunocompromised commons,
 *   preserving healthcare function) while extracting liberty from
 *   unvaccinated individuals through active enforcement, and its persistence
 *   depends on continuous proportionality reassessment.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__proportionality_reading, 0.45).
domain_priors:suppression_score(public_health_mandate_authority__proportionality_reading, 0.6).
domain_priors:theater_ratio(public_health_mandate_authority__proportionality_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(public_health_mandate_authority__proportionality_reading, "Proportionality-Based Public Health Mandate Authority").
narrative_ontology:topic_domain(public_health_mandate_authority__proportionality_reading, "public_health_law/constitutional_rights/bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__proportionality_reading, '339f75b6-d2f1-4a3c-abdc-8188aded4bb0').
narrative_ontology:cs_kernel_codification('339f75b6-d2f1-4a3c-abdc-8188aded4bb0', formalized).
narrative_ontology:cs_authority_grounding('339f75b6-d2f1-4a3c-abdc-8188aded4bb0', lineage).
narrative_ontology:cs_interpretation_layer_present('339f75b6-d2f1-4a3c-abdc-8188aded4bb0').
narrative_ontology:cs_reading_relation('339f75b6-d2f1-4a3c-abdc-8188aded4bb0', public_health_mandate_authority__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('339f75b6-d2f1-4a3c-abdc-8188aded4bb0', public_health_mandate_authority__public_health_primary, influences).
narrative_ontology:cs_axiom('339f75b6-d2f1-4a3c-abdc-8188aded4bb0', foundational, proportionality_test_required).
narrative_ontology:cs_axiom_status(proportionality_test_required, holdable).
narrative_ontology:cs_axiom_grounding('339f75b6-d2f1-4a3c-abdc-8188aded4bb0', proportionality_test_required, conventional).
narrative_ontology:cs_axiom('339f75b6-d2f1-4a3c-abdc-8188aded4bb0', foundational, least_restrictive_means_mandatory).
narrative_ontology:cs_axiom_status(least_restrictive_means_mandatory, holdable).
narrative_ontology:cs_axiom_grounding('339f75b6-d2f1-4a3c-abdc-8188aded4bb0', least_restrictive_means_mandatory, conventional).
narrative_ontology:cs_reference_frame('339f75b6-d2f1-4a3c-abdc-8188aded4bb0', constitutional_proportionality_framework).
narrative_ontology:cs_drift_state('339f75b6-d2f1-4a3c-abdc-8188aded4bb0', contemporary_pandemic_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('339f75b6-d2f1-4a3c-abdc-8188aded4bb0', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__proportionality_reading, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, immunocompromised_individuals).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, healthcare_infrastructure).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, public_health_system).
narrative_ontology:constraint_victim(public_health_mandate_authority__proportionality_reading, unvaccinated_individuals).
narrative_ontology:constraint_victim(public_health_mandate_authority__proportionality_reading, minority_communities_medical_distrust).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, general_population).
narrative_ontology:constraint_victim(public_health_mandate_authority__proportionality_reading, general_population).
narrative_ontology:constraint_vindicates(public_health_mandate_authority__proportionality_reading, proportionality_principle).
narrative_ontology:constraint_vindicates(public_health_mandate_authority__proportionality_reading, public_health_necessity).
narrative_ontology:constraint_vindicates(public_health_mandate_authority__proportionality_reading, least_restrictive_means_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issue mandate orders, define threat severity thresholds, determine available alternatives, set coercion magnitude and duration. Justify mandates as necessary and proportional. Control enforcement machinery and data infrastructure. Can modify or withdraw mandates as epidemiological conditions change.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Depend on high population-level protection to safely participate in society. Benefit from mandates that reduce community transmission. Have limited exit options — cannot individually avoid exposure in shared spaces. Their vulnerability is the primary justification for the proportionality framework's protective pole.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, immunocompromised_individuals, beneficiary,
    moderate, biographical, constrained, national).

% Bear direct coercion (fines, exclusion from venues, employment consequences) and indirect costs (social stigma, restricted mobility). Their reasons vary: medical contraindications, religious objection, distrust, access barriers. Exit requires either vaccination (which some cannot or will not accept) or accepting exclusion. The proportionality test is meant to limit but not eliminate this burden.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, unvaccinated_individuals, payer,
    moderate, biographical, constrained, national).

% Gain reduced transmission risk and healthcare system stability from mandates. Simultaneously bear restrictions on gathering, movement, and commerce. Can often exit by vaccinating (low friction for most). Experience the mandate as background condition of daily life rather than acute coercion.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, general_population, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(public_health_mandate_authority__proportionality_reading, general_population, payer).

% Disproportionately bear enforcement consequences (targeted policing, vaccine access barriers compounded by mandate penalties). Historical medical exploitation creates justified distrust that the proportionality framework does not structurally address. Exit is blocked by both structural barriers and the mandate itself — cannot easily vaccinate, cannot avoid penalties.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, minority_communities_medical_distrust, payer,
    powerless, generational, trapped, national).

% Argue that any non-consensual medical intervention violates fundamental sovereignty regardless of proportionality. Are structurally excluded from the proportionality calculus because the framework presupposes mandates can be legitimate — their categorical objection has no seat at the table. Seek constitutional or legislative foreclosure of mandate authority entirely.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, bodily_autonomy_advocates, excluded,
    organized, civilizational, analytical, global).

% Adjudicate and theorize the proportionality test: define the sliding scale's variables, review threat assessments, police the least-restrictive-means requirement. Their rulings determine whether the constraint operates as genuine coordination or de facto snare. Do not personally bear mandate costs or collect mandate benefits.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, courts_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a structured framework for legitimate state action during health emergencies that balances collective protection against individual liberty by requiring necessity, proportionality, least restrictive means, and time-limitation — replacing categorical permission or categorical prohibition with a calibrated test.
% TRANSFER_FUNCTION: Moves autonomy/liberty interests from unvaccinated and vaccine-hesitant individuals toward collective protection of immunocompromised persons and healthcare system capacity; moves enforcement resources and surveillance infrastructure to state authorities; moves evidentiary burden to the state to justify each mandate's severity-duration-coercion profile.
% ABSENT_VOICES: Unvaccinated individuals directly subject to mandates (especially those with access barriers rather than ideological objection), minority communities with historical medical distrust who bear disproportionate enforcement, children and incapacitated persons subjected to mandates without consent capacity, future generations who inherit the precedent of proportionality review for bodily integrity.
% DISAPPEARANCE_RATIONALE: Without the proportionality framework, mandate authority would collapse to either the bodily_autonomy_primary reading (categorical prohibition — no mandates ever) or the public_health_primary reading (categorical obligation — mandates whenever authorities declare necessity). The sliding scale is the only structure that calibrates state power to epidemiological reality; its disappearance forces a binary choice that rearranges the entire legal landscape of health emergencies.
% FOUNDING_PROBLEM: Legitimate state response to infectious disease threats that protects vulnerable populations without collapsing into either tyranny of the majority (unlimited mandate power) or suicide pact (zero mandate power). The proportionality test was built to navigate between these poles by making mandate legitimacy contingent on measurable threat severity, exhausted alternatives, calibrated coercion, and bounded duration.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional courts in Germany (BVerfG), Canada (SCC), South Africa (CC), and the European Court of Human Rights have independently adopted proportionality review for rights-limiting health measures — none of these courts are beneficiaries of mandate power. Public health law scholars (Gostin, Wiley, Burris) outside state authority attest to proportionality as the dominant legitimating framework. The WHO International Health Regulations (2005) embed proportionality language. No corroboration exists from within the mandate-enforcing agencies themselves.
narrative_ontology:disappearance_verdict(public_health_mandate_authority__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(public_health_mandate_authority__proportionality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(public_health_mandate_authority__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(public_health_mandate_authority__proportionality_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_health_mandate_authority__proportionality_reading_tests).
:- end_tests(public_health_mandate_authority__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness 0.45 reflects the constraint's conditional nature — it is not inherently extractive (like a snare) nor purely coordinative (like a rope). At high threat (Ebola-level) the proportionality test permits high extraction; at low threat (seasonal flu) it permits near-zero. The authored value represents the time-averaged operation across the interval. Suppression 0.60 captures that mandates require active enforcement (exclusion orders, fines, passport systems) and the proportionality test does not eliminate coercion — it structures it. Theater_ratio 0.30 reflects performative compliance rituals (temperature theater, hygiene theater) that persist even when epidemiological justification is thin. Accessibility_collapse 0.40: alternatives exist (vaccination, remote work, masking) but narrow as threat rises. Resistance 0.65: sustained legal challenges, protests, and non-compliance from bodily autonomy and medical freedom movements.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seat, the constraint appears as genuine coordination: a rational framework that prevents both under- and over-response. From the payer seats (unvaccinated, minority communities), the same structure operates as conditional extraction — the proportionality test legitimates coercion that would otherwise be illegitimate. The engine computes this divergence from the structural data; the proclaimed neutrality of the proportionality test is itself a structural feature that masks asymmetric burden distribution.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities are agenda_setters with institutional power and arbitrage-grade exit (they design the test, control the data, can modify rules). Immunocompromised individuals are beneficiaries with constrained exit — they gain protection but cannot individually opt out of vulnerability. Unvaccinated individuals are payers with constrained exit — they bear coercion, and exit requires either vaccination (which some cannot/won't accept) or accepting exclusion. Minority communities with medical distrust are payers with trapped exit — structural barriers compound mandate penalties. General population is dual beneficiary/payer with mobile exit (vaccination is low-friction for most). Bodily autonomy advocates are excluded — their categorical objection finds no purchase in a framework that presupposes mandates can be proportional. Courts are analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legitimate calibrated state response) remains contested — not dead, because infectious disease threats recur, but not live in the sense of settled consensus. The proportionality test prevents mislabeling coordination as pure extraction (by requiring necessity and least restrictive means) AND prevents mislabeling extraction as pure coordination (by requiring sunset and proportionality). Mandatrophy is unresolved: the framework persists between emergencies as dormant capacity, reactivating with each threat cycle. Whether this is adaptive scaffolding or institutional ratchet is the central diagnostic question.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_proportionality_reading,
    'This constraint is one reading (proportionality_reading) of the contested kernel public_health_mandate_authority. What structural elements would change if a sibling reading were instantiated instead?',
    'Compare the beneficiary/victim sets, extractiveness profiles, and enforcement logics across the three readings. The proportionality reading has a dynamic victim boundary and threat-indexed extractiveness; bodily_autonomy_primary fixes unvaccinated as sole victims with high extractiveness; public_health_primary fixes immunocompromised as sole beneficiaries with low extractiveness.',
    'If the kernel structure is misidentified as a single constraint rather than a family of three, the engine will compute a single classification that averages across irreconcilable structural differences, masking the contestation that is the kernel''s defining feature.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_proportionality_reading, conceptual, 'Commitment-system kernel decomposition: one kernel, three readings, three constraints.').

omega_variable(
    proportionality_test_genuineness,
    'Is the proportionality test a genuine constraint on state power that occasionally blocks mandates, or a rationalization framework that ratifies virtually any mandate authorities wish to impose?',
    'Empirical survey of judicial review outcomes: what fraction of challenged mandates are struck down or modified on proportionality grounds? Track whether the least-restrictive-means prong has independent bite or merely echoes the necessity prong.',
    'If the test is a rationalization, the constraint is a snare wearing a rope''s clothing — the coordination function is theater. If it genuinely blocks disproportionate mandates, the tangled_rope classification holds: real coordination function, real extraction, active enforcement required.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_test_genuineness, empirical, 'Whether the proportionality framework''s internal limits are operative or ceremonial.').

omega_variable(
    sliding_scale_ratchet,
    'Does the sliding scale create a stable equilibrium where mandates expand and contract with threat, or does it ratchet — each emergency normalizing a higher baseline of mandate authority?',
    'Longitudinal comparison of mandate scope/duration/coercion across successive health emergencies (H1N1 2009, Ebola 2014, COVID-19 2020, mpox 2022). Measure whether the ''floor'' of accepted mandate power rises over time.',
    'If ratchet dynamics dominate, the constraint drifts from tangled_rope toward snare — the coordination function atrophies while extraction capacity accumulates. This would trigger T17 mountain_extraction_accumulation logic if the constraint were claimed as mountain, but here it signals mandatrophy in a coordination-extraction hybrid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sliding_scale_ratchet, empirical, 'Whether the proportionality framework''s dynamic range is symmetric or ratchets toward expansion.').

omega_variable(
    victim_boundary_dynamism,
    'The proportionality reading places both immunocompromised and unvaccinated individuals in the potential victim set depending on threat assessment. Does this dynamism reflect genuine structural fluidity, or does it mask a fixed victim boundary that the proportionality language obscures?',
    'Trace whether immunocompromised individuals ever functionally occupy the victim seat (i.e., bear net costs from the mandate regime) in practice. During COVID-19, did mandates protect them sufficiently that they were net beneficiaries, or did mandate failures (breakthrough transmission, delayed care) make them net victims?',
    'If immunocompromised individuals are structurally always beneficiaries and unvaccinated always victims regardless of threat level, the ''dynamic victim boundary'' is a conceptual artifact and the constraint reduces to a fixed-extraction snare with proportionality theater.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_boundary_dynamism, empirical, 'Whether the reading''s claimed dynamic victim boundary is empirically realized or theoretically posited.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__proportionality_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(phm_proportionality_tr_t0, public_health_mandate_authority__proportionality_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(phm_proportionality_tr_t5, public_health_mandate_authority__proportionality_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(phm_proportionality_tr_t10, public_health_mandate_authority__proportionality_reading, theater_ratio, 10, 0.4).
narrative_ontology:measurement(phm_proportionality_tr_t15, public_health_mandate_authority__proportionality_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(phm_proportionality_tr_t20, public_health_mandate_authority__proportionality_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(phm_proportionality_tr_t25, public_health_mandate_authority__proportionality_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement(phm_proportionality_tr_t30, public_health_mandate_authority__proportionality_reading, theater_ratio, 30, 0.3).

% Extraction over time
narrative_ontology:measurement(phm_proportionality_be_t0, public_health_mandate_authority__proportionality_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(phm_proportionality_be_t5, public_health_mandate_authority__proportionality_reading, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(phm_proportionality_be_t10, public_health_mandate_authority__proportionality_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(phm_proportionality_be_t15, public_health_mandate_authority__proportionality_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(phm_proportionality_be_t20, public_health_mandate_authority__proportionality_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(phm_proportionality_be_t25, public_health_mandate_authority__proportionality_reading, base_extractiveness, 25, 0.38).
narrative_ontology:measurement(phm_proportionality_be_t30, public_health_mandate_authority__proportionality_reading, base_extractiveness, 30, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(phm_proportionality_su_t0, public_health_mandate_authority__proportionality_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(phm_proportionality_su_t5, public_health_mandate_authority__proportionality_reading, suppression_requirement, 5, 0.45).
narrative_ontology:measurement(phm_proportionality_su_t10, public_health_mandate_authority__proportionality_reading, suppression_requirement, 10, 0.75).
narrative_ontology:measurement(phm_proportionality_su_t15, public_health_mandate_authority__proportionality_reading, suppression_requirement, 15, 0.65).
narrative_ontology:measurement(phm_proportionality_su_t20, public_health_mandate_authority__proportionality_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(phm_proportionality_su_t25, public_health_mandate_authority__proportionality_reading, suppression_requirement, 25, 0.5).
narrative_ontology:measurement(phm_proportionality_su_t30, public_health_mandate_authority__proportionality_reading, suppression_requirement, 30, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_mandate_authority__proportionality_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(public_health_mandate_authority__proportionality_reading, 0.1).
narrative_ontology:affects_constraint(public_health_mandate_authority__proportionality_reading, public_health_mandate_authority__bodily_autonomy_primary).
narrative_ontology:affects_constraint(public_health_mandate_authority__proportionality_reading, public_health_mandate_authority__public_health_primary).

% DUAL FORMULATION NOTE:
% This constraint (proportionality_reading) and its two siblings (bodily_autonomy_primary, public_health_primary) form a constraint family decomposing the kernel public_health_mandate_authority. Each reading instantiates a distinct constraint with different beneficiary/victim structures, extractiveness profiles, and classification outcomes. The proportionality reading is the only one with dynamic, threat-indexed extractiveness and a fluid victim boundary. The bodily_autonomy_primary reading classifies as snare (categorical extraction from unvaccinated). The public_health_primary reading classifies as rope or scaffold (coordination to protect vulnerable commons). This decomposition follows the BGS pattern: a single colloquial label ('public health mandate authority') conceals structurally distinct constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(public_health_mandate_authority__proportionality_reading, institutional, 0.15).
constraint_indexing:directionality_override(public_health_mandate_authority__proportionality_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
