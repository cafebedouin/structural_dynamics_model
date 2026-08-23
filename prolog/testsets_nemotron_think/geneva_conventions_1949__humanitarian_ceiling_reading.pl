% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__humanitarian_ceiling_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_1949__humanitarian_ceiling_reading, []).

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
 *   constraint_id: geneva_conventions_1949__humanitarian_ceiling_reading
 *   human_readable: Geneva Conventions 1949 — Humanitarian Ceiling Reading
 *   domain: international_humanitarian_law
 *
 * SUMMARY:
 *   This reading of the 1949 Geneva Conventions holds that the treaties
 *   establish absolute humanitarian minimums — a ceiling on permissible
 *   violence — that bind states irrespective of adversary compliance. The
 *   conventions' core protections (Common Article 3, GC III/IV fundamental
 *   guarantees) are non-derogable: they apply in all armed conflicts, to all
 *   persons, without reciprocity conditions. This reading suppresses security
 *   rationales that would degrade protections for irregular combatants or in
 *   asymmetric warfare, and imposes an asymmetric burden on state militaries
 *   who must comply even when adversaries do not. The constraint is claimed
 *   as tangled_rope: genuine coordination (universal humanitarian floor) with
 *   asymmetric extraction (states bear disproportionate costs), requiring
 *   active enforcement through ICRC monitoring, international courts, and
 *   state reporting.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__humanitarian_ceiling_reading, 0.45).
domain_priors:suppression_score(geneva_conventions_1949__humanitarian_ceiling_reading, 0.6).
domain_priors:theater_ratio(geneva_conventions_1949__humanitarian_ceiling_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__humanitarian_ceiling_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_1949__humanitarian_ceiling_reading, "Geneva Conventions 1949 — Humanitarian Ceiling Reading").
narrative_ontology:topic_domain(geneva_conventions_1949__humanitarian_ceiling_reading, "international_humanitarian_law").

domain_priors:requires_active_enforcement(geneva_conventions_1949__humanitarian_ceiling_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__humanitarian_ceiling_reading, '0d3cb25c-01fb-4912-96b4-302086561f7e').
narrative_ontology:cs_kernel_codification('0d3cb25c-01fb-4912-96b4-302086561f7e', fixed_text).
narrative_ontology:cs_authority_grounding('0d3cb25c-01fb-4912-96b4-302086561f7e', lineage).
narrative_ontology:cs_interpretation_layer_present('0d3cb25c-01fb-4912-96b4-302086561f7e').
narrative_ontology:cs_reading_relation('0d3cb25c-01fb-4912-96b4-302086561f7e', geneva_conventions_1949__conditional_reciprocity_reading, forecloses).
narrative_ontology:cs_reading_relation('0d3cb25c-01fb-4912-96b4-302086561f7e', geneva_conventions_1949__security_maximization_reading, forecloses).
narrative_ontology:cs_axiom('0d3cb25c-01fb-4912-96b4-302086561f7e', foundational, humanitarian_protections_are_non_derogable).
narrative_ontology:cs_axiom_status(humanitarian_protections_are_non_derogable, holdable).
narrative_ontology:cs_axiom_grounding('0d3cb25c-01fb-4912-96b4-302086561f7e', humanitarian_protections_are_non_derogable, deontological).
narrative_ontology:cs_axiom('0d3cb25c-01fb-4912-96b4-302086561f7e', foundational, irregular_combatants_retain_basic_protections).
narrative_ontology:cs_axiom_status(irregular_combatants_retain_basic_protections, holdable).
narrative_ontology:cs_axiom_grounding('0d3cb25c-01fb-4912-96b4-302086561f7e', irregular_combatants_retain_basic_protections, deontological).
narrative_ontology:cs_reference_frame('0d3cb25c-01fb-4912-96b4-302086561f7e', geneva_1949_humanitarian_floor).
narrative_ontology:cs_drift_state('0d3cb25c-01fb-4912-96b4-302086561f7e', contemporary_asymmetric_warfare_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0d3cb25c-01fb-4912-96b4-302086561f7e', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__humanitarian_ceiling_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, civilian_populations).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, detainees_prisoners).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, irregular_combatants).
narrative_ontology:constraint_victim(geneva_conventions_1949__humanitarian_ceiling_reading, state_militaries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(geneva_conventions_1949__humanitarian_ceiling_reading, non_state_armed_groups).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__humanitarian_ceiling_reading, humanitarian_law_supremacy).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__humanitarian_ceiling_reading, non_derogable_rights_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Protected from direct attack, indiscriminate warfare, and collective punishment regardless of which party controls their territory. Cannot exit conflict zones; protection depends entirely on belligerent compliance with conventions they did not negotiate.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, civilian_populations, beneficiary,
    powerless, civilizational, trapped, global).

% Entitled to humane treatment, judicial guarantees, and protection from torture or degrading treatment under Common Article 3 and Geneva Convention III/IV irrespective of their status or captor's reciprocity. Physically unable to exit detention; protection is their only shield.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, detainees_prisoners, beneficiary,
    powerless, biographical, trapped, global).

% Retain fundamental guarantees (Common Article 3, Additional Protocol II) — humane treatment, fair trial protections, prohibition of violence to life and person — even without lawful combatant or POW status. Cannot exit the conflict; their protection is structural, not conditional on state recognition.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, irregular_combatants, beneficiary,
    powerless, biographical, trapped, global).

% Bear asymmetric compliance burden: must respect protections even when adversaries violate them, accept operational constraints (distinction, proportionality, precaution) that adversaries ignore, and submit to international monitoring. Cannot exit treaty obligations without severe reputational and legal consequences; compliance is structurally enforced.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, state_militaries, payer,
    institutional, generational, constrained, global).

% ICRC, UN treaty bodies, and international courts monitor compliance, issue authoritative interpretations, and maintain the convention's normative architecture. Their mandate derives from the conventions themselves; they set the agenda for what constitutes violation and drive enforcement discourse.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, ichr_monitoring_bodies, agenda_setter,
    institutional, generational, analytical, global).

% Formally bound by Common Article 3 and customary IHL but structurally excluded from treaty negotiation and often from compliance mechanisms. Face pressure to comply without reciprocal guarantees; their fighters receive protections they may not extend. Exit from obligations is not legally available but practical non-compliance is widespread.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, non_state_armed_groups, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_1949__humanitarian_ceiling_reading, non_state_armed_groups, excluded).

% Operate in conflict zones under convention-derived protections (medical missions, relief access). Document violations, advocate compliance, and provide services the conventions envisage. Their presence depends on belligerent consent; they observe but do not adjudicate.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, humanitarian_ngos, observer,
    organized, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal humanitarian floor protecting civilians, detainees, and combatants hors de combat from the worst effects of war, solving the coordination problem of mutual restraint in armed conflict by making core protections non-derogable and reciprocal only in form, not in condition.
% TRANSFER_FUNCTION: Transfers operational freedom and military advantage from state militaries (who bear compliance costs and asymmetric restraint) to protected persons (civilians, detainees, irregular combatants) who receive absolute protections regardless of adversary reciprocity or compliance.
% ABSENT_VOICES: Non-state armed groups who reject the conventions entirely; revisionist states arguing for security exceptions or 'terrorism' carve-outs; populations in conflict zones whose voices are mediated through state or NGO channels rather than heard directly in treaty bodies.
% DISAPPEARANCE_RATIONALE: The conventions provide the only universal legal architecture for humanitarian protection in war; without them, state practice would regress to unrestricted violence against protected categories, detainee treatment would collapse to the captor's discretion, and irregular warfare would lack even minimal legal restraints.
% FOUNDING_PROBLEM: The absence of universal legal restraints on state violence against civilians and captured combatants, exposed by WWII atrocities and the failure of prior Hague law to protect non-combatants in total war.
% FOUNDING_PROBLEM_CORROBORATION: ICRC historical records, Nuremberg Tribunal judgments, UN Charter preamble, and contemporary state practice in human rights treaty bodies all corroborate the founding problem from outside the beneficiary set.
narrative_ontology:disappearance_verdict(geneva_conventions_1949__humanitarian_ceiling_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_1949__humanitarian_ceiling_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__humanitarian_ceiling_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(geneva_conventions_1949__humanitarian_ceiling_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_1949__humanitarian_ceiling_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__humanitarian_ceiling_reading_tests).
:- end_tests(geneva_conventions_1949__humanitarian_ceiling_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) reflects real but bounded costs to state militaries: operational constraints, legal exposure, and asymmetric compliance. Suppression (0.60) is significant — security justifications for violating core protections are structurally excluded — but not total (military necessity operates within the floor). Theater ratio (0.25) is low-moderate: compliance is substantially functional (ICRC visits, prisoner exchanges, distinction in targeting) though performative rhetoric exists. Accessibility collapse (0.60) and resistance (0.50) are moderate: alternatives (unrestricted warfare) are legally foreclosed but practically available; states resist selectively while largely complying. Measurements track post-1949 evolution: Additional Protocols (1977), ad hoc tribunals (1993), post-9/11 stress (2001), contemporary asymmetric conflict (2011, 2024).
 *
 * PERSPECTIVAL GAP:
 *   From the state military seat, the constraint appears as asymmetric extraction: they pay in blood and treasure for protections adversaries violate with impunity. From the civilian/detainee seat, it appears as the only shield against annihilation — a coordination function that works precisely because it is non-reciprocal. From the ICRC seat, it appears as a living legal architecture requiring constant interpretation to maintain relevance. The engine computes this divergence; the claimed type (tangled_rope) reflects the structural reality that both coordination and asymmetric extraction are simultaneously true.
 *
 * DIRECTIONALITY LOGIC:
 *   State militaries are the primary payers (d near target end): they bear compliance costs, accept operational constraints adversaries ignore, and face enforcement. Civilian populations, detainees, and irregular combatants are beneficiaries (d near beneficiary end): they receive protections they cannot enforce and did not negotiate. ICRC/monitoring bodies are agenda_setters (d analytical): they administer the constraint but do not bear its costs or receive its protections. Non-state armed groups are dual payer/excluded: formally bound but structurally excluded from the reciprocity that states claim justifies the regime. The engine computes per-seat effective extraction from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (WWII atrocities, no universal civilian protection) remains live — armed conflict persists, civilians and detainees still need protection. No mandatrophy: the convention's function has not atrophied; its core coordination problem is unsolved. The asymmetric burden on states is not extraction for its own sake but the price of a coordination function that cannot be reciprocal without collapsing. The reading prevents mislabeling this coordination as pure extraction by insisting on the non-derogable floor: without it, the constraint becomes a snare (states extract from each other); with it, the constraint is a tangled rope (coordination with asymmetric cost).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_kernel_reading,
    'How does this reading''s structural classification change if the kernel''s other readings (conditional_reciprocity, security_maximization) are treated as co-constitutive of the convention''s actual operation rather than competing interpretations?',
    'Comparative analysis of state practice: do states act as if protections are absolute (this reading), conditional (reciprocity reading), or security-contingent (maximization reading)? The dominant practice pattern determines which reading captures the operative constraint.',
    'If state practice aligns with conditional_reciprocity or security_maximization, this reading''s claimed tangled_rope (coordination + asymmetric extraction) may describe an aspirational norm rather than the operative constraint — the real constraint would be a different reading with different ε and beneficiary structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Whether the humanitarian ceiling reading describes the operative constraint or an aspirational interpretation.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.60) primarily structural (legal prohibition, monitoring, prosecution risk) or internalized (military professionalization, normative internalization by officer corps)?',
    'Post-compliance trajectory analysis: if suppression persists after enforcement mechanisms weaken (e.g., ICC jurisdiction gaps), internalization is significant. Compare compliance in conflicts with vs. without active monitoring.',
    'If substantially internalized, the constraint''s effective suppression is higher than structural measures suggest — the constraint survives enforcement decay. If primarily structural, suppression tracks enforcement capacity and the constraint is more fragile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in state military compliance.').

omega_variable(
    extraction_boundary_asymmetric_warfare,
    'Does the asymmetric burden on state militaries in asymmetric conflicts (where non-state groups systematically violate conventions) constitute extraction beyond the coordination function''s inherent cost, or is it the necessary price of maintaining a universal floor?',
    'Counterfactual cost analysis: compare compliance costs in symmetric vs. asymmetric conflicts; assess whether the marginal cost in asymmetric settings exceeds the coordination function''s inherent transaction cost (Boltzmann floor for enforcement_mechanism coordination type).',
    'If marginal cost exceeds coordination floor, the constraint extracts surplus from state militaries in asymmetric settings — strengthening the tangled_rope classification. If not, the asymmetry is inherent to the coordination problem, not extractive overhead.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_boundary_asymmetric_warfare, conceptual, 'Whether asymmetric warfare compliance burden is extractive surplus or coordination necessity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__humanitarian_ceiling_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1949, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 1949, 0.15).
narrative_ontology:measurement(gene_tr_t1977, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 1977, 0.18).
narrative_ontology:measurement(gene_tr_t1993, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 1993, 0.2).
narrative_ontology:measurement(gene_tr_t2001, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 2001, 0.22).
narrative_ontology:measurement(gene_tr_t2011, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 2011, 0.24).
narrative_ontology:measurement(gene_tr_t2024, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(gene_be_t1949, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 1949, 0.35).
narrative_ontology:measurement(gene_be_t1977, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 1977, 0.4).
narrative_ontology:measurement(gene_be_t1993, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 1993, 0.42).
narrative_ontology:measurement(gene_be_t2001, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 2001, 0.44).
narrative_ontology:measurement(gene_be_t2011, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 2011, 0.45).
narrative_ontology:measurement(gene_be_t2024, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1949, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 1949, 0.45).
narrative_ontology:measurement(gene_su_t1977, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 1977, 0.5).
narrative_ontology:measurement(gene_su_t1993, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 1993, 0.55).
narrative_ontology:measurement(gene_su_t2001, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 2001, 0.58).
narrative_ontology:measurement(gene_su_t2011, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 2011, 0.59).
narrative_ontology:measurement(gene_su_t2024, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__humanitarian_ceiling_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(geneva_conventions_1949__humanitarian_ceiling_reading, 0.12).
narrative_ontology:affects_constraint(geneva_conventions_1949__humanitarian_ceiling_reading, geneva_conventions_1949__conditional_reciprocity_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__humanitarian_ceiling_reading, geneva_conventions_1949__security_maximization_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__humanitarian_ceiling_reading, additional_protocols_1977).
narrative_ontology:affects_constraint(geneva_conventions_1949__humanitarian_ceiling_reading, icc_rome_statute).

% DUAL FORMULATION NOTE:
% This reading and its siblings form the geneva_conventions_1949 constraint family. The ε-invariance principle requires separate stories: this reading (absolute minimums, ε≈0.45) has substantially lower extraction than security_maximization_reading (which permits degradation, ε≈0.70) but higher coordination integrity than conditional_reciprocity_reading (which makes protections contingent, ε≈0.55 with reciprocity-conditional coordination). The family is linked by shared treaty text but diverges on the structural relationship between reciprocity and protection.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(geneva_conventions_1949__humanitarian_ceiling_reading, institutional, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
