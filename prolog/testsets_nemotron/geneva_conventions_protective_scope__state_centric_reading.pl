% ============================================================================
% CONSTRAINT STORY: geneva_conventions_protective_scope__state_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_protective_scope__state_centric_reading, []).

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
 *   constraint_id: geneva_conventions_protective_scope__state_centric_reading
 *   human_readable: State-Centric Reading of Geneva Protective Scope (Art. 4 GC III)
 *   domain: international_humanitarian_law
 *
 * SUMMARY:
 *   This constraint story captures the state-centric reading of Geneva
 *   Convention III Article 4's protective scope — the interpretation that
 *   limits POW status and combatant immunity to uniformed members of state
 *   armed forces (or assimilated groups) under responsible command with fixed
 *   distinctive signs and open carrying of arms. Unprivileged belligerents
 *   (non-state armed group members, francs-tireurs, 'unlawful combatants')
 *   fall outside the treaty's protective regime and may be targeted and
 *   detained without the privileges of combatant status. The reading is
 *   structurally extractive: it coordinates lawful warfare among states (the
 *   rope function) while extracting protections from non-state actors and
 *   civilians in asymmetric conflicts (the snare function). Active
 *   enforcement is required — states must actively deny status, maintain
 *   detention frameworks, and suppress alternative interpretations through
 *   military doctrine and domestic law. The theater ratio reflects the
 *   growing gap between the coordination justification (clear rules for
 *   conventional war) and the extraction reality (the reading's expansion
 *   into conflicts it was not designed for).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__state_centric_reading, 0.68).
domain_priors:suppression_score(geneva_conventions_protective_scope__state_centric_reading, 0.72).
domain_priors:theater_ratio(geneva_conventions_protective_scope__state_centric_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__state_centric_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__state_centric_reading, "State-Centric Reading of Geneva Protective Scope (Art. 4 GC III)").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__state_centric_reading, "international_humanitarian_law").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__state_centric_reading, '002025a5-7709-44f1-96ee-16902cb3fdc5').
narrative_ontology:cs_kernel_codification('002025a5-7709-44f1-96ee-16902cb3fdc5', formalized).
narrative_ontology:cs_authority_grounding('002025a5-7709-44f1-96ee-16902cb3fdc5', lineage).
narrative_ontology:cs_interpretation_layer_present('002025a5-7709-44f1-96ee-16902cb3fdc5').
narrative_ontology:cs_reading_relation('002025a5-7709-44f1-96ee-16902cb3fdc5', geneva_conventions_protective_scope__universal_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('002025a5-7709-44f1-96ee-16902cb3fdc5', geneva_conventions_protective_scope__hybrid_proportionality_reading, influences).
narrative_ontology:cs_axiom('002025a5-7709-44f1-96ee-16902cb3fdc5', foundational, combatant_privilege_requires_formal_status).
narrative_ontology:cs_axiom_status(combatant_privilege_requires_formal_status, holdable).
narrative_ontology:cs_axiom_grounding('002025a5-7709-44f1-96ee-16902cb3fdc5', combatant_privilege_requires_formal_status, conventional).
narrative_ontology:cs_axiom('002025a5-7709-44f1-96ee-16902cb3fdc5', foundational, state_monopoly_on_legitimate_force_grounds_ihl).
narrative_ontology:cs_axiom_status(state_monopoly_on_legitimate_force_grounds_ihl, holdable).
narrative_ontology:cs_axiom_grounding('002025a5-7709-44f1-96ee-16902cb3fdc5', state_monopoly_on_legitimate_force_grounds_ihl, conventional).
narrative_ontology:cs_reference_frame('002025a5-7709-44f1-96ee-16902cb3fdc5', id_1949_geneva_convention_iii_article_4_formal_status_threshold).
narrative_ontology:cs_drift_state('002025a5-7709-44f1-96ee-16902cb3fdc5', contemporary_asymmetric_conflict_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('002025a5-7709-44f1-96ee-16902cb3fdc5', '').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__state_centric_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__state_centric_reading, conventional_state_militaries).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__state_centric_reading, state_legal_advisors).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__state_centric_reading, non_state_armed_group_members).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__state_centric_reading, civilian_populations_in_asymmetric_conflicts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and enforce the combatant status threshold through military doctrine, rules of engagement, and domestic implementing legislation. Benefit from the ability to target unprivileged belligerents without granting POW protections or combatant immunity. Their legal advisors author the interpretive guidance that narrows Article 4 application.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, conventional_state_militaries, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__state_centric_reading, conventional_state_militaries, beneficiary).

% Produce the legal opinions and targeting guidance that operationalize the state-centric reading. Their professional standing and institutional access depend on producing analyses that validate state military discretion. They rotate between government service, academia, and international tribunals.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, state_legal_advisors, beneficiary,
    organized, biographical, mobile, national).

% Fight in conflicts where they cannot meet Article 4 criteria (no uniforms, no responsible command, no fixed distinctive sign). Captured members face trial as criminals rather than POW status; killed members are treated as lawful targets without combatant immunity. No realistic path to acquire state recognition mid-conflict.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, non_state_armed_group_members, payer,
    powerless, immediate, trapped, local).

% Bear the humanitarian costs when the protective scope narrows: state forces apply looser targeting rules in areas where non-state actors operate, distinction becomes harder to observe, and the 'unprivileged belligerent' category blurs into collective suspicion of military-age males. Displacement, arbitrary detention, and extrajudicial killing rise when the legal floor drops.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, civilian_populations_in_asymmetric_conflicts, payer,
    powerless, biographical, constrained, regional).

% Monitors compliance, visits detainees, and promotes the broadest protective reading. Its mandate requires engaging all parties, but its access depends on state consent. Publishes the Commentaries that articulate the hybrid/universal readings as the institutional position.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, international_committee_of_the_red_cross, observer,
    institutional, generational, analytical, global).

% Investigate and prosecute war crimes where the protective scope is contested. Their charging decisions implicitly adopt a reading: treating non-state fighters as civilians entitled to protection (universal reading) versus treating them as lawful targets when directly participating (state-centric reading). Jurisdictional limits mean they only reach non-party states via UNSC referral.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, international_criminal_court_prosecutors, observer,
    institutional, generational, analytical, global).

% Would claim combatant status for their fighters if the framework allowed it, but are structurally excluded from the treaty-making process and the interpretive community. Their only leverage is reciprocal treatment of captured state soldiers — a fragile dynamic that collapses when the state denies their standing entirely.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, non_state_armed_group_leadership, excluded,
    moderate, biographical, identity_locked, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, administrable threshold for distinguishing lawful combatants (who receive POW protections and combatant immunity) from those who do not, enabling state militaries to operate with legal certainty in conventional wars.
% TRANSFER_FUNCTION: Transfers legal protection and immunity from non-state actors and conflict-affected civilians to state militaries, who gain operational freedom to target and detain without the procedural constraints that POW status would impose.
% ABSENT_VOICES: Non-state armed group leadership and the civilian populations living under their control are excluded from the interpretive community that defines Article 4. They would argue for a functional rather than formal test of combatant status, but have no seat at the diplomatic conferences or in the military legal academies that produce the dominant reading.
% DISAPPEARANCE_RATIONALE: If the state-centric reading vanished overnight, state militaries would lose the legal basis for denying POW status to captured non-state fighters, requiring either extension of Geneva protections or creation of a new detention framework. Targeting rules would tighten. The entire legal architecture of the 'war on terror' (unlawful combatant designation, military commissions, Guantanamo) would lose its doctrinal anchor.
% FOUNDING_PROBLEM: The 1949 Diplomatic Conference needed to define who qualifies as a prisoner of war after WWII revealed the 1929 Convention's gaps: irregular forces, resistance movements, and levy en masse created ambiguity about who was entitled to combatant immunity and who could be punished for mere participation.
% FOUNDING_PROBLEM_CORROBORATION: The ICRC Commentaries (1952, 1960, 2020) attest the founding problem was conventional interstate war, not asymmetric conflict. State military manuals (US DoD Law of War Manual 2015, UK Manual 2004) attest the problem persists but has shifted to non-state actors. Human rights bodies (UN Special Rapporteurs, ECtHR in Al-Skeini, Hassan v UK) attest the original problem is substantially solved for conventional war and the reading now serves a different function.
narrative_ontology:disappearance_verdict(geneva_conventions_protective_scope__state_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_protective_scope__state_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_protective_scope__state_centric_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(geneva_conventions_protective_scope__state_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_protective_scope__state_centric_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_protective_scope__state_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_protective_scope__state_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_protective_scope__state_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is substantial because the reading denies protections to a growing share of actual conflict participants while lowering legal costs for state military operations. Suppression (0.72) is high because maintaining the narrow reading requires active legal and military effort: status determinations, military commissions, diplomatic resistance to AP I ratification, and the practical exclusion of non-state actors from the interpretive community. Theater ratio (0.38) has risen over time as the reading's application to asymmetric conflicts — where Article 4's criteria are structurally unmeetable by non-state groups — becomes a larger share of its operational domain. Accessibility collapse (0.42) is moderate: alternatives exist (AP I Art. 44, human rights law, functional combatant tests) but are politically foreclosed for major military powers. Resistance (0.55) is significant: the ICRC, human rights bodies, and some domestic courts push back, but state practice remains dominant.
 *
 * PERSPECTIVAL GAP:
 *   From the state military seat, this is a genuine coordination mechanism — clear rules for conventional war, preventing the chaos of status ambiguity. From the non-state fighter seat, it is a snare — a rule designed by their adversary that denies them the protections their adversary claims. From the civilian seat, it is a tangled rope — some coordination benefit (clearer distinction in conventional war) but substantial extraction (looser targeting in their areas). The engine computes these divergences from the structural data; the claimed_type (tangled_rope) reflects the authoring seat's assessment that both functions are real and inseparable.
 *
 * DIRECTIONALITY LOGIC:
 *   Conventional state militaries are structural beneficiaries (d near 0.0): they write the rules, control the status determinations, and collect the operational freedom. State legal advisors are secondary beneficiaries (d ~ 0.2): their professional ecosystem depends on the reading's dominance. Non-state armed group members are full targets (d near 1.0): trapped, identity-locked to their role, no exit from the extraction. Conflict-affected civilians are constrained payers (d ~ 0.7): they bear collateral consequences but have slightly more mobility than fighters. The ICRC and ICC prosecutors are analytical observers (d = 0.5): they see the full structure but lack enforcement power. Non-state leadership is excluded (d undefined): their structural position would be payer if admitted, but the reading's coherence depends on their exclusion.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (status clarity in conventional interstate war) is contested: substantially solved for its original domain but the reading persists and expands into asymmetric conflicts where it was never tested. The coordination function (clear combatant threshold) is real but has been stretched to cover conflicts where the threshold is structurally exclusionary. The extraction function (denying protections to non-state actors) has become the reading's primary operational effect in contemporary conflict. This is a classic mandatrophy pattern: a coordination mechanism whose original problem has shifted, now maintained because it benefits the powerful actors who administer it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_44_ap1_displacement,
    'Does AP I Article 44 (combatant status for irregular forces in IAC) structurally displace the state-centric reading for conflicts where it applies, or has state practice hollowed out Article 44 through non-ratification and persistent objection?',
    'Comparative analysis of state practice in AP I party vs. non-party conflicts involving non-state actors; ICJ/ICC jurisprudence on Article 44''s customary status.',
    'If Article 44 is customary and operational, the state-centric reading''s domain shrinks to non-AP-I conflicts (mostly NIAC and conflicts with non-party states). If hollowed out, the reading''s domain remains global.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_44_ap1_displacement, empirical, 'Whether the primary treaty alternative to the state-centric reading has been displaced or neutralized.').

omega_variable(
    functional_vs_formal_combatant_test,
    'Would a functional combatant test (effective command, actual distinction from civilians, sustained military operations) produce a different protective scope than the formal Article 4 criteria, and is the formal test maintained because it produces the narrower scope?',
    'Counterfactual analysis: apply functional criteria to recent asymmetric conflicts (Afghanistan, Iraq, Syria, Yemen) and compare resulting protected populations vs. formal criteria outcomes.',
    'If functional test significantly expands protections, the formal test''s persistence is evidence of extraction-motivated maintenance. If outcomes converge, the formal test is a genuine coordination mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(functional_vs_formal_combatant_test, conceptual, 'Whether the formal Article 4 criteria serve as a proxy for functional distinction or as a designed exclusion mechanism.').

omega_variable(
    cs_framing_kernel_vs_reading,
    'Does the Geneva protective scope kernel admit a single authoritative reading (making this reading the kernel itself), or is the kernel inherently multivalent with this reading as one instantiation?',
    'Analyze the 1949 drafting history: did the delegates intend a fixed scope (kernel = state_centric_reading) or a framework for evolving interpretation (kernel = multivalent)?',
    'If the kernel is fixed, this reading is the kernel and siblings are deviations. If multivalent, this reading is one of several legitimate instantiations and the engine''s per-seat computation is the correct analytic level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_kernel_vs_reading, conceptual, 'Whether the Geneva protective scope is a fixed kernel or a contested kernel generating multiple readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__state_centric_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1949, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 1949, 0.15).
narrative_ontology:measurement(gene_tr_t1977, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 1977, 0.22).
narrative_ontology:measurement(gene_tr_t2001, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 2001, 0.31).
narrative_ontology:measurement(gene_tr_t2010, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 2010, 0.35).
narrative_ontology:measurement(gene_tr_t2024, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(gene_be_t1949, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 1949, 0.45).
narrative_ontology:measurement(gene_be_t1977, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 1977, 0.55).
narrative_ontology:measurement(gene_be_t2001, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 2001, 0.62).
narrative_ontology:measurement(gene_be_t2010, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(gene_be_t2024, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1949, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 1949, 0.45).
narrative_ontology:measurement(gene_su_t1977, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 1977, 0.58).
narrative_ontology:measurement(gene_su_t2001, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 2001, 0.68).
narrative_ontology:measurement(gene_su_t2010, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(gene_su_t2024, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_protective_scope__state_centric_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(geneva_conventions_protective_scope__state_centric_reading, 0.12).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__state_centric_reading, geneva_conventions_protective_scope__universal_rights_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__state_centric_reading, geneva_conventions_protective_scope__hybrid_proportionality_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__state_centric_reading, non_international_armed_conflict_threshold).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__state_centric_reading, direct_participation_in_hostilities_interpretation).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__state_centric_reading, unlawful_combatant_designation_post_9_11).

% DUAL FORMULATION NOTE:
% Part of the geneva_conventions_protective_scope constraint family. This reading (state_centric) narrows the protective scope relative to universal_rights_reading and hybrid_proportionality_reading. The three readings share the kernel (Geneva Conventions protective scope) but instantiate different constraints with different beneficiary/victim structures and extractiveness values. The state_centric_reading is the upstream constraint: its dominance in state practice and military doctrine creates the baseline from which the other readings must argue deviation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(geneva_conventions_protective_scope__state_centric_reading, institutional, 0.1).
constraint_indexing:directionality_override(geneva_conventions_protective_scope__state_centric_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
