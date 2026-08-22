% ============================================================================
% CONSTRAINT STORY: article_27_veto_power__sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-31
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_27_veto_power__sovereignty_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: article_27_veto_power__sovereignty_reading
 *   human_readable: P5 Veto Power — Sovereignty Reading
 *   domain: international_relations/institutional_design/constitutional_law
 *
 * SUMMARY:
 *   This story instantiates the sovereignty_reading of the
 *   article_27_veto_power kernel. The constraint is the P5 veto as the
 *   institutional form of the Westphalian consent principle applied to states
 *   whose enforcement capacity makes them uncompellable. The veto is not a
 *   privilege granted by the Charter; it is the Charter's recognition of a
 *   pre-institutional reality: no global authority can bind a state that can
 *   physically resist any enforcement mechanism. The reading classifies the
 *   veto as Mountain — a structural inevitability with near-zero extraction —
 *   because any universal-security institution that includes great powers
 *   must either (a) require their consent for binding action or (b) fail to
 *   secure their participation. The coordination_reading and
 *   oligopoly_reading are sibling constraints (separate files) that read the
 *   same kernel differently.
 *
 * KEY AGENTS:
 *   - p5_nuclear_states: Primary agenda_setter (institutional/analytical) — holds the veto as structural expression of enforcement asymmetry
 *   - non_nuclear_states: Observer (organized/constrained) — experiences the veto as structural ceiling on Council action
 *   - international_lawyers_scholars: Observer (analytical/analytical) — distinguishes the three readings of the kernel
 *   - security_council_secretariat: Observer (institutional/analytical) — administers the veto's procedural machinery
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__sovereignty_reading, 0.02).
domain_priors:suppression_score(article_27_veto_power__sovereignty_reading, 0.05).
domain_priors:theater_ratio(article_27_veto_power__sovereignty_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, extractiveness, 0.02).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__sovereignty_reading, mountain).
narrative_ontology:human_readable(article_27_veto_power__sovereignty_reading, "P5 Veto Power — Sovereignty Reading").
narrative_ontology:topic_domain(article_27_veto_power__sovereignty_reading, "international_relations/institutional_design/constitutional_law").

domain_priors:emerges_naturally(article_27_veto_power__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__sovereignty_reading, 'fd92541b-441a-41a1-ae8e-1ce67107897a').
narrative_ontology:cs_kernel_codification('fd92541b-441a-41a1-ae8e-1ce67107897a', formalized).
narrative_ontology:cs_authority_grounding('fd92541b-441a-41a1-ae8e-1ce67107897a', lineage).
narrative_ontology:cs_interpretation_layer_present('fd92541b-441a-41a1-ae8e-1ce67107897a').
narrative_ontology:cs_reading_relation('fd92541b-441a-41a1-ae8e-1ce67107897a', article_27_veto_power__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('fd92541b-441a-41a1-ae8e-1ce67107897a', article_27_veto_power__oligopoly_reading, coexists_with).
narrative_ontology:cs_axiom('fd92541b-441a-41a1-ae8e-1ce67107897a', foundational, no_binding_without_consent_of_enforcement_capable).
narrative_ontology:cs_axiom_status(no_binding_without_consent_of_enforcement_capable, holdable).
narrative_ontology:cs_axiom_grounding('fd92541b-441a-41a1-ae8e-1ce67107897a', no_binding_without_consent_of_enforcement_capable, deontological).
narrative_ontology:cs_axiom('fd92541b-441a-41a1-ae8e-1ce67107897a', foundational, enforcement_asymmetry_necessitates_veto).
narrative_ontology:cs_axiom_status(enforcement_asymmetry_necessitates_veto, holdable).
narrative_ontology:cs_axiom_grounding('fd92541b-441a-41a1-ae8e-1ce67107897a', enforcement_asymmetry_necessitates_veto, empirically_contingent).
narrative_ontology:cs_reference_frame('fd92541b-441a-41a1-ae8e-1ce67107897a', westphalian_consent_under_enforcement_asymmetry).
narrative_ontology:cs_drift_state('fd92541b-441a-41a1-ae8e-1ce67107897a', contemporary_multipolar_era, gap(stable, minor, false)).
narrative_ontology:cs_created_at('fd92541b-441a-41a1-ae8e-1ce67107897a', '2026-07-31T12:00:00Z').
narrative_ontology:cs_kernel_id(article_27_veto_power__sovereignty_reading, article_27_veto_power).

% --- Structural relationships ---
narrative_ontology:constraint_vindicates(article_27_veto_power__sovereignty_reading, westphalian_sovereignty_principle).
narrative_ontology:constraint_vindicates(article_27_veto_power__sovereignty_reading, consent_basis_of_international_law).
narrative_ontology:constraint_vindicates(article_27_veto_power__sovereignty_reading, great_power_enforcement_capacity_asymmetry).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the veto as the structural expression of their enforcement-capacity asymmetry. They do not 'benefit' from the veto in a rent-seeking sense; the veto is the institutional form of the physical fact that no global authority can compel them. Any institution that attempted to bind them without consent would face the same coordination failure — the veto merely names the floor.
narrative_ontology:constraint_stakeholder(article_27_veto_power__sovereignty_reading, p5_nuclear_states, agenda_setter,
    institutional, generational, analytical, global).

% Experience the veto as a structural ceiling on Security Council action. They do not extract from the veto, nor are they targeted by it as a rent-seeking mechanism. They operate within a system whose enforceable commitments are bounded by the consent of the most powerful states — a boundedness that follows from the physics of coercion, not from a designed extraction.
narrative_ontology:constraint_stakeholder(article_27_veto_power__sovereignty_reading, non_nuclear_states, observer,
    organized, biographical, constrained, regional).

% Study the veto as the institutional crystallization of the consent principle under conditions of extreme power asymmetry. Their analysis distinguishes the sovereignty reading (veto as inevitable expression of consent principle + enforcement reality) from the coordination reading (veto as war-prevention mechanism) and the oligopoly reading (veto as rent-extraction entrenchment).
narrative_ontology:constraint_stakeholder(article_27_veto_power__sovereignty_reading, international_lawyers_scholars, observer,
    analytical, generational, analytical, universal).

% Administers the procedural machinery of the veto. Their operations are shaped by the veto's existence but they do not set its terms, collect its rents, or bear its costs as a target. They are the institutional substrate through which the constraint is operationalized.
narrative_ontology:constraint_stakeholder(article_27_veto_power__sovereignty_reading, security_council_secretariat, observer,
    institutional, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the only possible institutional form for a universal-security organization that includes great powers with global-reach enforcement capacity: a body whose binding resolutions require the consent of those powers, because any body that could compel them without consent would fail to secure their participation and thus fail at its coordination task.
% TRANSFER_FUNCTION: Transfers no resources, status, or authority as a designed flow. The veto is not a mechanism that moves value from one party to another; it is the structural expression of the fact that enforcement capacity is asymmetrically distributed. Any apparent 'transfer' (e.g., a resolution blocked) is the coordination function operating — the system correctly refusing to generate a commitment the compelled party would not honor.
% ABSENT_VOICES: States that would prefer a Security Council able to bind great powers without their consent. They are absent because no such council could exist with great-power participation; their preference is structurally unrealizable within a universal-membership security organization.
% DISAPPEARANCE_RATIONALE: If the veto disappeared overnight — i.e., if the Security Council could pass binding resolutions over P5 objections — the P5 would not comply with resolutions that crossed their vital enforcement-capacity interests. The UN would either become a forum without enforcement teeth (resolutions ignored) or fracture as P5 states withdrew from the binding mechanism. The world rearranges because the constraint expresses a physical-enforcement reality, not a designed rule.
% FOUNDING_PROBLEM: How to construct a universal-security institution that includes states whose enforcement capacity exceeds any collective mechanism's ability to compel them. The founding problem is not 'how to prevent war' (coordination reading) or 'how to entrench power' (oligopoly reading) but 'how to institutionalize the consent principle under conditions of radical enforcement asymmetry.'
% FOUNDING_PROBLEM_CORROBORATION: The persistence of the veto through Charter amendment attempts (Article 108/109 require P5 consent), the consistent refusal of P5 states to join any binding dispute-resolution mechanism that could compel them (ICJ optional clause reservations, ICC non-ratification), and the structural failure of any post-1945 proposal for a 'world government' with compulsory jurisdiction over great powers. Corroborated by the historical record of institutional design attempts, not by the veto's beneficiaries.
narrative_ontology:disappearance_verdict(article_27_veto_power__sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_27_veto_power__sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(article_27_veto_power__sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_27_veto_power__sovereignty_reading, 0.02, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_27_veto_power__sovereignty_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, ExtMetricName, E),
    domain_priors:suppression_score(article_27_veto_power__sovereignty_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(article_27_veto_power__sovereignty_reading),
    narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(article_27_veto_power__sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-zero (0.02) because the veto does not extract value from target parties — it prevents the generation of commitments that would not be honored. Suppression is minimal (0.05) because the constraint does not actively suppress alternatives; it defines the boundary of what a universal-security institution can bind. Theater_ratio is low (0.1) because the veto's procedural theater (the casting of vetoes, the debates) is a small fraction of the constraint's operational reality — the real constraint is the enforcement-capacity floor it names. Accessibility_collapse is very high (0.92) because alternatives (compulsory jurisdiction over great powers) collapse completely once the enforcement asymmetry is understood — no institutional design has ever circumvented it. Resistance is near-zero (0.03) because the constraint is not a rule that parties resist; it is a structural fact that institutional designs either accommodate or fail.
 *
 * PERSPECTIVAL GAP:
 *   The coordination_reading would compute this constraint as Rope (war-prevention coordination with modest extraction). The oligopoly_reading would compute it as Tangled Rope or Snare (oligopoly entrenchment with substantial extraction). This reading computes it as Mountain because it takes the enforcement-capacity asymmetry as the primary structural given, not the war-prevention function or the oligopoly benefit. The engine will compute per-seat types from the structural data; the divergence across readings is the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiary/victim framing is declared because the constraint derives from physical enforcement-capacity asymmetry, not from a designed transfer. The P5 states are agenda_setters in the procedural sense (they operate the veto) but they do not 'benefit' in the extraction sense — they simply occupy the structural position that any state with global-reach enforcement capacity would occupy in any universal-security institution. Non-nuclear states are not 'victims' of extraction; they are participants in an institution whose enforceable commitments are bounded by the consent of the most powerful. The directionality derivation chain correctly yields near-symmetric d for all seats because the constraint is not extractive in structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The sovereignty reading prevents mislabeling the veto as pure extraction (Snare) by showing that the veto's persistence does not depend on active suppression of alternatives — alternatives are structurally unavailable. It prevents mislabeling it as coordination-with-rent (Tangled Rope) by showing that the enforcement-capacity floor is not a negotiable design choice but a physical constraint on institutional possibility. The mandate (universal security including great powers) has not outlived its function; the function itself requires the veto. mandatrophy_resolved is false because the founding problem is live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_capacity_floor_vs_designed_privilege,
    'Is the veto''s Mountain classification robust to the possibility that enforcement-capacity asymmetry is a contingent historical fact rather than a permanent structural feature? If great powers lost their enforcement monopoly (e.g., through AI-enabled decentralized coercion), would the veto''s structural necessity dissolve?',
    'Track historical episodes where enforcement asymmetry shifted (nuclear monopoly → parity, conventional superiority → anti-access/area-denial) and assess whether the veto''s operational logic tracked the asymmetry or the Charter text. Model counterfactual institutional designs under reduced asymmetry.',
    'If the veto''s necessity tracks enforcement asymmetry contingently, the Mountain classification holds only for the current regime — the constraint would be a contingent Mountain (Mountain* in the taxonomy), not a necessary one. If the veto persists even when asymmetry declines, the oligopoly_reading gains structural support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_floor_vs_designed_privilege, empirical, 'Whether the veto''s structural necessity is contingent on current enforcement-capacity distribution').

omega_variable(
    consent_principle_universality,
    'Does the Westphalian consent principle (no state bound without consent) apply universally, or only to states with enforcement capacity to make consent effective? If the latter, the sovereignty reading collapses into the oligopoly reading for weak states.',
    'Examine whether weak states are in fact bound by Security Council resolutions without their consent (Chapter VII enforcement). If they are, the consent principle is selectively applied — a structural feature of the oligopoly reading, not the sovereignty reading.',
    'If consent principle applies only to enforcement-capable states, the sovereignty reading''s claim to Mountain status (universal principle) fails — it becomes a special privilege of the powerful, i.e., oligopoly. If consent principle is universal but only enforceable by the powerful, the sovereignty reading holds as Mountain but with a distributive injustice that the oligopoly reading captures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consent_principle_universality, conceptual, 'Whether the consent principle grounding the sovereignty reading is universal or enforcement-conditional').

omega_variable(
    reading_relations_structural_location,
    'Where exactly in the constraint''s structure do the three readings diverge? Is it in ε (extraction level), in the beneficiary/victim structure, in the coordination function, or in the reference frame (what the kernel is taken to be)?',
    'Decompose each reading''s authored metrics and structural declarations. The coordination_reading should show low but non-zero ε and a coordination function (war prevention). The oligopoly_reading should show substantial ε and named beneficiaries (P5 as rent-collectors). The sovereignty_reading shows near-zero ε, no beneficiaries/victims, and a consent-principle reference frame. The divergence location determines whether the readings are truly distinct constraints or framings of one constraint.',
    'If divergence is only in claimed_type with identical metrics, the kernel decomposition is linguistic not structural — one constraint, three framings. If metrics diverge (as authored), the ε-invariance principle is satisfied and the three files are valid distinct constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_relations_structural_location, conceptual, 'Structural locus of divergence among the three kernel readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__sovereignty_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(article_27_veto_power__sovereignty_reading_tr_t1945, article_27_veto_power__sovereignty_reading, theater_ratio, 1945, 0.05).
narrative_ontology:measurement(article_27_veto_power__sovereignty_reading_tr_t1960, article_27_veto_power__sovereignty_reading, theater_ratio, 1960, 0.08).
narrative_ontology:measurement(article_27_veto_power__sovereignty_reading_tr_t1975, article_27_veto_power__sovereignty_reading, theater_ratio, 1975, 0.09).
narrative_ontology:measurement(article_27_veto_power__sovereignty_reading_tr_t1991, article_27_veto_power__sovereignty_reading, theater_ratio, 1991, 0.1).
narrative_ontology:measurement(article_27_veto_power__sovereignty_reading_tr_t2005, article_27_veto_power__sovereignty_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(article_27_veto_power__sovereignty_reading_tr_t2025, article_27_veto_power__sovereignty_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(article_27_veto_power__sovereignty_reading_be_t1945, article_27_veto_power__sovereignty_reading, base_extractiveness, 1945, 0.02).
narrative_ontology:measurement(article_27_veto_power__sovereignty_reading_be_t1960, article_27_veto_power__sovereignty_reading, base_extractiveness, 1960, 0.02).
narrative_ontology:measurement(article_27_veto_power__sovereignty_reading_be_t1975, article_27_veto_power__sovereignty_reading, base_extractiveness, 1975, 0.02).
narrative_ontology:measurement(article_27_veto_power__sovereignty_reading_be_t1991, article_27_veto_power__sovereignty_reading, base_extractiveness, 1991, 0.02).
narrative_ontology:measurement(article_27_veto_power__sovereignty_reading_be_t2005, article_27_veto_power__sovereignty_reading, base_extractiveness, 2005, 0.02).
narrative_ontology:measurement(article_27_veto_power__sovereignty_reading_be_t2025, article_27_veto_power__sovereignty_reading, base_extractiveness, 2025, 0.02).

% Suppression requirement over time
narrative_ontology:measurement(article_27_veto_power__sovereignty_reading_su_t1945, article_27_veto_power__sovereignty_reading, suppression_requirement, 1945, 0.03).
narrative_ontology:measurement(article_27_veto_power__sovereignty_reading_su_t1960, article_27_veto_power__sovereignty_reading, suppression_requirement, 1960, 0.04).
narrative_ontology:measurement(article_27_veto_power__sovereignty_reading_su_t1975, article_27_veto_power__sovereignty_reading, suppression_requirement, 1975, 0.05).
narrative_ontology:measurement(article_27_veto_power__sovereignty_reading_su_t1991, article_27_veto_power__sovereignty_reading, suppression_requirement, 1991, 0.05).
narrative_ontology:measurement(article_27_veto_power__sovereignty_reading_su_t2005, article_27_veto_power__sovereignty_reading, suppression_requirement, 2005, 0.05).
narrative_ontology:measurement(article_27_veto_power__sovereignty_reading_su_t2025, article_27_veto_power__sovereignty_reading, suppression_requirement, 2025, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_27_veto_power__sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_27_veto_power__sovereignty_reading, 0.08).
narrative_ontology:affects_constraint(article_27_veto_power__sovereignty_reading, article_27_veto_power__coordination_reading).
narrative_ontology:affects_constraint(article_27_veto_power__sovereignty_reading, article_27_veto_power__oligopoly_reading).

% DUAL FORMULATION NOTE:
% This is the sovereignty_reading of the article_27_veto_power kernel. The kernel decomposes into three constraint stories: coordination_reading (veto as war-prevention Rope), oligopoly_reading (veto as rent-extracting Tangled Rope/Snare), and sovereignty_reading (veto as Mountain expressing consent principle under enforcement asymmetry). All three share the kernel_id article_27_veto_power. This reading influences both siblings by establishing the enforcement-capacity floor — the coordination_reading's war-prevention function only operates if the floor holds; the oligopoly_reading's rent extraction only persists if the floor is treated as immutable Charter text rather than contingent asymmetry.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
