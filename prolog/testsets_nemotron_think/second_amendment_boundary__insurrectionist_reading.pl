% ============================================================================
% CONSTRAINT STORY: second_amendment_boundary__insurrectionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_boundary__insurrectionist_reading, []).

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
 *   constraint_id: second_amendment_boundary__insurrectionist_reading
 *   human_readable: Second Amendment Insurrectionist Reading — Armed Resistance as Constitutional Purpose
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   The insurrectionist reading of the Second Amendment holds that the
 *   right's core purpose is preserving the capacity for armed resistance
 *   against tyrannical government. Individual possession is instrumental to
 *   this collective overthrow capacity, not an end in itself. This reading
 *   logically extends protection to military-grade arms (the tools of
 *   effective resistance) and treats state disarmament efforts as precursors
 *   to tyranny. It creates a structural arrangement where armed citizens
 *   claim deterrent legitimacy against the state, while the state security
 *   apparatus and civilians bear the extracted costs — constrained
 *   operational doctrine for the former, existential risk for the latter. The
 *   reading is advanced by an organized movement (gun rights organizations)
 *   that has achieved significant doctrinal victories (Heller, Bruen) but has
 *   not yet secured the full insurrectionist logic in binding precedent. The
 *   constraint operates through judicial enforcement, political mobilization,
 *   and identity fusion among adherents.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__insurrectionist_reading, 0.78).
domain_priors:suppression_score(second_amendment_boundary__insurrectionist_reading, 0.82).
domain_priors:theater_ratio(second_amendment_boundary__insurrectionist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__insurrectionist_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_boundary__insurrectionist_reading, "Second Amendment Insurrectionist Reading — Armed Resistance as Constitutional Purpose").
narrative_ontology:topic_domain(second_amendment_boundary__insurrectionist_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_boundary__insurrectionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__insurrectionist_reading, 'e5ed3d4f-23ba-4b60-a977-e2414115afe0').
narrative_ontology:cs_kernel_codification('e5ed3d4f-23ba-4b60-a977-e2414115afe0', fixed_text).
narrative_ontology:cs_authority_grounding('e5ed3d4f-23ba-4b60-a977-e2414115afe0', lineage).
narrative_ontology:cs_interpretation_layer_present('e5ed3d4f-23ba-4b60-a977-e2414115afe0').
narrative_ontology:cs_reading_relation('e5ed3d4f-23ba-4b60-a977-e2414115afe0', second_amendment_boundary__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('e5ed3d4f-23ba-4b60-a977-e2414115afe0', second_amendment_boundary__militia_conditioned_reading, forecloses).
narrative_ontology:cs_axiom('e5ed3d4f-23ba-4b60-a977-e2414115afe0', foundational, armed_resistance_against_tyranny_is_constitutional_purpose).
narrative_ontology:cs_axiom_status(armed_resistance_against_tyranny_is_constitutional_purpose, holdable).
narrative_ontology:cs_axiom_grounding('e5ed3d4f-23ba-4b60-a977-e2414115afe0', armed_resistance_against_tyranny_is_constitutional_purpose, deontological).
narrative_ontology:cs_axiom('e5ed3d4f-23ba-4b60-a977-e2414115afe0', foundational, military_grade_arms_protected_for_insurrectionary_capacity).
narrative_ontology:cs_axiom_status(military_grade_arms_protected_for_insurrectionary_capacity, holdable).
narrative_ontology:cs_axiom_grounding('e5ed3d4f-23ba-4b60-a977-e2414115afe0', military_grade_arms_protected_for_insurrectionary_capacity, empirically_contingent).
narrative_ontology:cs_reference_frame('e5ed3d4f-23ba-4b60-a977-e2414115afe0', founding_era_armed_populace_deterrent).
narrative_ontology:cs_drift_state('e5ed3d4f-23ba-4b60-a977-e2414115afe0', post_bruen_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e5ed3d4f-23ba-4b60-a977-e2414115afe0', '').
narrative_ontology:cs_kernel_id(second_amendment_boundary__insurrectionist_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__insurrectionist_reading, armed_citizens_claiming_deterrent_legitimacy).
narrative_ontology:constraint_victim(second_amendment_boundary__insurrectionist_reading, state_security_apparatus).
narrative_ontology:constraint_victim(second_amendment_boundary__insurrectionist_reading, civilians_in_hypothetical_armed_conflict).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(second_amendment_boundary__insurrectionist_reading, federal_legislature).
narrative_ontology:constraint_vindicates(second_amendment_boundary__insurrectionist_reading, insurrectionist_second_amendment).
narrative_ontology:constraint_vindicates(second_amendment_boundary__insurrectionist_reading, armed_resistance_as_constitutional_deterrent).
narrative_ontology:constraint_vindicates(second_amendment_boundary__insurrectionist_reading, military_grade_arms_protected_for_overthrow_capacity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organize around the insurrectionist interpretation as core to their political identity and self-conception as guardians of liberty. They advocate for this reading through litigation, lobbying, and cultural production. The reading protects their access to military-grade arms and legitimizes their deterrent posture. Exit would require abandoning a fused identity — 'armed citizen' is who they are, not just what they do.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, armed_citizens_claiming_deterrent_legitimacy, beneficiary,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_boundary__insurrectionist_reading, armed_citizens_claiming_deterrent_legitimacy, agenda_setter).

% Military, federal law enforcement, and national guard forces that would face armed resistance if the insurrectionist premise were activated. The reading constrains their operational doctrine (cannot assume monopoly on force) and treats their disarmament efforts as tyranny precursors. They cannot exit the constraint — it defines the threat environment they must plan against — but they exercise institutional power to shape its judicial boundaries.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, state_security_apparatus, payer,
    institutional, generational, constrained, national).

% Non-combatant populations who would bear collateral damage, displacement, and societal breakdown if insurrectionary violence materialized. They have no voice in the constitutional interpretation that creates this risk, no organized representation in the doctrine's adjudication, and no exit from the geographic space where conflict would occur. Their victimization is structural — the reading's logic makes their suffering an accepted cost of the deterrent.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, civilians_in_hypothetical_armed_conflict, payer,
    powerless, immediate, trapped, local).

% Advocacy groups, public health researchers, and legislators seeking comprehensive firearms regulation. They are structurally excluded from the insurrectionist reading's internal logic — the reading treats their regulatory goals as tyranny-adjacent, not as legitimate policy disagreement. They participate in the broader political contest but cannot gain traction within the reading's own framework.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, gun_control_advocates, excluded,
    organized, biographical, constrained, national).

% Academic historians, legal theorists, and originalist scholars who analyze the historical record and doctrinal evolution. They do not collect rents from the constraint nor bear its direct costs, but their work shapes the intellectual environment in which courts and movements operate. Their exit is analytical — they can change frameworks without personal cost.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, universal).

% The nine justices who authoritatively adjudicate the Second Amendment's meaning. They set the binding constraint through precedent (Heller, McDonald, Bruen, Rahimi). Their individual judicial philosophies determine whether the insurrectionist reading gains doctrinal foothold. They face no personal extraction from the constraint and can arbitrage between interpretive methodologies.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, supreme_court_justices, agenda_setter,
    institutional, generational, arbitrage, national).

% Congress, which seeks to enact firearms regulations (background checks, assault weapons bans, trafficking laws) but operates under the constraint of judicial review informed by this reading. The insurrectionist reading raises the constitutional threshold for permissible regulation, extracting legislative capacity. They cannot exit the constraint — it is the supreme law — but they can attempt to shift judicial composition or amend the Constitution.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, federal_legislature, payer,
    institutional, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates armed citizens around a shared constitutional narrative that legitimizes possession of military-grade arms as a deterrent against tyrannical government, solving the collective-action problem of credible resistance capacity.
% TRANSFER_FUNCTION: Transfers regulatory authority over military-grade arms from the state to armed citizens; transfers the risk of insurrectionary violence and societal breakdown to civilians and state security forces; transfers interpretive authority over 'tyranny' from democratic institutions to armed citizen judgment.
% ABSENT_VOICES: Gun control advocates, communities disproportionately affected by gun violence (particularly urban Black and Latino communities), international human rights bodies, and public health researchers are structurally excluded from the insurrectionist framework's internal logic — the reading treats their regulatory goals as tyranny-adjacent rather than legitimate policy disagreement. They appear in the broader political contest but have no seat in the reading's own constitutive narrative.
% DISAPPEARANCE_RATIONALE: If the insurrectionist reading vanished overnight, the constitutional barrier to comprehensive firearms regulation would collapse. Military-grade arms restrictions would become presumptively valid, the 'tyranny precursor' frame for disarmament efforts would dissolve, and the deterrent threat that structures state-citizen power relations would evaporate. The regulatory landscape, law enforcement doctrine, and civilian risk profile would fundamentally reorganize.
% FOUNDING_PROBLEM: The founding generation's fear of standing armies and federal tyranny, rooted in English history (Stuart-era disarmament, Blackstone's right of resistance) and revolutionary experience — the problem of how a free people preserve the capacity to overthrow a government that becomes tyrannical when that government controls the military.
% FOUNDING_PROBLEM_CORROBORATION: Historical scholarship outside the gun rights movement (Cornell, Rakove, Waldman) corroborates the founding-era fear of standing armies but contests the individual insurrectionist reading as ahistorical — the militia clause was understood as collective, not individual. The gun rights movement's own scholars (Halbrook, Kopel) attest the insurrectionist purpose is live and textually grounded. No consensus exists; the corroboration split maps onto the ideological divide.
narrative_ontology:disappearance_verdict(second_amendment_boundary__insurrectionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_boundary__insurrectionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__insurrectionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_boundary__insurrectionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_boundary__insurrectionist_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_boundary__insurrectionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_boundary__insurrectionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_boundary__insurrectionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the reading transfers substantial regulatory authority from the state to armed citizens and imposes catastrophic risk on non-consenting civilians. Suppression is higher (0.82) because the reading's persistence depends on actively suppressing regulatory alternatives — not merely opposing them politically but rendering them constitutionally illegitimate through the 'tyranny precursor' frame. Theater ratio is moderate (0.42): the deterrent coordination function is genuine in the reading's internal logic, but a growing share of advocacy activity performs revolutionary rhetoric without operationalizing it (performative militia cosplay, rhetorical escalation untethered from organizational capacity). Accessibility collapse (0.71) reflects that once the insurrectionist premise is accepted, regulatory alternatives collapse — but the premise itself is contested, preventing total collapse. Resistance (0.79) captures the intense political, legal, and cultural opposition from gun control advocates, most legal historians, and the institutional judiciary (which has adopted individual-right but not insurrectionist logic).
 *
 * PERSPECTIVAL GAP:
 *   From the armed citizen seat, the reading is a Rope — genuine coordination solving the tyrant-resistance problem. From the civilian seat, it is a Snare — pure extraction of safety with no coordination benefit. From the state security seat, it is a Tangled Rope — coordination (clearer rules of engagement) mixed with extraction (loss of disarmament authority). The engine computes this divergence from the structural data; the authored claim (tangled_rope) represents the generating model's structural assessment.
 *
 * DIRECTIONALITY LOGIC:
 *   Armed citizens claiming deterrent legitimacy are structural beneficiaries (d near 0.15) — they collect the constitutional protection, define the threat environment, and fuse identity to the reading. State security apparatus are payers (d near 0.85) — they bear operational constraints and threat exposure, cannot exit, but retain institutional power to shape doctrine. Civilians in hypothetical conflict are trapped payers (d near 0.95) — zero exit, zero voice, catastrophic downside. Gun control advocates are excluded (d structurally undefined — they are outside the reading's coordinate system). Constitutional scholars are analytical observers (d = 0.5). Supreme Court justices are agenda_setters with arbitrage-grade exit (d near 0.1 — they choose the reading's fate). Federal legislature are constrained payers (d near 0.75) — institutionally powerful but constitutionally bound.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (fear of standing armies/federal tyranny) is contested — historical conditions have changed (standing army is permanent, nuclear deterrent exists, tyranny takes regulatory not military form). The reading persists because the identity-fused beneficiary group (armed citizens) treats the mandate as eternal, and the institutional agenda-setters (courts) have partially incorporated the individual-right half while resisting the insurrectionist half. Mandatrophy is unresolved: the original coordination problem is arguably dead, but the arrangement has acquired new coordination functions (identity cohesion, political mobilization) and new extraction vectors (blocking popular regulations).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_naturalness,
    'Is the insurrectionist reading a Mountain (natural law right of revolution) or a constructed constitutional interpretation?',
    'Comparative constitutional analysis: if analogous insurrectionist provisions in other constitutions (e.g., Honduras, Mexico historical) produce similar doctrinal structures, the reading tracks a transnational natural-law pattern; if unique to US originalism, it is constructed.',
    'If Mountain, the constraint emerges_naturally and FSM does not apply despite beneficiaries. If constructed, the beneficiary declaration triggers false_summit_mountain evaluation and the reading reclassifies as tangled_rope/snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_naturalness, conceptual, 'Whether the insurrectionist premise is a discovered natural right or an invented constitutional theory.').

omega_variable(
    civilian_victim_set_speculative_vs_structural,
    'Is the civilian victim set (collateral damage in hypothetical insurrection) a speculative tail risk or a structural feature of the reading''s logic?',
    'Counterfactual modeling: if the reading were fully instantiated (military-grade arms widely held, tyranny trigger activated), does the logic internally generate civilian casualties as necessary cost, or are they an accidental byproduct?',
    'If structural, the reading is unambiguously extractive (snare/tangled_rope) — civilians are designed-in victims. If speculative, the extraction metric may overstate the reading''s actual operation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civilian_victim_set_speculative_vs_structural, empirical, 'Whether civilian risk is built into the reading''s operational logic or an external contingency.').

omega_variable(
    coordination_extraction_boundary,
    'Is the deterrent coordination function genuine (credible threat stabilizes government behavior) or cover for extraction (rhetorical deterrent masking political power)?',
    'Game-theoretic analysis of state-citizen interaction under the reading: does the armed citizen deterrent actually constrain state action in observable cases, or does the state absorb the threat while extracting compliance through other means?',
    'If genuine coordination, the reading is tangled_rope (coordination + extraction). If cover, it is snare (pure extraction with coordination theater).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the insurrectionist deterrent is operationally real or performative.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.82) structural (judicial doctrine blocking regulation) or internalized (legislators self-censor due to political threat)?',
    'Post-judicial-shift observation: if a Court majority explicitly rejects the insurrectionist reading, does legislative suppression persist (internalized) or relax (structural)?',
    'If internalized, effective suppression exceeds the structural measure — the constraint carries its own enforcement inside political actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the political arena.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__insurrectionist_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sa2_insurrectionist_tr_t0, second_amendment_boundary__insurrectionist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(sa2_insurrectionist_tr_t5, second_amendment_boundary__insurrectionist_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(sa2_insurrectionist_tr_t10, second_amendment_boundary__insurrectionist_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement(sa2_insurrectionist_tr_t15, second_amendment_boundary__insurrectionist_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(sa2_insurrectionist_tr_t20, second_amendment_boundary__insurrectionist_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(sa2_insurrectionist_tr_t25, second_amendment_boundary__insurrectionist_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(sa2_insurrectionist_tr_t30, second_amendment_boundary__insurrectionist_reading, theater_ratio, 30, 0.42).

% Extraction over time
narrative_ontology:measurement(sa2_insurrectionist_be_t0, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(sa2_insurrectionist_be_t5, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(sa2_insurrectionist_be_t10, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(sa2_insurrectionist_be_t15, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(sa2_insurrectionist_be_t20, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 20, 0.71).
narrative_ontology:measurement(sa2_insurrectionist_be_t25, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 25, 0.75).
narrative_ontology:measurement(sa2_insurrectionist_be_t30, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 30, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(sa2_insurrectionist_su_t0, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(sa2_insurrectionist_su_t5, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(sa2_insurrectionist_su_t10, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(sa2_insurrectionist_su_t15, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 15, 0.73).
narrative_ontology:measurement(sa2_insurrectionist_su_t20, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 20, 0.77).
narrative_ontology:measurement(sa2_insurrectionist_su_t25, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 25, 0.8).
narrative_ontology:measurement(sa2_insurrectionist_su_t30, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 30, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__insurrectionist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(second_amendment_boundary__insurrectionist_reading, 0.08).
narrative_ontology:affects_constraint(second_amendment_boundary__insurrectionist_reading, second_amendment_boundary__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__insurrectionist_reading, second_amendment_boundary__militia_conditioned_reading).

% DUAL FORMULATION NOTE:
% Second Amendment kernel family: three readings of the same text. The insurrectionist reading (this story) takes armed overthrow capacity as the constitutional purpose, pulling military-grade arms into protected domain. The individual_right_reading takes self-defense as the purpose, permitting some regulation. The militia_conditioned_reading takes collective defense as the purpose, permitting comprehensive regulation. All three share the fixed text kernel but instantiate different constraints with different ε, beneficiaries, and victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_boundary__insurrectionist_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
