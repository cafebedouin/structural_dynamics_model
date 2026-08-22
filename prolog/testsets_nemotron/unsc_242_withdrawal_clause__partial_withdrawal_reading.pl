% ============================================================================
% CONSTRAINT STORY: unsc_242_withdrawal_clause__partial_withdrawal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unsc_242_withdrawal_clause__partial_withdrawal_reading, []).

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
 *   constraint_id: unsc_242_withdrawal_clause__partial_withdrawal_reading
 *   human_readable: UNSC 242 Partial Withdrawal Reading — Indefinite Article / Secure Boundaries
 *   domain: international_law/diplomatic_history/treaty_interpretation
 *
 * SUMMARY:
 *   UN Security Council Resolution 242 (1967) calls for 'withdrawal of
 *   Israeli armed forces from territories occupied in the recent conflict' —
 *   the English text uses the indefinite 'territories' while the equally
 *   authentic French text uses the definite 'des territoires'. This reading
 *   treats the English indefiniteness as encoding drafters' intent to permit
 *   discretionary, partial withdrawal, legitimized by the 'secure and
 *   recognized boundaries' principle. The constraint is the standing
 *   interpretive regime that converts this textual ambiguity into a phased
 *   negotiation framework where the occupying power controls the scope and
 *   pace of withdrawal. The reading instantiates a ledger: indefiniteness
 *   becomes negotiating leverage; the occupying state and mediators benefit
 *   from process control; claimants bear the cost of open-ended occupation
 *   without enforcement line.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.55).
domain_priors:suppression_score(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.62).
domain_priors:theater_ratio(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__partial_withdrawal_reading, tangled_rope).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__partial_withdrawal_reading, "UNSC 242 Partial Withdrawal Reading — Indefinite Article / Secure Boundaries").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__partial_withdrawal_reading, "international_law/diplomatic_history/treaty_interpretation").

domain_priors:requires_active_enforcement(unsc_242_withdrawal_clause__partial_withdrawal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__partial_withdrawal_reading, '045c9899-996c-42e6-b23d-13917e4726ff').
narrative_ontology:cs_kernel_codification('045c9899-996c-42e6-b23d-13917e4726ff', fixed_text).
narrative_ontology:cs_authority_grounding('045c9899-996c-42e6-b23d-13917e4726ff', lineage).
narrative_ontology:cs_interpretation_layer_present('045c9899-996c-42e6-b23d-13917e4726ff').
narrative_ontology:cs_reading_relation('045c9899-996c-42e6-b23d-13917e4726ff', unsc_242_withdrawal_clause__maximal_withdrawal_reading, forecloses).
narrative_ontology:cs_reading_relation('045c9899-996c-42e6-b23d-13917e4726ff', unsc_242_withdrawal_clause__interpretive_authority_structure, influences).
narrative_ontology:cs_axiom('045c9899-996c-42e6-b23d-13917e4726ff', foundational, english_text_controls_withdrawal_scope).
narrative_ontology:cs_axiom_status(english_text_controls_withdrawal_scope, holdable).
narrative_ontology:cs_axiom_grounding('045c9899-996c-42e6-b23d-13917e4726ff', english_text_controls_withdrawal_scope, conventional).
narrative_ontology:cs_axiom('045c9899-996c-42e6-b23d-13917e4726ff', foundational, secure_boundaries_permit_strategic_retention).
narrative_ontology:cs_axiom_status(secure_boundaries_permit_strategic_retention, holdable).
narrative_ontology:cs_axiom_grounding('045c9899-996c-42e6-b23d-13917e4726ff', secure_boundaries_permit_strategic_retention, instrumental).
narrative_ontology:cs_reference_frame('045c9899-996c-42e6-b23d-13917e4726ff', id_1967_security_council_consensus).
narrative_ontology:cs_drift_state('045c9899-996c-42e6-b23d-13917e4726ff', post_oslo_settlement_expansion, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('045c9899-996c-42e6-b23d-13917e4726ff', '').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__partial_withdrawal_reading, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_state_israel).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__partial_withdrawal_reading, mediating_states_quartet).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__partial_withdrawal_reading, displaced_palestinian_claimants).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__partial_withdrawal_reading, frontline_arab_states_egypt_jordan_syria).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__partial_withdrawal_reading, frontline_arab_states_egypt_jordan_syria).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains discretionary control over withdrawal scope and timing, leveraging the text's ambiguity to negotiate territorial concessions and security arrangements; collects strategic depth and bargaining leverage from the phased process; exit is structurally open — it controls the enforcement timeline and can shape outcomes.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_state_israel, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_state_israel, agenda_setter).

% Controls the diplomatic process that converts textual ambiguity into negotiated outcomes; collects institutional relevance and geopolitical influence from managing the phased withdrawal; can shift mediation frameworks or disengage if process stalls.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, mediating_states_quartet, agenda_setter,
    institutional, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__partial_withdrawal_reading, mediating_states_quartet, beneficiary).

% Bear the human cost of indefinite occupation without a fixed withdrawal line; their claims to return and self-determination are deferred by the very ambiguity the reading invokes; no exit from the structural condition — no state, no enforcement mechanism, no leverage.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, displaced_palestinian_claimants, payer,
    powerless, generational, trapped, local).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__partial_withdrawal_reading, displaced_palestinian_claimants, excluded).

% Carry the security and demographic burden of unresolved conflict; gain phased normalization and bilateral treaties as partial benefit; exit is constrained by regional power dynamics and dependence on mediating states — can push for implementation but cannot unilaterally enforce withdrawal.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, frontline_arab_states_egypt_jordan_syria, payer,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__partial_withdrawal_reading, frontline_arab_states_egypt_jordan_syria, beneficiary).

% Claims judicial authority to interpret the clause definitively; its advisory opinions and judgments are cited by all parties but lack enforcement power; observes the structural drift from its reference frame without capacity to compel compliance.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, icj_interpretive_authority, observer,
    institutional, civilizational, analytical, universal).

% Authored the English text whose indefiniteness is the reading's foundation; now invoke drafters' intent retrospectively to legitimize the partial withdrawal outcome; hold no current enforcement role but their archival record structures the interpretive field.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, drafting_states_uk_us, observer,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__partial_withdrawal_reading, drafting_states_uk_us, agenda_setter).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts an irreconcilable territorial dispute into a phased, negotiated process where withdrawal extent and security arrangements are traded iteratively — the indefiniteness IS the mechanism that allows parties to stay at the table rather than collapse into all-or-nothing confrontation.
% TRANSFER_FUNCTION: Moves territorial control and security guarantees from claimants to the occupying power in exchange for phased withdrawal commitments and diplomatic recognition; the occupying power retains strategic territories as negotiation capital while claimants receive process without fixed endpoint.
% ABSENT_VOICES: The 1967 displaced Palestinian population and their descendants — who would demand full withdrawal to the June 4 lines as a precondition for any settlement — are structurally excluded from the negotiation table; their absence is what makes the phased, discretionary withdrawal politically sustainable for the occupying power.
% DISAPPEARANCE_RATIONALE: If the partial withdrawal reading vanished overnight, the maximal withdrawal reading (full withdrawal per French text / Article 2(4)) would become the default legal baseline, forcing immediate diplomatic crisis and likely renewed confrontation; the entire Oslo-era architecture of phased territorial transfer would lose its legal anchor.
% FOUNDING_PROBLEM: The 1967 war created a territorial deadlock: Israel held all captured territories but faced universal non-recognition; Arab states refused direct negotiation; the Security Council needed a formula that could pass unanimously and allow phased disengagement without prejudicing final borders.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the 1967 Security Council record (public debate, voting record) and by diplomatic historians outside the benefiting parties (e.g., William Quandt, Rashid Khalidi, Avi Shlaim) who document the deliberate ambiguity as a consensus-engineering device. The reading's current beneficiaries (Israel, Quartet mediators) assert the problem remains live; claimant representatives and ICJ jurisprudence assert the problem was solved by the maximal reading and the arrangement persists as extraction.
narrative_ontology:disappearance_verdict(unsc_242_withdrawal_clause__partial_withdrawal_reading, world_rearranges).
narrative_ontology:founding_problem_status(unsc_242_withdrawal_clause__partial_withdrawal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unsc_242_withdrawal_clause__partial_withdrawal_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(unsc_242_withdrawal_clause__partial_withdrawal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.55, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unsc_242_withdrawal_clause__partial_withdrawal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unsc_242_withdrawal_clause__partial_withdrawal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unsc_242_withdrawal_clause__partial_withdrawal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.55) because the constraint is conditional and phased — it extracts territorial control and time, not permanent annexation (though settlement expansion blurs this). Suppression is higher (0.62) because the constraint's persistence depends on actively preventing enforcement of the maximal reading: blocking ICJ jurisdiction, vetoing enforcement resolutions, conditioning recognition on process compliance. Theater ratio (0.41) reflects that the 'peace process' machinery performs coordination while the structural drift favors the occupying power's retention of strategic territories. The measurement grid is shared across metrics at six historical inflection points.
 *
 * PERSPECTIVAL GAP:
 *   From the occupying state's seat, the constraint is a rope — genuine coordination converting deadlock into phased disengagement. From the claimants' seat, it is a snare — the coordination story is cover for open-ended extraction. The engine computes this divergence from the structural data; the claimed tangled_rope captures the hybrid reality that both seats experience simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   The occupying state is the structural beneficiary (d near 0.15): it collects strategic depth, settlement latitude, and process control. Mediating states are secondary beneficiaries (d ~0.25): they collect diplomatic relevance. Displaced claimants are full targets (d ~0.95): trapped, identity-locked to the land, no exit. Frontline Arab states are constrained payers (d ~0.65): they pay security/demographic costs but extract normalization benefits. The two observer seats are analytical (d=0.5 by definition).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1967 deadlock) was structurally solved for Egypt (1979) and Jordan (1994) via bilateral treaties — but the arrangement persisted and expanded to new claimants (Palestinians, Syria) without new consent. The mandate has outlived its function for the original parties but was repurposed for new extraction. The engine's mandatrophy_resolved flag should fire for the Egypt/Jordan sub-constraint but not for the Palestinian/Syrian sub-constraint — this reading's ε-invariance requires decomposing by claimant population, which the kernel structure does not do.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    indefiniteness_intentionality,
    'Was the English indefinite article ''territories'' a deliberate drafters'' choice to permit partial withdrawal, or an ambiguity exploited post facto?',
    'Archival research on 1967 drafting record: UK/US delegation instructions, Security Council verbatim records, Lord Caradon''s later statements. If deliberative intent for partial withdrawal is documented, the reading''s coordination claim strengthens; if post-hoc, it is a constructed cover.',
    'If intentional, the constraint has genuine coordination DNA (tangled_rope); if post-hoc, it is a snare with a fabricated coordination story. The ε value would shift downward or upward accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indefiniteness_intentionality, empirical, 'Whether the textual ambiguity was designed as a coordination mechanism or emerged as an extraction enabler.').

omega_variable(
    secure_boundaries_operationalization,
    'Does the ''secure and recognized boundaries'' principle functionally require territorial retention beyond the 1967 lines, or has it become a blank check for strategic annexation?',
    'Compare 1967-1973 security assessments (pre-settlement) with post-1977 settlement-driven boundary claims. If security requirements have expanded with settlement facts rather than preceding them, the principle is retrofitted cover.',
    'If the principle is retrofitted, the coordination function is substantially hollowed — the constraint drifts toward snare. If it tracks genuine security evolution, the tangled_rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(secure_boundaries_operationalization, conceptual, 'Whether the secure boundaries principle operates as a genuine coordination standard or a malleable extraction warrant.').

omega_variable(
    phased_withdrawal_completion_trajectory,
    'Is the phased withdrawal process structurally convergent (approaching a final settlement) or divergent (accumulating facts that preclude final withdrawal)?',
    'Track net territorial transfer to Palestinian control vs. settlement expansion over 1993-2024. If the ledger shows net reversion, the process is convergent; if net retention, it is divergent extraction disguised as process.',
    'A divergent trajectory would reclassify the constraint as snare with mandatrophy_unresolved; a convergent trajectory supports tangled_rope with live founding problem.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(phased_withdrawal_completion_trajectory, empirical, 'Whether the conditional/phased structure is moving toward resolution or entrenching extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__partial_withdrawal_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsc_tr_t1967, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 1967, 0.18).
narrative_ontology:measurement(unsc_tr_t1973, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 1973, 0.24).
narrative_ontology:measurement(unsc_tr_t1978, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 1978, 0.29).
narrative_ontology:measurement(unsc_tr_t1993, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 1993, 0.36).
narrative_ontology:measurement(unsc_tr_t2000, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 2000, 0.4).
narrative_ontology:measurement(unsc_tr_t2024, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 2024, 0.41).

% Extraction over time
narrative_ontology:measurement(unsc_be_t1967, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 1967, 0.22).
narrative_ontology:measurement(unsc_be_t1973, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 1973, 0.31).
narrative_ontology:measurement(unsc_be_t1978, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 1978, 0.38).
narrative_ontology:measurement(unsc_be_t1993, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 1993, 0.46).
narrative_ontology:measurement(unsc_be_t2000, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 2000, 0.52).
narrative_ontology:measurement(unsc_be_t2024, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 2024, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(unsc_su_t1967, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 1967, 0.35).
narrative_ontology:measurement(unsc_su_t1973, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 1973, 0.42).
narrative_ontology:measurement(unsc_su_t1978, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 1978, 0.48).
narrative_ontology:measurement(unsc_su_t1993, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 1993, 0.55).
narrative_ontology:measurement(unsc_su_t2000, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 2000, 0.59).
narrative_ontology:measurement(unsc_su_t2024, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unsc_242_withdrawal_clause__partial_withdrawal_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.12).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__partial_withdrawal_reading, unsc_242_withdrawal_clause__maximal_withdrawal_reading).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__partial_withdrawal_reading, unsc_242_withdrawal_clause__interpretive_authority_structure).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__partial_withdrawal_reading, oslo_accords_phased_transfer).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__partial_withdrawal_reading, arab_peace_initiative_2002).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__partial_withdrawal_reading, icj_wall_advisory_opinion_2004).

% DUAL FORMULATION NOTE:
% This reading and the maximal_withdrawal_reading are ε-distinct constraints linked by the same kernel text. The partial reading's ε (0.55) reflects conditional, phased extraction; the maximal reading's ε would be lower (~0.25) because it denies the occupying power discretionary retention. The interpretive_authority_structure reading is meta-constraint on which ε is authoritative.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unsc_242_withdrawal_clause__partial_withdrawal_reading, institutional, 0.15).
constraint_indexing:directionality_override(unsc_242_withdrawal_clause__partial_withdrawal_reading, powerless, 0.95).
constraint_indexing:directionality_override(unsc_242_withdrawal_clause__partial_withdrawal_reading, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
