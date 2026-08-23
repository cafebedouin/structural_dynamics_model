% ============================================================================
% CONSTRAINT STORY: vatican_ii_authority__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_authority__composite_overdetermination_reading, []).

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
 *   constraint_id: vatican_ii_authority__composite_overdetermination_reading
 *   human_readable: Vatican II Authority as Overdetermined Composite (Composite Overdetermination Reading)
 *   domain: theology/ecclesiology/religious_authority
 *
 * SUMMARY:
 *   This constraint story instantiates the
 *   composite_overdetermination_reading of the vatican_ii_authority kernel.
 *   The standing arrangement under contest is the institutional claim that
 *   Vatican II possesses a single authoritative interpretation (the
 *   'hermeneutic of continuity'). This reading assesses that claim as a
 *   tangled rope: it performs a genuine coordination function (holding the
 *   Church together as a unified doctrinal subject) but does so through
 *   active suppression of the council's irreducibly composite character — the
 *   documents encode genuine theological contradictions from factional
 *   compromises (e.g., on religious liberty, collegiality, liturgy,
 *   ecumenism) that cannot be resolved into either continuity or rupture
 *   without doctrinal violence. The extraction falls on scholars who see the
 *   complexity (career costs, censorship) and pastoral workers who must
 *   implement univocal directives in situations where the texts pull both
 *   ways. The beneficiaries are the institutional magisterium (which
 *   maintains its interpretive monopoly) and traditionalist factions (which
 *   gain a clear boundary against dissent). Post-conciliar conflicts
 *   (lefebvrist schism, progressive dissent, liturgy wars) are structural
 *   consequences of the constraint, not accidental failures of reception.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_authority__composite_overdetermination_reading, 0.78).
domain_priors:suppression_score(vatican_ii_authority__composite_overdetermination_reading, 0.82).
domain_priors:theater_ratio(vatican_ii_authority__composite_overdetermination_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_authority__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_authority__composite_overdetermination_reading, "Vatican II Authority as Overdetermined Composite (Composite Overdetermination Reading)").
narrative_ontology:topic_domain(vatican_ii_authority__composite_overdetermination_reading, "theology/ecclesiology/religious_authority").

domain_priors:requires_active_enforcement(vatican_ii_authority__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_authority__composite_overdetermination_reading, 'e1a6b79a-b35b-4292-bdc9-c2075bb42103').
narrative_ontology:cs_kernel_codification('e1a6b79a-b35b-4292-bdc9-c2075bb42103', fixed_text).
narrative_ontology:cs_authority_grounding('e1a6b79a-b35b-4292-bdc9-c2075bb42103', lineage).
narrative_ontology:cs_interpretation_layer_present('e1a6b79a-b35b-4292-bdc9-c2075bb42103').
narrative_ontology:cs_reading_relation('e1a6b79a-b35b-4292-bdc9-c2075bb42103', vatican_ii_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('e1a6b79a-b35b-4292-bdc9-c2075bb42103', vatican_ii_authority__rupture_reading, coexists_with).
narrative_ontology:cs_axiom('e1a6b79a-b35b-4292-bdc9-c2075bb42103', foundational, vatican_ii_irreducibly_composite).
narrative_ontology:cs_axiom_status(vatican_ii_irreducibly_composite, holdable).
narrative_ontology:cs_axiom_grounding('e1a6b79a-b35b-4292-bdc9-c2075bb42103', vatican_ii_irreducibly_composite, empirically_contingent).
narrative_ontology:cs_axiom('e1a6b79a-b35b-4292-bdc9-c2075bb42103', foundational, factional_compromise_entails_contradiction).
narrative_ontology:cs_axiom_status(factional_compromise_entails_contradiction, holdable).
narrative_ontology:cs_axiom_grounding('e1a6b79a-b35b-4292-bdc9-c2075bb42103', factional_compromise_entails_contradiction, empirically_contingent).
narrative_ontology:cs_axiom('e1a6b79a-b35b-4292-bdc9-c2075bb42103', secondary, univocal_interpretation_structurally_false).
narrative_ontology:cs_axiom_status(univocal_interpretation_structurally_false, holdable).
narrative_ontology:cs_axiom_grounding('e1a6b79a-b35b-4292-bdc9-c2075bb42103', univocal_interpretation_structurally_false, deontological).
narrative_ontology:cs_reference_frame('e1a6b79a-b35b-4292-bdc9-c2075bb42103', composite_factional_compromise).
narrative_ontology:cs_drift_state('e1a6b79a-b35b-4292-bdc9-c2075bb42103', post_conciliar_reception, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e1a6b79a-b35b-4292-bdc9-c2075bb42103', '').
narrative_ontology:cs_kernel_id(vatican_ii_authority__composite_overdetermination_reading, vatican_ii_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_authority__composite_overdetermination_reading, complexity_recognizing_scholars).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, institutional_authority_claiming_univocal_interpretation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__composite_overdetermination_reading, traditionalist_factions).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, complexity_recognizing_scholars).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, pastoral_workers_navigating_ambiguity).
narrative_ontology:constraint_vindicates(vatican_ii_authority__composite_overdetermination_reading, hermeneutic_of_complexity).
narrative_ontology:constraint_vindicates(vatican_ii_authority__composite_overdetermination_reading, factional_compromise_entails_irreconcilable_contradiction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims and enforces univocal authoritative interpretation of Vatican II through magisterial teaching, canon law, and appointment powers. Its legitimacy depends on the council being readable as continuous with tradition. Suppresses readings that expose irreducible contradictions.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, institutional_magisterium, agenda_setter,
    institutional, generational, identity_locked, universal).

% Rally around the univocal continuity reading as a bulwark against doctrinal dissolution. Gain rhetorical clarity and institutional recognition when the magisterium enforces univocality. Their position is structurally dependent on the constraint they help maintain.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, traditionalist_factions, beneficiary,
    organized, biographical, constrained, global).

% Produce scholarship demonstrating the council's overdetermined composite character. Bear career costs (censure, denied appointments, marginalization) for refusing univocal readings. Simultaneously benefit intellectually from the constraint's falsity — the ambiguity is their research object and validation.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, complexity_recognizing_scholars, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_authority__composite_overdetermination_reading, complexity_recognizing_scholars, beneficiary).

% Must implement magisterial univocal directives in concrete pastoral situations where the council's own texts pull in opposite directions (e.g., liturgy, ecumenism, religious liberty). Bear the practical cost of the constraint's disconnect from lived reality.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, pastoral_workers_navigating_ambiguity, payer,
    powerless, immediate, trapped, local).

% Receive catechesis and preaching that presents Vatican II as unambiguously continuous (or, in some circles, as rupture). Would object to the simplification if the structural ambiguity were honestly presented, but are not consulted in the interpretive process.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, laity_receiving_simplified_teaching, excluded,
    powerless, biographical, constrained, global).

% Study the council's reception history without institutional stakes. Their consensus — that the documents contain genuine tensions from factional compromise — corroborates the composite reading but carries no enforcement power within the Church.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, external_historical_theologians, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The univocal authority claim coordinates the Church's self-understanding as a unified doctrinal subject across time and space; it solves the problem of maintaining communion when a council's texts pull in multiple directions.
% TRANSFER_FUNCTION: Moves interpretive authority and doctrinal certainty from the scholarly field (where ambiguity is recognized) to the magisterium (which claims univocal resolution); the cost is borne by scholars and pastoral workers who must suppress or navigate the suppressed complexity.
% ABSENT_VOICES: The laity receiving simplified teaching (excluded stakeholders) would object to being fed a univocal narrative when the documents themselves are multivocal. Also absent: the conciliar fathers from minority factions whose compromise positions were written into the texts but are now read out of them.
% DISAPPEARANCE_RATIONALE: If the univocal authority claim vanished overnight, the magisterium would lose its primary warrant for post-conciliar governance; scholars would publish openly on the texts' contradictions; pastoral practice would diversify; traditionalist and progressive factions would no longer share a fake common text to fight over — the ecclesial field would reorganize around acknowledged plurality.
% FOUNDING_PROBLEM: After a council that deliberately used ambiguous compromise formulas to achieve consensus, the Church needed a way to present the outcome as a coherent, authoritative act of the Holy Spirit rather than a human political settlement.
% FOUNDING_PROBLEM_CORROBORATION: Historical scholarship on the conciliar debates (O'Malley, Alberigo, Komonchak) documents the factional compromises and deliberate ambiguities. The magisterium's own subsequent interventions (e.g., 1985 Extraordinary Synod, hermeneutic of continuity) implicitly acknowledge the founding problem by attempting to re-secure unity — corroboration from outside the beneficiary set (scholars) is robust; the magisterium's own defensive posture corroborates that the problem it was built to solve (credible unity) persists but the solution (univocal claim) has failed.
narrative_ontology:disappearance_verdict(vatican_ii_authority__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_authority__composite_overdetermination_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_authority__composite_overdetermination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_authority__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_authority__composite_overdetermination_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_authority__composite_overdetermination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_authority__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_authority__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the univocal claim demands assent to a reading the texts do not sustain; the cost of compliance is intellectual dishonesty or institutional sanction. Suppression (0.82) is very high — the magisterium uses canonical, appointive, and pedagogical machinery to enforce univocality; alternatives are not merely discouraged but structurally excluded from official formation. Theater ratio (0.45) reflects that the coordination function (unity) is real but increasingly performative — the unity maintained is unity around a falsified reading. Accessibility collapse (0.75) is high because the institutional definition of 'faithful interpretation' collapses the alternative space; Resistance (0.55) is moderate — scholarly resistance persists but is contained within academia; pastoral resistance is fragmented and individualized.
 *
 * PERSPECTIVAL GAP:
 *   The magisterium experiences the constraint as genuine coordination (rope-like) — it believes it is preserving the deposit of faith. Scholars experience it as extraction (snare-like) — they see the suppression of evident textual contradictions. Pastoral workers experience it as incoherent demand (piton-like) — they perform compliance while knowing it doesn't fit reality. The engine computes these divergent seat classifications from the structural data; this reading's claim (tangled_rope) captures the structural hybridity that no single seat fully sees.
 *
 * DIRECTIONALITY LOGIC:
 *   The magisterium (institutional, identity_locked) sits at the beneficiary extreme (d ≈ 0.1): it sets the agenda, collects the authority-rents, and its identity is fused with the constraint's maintenance. Traditionalist factions (organized, constrained) are secondary beneficiaries (d ≈ 0.25) — they gain rhetorical ground but depend on the magisterium's enforcement. Complexity-recognizing scholars (moderate, constrained) are primary targets (d ≈ 0.85) — they bear the extraction directly through career suppression; their exit is constrained by vocation. Pastoral workers (powerless, trapped) are secondary targets (d ≈ 0.9) — they implement the constraint daily with no voice in its formation. Laity (powerless, constrained, excluded) are diffuse targets (d ≈ 0.7) — they receive the simplified product. External observers (analytical, analytical) sit at d = 0.5 (symmetric) — they analyze without stake.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (credible post-conciliar unity) is dead — the council's compromise formulas did not produce a stable consensus, and the univocal claim has not prevented fragmentation (schism, dissent, polarization). The constraint persists because the magisterium's identity is locked to the claim that Vatican II is univocally continuous; admitting the composite character would undermine the authority structure that enforces the claim. This is mandatrophy: the constraint's mandate (unity) has been inverted into its opposite (enforced unity around a false reading generates the very fragmentation it was built to prevent).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the composite_overdetermination_reading a description of the council''s historical reality, or a hermeneutic choice that could be resolved by a better continuity or rupture framework?',
    'Consensus among historical theologians working from the conciliar acta and intervention texts; if the factional compromises and deliberate ambiguities are documented historical facts, the reading is empirically grounded; if they are theory-laden constructions, the reading is contestable.',
    'If empirically grounded, the univocal authority claim is a snare/tangled_rope suppressing known facts; if hermeneutic, the three readings coexist as interpretive options and the constraint is the field''s inability to choose — shifting classification toward rope or scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, empirical, 'Whether the composite character is historical fact or interpretive stance.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of composite readings primarily structural (canonical penalties, appointment control) or internalized (scholars self-censor because their professional identity requires magisterial legitimacy)?',
    'Track suppression trajectory post-exit: scholars who leave institutional positions — does their work immediately engage the composite reading, or do they continue self-censoring? If the latter, internalized suppression is significant.',
    'If internalized, effective suppression exceeds the structural measure; the constraint''s extraction is amplified by identity capture. This would increase χ for the scholar seat beyond what structural suppression alone predicts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the theological academy.').

omega_variable(
    factional_compromise_irreducibility,
    'Are the council''s contradictions genuinely irreducible (no theological synthesis possible), or does a higher synthesis exist that the composite reading misses?',
    'Systematic theological engagement: can a coherent doctrinal account be given that honors all conciliar texts without distortion? The 60-year failure of both continuity and rupture frameworks to achieve consensus is negative evidence; a successful synthesis would be positive evidence.',
    'If irreducible, the univocal claim is necessarily extractive (it demands assent to a falsehood); if synthesizable, the constraint is a temporary scaffold awaiting the synthesis — shifting claimed_type toward scaffold.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(factional_compromise_irreducibility, conceptual, 'Whether the council''s tensions admit of theological resolution.').

omega_variable(
    cs_framing_underdetermination,
    'Does the composite reading frame the kernel as a fixed-text commitment system (Vatican II documents as canonical text) or as a distributed practice system (the living reception of the council)?',
    'Compare the constraint''s operation: if authority appeals to the texts'' literal wording as binding, it''s fixed-text; if authority appeals to the ''spirit of the council'' as mediated by the magisterium, it''s practice/distributed. The composite reading''s evidence (textual contradictions) assumes fixed-text framing.',
    'Fixed-text framing makes the composite reading empirically testable against the documents; practice framing makes it a contest over legitimate interpretation. The cs_structure classification (fixed_text + lineage + interpretation_layer_present) commits to fixed-text; if practice framing is more accurate, the axioms'' grounding_type shifts from empirically_contingent to conventional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Whether the kernel''s codification is fixed-text or practice-based — affects axiom grounding and drift analysis.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_authority__composite_overdetermination_reading, 1965, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vatican_ii_authority__composite_overdetermination_reading_tr_t1965, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 1965, 0.2).
narrative_ontology:measurement(vatican_ii_authority__composite_overdetermination_reading_tr_t1975, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 1975, 0.28).
narrative_ontology:measurement(vatican_ii_authority__composite_overdetermination_reading_tr_t1985, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 1985, 0.38).
narrative_ontology:measurement(vatican_ii_authority__composite_overdetermination_reading_tr_t1995, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 1995, 0.42).
narrative_ontology:measurement(vatican_ii_authority__composite_overdetermination_reading_tr_t2005, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 2005, 0.44).
narrative_ontology:measurement(vatican_ii_authority__composite_overdetermination_reading_tr_t2015, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 2015, 0.45).
narrative_ontology:measurement(vatican_ii_authority__composite_overdetermination_reading_tr_t2025, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(vatican_ii_authority__composite_overdetermination_reading_be_t1965, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 1965, 0.35).
narrative_ontology:measurement(vatican_ii_authority__composite_overdetermination_reading_be_t1975, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 1975, 0.52).
narrative_ontology:measurement(vatican_ii_authority__composite_overdetermination_reading_be_t1985, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 1985, 0.68).
narrative_ontology:measurement(vatican_ii_authority__composite_overdetermination_reading_be_t1995, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 1995, 0.72).
narrative_ontology:measurement(vatican_ii_authority__composite_overdetermination_reading_be_t2005, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 2005, 0.76).
narrative_ontology:measurement(vatican_ii_authority__composite_overdetermination_reading_be_t2015, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 2015, 0.77).
narrative_ontology:measurement(vatican_ii_authority__composite_overdetermination_reading_be_t2025, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(vatican_ii_authority__composite_overdetermination_reading_su_t1965, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 1965, 0.4).
narrative_ontology:measurement(vatican_ii_authority__composite_overdetermination_reading_su_t1975, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 1975, 0.55).
narrative_ontology:measurement(vatican_ii_authority__composite_overdetermination_reading_su_t1985, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 1985, 0.7).
narrative_ontology:measurement(vatican_ii_authority__composite_overdetermination_reading_su_t1995, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 1995, 0.78).
narrative_ontology:measurement(vatican_ii_authority__composite_overdetermination_reading_su_t2005, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 2005, 0.8).
narrative_ontology:measurement(vatican_ii_authority__composite_overdetermination_reading_su_t2015, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 2015, 0.81).
narrative_ontology:measurement(vatican_ii_authority__composite_overdetermination_reading_su_t2025, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 2025, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_authority__composite_overdetermination_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vatican_ii_authority__composite_overdetermination_reading, 0.08).
narrative_ontology:affects_constraint(vatican_ii_authority__composite_overdetermination_reading, vatican_ii_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__composite_overdetermination_reading, vatican_ii_authority__rupture_reading).

% DUAL FORMULATION NOTE:
% This constraint (composite_overdetermination_reading) and its two siblings form the vatican_ii_authority constraint family. The kernel is the authority of Vatican II. The continuity_reading and rupture_reading each impose univocal frameworks (continuity or rupture) on the same texts; this reading claims both frameworks are falsified by the texts' composite character. The three stories are linked: the continuity and rupture readings are the enforcement targets of the institutional constraint this story analyzes; this reading's validation would structurally undermine both siblings' coherence. All three share the same referent (the conciliar texts and their reception) but instantiate different constraints with different ε, beneficiaries, and victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_authority__composite_overdetermination_reading, institutional, 0.1).
constraint_indexing:directionality_override(vatican_ii_authority__composite_overdetermination_reading, moderate, 0.85).
constraint_indexing:directionality_override(vatican_ii_authority__composite_overdetermination_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
