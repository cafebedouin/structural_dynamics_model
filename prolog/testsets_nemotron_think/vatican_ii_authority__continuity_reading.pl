% ============================================================================
% CONSTRAINT STORY: vatican_ii_authority__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_authority__continuity_reading, []).

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
 *   constraint_id: vatican_ii_authority__continuity_reading
 *   human_readable: Vatican II Continuity Reading: Organic Doctrinal Development
 *   domain: theology/ecclesiology/religious_authority
 *
 * SUMMARY:
 *   This constraint story instantiates the continuity_reading of the
 *   vatican_ii_authority kernel. The reading holds that all 16 conciliar
 *   documents are valid expressions of the unchanging deposit of faith, that
 *   post-conciliar reforms are legitimate when faithful to the conciliar
 *   texts, and that ambiguities in those texts are resolvable through
 *   traditional hermeneutical principles (the 'hermeneutic of continuity').
 *   The reading presents Vatican II as a rope: a genuine coordination
 *   mechanism that solves the problem of how the Church engages modernity
 *   without doctrinal rupture. Beneficiaries are progressive reformers who
 *   claim continuity as their legitimating framework; the reading asserts no
 *   victims exist because reforms are cost-free organic development. This is
 *   one of three structurally distinct readings of the same kernel — the
 *   rupture_reading and composite_overdetermination_reading instantiate
 *   different constraints with different ε values and victim structures.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_authority__continuity_reading, 0.15).
domain_priors:suppression_score(vatican_ii_authority__continuity_reading, 0.1).
domain_priors:theater_ratio(vatican_ii_authority__continuity_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_authority__continuity_reading, rope).
narrative_ontology:human_readable(vatican_ii_authority__continuity_reading, "Vatican II Continuity Reading: Organic Doctrinal Development").
narrative_ontology:topic_domain(vatican_ii_authority__continuity_reading, "theology/ecclesiology/religious_authority").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_authority__continuity_reading, 'cf3d732d-8ef1-4a2e-a83c-60fb978f9251').
narrative_ontology:cs_kernel_codification('cf3d732d-8ef1-4a2e-a83c-60fb978f9251', fixed_text).
narrative_ontology:cs_authority_grounding('cf3d732d-8ef1-4a2e-a83c-60fb978f9251', lineage).
narrative_ontology:cs_interpretation_layer_present('cf3d732d-8ef1-4a2e-a83c-60fb978f9251').
narrative_ontology:cs_reading_relation('cf3d732d-8ef1-4a2e-a83c-60fb978f9251', vatican_ii_authority__rupture_reading, coexists_with).
narrative_ontology:cs_reading_relation('cf3d732d-8ef1-4a2e-a83c-60fb978f9251', vatican_ii_authority__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('cf3d732d-8ef1-4a2e-a83c-60fb978f9251', foundational, conciliar_documents_univocally_continuous).
narrative_ontology:cs_axiom_status(conciliar_documents_univocally_continuous, holdable).
narrative_ontology:cs_axiom_grounding('cf3d732d-8ef1-4a2e-a83c-60fb978f9251', conciliar_documents_univocally_continuous, deontological).
narrative_ontology:cs_axiom('cf3d732d-8ef1-4a2e-a83c-60fb978f9251', secondary, traditional_hermeneutics_resolve_ambiguities).
narrative_ontology:cs_axiom_status(traditional_hermeneutics_resolve_ambiguities, holdable).
narrative_ontology:cs_axiom_grounding('cf3d732d-8ef1-4a2e-a83c-60fb978f9251', traditional_hermeneutics_resolve_ambiguities, conventional).
narrative_ontology:cs_reference_frame('cf3d732d-8ef1-4a2e-a83c-60fb978f9251', conciliar_texts_as_received).
narrative_ontology:cs_drift_state('cf3d732d-8ef1-4a2e-a83c-60fb978f9251', post_conciliar_implementation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cf3d732d-8ef1-4a2e-a83c-60fb978f9251', '').
narrative_ontology:cs_kernel_id(vatican_ii_authority__continuity_reading, vatican_ii_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, progressive_reformers_claiming_continuity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, magisterium).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, laity_generally).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, conciliar_theologians_continuity_school).
narrative_ontology:constraint_victim(vatican_ii_authority__continuity_reading, laity_generally).
narrative_ontology:constraint_vindicates(vatican_ii_authority__continuity_reading, organic_doctrinal_development).
narrative_ontology:constraint_vindicates(vatican_ii_authority__continuity_reading, hermeneutic_of_continuity).
narrative_ontology:constraint_vindicates(vatican_ii_authority__continuity_reading, deposit_of_faith_unchanging).
narrative_ontology:constraint_vindicates(vatican_ii_authority__continuity_reading, conciliar_documents_univocally_valid).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Theologians, bishops, and movements that use the hermeneutic of continuity to legitimate liturgical, ecumenical, and pastoral reforms. They gain interpretive authority and institutional momentum from the reading's claim that all conciliar documents are univocally valid. Their exit option is mobile — they could adopt a different hermeneutic but would lose the legitimating framework.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, progressive_reformers_claiming_continuity, beneficiary,
    organized, generational, mobile, global).

% The teaching authority (pope, curia, episcopal conferences) that promulgates and guards the hermeneutic of continuity. They administer the interpretive framework and benefit from its unifying function, but are constrained by it — they cannot easily abandon it without destabilizing their own authority. Exit is constrained by the institutional role itself.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, magisterium, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_authority__continuity_reading, magisterium, beneficiary).

% Communities (SSPX, sedevacantists, conservative dioceses, traditionalist laity) who reject the continuity reading and see rupture. The reading renders them invisible as victims by framing their position as self-exclusion from the hermeneutic. Their exit is identity_locked — their ecclesial identity is constituted by rejection of the post-conciliar framework; leaving would mean abandoning their self-understanding as the faithful remnant.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, traditionalist_catholics, excluded,
    organized, generational, identity_locked, global).

% Ordinary Catholics who receive a unified interpretive framework for the Council's teachings but bear the costs of ambiguous implementation (liturgical instability, catechetical confusion, pastoral inconsistency). They benefit from coherence but pay for the gap between conciliar texts and lived practice. Exit is constrained — leaving the Church is a grave step; staying means living with the ambiguity.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, laity_generally, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_authority__continuity_reading, laity_generally, payer).

% Theological school (Ratzinger, de Lubac, Congar, von Balthasar, and successors) that developed and maintains the hermeneutic of continuity. They set the intellectual agenda and benefit from its institutional adoption. Their exit is mobile — they could shift schools but have invested careers in this framework.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, conciliar_theologians_continuity_school, agenda_setter,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_authority__continuity_reading, conciliar_theologians_continuity_school, beneficiary).

% External observers (scholars of religion, historians, comparative theologians) who see the full kernel structure — all three readings as live positions in a contested field. They neither collect nor pay; they analyze the structural relationships between readings.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified hermeneutic for interpreting all 16 conciliar documents as organically continuous with pre-conciliar tradition, enabling the Church to claim both renewal and doctrinal identity simultaneously.
% TRANSFER_FUNCTION: Moves interpretive authority from rupture-reading critics (who would declare the Council a break) to continuity-reading reformers (who claim the Council as legitimate development). Moves legitimating power from pre-conciliar schemas to post-conciliar implementations that pass the continuity test.
% ABSENT_VOICES: Traditionalist Catholics (SSPX, sedevacantists, conservative laity) who experience the continuity reading as an imposed framework that delegitimizes their experience of rupture. They are structurally excluded because the reading defines them out of the conversation — their objection is treated as proof of their own hermeneutical failure rather than as evidence against the reading. Also absent: laity in the Global South for whom 'organic development' maps poorly onto inculturated practice.
% DISAPPEARANCE_RATIONALE: If the continuity reading vanished overnight, the Church would lose its primary framework for legitimating post-conciliar reforms. The magisterium would face a crisis of authority: either adopt the rupture reading (conceding the Council broke tradition) or the composite reading (conceding irresolvable ambiguity). Liturgical, ecumenical, and canonical reforms justified by continuity would lose their warrant. Schism dynamics would accelerate.
% FOUNDING_PROBLEM: How to renew the Church's engagement with the modern world (aggiornamento) without breaking doctrinal continuity with the deposit of faith — specifically, how to interpret the Council's novel formulations (religious liberty, collegiality, liturgical vernacular, engagement with non-Christian religions) as developments of prior teaching rather than contradictions of it.
% FOUNDING_PROBLEM_CORROBORATION: The Council Fathers themselves (in conciliar acts and post-conciliar interventions), Pope Paul VI (closing address, encyclicals), Pope John Paul II (Novo Millennio Ineunte, interpretive addresses), Pope Benedict XVI (hermeneutic of continuity addresses, 2005 and 2013), and the International Theological Commission (various documents). All attest from within the magisterial tradition; no major corroborating source outside the benefiting parties (progressive reformers + magisterium) explicitly endorses the founding problem as still live in the continuity reading's terms — traditionalist and rupture-reading sources attest the problem is dead or was misdiagnosed.
narrative_ontology:disappearance_verdict(vatican_ii_authority__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_authority__continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_authority__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_authority__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_authority__continuity_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_authority__continuity_reading_tests).
:- end_tests(vatican_ii_authority__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the reading frames the constraint as enabling development rather than extracting compliance; the 'cost' of adherence is framed as the cost of fidelity itself. Suppression is low (0.10) because the reading does not require active enforcement — dissenters are not coerced but rather argued with (though canonical penalties for schism exist at the institutional level, they are not central to this reading's operation). Theater ratio is low (0.12) because the hermeneutic work is genuine scholarly engagement, not performative. Accessibility collapse is moderate (0.45) because alternative readings (rupture, composite) remain live and structurally coherent — the constraint does not foreclose them. Resistance is moderate (0.35) because traditionalist and progressive rupture-reading communities actively resist the continuity framework.
 *
 * PERSPECTIVAL GAP:
 *   The continuity reading's own seat experiences the constraint as rope (genuine coordination). The rupture_reading's seat would experience the same conciliar documents as snare (extraction disguised as development). The composite_overdetermination_reading's seat experiences the kernel as tangled_rope (multiple coordination/extraction layers). The engine computes these per-seat classifications from the structural data; this story authors only the continuity reading's structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The primary beneficiary is progressive_reformers_claiming_continuity: they gain interpretive authority and legitimacy for reforms by anchoring them in the conciliar texts read through the hermeneutic of continuity. Their directionality d is near 0.0 (full beneficiary). The magisterium (pope, curia, bishops) holds role agenda_setter with d near 0.2 — they administer the hermeneutic but also bear responsibility for its coherence. Traditionalist Catholics are not declared victims in this reading (the reading asserts they self-exclude by rejecting the hermeneutic), but the omega variable victim_absence_claim flags this as contested. Laity generally have role beneficiary with d ~0.3 — they receive a unified framework but bear costs of ambiguous implementation. The analytical observer seat sees the full kernel structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — renewing the Church's engagement with modernity without doctrinal rupture — remains live (status: live). The continuity reading has not become mandatrophic because the problem it was built to solve (how to read the Council) persists. However, if the hermeneutic of continuity proves unable to resolve key ambiguities without circular reasoning, the reading could drift toward piton (theatrical maintenance of a coordination claim whose function has atrophied). The theater_ratio's slow rise from 0.05 to 0.12 over 60 years warrants monitoring.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'Is this constraint one reading of a contested kernel (vatican_ii_authority) rather than a free-standing constraint?',
    'Cross-reference with sibling readings rupture_reading and composite_overdetermination_reading; if they instantiate structurally distinct constraints with different ε, beneficiary/victim sets, and type classifications, the kernel frame is confirmed.',
    'If confirmed, the continuity_reading''s ε=0.15 describes the standing arrangement under THIS reading''s lights only; the rupture_reading would author a substantially higher ε for the same referent (post-conciliar reforms as rupture). The composite reading would author a different structure again.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Commitment-system kernel structure: this constraint is the continuity_reading of vatican_ii_authority').

omega_variable(
    victim_absence_claim,
    'Does the continuity reading genuinely have no victims, or are traditionalist Catholics and communities harmed by ambiguous implementations structural victims that the reading renders invisible?',
    'Examine whether traditionalist communities (SSPX, sedevacantists, conservative dioceses) bear costs (canonical irregularity, loss of liturgical stability, marginalization) that the continuity reading treats as self-inflicted rather than constraint-imposed.',
    'If traditionalists are structural victims, the constraint reclassifies from rope toward tangled_rope (coordination + asymmetric extraction). The reading''s claim of ''cost-free development'' would be a false coordination story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_absence_claim, empirical, 'Whether the continuity reading''s ''no victims'' claim holds under structural scrutiny').

omega_variable(
    hermeneutic_resolution_capacity,
    'Can traditional hermeneutics actually resolve the documented ambiguities in conciliar texts (e.g., religious liberty, collegiality, liturgical reform) without importing external theological commitments?',
    'Test whether leading continuity-reading theologians (Ratzinger/Benedict XVI, de Lubac, Congar) produce convergent resolutions on contested texts using only pre-conciliar hermeneutical principles, or whether resolutions require the very post-conciliar developments they claim to ground.',
    'If hermeneutics cannot resolve ambiguities internally, the continuity reading''s coordination function is overstated; the constraint operates more as a legitimating narrative (tangled_rope) than genuine coordination (rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hermeneutic_resolution_capacity, conceptual, 'Whether the continuity reading''s coordination mechanism (traditional hermeneutics) is functionally adequate').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_authority__continuity_reading, 1965, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vatican_ii_continuity_tr_t1965, vatican_ii_authority__continuity_reading, theater_ratio, 1965, 0.05).
narrative_ontology:measurement(vatican_ii_continuity_tr_t1975, vatican_ii_authority__continuity_reading, theater_ratio, 1975, 0.08).
narrative_ontology:measurement(vatican_ii_continuity_tr_t1985, vatican_ii_authority__continuity_reading, theater_ratio, 1985, 0.1).
narrative_ontology:measurement(vatican_ii_continuity_tr_t1995, vatican_ii_authority__continuity_reading, theater_ratio, 1995, 0.11).
narrative_ontology:measurement(vatican_ii_continuity_tr_t2005, vatican_ii_authority__continuity_reading, theater_ratio, 2005, 0.12).
narrative_ontology:measurement(vatican_ii_continuity_tr_t2015, vatican_ii_authority__continuity_reading, theater_ratio, 2015, 0.12).
narrative_ontology:measurement(vatican_ii_continuity_tr_t2025, vatican_ii_authority__continuity_reading, theater_ratio, 2025, 0.12).

% Extraction over time
narrative_ontology:measurement(vatican_ii_continuity_be_t1965, vatican_ii_authority__continuity_reading, base_extractiveness, 1965, 0.08).
narrative_ontology:measurement(vatican_ii_continuity_be_t1975, vatican_ii_authority__continuity_reading, base_extractiveness, 1975, 0.1).
narrative_ontology:measurement(vatican_ii_continuity_be_t1985, vatican_ii_authority__continuity_reading, base_extractiveness, 1985, 0.12).
narrative_ontology:measurement(vatican_ii_continuity_be_t1995, vatican_ii_authority__continuity_reading, base_extractiveness, 1995, 0.13).
narrative_ontology:measurement(vatican_ii_continuity_be_t2005, vatican_ii_authority__continuity_reading, base_extractiveness, 2005, 0.14).
narrative_ontology:measurement(vatican_ii_continuity_be_t2015, vatican_ii_authority__continuity_reading, base_extractiveness, 2015, 0.15).
narrative_ontology:measurement(vatican_ii_continuity_be_t2025, vatican_ii_authority__continuity_reading, base_extractiveness, 2025, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(vatican_ii_continuity_su_t1965, vatican_ii_authority__continuity_reading, suppression_requirement, 1965, 0.05).
narrative_ontology:measurement(vatican_ii_continuity_su_t1975, vatican_ii_authority__continuity_reading, suppression_requirement, 1975, 0.08).
narrative_ontology:measurement(vatican_ii_continuity_su_t1985, vatican_ii_authority__continuity_reading, suppression_requirement, 1985, 0.1).
narrative_ontology:measurement(vatican_ii_continuity_su_t1995, vatican_ii_authority__continuity_reading, suppression_requirement, 1995, 0.1).
narrative_ontology:measurement(vatican_ii_continuity_su_t2005, vatican_ii_authority__continuity_reading, suppression_requirement, 2005, 0.1).
narrative_ontology:measurement(vatican_ii_continuity_su_t2015, vatican_ii_authority__continuity_reading, suppression_requirement, 2015, 0.1).
narrative_ontology:measurement(vatican_ii_continuity_su_t2025, vatican_ii_authority__continuity_reading, suppression_requirement, 2025, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_authority__continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vatican_ii_authority__continuity_reading, 0.08).
narrative_ontology:affects_constraint(vatican_ii_authority__continuity_reading, vatican_ii_authority__rupture_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__continuity_reading, vatican_ii_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the vatican_ii_authority constraint family. The continuity_reading (this story) authors ε=0.15, claimed_type=rope, beneficiaries=progressive_reformers_claiming_continuity, victims=none. The rupture_reading would author substantially higher ε, claimed_type=snare or tangled_rope, victims=traditionalist_communities. The composite_overdetermination_reading would author a multi-layer structure with tangled_rope characteristics. The three stories are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_authority__continuity_reading, institutional, 0.2).
constraint_indexing:directionality_override(vatican_ii_authority__continuity_reading, organized, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
