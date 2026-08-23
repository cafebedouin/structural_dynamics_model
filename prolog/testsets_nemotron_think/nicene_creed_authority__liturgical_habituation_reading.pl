% ============================================================================
% CONSTRAINT STORY: nicene_creed_authority__liturgical_habituation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_creed_authority__liturgical_habituation_reading, []).

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
 *   constraint_id: nicene_creed_authority__liturgical_habituation_reading
 *   human_readable: Nicene Creed as Liturgical Habituation Identity Marker
 *   domain: systematic_theology/ecclesiology/history_of_christian_doctrine
 *
 * SUMMARY:
 *   This reading of the Nicene Creed's authority treats the creed not as a
 *   metaphysical boundary enforced by sanction (strict_orthodox) nor as a
 *   historically contingent witness subject to communal discernment
 *   (symbolic_confessional), but as a liturgical habitus — a weekly
 *   performance that forms Christian identity through repetition,
 *   independently of whether the reciter cognitively assents to each clause.
 *   The creed functions as a coordination mechanism (rope): it solves the
 *   problem of maintaining communion across doctrinal diversity by giving
 *   communities a shared performative grammar. Extraction is near-zero
 *   because participation is voluntary, the performance is the benefit, and
 *   no party collects rents from the arrangement. The constraint feeds both
 *   sibling readings by providing the social substrate — the liturgical 'we'
 *   — that makes doctrinal enforcement or pluralist reinterpretation
 *   possible.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_creed_authority__liturgical_habituation_reading, 0.08).
domain_priors:suppression_score(nicene_creed_authority__liturgical_habituation_reading, 0.15).
domain_priors:theater_ratio(nicene_creed_authority__liturgical_habituation_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_creed_authority__liturgical_habituation_reading, rope).
narrative_ontology:human_readable(nicene_creed_authority__liturgical_habituation_reading, "Nicene Creed as Liturgical Habituation Identity Marker").
narrative_ontology:topic_domain(nicene_creed_authority__liturgical_habituation_reading, "systematic_theology/ecclesiology/history_of_christian_doctrine").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_creed_authority__liturgical_habituation_reading, '2608b4e6-3a6f-4c0b-bc5a-ebb76961484f').
narrative_ontology:cs_kernel_codification('2608b4e6-3a6f-4c0b-bc5a-ebb76961484f', fixed_text).
narrative_ontology:cs_authority_grounding('2608b4e6-3a6f-4c0b-bc5a-ebb76961484f', practice).
narrative_ontology:cs_interpretation_layer_present('2608b4e6-3a6f-4c0b-bc5a-ebb76961484f').
narrative_ontology:cs_reading_relation('2608b4e6-3a6f-4c0b-bc5a-ebb76961484f', nicene_creed_authority__strict_orthodox_reading, coexists_with).
narrative_ontology:cs_reading_relation('2608b4e6-3a6f-4c0b-bc5a-ebb76961484f', nicene_creed_authority__symbolic_confessional_reading, coexists_with).
narrative_ontology:cs_axiom('2608b4e6-3a6f-4c0b-bc5a-ebb76961484f', foundational, liturgical_performance_forms_identity).
narrative_ontology:cs_axiom_status(liturgical_performance_forms_identity, holdable).
narrative_ontology:cs_axiom_grounding('2608b4e6-3a6f-4c0b-bc5a-ebb76961484f', liturgical_performance_forms_identity, conventional).
narrative_ontology:cs_axiom('2608b4e6-3a6f-4c0b-bc5a-ebb76961484f', foundational, assent_follows_performance).
narrative_ontology:cs_axiom_status(assent_follows_performance, holdable).
narrative_ontology:cs_axiom_grounding('2608b4e6-3a6f-4c0b-bc5a-ebb76961484f', assent_follows_performance, empirically_contingent).
narrative_ontology:cs_reference_frame('2608b4e6-3a6f-4c0b-bc5a-ebb76961484f', liturgical_habituation_framework).
narrative_ontology:cs_drift_state('2608b4e6-3a6f-4c0b-bc5a-ebb76961484f', contemporary_liturgical_renewal, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('2608b4e6-3a6f-4c0b-bc5a-ebb76961484f', '').
narrative_ontology:cs_kernel_id(nicene_creed_authority__liturgical_habituation_reading, nicene_creed_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_creed_authority__liturgical_habituation_reading, liturgical_community).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__liturgical_habituation_reading, ecclesial_identity_cohort).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__liturgical_habituation_reading, catechumens_new_members).
narrative_ontology:constraint_vindicates(nicene_creed_authority__liturgical_habituation_reading, liturgical_formation_shapes_identity).
narrative_ontology:constraint_vindicates(nicene_creed_authority__liturgical_habituation_reading, performance_precedes_assent).
narrative_ontology:constraint_vindicates(nicene_creed_authority__liturgical_habituation_reading, shared_rite_binds_diverse_assent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The worshipping assembly that recites the creed weekly; their corporate identity is constituted through this repeated performance. The community sets the liturgical calendar and rubrics that mandate creedal recitation. Members experience the creed as the grammar of their belonging — leaving the performance means leaving the identity.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, liturgical_community, agenda_setter,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(nicene_creed_authority__liturgical_habituation_reading, liturgical_community, beneficiary).

% Lifelong participants whose sense of 'being Christian' is inseparable from having said these words in this community for decades. They receive identity coherence and intercommunion recognition through the habit. Exit would fracture their self-understanding and social ecology.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, ecclesial_identity_cohort, beneficiary,
    organized, biographical, identity_locked, global).

% Those entering the community through catechesis culminating in creedal profession at baptism. The creed functions as the threshold performance — saying it makes them 'one of us.' They have some exit option before baptism but face high identity cost after.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, catechumens_new_members, beneficiary,
    moderate, biographical, constrained, local).

% Presiders and teachers who guard the liturgical form and teach its performance. Their authority derives from faithful transmission of the rite. They could modify the rite but face institutional and communal resistance; their vocation is bound to the tradition.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, clergy_liturgical_leaders, agenda_setter,
    institutional, generational, constrained, global).

% Theologians within the communion who reject the creed's metaphysical claims but remain in the community. They participate in the performance while internally dissenting. Their exclusion is from the 'assent' frame, not the liturgical frame — they are silenced in doctrinal forums but present in the pew.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, dissident_theologians, excluded,
    moderate, biographical, constrained, global).

% Scholars of liturgy, doctrine, and social anthropology who study the creed's identity-forming function across traditions. They neither participate in nor are bound by the performance; they map its structural effects.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared liturgical performance that binds community identity across metaphysical disagreement — the creed operates as a 'password' or 'handshake' that signals belonging without requiring uniform cognitive assent to every proposition.
% TRANSFER_FUNCTION: Moves communal belonging and identity coherence from individual participants to the collective through repeated weekly performance; the community gains a stable identity substrate that persists despite doctrinal diversity, while participants receive recognition as 'insiders' and access to intercommunion.
% ABSENT_VOICES: Those who cannot participate in liturgical performance — homebound, persecuted, non-literate, or non-verbal members — yet claim full community membership. Also historical communities that used different identity markers (e.g., early Syrian churches with different creedal forms). Their absence from the performance frame is noted but does not break the coordination function.
% DISAPPEARANCE_RATIONALE: If creedal recitation vanished overnight, communities would lose the primary weekly performance that constitutes their corporate identity across doctrinal difference. New identity substrates would need to be constructed — likely through expanded eucharistic theology, shared social action, or alternative liturgical texts — but the transition would rearrange ecclesial boundaries and intercommunion recognition.
% FOUNDING_PROBLEM: The early church needed a performative identity marker that could hold together diverse metaphysical interpretations (Alexandrian, Antiochene, Western) within one communion, without requiring philosophical uniformity that would fracture the body.
% FOUNDING_PROBLEM_CORROBORATION: Liturgical scholars across traditions — Alexander Schmemann (Orthodox, For the Life of the World), James K.A. Smith (Reformed, Desiring the Kingdom), Catholic liturgical movement theologians (Guardini, Ratzinger pre-papacy) — attest to the creed's identity-forming function independent of its use as a doctrinal test. The convergence across hostile theological camps corroborates the structural claim.
narrative_ontology:disappearance_verdict(nicene_creed_authority__liturgical_habituation_reading, world_rearranges).
narrative_ontology:founding_problem_status(nicene_creed_authority__liturgical_habituation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_creed_authority__liturgical_habituation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nicene_creed_authority__liturgical_habituation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nicene_creed_authority__liturgical_habituation_reading, 0.08, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_creed_authority__liturgical_habituation_reading_tests).
:- end_tests(nicene_creed_authority__liturgical_habituation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is minimal (0.08) because the creed's performance is the good itself — participants receive identity formation, not extraction. Suppression is low (0.15) because the constraint persists through habituation, not coercion; those who stop reciting simply drift away rather than being sanctioned. Theater ratio is moderate (0.25) because the performance has genuine formative function but also carries symbolic weight that exceeds its literal propositions. Accessibility collapse is low (0.30) because alternative identity markers exist (baptismal vows, eucharistic participation, creedal variants). Resistance is low (0.20) because the arrangement is experienced as gift, not burden. The claimed type is rope — a coordination mechanism solving the collective action problem of maintaining unity amid diversity.
 *
 * PERSPECTIVAL GAP:
 *   The strict_orthodox reading experiences this constraint as a snare (it fails to enforce metaphysical conformity); the symbolic_confessional reading experiences it as a scaffold (it should yield to communal discernment). The liturgical_habituation reading experiences it as rope — the performance works. The engine computes these seat divergences from the structural data; the claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The liturgical_community and clergy_liturgical_leaders are agenda_setters who also benefit (d near 0.0-0.2) — they maintain the form that constitutes them. The ecclesial_identity_cohort and catechumens are beneficiaries (d near 0.1-0.3) — they receive identity coherence. Dissident_theologians are excluded from the assent frame but participate in the performance frame — their directionality is complex: they pay cognitive dissonance cost (d ~0.4) but receive identity belonging (d ~0.2). The engine will compute per-seat χ from these structural positions. No party is a net payer — the constraint is not extractive.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (holding diverse metaphysics in one communion) remains live — ecumenical and intra-tradition diversity persists. The arrangement has not atrophied; it has expanded as liturgical renewal movements restored weekly creedal recitation in traditions that had lost it. No mandatrophy: the coordination function is active and the constraint is not maintained by inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_liturgical_habituation,
    'Does this constraint instantiate a distinct reading of the nicene_creed_authority kernel, structurally separable from strict_orthodox and symbolic_confessional readings?',
    'Comparative analysis of the three readings'' beneficiary/victim structures, enforcement mechanisms, and identity-formation claims. If the liturgical_habituation reading shows rope metrics (low ε, low suppression, coordination function) while siblings show snare/tangled_rope metrics, the structural distinction is confirmed.',
    'If confirmed, the kernel decomposition is valid per ε-invariance: each reading is a separate constraint with its own ε. If not, the ''liturgical habituation'' claim collapses into one of the other readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_liturgical_habituation, conceptual, 'Whether the liturgical habituation frame constitutes a structurally distinct constraint reading of the Nicene Creed kernel.').

omega_variable(
    assent_independence_claim,
    'Does liturgical habituation genuinely operate independent of cognitive metaphysical assent, or does performance inevitably shape assent over time (lex orandi, lex credendi)?',
    'Longitudinal study of dissident_theologians and catechumens: track whether sustained liturgical performance without initial assent leads to assent formation, or whether stable dissociation persists. Compare communities with high vs. low doctrinal enforcement.',
    'If performance shapes assent, the ''independent of assent'' claim is empirically false — the constraint has a covert transfer function (assent formation) that increases its effective extractiveness. If dissociation persists, the coordination function is genuinely assent-neutral.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(assent_independence_claim, empirical, 'Whether the creed''s identity-forming function operates without cognitive metaphysical assent.').

omega_variable(
    persecution_context_extraction_shift,
    'Does the constraint''s extractiveness remain low under persecution conditions where liturgical performance becomes a costly signal?',
    'Historical analysis of creedal recitation in persecuted churches (e.g., Soviet-era Orthodox, Chinese house churches, early church pre-Constantine). Measure whether the performance''s cost transforms the constraint from rope to snare/tangled_rope.',
    'If persecution raises ε substantially, the rope classification is context-dependent — the constraint is a rope only in religious liberty conditions. This would require a conditional classification or a separate constraint story for persecution contexts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(persecution_context_extraction_shift, empirical, 'Whether the low-extraction rope classification holds under high-cost signaling conditions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_creed_authority__liturgical_habituation_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nice_tr_t0, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(nice_tr_t10, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(nice_tr_t20, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 20, 0.23).
narrative_ontology:measurement(nice_tr_t30, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement(nice_tr_t40, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement(nice_tr_t50, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 50, 0.25).

% Extraction over time
narrative_ontology:measurement(nice_be_t0, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(nice_be_t10, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 10, 0.06).
narrative_ontology:measurement(nice_be_t20, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 20, 0.07).
narrative_ontology:measurement(nice_be_t30, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 30, 0.07).
narrative_ontology:measurement(nice_be_t40, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 40, 0.08).
narrative_ontology:measurement(nice_be_t50, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 50, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(nice_su_t0, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(nice_su_t10, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 10, 0.12).
narrative_ontology:measurement(nice_su_t20, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 20, 0.13).
narrative_ontology:measurement(nice_su_t30, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 30, 0.14).
narrative_ontology:measurement(nice_su_t40, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 40, 0.15).
narrative_ontology:measurement(nice_su_t50, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 50, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_creed_authority__liturgical_habituation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(nicene_creed_authority__liturgical_habituation_reading, 0.08).
narrative_ontology:affects_constraint(nicene_creed_authority__liturgical_habituation_reading, nicene_creed_authority__strict_orthodox_reading).
narrative_ontology:affects_constraint(nicene_creed_authority__liturgical_habituation_reading, nicene_creed_authority__symbolic_confessional_reading).

% DUAL FORMULATION NOTE:
% The nicene_creed_authority kernel decomposes into three structurally distinct constraints: (1) liturgical_habituation_reading — rope, ε≤0.10, coordination via performance; (2) strict_orthodox_reading — tangled_rope/snare, higher ε, enforcement via sanction; (3) symbolic_confessional_reading — scaffold/rope, ε variable, authority via discernment. This reading provides the social substrate (the liturgical 'we') that both metaphysical readings presuppose. The strict_orthodox reading uses the shared performance as the enforcement surface; the symbolic_confessional reading uses it as the conversation starter. ε differs because the strict reading extracts conformity, the symbolic reading extracts interpretive labor, while this reading extracts near-zero.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
