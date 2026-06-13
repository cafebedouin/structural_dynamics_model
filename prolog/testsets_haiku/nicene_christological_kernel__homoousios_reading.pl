% ============================================================================
% CONSTRAINT STORY: nicene_christological_kernel__homoousios_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_christological_kernel__homoousios_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: nicene_christological_kernel__homoousios_reading
 *   human_readable: Nicene Homoousios Christological Doctrine
 *   domain: ecclesiastical/theological
 *
 * SUMMARY:
 *   The First Council of Nicaea (325 CE) mandates that Christ is homoousios
 *   (of the same divine substance) with God the Father, establishing full
 *   equality of the two persons within the Trinity. This doctrine is enforced
 *   through anathema (excommunication), exile, property confiscation, and
 *   imperial legislation against alternative readings, particularly Arianism
 *   (homoiousios: similar but distinct substance). The constraint operates as
 *   an ecclesiastical-imperial coordination mechanism: the council
 *   establishes doctrinal uniformity, the empire supplies enforcement
 *   machinery, and both institutions benefit from consolidated religious
 *   authority. The victims are theological diversity (suppressed readings),
 *   regional ecclesiastical autonomy (subordinated to ecumenical authority),
 *   and Arian and other minority communities (directly persecuted). The
 *   founding problem — genuine Christological ambiguity — is substantially
 *   resolved by the 5th century through multiple councils and imperial
 *   legislation, yet the constraint persists: extractiveness and suppression
 *   both rise during the interval (325–381), suggesting the constraint
 *   continues beyond its original coordination function. This story
 *   instantiates ONE READING of the contested Nicene Christological kernel;
 *   the homoiousios reading produces a sibling constraint (different story,
 *   same kernel) with different victim sets and extractiveness profile.
 *
 * KEY AGENTS:
 *   - Ecumenical Council Authority: Assembly of bishops with power to define doctrine; benefits from jurisdictional consolidation and doctrinal control; enforces homoousios through anathema and institutional suppression.
 *   - Imperial Ecclesiastical Alignment: Roman state (Constantine, Theodosius I) provides legislative and coercive enforcement (exile, confiscation, prohibition); benefits from religious uniformity as tool of political control.
 *   - Gothic Arian Communities: Organized Christian populations holding Arian Christology; directly victimized by anathema, exile, property confiscation, and exclusion from councils.
 *   - Regional Autonomous Traditions: Semi-independent regional churches (North African, Syrian, Egyptian); identity-locked victims (rejecting homoousios means breaking communion with universal church); lose interpretive autonomy.
 *   - Homoiousios Advocates: Theologians and bishops holding the similar-substance reading (not same-substance); excluded from post-Nicene councils; numerically significant but institutionally suppressed.
 *   - Theological Diversity Parties: Minority Christologies (Nestorian, Apollinarian, other readings); powerless victims; suppressed through institutional exclusion and anathema.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_christological_kernel__homoousios_reading, 0.78).
domain_priors:suppression_score(nicene_christological_kernel__homoousios_reading, 0.81).
domain_priors:theater_ratio(nicene_christological_kernel__homoousios_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_christological_kernel__homoousios_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoousios_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoousios_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_christological_kernel__homoousios_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoousios_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_christological_kernel__homoousios_reading, tangled_rope).
narrative_ontology:human_readable(nicene_christological_kernel__homoousios_reading, "Nicene Homoousios Christological Doctrine").
narrative_ontology:topic_domain(nicene_christological_kernel__homoousios_reading, "ecclesiastical/theological").

domain_priors:requires_active_enforcement(nicene_christological_kernel__homoousios_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_christological_kernel__homoousios_reading, '7dcf2872-629a-48ab-9ab4-381de1b0b93e').
narrative_ontology:cs_kernel_codification('7dcf2872-629a-48ab-9ab4-381de1b0b93e', formalized).
narrative_ontology:cs_authority_grounding('7dcf2872-629a-48ab-9ab4-381de1b0b93e', lineage).
narrative_ontology:cs_interpretation_layer_present('7dcf2872-629a-48ab-9ab4-381de1b0b93e').
narrative_ontology:cs_reading_relation('7dcf2872-629a-48ab-9ab4-381de1b0b93e', nicene_christological_kernel__homoiousios_reading, forecloses).
narrative_ontology:cs_axiom('7dcf2872-629a-48ab-9ab4-381de1b0b93e', foundational, christ_same_divine_substance).
narrative_ontology:cs_axiom_status(christ_same_divine_substance, holdable).
narrative_ontology:cs_axiom_grounding('7dcf2872-629a-48ab-9ab4-381de1b0b93e', christ_same_divine_substance, deontological).
narrative_ontology:cs_axiom('7dcf2872-629a-48ab-9ab4-381de1b0b93e', foundational, salvation_requires_full_divinity).
narrative_ontology:cs_axiom_status(salvation_requires_full_divinity, holdable).
narrative_ontology:cs_axiom_grounding('7dcf2872-629a-48ab-9ab4-381de1b0b93e', salvation_requires_full_divinity, deontological).
narrative_ontology:cs_reference_frame('7dcf2872-629a-48ab-9ab4-381de1b0b93e', apostolic_christological_continuity).
narrative_ontology:cs_drift_state('7dcf2872-629a-48ab-9ab4-381de1b0b93e', post_constantinopolitan_codification_381, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7dcf2872-629a-48ab-9ab4-381de1b0b93e', '').
narrative_ontology:cs_kernel_id(nicene_christological_kernel__homoousios_reading, nicene_christological_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoousios_reading, ecumenical_council_authority).
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoousios_reading, imperial_ecclesiastical_alignment).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoousios_reading, gothic_arian_communities).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoousios_reading, regional_autonomous_traditions).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoousios_reading, theological_diversity_parties).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_christological_kernel__homoousios_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(nicene_christological_kernel__homoousios_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_christological_kernel__homoousios_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nicene_christological_kernel__homoousios_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nicene_christological_kernel__homoousios_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.62 (325) to 0.78 (381) because the doctrine's enforcement mechanisms intensify: initial conciliar declaration (Nicaea, 325) is followed by imperial legislation (Constantine's exile of Arius, confiscation of Arian church property), repeated councils reinforcing the doctrine, and finally imperial prohibition of alternative doctrine (Theodosius I, 380–381). Suppression rises from 0.55 to 0.81 on the same trajectory: initial suppression is institutional (anathema, council exclusion); by 381, suppression includes state-level coercion (exile, legal prohibition). Theater rises from 0.22 to 0.42 because the constraint's original coordination function (resolving genuine doctrinal ambiguity about Christ's nature) is substantially complete by 355–365, yet enforcement intensifies. By 381, the constraint exhibits piton characteristics (rising theater as primary function atrophies), but high suppression (0.81) prevents full piton classification — the constraint remains tangled_rope, with asymmetric extraction (beneficiaries: ecumenical council, imperial apparatus; victims: Arian communities, regional traditions, theological diversity). The measurements use a single shared time grid (every metric at every point: 325, 335, 345, 355, 365, 375, 381) so temporal analysis is aligned.
 *
 * PERSPECTIVAL GAP:
 *   The ecumenical council and imperial apparatus compute this constraint very differently from the suppressed victims. From the council's seat, homoousios is genuine coordination (unifying doctrine, enabling communion, preserving salvation truth); from the Arian and regional tradition seats, the same constraint is enforced extraction (suppression of legitimate theological alternatives, consolidation of institutional power at the expense of regional autonomy). The engine computes directionality from beneficiary/victim declarations (council and empire as beneficiaries; Arians, regional traditions, theological diversity as victims) and produces per-seat classifications that reflect this asymmetry. The agenda-setter seat (council authority) and the payer seats (victims of suppression) should produce divergent type assessments.
 *
 * DIRECTIONALITY LOGIC:
 *   Ecumenical Council Authority: beneficiary seat (benefits from doctrinal control, jurisdictional consolidation, institutional authority); d near 0.0 (full beneficiary; exits include council dissent but are expensive — institutional reputation, loss of authority). Imperial Ecclesiastical Alignment: beneficiary seat (benefits from religious uniformity, political control mechanism); d near 0.0 (full beneficiary; arbitrage exit via policy change but maintains other sources of power). Gothic Arian Communities: victim seat (pays through anathema, exile, property loss, exclusion from councils); d near 1.0 (full target; trapped exit — rejecting homoousios means losing Christian community entirely). Regional Autonomous Traditions: victim seat (pays through subordination to ecumenical authority, loss of interpretive autonomy); d near 0.8 (primarily target; identity-locked exit — local tradition fused with regional identity). Theological Diversity Parties: victim seat (pays through suppression of alternative readings); d near 0.9 (full target; constrained exit — private belief possible but no public voice). Homoiousios Advocates: victim seat (excluded from councils, anathematized); d near 0.85 (target; trapped exit — holding homoiousios after Nicaea means breaking communion). The directionality spread (beneficiaries at d ~0.1; victims at d ~0.8–0.9) is the key to the asymmetric extraction profile.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (genuine Christological ambiguity about Christ's relationship to God the Father) is classified as DEAD: multiple councils (Nicaea 325, Constantinople 381) and imperial legislation establish homoousios as binding orthodoxy, settle the ambiguity, and enable doctrinal communion. By 381, the founding problem is substantially resolved across the church (even dissenting communities accept homoousios as the official doctrine). Yet the constraint persists and extractiveness rises (0.62 to 0.78) during the interval — the disappearance_verdict is world_rearranges (without homoousios enforcement, regional churches resume autonomous traditions, homoiousios resurfaces, theological diversity reorganizes). This combination (founding_problem_status=dead + disappearance_verdict=world_rearranges) triggers the mandatrophy flag: the constraint's original justification (resolving doctrinal ambiguity) is obsolete, but the constraint persists because it benefits institutional authority. The theater ratio (rising from 0.22 to 0.42) captures this dynamic: enforcement that once served genuine coordination increasingly performs doctrinal consolidation as the founding problem recedes. This is institutional inertia with extractive consequences — a zombie constraint maintained by beneficiaries (council authority, imperial apparatus) against suppressed victims even after its original function is dead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Is homoousios (same substance) the only defensible Christological reading of the person of Christ, or is homoiousios (similar substance) an equally coherent alternative theological position?',
    'Comparative theological analysis of the logical structure, metaphysical coherence, and scriptural warrant of both readings. Assessment of whether the doctrinal debate was resolved by theological argument or by institutional suppression. Post-Reformation theological scholarship provides substantial evidence.',
    'If homoiousios is equally defensible, the homoousios ruling is institutional power consolidation disguised as doctrinal discovery; if homoousios uniquely preserves Christian truth, the enforcement serves a real coordination function. The reading''s standing depends on this distinction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Whether the homoousios reading is the unique coherent Christological position or one reading among live alternatives.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.81 at interval end) structural (exile, anathema, property confiscation, legislative prohibition) or internalized (theological conviction that homoousios is truth, adopted by suppressed parties themselves)?',
    'Post-suppression trajectory analysis: Do suppressed communities (Arians, Gothic Christians, regional traditions) maintain homoiousios conviction after external enforcement is removed (5th century onwards)? If conviction persists, suppression was largely structural; if it dissolves, suppression had internalized the doctrine. Historical records show homoousios doctrine persists even after imperial suppression machinery weakens (5th-6th centuries), but Arian populations maintain their readings in isolated communities, suggesting suppression was predominantly structural.',
    'If structural, the constraint''s effective suppression is the measured value — external coercion. If internalized, suppressed parties carry the suppression with them even after exit, and the constraint''s effective extractiveness is higher than structural measures suggest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression operates through external coercion or has been internalized as conviction.').

omega_variable(
    ecumenical_vs_imperial_agency,
    'Who is the primary agent enforcing homoousios orthodoxy: the ecumenical councils (ecclesiastical authority) or the imperial apparatus (state coercion)?',
    'Timeline analysis of enforcement: Nicaea (325) is conciliar; Constantine''s subsequent legislation (exile of Arius, confiscation of Arian church property) is imperial. Constantinople (381) legislates with imperial backing. The constraint''s persistence depends on which institution — if imperial enforcement were withdrawn, would councils maintain the doctrine? If councils were dissolved, would imperial legislation persist? Historical evidence shows both are necessary; neither alone sustains the constraint.',
    'If councils are primary, the constraint is ecclesiastical institutional coordination with imperial support (tangled_rope from the ecclesiastical seat). If imperial apparatus is primary, the constraint is state enforcement of a doctrinal preference (snare from the suppressed seats). The directionality diverges by agency assignment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecumenical_vs_imperial_agency, empirical, 'Whether homoousios enforcement is primarily ecclesiastical governance or imperial state coercion.').

omega_variable(
    kernel_reading_alternative_framing,
    'If the Nicene kernel is interpreted through a homoiousios (similar substance) reading rather than homoousios (same substance), does that alternative reading produce a structurally different constraint with different extractiveness, beneficiaries, and victims?',
    'Generate a parallel constraint story instantiating the homoiousios reading of the same kernel and compare the base properties, beneficiary/victim sets, and measured extractiveness. The homoiousios_reading story is the sibling constraint in the kernel family. If the two readings produce markedly different ε values, they are genuinely different constraints; if they produce similar extractiveness with different victim sets, the kernel issue is victim selection, not constraint type.',
    'The presence of this omega documents the committer frame: this story instantiates ONE reading of a contested kernel. The sibling reading produces a different constraint story. Both are valid analytically; the kernel contest determines which one the historical church instantiated. The corpus gains two constraint stories from one kernel, linked via network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_alternative_framing, conceptual, 'Sibling reading (homoiousios) of the Nicene Christological kernel produces a distinct constraint with different structural properties.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_christological_kernel__homoousios_reading, 325, 381).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nice_tr_t325, nicene_christological_kernel__homoousios_reading, theater_ratio, 325, 0.22).
narrative_ontology:measurement(nice_tr_t335, nicene_christological_kernel__homoousios_reading, theater_ratio, 335, 0.26).
narrative_ontology:measurement(nice_tr_t345, nicene_christological_kernel__homoousios_reading, theater_ratio, 345, 0.31).
narrative_ontology:measurement(nice_tr_t355, nicene_christological_kernel__homoousios_reading, theater_ratio, 355, 0.35).
narrative_ontology:measurement(nice_tr_t365, nicene_christological_kernel__homoousios_reading, theater_ratio, 365, 0.38).
narrative_ontology:measurement(nice_tr_t375, nicene_christological_kernel__homoousios_reading, theater_ratio, 375, 0.41).
narrative_ontology:measurement(nice_tr_t381, nicene_christological_kernel__homoousios_reading, theater_ratio, 381, 0.42).

% Extraction over time
narrative_ontology:measurement(nice_be_t325, nicene_christological_kernel__homoousios_reading, base_extractiveness, 325, 0.62).
narrative_ontology:measurement(nice_be_t335, nicene_christological_kernel__homoousios_reading, base_extractiveness, 335, 0.68).
narrative_ontology:measurement(nice_be_t345, nicene_christological_kernel__homoousios_reading, base_extractiveness, 345, 0.71).
narrative_ontology:measurement(nice_be_t355, nicene_christological_kernel__homoousios_reading, base_extractiveness, 355, 0.75).
narrative_ontology:measurement(nice_be_t365, nicene_christological_kernel__homoousios_reading, base_extractiveness, 365, 0.77).
narrative_ontology:measurement(nice_be_t375, nicene_christological_kernel__homoousios_reading, base_extractiveness, 375, 0.78).
narrative_ontology:measurement(nice_be_t381, nicene_christological_kernel__homoousios_reading, base_extractiveness, 381, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(nice_su_t325, nicene_christological_kernel__homoousios_reading, suppression_requirement, 325, 0.55).
narrative_ontology:measurement(nice_su_t335, nicene_christological_kernel__homoousios_reading, suppression_requirement, 335, 0.62).
narrative_ontology:measurement(nice_su_t345, nicene_christological_kernel__homoousios_reading, suppression_requirement, 345, 0.68).
narrative_ontology:measurement(nice_su_t355, nicene_christological_kernel__homoousios_reading, suppression_requirement, 355, 0.74).
narrative_ontology:measurement(nice_su_t365, nicene_christological_kernel__homoousios_reading, suppression_requirement, 365, 0.78).
narrative_ontology:measurement(nice_su_t375, nicene_christological_kernel__homoousios_reading, suppression_requirement, 375, 0.8).
narrative_ontology:measurement(nice_su_t381, nicene_christological_kernel__homoousios_reading, suppression_requirement, 381, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_christological_kernel__homoousios_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(nicene_christological_kernel__homoousios_reading, 0.12).
narrative_ontology:affects_constraint(nicene_christological_kernel__homoousios_reading, nicene_christological_kernel__homoiousios_reading).

% DUAL FORMULATION NOTE:
% The Nicene Christological kernel (325 CE onwards) decomposes into two structurally distinct constraints: the homoousios reading (this story) and the homoiousios reading (sibling constraint). Both emerge from the same conciliar text and historical contest but instantiate different ε values, beneficiary/victim sets, and enforcement profiles. Homoousios enforces doctrinal uniformity through institutional suppression of alternatives, producing high extractiveness (0.78), clear beneficiaries (ecumenical council, imperial apparatus), and organized victims (Arian communities, regional traditions). Homoiousios preserves theological diversity while maintaining Christian communion, producing lower extractiveness, distributed beneficiaries, and minimal victims. The readings foreclose each other within a single ecclesiastical framework (you cannot hold both simultaneously coherently) but coexist across different historical parties and regional communities. Generate both constraint stories separately per ε-invariance discipline; link them via network.affects_constraints. The corpus gains two distinct constraints from one historical kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nicene_christological_kernel__homoousios_reading, institutional, 0.08).
constraint_indexing:directionality_override(nicene_christological_kernel__homoousios_reading, organized, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
