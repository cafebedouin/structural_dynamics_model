% ============================================================================
% CONSTRAINT STORY: homoousios_nicene__metaphysical_equality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homoousios_nicene__metaphysical_equality_reading, []).

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
 *   constraint_id: homoousios_nicene__metaphysical_equality_reading
 *   human_readable: Homoousios (Nicene Metaphysical Equality Reading): Secured Trinitarian Ontological Parity
 *   domain: historical_theology/ecclesiastical_authority/christological_metaphysics
 *
 * SUMMARY:
 *   The homoousios ('of one substance') doctrine, formalized at the First
 *   Council of Nicaea (325 CE), asserts that the Father and Son share an
 *   identical divine essence and possess full metaphysical equality —
 *   co-eternal, uncreated, without subordination in being. This constraint
 *   story instantiates ONE READING of the contested kernel 'homoousios
 *   Nicene.' This reading (metaphysical_equality_reading) interprets
 *   homoousios as securing genuine ontological parity: the Son's divinity is
 *   not derived, delegated, or functionally equivalent to the Father's, but
 *   identical in substance and power. The constraint exhibits Tangled Rope
 *   structure: the Nicene coalition benefits from the doctrine as an
 *   institutional power mechanism (conciliar supremacy, definitive orthodoxy,
 *   exclusion of competitors), while the doctrine also serves a genuine
 *   coordination function (preventing schism by establishing a binding
 *   doctrinal boundary) and imposes extraction costs on alternatives
 *   (suppression of subordinationist and other heterodox readings). The
 *   theater_ratio (0.68) reflects that the Council performed itself as
 *   discovering metaphysical truth while actually performing institutional
 *   settlement — the doctrine's necessity is asserted as metaphysical, but
 *   its origin is political. Suppression increases over time (0.65 → 0.72
 *   across 300 years) as the constraint becomes more institutionally embedded
 *   and alternative readings require greater suppressive force to contain.
 *   This reading coexists with sibling readings instantiating
 *   subordinationist reinterpretation and honorific similarity frameworks,
 *   all drawn from the same Nicene kernel but producing structurally distinct
 *   constraints with different beneficiary/victim configurations and
 *   different effective extractiveness profiles.
 *
 * KEY AGENTS:
 *   - Nicene Episcopal Coalition: Institutional beneficiary (institutional/arbitrage) — secures conciliar supremacy and definitive interpretive authority over Christology
 *   - Subordinationist Theology: Primary victim (powerless/trapped) — anathematized, banished, excluded from legitimate ecclesiastical voice
 *   - Heterodox Christological Traditions (Arianism, Modalism, etc.): Victims (powerless/trapped) — suppressed through conciliar decree, imperial legislation, and epistemic closure
 *   - Provincial Episcopate (Non-Coalition): Secondary victims/beneficiaries (moderate/constrained) — benefit from doctrinal clarity enabling institutional coordination; constrained by loss of theological autonomy
 *   - Conciliar Authority Institution: Institutional beneficiary (institutional/constrained) — derives legitimacy from homoousios as metaphysical discovery; constrained by need to suppress reinterpretations that threaten the doctrine's univocity
 *   - Epistemic Pluralism in Trinitarian Interpretation: Diffuse victim (analytical/trapped) — homoousios constraint systematically forecloses alternative frameworks for reasoning about Trinity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_nicene__metaphysical_equality_reading, 0.58).
domain_priors:suppression_score(homoousios_nicene__metaphysical_equality_reading, 0.72).
domain_priors:theater_ratio(homoousios_nicene__metaphysical_equality_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_nicene__metaphysical_equality_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_nicene__metaphysical_equality_reading, "Homoousios (Nicene Metaphysical Equality Reading): Secured Trinitarian Ontological Parity").
narrative_ontology:topic_domain(homoousios_nicene__metaphysical_equality_reading, "historical_theology/ecclesiastical_authority/christological_metaphysics").

domain_priors:requires_active_enforcement(homoousios_nicene__metaphysical_equality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_nicene__metaphysical_equality_reading, 'ce102a15-1d58-4dc5-8ac1-22a20b001c2d').
narrative_ontology:cs_kernel_codification('ce102a15-1d58-4dc5-8ac1-22a20b001c2d', formalized).
narrative_ontology:cs_authority_grounding('ce102a15-1d58-4dc5-8ac1-22a20b001c2d', lineage).
narrative_ontology:cs_interpretation_layer_present('ce102a15-1d58-4dc5-8ac1-22a20b001c2d').
narrative_ontology:cs_reading_relation('ce102a15-1d58-4dc5-8ac1-22a20b001c2d', homoousios_nicene__subordinationist_reading, influences).
narrative_ontology:cs_reading_relation('ce102a15-1d58-4dc5-8ac1-22a20b001c2d', homoousios_nicene__honorific_similarity_reading, coexists_with).
narrative_ontology:cs_axiom('ce102a15-1d58-4dc5-8ac1-22a20b001c2d', foundational, father_son_ontological_identity).
narrative_ontology:cs_axiom_status(father_son_ontological_identity, holdable).
narrative_ontology:cs_axiom_grounding('ce102a15-1d58-4dc5-8ac1-22a20b001c2d', father_son_ontological_identity, deontological).
narrative_ontology:cs_axiom('ce102a15-1d58-4dc5-8ac1-22a20b001c2d', foundational, conciliar_metaphysical_authority).
narrative_ontology:cs_axiom_status(conciliar_metaphysical_authority, holdable).
narrative_ontology:cs_axiom_grounding('ce102a15-1d58-4dc5-8ac1-22a20b001c2d', conciliar_metaphysical_authority, conventional).
narrative_ontology:cs_reference_frame('ce102a15-1d58-4dc5-8ac1-22a20b001c2d', nicene_metaphysical_orthodoxy).
narrative_ontology:cs_drift_state('ce102a15-1d58-4dc5-8ac1-22a20b001c2d', post_reformation_critical_theology, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ce102a15-1d58-4dc5-8ac1-22a20b001c2d', '2026-02-26T14:22:33Z').
narrative_ontology:cs_kernel_id(homoousios_nicene__metaphysical_equality_reading, homoousios_nicene).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_nicene__metaphysical_equality_reading, nicene_episcopal_coalition).
narrative_ontology:constraint_beneficiary(homoousios_nicene__metaphysical_equality_reading, conciliar_authority_institution).
narrative_ontology:constraint_victim(homoousios_nicene__metaphysical_equality_reading, heterodox_christological_traditions).
narrative_ontology:constraint_victim(homoousios_nicene__metaphysical_equality_reading, subordinationist_theology).
narrative_ontology:constraint_victim(homoousios_nicene__metaphysical_equality_reading, epistemic_pluralism_in_trinitarian_interpretation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBORDINATIONIST COMMUNITY (SNARE) — Trapped within the post-Nicene ecclesiastical order. Anathematized at the Council (325 CE) and repeatedly reconfirmed in successor councils. Exit from homoousios framework requires apostasy from recognized Christianity itself. Maximum suppression: banishment, confiscation, exile. No coordination benefit — pure extraction of doctrinal submission.
constraint_indexing:constraint_classification(homoousios_nicene__metaphysical_equality_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: PROVINCIAL EPISCOPATE (TANGLED ROPE) — Faces genuine coordination benefit (unified Christological doctrine prevents schism) and real extraction (homoousios doctrine requires epistemic submission to the Nicene definition, constraining legitimate theological exploration). High cost to exit: lose ecclesiastical standing and communion. But also benefits from the constraint — reduces ambiguity, enables institutional alignment, provides clear doctrinal boundary for orthodoxy.
constraint_indexing:constraint_classification(homoousios_nicene__metaphysical_equality_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: NICENE EPISCOPAL COALITION (ROPE) — Primary beneficiary. The homoousios doctrine secures the interpretive power of the Nicene coalition relative to alternative Christologies. Arbitrage exit: dissenting bishops can acquiesce to homoousios without cost (communion maintained, status preserved). Net benefit massively favors this coalition — the constraint is their victory in distributing conciliar authority. Coordination function is genuine: enables unified ecclesiastical discipline.
constraint_indexing:constraint_classification(homoousios_nicene__metaphysical_equality_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: CONCILIAR AUTHORITY INSTITUTION (TANGLED ROPE) — Organized institutional actor. Benefits from homoousios as the mechanism that anchors conciliar supremacy in doctrinal necessity (the Council discovered metaphysical truth, not merely enacted preference). Extraction: the institution claims interpretive monopoly over Trinitarian ontology, foreclosing heterodox alternatives. Constrained exit because the institution's legitimacy is constitutively tied to homoousios — abandoning the doctrine dissolves its authority. High theater: claims discovery of eternal metaphysical truth while performing institutional power consolidation.
constraint_indexing:constraint_classification(homoousios_nicene__metaphysical_equality_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: THEOLOGICAL HISTORIAN ANALYZING PITON DEGRADATION (PITON) — From outside the faith commitment, the claim that homoousios expresses an immutable metaphysical truth appears to be a naturalization of a contingent institutional settlement. The theater_ratio (0.68) reflects this: the Council performed itself as discovering eternal truth while actually performing power consolidation. The constraint persists through institutional inertia and creedal recitation, not because the metaphysical argument compels assent. The historian sees performative maintenance of doctrine rather than living metaphysical conviction.
constraint_indexing:constraint_classification(homoousios_nicene__metaphysical_equality_reading, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: SYSTEMATIC THEOLOGIAN (MOUNTAIN - FALSE SUMMIT CANDIDATE) — From within the tradition's systematic requirements, homoousios expresses a logical necessity: if God is the absolute and the Logos is divine, they must share absolute divinity — any subordination contradicts omnipotence. This perspective sees homoousios as deriving from rational metaphysics rather than from the Council's political settlement. However, this view naturalizes what is actually a choice point in Trinitarian logic: one could instead argue (as Arius did) that absolute monotheism requires subordination. The mountain classification reveals the false summit: metaphysical necessity that dissolves when the foundational assumptions are made explicit.
constraint_indexing:constraint_classification(homoousios_nicene__metaphysical_equality_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_nicene__metaphysical_equality_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(homoousios_nicene__metaphysical_equality_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(homoousios_nicene__metaphysical_equality_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(homoousios_nicene__metaphysical_equality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(homoousios_nicene__metaphysical_equality_reading, TR),
    TR >= 0.70.

:- end_tests(homoousios_nicene__metaphysical_equality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The Nicene metaphysical equality reading secures substantial benefit for the coalition (institutional power, interpretive monopoly) while imposing costs on alternatives (suppression, anathema, exile). The extraction is not maximal (0.70+) because homoousios also serves genuine coordination functions — the doctrine does prevent Christological fragmentation and enables institutional unity. The extraction is not minimal (0.30) because the coordination could be achieved through alternative frameworks (subordinationism, honorific similarity) that the Council explicitly rejected, suggesting the choice of homoousios was driven partly by institutional advantage rather than purely by metaphysical necessity. Suppression (0.72): High. Post-Nicene history shows systematic suppression of heterodox alternatives through imperial legislation (exile, confiscation), exclusion from ecclesiastical office, anathema, and epistemological gatekeeping. Subordinationist arguments are not merely disagreed with but actively eliminated from legitimate theological discourse. Suppression increases over time as the constraint becomes institutionally entrenched — later councils and imperial decrees are required to maintain the suppression. Theater ratio (0.68): Moderately high. The constraint exhibits significant performative content. The Council claims to discover eternal metaphysical truth (the Father and Son always possessed homoousios equality), but the historical record shows the doctrine emerging through political negotiation, coalition-building, and institutional power consolidation. The post-Nicene interpretive tradition engages in continuous reinterpretation (Cappadocian Fathers introducing hypostases terminology; later theologians emphasizing the Father's monarchy) to adapt the doctrine to changing philosophical contexts, suggesting the original formulation was less metaphysically necessary than claimed. However, the theater is not complete — homoousios does function as a genuine doctrinal boundary enabling institutional coherence.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates radical perspectival divergence between beneficiary and victim contexts. The Nicene coalition sees rope (coordination mechanism enabling institutional unity without perceiving extraction as problematic). The subordinationist sees snare (trapped, anathematized, suppressed with no exit). The provincial episcopate sees tangled rope (genuine coordination benefit coupled with real epistemic constraint). The conciliar institution sees tangled rope (benefits from the doctrine's institutional function while constrained by the need to suppress reinterpretation that would dissolve the doctrine's univocity). The theological historian sees piton (performative maintenance of a doctrine whose necessity is asserted but not rationally compelled). The systematic theologian sees mountain (homoousios as metaphysical necessity flowing from the Christian commitment to monotheism and Logos theology). The perspectival gap between mountain and piton is particularly diagnostic: the same doctrine appears as both an immutable metaphysical truth and as institutional theater, revealing that 'metaphysical necessity' is the cover story for institutional settlement.
 *
 * DIRECTIONALITY LOGIC:
 *   The Nicene coalition occupies the position of beneficiary with arbitrage exit: they can maintain homoousios without cost (it is their victory) and can even reinterpret it with institutional flexibility (hypostases, economia, etc.). Their d-value is low (≈0.15), yielding low or slightly negative χ. Subordinationists occupy the position of victims with trapped exit: anathema and imperial suppression close all paths to legitimate theological voice within Christianity; apostasy is the only escape. Their d-value is high (≈0.95), yielding maximum χ. The provincial episcopate are moderate agents with constrained exit: they face career and communion costs if they reject homoousios (constrained exit), but also derive coordination benefits from the doctrine. Their d-value is middle-range (≈0.60), yielding moderate χ. The conciliar institution is institutional with constrained exit: abandoning homoousios would dissolve the institution's legitimacy claim (identity-locked exit dimension). The directional dynamics create the tangled rope classification: high suppression (0.72) coupled with genuine coordination function and institutional benefit distribution that depends on the doctrine's binding power.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metaphysical_necessity_vs_institutional_choice,
    'Does homoousios express a logical necessity following from Christian monotheism and Logos theology, or does it represent a contingent institutional choice among defensible alternatives?',
    'Comparative analysis of Trinitarian logic across competing Christologies: Arianism, Subordinationism, Modalism, and Homoousios. Identification of whether the core monotheistic commitment entails homoousios uniquely or whether multiple consistent formulations exist.',
    'If metaphysically necessary: Mountain classification is correct; conciliar authority merely discovered and articulated pre-existing truth. If contingent choice: False summit detected; the mountain is a naturalization of institutional power consolidation disguised as metaphysical discovery.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(metaphysical_necessity_vs_institutional_choice, conceptual, 'Whether homoousios is metaphysically necessary or institutionally contingent').

omega_variable(
    kernel_stability_across_reading_frameworks,
    'Does the Nicene homoousios maintain a stable, univocal meaning across the competing reading traditions (metaphysical equality, subordinationist reinterpretation, honorific similarity), or does the kernel term collapse into equivocation?',
    'Diachronic textual analysis of homoousios usage across post-Nicene councils (Constantinople 381, Chalcedon 451, Constantinople 553, etc.). Tracking whether bishops could genuinely affirm homoousios while interpreting it in incompatible ways (e.g., some read as metaphysical identity, others as functional similarity).',
    'If univocal: the kernel is stable; the three readings are genuine alternatives and the constraint exhibits genuine structure. If equivocal: the homoousios is a shared label masking incommensurable commitments; the constraint''s coherence is illusory and enforced entirely through suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_stability_across_reading_frameworks, empirical, 'Whether homoousios maintains semantic stability across reading traditions').

omega_variable(
    suppression_mechanism_sources,
    'To what degree does suppression of heterodox readings derive from (a) metaphysical conviction that homoousios is true and alternatives are false, vs (b) institutional power maintenance by the Nicene coalition?',
    'Comparative study of suppression intensity and mechanism across different periods: early post-Nicene (political exile), later medieval (doctrinal disputation without state violence), post-Reformation (theological argument). If suppression source is (a), intensity should reflect perception of doctrinal threat. If source is (b), intensity should correlate with institutional threat to episcopal hierarchy.',
    'If (a) dominates: suppression is legitimate defense of metaphysical truth; the constraint is justified extraction. If (b) dominates: suppression is institutional self-protection; the constraint is a snare disguised as truth-seeking.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_sources, empirical, 'Attribution of suppression to metaphysical conviction vs institutional power maintenance').

omega_variable(
    foreclose_status_of_subordinationism,
    'Does the metaphysical equality reading foreclose the subordinationist reading logically (both cannot be true in any coherent framework), or do they coexist as alternative readings held by different parties?',
    'Formal logical analysis of core commitments: (Metaphysical Equality) Father and Son possess identical divine attributes and are eternally co-equal in being. (Subordinationism) Father possesses attributes distinctly; Son derives existence from Father and possesses derivative status. Do these admit of a coherent joint framework, or does affirming one require denying the other?',
    'If foreclose: the reading_relations entry should be ''forecloses''. If coexist: should be ''coexists_with''. Determines whether the Nicene victory was a logical necessity or a political settlement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(foreclose_status_of_subordinationism, conceptual, 'Whether metaphysical equality logically forecloses subordinationism').

omega_variable(
    post_nicene_reinterpretation_drift,
    'Have post-Nicene theological traditions systematically reinterpreted homoousios in ways that approximate the subordinationist reading without formally abandoning homoousios (e.g., through emphasizing the Father''s monarchy, the Son''s dependence on the Father''s will, or functional hierarchy)?',
    'Textual tracking of homoousios interpretation across Nicene, Cappadocian, Thomistic, and Reformation theology. Measurement of doctrinal distance from the original 325 CE Nicene definition. Identification of whether ''living tradition'' interpretation has systematized drift toward subordinationism.',
    'If substantial drift detected: the constraint shows mandatrophy dynamics; suppression must increase to maintain the fiction of semantic stability. If minimal drift: the tradition maintains genuine continuity with Nicene intent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_nicene_reinterpretation_drift, empirical, 'Degree of post-Nicene reinterpretation drift from original homoousios definition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_nicene__metaphysical_equality_reading, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_meta_theater_0, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(homo_meta_theater_150, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 150, 0.62).
narrative_ontology:measurement(homo_meta_theater_300, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 300, 0.68).

% Extraction over time
narrative_ontology:measurement(homo_meta_extract_0, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(homo_meta_extract_150, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 150, 0.55).
narrative_ontology:measurement(homo_meta_extract_300, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 300, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(homo_meta_suppress_0, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(homo_meta_suppress_150, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 150, 0.7).
narrative_ontology:measurement(homo_meta_suppress_300, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 300, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_nicene__metaphysical_equality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(homoousios_nicene__metaphysical_equality_reading, homoousios_nicene__subordinationist_reading).
narrative_ontology:affects_constraint(homoousios_nicene__metaphysical_equality_reading, homoousios_nicene__honorific_similarity_reading).
narrative_ontology:affects_constraint(homoousios_nicene__metaphysical_equality_reading, trinity_doctrinal_monopoly).
narrative_ontology:affects_constraint(homoousios_nicene__metaphysical_equality_reading, nicene_conciliar_authority).

% DUAL FORMULATION NOTE:
% The homoousios Nicene is a kernel with three structurally distinct constraint readings, each instantiating a different epsilon and beneficiary/victim configuration. The metaphysical_equality_reading (this constraint) ε=0.58, prioritizes metaphysical identity and institutional supremacy of Nicene coalition. The subordinationist_reading reinterprets the same kernel to minimize extraction while nominally assenting to homoousios; distinct epsilon reflecting lower institutional enforcement. The honorific_similarity_reading interprets homoousios as functional parity in worship; distinct epsilon and different victim set. All three share the same kernel (the Council's formulaic homoousios) but produce different constraints because they resolve the kernel's ambiguity in incompatible ways. Link all sibling stories via network.affects_constraints. The metaphysical equality reading is upstream (historically primary, provides the institutional pressure that makes the other readings necessary as reinterpretations).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(homoousios_nicene__metaphysical_equality_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
