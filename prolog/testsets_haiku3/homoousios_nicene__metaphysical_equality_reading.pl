% ============================================================================
% CONSTRAINT STORY: homoousios_nicene__metaphysical_equality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: homoousios_nicene__metaphysical_equality_reading
 *   human_readable: Homoousios Metaphysical Equality (Nicene Reading)
 *   domain: historical theology / ecclesiastical authority
 *
 * SUMMARY:
 *   The Council of Nicaea (325 CE) establishes homoousios (same divine
 *   essence) as the binding metaphysical doctrine of Trinitarian equality:
 *   the Father and Son share the same being, are co-eternal, and admit no
 *   subordination. This reading forecloses the Arian subordinationist reading
 *   (Son as highest creation but not eternal) and marginalizes the
 *   honorific-similarity reading (homoousios as likeness, not identity). The
 *   constraint enforces metaphysical precision through anathematization,
 *   exile of heterodox bishops, book-burning, and imperial law. The Nicene
 *   epistemic hierarchy claims conciliar authority to interpret scripture and
 *   define orthodoxy; beneficiaries collect interpretive monopoly and
 *   institutional power; victims bear anathematization and erasure. The
 *   measurement series traces extraction accumulation and suppression
 *   hardening from Nicaea through Late Antiquity as the metaphysical reading
 *   consolidates into unquestioned orthodoxy.
 *
 * KEY AGENTS:
 *   - Nicene episcopal hierarchy: defines and enforces the metaphysical equality reading through conciliar authority and imperial backing.
 *   - Arian subordinationist theologians: defend the scriptural compatibility of functional/ontological subordination; anathematized and exiled.
 *   - Honorific-similarity adherents: occupy middle ground between Nicene and Arian; suppressed as heretics despite occupying a seemingly coherent position.
 *   - Christian laity and local churches: receive creedal unity and coherent Christology; bear identity-lock cost of enforcing conformity through liturgical confession and loyalty tests.
 *   - Imperial secular authority (Constantine et al.): convene and enforce the council; benefit from religious unity; become inseparable from orthodox doctrine.
 *   - Scriptural realist interpreters: excluded from the conciliar framework; their objection that homoousios imports metaphysics into scripture is never heard.
 *   - Later revisionist councils and theologians: inherit and test the constraint; observe whether it is genuine coordination or contingent extraction of power.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_nicene__metaphysical_equality_reading, 0.68).
domain_priors:suppression_score(homoousios_nicene__metaphysical_equality_reading, 0.79).
domain_priors:theater_ratio(homoousios_nicene__metaphysical_equality_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, accessibility_collapse, 0.81).
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_nicene__metaphysical_equality_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_nicene__metaphysical_equality_reading, "Homoousios Metaphysical Equality (Nicene Reading)").
narrative_ontology:topic_domain(homoousios_nicene__metaphysical_equality_reading, "historical theology / ecclesiastical authority").

domain_priors:requires_active_enforcement(homoousios_nicene__metaphysical_equality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_nicene__metaphysical_equality_reading, '1eadd6f2-253f-4d0f-80fc-670dfea236b4').
narrative_ontology:cs_kernel_codification('1eadd6f2-253f-4d0f-80fc-670dfea236b4', formalized).
narrative_ontology:cs_authority_grounding('1eadd6f2-253f-4d0f-80fc-670dfea236b4', extraction).
narrative_ontology:cs_interpretation_layer_present('1eadd6f2-253f-4d0f-80fc-670dfea236b4').
narrative_ontology:cs_reading_relation('1eadd6f2-253f-4d0f-80fc-670dfea236b4', homoousios_nicene__subordinationist_reading, forecloses).
narrative_ontology:cs_reading_relation('1eadd6f2-253f-4d0f-80fc-670dfea236b4', homoousios_nicene__honorific_similarity_reading, forecloses).
narrative_ontology:cs_axiom('1eadd6f2-253f-4d0f-80fc-670dfea236b4', foundational, father_son_ontological_equality).
narrative_ontology:cs_axiom_status(father_son_ontological_equality, holdable).
narrative_ontology:cs_axiom_grounding('1eadd6f2-253f-4d0f-80fc-670dfea236b4', father_son_ontological_equality, deontological).
narrative_ontology:cs_axiom('1eadd6f2-253f-4d0f-80fc-670dfea236b4', foundational, conciliar_authority_doctrinal_definition).
narrative_ontology:cs_axiom_status(conciliar_authority_doctrinal_definition, holdable).
narrative_ontology:cs_axiom_grounding('1eadd6f2-253f-4d0f-80fc-670dfea236b4', conciliar_authority_doctrinal_definition, conventional).
narrative_ontology:cs_reference_frame('1eadd6f2-253f-4d0f-80fc-670dfea236b4', nicene_metaphysical_standard).
narrative_ontology:cs_drift_state('1eadd6f2-253f-4d0f-80fc-670dfea236b4', late_antiquity_and_medieval_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1eadd6f2-253f-4d0f-80fc-670dfea236b4', '').
narrative_ontology:cs_kernel_id(homoousios_nicene__metaphysical_equality_reading, homoousios_nicene).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_nicene__metaphysical_equality_reading, nicene_episcopal_hierarchy).
narrative_ontology:constraint_beneficiary(homoousios_nicene__metaphysical_equality_reading, orthodox_christological_consensus).
narrative_ontology:constraint_victim(homoousios_nicene__metaphysical_equality_reading, arian_subordinationist_theologians).
narrative_ontology:constraint_victim(homoousios_nicene__metaphysical_equality_reading, honorific_similarity_adherents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(homoousios_nicene__metaphysical_equality_reading, christian_laity_and_local_churches).
narrative_ontology:constraint_beneficiary(homoousios_nicene__metaphysical_equality_reading, imperial_secular_authority).
narrative_ontology:constraint_victim(homoousios_nicene__metaphysical_equality_reading, christian_laity_and_local_churches).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Council of Nicaea and its episcopal defenders establish and enforce homoousios as the binding metaphysical doctrine. They interpret scripture through the metaphysical-equality lens, adjudicate heterodox claims, and anathematize competing readings. Their authority to define orthodoxy rides on the metaphysical reading's stability—if homoousios could mean honorific similarity or allowed subordination, their interpretive monopoly collapses.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, nicene_episcopal_hierarchy, agenda_setter,
    institutional, civilizational, constrained, universal).

% Defend the subordinationist reading: the Son derives being from the Father, shares divinity but not full equality. Under the metaphysical reading, their entire intellectual position is anathematized, their writings condemned, their schools closed. They face exile, deprivation of episcopal office, and erasure from the historical record. Their exit is only apostasy or flight beyond imperial reach.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, arian_subordinationist_theologians, payer,
    moderate, biographical, trapped, universal).

% Hold the reading that homoousios signals honorific likeness or similarity, not strict metaphysical identity. They are suppressed as heretics under the metaphysical reading, though their position occupies the middle ground between Nicene and Arian. Anathematization and institutional exclusion are their cost of holding this reading. Their intellectual position is rendered unspeakable by the metaphysical reading's dominance.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, honorific_similarity_adherents, payer,
    moderate, biographical, trapped, universal).

% Receive a unified, stable creedal standard that clarifies Christian identity and bars Christological confusion. They are bound by the metaphysical reading through baptismal confession and communal liturgy. They also bear the cost of the constraint's enforcement: schism, loyalty tests, suppression of local theological variation. Exit means abandoning Christian identity itself, which is identity-locked for most believers.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, christian_laity_and_local_churches, beneficiary,
    powerless, generational, identity_locked, universal).
narrative_ontology:stakeholder_secondary_role(homoousios_nicene__metaphysical_equality_reading, christian_laity_and_local_churches, payer).

% Constantine convenes Nicaea to resolve the Arian controversy and secure religious unity as a tool of imperial cohesion. The emperor benefits from unified doctrine and stable orthodoxy under imperial authority. The metaphysical reading, enforced through imperial law and council authority, becomes inseparable from imperial power. The emperor enforces the doctrine and in return commands the church's loyalty and doctrinal output.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, imperial_secular_authority, agenda_setter,
    institutional, generational, constrained, universal).
narrative_ontology:stakeholder_secondary_role(homoousios_nicene__metaphysical_equality_reading, imperial_secular_authority, beneficiary).

% Would argue that homoousios is nowhere explicit in scripture and that metaphysical precision beyond the biblical text is presumptuous. They are structurally excluded from the Nicene framework, which treats the term as the binding interpretation of what scripture implicitly requires. Their objection—that the constraint reads metaphysical doctrine INTO scripture rather than FROM it—is never heard in the conciliar setting.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, scriptural_realist_interpreters, excluded,
    moderate, biographical, trapped, universal).

% Subsequent councils (Constantinople, Chalcedon, and later ecumenical bodies) inherit and test the Nicene reading against theological outcomes: does it prevent subordinationism, does it cohere with Christological formulas, does it resolve the Filioque question, does it generate stable doctrine? They measure whether the constraint is a genuine coordination mechanism or a historically contingent extraction of interpretive power.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, later_revisionist_councils_and_theologians, observer,
    institutional, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(homoousios_nicene__metaphysical_equality_reading, nicene_episcopal_hierarchy).
narrative_ontology:fixing_cost_class(homoousios_nicene__metaphysical_equality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, binding metaphysical formulation of Trinitarian equality that prevents the Christological confusion of early Christianity: homoousios certifies that Father and Son share the same divine essence, are co-eternal, and admit no subordination in being. This solves the coordination problem of divergent Christological readings that fragmented local churches and threatened the religious foundation of imperial unity.
% TRANSFER_FUNCTION: Transfers interpretive authority from local bishops and scriptural communities to the ecumenical council and imperial-backed orthodox hierarchy. Transfers the right to define legitimate Christology from open theological disputation to binding conciliar pronouncement. Transfers the cost of enforcing a single metaphysical reading (anathematization, exile of heterodox bishops, book-burning, schism) onto subordinationist and honorific-similarity theologians.
% ABSENT_VOICES: Scriptural literalists who would argue homoousios is not explicit in scripture and represents an over-reach of metaphysical precision; lay theologians and monastic communities with local Christological traditions whose voices are not represented at Nicaea; future theological schools that would develop alternative metaphysical frameworks (Cappadocians, Nestorians, Monophysites). The council's composition—imperial convocation, episcopal hierarchy only—structurally excludes voices outside institutional church leadership.
% DISAPPEARANCE_RATIONALE: If the metaphysical-equality reading and its enforcement vanished overnight, Christian theology would immediately fragment into Arian, subordinationist, and local Christological variants. Imperial religious unity would collapse; the episcopal hierarchy's claim to doctrinal authority would dissolve; the anathematized theologians would be rehabilitated; and Christian communities would organize around competing metaphysical readings for centuries. The constraint's persistence reshapes the theological and ecclesiastical landscape fundamentally.
% FOUNDING_PROBLEM: The early church faces severe Christological confusion: Is the Son eternal or created? Co-equal with the Father or subordinate? Is he divine in essence or only in honor and power? These questions fragment local churches, undermine baptismal unity, and threaten the religious legitimacy Constantine needs for imperial cohesion. Arius's subordinationism—that the Son is the highest creation but not eternal—offers a metaphysically coherent but theologically destabilizing reading. Nicaea aims to settle the question decisively through metaphysical precision: homoousios establishes identity, not hierarchy.
% FOUNDING_PROBLEM_CORROBORATION: Constantine's imperial convocation and the post-Nicene historical record attest the founding problem was live and urgent: Arian controversy threatened religious unity. But later theologians and modern historians attest the problem was CREATED as much as solved: the metaphysical precision Nicaea demands (homoousios, one substance, co-eternal) is not explicitly biblical and arguably imports Platonic metaphysics into Christian doctrine. Subordinationist and honorific-similarity theologians attest the founding problem was a false framing designed to eliminate legitimate alternative readings. Modern scholarship attests that Arian Christology was metaphysically defensible and scripturally grounded, and that the contest was partly a power struggle over interpretive authority. No corroborating voice outside the Nicene beneficiary class endorses the founding-problem diagnosis as formulated by Nicaea; all authoritative corroboration comes from within the orthodoxy the constraint establishes.
narrative_ontology:disappearance_verdict(homoousios_nicene__metaphysical_equality_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_nicene__metaphysical_equality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_nicene__metaphysical_equality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(homoousios_nicene__metaphysical_equality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_nicene__metaphysical_equality_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_nicene__metaphysical_equality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(homoousios_nicene__metaphysical_equality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(homoousios_nicene__metaphysical_equality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness reaches 0.68 by the end of the interval because the metaphysical reading consolidates interpretive monopoly: the council's pronouncement becomes indisputable doctrine, and any theological deviation is heresy by definition. Suppression is high (0.79) because the constraint's persistence requires active enforcement—anathematization, exile, imperial law, destruction of heterodox texts. Theater rises from 0.25 to 0.42 over the interval as the founding coordination problem (preventing Christological confusion) atrophies and the constraint increasingly functions as a theatrical re-affirmation of orthodoxy—councils repeat the Nicene formula, bishops confess it, but its living function as problem-solving fades. The measurement grid shows extraction and suppression accumulating as the reading hardens into unquestioned tradition; theater rising indicates the constraint increasingly performs orthodoxy rather than solving the original coordination problem. Accessibility collapse is high (0.81) because once homoousios enters the creedal standard, alternatives become literally unspeakable—they are anathemata, damned propositions. The constraint forecloses exit from the orthodoxy set: you confess homoousios in baptism or you are not Christian.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence emerges from the constraint's dual function. From the Nicene agenda-setter seat: the metaphysical reading is coordination (solves fragmentation) plus legitimate authority (only the council can settle doctrine). From the victim seats: the reading is a cover story for extracting interpretive monopoly using the genuine coordination problem as justification. From the laity seat: it is genuine coordination (I know what I believe) fused with identity-lock (I cannot think otherwise without apostasy). From the imperial seat: it is purely instrumental (religious unity = imperial power). The constraint's type computation depends on which directionality dominates: if beneficiary power is high and victim exit is trapped, the computed type at the victim seat is snare (extraction cover story); at the beneficiary seat, it may compute as rope (genuine coordination). The authored claim (tangled rope) reflects the structural truth: BOTH coordination AND extraction coexist in one constraint, held together by the asymmetric power of the hierarchy to define the terms.
 *
 * DIRECTIONALITY LOGIC:
 *   The Nicene episcopal hierarchy sits at d near 0.0 (full beneficiary): they collect interpretive authority, institutional power, imperial backing, and the power to define legitimate theology. Arian and honorific-similarity theologians sit at d near 1.0 (full targets): they are anathematized, their positions declared damned, their books destroyed, their communities exiled. Christian laity sit near d = 0.5-0.6 (symmetric to slightly payer): they receive genuine creedal clarity and unified worship but bear the cost of identity-lock and enforced conformity; they cannot exit without apostasy. Imperial authority sits near d = 0.1-0.2 (beneficiary): they gain religious unity without bearing the suppressive burden. Scriptural realists sit at d near 1.0 if they were visible, but they are structurally excluded (exit is to silence). The directionality chain: beneficiaries (hierarchy, empire) have high arbitrage and organized power (d low); victims (heterodox theologians) have identity-locked, trapped exit and moderate power (d high); the laity are identity-locked to Christianity itself (d elevated despite nominal beneficiary role). No overrides needed—the derived directionality from beneficiary/victim + exit matches the structural truth.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem status is 'contested' because later scholarship and excluded voices attest the founding problem was partly constructed. Nicaea framed Arianism as incoherent heresy; modern historians and theologians attest Arian Christology was metaphysically defensible and scripturally grounded, and that the contest was partly a power struggle over interpretive authority. If the founding problem is actually solved (Christological fragmentation is ended by Nicene formulation), the constraint is tangled rope—genuine coordination with asymmetric extraction. If the founding problem is dead or manufactured (Arianism was not actually incoherent; the constraint perpetuates a false necessity), the constraint trends toward snare (the extraction persists via cover story). The mandatrophy signal: founding_problem_status = 'contested' + disappearance_verdict = 'world_rearranges' indicates a constraint whose founding rationale is disputed but whose institutional persistence is undeniable. The constraint does NOT resolve mandatrophy—it sits in the borderland between coordination and pure extraction, depending on whether you credit the founding problem as real or constructed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metaphysical_precision_scriptural_warrant,
    'Does homoousios have explicit scriptural warrant, or does the Nicene reading import Platonic metaphysics into Christian doctrine?',
    'Scriptural-linguistic analysis of homoousios, ousia, and cognates in early Christian texts vs. later conciliar usage; historical tracing of Platonic influence on Nicene metaphysics; testimony from scriptural literalist interpreters (excluded from Nicaea) about whether the term''s precision is biblically necessary or philosophically imposed.',
    'If homoousios is scripturally emergent, the reading is coordination (solving a real doctrinal problem). If it is philosophically imposed, the constraint is extraction (imposing metaphysical precision beyond scriptural requirement). The testimonial gap—only Nicene hierarchy attests scriptural necessity—is itself a signal: excluded voices would dispute the founding-problem diagnosis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metaphysical_precision_scriptural_warrant, conceptual, 'Whether homoousios arises from scripture or from external metaphysical frameworks.').

omega_variable(
    arianism_coherence_and_legitimacy,
    'Was Arian Christology metaphysically incoherent and scripturally illegitimate (Nicene verdict) or defensible and coherent (subordinationist and later revisionist assessment)?',
    'Detailed reconstruction of Arian theological arguments from primary texts (Arius, Eusebius of Nicomedia); modern scholarly consensus on the metaphysical and scriptural defensibility of subordinationism; comparison of Arian and Nicene theological outputs in generating stable Christology.',
    'If Arianism was incoherent, Nicaea solved a genuine problem (tangled rope—coordination + asymmetric enforcement). If Arianism was coherent, Nicaea did not solve it but suppressed it via power (snare—pure extraction cover story). The foundational legitimacy of the constraint hinges on whether the victim reading (subordinationism) was actually erroneous or merely non-dominant.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(arianism_coherence_and_legitimacy, empirical, 'Whether Arianism was logically indefensible or merely theologically displaced.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of Arian and honorific-similarity readings structural (legal ban, exile, book-burning) or internalized (theologians have come to believe homoousios is necessary)?',
    'Post-suppression trajectory: did heterodox theologians in exile or in schismatic communities maintain their readings, or did suppression environments force internalization? Did later theologians adopt homoousios because it is logically necessary or because it is institutionally mandatory?',
    'If suppression is purely structural, the constraint could be reversed by removing the enforcement machinery. If internalized, the constraint persists even without active enforcement because the reading has become self-evidently true. High internalization indicates the constraint has become a mountain (natural law of Christian thought) rather than tangled rope (enforced doctrine).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of alternatives is structural or internalized via centuries of enforcement.').

omega_variable(
    conciliar_authority_origin,
    'Does the ecumenical council''s authority to define doctrine arise from its function (solving Christological fragmentation) or from its power (imperial backing + episcopal hierarchy)?',
    'Historical analysis of council convocation: was Nicaea called to solve a pressing problem (problem-solving framing) or to consolidate Constantine''s religious control (power-consolidation framing)? Did later councils inherit binding authority from Nicaea''s success or from its institutional precedent?',
    'If conciliar authority arises from problem-solving function, the constraint is genuine coordination. If it arises from power consolidation, the authority is itself an extraction mechanism—the constraint extracts interpretive monopoly for the hierarchy while using Christological fragmentation as the justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conciliar_authority_origin, conceptual, 'Whether conciliar authority serves coordination or extracts power.').

omega_variable(
    kernel_contest_logical_foreclosure,
    'Does the metaphysical-equality reading logically foreclose the subordinationist reading (they cannot coexist in any coherent framework) or do they coexist as different parties'' live commitments?',
    'Formal logical analysis: can homoousios (same divine essence, co-eternal) be coherently held alongside the subordinationist claim (Son derives being from Father)? Do later theologians who attempt reconciliation (e.g., Cappadocian fathers) succeed in finding a coherent middle ground, or do they collapse the distinction?',
    'If foreclosure is true, this reading forecloses its sibling readings within any single framework (rare, high-confidence foreclosure). If coexistence is possible, the readings remain live options held by different parties, and the constraint''s suppression is power-driven rather than logically necessary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_logical_foreclosure, conceptual, 'Whether the metaphysical-equality and subordinationist readings are logically incompatible or coexistent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_nicene__metaphysical_equality_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_tr_t0, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(homo_tr_t0, observed).
narrative_ontology:measurement(homo_tr_t10, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement_basis(homo_tr_t10, observed).
narrative_ontology:measurement(homo_tr_t25, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 25, 0.33).
narrative_ontology:measurement_basis(homo_tr_t25, observed).
narrative_ontology:measurement(homo_tr_t50, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 50, 0.39).
narrative_ontology:measurement_basis(homo_tr_t50, observed).
narrative_ontology:measurement(homo_tr_t75, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 75, 0.41).
narrative_ontology:measurement_basis(homo_tr_t75, observed).
narrative_ontology:measurement(homo_tr_t100, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 100, 0.42).
narrative_ontology:measurement_basis(homo_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(homo_be_t0, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(homo_be_t0, observed).
narrative_ontology:measurement(homo_be_t10, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(homo_be_t10, observed).
narrative_ontology:measurement(homo_be_t25, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 25, 0.6).
narrative_ontology:measurement_basis(homo_be_t25, observed).
narrative_ontology:measurement(homo_be_t50, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 50, 0.66).
narrative_ontology:measurement_basis(homo_be_t50, observed).
narrative_ontology:measurement(homo_be_t75, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 75, 0.67).
narrative_ontology:measurement_basis(homo_be_t75, observed).
narrative_ontology:measurement(homo_be_t100, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 100, 0.68).
narrative_ontology:measurement_basis(homo_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(homo_su_t0, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(homo_su_t0, observed).
narrative_ontology:measurement(homo_su_t10, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement_basis(homo_su_t10, observed).
narrative_ontology:measurement(homo_su_t25, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 25, 0.73).
narrative_ontology:measurement_basis(homo_su_t25, observed).
narrative_ontology:measurement(homo_su_t50, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 50, 0.77).
narrative_ontology:measurement_basis(homo_su_t50, observed).
narrative_ontology:measurement(homo_su_t75, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 75, 0.78).
narrative_ontology:measurement_basis(homo_su_t75, observed).
narrative_ontology:measurement(homo_su_t100, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 100, 0.79).
narrative_ontology:measurement_basis(homo_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_nicene__metaphysical_equality_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(homoousios_nicene__metaphysical_equality_reading, 0.12).
narrative_ontology:affects_constraint(homoousios_nicene__metaphysical_equality_reading, homoousios_nicene__subordinationist_reading).
narrative_ontology:affects_constraint(homoousios_nicene__metaphysical_equality_reading, homoousios_nicene__honorific_similarity_reading).
narrative_ontology:affects_constraint(homoousios_nicene__metaphysical_equality_reading, filioque_doctrinal_controversy).
narrative_ontology:affects_constraint(homoousios_nicene__metaphysical_equality_reading, christological_formula_chalcedon).

% DUAL FORMULATION NOTE:
% The homoousios kernel instantiates three structurally distinct constraints corresponding to the three readings: metaphysical_equality_reading (this file), subordinationist_reading (ontological hierarchy compatible with homoousios), and honorific_similarity_reading (likeness without identity). Each reading produces different beneficiary/victim structures, different suppression profiles, and different classifications. The three are linked via network.affects_constraints because the contest among readings is one historical event; a downstream constraint like the Filioque (whether the Holy Spirit proceeds from the Father alone or from Father and Son) inherits the metaphysical-equality reading as its upstream assumption.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
