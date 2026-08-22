% ============================================================================
% CONSTRAINT STORY: nicene_creed_authority__strict_orthodox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_creed_authority__strict_orthodox_reading, []).

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
 *   constraint_id: nicene_creed_authority__strict_orthodox_reading
 *   human_readable: Nicene Creed Metaphysical Orthodoxy Binding (Strict Orthodox Reading)
 *   domain: theological/ecclesiastical
 *
 * SUMMARY:
 *   The Nicene Creed, formalized at the Council of Nicaea (325 CE), asserts
 *   that Christ is 'of one substance with the Father' (homoousios) and binds
 *   all believers to this metaphysical claim. Under the strict-orthodox
 *   reading (instantiated here), the creed is not merely a testimony to lived
 *   faith or a liturgical marker of identity; it is a binding metaphysical
 *   requirement enforced by ecclesiastical authority. Deviation is heresy,
 *   punishable by excommunication, institutional exclusion, and (during
 *   periods of imperial-ecclesiastical alliance) civil sanctions. The reading
 *   treats doctrinal uniformity as the primary coordination function and
 *   positions the hierarchical magisterium as the authoritative interpreter.
 *   This reading coexists with two alternative readings: the
 *   liturgical-habituation reading (creed as identity boundary through
 *   performance, independent of cognitive metaphysical assent) and the
 *   symbolic-confessional reading (creed as historically contingent witness
 *   grounded in community discernment, not binding metaphysical claim). This
 *   story generates ONLY the strict-orthodox reading as a single ε-invariant
 *   constraint; the other readings are separate constraint stories, linked
 *   via network effects.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_creed_authority__strict_orthodox_reading, 0.68).
domain_priors:suppression_score(nicene_creed_authority__strict_orthodox_reading, 0.76).
domain_priors:theater_ratio(nicene_creed_authority__strict_orthodox_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_creed_authority__strict_orthodox_reading, tangled_rope).
narrative_ontology:human_readable(nicene_creed_authority__strict_orthodox_reading, "Nicene Creed Metaphysical Orthodoxy Binding (Strict Orthodox Reading)").
narrative_ontology:topic_domain(nicene_creed_authority__strict_orthodox_reading, "theological/ecclesiastical").

domain_priors:requires_active_enforcement(nicene_creed_authority__strict_orthodox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_creed_authority__strict_orthodox_reading, '7070cc57-470c-442d-918b-9e6f2555d18c').
narrative_ontology:cs_kernel_codification('7070cc57-470c-442d-918b-9e6f2555d18c', fixed_text).
narrative_ontology:cs_authority_grounding('7070cc57-470c-442d-918b-9e6f2555d18c', extraction).
narrative_ontology:cs_interpretation_layer_present('7070cc57-470c-442d-918b-9e6f2555d18c').
narrative_ontology:cs_reading_relation('7070cc57-470c-442d-918b-9e6f2555d18c', nicene_creed_authority__symbolic_confessional_reading, forecloses).
narrative_ontology:cs_reading_relation('7070cc57-470c-442d-918b-9e6f2555d18c', nicene_creed_authority__liturgical_habituation_reading, coexists_with).
narrative_ontology:cs_axiom('7070cc57-470c-442d-918b-9e6f2555d18c', foundational, metaphysical_uniformity_required_for_apostolic_authority).
narrative_ontology:cs_axiom_status(metaphysical_uniformity_required_for_apostolic_authority, holdable).
narrative_ontology:cs_axiom_grounding('7070cc57-470c-442d-918b-9e6f2555d18c', metaphysical_uniformity_required_for_apostolic_authority, deontological).
narrative_ontology:cs_axiom('7070cc57-470c-442d-918b-9e6f2555d18c', foundational, magisterial_interpretation_binding_on_conscience).
narrative_ontology:cs_axiom_status(magisterial_interpretation_binding_on_conscience, holdable).
narrative_ontology:cs_axiom_grounding('7070cc57-470c-442d-918b-9e6f2555d18c', magisterial_interpretation_binding_on_conscience, conventional).
narrative_ontology:cs_axiom('7070cc57-470c-442d-918b-9e6f2555d18c', secondary, heterodoxy_sanctions_justified_by_doctrinal_necessity).
narrative_ontology:cs_axiom_status(heterodoxy_sanctions_justified_by_doctrinal_necessity, holdable).
narrative_ontology:cs_axiom_grounding('7070cc57-470c-442d-918b-9e6f2555d18c', heterodoxy_sanctions_justified_by_doctrinal_necessity, deontological).
narrative_ontology:cs_reference_frame('7070cc57-470c-442d-918b-9e6f2555d18c', magisterial_metaphysical_uniformity).
narrative_ontology:cs_drift_state('7070cc57-470c-442d-918b-9e6f2555d18c', reformation_post_council_trent, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7070cc57-470c-442d-918b-9e6f2555d18c', '').
narrative_ontology:cs_kernel_id(nicene_creed_authority__strict_orthodox_reading, nicene_creed_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_creed_authority__strict_orthodox_reading, hierarchical_clergy).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__strict_orthodox_reading, orthodox_magisterium).
narrative_ontology:constraint_victim(nicene_creed_authority__strict_orthodox_reading, heterodox_communities).
narrative_ontology:constraint_victim(nicene_creed_authority__strict_orthodox_reading, lay_interpreters).
narrative_ontology:constraint_victim(nicene_creed_authority__strict_orthodox_reading, mystical_visionaries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__strict_orthodox_reading, believing_laity).
narrative_ontology:constraint_victim(nicene_creed_authority__strict_orthodox_reading, believing_laity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Priests, bishops, and magisterial authorities who interpret, teach, and enforce the creed's metaphysical claims. They determine what counts as heresy, who may teach, and what sanctions apply. Benefit from the creed's authority by monopolizing legitimate theological speech and binding believers to their interpretations. Their institutional power depends on creed enforcement.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, hierarchical_clergy, agenda_setter,
    institutional, generational, arbitrage, universal).
narrative_ontology:stakeholder_secondary_role(nicene_creed_authority__strict_orthodox_reading, hierarchical_clergy, beneficiary).

% The collective doctrinal authority (councils, papal authority, conciliar bodies) that claims ownership of the creed's 'true' meaning. Defines orthodoxy, ratifies doctrine, and directs enforcement against deviation. Maintains institutional legitimacy by preserving doctrinal clarity and uniformity against competing interpretations.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, orthodox_magisterium, agenda_setter,
    institutional, civilizational, analytical, universal).

% Communities offering alternative metaphysical readings (Arians, Nestorians, Monophysites, Mystics, Reformers) who bear sanctions: excommunication, property confiscation, exile, execution. They remain within or proximate to the faith but refuse assent to the strict-orthodox metaphysical claims. Their identity as believers is caught in the constraint: exit means ceasing to be Christian (as defined by orthodoxy), which many cannot do despite the cost.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, heterodox_communities, payer,
    moderate, generational, identity_locked, universal).

% Lay believers and theologians whose interpretive authority is suppressed by the creed-binding rule. They may read scripture and reason theologically but must arrive at the magisterium's conclusions or face censure. Their constraint is epistemic: they are forbidden from authentic theological inquiry that might contradict the creed.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, lay_interpreters, payer,
    powerless, biographical, identity_locked, universal).

% Prophetic figures, mystics, and visionaries whose direct religious experience is subordinated to creedal doctrine. If their visions or teachings diverge from strict-orthodox metaphysics, they face institutional pressure to retract or face sanctions. The constraint restricts which religious experiences count as valid revelation.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, mystical_visionaries, payer,
    moderate, biographical, constrained, universal).

% Ordinary believers who receive the creed as authoritative teaching and find comfort in metaphysical clarity and unified doctrine. They benefit from the simplicity of unified belief and belonging to a coherent community. They also pay through epistemic subordination: their theological reasoning is constrained by the requirement to arrive at creedal conclusions; questions that challenge the creed are discouraged.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, believing_laity, beneficiary,
    organized, biographical, identity_locked, universal).
narrative_ontology:stakeholder_secondary_role(nicene_creed_authority__strict_orthodox_reading, believing_laity, payer).

% Other Christian communions, theological schools, and interpretive traditions that develop alternative metaphysical frameworks (Eastern Orthodox theology, Reformed theology, Catholic developments post-Reformation). They are excluded from legitimate participation in defining the creed; their doctrinal innovations are treated as deviation rather than development. The constraint actively suppresses their influence on what counts as 'true' Christianity.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, competing_theological_traditions, excluded,
    powerful, generational, trapped, universal).

% Historical deliberative bodies that codified the creed and ratified its binding force. They sit analytically after their initial act; later councils inherit their authority and add subsequent creeds. Their original role was substantive adjudication; their successor role is maintenance of the inherited tradition.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, councils_of_nicaea_and_successors, observer,
    institutional, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nicene_creed_authority__strict_orthodox_reading, hierarchical_clergy).
narrative_ontology:fixing_cost_class(nicene_creed_authority__strict_orthodox_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine problem of early Christian community fragmentation: without doctrinal unity, Christian identity dissolves into competing sects, each claiming apostolic authority. The creed provides a focal point for membership and shared metaphysical reference, enabling a unified church and reliable transmission of Christian teaching.
% TRANSFER_FUNCTION: Moves interpretive authority from distributed believers and local theological reasoning to the hierarchical magisterium. Believers transfer the right to develop theology independently in exchange for authoritative doctrine. Under the strict-orthodox reading, it also extracts loyalty to one metaphysical schema and transfers legitimacy to those who police it.
% ABSENT_VOICES: Heterodox communities and lay theological voices are structurally excluded. They would argue that the creed over-determines metaphysical claims admitting reasonable disagreement, that lived faith does not require cognitive assent to abstract ontology, and that enforced uniformity suppresses genuine Christian diversity and prophetic renewal. Their exclusion is maintained by the same enforcement machinery the creed requires.
% DISAPPEARANCE_RATIONALE: If the creed's binding force dissolved overnight, Christian communities would reorganize around local interpretation, prophetic experience, and practice-based identity rather than magisterial metaphysical assent. Multiple Christian traditions would flourish openly (Arian, Monophysite, Mystical, Reformed). The unified institutional church would fragment into regional and theological federations. Clergy would lose monopoly on theological interpretation. This rearrangement is precisely what the creed's enforcement structure exists to prevent.
% FOUNDING_PROBLEM: In the 4th century, diverse Christian communities offered incompatible answers to the nature of Christ and the Trinity. Without doctrinal settlement, the church fractured, imperial legitimacy was uncertain, and Christian identity became unclear. The creed aimed to settle the metaphysical question and restore unified institutional authority.
% FOUNDING_PROBLEM_CORROBORATION: The magisterium attests the founding problem remains live: doctrinal disorder leads to community fragmentation and loss of apostolic authority; creed enforcement prevents this. Reformation historians, modern ecumenical scholars, and comparative-theology researchers from outside the benefiting parties attest the founding problem is substantially solved and the creed-binding persists primarily as a mechanism for institutional authority preservation and suppression of legitimate theological diversity.
narrative_ontology:disappearance_verdict(nicene_creed_authority__strict_orthodox_reading, world_rearranges).
narrative_ontology:founding_problem_status(nicene_creed_authority__strict_orthodox_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_creed_authority__strict_orthodox_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nicene_creed_authority__strict_orthodox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nicene_creed_authority__strict_orthodox_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_creed_authority__strict_orthodox_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nicene_creed_authority__strict_orthodox_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nicene_creed_authority__strict_orthodox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.38 (325 CE, immediate post-council) to 0.68 (post-Reformation stabilization) as the creed's enforcement machinery matures and the institutional church consolidates state power. The early period shows low extraction because the creed was still contested and enforcement was inconsistent (Arian bishops held power in some regions; councils issued counter-decrees). By 450 CE, after Theodosius's edicts and the Council of Ephesus, enforcement intensifies and extraction rises to 0.52. Medieval institutionalization (800-1200) further hardens enforcement; the theater_ratio remains moderate (0.32 at 800, 0.38 at 1200) because ecclesiastical authority still genuinely teaches doctrine, not merely performing authority. By the Reformation (1545), theater rises to 0.42 as Reformation critiques reveal the gap between creed-maintenance and living faith; suppression intensifies (0.76) because the heterodox challenge grows more organized. Post-Reformation (1975), extractiveness and suppression plateau at 0.68/0.76: the creed remains enforced within Catholic and Orthodox traditions, but competing denominations have fragmented the universal church, so the creed binds fewer believers with less total suppression. Theater remains at 0.42: contemporary ecclesiastical teaching still invokes the creed's theological substance, but the discovery that believers can flourish (and retain Christian identity) outside strict-orthodox metaphysics has revealed the gap between stated coordination function and actual extraction. All metrics share one time grid, authored for every metric at every point.
 *
 * PERSPECTIVAL GAP:
 *   From the magisterium and hierarchical clergy, this is a rope-like arrangement they created to solve fragmentation and preserve apostolic authority. From heterodox communities, it is a snare: the stated coordination function (doctrinal clarity) is real but subordinate to the extraction function (monopoly on interpretation, suppression of alternatives). From lay interpreters, it is a tangled constraint: they are coordinated into a unified church AND their epistemic freedom is extracted. The engine computes these divergent classifications from the same structural data (beneficiary/victim declarations + power/exit atoms). This divergence is the measurement; the authored claim does not reconcile it.
 *
 * DIRECTIONALITY LOGIC:
 *   The hierarchical clergy (institutional power, arbitrage exit) benefits from creed authority without bearing the creed's costs. They interpret it, enforce it, and collect legitimacy from doing so. Their exit options are sophisticated: they can reinterpret the creed, shift emphasis toward practice, or accept reformed readings and retain institutional role. Their directionality is near the beneficiary end (d ≈ 0.10). Heterodox communities (moderate power, identity-locked exit) bear the full cost of deviation: their metaphysical claims are condemned, they are excluded from institutional church structures, and leaving means psychological rupture from religious identity. Their directionality is near the full-target end (d ≈ 0.90). The asymmetry is structural: the same metaphysical claim binds one group with benefit and the other with cost, depending on alignment with magisterial interpretation. Lay interpreters occupy intermediate directionality (d ≈ 0.70): they must conform their reasoning to creedal conclusions, suppressing authentic theological inquiry, but they retain membership and social stability if they conform.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits classic mandatrophy profile: the founding problem (4th-century doctrinal chaos) was real and urgent, but by the medieval period the problem is substantially solved (Christian identity is stable, doctrine is transmitted reliably, church unity is achieved). Yet the creed-binding persists with equal or greater institutional force. The measurement series shows extractiveness RISING (0.38 → 0.68) as the founding problem RECEDES—exactly the mandatrophy signature. The creed functions remain real (coordination, identity, teaching) but increasingly serve the secondary function of protecting institutional authority against challenge rather than solving the original fragmentation problem. By the Reformation, the creed's binding force is defended not because fragmentation threatens but because the hierarchy's monopoly is threatened by alternative interpretations. The theater_ratio rise from 0.12 to 0.42 corroborates mandatrophy: more of the creed-maintenance activity is performative authority-preservation than functional doctrinal clarification. The six_questions.founding_problem_status = contested directly captures this: the magisterium claims the problem is live (doctrinal chaos is always a threat); historians and reformers claim it is substantially dead (Christian identity thrives in diverse doctrinal contexts). Mandatrophy is resolved by declaring base_properties.mandatrophy_resolved = true if the story author accepts the historical judgment that the founding problem has substantially expired; it remains false (or unspecified) if the magisterial reading is endorsed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metaphysical_necessity_vs_conventionality,
    'Is the creed''s metaphysical content (homoousios, divine nature claims) a necessary truth about reality, or a historically contingent choice among legitimate alternatives?',
    'Theological history of the ecumenical councils: do they claim discovery of metaphysical truth or conventional agreement? Comparison with non-Nicene Christian traditions that retained Christian identity and mission without homoousios.',
    'If metaphysically necessary (discovered truth), the creed''s binding force is justified by reality itself, and extraction may be legitimate coordination cost. If contingent (conventional choice), the binding force is institutional choice, and extraction is clearer as pure hierarchy-protecting mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(metaphysical_necessity_vs_conventionality, conceptual, 'Whether the creed articulates discovered metaphysical necessity or institutional convention.').

omega_variable(
    enforcement_suppression_boundary,
    'Is the measured suppression (0.76) a structural property of maintaining any unified doctrine, or a choice by this particular hierarchy to suppress alternatives with unusual intensity?',
    'Comparative analysis: how much suppression do non-hierarchical, non-magisterial Christian traditions deploy to maintain doctrinal identity? How much do secular institutions deploy to maintain core identity commitments?',
    'High structural suppression (say, 0.6+ unavoidable for any doctrine) would suggest tangled-rope coordination inevitably requires suppression. Low structural suppression (0.3 or below suffices for coordination) would suggest the hierarchy chose intensity beyond what the coordination function demands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_suppression_boundary, empirical, 'How much suppression is functionally necessary vs. chosen by the hierarchy.').

omega_variable(
    cognitive_assent_requirement,
    'Does the strict-orthodox reading require believers to BELIEVE the creed''s metaphysics (cognitive assent to substance doctrine), or to affirm it liturgically and institutionally while permitting diverse metaphysical interpretations below the creed''s formulas?',
    'Historical case analysis: did medieval and reform theologians who affirmed the creed while developing diverse metaphysics (transubstantiation debates, Thomist-Franciscan distinctions, Calvin''s reformed metaphysics) face sanctions for the metaphysical differences? If yes, cognitive assent is required; if no, the creed is performative/institutional rather than requiring uniform metaphysics.',
    'If cognitive assent required, extraction operates at the epistemic level (control of thought); if institutional only, extraction is more limited (control of speech and institutional role, not internalized belief). This affects whether the constraint is identity-locking or merely identity-boundary-enforcing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_assent_requirement, empirical, 'Whether the creed enforces cognitive assent or institutional conformity alone.').

omega_variable(
    alternative_readings_coexistence,
    'Can the symbolic-confessional and liturgical-habituation readings coexist with the strict-orthodox reading within a single institutional framework, or does strict-orthodoxy logically foreclose them?',
    'Theological analysis of axiom compatibility: if a believer holds both ''the creed is binding metaphysical truth'' and ''the creed is contingent witness to faith,'' what contradiction results? Is it logical contradiction or merely institutional conflict?',
    'Logical contradiction (foreclosure) would support a two-constraint decomposition with forecloses relation. Institutional conflict (coexistence) would suggest the readings are different seats'' framings of the same institution, held simultaneously by different parties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_readings_coexistence, conceptual, 'Whether alternative readings are logically incompatible with strict-orthodoxy or merely institutionally contested.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_creed_authority__strict_orthodox_reading, 325, 1975).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nice_tr_t325, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 325, 0.12).
narrative_ontology:measurement(nice_tr_t450, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 450, 0.22).
narrative_ontology:measurement(nice_tr_t800, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 800, 0.32).
narrative_ontology:measurement(nice_tr_t1200, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 1200, 0.38).
narrative_ontology:measurement(nice_tr_t1545, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 1545, 0.42).
narrative_ontology:measurement(nice_tr_t1975, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 1975, 0.42).

% Extraction over time
narrative_ontology:measurement(nice_be_t325, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 325, 0.38).
narrative_ontology:measurement(nice_be_t450, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 450, 0.52).
narrative_ontology:measurement(nice_be_t800, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 800, 0.61).
narrative_ontology:measurement(nice_be_t1200, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 1200, 0.65).
narrative_ontology:measurement(nice_be_t1545, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 1545, 0.68).
narrative_ontology:measurement(nice_be_t1975, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 1975, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(nice_su_t325, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 325, 0.35).
narrative_ontology:measurement(nice_su_t450, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 450, 0.58).
narrative_ontology:measurement(nice_su_t800, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 800, 0.68).
narrative_ontology:measurement(nice_su_t1200, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 1200, 0.73).
narrative_ontology:measurement(nice_su_t1545, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 1545, 0.76).
narrative_ontology:measurement(nice_su_t1975, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 1975, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_creed_authority__strict_orthodox_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(nicene_creed_authority__strict_orthodox_reading, 0.12).
narrative_ontology:affects_constraint(nicene_creed_authority__strict_orthodox_reading, nicene_creed_authority__symbolic_confessional_reading).
narrative_ontology:affects_constraint(nicene_creed_authority__strict_orthodox_reading, nicene_creed_authority__liturgical_habituation_reading).

% DUAL FORMULATION NOTE:
% The nicene_creed_authority kernel decomposes into three constraint stories, each instantiating a reading's structural implications. The strict-orthodox reading (this story) treats the creed as binding metaphysics enforced by hierarchy; it shows high extractiveness (ε ≈ 0.68) and clear victim/beneficiary asymmetry. The symbolic-confessional reading treats the creed as contingent witness grounded in community discernment; it shows low extractiveness (ε ≈ 0.20) and distributed beneficiary. The liturgical-habituation reading treats the creed as liturgical identity marker independent of cognitive assent; it shows moderate extractiveness (ε ≈ 0.22) and focus on belonging over doctrine. These are not three observations of the same constraint; they are three different constraints arising from three different interpretations of the kernel's authority and binding force. They are linked via network.affects_constraints because challenges to one reading (e.g., historical evidence that Christian identity is stable without strict metaphysical assent) structurally pressure the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
