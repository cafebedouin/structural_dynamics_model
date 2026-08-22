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
 *   constraint_id: nicene_christological_kernel__homoousios_reading
 *   human_readable: Nicene Homoousios Doctrine (Same Substance with the Father)
 *   domain: theological/ecclesiastical
 *
 * SUMMARY:
 *   The homoousios doctrine declares that Christ is of the same divine
 *   substance (ousia) as the Father—fully divine, not created, co-eternal and
 *   equal. Formally adopted at the Council of Nicaea (325 CE) under imperial
 *   pressure and later reaffirmed at Constantinople (381 CE), it became the
 *   binding orthodoxy of the Roman Empire and the standard by which other
 *   Christological formulations were branded heretical. The constraint's
 *   enforcement relied on imperial authority to exile bishops, confiscate
 *   property, suppress texts, and anathematize entire communities. This is
 *   ONE reading of the contested Nicene Christological kernel; the sibling
 *   reading (homoiousios) maintains that a distinction of substance, though
 *   not of rank, better preserves monotheistic clarity and allows regional
 *   theological autonomy. The homoousios reading extracts heavily: it
 *   concentrates theological authority in the imperial establishment,
 *   suppresses competing formulations, and benefits those who control the
 *   definition of orthodoxy.
 *
 * KEY AGENTS:
 *   - Imperial ecclesiastical authority: Constantine and imperial bishops — set the doctrine, enforce uniformity through anathema and property confiscation.
 *   - Nicene orthodox establishment: Bishops affirming homoousios — accrue institutional power, theological authority, access to imperial patronage.
 *   - Regional Christian communities: Local churches with diverse Christological traditions — bear the cost of conformity or face exile and institutional destruction.
 *   - Theological diversity advocates (Arians, homoiousians): Theologians who maintain ontological distinction — exiled, anathematized, their writings destroyed.
 *   - Gothic Arian populations: Germanic tribes evangelized in subordinationist Christology — face cultural and religious erasure under Nicene imposition.
 *   - Imperial political authority: Constantine and successors — use homoousios as a tool of religious uniformity and political control.
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
narrative_ontology:human_readable(nicene_christological_kernel__homoousios_reading, "Nicene Homoousios Doctrine (Same Substance with the Father)").
narrative_ontology:topic_domain(nicene_christological_kernel__homoousios_reading, "theological/ecclesiastical").

domain_priors:requires_active_enforcement(nicene_christological_kernel__homoousios_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_christological_kernel__homoousios_reading, '761e923c-b96b-422b-a719-00d0ae60e1ce').
narrative_ontology:cs_kernel_codification('761e923c-b96b-422b-a719-00d0ae60e1ce', fixed_text).
narrative_ontology:cs_authority_grounding('761e923c-b96b-422b-a719-00d0ae60e1ce', extraction).
narrative_ontology:cs_interpretation_layer_present('761e923c-b96b-422b-a719-00d0ae60e1ce').
narrative_ontology:cs_reading_relation('761e923c-b96b-422b-a719-00d0ae60e1ce', nicene_christological_kernel__homoiousios_reading, coexists_with).
narrative_ontology:cs_axiom('761e923c-b96b-422b-a719-00d0ae60e1ce', foundational, christ_ontologically_identical_to_father).
narrative_ontology:cs_axiom_status(christ_ontologically_identical_to_father, holdable).
narrative_ontology:cs_axiom_grounding('761e923c-b96b-422b-a719-00d0ae60e1ce', christ_ontologically_identical_to_father, deontological).
narrative_ontology:cs_axiom('761e923c-b96b-422b-a719-00d0ae60e1ce', secondary, doctrinal_uniformity_necessary_for_church_unity).
narrative_ontology:cs_axiom_status(doctrinal_uniformity_necessary_for_church_unity, holdable).
narrative_ontology:cs_axiom_grounding('761e923c-b96b-422b-a719-00d0ae60e1ce', doctrinal_uniformity_necessary_for_church_unity, instrumental).
narrative_ontology:cs_reference_frame('761e923c-b96b-422b-a719-00d0ae60e1ce', unified_nicene_christological_orthodoxy).
narrative_ontology:cs_drift_state('761e923c-b96b-422b-a719-00d0ae60e1ce', fifth_century_stabilization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('761e923c-b96b-422b-a719-00d0ae60e1ce', '').
narrative_ontology:cs_kernel_id(nicene_christological_kernel__homoousios_reading, nicene_christological_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoousios_reading, imperial_ecclesiastical_authority).
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoousios_reading, nicene_orthodox_establishment).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoousios_reading, regional_christian_communities).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoousios_reading, theological_diversity_advocates).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoousios_reading, gothic_arian_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoousios_reading, imperial_political_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Constantine and the imperial bishops convene the Council of Nicaea (325 CE) and establish homoousios as binding doctrine. The Empire enforces uniformity through ecclesiastical hierarchy, anathema, exile, and property confiscation of non-conforming bishops and communities. Benefits from doctrinal unity as a tool of imperial stability and religious legitimacy.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, imperial_ecclesiastical_authority, agenda_setter,
    institutional, generational, arbitrage, continental).

% The bishops and theologians who affirm homoousios gain institutional authority, access to imperial patronage, control of theological seminaries and councils, and the power to define orthodoxy for generations. They accumulate property and influence by serving as the interpretive authority on the constraint.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, nicene_orthodox_establishment, beneficiary,
    institutional, generational, mobile, continental).

% Local churches and regional bishops are required to assent to homoousios or face exile, property confiscation, and ecclesiastical excommunication. Many communities have developed their own formulations (homoiousios, subordinationism) rooted in local tradition and theological reasoning. They pay the cost of conformity or face institutional destruction.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, regional_christian_communities, payer,
    moderate, biographical, constrained, regional).

% Theologians like Arius, Eusebius of Caesarea, and the Homoiousian party maintain that a careful ontological distinction (homoiousios: similar substance vs. homoousios: same substance) better preserves both Christ's divinity AND monotheistic coherence. They are branded heretical, exiled, and their writings are destroyed. Their theological reasoning is suppressed by anathema.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, theological_diversity_advocates, payer,
    powerful, biographical, identity_locked, continental).

% Germanic Gothic tribes, evangelized by Arian missionaries (Ulfilas), have developed a theologically coherent Christian identity rooted in subordinationist Christology. The imposition of Nicene homoousios is experienced as cultural imperialism and religious erasure. They face conversion pressure, legal exclusion, and military conquest if they resist.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, gothic_arian_populations, payer,
    powerless, generational, trapped, regional).

% Monastic and ascetic communities in Egypt and North Africa have developed rich contemplative and devotional practices around Christ that are not primarily ontological/dogmatic. They are theoretically included in the constraint but lack institutional voice; they navigate between homoousios enforcement and their own lived theological practice by pragmatic assent.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, north_african_desert_communities, excluded,
    moderate, biographical, constrained, regional).

% Constantine and his successors use the homoousios doctrine as a tool of imperial religious uniformity and political stability. A single doctrinal standard allows easier control and taxation of the church; religious diversity is read as political risk.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, imperial_political_authority, beneficiary,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(nicene_christological_kernel__homoousios_reading, imperial_political_authority, agenda_setter).

% Later historians and theologians analyze whether the homoousios doctrine represents genuine philosophical/theological truth or institutional power consolidation. The debate about whether the constraint persists from its intrinsic merit or from institutional enforcement remains contested.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, theological_historians_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nicene_christological_kernel__homoousios_reading, imperial_ecclesiastical_authority).
narrative_ontology:fixing_cost_class(nicene_christological_kernel__homoousios_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the theological crisis of how Christ relates to the Father in the Godhead: homoousios provides a single formulation that unifies the Church around a doctrine of full divine equality, enabling institutional coherence and preventing fragmentation of Christian identity.
% TRANSFER_FUNCTION: Moves theological authority from regional bishops and local communities to the imperial-ecclesiastical hierarchy; transfers property and voice from non-conforming communities to the Nicene establishment; transfers spiritual legitimacy from diverse Christological formulations to the single state-enforced doctrine.
% ABSENT_VOICES: Gothic and Germanic Christian communities, North African desert ascetics, and subordinationist theologians are excluded from the council's deliberations. They would argue for preserving regional theological autonomy, for ontological distinction as theologically coherent, and for the spiritual validity of diverse Christological praxis. Their absence enables the imposition; their presence would have fragmented the vote.
% DISAPPEARANCE_RATIONALE: If homoousios enforcement vanished, Christianity would not disappear, but it would reorganize: regional Christological formulations would resurface, Gothic and other Germanic Christian communities would retain their subordinationist theology, and the unified institutional Church would fracture into regional and doctrinal variants. The constraint's disappearance would cost the imperial religious establishment its tool for doctrinal uniformity.
% FOUNDING_PROBLEM: The Council of Nicaea was convened to resolve the Arian controversy: a crisis of theological coherence around whether Christ was eternally divine or created. Arius and his followers taught that Christ was the highest creature but not co-eternal with the Father. This was read as threatening Christian monotheism and producing irreconcilable Christological positions.
% FOUNDING_PROBLEM_CORROBORATION: Imperial and orthodox ecclesiastical sources (Eusebius of Caesarea, later Augustine) attest the crisis was genuine and homoousios resolved it. Subordinationist and homoiousian theologians and communities attest that the crisis was manufactured to justify institutional consolidation, and that their Christologies were equally coherent. No corroboration from outside both positions; the founding problem's reality is itself the point of contention.
narrative_ontology:disappearance_verdict(nicene_christological_kernel__homoousios_reading, contested).
narrative_ontology:founding_problem_status(nicene_christological_kernel__homoousios_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_christological_kernel__homoousios_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nicene_christological_kernel__homoousios_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nicene_christological_kernel__homoousios_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.78) because the constraint consolidates theological authority, excludes competing formulations, and persists through coercive enforcement rather than consent. The measurement series shows rising extractiveness from t=0 (post-Council initial enforcement) through t=120 (stabilized doctrine under later emperors), as the imperial machinery hardens and regional alternatives are progressively eliminated. Suppression is consistently high (0.81 at endpoint) because the constraint depends on active enforcement: exile of non-conforming bishops, destruction of homoiousian texts, military conquest of Gothic Christian populations, legal disability of communities that resist. Theater rises over time (0.15 to 0.42) as the original theological crisis recedes and enforcement becomes increasingly performative—the original coordination problem (resolving the Arian controversy) is claimed as solved, but enforcement continues to defend institutional uniformity rather than to resolve ongoing theological uncertainty. The claim/metric gap is deliberate: the constraint is CLAIMED as tangled_rope (genuine coordination problem + asymmetric extraction) while the metrics show that extraction progressively dominates the coordination function over the 120-year interval.
 *
 * PERSPECTIVAL GAP:
 *   The institutional beneficiary (imperial-ecclesiastical establishment) experiences homoousios as genuine coordination—a solution to theological chaos that unifies the Church and enables institutional governance. From this seat, the enforcement is justified as defense of truth and prevention of heretical fragmentation. The payer seats (regional communities, Gothic populations, subordinationist theologians) experience the same constraint as enforced doctrinal monopoly backed by imperial coercion—a suppression of theological reasoning that happens to serve the interests of those who control the ecclesiastical machinery. The engine should compute divergent types from these structural differences: the beneficiary seat sees coordination; the payer seats see extraction. The authorized claim (tangled_rope) reflects the structural hybrid; the metrics reflect the empirical balance shifting toward extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Imperial ecclesiastical authority sits at d ≈ 0.0-0.2 (beneficiary end): they set the doctrine, enforce it, and collect institutional authority and resources. The Nicene orthodox establishment sits at d ≈ 0.1-0.3 (beneficiary-leaning): they gain from homoousios as the canonical framework and face no suppression. Regional Christian communities sit at d ≈ 0.7-0.9 (target end): they must conform or face exile and property loss; their theological reasoning is suppressed; their regional autonomy is constrained. Gothic Arian populations sit at d ≈ 0.85-1.0 (full target end): they are trapped by the constraint (no exit except military defeat); their entire Christian identity is branded heretical; they face cultural erasure. This directionality structure is NOT fabricated by a scale parameter—it emerges from the beneficiary/victim declarations and exit options: beneficiaries with arbitrage/mobile exits sit at low d; victims with trapped/identity-locked exits and suppressed alternatives sit at high d.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was genuine: the Arian controversy created real theological uncertainty about the relationship between Christ and the Father. Homoousios provided one answer—full equality and identity of substance. But the constraint's persistence beyond the initial crisis rests increasingly on institutional enforcement, not on the intrinsic merit of the doctrine or the continued salience of the coordination problem. By t=120 (early 5th century), the theater_ratio has risen to 0.42, indicating that a substantial share of enforcement activity is performative (defending institutional uniformity) rather than functional (resolving ongoing theological uncertainty). The founding problem's status is CONTESTED: the imperial establishment claims it remains live (heresies keep arising, unity must be defended); the payer seats claim it is dead or manufactured (the original crisis was resolved; current enforcement defends power, not theology). The constraint's persistence beyond its founding justification, combined with rising theater_ratio and the concentration of gain in institutional beneficiaries, is the signature of mandatrophy—a constraint whose original function has atrophied but whose enforcement machinery persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_vs_political_basis,
    'Is the homoousios doctrine enforced because it is theologically superior to alternatives, or because imperial authority uses it as a tool of religious uniformity and political control?',
    'Historical and textual analysis of the Council proceedings, the theological arguments advanced by each party, and the timing and methods of enforcement (exile, property confiscation, military conquest) relative to genuine theological debate. If enforcement precedes theological argument or suppresses argument through coercion, political motive is indicated; if enforcement follows rational theological resolution, theological warrant is indicated.',
    'If primarily political, the constraint should be reclassified from tangled_rope (genuine coordination + asymmetric extraction) toward snare (pure extraction using theological cover). If primarily theological, the high extractiveness may be justified as the cost of resolving a genuine crisis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_vs_political_basis, empirical, 'Whether homoousios derives from theological reasoning or serves political consolidation.').

omega_variable(
    homoousios_vs_homoiousios_logical_foreclosure,
    'Do homoousios and homoiousios represent logically incompatible premises, or are they both coherent readings of the Christological problem?',
    'Formal logical analysis of each reading''s core premises and their implications for monotheism, Trinitarian theology, and Christological coherence. If each can be stated without internal contradiction and can incorporate counterarguments from the other, they are coexistent (not foreclosing). If one strictly implies the negation of the other''s core premise, foreclosure holds.',
    'If coexistent, the constraint should record ''coexists_with'' in reading_relations; the enforcement of homoousios over homoiousios is then institutional suppression, not logical necessity. If foreclosing, ''forecloses'' applies; the enforcement is then the assertion of a necessary truth.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(homoousios_vs_homoiousios_logical_foreclosure, conceptual, 'Whether homoousios logically forecloses homoiousios or both readings remain internally coherent.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of homoiousian and subordinationist readings maintained by structural barriers (exile, property confiscation, text destruction, military conquest) or by internalized acceptance (the communities genuinely come to believe homoousios is correct)?',
    'Textual evidence of resistance, clandestine practice, and revival attempts after suppression is relaxed (e.g., Gothic Arian communities'' persistence despite centuries of pressure; monastic and underground theological writing). If suppression persists only under active coercion and immediately resurfaces when coercion is removed, it is structural; if communities continue assenting after coercion ends, it is internalized.',
    'If structural, the measured suppression (0.81) reflects only external barriers; communities carry suppressed alternatives into the post-enforcement period. If internalized, the constraint''s effective suppression is higher—targets carry the constraint''s logic with them even after formal enforcement ends.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of alternatives is maintained by external barriers or internalized acceptance.').

omega_variable(
    kernel_reading_coexistence_vs_foreclosure,
    'Can the homoousios and homoiousios readings coexist in a single institutional framework, or does adoption of homoousios logically foreclose homoiousios?',
    'Examination of post-Nicene church history: did communities that affirmed homoousios also tolerate or incorporate homoiousian theology, or was suppression necessary to maintain homoousios as binding? Did any institutional framework hold both readings simultaneously?',
    'Determines whether ''forecloses'' or ''coexists_with'' is the correct reading_relation. If the readings were held simultaneously by different parties or incorporated into a unified framework, coexistence holds and the enforcement is institutional choice, not logical necessity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_coexistence_vs_foreclosure, conceptual, 'Whether homoousios and homoiousios readings are logically coexistent or mutually foreclosing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_christological_kernel__homoousios_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nice_tr_t0, nicene_christological_kernel__homoousios_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(nice_tr_t10, nicene_christological_kernel__homoousios_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(nice_tr_t30, nicene_christological_kernel__homoousios_reading, theater_ratio, 30, 0.32).
narrative_ontology:measurement(nice_tr_t60, nicene_christological_kernel__homoousios_reading, theater_ratio, 60, 0.39).
narrative_ontology:measurement(nice_tr_t90, nicene_christological_kernel__homoousios_reading, theater_ratio, 90, 0.41).
narrative_ontology:measurement(nice_tr_t120, nicene_christological_kernel__homoousios_reading, theater_ratio, 120, 0.42).

% Extraction over time
narrative_ontology:measurement(nice_be_t0, nicene_christological_kernel__homoousios_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(nice_be_t10, nicene_christological_kernel__homoousios_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(nice_be_t30, nicene_christological_kernel__homoousios_reading, base_extractiveness, 30, 0.71).
narrative_ontology:measurement(nice_be_t60, nicene_christological_kernel__homoousios_reading, base_extractiveness, 60, 0.76).
narrative_ontology:measurement(nice_be_t90, nicene_christological_kernel__homoousios_reading, base_extractiveness, 90, 0.77).
narrative_ontology:measurement(nice_be_t120, nicene_christological_kernel__homoousios_reading, base_extractiveness, 120, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(nice_su_t0, nicene_christological_kernel__homoousios_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(nice_su_t10, nicene_christological_kernel__homoousios_reading, suppression_requirement, 10, 0.74).
narrative_ontology:measurement(nice_su_t30, nicene_christological_kernel__homoousios_reading, suppression_requirement, 30, 0.79).
narrative_ontology:measurement(nice_su_t60, nicene_christological_kernel__homoousios_reading, suppression_requirement, 60, 0.81).
narrative_ontology:measurement(nice_su_t90, nicene_christological_kernel__homoousios_reading, suppression_requirement, 90, 0.81).
narrative_ontology:measurement(nice_su_t120, nicene_christological_kernel__homoousios_reading, suppression_requirement, 120, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_christological_kernel__homoousios_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(nicene_christological_kernel__homoousios_reading, 0.12).
narrative_ontology:affects_constraint(nicene_christological_kernel__homoousios_reading, nicene_christological_kernel__homoiousios_reading).

% DUAL FORMULATION NOTE:
% The Nicene Christological kernel decomposes into at least two structurally distinct constraint stories: homoousios_reading (this file) and homoiousios_reading (sibling). The kernel is the persisting theological commitment to defining Christ's relationship to the Father. The readings instantiate incompatible constraint structures: homoousios enforces doctrinal uniformity through imperial coercion and suppresses alternatives; homoiousios preserves theological distinction and regional autonomy. The ε values differ significantly: homoousios measures at 0.78 (high extraction, high suppression); homoiousios would measure lower (coordination without imperial enforcement machinery). These are not the same constraint viewed from two angles—they are two constraints whose persistence and operation depend on mutually exclusive institutional and theological premises.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nicene_christological_kernel__homoousios_reading, institutional, 0.15).
constraint_indexing:directionality_override(nicene_christological_kernel__homoousios_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
