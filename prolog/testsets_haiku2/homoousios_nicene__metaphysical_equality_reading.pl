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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Homoousios: Metaphysical Equality of Father and Son (Nicene Reading)
 *   domain: theology/ecclesiastical_authority
 *
 * SUMMARY:
 *   The Nicene Council of 325 CE defined the Son as homoousios (same essence)
 *   with the Father—fully equal in being, co-eternal, not subordinate in any
 *   way. This constraint represents one reading of a contested kernel: the
 *   metaphysical claim that Father and Son share identical divine essence and
 *   that no subordination or ontological gradation exists between them. The
 *   constraint is authoritatively enforced by the conciliar-episcopal
 *   hierarchy through anathema, excommunication, exile, property
 *   confiscation, and (after Constantine) imperial law. Subordinationist
 *   theologians and non-Nicene Christian communities are branded heretical,
 *   suppressed, and excluded from communion. The constraint's extraction
 *   rises over the first four decades (325–365 CE) as imperial enforcement
 *   intensifies, then plateaus once the doctrine is institutionalized and
 *   internalized. The constraint is claimed as rope (genuine coordination
 *   around unified doctrine) but authored with high extractiveness (0.68),
 *   suppression (0.82), and theater (0.41), suggesting the engine will
 *   compute it as tangled rope or snare from various seats—a claim/metric
 *   divergence that is intentional.
 *
 * KEY AGENTS:
 *   - Nicene episcopacy: institutional agenda-setter; defends homoousios as truth and conciliar authority as its custodian
 *   - Subordinationist theologians: organized resistance; argue for ontological gradation or functional subordination
 *   - Non-Nicene Christian communities: trapped victims; lose legal standing, property, and ecclesiastical voice
 *   - Imperial authority (Constantine and successors): institutional beneficiary/agenda-setter; enforces homoousios for political consolidation
 *   - Orthodox faithful: beneficiaries of unified doctrine; gain communion security and clear boundaries
 *   - Later theological tradition: analytical observers who interpret, critique, and reframe homoousios in new philosophical systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_nicene__metaphysical_equality_reading, 0.68).
domain_priors:suppression_score(homoousios_nicene__metaphysical_equality_reading, 0.82).
domain_priors:theater_ratio(homoousios_nicene__metaphysical_equality_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_nicene__metaphysical_equality_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_nicene__metaphysical_equality_reading, "Homoousios: Metaphysical Equality of Father and Son (Nicene Reading)").
narrative_ontology:topic_domain(homoousios_nicene__metaphysical_equality_reading, "theology/ecclesiastical_authority").

domain_priors:requires_active_enforcement(homoousios_nicene__metaphysical_equality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_nicene__metaphysical_equality_reading, '6c66c79b-f06a-4c84-a424-8d283c2de7bb').
narrative_ontology:cs_kernel_codification('6c66c79b-f06a-4c84-a424-8d283c2de7bb', formalized).
narrative_ontology:cs_authority_grounding('6c66c79b-f06a-4c84-a424-8d283c2de7bb', extraction).
narrative_ontology:cs_interpretation_layer_present('6c66c79b-f06a-4c84-a424-8d283c2de7bb').
narrative_ontology:cs_reading_relation('6c66c79b-f06a-4c84-a424-8d283c2de7bb', homoousios_nicene__subordinationist_reading, forecloses).
narrative_ontology:cs_reading_relation('6c66c79b-f06a-4c84-a424-8d283c2de7bb', homoousios_nicene__honorific_similarity_reading, coexists_with).
narrative_ontology:cs_axiom('6c66c79b-f06a-4c84-a424-8d283c2de7bb', foundational, father_son_metaphysical_equality).
narrative_ontology:cs_axiom_status(father_son_metaphysical_equality, holdable).
narrative_ontology:cs_axiom_grounding('6c66c79b-f06a-4c84-a424-8d283c2de7bb', father_son_metaphysical_equality, deontological).
narrative_ontology:cs_axiom('6c66c79b-f06a-4c84-a424-8d283c2de7bb', foundational, conciliar_authority_binding_christology).
narrative_ontology:cs_axiom_status(conciliar_authority_binding_christology, holdable).
narrative_ontology:cs_axiom_grounding('6c66c79b-f06a-4c84-a424-8d283c2de7bb', conciliar_authority_binding_christology, conventional).
narrative_ontology:cs_reference_frame('6c66c79b-f06a-4c84-a424-8d283c2de7bb', nicene_metaphysical_equality_doctrine).
narrative_ontology:cs_drift_state('6c66c79b-f06a-4c84-a424-8d283c2de7bb', post_constantius_consolidation_380_ce, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6c66c79b-f06a-4c84-a424-8d283c2de7bb', '').
narrative_ontology:cs_kernel_id(homoousios_nicene__metaphysical_equality_reading, homoousios_nicene).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_nicene__metaphysical_equality_reading, nicene_episcopacy).
narrative_ontology:constraint_beneficiary(homoousios_nicene__metaphysical_equality_reading, conciliar_authority).
narrative_ontology:constraint_victim(homoousios_nicene__metaphysical_equality_reading, subordinationist_theologians).
narrative_ontology:constraint_victim(homoousios_nicene__metaphysical_equality_reading, non_nicene_christian_communities).
narrative_ontology:constraint_victim(homoousios_nicene__metaphysical_equality_reading, alternative_christological_readings).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(homoousios_nicene__metaphysical_equality_reading, imperial_authority).
narrative_ontology:constraint_beneficiary(homoousios_nicene__metaphysical_equality_reading, orthodox_faithful).
narrative_ontology:constraint_vindicates(homoousios_nicene__metaphysical_equality_reading, trinitarian_coequality).
narrative_ontology:constraint_vindicates(homoousios_nicene__metaphysical_equality_reading, homoousianism).
narrative_ontology:constraint_vindicates(homoousios_nicene__metaphysical_equality_reading, episcopal_conciliar_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The episcopal hierarchy that formulated, defended, and enforced the Nicene homoousios formula at the Council of Nicaea (325 CE) and through subsequent synods and creeds (Nicene Creed revised at Constantinople 381, Chalcedon 451). They define the ontological content of homoousios (same divine essence, co-eternal, no subordination), enforce it through anathema and communion discipline, and derive ecclesiastical authority from being the keepers of right doctrine. Their power rides on the claim that only episcopal conciliar authority can bind the church to dogmatic truth.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, nicene_episcopacy, agenda_setter,
    institutional, generational, arbitrage, continental).

% Theologians and communities (Arius, Eusebians, later Arian and semi-Arian factions) who held that the Son derives being from the Father, shares divinity but is subordinate—not strictly equal in essence or eternity. They are branded heretical by the homoousios reading, subject to anathema, excommunication, church property confiscation, and exile. Their theological voice is suppressed from official church discourse; their communities face legal and social pressure. Exit means renouncing their reading and submitting to Nicene doctrine, or separating into non-Nicene communities (which face institutional disadvantage).
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, subordinationist_theologians, payer,
    organized, biographical, constrained, continental).

% Established Christian communities that do not accept the homoousios formula—Nestorian churches (East Syria), Monophysite/Oriental Orthodox churches, and various regional Christian bodies. They are denied communion with the imperial church, barred from official religious functions, subject to legal disabilities (restrictions on church building, property ownership, ordination), and face social stigma as heretical. Their exit options are to submit to Nicene doctrine (requiring institutional reorganization and theological recanting) or separate into parallel ecclesiastical structures (which reduces institutional power and resource access).
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, non_nicene_christian_communities, payer,
    moderate, generational, trapped, continental).

% Non-Christian and heterodox philosophical frameworks that might generate alternative readings of divine nature, incarnation, or trinitarian relationship (Neoplatonism, Stoicism, Gnostic systems, later Islamic and Jewish philosophical theology). They are structurally excluded from the ecclesiastical conversation and have no seat at councils. They would argue for alternative ontological frameworks if admitted to the conversation but are kept out by the jurisdiction of Christian conciliar authority itself.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, alternative_christological_readings, excluded,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(homoousios_nicene__metaphysical_equality_reading, alternative_christological_readings).

% The Roman Emperor (Constantine and successors) convenes the Council of Nicaea, legitimates the homoousios formula through imperial authority and edict, and enforces it through legal power—confiscating Arian church property, exiling Arian bishops, eventually making non-Nicene Christianity illegal. The emperor benefits from religious unity (homoousios becomes the unifying doctrine) and uses ecclesiastical authority as a tool of state consolidation. The emperor's interest in enforcing homoousios is not purely doctrinal but political—the constraint stabilizes the church as a state-aligned institution.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, imperial_authority, agenda_setter,
    institutional, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_nicene__metaphysical_equality_reading, imperial_authority, beneficiary).

% Christian communities that accept the homoousios formula and benefit from the clarity, security, and communion access it provides. They have a unified faith anchored in conciliar definition; they inherit a stable ecclesiastical order; they avoid the cognitive and social burden of heresy accusations. They are not empowered to reject or revise the formula once concilially defined—their exit option is only assent. The constraint appears to them as natural doctrine, not extraction, because they identify with the outcomes it produces.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, orthodox_faithful, beneficiary,
    organized, generational, constrained, continental).

% Medieval, Reformation, and modern theological scholarship that interprets, critiques, and redefines homoousios within new frameworks (Thomas Aquinas reconciling Aristotle with Nicene theology, Protestant reformers re-interrogating conciliar authority, modern theology asking whether homoousios is metaphysically coherent or linguistically contingent). They analyze the constraint historically and philosophically but are not parties to its original enforcement; their role is analytical distance.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, later_theological_tradition, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(homoousios_nicene__metaphysical_equality_reading, nicene_episcopacy).
narrative_ontology:fixing_cost_class(homoousios_nicene__metaphysical_equality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified, conciliar definition of divine nature that enables the church to adjudicate orthodox christology, bind all bishoprics to one reading, and prevent doctrinal fragmentation. The coordination problem: early Christian theology permitted multiple incompatible readings of Christ's relation to the Father; without conciliar authority and a binding formula, the church faced endless dispute and schism. Homoousios (metaphysical equality) solves this by centralizing doctrinal authority in councils and making the formula binding on all Christians.
% TRANSFER_FUNCTION: Moves interpretive power from local bishops and alternative theologians to the conciliar-episcopal hierarchy. Subordinationist theologians and non-Nicene communities transfer doctrinal autonomy to the Nicene establishment, which exercises anathema and exile. The constraint also transfers ecclesiastical authority from horizontal episcopal consensus to vertical conciliar supremacy. Imperial authority transfers political consolidation needs onto the church's doctrinal machinery—the constraint becomes both ecclesiastical and political. Resources (property, legal standing, social position) flow from non-Nicene communities to the Nicene establishment.
% ABSENT_VOICES: Subordinationist and non-Nicene theologians are present at councils as defendants to be refuted, not as genuine interlocutors. Their voice is structured to be overridden by the homoousios formula, not heard. Non-Christian philosophical traditions (Neoplatonism, later Islamic theology) would argue for alternative frameworks of divine nature and unity/distinction if admitted, but are excluded from the ecclesiastical conversation by the jurisdiction of Christian conciliar authority itself. Women theologians and lay believers have no formal voice in doctrinal definition. Regional Christian communities outside the conciliar apparatus (Egypt, Syria, beyond the empire) are not represented.
% DISAPPEARANCE_RATIONALE: If the homoousios constraint and conciliar enforcement vanished, Christian theology would re-fragment into competing readings: subordinationism, honorific similarity, adoptionism, modalism would resurface as live theological options; ecclesiastical unity would fracture along regional and doctrinal lines; the Nicene creed's binding force would collapse; imperial political consolidation through a unified church would fail. Medieval and early modern Christianity would not exist in its actual form. The world of ecclesiastical Christendom depends on homoousios and its enforcement.
% FOUNDING_PROBLEM: Early Christian theology lacked a binding conciliar mechanism to settle fundamental disputes about Christ's divine nature and relation to the Father. Multiple Christian communities held incompatible christologies; subordinationist and alternative readings were widespread, articulate, and institutionally supported. This threatened ecclesiastical unity (different Christians held contradictory doctrines) and imperial political consolidation (Constantine needed a unified church as a tool of state power). The homoousios formula was devised at the Council of Nicaea to settle the dispute through conciliar authority binding all bishops to metaphysical equality and using imperial enforcement to suppress alternatives.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem was real and urgent: Constantine convened Nicaea because doctrinal chaos threatened both church unity and imperial order. The majority of bishops at Nicaea agreed that binding conciliar authority was necessary. However, by 365 CE (40 years after Nicaea), it became clear that homoousios was not solving the stated problem but entrenching a reading. When the Eusbian coalition dominated ecclesiastical politics under Constantius II (337–361 CE), they modified enforcement without solving the underlying doctrinal dispute—indicating the problem had been replaced, not solved. By 381 CE (Constantinople II), homoousios was defended not as a response to ongoing theological chaos but as the vehicle of absolute truth whose conciliar binding force was self-evident. Competition authorities (Protestant reformers, historical scholars, secular philosophers) attest that the founding problem—the need for conciliar unity against doctrinal chaos—was substantially solved by the mid-4th century, and homoousios persists thereafter not as a solution to active theological dispute but as institutional inertia and conciliar authority's extraction stake in the formula.
narrative_ontology:disappearance_verdict(homoousios_nicene__metaphysical_equality_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_nicene__metaphysical_equality_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_nicene__metaphysical_equality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.68) is high because the homoousios formula concentrates interpretive power in the conciliar hierarchy and excludes alternative christological readings from legitimate theological discourse. The formula is presented as self-evident truth (inevitable fact about God's nature), but it is in fact a human-authored doctrinal boundary that benefits those who control its interpretation. Suppression (0.82) is higher still because the constraint's persistence depends on active enforcement: anathema against heretics, imperial laws against non-Nicene communities, exile of bishops who resist, property confiscation, and increasingly internalized theological conviction that homoousios is obviously right. Theater (0.41) is moderate: the security function of homoousios (preventing doctrinal chaos) is real, but by the 4th century the constraint is increasingly defended as the vehicle of absolute truth rather than as a pragmatic solution to a historical problem. The measurements show extraction and suppression rising steeply from 325 to 365 CE (imperial enforcement at its height), then plateauing by 381 CE onward (doctrine becomes institutionalized and assumed to be self-evident). Theater rises throughout as the original pragmatic function becomes buried under metaphysical certainty. All three metrics are measured on one shared time grid (every metric at every time point 0, 5, 10, 15, 20, 30, 40) so temporal analysis has coherent data.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (nicene_episcopacy + imperial_authority) should compute as high beneficiary (low directionality) because they exercise the power to define doctrine, bind bishops, and exclude alternatives. They authorize the constraint and collect the concentrated power it produces. The payer seats (subordinationist_theologians, non_nicene_christian_communities) should compute as high-target (high directionality) because they are forced to submit, pay social/legal costs, and have constrained exit options. The orthodox_faithful seat sits near symmetric (d ≈ 0.5) because they gain unified communion and doctrinal clarity (beneficiary function) but lose the ability to question or revise the formula (constrained exit). The engine computes these directional divergences from the structural data—the beneficiary/victim declarations, power atoms, exit options, and spatial scope—without pre-judging whether the computed type matches the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Nicene bishops and imperial authority are the constraint's beneficiaries: they gain concentrated power to define orthodoxy, bind other bishops through conciliar rulings, excommunicate dissidents, and control the ecclesiastical apparatus. Their exit from the homoousios frame would mean surrendering conciliar authority to alternative readings or regional episcopal autonomy—a high-cost exit, placing them at low directionality (d near 0.0–0.2). Subordinationist theologians and non-Nicene communities are the constraint's victims: they are forced to submit their theology to conciliar judgment, face anathema and exile, lose property and legal standing, and have constrained exit (submit or separate into parallel communities that face institutional disadvantage). They are high-target seats at high directionality (d near 0.8–1.0). Orthodox faithful are somewhat trapped by identity—their Christian identity and ecclesiastical belonging become fused with Nicene orthodoxy—placing them at identity_locked exit; their directionality is moderate (d ≈ 0.4–0.6) because they gain communion security and unified doctrine but cannot revise it. The constraint's power rides on this directionality gradient: those who set it extract from those who must obey it.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a tangled_rope (not pure rope, not pure snare): it does solve a genuine problem (preventing doctrinal fragmentation and ecclesiastical chaos) but simultaneously enables extraction (conciliar authority consolidates power, subordinationist and non-Nicene readings are suppressed, the constraint's continued enforcement depends on active coercion—not just voluntary coordination). The coordination function (unified doctrine) is real; the extraction function (hierarchical power consolidation) is equally real and is embedded in the same structure. Without the extraction mechanism (anathema, imperial law, social pressure), the coordination function would weaken—alternative readings would resurge and doctrinal unity would fracture. This is the signature of tangled rope: coordination and extraction are inseparable in practice, both running through the same institutional mechanism (conciliar authority + imperial enforcement). The constraint would not persist in pure rope form without the active suppression that makes it tangled. A mandatrophy analysis asks: has the founding problem (doctrinal chaos, multiple incompatible christologies) actually been solved, or does it only appear solved because alternatives are suppressed? By the 4th century, evidence suggests the problem was solved by the historical dominance of homoousios—but the constraint persists and intensifies because conciliar authority now has a stake in defending homoousios as self-evident truth, not as a pragmatic solution to a dead problem. This is mild mandatrophy: the constraint's founding problem has shifted from 'How do we settle doctrinal chaos?' to 'How do we maintain conciliar supremacy and prevent doctrinal questioning?' The new mandate is less justified and more extractive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metaphysical_vs_linguistic_status,
    'Is homoousios a metaphysical claim about the actual nature of divine being, or a linguistic/conventional boundary imposed by conciliar authority for ecclesiastical order?',
    'Philosophical analysis: does homoousios describe a mind-independent fact about God''s nature, or does it describe a human-authored formula that the church treats as if it described such a fact? Historians and theologians from outside the Nicene establishment (Subordinationists, later critical scholars) would argue for the latter; Nicene theologians defend the former.',
    'If homoousios is metaphysical fact, the suppression and anathema of alternatives are justified as defense of truth. If homoousios is linguistic convention elevated to dogma, the suppression is elevated extraction: the conciliar hierarchy is using doctrinal authority to extract power by controlling interpretation of a fundamentally ambiguous metaphysical claim. This distinction affects whether the constraint is genuine coordination (unified doctrine around a true claim) or power consolidation dressed as truth-defense.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(metaphysical_vs_linguistic_status, conceptual, 'Whether homoousios is metaphysical or conventional; distinguishes genuine coordination from extraction-via-doctrine.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of subordinationist and non-Nicene readings structural (external legal and ecclesiastical force: anathema, exile, property confiscation, imperial law) or internalized (theologians internalize the claim that homoousios is obviously true and alternative readings are obviously false)?',
    'Historical and phenomenological analysis: in regions where imperial enforcement decayed (after 410 CE barbarian invasions; in non-Roman territories), did subordinationist and non-Nicene theology resurge, indicating structural suppression? Did theologians in those regions begin to question homoousios, or did they continue to accept it as self-evident despite reduced enforcement? Post-exit behavior: when exiled or anathematized bishops were restored to favor (during Constantius II''s semi-Arian period), did they immediately recant homoousios, or did they defend alternatives, indicating their suppression had been less internalized than it appeared?',
    'If structural, the constraint''s effective suppression is contingent on continued enforcement—withdrawal of imperial backing weakens it. If internalized, subordinationist and non-Nicene communities carry the suppression with them; even if enforcement machinery was removed, they would struggle to recover the theological confidence to articulate alternatives. Internalized suppression is harder to reverse and suggests deeper power consolidation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of alternatives is structural or internalized.').

omega_variable(
    kernel_identity_homoousios_vs_conciliar_authority,
    'Is the kernel (the thing under contest) the referent homoousios names (the metaphysical fact of Father-Son equality), or is the kernel the authority to define christological doctrine (conciliar supremacy)?',
    'Analyze what different readings are actually contesting. Subordinationists contest the claim that Father and Son are metaphysically equal—they dispute the referent. But they may or may not contest conciliar authority to settle doctrine (some early subordinationists accepted conciliar process, just disagreed with the outcome). Honorific-similarity readers contest what homoousios means—they may accept conciliar authority and still interpret the formula differently. This reading (metaphysical equality) treats the metaphysical claim as primary and uses conciliar authority as the mechanism to bind it. But later developments show conciliar authority becoming the kernel—the question shifts from ''Is homoousios true?'' to ''Can the council bind us to any doctrine?'' This is a different constraint masquerading under the same name.',
    'If the kernel is the metaphysical claim, sibling readings coexist_with or foreclose each other based on logical contradiction. If the kernel is conciliar authority, all three readings coexist_with each other under the rule that councils can bind any doctrine. This affects how we model the contest: is it metaphysical argument or institutional power competition? The constraint as authored treats metaphysical equality as the kernel; if future analysis shows conciliar authority is the actual kernel, this story would decompose (per ε-invariance rule) into a separate story about ecclesiastical governance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_identity_homoousios_vs_conciliar_authority, conceptual, 'What is the actual kernel being contested: metaphysical claim or conciliar authority?').

omega_variable(
    reading_historical_identity_nicene_vs_later_orthodox,
    'Does this story describe the Nicene homoousios formula of 325 CE, or the Nicene-Constantinopolitan formula of 381 CE, or the entire post-Nicene orthodox tradition that claims lineal descent from Nicaea?',
    'The 325 and 381 formulas differ substantively (381 adds material about the Spirit, clarifies some subordinationist language from 325). The post-Nicene orthodox tradition reinterprets homoousios through later philosophical frameworks (Cappadocian Fathers'' ousia-hypostasis distinction, medieval scholasticism). Each of these is a somewhat different constraint with slightly different ε values, beneficiary sets, and enforcement mechanisms. Clarification: which historical moment or textual version is the referent?',
    'If the referent is 325 CE Nicaea, the story should emphasize Constantine''s direct enforcement and the immediate subordinationist resistance. If the referent is 381 CE or later, the story should emphasize ecclesiastical rather than imperial enforcement and the theological reinterpretation that makes homoousios seem self-evident. The ε value might differ slightly: early enforcement is more overtly coercive; later orthodoxy relies more on internalized acceptance and theological refinement. The beneficiary story (imperial consolidation vs. ecclesiastical unity) shifts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_historical_identity_nicene_vs_later_orthodox, conceptual, 'Historical specificity of ''Nicene'' homoousios: which moment, which formula, which tradition?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_nicene__metaphysical_equality_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_tr_t0, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(homo_tr_t0, observed).
narrative_ontology:measurement(homo_tr_t5, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(homo_tr_t5, observed).
narrative_ontology:measurement(homo_tr_t10, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(homo_tr_t10, observed).
narrative_ontology:measurement(homo_tr_t15, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement_basis(homo_tr_t15, observed).
narrative_ontology:measurement(homo_tr_t20, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement_basis(homo_tr_t20, observed).
narrative_ontology:measurement(homo_tr_t30, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(homo_tr_t30, observed).
narrative_ontology:measurement(homo_tr_t40, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(homo_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(homo_be_t0, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(homo_be_t0, observed).
narrative_ontology:measurement(homo_be_t5, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(homo_be_t5, observed).
narrative_ontology:measurement(homo_be_t10, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 10, 0.59).
narrative_ontology:measurement_basis(homo_be_t10, observed).
narrative_ontology:measurement(homo_be_t15, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement_basis(homo_be_t15, observed).
narrative_ontology:measurement(homo_be_t20, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(homo_be_t20, observed).
narrative_ontology:measurement(homo_be_t30, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(homo_be_t30, observed).
narrative_ontology:measurement(homo_be_t40, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(homo_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(homo_su_t0, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(homo_su_t0, observed).
narrative_ontology:measurement(homo_su_t5, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement_basis(homo_su_t5, observed).
narrative_ontology:measurement(homo_su_t10, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 10, 0.72).
narrative_ontology:measurement_basis(homo_su_t10, observed).
narrative_ontology:measurement(homo_su_t15, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 15, 0.77).
narrative_ontology:measurement_basis(homo_su_t15, observed).
narrative_ontology:measurement(homo_su_t20, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 20, 0.8).
narrative_ontology:measurement_basis(homo_su_t20, observed).
narrative_ontology:measurement(homo_su_t30, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 30, 0.82).
narrative_ontology:measurement_basis(homo_su_t30, observed).
narrative_ontology:measurement(homo_su_t40, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 40, 0.82).
narrative_ontology:measurement_basis(homo_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_nicene__metaphysical_equality_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(homoousios_nicene__metaphysical_equality_reading, 0.12).
narrative_ontology:affects_constraint(homoousios_nicene__metaphysical_equality_reading, homoousios_nicene__subordinationist_reading).
narrative_ontology:affects_constraint(homoousios_nicene__metaphysical_equality_reading, homoousios_nicene__honorific_similarity_reading).
narrative_ontology:affects_constraint(homoousios_nicene__metaphysical_equality_reading, nicene_creed_enforcement_machinery).
narrative_ontology:affects_constraint(homoousios_nicene__metaphysical_equality_reading, conciliar_episcopal_authority_binding_power).

% DUAL FORMULATION NOTE:
% The homoousios_nicene kernel decomposes into three structurally distinct constraints corresponding to three readings: metaphysical_equality_reading (this file, high extractiveness, conciliar enforcement), subordinationist_reading (lower extractiveness, alternative metaphysical structure), and honorific_similarity_reading (intermediate extractiveness, ambiguous interpretive space). Each reading has distinct ε values, beneficiary/victim structures, and enforcement profiles. All three share the same kernel (the conciliar definition of homoousios) but instantiate different constraints because they answer differently what the kernel metaphysically means. The three constraints are linked via network.affects_constraints to show constraint family dependency: changes in one reading's institutional power affect the others' viability. They are NOT three versions of one constraint—they are three constraints arising from one contested kernel, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(homoousios_nicene__metaphysical_equality_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
