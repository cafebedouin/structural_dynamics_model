% ============================================================================
% CONSTRAINT STORY: homoousios_nicene__metaphysical_equality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: homoousios_nicene__metaphysical_equality_reading
 *   human_readable: Homoousios: Nicene Metaphysical Equality of Father and Son
 *   domain: theological/ecclesiastical
 *
 * SUMMARY:
 *   The Council of Nicaea (325 CE) issues the term homoousios (of one
 *   substance/essence) to define the Father-Son relationship as
 *   metaphysically equal, eternal, and undivided. This reading instantiates
 *   the Nicene doctrine: the Father and Son share the same divine essence;
 *   there is no subordination in being; the Son is not generated in time or
 *   created from nothing; trinitarian ontological equality is non-negotiable.
 *   This constraint coordinates Christian doctrine across the empire and
 *   simultaneously extracts authority into the episcopal hierarchy and
 *   suppresses subordinationist, arian, and non-conciliar readings. The
 *   constraint is claimed as tangled_rope (real coordination function +
 *   asymmetric extraction via enforced suppression) and the authored metrics
 *   reflect high extractiveness (0.78), high suppression (0.87), and rising
 *   theater (0.42), indicating that enforcement work has accumulated over the
 *   interval and coordination justification increasingly carries performative
 *   weight.
 *
 * KEY AGENTS:
 *   - Nicene episcopal orthodoxy: authorizes and enforces homoousios as metaphysical boundary; derives institutional power from successful enforcement.
 *   - Roman imperial authority: convenes councils, backs enforcement, gains religious unity and stability; co-beneficiary.
 *   - Subordinationist and Arian theologians: anathematized, suppressed, trapped; primary payer.
 *   - Non-conciliar Christian communities: marginalized, their theologies defined as heretical; payers with no voice.
 *   - Conciliar authority structure: the administrative apparatus that distributes interpretive power; sustains itself via homoousios enforcement.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_nicene__metaphysical_equality_reading, 0.78).
domain_priors:suppression_score(homoousios_nicene__metaphysical_equality_reading, 0.87).
domain_priors:theater_ratio(homoousios_nicene__metaphysical_equality_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 0.87).
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, accessibility_collapse, 0.81).
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_nicene__metaphysical_equality_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_nicene__metaphysical_equality_reading, "Homoousios: Nicene Metaphysical Equality of Father and Son").
narrative_ontology:topic_domain(homoousios_nicene__metaphysical_equality_reading, "theological/ecclesiastical").

domain_priors:requires_active_enforcement(homoousios_nicene__metaphysical_equality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_nicene__metaphysical_equality_reading, '733a5846-d930-48fb-8a6c-627fc483e446').
narrative_ontology:cs_kernel_codification('733a5846-d930-48fb-8a6c-627fc483e446', formalized).
narrative_ontology:cs_authority_grounding('733a5846-d930-48fb-8a6c-627fc483e446', extraction).
narrative_ontology:cs_interpretation_layer_present('733a5846-d930-48fb-8a6c-627fc483e446').
narrative_ontology:cs_reading_relation('733a5846-d930-48fb-8a6c-627fc483e446', homoousios_nicene__subordinationist_reading, forecloses).
narrative_ontology:cs_reading_relation('733a5846-d930-48fb-8a6c-627fc483e446', homoousios_nicene__honorific_similarity_reading, influences).
narrative_ontology:cs_axiom('733a5846-d930-48fb-8a6c-627fc483e446', foundational, trinitarian_ontological_equality_non_negotiable).
narrative_ontology:cs_axiom_status(trinitarian_ontological_equality_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('733a5846-d930-48fb-8a6c-627fc483e446', trinitarian_ontological_equality_non_negotiable, deontological).
narrative_ontology:cs_axiom('733a5846-d930-48fb-8a6c-627fc483e446', foundational, no_subordination_in_divine_being).
narrative_ontology:cs_axiom_status(no_subordination_in_divine_being, holdable).
narrative_ontology:cs_axiom_grounding('733a5846-d930-48fb-8a6c-627fc483e446', no_subordination_in_divine_being, deontological).
narrative_ontology:cs_axiom('733a5846-d930-48fb-8a6c-627fc483e446', secondary, conciliar_authority_competent_to_adjudicate_metaphysics).
narrative_ontology:cs_axiom_status(conciliar_authority_competent_to_adjudicate_metaphysics, holdable).
narrative_ontology:cs_axiom_grounding('733a5846-d930-48fb-8a6c-627fc483e446', conciliar_authority_competent_to_adjudicate_metaphysics, conventional).
narrative_ontology:cs_reference_frame('733a5846-d930-48fb-8a6c-627fc483e446', metaphysical_trinitarian_equality_framework).
narrative_ontology:cs_drift_state('733a5846-d930-48fb-8a6c-627fc483e446', council_of_chalcedon_451, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('733a5846-d930-48fb-8a6c-627fc483e446', '').
narrative_ontology:cs_kernel_id(homoousios_nicene__metaphysical_equality_reading, homoousios_nicene).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_nicene__metaphysical_equality_reading, nicene_episcopal_orthodoxy).
narrative_ontology:constraint_beneficiary(homoousios_nicene__metaphysical_equality_reading, conciliar_authority_structure).
narrative_ontology:constraint_victim(homoousios_nicene__metaphysical_equality_reading, arian_and_subordinationist_theologies).
narrative_ontology:constraint_victim(homoousios_nicene__metaphysical_equality_reading, non_conciliar_christian_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(homoousios_nicene__metaphysical_equality_reading, roman_imperial_authority).
narrative_ontology:constraint_beneficiary(homoousios_nicene__metaphysical_equality_reading, nicene_bishops_and_clergy).
narrative_ontology:constraint_beneficiary(homoousios_nicene__metaphysical_equality_reading, lay_christian_communities).
narrative_ontology:constraint_victim(homoousios_nicene__metaphysical_equality_reading, lay_christian_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The organized orthodox hierarchy that enforces homoousios as the metaphysical boundary. They convene councils, author canons, anathematize dissidents, and administer the constraint through episcopal succession and creedal authority. They benefit from consolidated institutional power and the authority to define legitimate Christianity. Their exit options include doctrinal reinterpretation (they can adjust what homoousios 'really means' within limits), but abandoning the constraint would require surrendering the institutional monopoly on Christian interpretation.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, nicene_episcopal_orthodoxy, agenda_setter,
    institutional, civilizational, arbitrage, universal).

% Constantine and his successors use homoousios to achieve religious uniformity and political stability. They convene councils, enforce the boundary, and suppress dissent using imperial machinery (legal penalty, exile, institutional dismantling). They collect religious peace and unified Christendom as the return. Their exit options include withdrawing imperial support for enforcement (Julian does this, though briefly), but sustaining empire-scale control requires some religious unification mechanism—homoousios provides it.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, roman_imperial_authority, beneficiary,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_nicene__metaphysical_equality_reading, roman_imperial_authority, agenda_setter).

% Theologians and movements (Arius, semi-Arians, later Nestorians and Monophysites) that read homoousios as compatible with subordination or that reject it outright. They are anathematized, their writings burned, their institutional bases dismantled, their voices excluded from legitimate conciliar conversation. They face recantation (renouncing their theology under coercion), exile (fleeing to regions outside imperial control), or institution extinction. They bear the constraint's suppression costs directly. Their powerful institutional influence (the semi-Arian party nearly controls ecumenical consensus 350–370) does not translate to exit options because the constraint operates via coercive machinery, not persuasion.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, arian_and_subordinationist_theologies, payer,
    powerful, biographical, trapped, continental).

% Syriac, Coptic, North African, and other Christian communities developing theology outside the conciliar apparatus or in resistance to it. They do not attend Nicaea or its successor councils; their theological readings (whether subordinationist, non-chalcedonian christological, or simply independent) place them outside orthodoxy by definition. Enforcement follows: imperial patronage withdrawn, liturgical suppression, institutional incorporation or dissolution. Their powerlessness and geographic remoteness does not constitute exit—trapping comes from imperial reach and institutional dismantling of alternatives.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, non_conciliar_christian_communities, payer,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(homoousios_nicene__metaphysical_equality_reading, non_conciliar_christian_communities, excluded).

% The organized clerical hierarchy that consolidates power through homoousios enforcement. Bishops adhering to Nicene orthodoxy gain imperial patronage, administrative authority over dioceses, and intellectual legitimacy. Those resisting the constraint face removal or marginalization. The constraint amplifies episcopal authority over lay Christians and competes with imperial direct rule. Their exit options are constrained: leaving the hierarchy means losing clerical status, though doctrinal reinterpretation offers some theological flexibility within the framework.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, nicene_bishops_and_clergy, beneficiary,
    organized, generational, constrained, continental).

% Receive doctrinal clarity and unified Christian identity enforced from above. They benefit from resolved theological chaos and religious peace across the empire. They pay through enforced orthodoxy, suppression of dissenting theology, loss of local theological autonomy, and absorption into a uniform creedal structure. Their choice set is entirely bounded by what the episcopal hierarchy permits. Exit is near-impossible without apostasy or exile.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, lay_christian_communities, beneficiary,
    powerless, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_nicene__metaphysical_equality_reading, lay_christian_communities, payer).

% The machinery of ecumenical councils, patriarchal succession, canons, and theological adjudication that distributes interpretive power within orthodoxy. It administers the homoousios constraint and derives its legitimacy FROM successful enforcement. The conciliar structure itself is the constraint's primary beneficiary—it would be delegitimized and potentially dissolved if the boundary collapsed or if alternative readings were permitted. Its analytical exit reflects that it has no situated interest independent of the constraint (it is the apparatus, not an agent using it).
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, conciliar_authority_structure, agenda_setter,
    institutional, civilizational, analytical, universal).

% Nestorian, Monophysite, and other christological traditions that develop readings of incarnation and trinitarian metaphysics incompatible with or outside the homoousios framework. They are condemned at councils (Ephesus 431, Chalcedon 451), their institutional bases severed, their leadership exiled or martyred. They flourish in the empire's margins (Nestorians in Persia and Central Asia, Monophysites in Egypt and Syria) precisely because imperial enforcement cannot reach them. They have no legitimate voice in conciliar conversation but continue to exist and teach outside the empire's control.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, heterodox_christological_schools, excluded,
    moderate, generational, trapped, regional).

% Court theologians, imperial bishops, and doctrinal advisors who observe and counsel on whether homoousios enforcement serves or destabilizes the empire. They have no permanent institutional stake but can shift imperial policy and enforcement priorities. Their analytical exit reflects their advisory role; they are not directly harmed or benefited by homoousios persistence, only instrumentally interested in its effects on imperial order.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, imperial_theological_advisors, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(homoousios_nicene__metaphysical_equality_reading, conciliar_authority_structure).
narrative_ontology:fixing_cost_class(homoousios_nicene__metaphysical_equality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies Christian doctrine across the empire by anchoring trinitarian theology in a shared metaphysical commitment: the Father and Son are of one divine essence (homoousios), eternally coequal, undivided. This resolves the theological chaos of the pre-Nicene period, in which competing Christologies (Arian, subordinationist, Docetic, etc.) fragmented Christian identity and threatened imperial religious peace. A single doctrinal grammar allows bishops, emperors, and lay Christians to speak from the same metaphysical foundation.
% TRANSFER_FUNCTION: Transfers authority over Christian metaphysics from local and lay theological reflection to the conciliar episcopal hierarchy. It also transfers the authority to define and suppress heterodoxy from communities to imperial-backed councils. The constraint moves interpretive power upward (to councils and patriarchs) and outward (to enforcing uniformity across Christendom). Theologically, it asserts that the Father and Son are metaphysically equal—a claim that only the conciliar apparatus is authorized to adjudicate.
% ABSENT_VOICES: Subordinationist theologians (Arius, his successors, semi-Arian moderates), non-conciliar Christian communities in Syriac, Coptic, and other traditions, lay theologians excluded from councils, and alternative christological schools (Nestorian, Monophysite, Ebionite traditions). They would argue that homoousios forecloses legitimate theological diversity, that the term itself was a forced innovation, that metaphysical equality does not account for the incarnation's asymmetry, and that imperial coercion is not a valid ground for doctrinal authority. Their absence from Nicaea and subsequent councils is structural: the conciliar apparatus does not seat them.
% DISAPPEARANCE_RATIONALE: If homoousios and its enforcement vanished, Christian theology would splinter back into competing subordinationist, arian, honorific, and non-conciliar christologies. The unified doctrinal empire would dissolve; imperial-backed orthodoxy would lose its metaphysical anchor; bishops would lose conciliar authority. The constraint is the linchpin of ecumenical Christianity and imperial religious control.
% FOUNDING_PROBLEM: Pre-Nicene Christianity lacks a unified metaphysical grammar for the Father-Son relationship. Competing theologies (Arius teaches the Son is a creature, eternally generated but not equal; Sabellians teach they are modes of one person; Docetists teach the incarnation is apparent, not real) fragment the faith, destabilize imperial religion, and create doctrinal chaos. The Council of Nicaea convenes to resolve this: it defines homoousios as the boundary, anathematizes subordinationism, and asserts metaphysical equality as the non-negotiable ground of Christian faith.
% FOUNDING_PROBLEM_CORROBORATION: The Nicene bishops attest the founding problem is solved: Arianism is anathematized, orthodoxy is unified, imperial peace is secured. However, the 4th and 5th centuries witness immediate defection (many bishops initially signed Nicaea under duress and later abandoned it; the semi-Arian party nearly captured the ecumenical consensus in the 350s–370s), the explosion of subordinationist theology in the East, the parallel rise of Nestorian and Monophysite schools outside the conciliar framework, and persistent lay confusion about what homoousios means. Outside corroborators (non-Nicene theologians, imperial administrators commenting on ongoing sectarian violence despite Nicaea) attest that the founding problem was NOT solved—only suppressed, and imperfectly. The constraint persists as a boundary maintained by enforcement, not by achieved unanimity.
narrative_ontology:disappearance_verdict(homoousios_nicene__metaphysical_equality_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_nicene__metaphysical_equality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_nicene__metaphysical_equality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(homoousios_nicene__metaphysical_equality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_nicene__metaphysical_equality_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness begins at 0.62 (Nicaea) because the constraint coordinates genuine theological unification AND immediately extracts authority into the conciliar apparatus. The founding theological work (defining homoousios, anathematizing Arius) is real coordination; the suppression machinery is extraction. Over the interval, extractiveness rises to 0.78 as imperial enforcement hardens, alternative readings are crushed, and the conciliar hierarchy consolidates control. Suppression is consistently high (0.71 to 0.87) because the constraint depends on active exclusion of subordinationist voices—these are not voluntary exits but enforced silences. Theater rises from 0.22 to 0.42 as the interval progresses: by the 5th century (Council of Ephesus 431, Council of Chalcedon 451), the constraint's coordination work is largely done (theological grammar established, imperial peace achieved), but enforcement activity continues theatrically—recitations of homoousios orthodoxy, anathematizations of shadow opponents, conciliar theater defending a settled truth. The measured theater tracks the growing disjunction between the real coordination (now historical) and the maintained enforcement (now performative).
 *
 * PERSPECTIVAL GAP:
 *   From the Nicene episcopal seat, homoousios solves the theological chaos of pre-Nicene Christianity and unifies Christendom under sound metaphysics; the constraint is genuine coordination. From the subordinationist or non-conciliar seats, homoousios is a coercive foreclosure of legitimate theological inquiry, imposed by imperial force and episcopal power-consolidation; the same structure operates as pure extraction. The engine computes per-seat classification from the structural data: Nicene bishops (role=beneficiary, arbitrage exit) will compute toward the lower-extraction end; subordinationist theologians (role=payer, trapped exit) will compute toward the high-extraction, snare-class end. The divergence is the measurement the system takes.
 *
 * DIRECTIONALITY LOGIC:
 *   Nicene orthodoxy and imperial authority benefit from the constraint: they collect doctrinal authority and institutional control. Subordinationist and non-conciliar communities lose—their voices are suppressed, their institutional bases dismantled, their exit routes blocked (recantation or extinction). The beneficiary/victim divide is sharp and durable across the interval. Lay Christian communities sit near-symmetric: they gain doctrinal clarity and religious peace; they lose theological autonomy and diversity. The conciliar authority structure derives its legitimacy FROM homoousios enforcement—it is neither purely beneficiary (it administers rather than merely collecting) nor purely payer, but its existence depends on the constraint persisting.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is theological chaos in the pre-Nicene period; the problem status at 451 is contested. The disappearance verdict is world_rearranges—the constraint is essential to maintaining Christian unity and imperial religious peace. However, the measurement series shows rising suppression and theater with only modest extractiveness growth: the constraint has become increasingly performative (theater +0.20 over 126 years) while enforcement requirements have hardened (suppression +0.16). This is diagnostic of mandatrophy creeping in: the founding problem (theological pluralism) is not SOLVED but SUPPRESSED. The conciliar apparatus must constantly reinforce the boundary because alternatives persist (semi-Arianism nearly captures councils in the 350s–370s; Nestorian and Monophysite schools thrive outside the empire; lay confusion persists about what homoousios means). By 451, Chalcedon reaffirms homoousios and anathematizes new heterodoxies, but the effort required has increased. The constraint is slowly transitioning from solving-a-problem-through-coordination to managing-a-problem-through-suppression. The theater ratio documents this drift: coordination work (real problem-solving) declines in proportion; enforcement theater (maintaining a boundary against persistent alternatives) rises.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metaphysical_vs_linguistic_homoousios,
    'Does homoousios denote metaphysical identity or linguistic/conceptual unity? Is the equality grounded in being (ousia) or in expression (homonymy)?',
    'Historical examination of pre-conciliar and post-conciliar usage, patristic commentary on whether homoousios admits of degrees or is absolute, 5th-century disputes over whether homoousios allows any real distinction between Father and Son.',
    'If metaphysical identity is mandatory, subordinationist readings are logically foreclosed and the constraint operates as a strict boundary. If the term admits linguistic flexibility, the honorific reading becomes structurally viable and suppression of it is purely extractive. The reading-classification divergence hinges on this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metaphysical_vs_linguistic_homoousios, conceptual, 'Metaphysical vs. linguistic grounding of homoousios identity.').

omega_variable(
    suppression_mechanism_internalized_or_structural,
    'Is the suppression of subordinationist theology structural (imperial legal and institutional machinery) or internalized (convinced acceptance of Nicene metaphysics)? Or both, and in what proportion?',
    'Post-suppression trajectory: when subordinationist theologies re-emerge in the 5th–6th centuries (Nestorian churches flourish in Persia and Asia; the Monophysite schism splits the empire), do they carry internalized orthodox frames or fully revived subordinationist metaphysics? If the latter, suppression was structural; if the former, some internalization occurred.',
    'If suppression is primarily structural, the constraint''s measured suppression (0.87) is the external machinery; subordinationist voices escape it geographically or temporally and re-emerge unchanged. If suppression is partly internalized, the constraint has re-written some theologians'' metaphysical commitments and the extraction is deeper (the target carries suppression even after escape). This affects whether subordinationists are correctly classified as payers or as partially-captured beneficiaries.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_or_structural, empirical, 'Structural vs. internalized suppression of heterodox christologies.').

omega_variable(
    imperial_extraction_vs_coordination_benefit,
    'Does the empire''s interest in homoousios enforcement arise from genuine preference for theological truth (coordination), from desire for religious peace (partial coordination), or from maximizing institutional control (extraction)? Are these separable?',
    'Comparison of imperial behavior when homoousios is enforced (Nicaea, Constantine''s support; Council of Constantinople 381, Theodosius''s enforcement) vs. when suppression wavers (Julian''s reign, imperial theology tilts tolerant), and imperial theological writings comparing their justifications.',
    'If empire motives are primarily extractive (consolidating power, not solving theology), the beneficiary/victim structure shifts: imperial authority becomes a pure extraction agent, and lay Christian communities lose their partial-beneficiary status (their doctrinal clarity is byproduct, not motivation). This would reclassify the constraint more sharply as snare-class from the imperial perspective, though Nicene bishops remain structural beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imperial_extraction_vs_coordination_benefit, empirical, 'Whether imperial interest in homoousios stems from coordination or extraction.').

omega_variable(
    conciliar_legitimacy_temporal_decay,
    'Does the conciliar authority structure''s legitimacy decay over the interval (as alternative readings proliferate and require increasing enforcement), or is it reinforced (as each new council reaffirms authority)?',
    'Temporal analysis of conciliar frequency, enforcement intensity, theological innovation in councils vs. outside councils, and acceptance of council verdicts among lay and clerical populations 325–451.',
    'If legitimacy decays, the theater ratio rise (0.22 to 0.42) reflects performing authority that is increasingly hollow—the constraint is becoming piton-class (inertial, maintained by reduced beneficiaries and diffuse payer costs). If legitimacy reinforces, the theater rise reflects successful normalization of what was once controversial (homoousios accepted, theater is celebration not defense). The trajectory determines whether mandatrophy is advancing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conciliar_legitimacy_temporal_decay, empirical, 'Temporal trajectory of conciliar authority''s legitimacy and acceptance.').

omega_variable(
    reading_kernel_committer_ambiguity,
    'Is the homoousios kernel itself independent of readings, or is the kernel defined BY the metaphysical_equality_reading? Can the three readings coexist in a single framework, or do they decompose into three different kernels?',
    'Historical study of whether 4th-century actors understood a unified homoousios kernel with multiple legitimate readings, or whether they understood three competing definitions of what homoousios MEANS (three kernels masquerading as one term).',
    'If three kernels: each reading is generatively independent, the framework is truly pluralist, and suppression is constraint-external (political power imposing one kernel, not adjudicating readings of one kernel). If one kernel and three readings: the framework permits metaphorical/honorific/functional distinctions within a unified metaphysical commitment—this is the official Nicene position. This omega documents the possibility that the ''kernel contest'' is actually a kernel-clash.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_committer_ambiguity, conceptual, 'Whether homoousios is one kernel with three readings or three competing kernel definitions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_nicene__metaphysical_equality_reading, 325, 451).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_tr_t325, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 325, 0.22).
narrative_ontology:measurement(homo_tr_t350, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 350, 0.28).
narrative_ontology:measurement(homo_tr_t375, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 375, 0.35).
narrative_ontology:measurement(homo_tr_t400, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 400, 0.4).
narrative_ontology:measurement(homo_tr_t425, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 425, 0.42).
narrative_ontology:measurement(homo_tr_t451, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 451, 0.42).

% Extraction over time
narrative_ontology:measurement(homo_be_t325, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 325, 0.62).
narrative_ontology:measurement(homo_be_t350, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 350, 0.68).
narrative_ontology:measurement(homo_be_t375, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 375, 0.75).
narrative_ontology:measurement(homo_be_t400, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 400, 0.77).
narrative_ontology:measurement(homo_be_t425, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 425, 0.78).
narrative_ontology:measurement(homo_be_t451, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 451, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(homo_su_t325, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 325, 0.71).
narrative_ontology:measurement(homo_su_t350, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 350, 0.79).
narrative_ontology:measurement(homo_su_t375, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 375, 0.84).
narrative_ontology:measurement(homo_su_t400, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 400, 0.86).
narrative_ontology:measurement(homo_su_t425, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 425, 0.87).
narrative_ontology:measurement(homo_su_t451, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 451, 0.87).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_nicene__metaphysical_equality_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(homoousios_nicene__metaphysical_equality_reading, 0.18).
narrative_ontology:affects_constraint(homoousios_nicene__metaphysical_equality_reading, homoousios_nicene__subordinationist_reading).
narrative_ontology:affects_constraint(homoousios_nicene__metaphysical_equality_reading, homoousios_nicene__honorific_similarity_reading).
narrative_ontology:affects_constraint(homoousios_nicene__metaphysical_equality_reading, nicene_creedal_authority).
narrative_ontology:affects_constraint(homoousios_nicene__metaphysical_equality_reading, episcopal_hierarchy_and_doctrinal_gatekeeping).

% DUAL FORMULATION NOTE:
% The homoousios_nicene kernel decomposes into three structurally distinct constraint readings: metaphysical_equality_reading (this file, ε=0.78, tangled_rope) asserts strict ontological equality and forecloses subordinationist metaphysics; subordinationist_reading (ε higher, snare-class from subordinationist perspective) permits ontological or functional subordination; honorific_similarity_reading (ε moderate, rope-class) permits linguistic flexibility and honorific unity without strict metaphysical identity. All three instantiate the same conciliar text ('Father and Son homoousios') but interpret it structurally differently. The ε-invariance principle requires three separate files because the metaphysical commitments diverge sharply—measuring subordinationist theology under the metaphysical-equality reading's referent yields high extraction because subordination is anathematized; measuring under the subordinationist reading's referent yields low extraction because subordination is legitimate. The readings are causally linked: metaphysical_equality_reading forecloses subordinationist_reading within Nicene frameworks; both influence honorific_similarity_reading by setting the debate's parameters.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(homoousios_nicene__metaphysical_equality_reading, institutional, 0.25).
constraint_indexing:directionality_override(homoousios_nicene__metaphysical_equality_reading, powerful, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
