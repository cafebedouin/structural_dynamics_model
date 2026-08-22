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
 *   human_readable: Nicene Homoousios as Enforced Metaphysical Equality Boundary
 *   domain: historical theology/ecclesiastical history/philosophy of religion
 *
 * SUMMARY:
 *   Between 325 and 381 the church, increasingly with imperial backing,
 *   enforced a specific reading of the homoousios clause: that Father and Son
 *   share one numerically identical divine essence, co-eternal, with no
 *   subordination in being. This file instantiates THAT reading as a clean,
 *   epsilon-invariant constraint — the enforced metaphysical-equality
 *   boundary with its creed, its anathemas, and its episcopal enforcement
 *   machinery. The colloquial label 'the Nicene homoousios' decomposes into
 *   three structurally distinct constraints (this reading, the
 *   subordinationist_reading, and the honorific_similarity_reading); the
 *   siblings are separate stories linked through network.affects_constraints,
 *   and their epsilon values differ because the victim sets, enforcement
 *   targets, and contested claims differ. The claim/metric independence rule
 *   is honored: the constraint is CLAIMED as tangled_rope because it carries
 *   both a genuine coordination function (a single confession solving the
 *   worship-coherence crisis) and asymmetric extraction (anathematized
 *   minorities losing sees, standing, and livelihood), while the authored
 *   metrics describe heavily extractive, actively enforced operation —
 *   particularly after 380, when the boundary fused with imperial legal
 *   monopoly. Time points map t = year minus 325 (t0 = Nicaea, t56 =
 *   Constantinople II).
 *
 * KEY AGENTS:
 *   - - nicene_episcopal_hierarchy: agenda-setter and principal beneficiary (institutional/identity_locked) — convenes councils, defines the confession, controls ordination and communion; collects deposed rivals' sees and concentrates interpretive authority
 *   - - roman_imperial_administration: contingent beneficiary and co-agenda-setter (institutional/arbitrage) — convokes councils and legislates conformity; committed to uniformity per se, not content, and switched backing between readings as dynastic politics shifted
 *   - - arian_subordinationist_clergy: primary target (organized/trapped) — teaches the Son's derived divinity; loses see, stipend, and legal standing under the enforced reading; recantation is the only cheap exit
 *   - - homoiousian_middle_party: secondary target (organized/constrained) — sought 'of like substance' as compromise; squeezed between anathema and absorption, its vocabulary erased from the authorized lexicon
 *   - - ordinary_laity: dual-positioned beneficiary/payer (moderate/constrained) — receives a single creed and legible communal identity; bears riots, splits, and imposed bishops with no formal voice
 *   - - gothic_arian_churches: extraterritorial target (organized/mobile) — carried the condemned reading beyond the frontier; flourished outside imperial jurisdiction under permanent stigma
 *   - - hellenic_philosophical_schools: excluded voice (moderate/mobile) — supplied the ousia/hypostasis vocabulary the debate consumed; objections never seated in the conciliar process
 *   - - modern_historical_theologians: analytical observer (analytical/analytical) — reconstructs the controversy from correspondence and acta; attests both the founding problem's reality and the enforcement asymmetry
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_nicene__metaphysical_equality_reading, 0.7).
domain_priors:suppression_score(homoousios_nicene__metaphysical_equality_reading, 0.8).
domain_priors:theater_ratio(homoousios_nicene__metaphysical_equality_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_nicene__metaphysical_equality_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_nicene__metaphysical_equality_reading, "Nicene Homoousios as Enforced Metaphysical Equality Boundary").
narrative_ontology:topic_domain(homoousios_nicene__metaphysical_equality_reading, "historical theology/ecclesiastical history/philosophy of religion").

domain_priors:requires_active_enforcement(homoousios_nicene__metaphysical_equality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_nicene__metaphysical_equality_reading, '771efcd5-5f94-4fbd-9d55-4e24a3fc0012').
narrative_ontology:cs_kernel_codification('771efcd5-5f94-4fbd-9d55-4e24a3fc0012', fixed_text).
narrative_ontology:cs_authority_grounding('771efcd5-5f94-4fbd-9d55-4e24a3fc0012', lineage).
narrative_ontology:cs_interpretation_layer_present('771efcd5-5f94-4fbd-9d55-4e24a3fc0012').
narrative_ontology:cs_reading_relation('771efcd5-5f94-4fbd-9d55-4e24a3fc0012', homoousios_nicene__subordinationist_reading, forecloses).
narrative_ontology:cs_reading_relation('771efcd5-5f94-4fbd-9d55-4e24a3fc0012', homoousios_nicene__honorific_similarity_reading, forecloses).
narrative_ontology:cs_axiom('771efcd5-5f94-4fbd-9d55-4e24a3fc0012', foundational, numerical_identity_of_divine_essence).
narrative_ontology:cs_axiom_status(numerical_identity_of_divine_essence, holdable).
narrative_ontology:cs_axiom_grounding('771efcd5-5f94-4fbd-9d55-4e24a3fc0012', numerical_identity_of_divine_essence, theological).
narrative_ontology:cs_axiom('771efcd5-5f94-4fbd-9d55-4e24a3fc0012', secondary, coeternity_of_son_excludes_derived_being).
narrative_ontology:cs_axiom_status(coeternity_of_son_excludes_derived_being, holdable).
narrative_ontology:cs_axiom_grounding('771efcd5-5f94-4fbd-9d55-4e24a3fc0012', coeternity_of_son_excludes_derived_being, theological).
narrative_ontology:cs_reference_frame('771efcd5-5f94-4fbd-9d55-4e24a3fc0012', conciliar_definition_as_apostolic_truth).
narrative_ontology:cs_drift_state('771efcd5-5f94-4fbd-9d55-4e24a3fc0012', contemporary_analytic_theology_era, gap(axiom_overriding, minor, false)).
narrative_ontology:cs_created_at('771efcd5-5f94-4fbd-9d55-4e24a3fc0012', '').
narrative_ontology:cs_kernel_id(homoousios_nicene__metaphysical_equality_reading, homoousios_nicene).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_nicene__metaphysical_equality_reading, nicene_episcopal_hierarchy).
narrative_ontology:constraint_beneficiary(homoousios_nicene__metaphysical_equality_reading, roman_imperial_administration).
narrative_ontology:constraint_victim(homoousios_nicene__metaphysical_equality_reading, arian_subordinationist_clergy).
narrative_ontology:constraint_victim(homoousios_nicene__metaphysical_equality_reading, homoiousian_middle_party).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(homoousios_nicene__metaphysical_equality_reading, ordinary_laity).
narrative_ontology:constraint_victim(homoousios_nicene__metaphysical_equality_reading, ordinary_laity).
narrative_ontology:constraint_victim(homoousios_nicene__metaphysical_equality_reading, gothic_arian_churches).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convenes councils, drafts and guards the confession, controls ordination and communion across the church network. When rival teachers are condemned, their sees, stipends, and congregations pass to conforming bishops, and interpretive authority over the faith concentrates in this body. Its members' entire formation, status, and social existence are constituted by the institution whose boundaries they police; leaving it means ceasing to be what they are.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, nicene_episcopal_hierarchy, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_nicene__metaphysical_equality_reading, nicene_episcopal_hierarchy, beneficiary).

% Convokes councils at imperial expense, banishes condemned teachers, burns condemned writings, and after 380 makes conformity a condition of civic standing. Its benefit is uniformity as such — a single legible religion for a single polity — and its commitment runs to order rather than content: under Constantius II it pivoted to enforce the rival reading, demonstrating that its position is held by arbitrage rather than conviction.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, roman_imperial_administration, beneficiary,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_nicene__metaphysical_equality_reading, roman_imperial_administration, agenda_setter).

% Teaches that the Son derives his divinity from the Father and is subordinate in being. Under the enforced reading its teachers lose their sees, stipends, legal standing, and eventually their congregations; Arius dies in disgrace on the eve of reinstatement, and the party's books are burned by imperial order. Recantation is the only inexpensive exit and it costs the doctrinal integrity that made their position worth holding. Their organized strength — court connections, major sees, popular support in Alexandria and Constantinople — sustained decades of resistance.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, arian_subordinationist_clergy, payer,
    organized, biographical, trapped, continental).

% Proposed 'of like substance' as a formula preserving the Son's true divinity without what it read as materialist connotations in 'same substance.' Squeezed from both sides: refusal of homoousios classes them with the condemned, signing absorbs them and erases their compromise vocabulary from the authorized lexicon. Their exit is constrained because every available formula either condemns them or dissolves their position.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, homoiousian_middle_party, payer,
    organized, biographical, constrained, continental).

% Receives a single creed, a common baptismal formula, and a legible boundary of communal belonging — genuine goods in a movement that had grown faster than its self-understanding. Also bears the costs: riots between congregational factions, bishops imposed or expelled by distant politics, and parish loyalties shattered when sees change hands. Has no formal channel through which to contest or ratify a definition.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, ordinary_laity, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(homoousios_nicene__metaphysical_equality_reading, ordinary_laity, payer).

% Ulfilas' mission carried the condemned reading beyond the imperial frontier, where it became the Christianity of the Gothic kingdoms and flourished for centuries. Outside the enforcement machinery's jurisdiction they suffered no exile or deposition, yet lived under permanent stigma from the imperial church — the enforcement pushed its targets outward rather than extinguishing them, and the stigma traveled with the label wherever they went.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, gothic_arian_churches, payer,
    organized, generational, mobile, continental).

% Neoplatonist and Peripatetic metaphysicians supplied the ousia and hypostasis vocabulary the controversy consumed, and recognized the borrowing — some objected that substance-language imported material connotation into divine predication. They stood wholly outside the confessional boundary: never seated in any council, their objections entered the record only as pagan commentary. They could ignore the machinery entirely, which is precisely why they were never part of the conversation that fixed the term's meaning.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, hellenic_philosophical_schools, excluded,
    moderate, civilizational, mobile, continental).

% Reconstructs the controversy from conciliar acta, episcopal correspondence, and polemical literature; attests both the reality of the founding dispute and the asymmetry of its enforcement. Holds no stake in either reading's triumph and can compare the enforced semantics against the pre-Nicene textual record.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, modern_historical_theologians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(homoousios_nicene__metaphysical_equality_reading, nicene_episcopal_hierarchy).
narrative_ontology:fixing_cost_class(homoousios_nicene__metaphysical_equality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Settled a live dispute over the object of Christian worship — whether the Son is true God or a created intermediary — producing a single shared confession, a common baptismal formula, and a determinate criterion for episcopal communion across a geographically dispersed church network that had been ordaining mutually incompatible theologies.
% TRANSFER_FUNCTION: Moves interpretive authority over the faith from local presbyters, congregations, and rival teachers to the conciliar-episcopal hierarchy; moves concrete goods — sees, stipends, legal standing, congregations — from condemned clergy to conforming bishops; moves compliance (signed creeds, anathema recitations) from the governed clergy to the enforcing center.
% ABSENT_VOICES: Lay congregations had no formal voice in any council that defined what they would confess. The Hellenic philosophical schools whose vocabulary the definition consumed were never seated. After 381 the subordinationist tradition survived only outside imperial jurisdiction or in whispered continuity — its holders could no longer speak in the authorized venues. The homoiousian middle party was present at the councils but overruled, its compromise position erased from the final texts except as an implicit target.
% DISAPPEARANCE_RATIONALE: If the enforced boundary vanished overnight, the church would reopen the fourth-century controversy at full intensity: baptismal formulas would diverge, communion between sees would fracture along ontological lines, the imperial religious settlement of 380 would lose its object, and the later creedal tradition (Constantinople, Chalcedon, the Athanasian formula) would lose its foundation. The architecture of both Eastern and Western Christianity presupposes this boundary.
% FOUNDING_PROBLEM: Whether the Son is true God from true God or a created intermediary — a question forced by Arius' preaching that threatened the coherence of baptismal confession, the propriety of worship addressed to Christ, and the civic peace of an empire whose unity increasingly ran through the church.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: Arius' own letters and the Thalia fragments attest the dispute from the condemned side; the correspondence of Eusebius of Nicomedia maps the episcopal alignments independent of victors' records; the pagan Ammianus Marcellinus records the turmoil as external observation; and modern critical historiography (R.P.C. Hanson, The Search for the Christian Doctrine of God) confirms both the reality of the founding problem and the enforcement asymmetry this story encodes. The status is contested because the ecclesiastical question (communion criteria) was settled while the underlying ontological question remains live in analytic theology and philosophy of religion.
narrative_ontology:disappearance_verdict(homoousios_nicene__metaphysical_equality_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_nicene__metaphysical_equality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_nicene__metaphysical_equality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(homoousios_nicene__metaphysical_equality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_nicene__metaphysical_equality_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is 0.70 at interval end because the enforced boundary transferred concrete goods — sees, stipends, legal standing, interpretive authority — from condemned clergy to conforming ones, and the transfer rate accelerated once imperial law made conformity compulsory. Suppression is 0.80 as a RAW structural property (unscaled by power or scope; only extractiveness is scaled in the engine's computation): banishment, deposition, book burning, and after 380 statutory compulsion. Theater is 0.38: the doctrinal stakes were real, but creedal recitation progressively routinized after victory, and the mid-interval forced-subscription episodes show conformity gathered as signature rather than conviction. Accessibility_collapse is 0.55 — alternatives were expelled from the imperial core but survived externally (Gothic churches) and resurfaced internally whenever enforcement slackened, so alternatives narrowed without collapsing. Resistance is 0.70: five exiles of Athanasius, Alexandrian riots, and decades of controversy attest sustained active opposition. The measurement series run on ONE shared eight-point grid (every tracked metric authored at every examined time point). Base_extractiveness traces a V: the boundary's extraction collapsed during 335-361 when the homoian party held the enforcement machinery and this constraint operated defensively, then rebuilt and exceeded its starting level after 380 — the constraint returned fused with monopoly, which is why the endpoint exceeds the Nicaea-era value. Suppression_requirement is authored because this story specifically tracks enforcement-capacity change: decay through the middle interval, rebuild under the pro-Nicene network, spike at the Theodosian settlement. Theater_ratio drifts gently upward throughout as victory converts living controversy into routine confession. The cyclical pattern (ascendancy, displacement, restoration) is documented rather than smoothed; omega dynastic_oscillation_mechanism flags whether the cycle itself was an extraction mechanism.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and the divergence is the data. From the episcopal seat the arrangement is a stewardship it built: a hard-won settlement of the worship-coherence crisis, defended against destabilizing innovation — a coordination experience. From the trapped clerical seats the identical structure operates as confiscatory: career, community, and standing seized for a metaphysical formula they read differently. The imperial seat experiences neither: its commitment ran to uniformity, its exit was arbitrage (Constantius II demonstrated the pivot by backing the rival reading), and its directionality is contingent on which reading serves order. The laity sit near symmetric — genuine liturgical and identity benefit against diffuse costs borne without voice. Coalition potential matters for the target seats: the subordinationist and homoiousian parties repeatedly aligned against the enforced formula, and their combined organized strength was sufficient to capture the enforcement machinery for a generation — the constraint's survival was never structurally guaranteed.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for the episcopal hierarchy (collects sees and authority; identity_locked exit amplifies its stake) and moderate-low for the imperial administration (benefits from uniformity per se but bears enforcement costs and holds arbitrage exit, damping its derived beneficiary position). Victim declarations drive high directionality for arian_subordinationist_clergy (trapped exit places them near the full-target end) and homoiousian_middle_party (constrained exit, slightly less severe). Ordinary laity derive near-symmetric d from their dual beneficiary/payer position. Gothic arian churches carry high d but mobile exit — the enforcement pushed them beyond the machinery's reach, so effective extraction is dampened relative to their structural target position. Hellenic philosophical schools are excluded rather than coordinated; their exclusion is commentary-grade context, not a classification input. No directionality overrides are authored: the beneficiary/victim declarations plus exit options produce the correct relationships, and the imperial seat's contingency is handled by its arbitrage exit rather than an override.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two symmetrical misreadings. Reading the settlement as pure doctrine — a mountain of revealed metaphysics or a clean rope of voluntary consensus — erases the anathematized: the exiled clergy, the erased homoiousian vocabulary, the burned books. Reading it as pure persecution — a snare wearing a creed as cover — erases the genuine coordination achievement: before Nicaea the church lacked a shared answer to whether baptism into Christ was monotheist worship at all, and the boundary did solve that problem for the overwhelming majority who assented. Tangled rope preserves both facts: real coordination function, asymmetric extraction through the same structure, active enforcement required to hold it. The founding-problem interview keeps the genealogy open: the ontological question the boundary answered is contested rather than dead, which blocks any mandatrophy resolution — the mandate has not outlived its function so much as its function remains disputed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_semantics_ambiguity,
    'Does the credal term homoousios carry the metaphysical-equality semantics this reading assigns to it (numerical identity of essence), or the similarity semantics of the honorific_similarity_reading?',
    'Philological analysis of pre-Nicene usage (Origen''s flexible application, the Dionysius of Alexandria controversy), the drafting history of the creed''s anathemas, and reception history of the term between 325 and 381.',
    'If similarity semantics are correct, the victim set contracts sharply (the homoiousian middle party becomes legitimate rather than anathematized), measured suppression drops, and the computed type shifts toward pure coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_semantics_ambiguity, conceptual, 'Committer-frame omega: this story is one reading of kernel homoousios_nicene; sibling readings assign different semantics to the same term.').

omega_variable(
    enforcement_coordination_separability,
    'Is the anathema-and-exile machinery structurally necessary to the boundary''s coordination function, or separable from it?',
    'Compare communities beyond enforcement reach: the Gothic churches retained the condemned reading for centuries without imperial coercion, while the Armenian church adopted Nicene terminology without the imperial enforcement apparatus — if doctrinal stability held where enforcement did not reach, the functions are separable.',
    'If separable, the extraction component is enforcement overhead riding on a genuine confession, pushing the computed type toward snare; if inseparable, part of the measured extraction is the price of the coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_coordination_separability, empirical, 'Whether the boundary''s coordination and coercive components can be structurally distinguished.').

omega_variable(
    imperial_capture_or_completion,
    'Was the post-380 fusion of the metaphysical-equality boundary with imperial legal monopoly an external capture of a doctrinal consensus, or the consensus''s natural completion?',
    'Trace the internal church record independently of imperial acta: synodal correspondence, the Cappadocian writings, and ascetic-network testimony show whether the hierarchy sought imperial enforcement before Theodosius offered it.',
    'If capture, the t56 extraction spike attributes to the captor rather than the constraint and the effective extraction of the episcopal seat should be discounted; if completion, the spike is the constraint''s own mature form.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(imperial_capture_or_completion, conceptual, 'Attribution ambiguity for the late-interval extraction intensification.').

omega_variable(
    dynastic_oscillation_mechanism,
    'Was the 325-381 oscillation between Nicene and homoian ascendancy an exogenous dynastic accident, or an intermittent-reinforcement mechanism that itself disciplined conformity?',
    'Model episcopal career survival as a function of doctrinal flexibility versus conviction across reign changes; if survivors were disproportionately the flexible, the oscillation functioned as a selection filter.',
    'If the oscillation trained conformity, part of the post-381 stability is preference falsification rather than conviction, meaning the constraint''s apparent consolidation overstates its genuine coordination achievement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dynastic_oscillation_mechanism, empirical, 'Whether the cyclical enforcement history is noise or an extraction mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_nicene__metaphysical_equality_reading, 0, 56).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homoousios_eq_tr_t0, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(homoousios_eq_tr_t8, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(homoousios_eq_tr_t16, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 16, 0.25).
narrative_ontology:measurement(homoousios_eq_tr_t24, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 24, 0.28).
narrative_ontology:measurement(homoousios_eq_tr_t32, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 32, 0.3).
narrative_ontology:measurement(homoousios_eq_tr_t40, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 40, 0.32).
narrative_ontology:measurement(homoousios_eq_tr_t48, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 48, 0.35).
narrative_ontology:measurement(homoousios_eq_tr_t56, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 56, 0.38).

% Extraction over time
narrative_ontology:measurement(homoousios_eq_be_t0, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(homoousios_eq_be_t8, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 8, 0.35).
narrative_ontology:measurement(homoousios_eq_be_t16, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 16, 0.25).
narrative_ontology:measurement(homoousios_eq_be_t24, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 24, 0.2).
narrative_ontology:measurement(homoousios_eq_be_t32, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 32, 0.18).
narrative_ontology:measurement(homoousios_eq_be_t40, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 40, 0.3).
narrative_ontology:measurement(homoousios_eq_be_t48, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 48, 0.45).
narrative_ontology:measurement(homoousios_eq_be_t56, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 56, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(homoousios_eq_su_t0, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(homoousios_eq_su_t8, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(homoousios_eq_su_t16, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 16, 0.3).
narrative_ontology:measurement(homoousios_eq_su_t24, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 24, 0.25).
narrative_ontology:measurement(homoousios_eq_su_t32, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 32, 0.22).
narrative_ontology:measurement(homoousios_eq_su_t40, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 40, 0.35).
narrative_ontology:measurement(homoousios_eq_su_t48, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 48, 0.5).
narrative_ontology:measurement(homoousios_eq_su_t56, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 56, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_nicene__metaphysical_equality_reading, identity_coordination).
narrative_ontology:affects_constraint(homoousios_nicene__metaphysical_equality_reading, subordinationist_reading).
narrative_ontology:affects_constraint(homoousios_nicene__metaphysical_equality_reading, honorific_similarity_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'the Nicene homoousios' per the epsilon-invariance principle: the single term supports three structurally distinct constraints with different epsilon values, victim sets, and enforcement histories. This story (metaphysical_equality_reading) is the upstream member — it won the contest, became the authorized semantics, and its enforcement machinery is what displaced the sibling readings' adherents. The upstream reading influences the downstream siblings by defining the anathematized position each was forced into: subordinationist_reading survives as the condemned minority tradition (and later extraterritorial Gothic Christianity), honorific_similarity_reading as the erased middle-party compromise. Each sibling story links back to this one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
