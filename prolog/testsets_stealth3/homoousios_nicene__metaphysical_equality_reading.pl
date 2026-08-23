% ============================================================================
% CONSTRAINT STORY: homoousios_nicene__metaphysical_equality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   human_readable: Nicene Homoousios — Metaphysical Equality Reading (Enforced Conciliar Settlement)
 *   domain: historical_theology/ecclesiastical_history/philosophy_of_religion
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the homoousios kernel: the
 *   metaphysical-equality reading, on which the conciliar term secures full
 *   ontological equality of Father and Son — same divine essence, co-eternal,
 *   no subordination in being — as enforced by conciliar and imperial
 *   machinery from Nicaea (325) to the Theodosian consolidation (381). The
 *   standing arrangement under contest is that enforced settlement, and
 *   epsilon is authored for it by this reading's own lights: the reading
 *   holds the doctrine itself as true and necessary, while the structural
 *   data record what the settlement's operation cost its dissenters and
 *   dissenting communities. The constraint carries a genuine coordination
 *   function (one grammar of worship across a fractured empire-church) AND
 *   asymmetric extraction (heterodox christologies anathematized, careers and
 *   sees destroyed, interpretive monopoly vested in the episcopal hierarchy),
 *   which is why the claimed type is tangled_rope. The sibling readings —
 *   subordinationist_reading and honorific_similarity_reading — are separate
 *   constraints in separate files, linked through
 *   network.affects_constraints; per the epsilon-invariance principle this
 *   file neither describes nor averages over them. Claim and metrics are
 *   independent authored facts: the type claim states the structure; the
 *   metrics describe the operation; the engine computes per-seat
 *   classifications from the structural data.
 *
 * KEY AGENTS:
 *   - episcopal_conciliar_authority: agenda-setter (institutional/constrained) — defines, anathematizes, administers subscription; bound by its own prior definitions
 *   - pro_nicene_episcopate: primary beneficiary (institutional/identity_locked) — collects sees, interpretive monopoly, career security; its office is constituted by the settlement
 *   - imperial_state_authority: enforcement arm and contingent beneficiary (institutional/mobile) — supplies edicts and exile; shifts support between readings as court politics dictate
 *   - arian_subordinationist_bishops: primary target (organized/trapped by interval end) — bears deposition, exile, criminalization
 *   - semi_arian_homoiousian_party: squeezed middle target (organized/constrained) — mediating formula rejected by both poles, absorbed on unfavorable terms
 *   - anathematized_teacher_congregations: diffuse target (powerless/trapped) — lose teachers, buildings, sacramental continuity
 *   - orthodox_laity: coordinated beneficiaries bearing diffuse costs (powerless/constrained) — receive certainty and unity, pay in bound conscience and financed hierarchy
 *   - lay_theologians_and_monastics: excluded voice (moderate/constrained) — spiritual-practical criteria never seated
 *   - ecclesiastical_historians: analytical observer (analytical/analytical) — sees the full structure including the winners' custody of the record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_nicene__metaphysical_equality_reading, 0.66).
domain_priors:suppression_score(homoousios_nicene__metaphysical_equality_reading, 0.82).
domain_priors:theater_ratio(homoousios_nicene__metaphysical_equality_reading, 0.21).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, theater_ratio, 0.21).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_nicene__metaphysical_equality_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_nicene__metaphysical_equality_reading, "Nicene Homoousios — Metaphysical Equality Reading (Enforced Conciliar Settlement)").
narrative_ontology:topic_domain(homoousios_nicene__metaphysical_equality_reading, "historical_theology/ecclesiastical_history/philosophy_of_religion").

domain_priors:requires_active_enforcement(homoousios_nicene__metaphysical_equality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_nicene__metaphysical_equality_reading, '45285ec4-9234-491d-9eaa-9ad4b04011c6').
narrative_ontology:cs_kernel_codification('45285ec4-9234-491d-9eaa-9ad4b04011c6', formalized).
narrative_ontology:cs_authority_grounding('45285ec4-9234-491d-9eaa-9ad4b04011c6', lineage).
narrative_ontology:cs_interpretation_layer_present('45285ec4-9234-491d-9eaa-9ad4b04011c6').
narrative_ontology:cs_reading_relation('45285ec4-9234-491d-9eaa-9ad4b04011c6', homoousios_nicene__subordinationist_reading, forecloses).
narrative_ontology:cs_reading_relation('45285ec4-9234-491d-9eaa-9ad4b04011c6', homoousios_nicene__honorific_similarity_reading, forecloses).
narrative_ontology:cs_axiom('45285ec4-9234-491d-9eaa-9ad4b04011c6', foundational, son_true_god_from_true_god_not_created).
narrative_ontology:cs_axiom_status(son_true_god_from_true_god_not_created, holdable).
narrative_ontology:cs_axiom_grounding('45285ec4-9234-491d-9eaa-9ad4b04011c6', son_true_god_from_true_god_not_created, theological).
narrative_ontology:cs_axiom('45285ec4-9234-491d-9eaa-9ad4b04011c6', foundational, no_subordination_in_being_father_son).
narrative_ontology:cs_axiom_status(no_subordination_in_being_father_son, holdable).
narrative_ontology:cs_axiom_grounding('45285ec4-9234-491d-9eaa-9ad4b04011c6', no_subordination_in_being_father_son, theological).
narrative_ontology:cs_axiom('45285ec4-9234-491d-9eaa-9ad4b04011c6', secondary, creedal_anathema_binds_dissent).
narrative_ontology:cs_axiom_status(creedal_anathema_binds_dissent, holdable).
narrative_ontology:cs_axiom_grounding('45285ec4-9234-491d-9eaa-9ad4b04011c6', creedal_anathema_binds_dissent, conventional).
narrative_ontology:cs_reference_frame('45285ec4-9234-491d-9eaa-9ad4b04011c6', apostolic_faith_preserved_by_conciliar_definition).
narrative_ontology:cs_drift_state('45285ec4-9234-491d-9eaa-9ad4b04011c6', post_historical_critical_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('45285ec4-9234-491d-9eaa-9ad4b04011c6', '').
narrative_ontology:cs_kernel_id(homoousios_nicene__metaphysical_equality_reading, homoousios_nicene).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_nicene__metaphysical_equality_reading, pro_nicene_episcopate).
narrative_ontology:constraint_beneficiary(homoousios_nicene__metaphysical_equality_reading, imperial_state_authority).
narrative_ontology:constraint_beneficiary(homoousios_nicene__metaphysical_equality_reading, orthodox_laity).
narrative_ontology:constraint_victim(homoousios_nicene__metaphysical_equality_reading, arian_subordinationist_bishops).
narrative_ontology:constraint_victim(homoousios_nicene__metaphysical_equality_reading, semi_arian_homoiousian_party).
narrative_ontology:constraint_victim(homoousios_nicene__metaphysical_equality_reading, anathematized_teacher_congregations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(homoousios_nicene__metaphysical_equality_reading, orthodox_laity).
narrative_ontology:constraint_vindicates(homoousios_nicene__metaphysical_equality_reading, trinitarian_ontological_equality).
narrative_ontology:constraint_vindicates(homoousios_nicene__metaphysical_equality_reading, soteriological_divinity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convenes councils, drafts credal definitions, attaches anathemas, and administers subscription across the empire's churches. Once it defined the Son's relation to the Father with a technical term, retracting or redefining that term would destroy the credibility of every prior definition it issued, so its room for maneuver is bounded by its own earlier acts.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, episcopal_conciliar_authority, agenda_setter,
    institutional, generational, constrained, continental).

% Holds the major sees, chairs the tribunals, and controls which teachings count as legitimate. The settlement concentrates interpretive authority and career security in this body; a bishop who abandoned it would forfeit the office, standing, and identity the settlement constitutes.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, pro_nicene_episcopate, beneficiary,
    institutional, generational, identity_locked, continental).

% Enforces the settlement through edicts, exile decrees, and police action, and profits from religious uniformity as an instrument of governance. Its support is contingent and instrumental: successive emperors shifted enforcement between rival theological parties as court politics dictated, and it can redirect its machinery at will.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, imperial_state_authority, beneficiary,
    institutional, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_nicene__metaphysical_equality_reading, imperial_state_authority, agenda_setter).

% Taught that the Son derives his being from the Father and is divine without being equal. They commanded real networks of bishops and at times court influence, but bore the settlement's direct penalties: deposition, exile, confiscation, and after 381 criminal liability. By the interval's end their options inside the empire were confession, silence, or banishment.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, arian_subordinationist_bishops, payer,
    organized, biographical, trapped, continental).

% Proposed 'of similar substance' as a mediating formula and commanded a large eastern following through the 350s. Rejected by both poles, they were squeezed by successive settlements until absorption on unfavorable terms; their middle position bought them persecution from every direction and a seat at no final table.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, semi_arian_homoiousian_party, payer,
    organized, biographical, constrained, continental).

% Local churches whose teachers were deposed or exiled. They faced a choice between receiving replacement clergy imposed from the winning party or losing sacramental life altogether; their buildings, funds, and communal bonds passed to the settlement's administrators.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, anathematized_teacher_congregations, payer,
    powerless, biographical, trapped, regional).

% Receive doctrinal certainty, a unified baptismal confession, and liturgical commonality spanning languages and provinces. They memorize the creed at baptism, finance the clerical order through tithe and gift, and carry the cost of a bounded conscience: no legally tolerated alternative community exists to join, and private doubt has no institutional expression.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, orthodox_laity, beneficiary,
    powerless, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_nicene__metaphysical_equality_reading, orthodox_laity, payer).

% Produced influential ascetical and devotional theology and would have pressed practical and spiritual criteria against juridical definition, but held no seat in any council. Their absence from the deliberations meant the definition was framed entirely by juristically minded bishops, and their traditions entered the settlement only afterward, on the settlement's terms.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, lay_theologians_and_monastics, excluded,
    moderate, generational, constrained, regional).

% Reconstruct the controversy from council acts, imperial rescripts, exile lists, and surviving correspondence of all parties, including the losers. They see the full structure — including the winners' custody of the archival record — and can compare enforcement intensity across reigns and regions.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, ecclesiastical_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(homoousios_nicene__metaphysical_equality_reading, pro_nicene_episcopate).
narrative_ontology:fixing_cost_class(homoousios_nicene__metaphysical_equality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single shared ontological grammar for worship, baptism, and teaching across a linguistically and culturally fragmented empire-wide church, adjudicating incompatible claims about the object of worship so that congregations from Antioch to Gaul confess and baptize identically.
% TRANSFER_FUNCTION: Moves interpretive authority and doctrinal legitimacy from dispersed local teachers and minority christological communities to the episcopal conciliar center; moves salvational assurance and liturgical unity to confessing laity; moves careers, sees, buildings, and legal tolerance away from dissenting christologies.
% ABSENT_VOICES: The anathematized themselves sat in councils only as defendants; rural congregations received definitions without voice; lay theologians and monastics — who would have weighed ascetical and spiritual criteria — had no seat anywhere in the process. Dissent entered the record chiefly as the thing defined against.
% DISAPPEARANCE_RATIONALE: If the settlement vanished overnight, the status of Christ would immediately revert to open litigation: baptisms and liturgies would lose their common warrant, imperial religious law would be void, episcopal jurisdictions realign around rival formulas, and the entire subsequent architecture of Christian doctrine and ecumenical method — every later council defines by reference to this one — would lack its foundation.
% FOUNDING_PROBLEM: Arius's teaching split the churches on a question with sacramental consequences: if the Son is a creature, worship and baptism addressed to him appear idolatrous; if he is fully God, inherited monotheism appears compromised. The churches needed a public rule to adjudicate incompatible answers about the object of worship before communion itself fractured.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the eventual winners: Arius's own letters to Eusebius of Nicomedia and Bishop Alexander state the problem as its losers saw it; Eusebian moderate correspondence treats it as unresolved through the 340s; the pagan observer Ammianus Marcellinus records the episcopal strife as a civic fact; modern critical historians corroborate the crisis's reality independently of confessional allegiance. No fully neutral fourth-century seat existed — every contemporary party had a stake — so corroboration is cross-party rather than disinterested.
narrative_ontology:disappearance_verdict(homoousios_nicene__metaphysical_equality_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_nicene__metaphysical_equality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_nicene__metaphysical_equality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(homoousios_nicene__metaphysical_equality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_nicene__metaphysical_equality_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness settles at 0.66: the settlement delivered goods its subjects demonstrably valued (baptismal commonality, doctrinal certainty, an answer to the sacramental crisis), yet its operation destroyed the livelihoods, offices, and legal existence of dissenting christologies and concentrated interpretive authority in one body — extraction is substantial but not total because much of what it took was taken in exchange for goods received. Suppression is high (0.82) as a raw structural property: imperial edicts, exile decrees, ordered book destruction, and after 380 criminal liability; suppression is unscaled by power or scope — only extractiveness is scaled downstream. Theater is low (0.21): creed recitation and anathema ceremony are functional boundary maintenance, with modest ceremonial accretion. Accessibility collapse is 0.68, not higher: within the empire alternatives largely collapsed after 381 (Gregory of Nyssa could report Constantinople empty of dissenters), but the subordinationist tradition survived trans-frontier among the Gothic kingdoms for two centuries, so alternatives were displaced rather than annihilated. Resistance is 0.74: fifty-six years of council against council, riot, exile-cycle, and counter-definition. The measurement series run on one shared ten-point grid and display a full oscillatory cycle driven by dynastic favor — Constantine's initial enforcement, his deathbed leniency, the Eusebian sidestep of 341, Constantius II's homoian ascendancy peaking at the Sirmium ban and Ariminum-Seleucia, Julian's universal recall, Valens's eastern persecution of Nicenes, and the Theodosian restoration. Each swing of the pendulum purged one party's leadership, so the oscillation itself operated as an extraction mechanism (intermittent reinforcement): survival required riding every reversal. Base_properties describe the settled end-state operation after 381, measured at the consolidation phase of the cycle.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats compute differently from identical structural data. From the pro-Nicene episcopal seat the settlement is the necessary defense of worship's object — the anathemas are boundary-keeping, not predation. From the subordinationist bishop's seat the same machinery is career destruction enforced by police power. From the laity's seat it is both assurance and confinement. The conciliar seat experiences constraint as self-binding obligation; the imperial seat experiences no constraint at all, only a switchable instrument. The engine computes these per-seat classifications from power, exit, and directional position; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. The pro-Nicene episcopate sits nearest the beneficiary pole: it collects the settlement's rents (sees, authority, definition-rights) and is identity-locked — its office exists only inside the settlement. The imperial authority declares as beneficiary but its exit is mobile and its support historically reversible, damping its subsidy below the pure-beneficiary level. Subordinationist bishops and the homoiousian party sit nearest the target pole: declared victims, organized but progressively trapped as criminal liability closed legal exit. The anathematized congregations are trapped and powerless — maximal exposure. Orthodox laity occupy a genuinely dual position: declared beneficiaries who receive real coordination goods, yet also bear the settlement's diffuse costs (financing the hierarchy, bound conscience, no tolerated alternative), placing them nearer symmetric than the bare beneficiary declaration implies; their secondary payer role encodes this. Scope is continental, which amplifies effective extraction modestly at the target seats because verification of conformity across provinces favored enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification is what keeps both mislabelings visible. Reading the settlement as pure snare erases the genuine coordination — the sacramental commonality millions consented to and the real adjudication of the worship crisis — and would predict collapse once coercion lapsed, which the trans-frontier comparison tests. Reading it as pure rope erases the anathematized, whose careers, congregations, and legal existence were the settlement's operating cost. On the R5 interview the founding problem (adjudicating the object of worship) is contested rather than dead: within the tradition the settlement resolved it and its mandate remains live; from outside, the enforcement-specific mandate — criminalizing dissent — is dead in any modern secular order while the doctrinal content persists. The mismatch consumer should find status=contested paired with verdict=world_rearranges: arrangements still depend on it, and the parties dispute whether the founding problem is solved or frozen. No zombie flag fires because the arrangement's function has not atrophied into performance — theater stays low across the whole series.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_location,
    'This constraint instantiates the metaphysical_equality_reading of kernel homoousios_nicene; the disagreement among readings is located in what the term homoousios asserts about the Father-Son relation — numerical identity of essence versus likeness without reduction versus derivational subordination. How would the classification and victim set change under each sibling?',
    'Generate the sibling stories (subordinationist_reading, honorific_similarity_reading) as separate constraints and compare seat structures, victim sets, and computed types; the victim-set inversion under the subordinationist reading is the decisive diagnostic.',
    'Under the subordinationist reading the pro-Nicene episcopate enters the victim set and the enforcement asymmetry reverses; under the honorific reading the anathematized class shrinks to strict-identity insistents. Effective extraction and per-seat types move accordingly — the current classification holds only for this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Committer structure: one kernel, three readings; this story is the metaphysical-equality instantiation and its classification is reading-relative.').

omega_variable(
    necessity_vs_settlement,
    'Is the ontological-equality requirement a logical entailment of monotheistic worship of Christ (in which case part of the measured extraction is the price of coherence the tradition could not avoid), or a historically contingent settlement that benefited its administrators (in which case the extraction is enforcement rent on a contestable choice)?',
    'Analyze whether rival grammars available at the time (homoiousios mediation, economic-Trinity formulations) could have sustained the worship and baptismal practice without the incoherence the settlement claimed to remove; test against the actual arguments of the homoiousian party, which claimed exactly this viability.',
    'If entailment, the constraint carries a mountain-like necessity within the tradition''s premises and the tangled_rope reading overstates discretionary extraction; if contingent, the settlement is a chosen allocation of interpretive power and the extraction is attributable to the choice, sharpening the snare-side seat classifications.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_vs_settlement, conceptual, 'Whether the equality boundary was forced by the tradition''s own premises or selected among live alternatives.').

omega_variable(
    conviction_vs_coercion_persistence,
    'How much of the settlement''s persistence after 381 rested on genuine conviction among its subjects versus coercive enforcement capacity?',
    'Natural experiment across enforcement gradients: trans-frontier Gothic subordinationism persisted roughly two centuries with zero imperial coercion, while dissent inside the empire collapsed within a generation of criminalization; differential decay rates under matched starting conditions estimate the coercion share.',
    'If coercion-dominated, per-seat classifications shift snare-ward and the coordination function is smaller than claimed; if conviction-dominated, the settlement approaches rope and the suppression metric overstates the operative force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conviction_vs_coercion_persistence, empirical, 'Relative contribution of belief and force to the settlement''s durability.').

omega_variable(
    suppression_internalization,
    'Is the measured suppression structural (edicts, exile, criminal liability) or internalized (creed memorized at baptism, liturgical repetition, educated conscience treating the boundary as self-evident)?',
    'Post-enforcement trajectory: examine whether subordinationist or similarity movements revived when imperial enforcement capacity lapsed (post-imperial west, missionary contact zones); durable absence of revived dissent after coercion removal indicates internalized suppression.',
    'If substantially internalized, effective suppression exceeds the structural measure — the constraint travels with its subjects after enforcement ends — and the accessibility_collapse figure understates how completely alternatives closed in perception.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Structural versus internalized components of the settlement''s suppressive force.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_nicene__metaphysical_equality_reading, 325, 381).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homoousios_meta_eq_tr_t325, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 325, 0.18).
narrative_ontology:measurement(homoousios_meta_eq_tr_t333, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 333, 0.16).
narrative_ontology:measurement(homoousios_meta_eq_tr_t341, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 341, 0.14).
narrative_ontology:measurement(homoousios_meta_eq_tr_t349, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 349, 0.17).
narrative_ontology:measurement(homoousios_meta_eq_tr_t357, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 357, 0.12).
narrative_ontology:measurement(homoousios_meta_eq_tr_t359, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 359, 0.1).
narrative_ontology:measurement(homoousios_meta_eq_tr_t361, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 361, 0.14).
narrative_ontology:measurement(homoousios_meta_eq_tr_t367, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 367, 0.15).
narrative_ontology:measurement(homoousios_meta_eq_tr_t374, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 374, 0.18).
narrative_ontology:measurement(homoousios_meta_eq_tr_t381, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 381, 0.21).

% Extraction over time
narrative_ontology:measurement(homoousios_meta_eq_be_t325, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 325, 0.58).
narrative_ontology:measurement(homoousios_meta_eq_be_t333, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 333, 0.52).
narrative_ontology:measurement(homoousios_meta_eq_be_t341, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 341, 0.46).
narrative_ontology:measurement(homoousios_meta_eq_be_t349, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 349, 0.55).
narrative_ontology:measurement(homoousios_meta_eq_be_t357, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 357, 0.38).
narrative_ontology:measurement(homoousios_meta_eq_be_t359, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 359, 0.32).
narrative_ontology:measurement(homoousios_meta_eq_be_t361, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 361, 0.41).
narrative_ontology:measurement(homoousios_meta_eq_be_t367, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 367, 0.5).
narrative_ontology:measurement(homoousios_meta_eq_be_t374, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 374, 0.6).
narrative_ontology:measurement(homoousios_meta_eq_be_t381, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 381, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(homoousios_meta_eq_su_t325, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 325, 0.62).
narrative_ontology:measurement(homoousios_meta_eq_su_t333, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 333, 0.5).
narrative_ontology:measurement(homoousios_meta_eq_su_t341, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 341, 0.42).
narrative_ontology:measurement(homoousios_meta_eq_su_t349, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 349, 0.55).
narrative_ontology:measurement(homoousios_meta_eq_su_t357, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 357, 0.3).
narrative_ontology:measurement(homoousios_meta_eq_su_t359, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 359, 0.25).
narrative_ontology:measurement(homoousios_meta_eq_su_t361, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 361, 0.35).
narrative_ontology:measurement(homoousios_meta_eq_su_t367, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 367, 0.45).
narrative_ontology:measurement(homoousios_meta_eq_su_t374, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 374, 0.56).
narrative_ontology:measurement(homoousios_meta_eq_su_t381, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 381, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_nicene__metaphysical_equality_reading, identity_coordination).
narrative_ontology:affects_constraint(homoousios_nicene__metaphysical_equality_reading, subordinationist_reading).
narrative_ontology:affects_constraint(homoousios_nicene__metaphysical_equality_reading, honorific_similarity_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the Nicene settlement of homoousios' decomposes into three structurally distinct constraints — one per reading of the kernel (metaphysical_equality_reading, subordinationist_reading, honorific_similarity_reading). Each carries its own epsilon, beneficiary/victim structure, and victim set; they are linked here because the metaphysical-equality reading's enforcement shaped the operating environment of its siblings (its anathemas defined what the others had to survive), making it the downstream-dominant member despite arriving last chronologically as a stable settlement. Epsilon differs across the family because the referent arrangement differs: under the subordinationist reading the enforced arrangement is the derivational-subordination settlement and the victim set inverts; under the honorific reading the enforced arrangement is a likeness formula with a smaller anathematized class. The label confusion was in the language ('what Nicaea decided'), not in the structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
