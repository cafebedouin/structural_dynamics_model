% ============================================================================
% CONSTRAINT STORY: homoousios_christology__pro_nicene_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homoousios_christology__pro_nicene_reading, []).

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
 *   constraint_id: homoousios_christology__pro_nicene_reading
 *   human_readable: Enforced Homoousios Confession (Pro-Nicene Settlement)
 *   domain: historical theology/ecclesiastical politics/commitment systems
 *
 * SUMMARY:
 *   At Nicaea (325) the church defined the Son as homoousios — identical in
 *   substance with the Father — and appended anathemas against contrary
 *   formulations; at Constantinople (381) the settlement was reimposed at
 *   full imperial strength. The standing arrangement this story is about is
 *   the enforced confession: a creedal formula administered by an episcopal
 *   hierarchy, backed by imperial edict and banishment, with communion itself
 *   as both the prize and the penalty. The arrangement solves a real
 *   coordination problem — one confession, one communion, one baptized object
 *   of worship — while transferring offices, jurisdiction, and social
 *   existence from dissenting clergy and congregations to the enforcing
 *   order, and delivering religious legitimation to the imperial
 *   administration. The ε referent is the standing pro-Nicene arrangement
 *   itself, assessed by this reading's own lights: the reading holds the
 *   doctrine true and the enforcement justified, and the structural costs
 *   dissenters bear are nonetheless what ε measures. Claim/metric
 *   independence: claimed_type is my structural judgment (genuine
 *   confessional coordination plus asymmetric extraction, actively enforced);
 *   the metrics are my descriptive estimates of the arrangement's actual
 *   operation; the engine computes per-seat types from the structural data.
 *   KEY AGENTS (by structural relationship): - nicene_episcopal_hierarchy:
 *   Agenda-setter and primary beneficiary (institutional/identity_locked) —
 *   administers the confession, collects vacated sees and definitional
 *   authority - imperial_administration: Beneficiary with enforcement arm
 *   (institutional/mobile) — collects legitimation and administrative unity -
 *   homoian_clergy: Primary target (organized/constrained) — bears deposition
 *   and exile - semi_arian_clergy: Secondary target (organized/constrained) —
 *   bears condemnation of the compromise formula - dissenting_congregations:
 *   Diffuse target (powerless/trapped) — bears communion exclusion -
 *   gothic_arian_churches: Excluded actor (organized/arbitrage) — outside
 *   enforcement reach - modern_historical_theologians: Analytical observer —
 *   sees the full structure
 *
 * KEY AGENTS:
 *   - nicene_episcopal_hierarchy: Agenda-setter and primary beneficiary (institutional/identity_locked) — administers the confession, collects vacated sees and definitional authority
 *   - imperial_administration: Beneficiary with enforcement arm (institutional/mobile) — collects legitimation and administrative unity
 *   - homoian_clergy: Primary target (organized/constrained) — bears deposition and exile
 *   - semi_arian_clergy: Secondary target (organized/constrained) — bears condemnation of the compromise formula
 *   - dissenting_congregations: Diffuse target (powerless/trapped) — bears communion exclusion
 *   - gothic_arian_churches: Excluded actor (organized/arbitrage) — outside enforcement reach
 *   - modern_historical_theologians: Analytical observer — sees the full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_christology__pro_nicene_reading, 0.78).
domain_priors:suppression_score(homoousios_christology__pro_nicene_reading, 0.85).
domain_priors:theater_ratio(homoousios_christology__pro_nicene_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, accessibility_collapse, 0.66).
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_christology__pro_nicene_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_christology__pro_nicene_reading, "Enforced Homoousios Confession (Pro-Nicene Settlement)").
narrative_ontology:topic_domain(homoousios_christology__pro_nicene_reading, "historical theology/ecclesiastical politics/commitment systems").

domain_priors:requires_active_enforcement(homoousios_christology__pro_nicene_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_christology__pro_nicene_reading, '5bfb65c9-cd24-4053-a887-fd53eb314624').
narrative_ontology:cs_kernel_codification('5bfb65c9-cd24-4053-a887-fd53eb314624', fixed_text).
narrative_ontology:cs_authority_grounding('5bfb65c9-cd24-4053-a887-fd53eb314624', extraction).
narrative_ontology:cs_interpretation_layer_present('5bfb65c9-cd24-4053-a887-fd53eb314624').
narrative_ontology:cs_reading_relation('5bfb65c9-cd24-4053-a887-fd53eb314624', homoousios_christology__arian_reading, forecloses).
narrative_ontology:cs_reading_relation('5bfb65c9-cd24-4053-a887-fd53eb314624', homoousios_christology__semi_arian_reading, forecloses).
narrative_ontology:cs_axiom('5bfb65c9-cd24-4053-a887-fd53eb314624', foundational, son_identical_divine_substance).
narrative_ontology:cs_axiom_status(son_identical_divine_substance, holdable).
narrative_ontology:cs_axiom_grounding('5bfb65c9-cd24-4053-a887-fd53eb314624', son_identical_divine_substance, theological).
narrative_ontology:cs_axiom('5bfb65c9-cd24-4053-a887-fd53eb314624', secondary, conciliar_definition_binds_communion).
narrative_ontology:cs_axiom_status(conciliar_definition_binds_communion, holdable).
narrative_ontology:cs_axiom_grounding('5bfb65c9-cd24-4053-a887-fd53eb314624', conciliar_definition_binds_communion, conventional).
narrative_ontology:cs_reference_frame('5bfb65c9-cd24-4053-a887-fd53eb314624', enforced_consubstantial_communion).
narrative_ontology:cs_drift_state('5bfb65c9-cd24-4053-a887-fd53eb314624', contemporary_ecumenical_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5bfb65c9-cd24-4053-a887-fd53eb314624', '2026-06-11T00:00:00Z').
narrative_ontology:cs_kernel_id(homoousios_christology__pro_nicene_reading, homoousios_christology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_christology__pro_nicene_reading, nicene_episcopal_hierarchy).
narrative_ontology:constraint_beneficiary(homoousios_christology__pro_nicene_reading, imperial_administration).
narrative_ontology:constraint_victim(homoousios_christology__pro_nicene_reading, homoian_clergy).
narrative_ontology:constraint_victim(homoousios_christology__pro_nicene_reading, semi_arian_clergy).
narrative_ontology:constraint_victim(homoousios_christology__pro_nicene_reading, dissenting_congregations).
narrative_ontology:constraint_vindicates(homoousios_christology__pro_nicene_reading, homoousios_doctrine).
narrative_ontology:constraint_vindicates(homoousios_christology__pro_nicene_reading, conciliar_definition_supremacy).
narrative_ontology:constraint_vindicates(homoousios_christology__pro_nicene_reading, apostolic_regula_fidei).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convenes councils, fixes the confessional formula, and administers communion boundaries. Receives the sees, jurisdiction, and definitional authority vacated by deposed dissenters. Its standing within the order is constituted by the confession it administers — a bishop who abandons the homoousios formula forfeits both his seat and his place in the succession. Its instruments are the anathema clauses written into the creed text, deposition, and referral of dissenters to imperial banishment.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, nicene_episcopal_hierarchy, agenda_setter,
    institutional, generational, identity_locked, continental).

% Extends legal recognition, funding, and police power to the settlement; issues edicts of uniformity and carries out banishments. Collects religious legitimation, a unified public cultus for administrative cohesion, and a compliant episcopal partner. Its commitment is instrumental and reversible: Constantius II redirected enforcement against the pro-Nicene bishops when a different alignment paid better, and Julian withdrew support entirely.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, imperial_administration, beneficiary,
    institutional, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_christology__pro_nicene_reading, imperial_administration, agenda_setter).

% Hold or held sees under the dominant mid-century imperial formula. After 381 they face deposition, expulsion from their churches, and exile. Their exit is constrained: ordination, livelihood, and communal standing are bound to the communion that now condemns them; some migrate beyond the frontier to Gothic congregations, at the cost of everything they held inside the empire.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, homoian_clergy, payer,
    organized, biographical, constrained, continental).

% Propound the homoiousios compromise — the Son of like but not identical substance — seeking a formula that avoids both creature-subordination and the collapse of Father and Son into one person that they fear the identical-substance wording implies. Condemned at Constantinople (381) alongside the Anomoeans, they lose the middle ground they occupied and are absorbed, deposed, or dispersed.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, semi_arian_clergy, payer,
    organized, biographical, constrained, continental).

% Urban and rural communities attached to deposed clergy or to condemned formulations. Communion is their social existence — festivals, burial, marriage, and alms run through the parish structure. Losing communion means social death; remaining means absorbing whatever formula the installed hierarchy imposes. The riots in Alexandria and Constantinople register the pressure this seat sits under.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, dissenting_congregations, payer,
    powerless, biographical, trapped, regional).

% Ulfilas's mission carried the Homoian formula beyond the Danube; Gothic churches confess a form of the condemned reading outside imperial jurisdiction. They hold no seat in the councils that fix the formula and bear no exposure to its enforcement — the frontier is their arbitrage.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, gothic_arian_churches, excluded,
    organized, generational, arbitrage, continental).

% Reconstruct the fourth-century controversies from conciliar acta, imperial correspondence, and exile literature; distinguish the doctrinal kernel from the enforcement arrangement built around it; take no side in communion.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, modern_historical_theologians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(homoousios_christology__pro_nicene_reading, nicene_episcopal_hierarchy).
narrative_ontology:fixing_cost_class(homoousios_christology__pro_nicene_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single shared confession defining the object of Christian worship and the boundary of catholic communion: one baptismal formula, one liturgical standard, one settled answer to whether the Son is Creator or creature, adopted once centrally instead of negotiated congregation by congregation.
% TRANSFER_FUNCTION: Moves ecclesiastical office, churches, and jurisdiction from deposed dissenting clergy to the enforcing hierarchy; moves coercive sanction (anathema, deposition, banishment) onto dissenting clergy and congregations; moves religious legitimation and administrative cohesion upward to the imperial administration.
% ABSENT_VOICES: Homoian and semi-Arian bishops were progressively removed from the councils that fixed the formula — Constantinople (381) summoned only bishops aligned with the settlement. Gothic churches beyond the frontier had no seat at all. Lay majorities in cities whose sympathies ran otherwise were represented only by whichever clergy the palace installed.
% DISAPPEARANCE_RATIONALE: If the enforced settlement vanished overnight, the imperial church would fragment back into competing communions organized around rival formulas; imperial religious policy would lose its principal instrument of cohesion; episcopal authority would rearrange around regional confessions; and the offices and churches currently held by the enforcing order would be contested anew.
% FOUNDING_PROBLEM: A church spanning the Mediterranean fractured over the Son's ontological status: one party taught the Son was the first and highest creature, another held that worship of Christ as God required his full deity. Baptismal formulas, hymns, and communion boundaries diverged; popular agitation spread the dispute through streets and barracks; the emperor needed religious coherence for administrative unity. The arrangement was built to settle the Son's status by conciliar definition and to enforce the settlement across one communion.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration from outside the benefiting parties: the exile literature of the losers (Eunomius's Apology; Philostorgius's history preserved in Photius), pagan observers (Ammianus Marcellinus on the violence of the clerical disputes), and modern critical historiography attest the enforcement motive and the costs borne by dissenters. The claim that the doctrinal question itself required the enforced settlement is attested almost exclusively by the winning hierarchy's own transmission chain — that asymmetry is itself signal.
narrative_ontology:disappearance_verdict(homoousios_christology__pro_nicene_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_christology__pro_nicene_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_christology__pro_nicene_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(homoousios_christology__pro_nicene_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_christology__pro_nicene_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_christology__pro_nicene_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(homoousios_christology__pro_nicene_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(homoousios_christology__pro_nicene_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high at interval end (0.78) because enforcement is decoupled from consent: after 381 the settlement is imposed by edict and police action on communities that did not assent, and the offices, churches, and jurisdiction of deposed dissenters transfer to the enforcing hierarchy. Suppression is higher still (0.85) because persistence depends on actively excluding rival formulations — anathema clauses written into the creed text itself, deposition, and imperial banishment — not on participant preference. Theater is moderate (0.38): the confessional function is real (baptism, liturgy, and communion boundaries run through the formula), but once the doctrinal question is settled a growing share of activity is ceremonial conformity and uniformity decree rather than live adjudication. Accessibility collapse is 0.66: rival formulations collapse almost completely inside imperial communion but persist beyond the frontier and in underground communities, so alternatives narrow sharply without vanishing. Resistance is 0.70: four decades of organized counter-councils, urban riots, and exile-endurance literature. The temporal series is deliberately non-monotonic: enforcement capacity tracks imperial patronage, not participant conviction — it collapses under Constantius II and Valens, then ratchets to its maximum under Theodosius. The oscillation is driven externally by who holds the palace, not by intermittent reinforcement internal to the arrangement. All three tracked metrics share one six-point grid so no metric is sampled against another metric's end-state.
 *
 * PERSPECTIVAL GAP:
 *   The hierarchy's seat and the deposed clergy's seat compute different types from the same text. From the episcopal seat the arrangement is the defense of revealed truth and the necessary price of one communion — the costs it observes are the price dissenters impose on unity. From the Homoian seat the same structure is confiscatory: it took sees, pulpits, and the social existence of communion and transferred them to the enforcing order. The imperial seat experiences a third thing again: an instrument of administrative cohesion whose doctrinal content is negotiable whenever a different alignment pays better. The engine computes this divergence from power, exit, and role data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The hierarchy sits near the beneficiary end: it administers the arrangement and collects the vacated offices. Its identity_locked exit binds it to the structure it runs, but identity lock modulates targets, not administrators — its directionality stays low because the beneficiary/administrator declarations dominate. The imperial administration also sits near the beneficiary end with mobility damping: it collects legitimation and unity and can redirect patronage (and did, repeatedly), which keeps it from fusing with the arrangement. Homoian and semi-Arian clergy sit near the target end: they pay offices and communion under constrained exit. Dissenting congregations sit nearest the full-target end: trapped exit (communion is social existence) with no organized power. Gothic Arian churches sit near the beneficiary/outside end: arbitrage-grade exit beyond enforcement reach means the arrangement barely prices them. Suppression is authored as a raw structural property and is not scaled by power or scope; effective extraction is computed by the engine from these directionalities and the continental scope, which amplifies verification difficulty.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification resists two mislabels. Reading the arrangement as pure extraction erases the genuine coordination function: a shared confession really did solve a collective problem — one baptismal formula, one communion boundary, one settled object of worship — that no participant could solve alone, and the doctrine's content is held as true by the tradition independently of the enforcement economics. Reading it as pure coordination erases the asymmetric extraction: the same structure transferred offices and jurisdiction from dissenters to the enforcing hierarchy under imperial sanction. The mandatrophy question turns on the founding problem: the fourth-century enforcement problem (imperial-era fragmentation requiring coerced uniformity) is dead — no imperial enforcement machinery survives — while the doctrinal kernel persists in recitation. The founding_problem_status x disappearance_verdict pairing (contested x world_rearranges) routes the zombie/capture flag: the arrangement persists past its enforcement function on institutional inertia and continued truth-guarding claims. The engine evaluates the arrangement, not the theology; the doctrine's truth is a question for the reading's own lights, and ε here is authored for the standing enforced settlement as this reading assesses it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading (pro_nicene_reading) of the homoousios_christology kernel; what structurally changes if a sibling reading (arian_reading, semi_arian_reading) is instantiated instead, and where exactly is the disagreement located?',
    'Cross-reading comparison of the three family files: the disagreement is located in the ontological-status predicate of the Son relative to the Father (identical substance vs created-and-subordinate vs similar-not-identical substance), which flips the entire beneficiary/victim structure — under Arian ascendancy the extraction ran against pro-Nicene bishops.',
    'Adopting a sibling reading replaces this constraint''s victim set with the pro-Nicene hierarchy, reverses the direction of extraction, and changes the claimed type surface; per-seat classifications computed here are valid only for this reading''s arrangement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this file is one reading of a contested kernel; siblings are separate constraints, not hedges inside this one.').

omega_variable(
    enforcement_vs_truth_guarding,
    'Is the arrangement''s persistence driven by guarding revealed truth (as the reading itself asserts) or by institutional and imperial rent preservation (as the enforcement economics suggest)?',
    'Compare enforcement behavior when the doctrine is uncontested versus contested: if enforcement intensity tracks doctrinal threat, truth-guarding dominates; if it tracks institutional interest (vacated sees, patronage flows, palace alignment), rent preservation dominates. The post-imperial survival of the doctrine without the enforcement machinery is the natural experiment.',
    'If rent-driven, the arrangement shifts toward pure extraction and the coordination story is cover; if truth-guarding with incidental rents, the tangled-rope reading holds and part of the measured extraction is the price of the boundary maintenance itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_vs_truth_guarding, conceptual, 'Whether the enforcement apparatus serves the doctrine or the doctrine serves the apparatus.').

omega_variable(
    epsilon_reading_indexing,
    'The ε authored here is indexed to the pro-Nicene seat assessing the enforced settlement it defends; how much does the reading''s own justification (''necessary defense of truth'') discount the extraction a dissenting seat would measure over the same referent?',
    'Compare the ε each sibling file authors over its own arrangement and over the shared referent; the spread between the pro-Nicene seat''s authored ε and the dissenting seats'' experienced costs locates the justification discount.',
    'Determines whether per-seat classifications diverge as the hybrid structure predicts, or converge — convergence would indicate the enforcement economics, not the reading, drive classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_reading_indexing, conceptual, 'Reading-indexed ε over a fixed referent: how the defending seat''s assessment discounts the costs it imposes.').

omega_variable(
    imperial_alignment_direction,
    'Was imperial-church alignment a benefit the church collected or a capture the empire imposed — who used whom?',
    'Trace the initiative of enforcement acts: imperial letters initiating uniformity campaigns versus conciliar petitions requesting them; examine whether episcopal policy bent toward palatine preferences (it did, repeatedly, under Constantius II) and whether the doctrine survived the empire''s fall intact (it did).',
    'If the empire captured the church, the imperial administration''s directionality sits nearer the target end and the hierarchy''s nearer the beneficiary end; if the church captured the empire, the polarity reverses and the hierarchy''s enforcement is partly subsidy-seeking rather than rent-collecting.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imperial_alignment_direction, empirical, 'Direction of the imperial-church alignment: collection or capture.').

omega_variable(
    anathema_person_vs_proposition,
    'Does the anathema bind propositions or persons — is the exclusion mechanism aimed at condemned formulations or at the people who hold them?',
    'Legal analysis of deposition criteria and banishment warrants against the creed text: the text anathematizes propositional denials (''those who say...''), while enforcement extended to persons (deposition, exile, book destruction). Determine whether person-directed sanction was a derivable application or an extension beyond the text.',
    'If the anathema binds propositions only, part of the measured suppression belongs to the imperial enforcement layer rather than the confessional layer, and the constraint''s suppression decomposes; if persons were always the operative target, the suppression measure stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anathema_person_vs_proposition, conceptual, 'Scope ambiguity in the exclusion mechanism: propositions versus persons.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_christology__pro_nicene_reading, 325, 381).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_tr_t325, homoousios_christology__pro_nicene_reading, theater_ratio, 325, 0.18).
narrative_ontology:measurement_basis(homo_tr_t325, observed).
narrative_ontology:measurement(homo_tr_t337, homoousios_christology__pro_nicene_reading, theater_ratio, 337, 0.24).
narrative_ontology:measurement_basis(homo_tr_t337, observed).
narrative_ontology:measurement(homo_tr_t350, homoousios_christology__pro_nicene_reading, theater_ratio, 350, 0.14).
narrative_ontology:measurement_basis(homo_tr_t350, observed).
narrative_ontology:measurement(homo_tr_t361, homoousios_christology__pro_nicene_reading, theater_ratio, 361, 0.12).
narrative_ontology:measurement_basis(homo_tr_t361, observed).
narrative_ontology:measurement(homo_tr_t373, homoousios_christology__pro_nicene_reading, theater_ratio, 373, 0.2).
narrative_ontology:measurement_basis(homo_tr_t373, observed).
narrative_ontology:measurement(homo_tr_t381, homoousios_christology__pro_nicene_reading, theater_ratio, 381, 0.38).
narrative_ontology:measurement_basis(homo_tr_t381, observed).

% Extraction over time
narrative_ontology:measurement(homo_be_t325, homoousios_christology__pro_nicene_reading, base_extractiveness, 325, 0.6).
narrative_ontology:measurement_basis(homo_be_t325, observed).
narrative_ontology:measurement(homo_be_t337, homoousios_christology__pro_nicene_reading, base_extractiveness, 337, 0.52).
narrative_ontology:measurement_basis(homo_be_t337, observed).
narrative_ontology:measurement(homo_be_t350, homoousios_christology__pro_nicene_reading, base_extractiveness, 350, 0.38).
narrative_ontology:measurement_basis(homo_be_t350, observed).
narrative_ontology:measurement(homo_be_t361, homoousios_christology__pro_nicene_reading, base_extractiveness, 361, 0.32).
narrative_ontology:measurement_basis(homo_be_t361, observed).
narrative_ontology:measurement(homo_be_t373, homoousios_christology__pro_nicene_reading, base_extractiveness, 373, 0.44).
narrative_ontology:measurement_basis(homo_be_t373, observed).
narrative_ontology:measurement(homo_be_t381, homoousios_christology__pro_nicene_reading, base_extractiveness, 381, 0.78).
narrative_ontology:measurement_basis(homo_be_t381, observed).

% Suppression requirement over time
narrative_ontology:measurement(homo_su_t325, homoousios_christology__pro_nicene_reading, suppression_requirement, 325, 0.58).
narrative_ontology:measurement_basis(homo_su_t325, observed).
narrative_ontology:measurement(homo_su_t337, homoousios_christology__pro_nicene_reading, suppression_requirement, 337, 0.5).
narrative_ontology:measurement_basis(homo_su_t337, observed).
narrative_ontology:measurement(homo_su_t350, homoousios_christology__pro_nicene_reading, suppression_requirement, 350, 0.28).
narrative_ontology:measurement_basis(homo_su_t350, observed).
narrative_ontology:measurement(homo_su_t361, homoousios_christology__pro_nicene_reading, suppression_requirement, 361, 0.2).
narrative_ontology:measurement_basis(homo_su_t361, observed).
narrative_ontology:measurement(homo_su_t373, homoousios_christology__pro_nicene_reading, suppression_requirement, 373, 0.35).
narrative_ontology:measurement_basis(homo_su_t373, observed).
narrative_ontology:measurement(homo_su_t381, homoousios_christology__pro_nicene_reading, suppression_requirement, 381, 0.85).
narrative_ontology:measurement_basis(homo_su_t381, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_christology__pro_nicene_reading, identity_coordination).
narrative_ontology:affects_constraint(homoousios_christology__pro_nicene_reading, arian_reading).
narrative_ontology:affects_constraint(homoousios_christology__pro_nicene_reading, semi_arian_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Nicene doctrine' decomposes into three structurally distinct constraint stories — one per reading of the homoousios kernel. Each reading instantiates a different arrangement with its own ε, beneficiary set, and enforcement economics: under arian_reading ascendancy (Constantius II era) the extraction ran in the opposite direction, deposing pro-Nicene bishops; under semi_arian_reading the compromise formula redistributed rather than resolved the costs. This file authors only the pro-Nicene instantiation; the family is linked so contamination propagation and reversal analysis can traverse the edges. Upstream/downstream: the pro-Nicene settlement is the downstream victor whose enforcement reshaped the operating environment of both siblings without logically resolving the exegetical dispute they live on.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
