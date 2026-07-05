% ============================================================================
% CONSTRAINT STORY: homoousios_christology__pro_nicene_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: homoousios_christology__pro_nicene_reading
 *   human_readable: Pro-Nicene Homoousios Doctrine as Imperial-Ecclesiastical Orthodoxy
 *   domain: historical_theology/ecclesiastical_politics
 *
 * SUMMARY:
 *   This story instantiates the pro-Nicene reading of the homoousios kernel:
 *   the claim that Christ is of identical divine substance with the Father,
 *   ratified at Nicaea (325) and Constantinople (381), and enforced through
 *   conciliar anathema, episcopal deposition, and eventually imperial law
 *   under Theodosius I. The reading genuinely solves a coordination problem —
 *   a single teachable Christological formula for an empire-wide communion —
 *   but the coordination function is fused with an extraction structure: sees
 *   and bishops who hold the winning formula gain lasting institutional and
 *   doctrinal capital, while clergy, converted peoples, and laity holding
 *   rival Christologies (Arian, semi-Arian) are anathematized, deposed,
 *   exiled, or have their prior formation delegitimized. The suppression
 *   trajectory shows a temporary relaxation around imperial succession (t=30,
 *   when pro-Arian emperors briefly held power) before hardening decisively
 *   under Theodosius. This is ONE of three sibling readings of the same
 *   kernel (arian_reading, semi_arian_reading are separate constraint files);
 *   this file does not describe or average across them.
 *
 * KEY AGENTS:
 *   - nicene_episcopal_hierarchy: agenda_setter (institutional/arbitrage) — drafts and enforces the winning formula
 *   - constantinian_imperial_authority: agenda_setter/beneficiary (institutional/arbitrage) — provides the coercive machinery and shifting political backing
 *   - athanasian_alexandrian_see: beneficiary (powerful/constrained) — durable theological and institutional winner
 *   - arian_clergy: payer (moderate/trapped) — anathematized and deposed
 *   - gothic_and_vandal_christian_communities: payer (powerless/trapped) — inherit disfavored Christology through no choice
 *   - subordinationist_laity: payer (powerless/trapped) — reclassified without voice
 *   - later_church_historians: observer (analytical/analytical) — sees the coordination and extraction operating simultaneously
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_christology__pro_nicene_reading, 0.68).
domain_priors:suppression_score(homoousios_christology__pro_nicene_reading, 0.79).
domain_priors:theater_ratio(homoousios_christology__pro_nicene_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_christology__pro_nicene_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_christology__pro_nicene_reading, "Pro-Nicene Homoousios Doctrine as Imperial-Ecclesiastical Orthodoxy").
narrative_ontology:topic_domain(homoousios_christology__pro_nicene_reading, "historical_theology/ecclesiastical_politics").

domain_priors:requires_active_enforcement(homoousios_christology__pro_nicene_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_christology__pro_nicene_reading, '3f7f685c-ba33-41c4-b133-664865d534af').
narrative_ontology:cs_kernel_codification('3f7f685c-ba33-41c4-b133-664865d534af', formalized).
narrative_ontology:cs_authority_grounding('3f7f685c-ba33-41c4-b133-664865d534af', lineage).
narrative_ontology:cs_interpretation_layer_present('3f7f685c-ba33-41c4-b133-664865d534af').
narrative_ontology:cs_reading_relation('3f7f685c-ba33-41c4-b133-664865d534af', homoousios_christology__arian_reading, forecloses).
narrative_ontology:cs_reading_relation('3f7f685c-ba33-41c4-b133-664865d534af', homoousios_christology__semi_arian_reading, influences).
narrative_ontology:cs_axiom('3f7f685c-ba33-41c4-b133-664865d534af', foundational, father_and_son_share_numerically_identical_substance).
narrative_ontology:cs_axiom_status(father_and_son_share_numerically_identical_substance, holdable).
narrative_ontology:cs_axiom_grounding('3f7f685c-ba33-41c4-b133-664865d534af', father_and_son_share_numerically_identical_substance, deontological).
narrative_ontology:cs_axiom('3f7f685c-ba33-41c4-b133-664865d534af', secondary, conciliar_anathema_binds_universal_communion).
narrative_ontology:cs_axiom_status(conciliar_anathema_binds_universal_communion, holdable).
narrative_ontology:cs_axiom_grounding('3f7f685c-ba33-41c4-b133-664865d534af', conciliar_anathema_binds_universal_communion, conventional).
narrative_ontology:cs_reference_frame('3f7f685c-ba33-41c4-b133-664865d534af', conciliar_creedal_orthodoxy).
narrative_ontology:cs_drift_state('3f7f685c-ba33-41c4-b133-664865d534af', post_theodosian_settlement, gap(authority_erosion, minor, false)).
narrative_ontology:cs_created_at('3f7f685c-ba33-41c4-b133-664865d534af', '').
narrative_ontology:cs_kernel_id(homoousios_christology__pro_nicene_reading, homoousios_christology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_christology__pro_nicene_reading, nicene_episcopal_hierarchy).
narrative_ontology:constraint_beneficiary(homoousios_christology__pro_nicene_reading, constantinian_imperial_authority).
narrative_ontology:constraint_beneficiary(homoousios_christology__pro_nicene_reading, athanasian_alexandrian_see).
narrative_ontology:constraint_victim(homoousios_christology__pro_nicene_reading, arian_clergy).
narrative_ontology:constraint_victim(homoousios_christology__pro_nicene_reading, gothic_and_vandal_christian_communities).
narrative_ontology:constraint_victim(homoousios_christology__pro_nicene_reading, subordinationist_laity).
narrative_ontology:constraint_vindicates(homoousios_christology__pro_nicene_reading, trinitarian_unity_doctrine).
narrative_ontology:constraint_vindicates(homoousios_christology__pro_nicene_reading, single_church_universal_communion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convenes and controls the councils (Nicaea 325, later ratified at Constantinople 381) that define homoousios as the required formula, drafts the anathemas attached to the creed, and holds sees and synodical machinery that can depose or excommunicate clergy who will not subscribe. Sets the boundary of communion and is positioned to occupy vacated sees when rivals are deposed.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, nicene_episcopal_hierarchy, agenda_setter,
    institutional, civilizational, arbitrage, continental).

% Convenes Nicaea as a state council, funds bishops' travel, and later uses imperial law (and exile) to enforce doctrinal uniformity as a tool of empire-wide unity. Benefits from a single settled formula that lets the state treat theological dissent as sedition and administer the church as an arm of imperial cohesion; the emperor's own theological preference shifts repeatedly (Constantine, then pro-Arian successors, then Theodosius), so this seat's enforcement direction is not stable across the story's interval.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, constantinian_imperial_authority, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_christology__pro_nicene_reading, constantinian_imperial_authority, beneficiary).

% Athanasius and the Alexandrian see become the doctrine's most durable champions, gaining lasting theological authority and eventual canonization of their position as orthodoxy, despite repeated depositions and exiles when imperial favor shifted against them. Their long-run institutional and doctrinal capital is built on homoousios prevailing.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, athanasian_alexandrian_see, beneficiary,
    powerful, biographical, constrained, regional).

% Bishops and presbyters who hold that the Son is created and subordinate are formally anathematized, deposed from their sees, and in later imperial law subject to exile or loss of clerical status. Their theological position, previously a live and widely-held view among churchmen (including many bishops at and after Nicaea), is redefined as heresy; recanting means subscribing to a formula they do not hold, refusing means loss of office, community, and legal standing under Christian empire.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, arian_clergy, payer,
    moderate, biographical, trapped, continental).

% Converted to Christianity under Arian missionary work (notably Ulfilas), these communities inherit a form of Christianity that pro-Nicene authority defines as heretical from the outset. This becomes a durable marker of political and religious exclusion once these groups migrate into and rule portions of the former Western Empire, complicating their relations with Nicene Roman populations for generations.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, gothic_and_vandal_christian_communities, payer,
    powerless, generational, trapped, continental).

% Ordinary believers whose received catechesis or local bishop's teaching held a subordinationist Christology find their prior formation reclassified as heretical, their sacraments and communion status thrown into question, and their local clergy replaced under episcopal or imperial pressure. They have essentially no voice in the councils that redefine their standing.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, subordinationist_laity, payer,
    powerless, biographical, trapped, regional).

% Bishops holding the homoiousios compromise (similar, not identical, substance) attempt a middle path between full Nicene and Arian positions in the decades after 325, but pro-Nicene councils ultimately treat the compromise as inadequate and fold or exclude it; their intermediate position gets little durable institutional recognition once Constantinople 381 settles the question in the pro-Nicene direction.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, semi_arian_bishops, excluded,
    moderate, biographical, constrained, continental).

% Assess the councils' proceedings, the political pressures on bishops (exile, imperial favor, mob violence in some sees), and the theological substance of the dispute from outside the fourth-century power struggle, generally documenting both a genuine theological question and heavy-handed enforcement machinery operating simultaneously.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, later_church_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(homoousios_christology__pro_nicene_reading, nicene_episcopal_hierarchy).
narrative_ontology:fixing_cost_class(homoousios_christology__pro_nicene_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the empire and church with a single, teachable formula for the divine status of Christ, allowing unified liturgy, shared creed, and a basis for treating the church as one communion across a vast and linguistically diverse empire — a genuine coordination problem given rival Christologies were spreading independently across regions.
% TRANSFER_FUNCTION: Moves ecclesiastical office, communion status, legal standing under Christian imperial law, and doctrinal legitimacy away from clergy and communities holding subordinationist or compromise Christologies, and toward sees and bishops who hold and enforce the homoousios formula.
% ABSENT_VOICES: Arian and semi-Arian bishops present at early stages of the dispute are progressively excluded from the councils that finally settle the question against them; Gothic and Vandal converts, evangelized under an Arian mission, have no seat at any council that later brands their inherited faith heretical; ordinary laity across the empire have no voice in a formula imposed from above by bishops and emperors.
% DISAPPEARANCE_RATIONALE: From the pro-Nicene seat, the world without homoousios rearranges catastrophically: the church loses a unifying account of Christ's divinity and fragments doctrinally in ways later councils insist would have undermined the coherence of Trinitarian worship and salvation theology. From the Arian and semi-Arian seats, the world before the anathemas was not obviously broken — multiple Christologies coexisted in the same communion for decades before Nicaea; the formula's disappearance would return the church to a plural, contested, but functioning state rather than dissolve it. This is precisely the kind of disagreement the framework should surface as contested rather than resolve by fiat.
% FOUNDING_PROBLEM: The church faced a live, unresolved theological dispute (Arius's teaching in Alexandria, c. 318-320) about the relationship between the Father and the Son, and lacked any single body with authority to settle disputed Christology empire-wide; Constantine additionally needed religious unity as a tool of imperial cohesion following the Edict of Milan.
% FOUNDING_PROBLEM_CORROBORATION: Pro-Nicene sources (Athanasius, the conciliar acts themselves, later imperial law under Theodosius) attest the problem was a genuine and dangerous doctrinal division requiring settlement. Independent corroboration from outside the pro-Nicene beneficiary set is harder to find in surviving sources because much Arian literature was destroyed or survives only in pro-Nicene quotation; however, modern historians of late antiquity (working from imperial correspondence, conciliar minutes, and non-theological administrative records) corroborate that a genuine, widely-held theological disagreement existed among churchmen prior to Nicaea, while also documenting that the settlement mechanism itself involved imperial coercion, exile of dissenting bishops, and later legal penalties — evidence that the founding theological problem was real but the resolution mechanism exceeded what pure doctrinal coordination required.
narrative_ontology:disappearance_verdict(homoousios_christology__pro_nicene_reading, contested).
narrative_ontology:founding_problem_status(homoousios_christology__pro_nicene_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_christology__pro_nicene_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(homoousios_christology__pro_nicene_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_christology__pro_nicene_reading, 0.68, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises from 0.35 to 0.68 across the interval because the coordination function (a shared Christological formula) is present from the outset but the enforcement and exclusion apparatus intensifies substantially between Nicaea (325) and the Theodosian settlement (381) and its aftermath, as anathema hardens from conciliar language into imperial law with real legal and physical consequences for dissenters. Theater ratio is moderate (0.42 at end) — genuine theological argument occurs throughout, but an increasing share of conciliar and legal activity functions to perform unity and justify existing depositions rather than resolve open questions. Suppression dips at t=30 reflecting the brief ascendancy of pro-Arian imperial factions before resuming its climb, illustrating that the enforcement direction of this reading is not monotonic — it depends on which faction controls imperial power at a given moment, even though the pro-Nicene formula eventually prevails durably.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (episcopal hierarchy, imperial authority), the arrangement is a necessary and successful resolution of a genuine doctrinal crisis threatening ecclesiastical and political unity. From the payer seats (Arian clergy, converted peoples, ordinary subordinationist believers), the identical structure operates as an enforced reclassification of previously legitimate belief into heresy, backed by exile and loss of legal standing. The engine's per-seat computation should register this divergence directly from the declared power/exit/scope data rather than from any narrative adjudication of which side is theologically correct.
 *
 * DIRECTIONALITY LOGIC:
 *   The episcopal hierarchy and imperial authority are structural beneficiaries: they set the formula, control the anathema mechanism, and gain durable institutional legitimacy and administrative unity from its acceptance (d near the beneficiary end). Arian clergy, Gothic/Vandal converts, and subordinationist laity are structural targets: they hold or inherit a Christology the councils redefine as heretical, and bear loss of office, communion, legal standing, or social legitimacy as a direct consequence of the same formula that benefits the hierarchy (d near the target end). Semi-Arian bishops occupy an intermediate, ultimately excluded position — their compromise reading is treated as a live but discarded alternative rather than either full alignment or full opposition.
 *
 * MANDATROPHY ANALYSIS:
 *   The coordination function (a shared Christological formula enabling unified worship and administration across a vast empire) does not disappear even where enforcement is heavy-handed — this prevents the classification from collapsing the pro-Nicene reading into pure extraction (a snare) despite substantial suppression. Conversely, the presence of durable, asymmetric institutional beneficiaries (sees, imperial administration) and identifiable victims (deposed clergy, disenfranchised converts, silenced laity) with active enforcement (anathema, imperial law, exile) prevents the classification from resting at a pure rope. Tangled rope captures both facts simultaneously: real coordination value coexists with asymmetric extraction running through the same structure, which is exactly the ambiguity a genealogical or purely theological reading would flatten in either direction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_theological_question_vs_power_struggle,
    'Was the homoousios/homoiousios distinction a substantive theological question with real doctrinal stakes for salvation theology, or was it primarily a proxy battlefield for factional and imperial power struggles that used theological vocabulary as cover?',
    'Close textual analysis of the theological arguments made by all sides independent of the political outcomes; comparison of doctrinal content in regions where the dispute had less immediate political stakes (e.g., theological writings preserved outside imperial court circles) against the intensity of the enforcement apparatus applied in politically central sees.',
    'If substantively theological, the coordination function is stronger and the tangled_rope classification is well-supported by a genuine underlying problem. If primarily a power struggle wearing theological vocabulary, the coordination story is closer to pure cover and the classification should shift toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_theological_question_vs_power_struggle, conceptual, 'Whether the doctrinal dispute was substantively theological or primarily a proxy for ecclesiastical-imperial power consolidation.').

omega_variable(
    kernel_reading_selection_mechanism,
    'Given that the same kernel (the nature of Christ''s relationship to the Father) supported at least three internally coherent readings held by serious churchmen simultaneously, what specifically determined which reading became ''orthodox'' — theological argument, imperial political alignment, numerical/geographic weight of bishops, or some combination — and how much weight should each factor carry in assessing this reading''s legitimacy versus its siblings?',
    'Detailed prosopographical and political history of the councils (attendance patterns, exile timing relative to imperial succession, correlation between doctrinal position and geographic/political faction) cross-referenced against the surviving theological argumentation on its own merits.',
    'If theological argument dominated, this reading''s institutional authority is closer to warranted; if imperial political alignment dominated, the reading''s persistence is closer to contingent power consolidation that happened to settle on this formula.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_mechanism, conceptual, 'What determined the kernel resolution in favor of this reading over its siblings.').

omega_variable(
    suppression_dip_interpretation,
    'Does the mid-interval dip in suppression_requirement (around t=30, corresponding to pro-Arian imperial ascendancy) represent a genuine relaxation of enforcement machinery, or a temporary reversal of WHICH reading held the enforcement machinery, with total systemic suppression roughly constant?',
    'Comparative measurement of exile and deposition rates against Arian clergy during pro-Nicene imperial periods versus Nicene clergy during the pro-Arian interregnum (e.g., under Constantius II and Valens) to determine whether enforcement intensity or merely enforcement direction changed.',
    'If enforcement intensity was roughly constant and merely redirected, the suppression metric understates the total coercive apparatus operating across the kernel as a whole (both readings enforcing against each other); if intensity genuinely dropped, the dip reflects real institutional weakness during the interregnum.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_dip_interpretation, empirical, 'Whether the interregnum suppression dip reflects reduced total coercion or merely reversed direction of coercion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_christology__pro_nicene_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_tr_t0, homoousios_christology__pro_nicene_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(homo_tr_t10, homoousios_christology__pro_nicene_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(homo_tr_t20, homoousios_christology__pro_nicene_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(homo_tr_t30, homoousios_christology__pro_nicene_reading, theater_ratio, 30, 0.34).
narrative_ontology:measurement(homo_tr_t40, homoousios_christology__pro_nicene_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(homo_tr_t50, homoousios_christology__pro_nicene_reading, theater_ratio, 50, 0.41).
narrative_ontology:measurement(homo_tr_t60, homoousios_christology__pro_nicene_reading, theater_ratio, 60, 0.42).

% Extraction over time
narrative_ontology:measurement(homo_be_t0, homoousios_christology__pro_nicene_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(homo_be_t10, homoousios_christology__pro_nicene_reading, base_extractiveness, 10, 0.44).
narrative_ontology:measurement(homo_be_t20, homoousios_christology__pro_nicene_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(homo_be_t30, homoousios_christology__pro_nicene_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(homo_be_t40, homoousios_christology__pro_nicene_reading, base_extractiveness, 40, 0.61).
narrative_ontology:measurement(homo_be_t50, homoousios_christology__pro_nicene_reading, base_extractiveness, 50, 0.66).
narrative_ontology:measurement(homo_be_t60, homoousios_christology__pro_nicene_reading, base_extractiveness, 60, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(homo_su_t0, homoousios_christology__pro_nicene_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(homo_su_t10, homoousios_christology__pro_nicene_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(homo_su_t20, homoousios_christology__pro_nicene_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(homo_su_t30, homoousios_christology__pro_nicene_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(homo_su_t40, homoousios_christology__pro_nicene_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(homo_su_t50, homoousios_christology__pro_nicene_reading, suppression_requirement, 50, 0.76).
narrative_ontology:measurement(homo_su_t60, homoousios_christology__pro_nicene_reading, suppression_requirement, 60, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_christology__pro_nicene_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(homoousios_christology__pro_nicene_reading, 0.1).
narrative_ontology:affects_constraint(homoousios_christology__pro_nicene_reading, arian_reading).
narrative_ontology:affects_constraint(homoousios_christology__pro_nicene_reading, semi_arian_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the homoousios_christology kernel. arian_reading (subordinationist Christology) and semi_arian_reading (homoiousios compromise) are separate constraint files, each with independently authored ε, beneficiaries, victims, and stakeholder sets reflecting their own structural position relative to fourth-century ecclesiastical-imperial power. The pro-Nicene reading here is the eventual institutional winner and therefore shows the most developed enforcement apparatus of the three; the sibling files should show different, non-averaged extraction profiles reflecting their own (ultimately unsuccessful, in the arian case; absorbed, in the semi-Arian case) positions within the same contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
