% ============================================================================
% CONSTRAINT STORY: homoousios_christology__semi_arian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homoousios_christology__semi_arian_reading, []).

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
 *   constraint_id: homoousios_christology__semi_arian_reading
 *   human_readable: Homoiousios Formula Regime (Semi-Arian Reading)
 *   domain: religious/political
 *
 * SUMMARY:
 *   Between roughly 351 and 381 the eastern church's public confession of
 *   Christ's relation to the Father was governed by a succession of
 *   'likeness' formulas, culminating in homoiousios ('of similar substance')
 *   and its looser cousin homoios ('like in all things'). This story
 *   instantiates the SEMI-ARIAN READING of the homoousios-christology kernel:
 *   the constraint as its own holders understood and administered it, a
 *   mediating confession meant to keep communion intact where Nicaea's 'same
 *   substance' could not command the East. The colloquial label 'the Arian
 *   controversy' covers three structurally distinct arrangements: the
 *   pro-Nicene, semi-Arian, and Arian readings each fix a different
 *   confession, a different beneficiary/victim structure, and a different
 *   epsilon, and are modeled as a linked constraint family rather than one
 *   story with a dial. The claim/metric relationship is deliberately
 *   unreconciled: the arrangement is CLAIMED as tangled_rope (genuine
 *   schism-avoidance coordination entangled with real extraction) while the
 *   metrics are authored independently from the historical record, including
 *   a suppression series that rises, collapses, rebuilds, and decays with
 *   each change of reign.
 *
 * KEY AGENTS:
 *   - homoiousian_episcopal_party: draughtsmen and initial administrators of the formula (organized/constrained) — authored the middle confession, later deposed by the party that captured it
 *   - imperial_court_constantius: enforcement principal (institutional/arbitrage) — summoned councils, ratified texts, removed refusers, switched formulas with the political weather
 *   - homoean_court_bishops: primary collectors (powerful/arbitrage) — rode the formula's flexibility into the major eastern sees
 *   - pro_nicene_exiles: primary bearers of costs (organized/trapped) — deposed and banished for refusing subscription
 *   - western_bishops_at_rimini: same-rank bearers (organized/constrained) — detained and signed under duress, then recanted
 *   - ordinary_congregations: dual-positioned laity (powerless/constrained) — kept communion, absorbed the churn
 *   - anomoean_strict_subordinationists: excluded radicals (moderate/constrained) — outside the negotiated text
 *   - ecclesiastical_historians: retrospective analytical seat (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_christology__semi_arian_reading, 0.46).
domain_priors:suppression_score(homoousios_christology__semi_arian_reading, 0.18).
domain_priors:theater_ratio(homoousios_christology__semi_arian_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_christology__semi_arian_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_christology__semi_arian_reading, "Homoiousios Formula Regime (Semi-Arian Reading)").
narrative_ontology:topic_domain(homoousios_christology__semi_arian_reading, "religious/political").

domain_priors:requires_active_enforcement(homoousios_christology__semi_arian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_christology__semi_arian_reading, '744aedd3-329d-4d13-b356-03af19d38d8c').
narrative_ontology:cs_kernel_codification('744aedd3-329d-4d13-b356-03af19d38d8c', formalized).
narrative_ontology:cs_authority_grounding('744aedd3-329d-4d13-b356-03af19d38d8c', extraction).
narrative_ontology:cs_interpretation_layer_present('744aedd3-329d-4d13-b356-03af19d38d8c').
narrative_ontology:cs_reading_relation('744aedd3-329d-4d13-b356-03af19d38d8c', homoousios_christology__pro_nicene_reading, influences).
narrative_ontology:cs_reading_relation('744aedd3-329d-4d13-b356-03af19d38d8c', homoousios_christology__arian_reading, coexists_with).
narrative_ontology:cs_axiom('744aedd3-329d-4d13-b356-03af19d38d8c', foundational, son_similar_not_identical_in_substance).
narrative_ontology:cs_axiom_status(son_similar_not_identical_in_substance, overridden).
narrative_ontology:cs_axiom_grounding('744aedd3-329d-4d13-b356-03af19d38d8c', son_similar_not_identical_in_substance, theological).
narrative_ontology:cs_axiom('744aedd3-329d-4d13-b356-03af19d38d8c', foundational, creedal_language_must_be_scriptural).
narrative_ontology:cs_axiom_status(creedal_language_must_be_scriptural, overridden).
narrative_ontology:cs_axiom_grounding('744aedd3-329d-4d13-b356-03af19d38d8c', creedal_language_must_be_scriptural, theological).
narrative_ontology:cs_reference_frame('744aedd3-329d-4d13-b356-03af19d38d8c', scriptural_likeness_middle_confession).
narrative_ontology:cs_drift_state('744aedd3-329d-4d13-b356-03af19d38d8c', constantinopolitan_settlement_381, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('744aedd3-329d-4d13-b356-03af19d38d8c', '').
narrative_ontology:cs_kernel_id(homoousios_christology__semi_arian_reading, homoousios_christology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_christology__semi_arian_reading, imperial_court_constantius).
narrative_ontology:constraint_beneficiary(homoousios_christology__semi_arian_reading, homoean_court_bishops).
narrative_ontology:constraint_victim(homoousios_christology__semi_arian_reading, pro_nicene_exiles).
narrative_ontology:constraint_victim(homoousios_christology__semi_arian_reading, homoiousian_episcopal_party).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(homoousios_christology__semi_arian_reading, ordinary_congregations).
narrative_ontology:constraint_victim(homoousios_christology__semi_arian_reading, western_bishops_at_rimini).
narrative_ontology:constraint_victim(homoousios_christology__semi_arian_reading, ordinary_congregations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Eastern bishops centered on Basil of Ancyra who drafted and promoted the 'like in substance' formula as a confession they believed faithful to Scripture and capable of spanning the dispute. They convened councils, wrote the texts, and held imperial favor through the 350s. When the court shifted patronage to the looser 'likeness in all things' party, they lost sees and influence, and several were deposed at Constantinople in 360. Leaving the hierarchy was not a live option for them: their office, vocation, and self-understanding were bound to the communion they were trying to hold together.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, homoiousian_episcopal_party, agenda_setter,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_christology__semi_arian_reading, homoiousian_episcopal_party, payer).

% The emperor and his advisers needed one confession across the empire and preferred formulas broad enough to command majorities without forcing a losing faction to capitulate. They summoned councils, set agendas, ratified results, and removed bishops who refused subscription. Because their interest was administrative unity rather than any particular metaphysics, they could replace one formula with another whenever the political balance shifted, and did so repeatedly between 351 and 361.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, imperial_court_constantius, agenda_setter,
    institutional, biographical, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_christology__semi_arian_reading, imperial_court_constantius, beneficiary).

% Court-aligned bishops (Ursacius, Valens, Acacius, Eudoxius) who worked with the flexibility of 'likeness' language. As the formula's meaning drifted from 'like in substance' toward 'like in all things' and finally toward silence about substance altogether, they advanced from junior partners to holders of the major eastern sees, occupying posts vacated by deposed colleagues. Their position depended on staying close to the court and on keeping the formula's meaning unsettled; when imperial patronage moved, they moved with it.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, homoean_court_bishops, beneficiary,
    powerful, biographical, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_christology__semi_arian_reading, homoean_court_bishops, agenda_setter).

% Bishops and clergy who held that Nicaea's 'same substance' was settled and that any likeness-formula betrayed it: Athanasius, Hosius, Liberius, Eustathius, and their networks. They refused subscription, were deposed and banished, and sustained resistance from exile through correspondence, appeals to the western church, and polemical writing. Signing was available to them at any time and was the one thing they could not do; abandoning their sees and flocks was equally unavailable, so they endured repeated exile across changes of reign.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, pro_nicene_exiles, payer,
    organized, generational, trapped, continental).

% The large western delegation at Rimini (359) that rejected the likeness-formula, was detained through the campaigning season far from their sees, and signed when supplies and permission to depart were withheld. Most returned and repudiated their subscriptions once free. Their treatment showed how the enforcement machinery operated on bishops of equal rank who lacked an imperial patron.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, western_bishops_at_rimini, payer,
    organized, biographical, constrained, continental).

% Laity and parish clergy across the eastern cities who received a succession of revised confessions, watched bishops installed and removed by imperial decree, and absorbed liturgical and pastoral disruption with each turnover. They retained communion, sacraments, and a shared church life throughout, which is the continuity the formula promised, but carried the confusion of changing creeds and the divisions left when clergy were replaced.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, ordinary_congregations, payer,
    powerless, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(homoousios_christology__semi_arian_reading, ordinary_congregations, beneficiary).

% Radical followers of Aetius and Eunomius who taught that the Son is unlike the Father in substance and pressed that position in the same councils and courts. The compromise formulas were drafted to be signable without them; their exclusion from the settlement's drafting was part of what made the middle formula viable, and they answered it by building their own network of sympathizers inside and outside the hierarchy.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, anomoean_strict_subordinationists, excluded,
    moderate, biographical, constrained, continental).

% Fifth-century writers (Socrates Scholasticus, Sozomen, Theodoret, Rufinus) who reconstructed the sequence of councils, formulas, and depositions after the controversy closed, working from documents and participants' accounts on all sides. Their seat is retrospective: they record what each party professed and did without bearing any of the costs.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, ecclesiastical_historians, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(homoousios_christology__semi_arian_reading, homoean_court_bishops).
narrative_ontology:fixing_cost_class(homoousios_christology__semi_arian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Kept the eastern episcopate in a single communion through a doctrinal dispute neither wing could win quickly; gave the imperial government a uniformity instrument broad enough to command subscribing majorities; allowed bishops who rejected both strict subordinationism and Nicene precision to confess one faith together.
% TRANSFER_FUNCTION: Moved episcopal sees, imperial favor, and council majorities toward bishops willing to subscribe to whatever likeness-text the court currently favored; moved doctrinal precision out of public confession (the transferred good was usable ambiguity); moved refusing clergy out of their cities into exile.
% ABSENT_VOICES: The strict Anomoeans were excluded from the settlement's drafting, present at councils but outside the negotiated text, and would have objected that any substance-likeness conceded too much. Pro-Nicene bishops attended but were outmaneuvered by imperial procedure and detained delegations. The laity, who lived under each successive formula, had no seat anywhere in the process.
% DISAPPEARANCE_RATIONALE: Remove the likeness-formulas overnight in 359 and the eastern episcopate faces an immediate binary: Nicaea's homoousios or outright subordinationism. Communion splits along that line years earlier than it historically did, the emperor loses the uniformity instrument his religious policy ran on, and the sees redistributed through the depositions of 355-360 fall vacant or revert. The arrangement was load-bearing for imperial governance and episcopal careers alike.
% FOUNDING_PROBLEM: After Nicaea (325) failed to settle practice: eastern bishops read homoousios as risking a fusion of Father and Son, while Arius's denial of the Son's true divinity had been condemned. The church needed a confession both wings could sign, and Constantius needed uniformity he could enforce without breaking a majority.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: Athanasius's De Synodis (359), written by the formula's chief victim, concedes the impasse was real while arguing the middle formulas evaded rather than solved it; the fifth-century historians attest the sequence of failed formulas; and the court party's own abandonment of substance-language at Sirmium (357) shows the drafters treated the middle as provisional. No party denies the founding impasse existed; the dispute is over whether the formula ever addressed it.
narrative_ontology:disappearance_verdict(homoousios_christology__semi_arian_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_christology__semi_arian_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_christology__semi_arian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(homoousios_christology__semi_arian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_christology__semi_arian_reading, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_christology__semi_arian_reading_tests).
:- end_tests(homoousios_christology__semi_arian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (end-state 0.46, series peaking 0.60 around the Constantinople-360 turnover) reflects an arrangement whose costs fell unevenly: subscription was cheap for the willing and ruinous for refusers, and the formula's flexibility was worth more to the party positioned to exploit it than to those who drafted it. Suppression (scalar 0.18) is deliberately low AT THE ENDPOINT because the enforcement machinery had been dismantled by the Theodosian settlement; the series carries the real story: build-up under Constantius (0.45 to 0.68), collapse after his death and Julian's recalls (0.30), Valens's partial rebuild (0.55), lapse during the Gothic crisis (0.35), dissolution (0.18). The oscillation is driven by exogenous reign changes, not by the constraint's own reinforcement cycle; it is not intermittent reinforcement, and the base_properties scalars are measured at the interval's end phase. Theater ratio climbs monotonically (0.15 to 0.45): early councils negotiated real texts; late ones performed unanimity over a formula whose substance had been hollowed. Accessibility collapse stays low (0.35) because alternatives never closed: Nicaea, strict subordinationism, and western resistance all remained live throughout. Resistance is high (0.70): the arrangement met continuous refusal from the moment it was proposed. Fixing cost was prohibitive for the only actor who could fix it: for the emperor, dropping the middle formula meant forcing half the episcopate to capitulate or schism, which is precisely why new middle formulas kept being minted.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. From the homoean collector's seat the arrangement is opportunity: a flexible instrument that delivered sees and influence. From the pro-Nicene exile's seat the same councils and texts are a persecution machine: maximal burden, no acceptable exit. From the homoiousian draughtsmen's seat it is first a life's work and then a betrayal: they built the instrument, lost control of its meaning, and were deposed by the party that captured it, so their computed position migrates across the interval from administrator to bearer. From the imperial seat it is neither doctrine nor persecution but administration: a uniformity tool to be swapped when politics moved. The engine computes these divergences from the structural data; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   The declared structure maps cleanly onto directionality. The imperial court and the homoean bishops sit at the beneficiary end: the court collects unity without bearing doctrinal cost, the homoeans collect sees and favor, and both hold arbitrage-grade exit because their positions never depended on the formula's content. Pro-Nicene exiles sit at the target end: they bear deposition and exile, and their exit is trapped in the specific sense that both available exits (sign, or abandon office and flock) are unacceptable to them. The homoiousian party begins near the beneficiary end and migrates toward the target end as the formula's meaning drifts past them; the static declaration records their interval-end position, with the migration documented in the series and commentary. Congregations sit near symmetric: real coordination benefit (communion held together) against diffuse real costs (churn, confusion). Western bishops at Rimini share the pro-Nicene target position but with constrained rather than trapped exit, since they could and did repudiate once released.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding mandate, mint a confession both wings can sign, expired when the binary resolved: by 381 the middle party had been absorbed into Nicaea rather than defeated by it, and the formula had no remaining function. What persisted through the 370s was increasingly maintenance of position rather than mediation, which the theater series captures. The classification guards both mislabels: a pure-extraction reading would erase the genuine decade of coordination, including the absorption channel by which much of the eastern episcopate entered Nicaea peacefully through this arrangement's personnel and habits; a pure-coordination reading would erase the exile machinery and the exploitation of ambiguity. The R5 mismatch (founding problem dead, world still arranged around the arrangement for roughly two decades past usefulness) is the expected zombie-flag signature here, and it is the honest one: this arrangement did persist past its function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_index,
    'This constraint is one reading (semi_arian) of the homoousios_christology kernel; what would change structurally if a sibling reading were instantiated instead?',
    'Compare the three family files directly: victim sets (pro-Nicene exiles exist only under the semi-Arian and Arian arrangements), enforcement principals (imperial court versus conciliar consensus), and epsilon referents differ by construction.',
    'Adopting the pro-Nicene reading relocates the enforcement burden post-381 and reverses the payer/beneficiary map; adopting the arian reading removes the coordination function entirely. Cross-reading epsilon comparisons are invalid: each reading authors its own.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_index, conceptual, 'Committer-frame index: one reading of a three-reading kernel.').

omega_variable(
    ambiguity_deliberateness,
    'Was the formula''s flexibility a sincere attempt at scriptural imprecision, or engineered room for the court party to hollow the confession over time?',
    'Drafting-history contrast: Basil of Ancyra''s 358 memoir (explicit substance-similarity, aimed at both extremes) versus the Sirmium 357 text banning substance-language outright and the progressive dilution through Rimini-Seleucia to Constantinople 360.',
    'If engineered, the extraction component is larger and the homoean collectors'' claim on the gains strengthens; if sincere, the coordination share rises and the draughtsmen''s later deposition reads as capture of their instrument rather than its design intent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ambiguity_deliberateness, empirical, 'Whether the vagueness was design or drift.').

omega_variable(
    absorption_mechanism,
    'Did the semi-Arian arrangement end by coercion (suppressed under Theodosian enforcement) or by convergence (its holders concluded Nicaea was right)?',
    'Trace individual trajectories 375-395: Meletius of Antioch, Cyril of Jerusalem, the Constantinople-381 episcopate; determine whether former homoiousians subscribed under threat or argued their way to homoousios.',
    'Convergence supports a coordination-led legacy in which the arrangement did its mediating work and dissolved into its successor; coercion supports an extraction-led ending and raises the effective burden attributed to the successor arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absorption_mechanism, empirical, 'Termination mode of the middle formula.').

omega_variable(
    east_west_enforcement_asymmetry,
    'Enforcement ran at very different intensities East (depositions, exiles) and West (detention and duress at Rimini, few depositions); does the single suppression scalar misstate either region?',
    'Regional enforcement ledgers: number of sees changed by decree East versus West, duration of detentions, and the western repudiation rate after Rimini.',
    'If the asymmetry is large, per-seat classifications for western bishops are overstated by the scalar and the arrangement is closer to a regional enforcement regime wrapped in an empire-wide coordination story; if small, the scalar stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(east_west_enforcement_asymmetry, empirical, 'Regional asymmetry behind the scalar suppression value.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_christology__semi_arian_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(semi_arian_homoiousios_tr_t0, homoousios_christology__semi_arian_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(semi_arian_homoiousios_tr_t6, homoousios_christology__semi_arian_reading, theater_ratio, 6, 0.25).
narrative_ontology:measurement(semi_arian_homoiousios_tr_t12, homoousios_christology__semi_arian_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(semi_arian_homoiousios_tr_t18, homoousios_christology__semi_arian_reading, theater_ratio, 18, 0.38).
narrative_ontology:measurement(semi_arian_homoiousios_tr_t24, homoousios_christology__semi_arian_reading, theater_ratio, 24, 0.42).
narrative_ontology:measurement(semi_arian_homoiousios_tr_t30, homoousios_christology__semi_arian_reading, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(semi_arian_homoiousios_be_t0, homoousios_christology__semi_arian_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(semi_arian_homoiousios_be_t6, homoousios_christology__semi_arian_reading, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(semi_arian_homoiousios_be_t12, homoousios_christology__semi_arian_reading, base_extractiveness, 12, 0.6).
narrative_ontology:measurement(semi_arian_homoiousios_be_t18, homoousios_christology__semi_arian_reading, base_extractiveness, 18, 0.54).
narrative_ontology:measurement(semi_arian_homoiousios_be_t24, homoousios_christology__semi_arian_reading, base_extractiveness, 24, 0.5).
narrative_ontology:measurement(semi_arian_homoiousios_be_t30, homoousios_christology__semi_arian_reading, base_extractiveness, 30, 0.46).

% Suppression requirement over time
narrative_ontology:measurement(semi_arian_homoiousios_su_t0, homoousios_christology__semi_arian_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(semi_arian_homoiousios_su_t6, homoousios_christology__semi_arian_reading, suppression_requirement, 6, 0.68).
narrative_ontology:measurement(semi_arian_homoiousios_su_t12, homoousios_christology__semi_arian_reading, suppression_requirement, 12, 0.3).
narrative_ontology:measurement(semi_arian_homoiousios_su_t18, homoousios_christology__semi_arian_reading, suppression_requirement, 18, 0.55).
narrative_ontology:measurement(semi_arian_homoiousios_su_t24, homoousios_christology__semi_arian_reading, suppression_requirement, 24, 0.35).
narrative_ontology:measurement(semi_arian_homoiousios_su_t30, homoousios_christology__semi_arian_reading, suppression_requirement, 30, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_christology__semi_arian_reading, identity_coordination).
narrative_ontology:affects_constraint(homoousios_christology__semi_arian_reading, homoousios_christology__pro_nicene_reading).
narrative_ontology:affects_constraint(homoousios_christology__semi_arian_reading, homoousios_christology__arian_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'the Arian controversy' conflates three structurally distinct arrangements. The pro-Nicene reading (upstream, settled at 381) and the arian reading (radical flank) are separate stories with their own epsilon, beneficiaries, and victims. This semi-Arian story links to both because the pro-Nicene arrangement inherited this one's personnel and institutional position, and the arian arrangement competed with this one for the same non-Nicene space. Upstream/downstream: the pro-Nicene story typically cites this arrangement's failure as evidence for the necessity of precise substance-language.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
