% ============================================================================
% CONSTRAINT STORY: homoousios_christology__pro_nicene_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Pro-Nicene Homoousios Doctrine as Imperial-Ecclesiastical Orthodoxy Standard
 *   domain: religious/political
 *
 * SUMMARY:
 *   This story authors the pro-Nicene reading of the homoousios kernel: the
 *   claim that Christ is of identical divine substance with the Father, as
 *   codified at Nicaea (325) and reaffirmed against sustained Arian and
 *   semi-Arian resistance through Constantinople (381) and beyond. This is
 *   NOT a story about whether homoousios is theologically true — it is a
 *   story about the structural operation of the pro-Nicene reading as an
 *   enforced ecclesiastical-imperial arrangement: who drafted it, who
 *   benefited from its adoption as the sole legitimate formula, who was
 *   excluded and on what terms, and how imperial civil power was mobilized to
 *   enforce a specific answer to a genuinely contested theological question.
 *   The suppression trajectory dips sharply under pro-Arian emperors
 *   (Constantius II, Valens) before climbing steeply after Theodosius I's 380
 *   edict made Nicene Christianity the sole legal religion of the empire —
 *   this V-shape reflects real oscillation in imperial backing, not
 *   measurement noise.
 *
 * KEY AGENTS:
 *   - athanasian_episcopal_network: Primary agenda-setter (institutional/arbitrage) — drafts and defends the formula, moves between exile and restoration
 *   - constantinian_imperial_authority: Primary beneficiary (institutional/arbitrage) — gains unified, governable church structure
 *   - arian_clergy: Primary target (organized/trapped) — declared heretical, exiled, stripped of office
 *   - gothic_and_germanic_converts: Secondary victims (powerless/constrained) — entire vernacular Christian tradition retroactively branded heretical
 *   - later_church_historians: Analytical observer — reconstructs the political dimension of the councils from outside any confessional stake
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
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_christology__pro_nicene_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_christology__pro_nicene_reading, "Pro-Nicene Homoousios Doctrine as Imperial-Ecclesiastical Orthodoxy Standard").
narrative_ontology:topic_domain(homoousios_christology__pro_nicene_reading, "religious/political").

domain_priors:requires_active_enforcement(homoousios_christology__pro_nicene_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_christology__pro_nicene_reading, '9f4ec072-841c-4851-b452-37979a38cbd7').
narrative_ontology:cs_kernel_codification('9f4ec072-841c-4851-b452-37979a38cbd7', formalized).
narrative_ontology:cs_authority_grounding('9f4ec072-841c-4851-b452-37979a38cbd7', extraction).
narrative_ontology:cs_interpretation_layer_present('9f4ec072-841c-4851-b452-37979a38cbd7').
narrative_ontology:cs_reading_relation('9f4ec072-841c-4851-b452-37979a38cbd7', homoousios_christology__arian_reading, forecloses).
narrative_ontology:cs_reading_relation('9f4ec072-841c-4851-b452-37979a38cbd7', homoousios_christology__semi_arian_reading, influences).
narrative_ontology:cs_axiom('9f4ec072-841c-4851-b452-37979a38cbd7', foundational, father_and_son_share_single_undivided_divine_substance).
narrative_ontology:cs_axiom_status(father_and_son_share_single_undivided_divine_substance, holdable).
narrative_ontology:cs_axiom_grounding('9f4ec072-841c-4851-b452-37979a38cbd7', father_and_son_share_single_undivided_divine_substance, theological).
narrative_ontology:cs_axiom('9f4ec072-841c-4851-b452-37979a38cbd7', secondary, conciliar_anathema_binds_universal_church).
narrative_ontology:cs_axiom_status(conciliar_anathema_binds_universal_church, holdable).
narrative_ontology:cs_axiom_grounding('9f4ec072-841c-4851-b452-37979a38cbd7', conciliar_anathema_binds_universal_church, conventional).
narrative_ontology:cs_reference_frame('9f4ec072-841c-4851-b452-37979a38cbd7', apostolic_scriptural_consensus).
narrative_ontology:cs_drift_state('9f4ec072-841c-4851-b452-37979a38cbd7', post_theodosian_legal_establishment, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9f4ec072-841c-4851-b452-37979a38cbd7', '').
narrative_ontology:cs_kernel_id(homoousios_christology__pro_nicene_reading, homoousios_christology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_christology__pro_nicene_reading, nicene_bishops_alexandrian_faction).
narrative_ontology:constraint_beneficiary(homoousios_christology__pro_nicene_reading, constantinian_imperial_authority).
narrative_ontology:constraint_beneficiary(homoousios_christology__pro_nicene_reading, athanasian_episcopal_network).
narrative_ontology:constraint_victim(homoousios_christology__pro_nicene_reading, arian_clergy).
narrative_ontology:constraint_victim(homoousios_christology__pro_nicene_reading, subordinationist_congregations).
narrative_ontology:constraint_victim(homoousios_christology__pro_nicene_reading, gothic_and_germanic_converts).
narrative_ontology:constraint_victim(homoousios_christology__pro_nicene_reading, dissenting_provincial_bishops).
narrative_ontology:constraint_vindicates(homoousios_christology__pro_nicene_reading, trinitarian_orthodoxy).
narrative_ontology:constraint_vindicates(homoousios_christology__pro_nicene_reading, single_divine_substance_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts and defends the homoousios formula at Nicaea and in the decades of councils that follow, controls the drafting of creeds and anathemas, and secures repeated imperial backing to depose rival bishops. Moves fluidly between exile and restoration depending on which emperor is in power, but always retains the doctrinal authority to declare others heretical.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, athanasian_episcopal_network, agenda_setter,
    institutional, civilizational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_christology__pro_nicene_reading, athanasian_episcopal_network, beneficiary).

% Convenes the council to end a doctrinal dispute threatening imperial unity, backs the homoousios formula with civil enforcement power (exile, property confiscation, military suppression of dissenting sees), and gains a single unified church structure that can be governed as an arm of empire. Benefits regardless of the formula's truth, since unity itself is the imperial good sought.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, constantinian_imperial_authority, beneficiary,
    institutional, generational, arbitrage, continental).

% Gains doctrinal primacy, control over the definition of orthodoxy, and a mechanism (anathema) for permanently excluding theological and political rivals from clerical office, property, and communion.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, nicene_bishops_alexandrian_faction, beneficiary,
    organized, generational, arbitrage, regional).

% Holds a competing reading of scripture and tradition, is declared heretical by conciliar anathema, loses ecclesiastical office, is subject to exile and confiscation under imperial edict, and has nowhere within the empire's church structure to appeal the ruling once the emperor enforces it.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, arian_clergy, payer,
    organized, biographical, trapped, regional).

% Ordinary believers whose received catechesis is retroactively declared heretical; face exclusion from communion, loss of local clergy to exile, and pressure to accept a formula (homoousios) that uses a non-scriptural philosophical term many find genuinely confusing or theologically suspect.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, subordinationist_congregations, payer,
    powerless, biographical, trapped, local).

% Converted to Christianity under Arian missionary influence (Ulfilas' translation work), these peoples' entire vernacular Christian tradition is later branded heretical by the pro-Nicene imperial church, creating centuries of religious-political conflict as Nicene rulers treat their faith as grounds for subjugation or forced reconversion.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, gothic_and_germanic_converts, payer,
    powerless, generational, constrained, continental).

% Bishops who favored the homoiousios (similar substance) compromise or who simply wanted to preserve local doctrinal latitude find themselves forced into a binary choice between full subscription to homoousios or formal exclusion, with imperial troops available to enforce the choice in contested sees.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, dissenting_provincial_bishops, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(homoousios_christology__pro_nicene_reading, dissenting_provincial_bishops, excluded).

% Reconstruct the councils' proceedings, imperial correspondence, and exile records to assess how much of the outcome reflects theological reasoning versus political consolidation. Their analysis has no power to alter the settled doctrine but shapes how the episode is understood.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, later_church_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the fractured fourth-century church with a single, precisely worded formula for Christ's relationship to the Father, ending (in principle) generations of competing local creeds and enabling unified liturgy, communion, and clerical recognition across a vast and diverse empire.
% TRANSFER_FUNCTION: Moves doctrinal authority, clerical office, church property, and imperial favor from bishops and congregations holding subordinationist or compromise Christologies to those holding the homoousios formula; also moves civil peace and administrative unity to the imperial authority at the cost of coerced conformity for dissenting populations.
% ABSENT_VOICES: Arian and semi-Arian bishops present at Nicaea were outvoted and later exiled rather than persuaded; the Gothic and Germanic converts evangelized under Arian missionaries had no representation at any council that later branded their faith heretical; ordinary lay believers whose local churches had used non-homoousian language for a generation were not consulted on the change.
% DISAPPEARANCE_RATIONALE: If the homoousios formula and its enforcement apparatus vanished, the fourth- and fifth-century church would likely have remained doctrinally fragmented along regional and imperial lines (much as it briefly did under pro-Arian emperors like Constantius II and Valens), clerical office and property would have been distributed differently, and the later trajectory of Trinitarian orthodoxy, Christological councils, and the East-West church split would have unfolded along a substantially different path.
% FOUNDING_PROBLEM: The church faced an unresolved and increasingly disruptive dispute over Christ's relationship to God the Father (Arius's subordinationist teaching versus the emerging Alexandrian consensus), which threatened both theological coherence and, crucially, the political unity Constantine wanted from a single state-recognized church.
% FOUNDING_PROBLEM_CORROBORATION: Pro-Nicene sources (Athanasius, later conciliar records) attest the problem was a genuine and dangerous heresy requiring definitive resolution. Independent testimony is harder to find since surviving sources are overwhelmingly pro-Nicene, but the fact that pro-Arian emperors later reversed the ruling, that entire Germanic Christian populations remained Arian for centuries without collapse into incoherence, and that modern historians of late antiquity (outside any confessional stake) describe the councils as substantially driven by imperial politics and factional rivalry, together suggest the founding problem was at least partly a construction of the winning faction rather than a self-evident theological necessity.
narrative_ontology:disappearance_verdict(homoousios_christology__pro_nicene_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_christology__pro_nicene_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_christology__pro_nicene_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored at 0.68 by 451: the formula itself may express a coherent theological claim, but its *operation* as an enforced imperial-ecclesiastical standard extracts clerical office, property, and communion status from dissenting factions who held a rival reading of the same scriptural and traditional sources. Suppression is high (0.79) and rises sharply after 381 because the mechanism of enforcement is not persuasion but anathema plus imperial civil penalty (exile, confiscation, later criminalization of Arian worship under Theodosius). Theater ratio is moderate-rising (0.42) reflecting that a substantial share of conciliar activity is genuine theological argument, but an increasing share becomes performative reaffirmation of settled orthodoxy against outgroups already excluded rather than live doctrinal reasoning.
 *
 * PERSPECTIVAL GAP:
 *   From the athanasian_episcopal_network's seat, the arrangement is a genuine and hard-won coordination achievement defending revealed truth against a corrosive heresy. From the arian_clergy or gothic_and_germanic_converts seats, the identical structure is an imperially-backed exclusion mechanism that declared their sincerely-held, textually-grounded faith criminal. The engine computes these as different per-seat classifications from the same structural data; this story does not adjudicate which reading is theologically correct — that adjudication is exactly what the kernel-reading framing routes outside a single ε-invariant constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   The imperial authority and the winning episcopal faction are structural beneficiaries: they gain unified governance and permanent doctrinal primacy respectively, regardless of the formula's truth-value — this is the imperial-church alignment the expected structural delta names. Arian clergy, subordinationist congregations, and Gothic/Germanic converts are structural targets: they hold a rival reading of the same textual tradition and pay through loss of office, exile, and centuries of being treated as outside orthodoxy. Dissenting provincial bishops who favored the homoiousios compromise are treated here as payers with constrained (not fully trapped) exit, since some could and did negotiate local accommodation depending on the reigning emperor's sympathies — this differentiates them from arian_clergy who bore fuller sanction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a genuinely fractured fourth-century Christology needing resolution for both theological and imperial-administrative reasons) was real in 325. But the arrangement's persistence as a coercive imperial-legal standard outlives the moment when it could plausibly be justified as urgent crisis-resolution: by the time of Theodosius's 380 edict, homoousios is not settling a live dispute so much as being weaponized to criminalize a still-substantial rival tradition (Arian Christianity remained the majority faith of several Germanic kingdoms for another two centuries). Classifying this as tangled_rope rather than either pure rope or pure snare prevents two mislabelings: treating the entire arrangement as innocent doctrinal coordination (ignoring the exile and confiscation machinery) and treating it as pure cynical power-grab (ignoring authentic theological effort by pro-Nicene bishops facing real, unresolved subordinationist claims).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_truth_vs_political_consolidation,
    'Is the pro-Nicene formula''s dominance best explained by superior theological reasoning that genuinely persuaded the majority of bishops, or by imperial political consolidation that made one reading enforceable regardless of its persuasive merits on the theological questions at stake?',
    'Comparative analysis of voting patterns at Nicaea and Constantinople against imperial pressure timing; examination of how quickly doctrinal allegiance shifted when imperial backing shifted (under Constantius II and Valens) versus how stable it was independent of imperial enforcement.',
    'If theological persuasion dominates, the pro-Nicene reading''s authority claim is closer to genuine expertise-grounded coordination (rope-leaning). If political consolidation dominates, the same structure is closer to imperially-enforced extraction dressed in theological language (snare-leaning). This story''s tangled_rope claim sits between these poles and would shift toward either pole depending on resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_truth_vs_political_consolidation, conceptual, 'Whether Nicene dominance reflects theological persuasion or imperial political consolidation').

omega_variable(
    homoousios_as_kernel_reading_not_topic,
    'Given that this story, the arian_reading, and the semi_arian_reading each author a different ε for what colloquial usage calls ''the Arian controversy,'' is the disagreement located in the theological content of the claims or in the structural position (winner/loser, enforcer/enforced) each reading occupies relative to the same historical events?',
    'Compare the three sibling stories'' beneficiary/victim declarations and enforcement structures directly; check whether the structural delta (who benefits, who is excluded, what enforcement exists) tracks the theological content or tracks which faction eventually held imperial power.',
    'If the structural delta tracks imperial power capture rather than theological content per se, this suggests the classification differences across the three kernel readings are substantially about who won the political contest, not about the intrinsic merits of the Christological claims — which would sharpen the political-consolidation reading of the prior omega.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(homoousios_as_kernel_reading_not_topic, conceptual, 'Whether inter-reading ε differences track theological content or political outcome').

omega_variable(
    anathema_mechanism_scope,
    'Was conciliar anathema (formal declaration of heresy) primarily a theological-disciplinary tool internal to the church''s self-understanding, or was it functionally inseparable from the civil penalties (exile, property confiscation) that the imperial state attached to it after Theodosius?',
    'Examine cases where anathema was pronounced without accompanying imperial civil enforcement (e.g., in periods or regions where the state did not back the ruling faction) and assess whether the anathema alone produced comparable exclusionary effects.',
    'If anathema without civil backing had limited practical effect, the suppression measured here is substantially an artifact of the imperial alliance rather than an intrinsic property of church doctrinal discipline — meaning the extraction is downstream of the church-state fusion, not the theological claim itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anathema_mechanism_scope, empirical, 'Whether anathema''s exclusionary force depends on imperial civil enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_christology__pro_nicene_reading, 325, 451).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_tr_t325, homoousios_christology__pro_nicene_reading, theater_ratio, 325, 0.2).
narrative_ontology:measurement_basis(homo_tr_t325, observed).
narrative_ontology:measurement(homo_tr_t350, homoousios_christology__pro_nicene_reading, theater_ratio, 350, 0.28).
narrative_ontology:measurement_basis(homo_tr_t350, observed).
narrative_ontology:measurement(homo_tr_t381, homoousios_christology__pro_nicene_reading, theater_ratio, 381, 0.35).
narrative_ontology:measurement_basis(homo_tr_t381, observed).
narrative_ontology:measurement(homo_tr_t410, homoousios_christology__pro_nicene_reading, theater_ratio, 410, 0.38).
narrative_ontology:measurement_basis(homo_tr_t410, observed).
narrative_ontology:measurement(homo_tr_t431, homoousios_christology__pro_nicene_reading, theater_ratio, 431, 0.4).
narrative_ontology:measurement_basis(homo_tr_t431, observed).
narrative_ontology:measurement(homo_tr_t451, homoousios_christology__pro_nicene_reading, theater_ratio, 451, 0.42).
narrative_ontology:measurement_basis(homo_tr_t451, observed).

% Extraction over time
narrative_ontology:measurement(homo_be_t325, homoousios_christology__pro_nicene_reading, base_extractiveness, 325, 0.45).
narrative_ontology:measurement_basis(homo_be_t325, observed).
narrative_ontology:measurement(homo_be_t350, homoousios_christology__pro_nicene_reading, base_extractiveness, 350, 0.52).
narrative_ontology:measurement_basis(homo_be_t350, observed).
narrative_ontology:measurement(homo_be_t381, homoousios_christology__pro_nicene_reading, base_extractiveness, 381, 0.6).
narrative_ontology:measurement_basis(homo_be_t381, observed).
narrative_ontology:measurement(homo_be_t410, homoousios_christology__pro_nicene_reading, base_extractiveness, 410, 0.64).
narrative_ontology:measurement_basis(homo_be_t410, observed).
narrative_ontology:measurement(homo_be_t431, homoousios_christology__pro_nicene_reading, base_extractiveness, 431, 0.66).
narrative_ontology:measurement_basis(homo_be_t431, observed).
narrative_ontology:measurement(homo_be_t451, homoousios_christology__pro_nicene_reading, base_extractiveness, 451, 0.68).
narrative_ontology:measurement_basis(homo_be_t451, observed).

% Suppression requirement over time
narrative_ontology:measurement(homo_su_t325, homoousios_christology__pro_nicene_reading, suppression_requirement, 325, 0.5).
narrative_ontology:measurement_basis(homo_su_t325, observed).
narrative_ontology:measurement(homo_su_t350, homoousios_christology__pro_nicene_reading, suppression_requirement, 350, 0.35).
narrative_ontology:measurement_basis(homo_su_t350, observed).
narrative_ontology:measurement(homo_su_t381, homoousios_christology__pro_nicene_reading, suppression_requirement, 381, 0.68).
narrative_ontology:measurement_basis(homo_su_t381, observed).
narrative_ontology:measurement(homo_su_t410, homoousios_christology__pro_nicene_reading, suppression_requirement, 410, 0.73).
narrative_ontology:measurement_basis(homo_su_t410, observed).
narrative_ontology:measurement(homo_su_t431, homoousios_christology__pro_nicene_reading, suppression_requirement, 431, 0.77).
narrative_ontology:measurement_basis(homo_su_t431, observed).
narrative_ontology:measurement(homo_su_t451, homoousios_christology__pro_nicene_reading, suppression_requirement, 451, 0.79).
narrative_ontology:measurement_basis(homo_su_t451, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_christology__pro_nicene_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(homoousios_christology__pro_nicene_reading, 0.08).
narrative_ontology:affects_constraint(homoousios_christology__pro_nicene_reading, arian_reading).
narrative_ontology:affects_constraint(homoousios_christology__pro_nicene_reading, semi_arian_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the homoousios_christology kernel. arian_reading and semi_arian_reading are separate constraint files with their own ε, beneficiary/victim structures, and classifications — they are not folded into this one. The pro-Nicene reading is authored here as the eventually-enforced imperial-ecclesiastical standard; the sibling readings author the same historical period from the structural position of the excluded and compromise factions respectively. See cs_structure.reading_relations for the typed relationship between this reading and each sibling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
