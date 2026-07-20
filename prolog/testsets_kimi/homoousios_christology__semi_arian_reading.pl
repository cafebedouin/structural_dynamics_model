% ============================================================================
% CONSTRAINT STORY: homoousios_christology__semi_arian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   human_readable: Semi-Arian Homoiousios Christology (homoiousios reading)
 *   domain: historical_theology/ecclesiastical_politics
 *
 * SUMMARY:
 *   The homoiousios ('of similar substance') christological formula emerged
 *   in the mid-fourth century as a compromise between Nicene
 *   consubstantiality and Arian subordinationism. Sponsored by Emperor
 *   Constantius II and a broad coalition of Eastern bishops, it was enforced
 *   through imperial council management, episcopal deposition, and exile. The
 *   constraint coordinated a middle party that could not accept homoousios as
 *   philosophically sound yet recoiled from Arian creatianism. It extracted
 *   conformity from both Nicene hardliners (exiled under the formula) and
 *   Arian extremists (excluded from the imperial church), while offering its
 *   beneficiaries imperial legitimacy and synodal majorities. Historically
 *   absorbed into the Pro-Nicene settlement after Constantinople I (381), its
 *   interval shows a rise and fall of enforcement-dependent coordination.
 *   This JSON instantiates the semi_arian_reading of the
 *   homoousios_christology kernel.
 *
 * KEY AGENTS:
 *   - imperial_court (institutional/constrained): agenda-setter â enforces the formula to secure political-religious unity
 *   - semi_arian_bishops (organized/constrained): beneficiaries â receive sees and legitimacy under the compromise
 *   - nicene_hardliners (moderate/trapped): payers â exiled and silenced for refusing subscription
 *   - arian_extremists (moderate/constrained): excluded â pushed outside the imperial settlement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_christology__semi_arian_reading, 0.48).
domain_priors:suppression_score(homoousios_christology__semi_arian_reading, 0.62).
domain_priors:theater_ratio(homoousios_christology__semi_arian_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_christology__semi_arian_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_christology__semi_arian_reading, "Semi-Arian Homoiousios Christology (homoiousios reading)").
narrative_ontology:topic_domain(homoousios_christology__semi_arian_reading, "historical_theology/ecclesiastical_politics").

domain_priors:requires_active_enforcement(homoousios_christology__semi_arian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_christology__semi_arian_reading, 'c7c42fb2-c71a-44ba-b39a-0a41d511f9e1').
narrative_ontology:cs_kernel_codification('c7c42fb2-c71a-44ba-b39a-0a41d511f9e1', fixed_text).
narrative_ontology:cs_authority_grounding('c7c42fb2-c71a-44ba-b39a-0a41d511f9e1', lineage).
narrative_ontology:cs_interpretation_layer_present('c7c42fb2-c71a-44ba-b39a-0a41d511f9e1').
narrative_ontology:cs_reading_relation('c7c42fb2-c71a-44ba-b39a-0a41d511f9e1', homoousios_christology__pro_nicene_reading, influences).
narrative_ontology:cs_reading_relation('c7c42fb2-c71a-44ba-b39a-0a41d511f9e1', homoousios_christology__arian_reading, forecloses).
narrative_ontology:cs_axiom('c7c42fb2-c71a-44ba-b39a-0a41d511f9e1', foundational, homoiousios_scriptural_fidelity).
narrative_ontology:cs_axiom_status(homoiousios_scriptural_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('c7c42fb2-c71a-44ba-b39a-0a41d511f9e1', homoiousios_scriptural_fidelity, theological).
narrative_ontology:cs_axiom('c7c42fb2-c71a-44ba-b39a-0a41d511f9e1', foundational, rejection_of_homoousios_as_sabellian).
narrative_ontology:cs_axiom_status(rejection_of_homoousios_as_sabellian, holdable).
narrative_ontology:cs_axiom_grounding('c7c42fb2-c71a-44ba-b39a-0a41d511f9e1', rejection_of_homoousios_as_sabellian, theological).
narrative_ontology:cs_reference_frame('c7c42fb2-c71a-44ba-b39a-0a41d511f9e1', scriptural_analogia_fidei).
narrative_ontology:cs_drift_state('c7c42fb2-c71a-44ba-b39a-0a41d511f9e1', post_constantinople_381, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('c7c42fb2-c71a-44ba-b39a-0a41d511f9e1', '').
narrative_ontology:cs_kernel_id(homoousios_christology__semi_arian_reading, homoousios_christology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_christology__semi_arian_reading, semi_arian_bishops).
narrative_ontology:constraint_beneficiary(homoousios_christology__semi_arian_reading, imperial_court).
narrative_ontology:constraint_victim(homoousios_christology__semi_arian_reading, nicene_hardliners).
narrative_ontology:constraint_victim(homoousios_christology__semi_arian_reading, arian_extremists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Constantius II and later emperors enforce homoiousios formulae through council convocation, episcopal deposition, and imperial edict to maintain religious unity across the Empire; they gain political stability from a unified episcopate and bear the cost of potential civil unrest if the church fractures.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, imperial_court, agenda_setter,
    institutional, generational, constrained, continental).

% Bishops who reject homoousios as unscriptural and Arianism as heretical; they receive imperial recognition, possession of major sees, and synodal majority status under the homoiousian settlement, but are bound to defend the middle formula against both extremes.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, semi_arian_bishops, beneficiary,
    organized, biographical, constrained, continental).

% Athanasian bishops and monks who insist on homoousios; they are deposed, exiled, or silenced when they refuse to subscribe to the homoiousios formula, bearing the direct cost of the constraint's enforcement.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, nicene_hardliners, payer,
    moderate, biographical, trapped, continental).

% Bishops holding that the Son is a creature dissimilar to the Father (anomoios); they are excluded from the imperial church settlement because the homoiousios formula is too high-Christology for them, yet they remain a persistent dissident faction.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, arian_extremists, excluded,
    moderate, biographical, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(homoousios_christology__semi_arian_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a broad middle party of Eastern bishops who reject both Nicene homoousios as Sabellian and Arian creatianism as heretical, supplying a shared theological vocabulary that prevents immediate schism across the Imperial church.
% TRANSFER_FUNCTION: Moves imperial legitimacy, episcopal office, and sacramental authority from Nicene and Arian hardliners toward the homoiousian middle, extracting doctrinal subscription and liturgical conformity in exchange for peace and institutional recognition.
% ABSENT_VOICES: Lay Nicene congregations in Egypt and monastic communities who would reject any dilution of homoousios but were not represented at the predominantly episcopal and imperial councils; also radical Arian laity and Germanic converts outside the imperial framework.
% DISAPPEARANCE_RATIONALE: If the homoiousios constraint vanished in 341, the Eastern episcopate would have splintered between Nicene and Arian factions, imperial religious policy would have lost its mediating formula, and the pattern of councils, depositions, and exiles would have followed a different trajectory; during its interval the world rearranged around it.
% FOUNDING_PROBLEM: The theological and political crisis after Nicaea (325) in which a large plurality of bishops found the term homoousios philosophically unacceptable and biblically unsupported, yet could not embrace outright Arian subordinationism, threatening empire-wide schism and the Christian emperor's legitimacy as guarantor of unity.
% FOUNDING_PROBLEM_CORROBORATION: Modern patristic historians (Hanson, Ayres, Beeley) corroborate the political pressure for unity and the terminological unsustainability of the middle position from outside the benefiting Semi-Arian and Imperial camps; contemporary Nicene hardliners like Athanasius explicitly denied that the problem required a compromise, attesting that the founding problem was framed by the Semi-Arians themselves rather than by an independent neutral party.
narrative_ontology:disappearance_verdict(homoousios_christology__semi_arian_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_christology__semi_arian_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_christology__semi_arian_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(homoousios_christology__semi_arian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_christology__semi_arian_reading, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_christology__semi_arian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(homoousios_christology__semi_arian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(homoousios_christology__semi_arian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48) reflects moderate doctrinal and political extraction: the formula compelled subscription under threat of deposition, but its enforcement was geographically uneven and its theological appeal genuine to a large middle party. Suppression (0.62) captures active imperial enforcement through councils and edicts, lower than a totalitarian snare because emperors lacked modern surveillance and relied on episcopal collaboration. Theater_ratio (0.45) registers that much conciliar activity was genuine theological negotiation, yet an increasing share after 350 performed unity rather than achieving it. Accessibility_collapse (0.58) indicates that while Nicene and Arian alternatives persisted underground, open adherence to them became structurally untenable within the imperial church. Resistance (0.68) is high due to sustained Nicene non-compliance and Athanasian polemics that kept the alternative alive.
 *
 * PERSPECTIVAL GAP:
 *   From the imperial seat the constraint appears as necessary coordination to prevent civil unrest and schism; from the Nicene hardliner seat it appears as coerced doctrinal adulteration backed by state power. The engine computes this divergence from the same structural data: the agenda-setter's constrained exit and generational horizon moderate its extracted experience, while the payers' trapped status and biographical horizon maximize theirs. The Semi-Arian bishops occupy a middle seat â beneficiaries with constrained exit â experiencing the constraint as both genuine theological home and precarious political compromise.
 *
 * DIRECTIONALITY LOGIC:
 *   The imperial_court and semi_arian_bishops are declared beneficiaries: they gain political stability and institutional recognition respectively, yielding low directionality. Nicene_hardliners and arian_extremists are declared victims, yielding high directionality. The court's exit is constrained by the need to maintain ecclesiastical legitimacy, but it retains arbitrage power relative to the bishops; the bishops' exit is constrained by their dependence on imperial favor. The victims are trapped or constrained, amplifying effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â post-Nicene terminological deadlock â was dead by 381, when the Council of Constantinople ratified homoousios as the settled formula. The constraint's persistence after its coordination function was complete would be a piton or snare if it had survived independently; instead it was absorbed, leaving a resolved mandatrophy. The classification as tangled_rope (not snare) is warranted by the genuine coordination function among bishops who honestly found homoousios unacceptable; the classification as tangled_rope (not rope) is warranted by the identifiable victims (exiled Nicenes) and active imperial enforcement. A pure rope would have no trapped payers; a pure snare would have no large body of genuine theological adherents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    semi_arian_kernel_reading,
    'Is the homoiousios position a genuinely distinct theological commitment or a political coordination device that temporarily masked convergence with the Pro-Nicene kernel?',
    'Examine post-381 subscriptions of former Semi-Arian bishops: if they adopted homoousios operationally without structural change to their theology, the reading dissolves into the Pro-Nicene sibling; if they retained a distinct soteriology or sacramental practice, the reading is a separate constraint.',
    'If purely political, classification shifts toward rope or scaffold; if genuinely theological, it remains a commitment-system constraint with doctrinal extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(semi_arian_kernel_reading, conceptual, 'Whether the Semi-Arian reading is a distinct theological constraint or a political wrapper.').

omega_variable(
    mandatrophy_by_absorption,
    'Did the Semi-Arian constraint atrophy because its coordination function was completed, or was it captured and repurposed by Pro-Nicene institutional power?',
    'Trace the institutional continuity of Semi-Arian bishops into the Theodosian church: if they retained sees and influence after 381, the constraint was absorbed by identity coordination; if they were systematically replaced, it was destroyed by external suppression.',
    'If absorbed by identity coordination, the post-381 period shows the constraint''s conversion into a Pro-Nicene rope; if destroyed, the original constraint terminates as a failed snare or tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_by_absorption, empirical, 'Whether the constraint dissolved naturally or was forcibly overridden.').

omega_variable(
    imperial_vs_theological_authority,
    'Does the constraint''s persistence depend on imperial enforcement or on genuine theological conviction among the episcopate?',
    'Compare the constraint''s strength under Constantius II (strong imperial support) versus Julian (toleration, no imperial enforcement): if it collapses without imperial backing, it is extraction-dependent; if it persists, it has independent coordination power.',
    'If purely imperial, directionality for the court is higher (it is a coercive agenda-setter, not a beneficiary of coordination); if theological, the bishops'' directionality is lower (genuine believers, not captured beneficiaries).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imperial_vs_theological_authority, empirical, 'Imperial enforcement versus theological conviction as the driver.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_christology__semi_arian_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(semi_arian_tr_t0, homoousios_christology__semi_arian_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(semi_arian_tr_t8, homoousios_christology__semi_arian_reading, theater_ratio, 8, 0.35).
narrative_ontology:measurement(semi_arian_tr_t16, homoousios_christology__semi_arian_reading, theater_ratio, 16, 0.48).
narrative_ontology:measurement(semi_arian_tr_t24, homoousios_christology__semi_arian_reading, theater_ratio, 24, 0.58).
narrative_ontology:measurement(semi_arian_tr_t32, homoousios_christology__semi_arian_reading, theater_ratio, 32, 0.52).
narrative_ontology:measurement(semi_arian_tr_t40, homoousios_christology__semi_arian_reading, theater_ratio, 40, 0.2).

% Extraction over time
narrative_ontology:measurement(semi_arian_be_t0, homoousios_christology__semi_arian_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(semi_arian_be_t8, homoousios_christology__semi_arian_reading, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(semi_arian_be_t16, homoousios_christology__semi_arian_reading, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(semi_arian_be_t24, homoousios_christology__semi_arian_reading, base_extractiveness, 24, 0.45).
narrative_ontology:measurement(semi_arian_be_t32, homoousios_christology__semi_arian_reading, base_extractiveness, 32, 0.3).
narrative_ontology:measurement(semi_arian_be_t40, homoousios_christology__semi_arian_reading, base_extractiveness, 40, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(semi_arian_su_t0, homoousios_christology__semi_arian_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(semi_arian_su_t8, homoousios_christology__semi_arian_reading, suppression_requirement, 8, 0.78).
narrative_ontology:measurement(semi_arian_su_t16, homoousios_christology__semi_arian_reading, suppression_requirement, 16, 0.72).
narrative_ontology:measurement(semi_arian_su_t24, homoousios_christology__semi_arian_reading, suppression_requirement, 24, 0.4).
narrative_ontology:measurement(semi_arian_su_t32, homoousios_christology__semi_arian_reading, suppression_requirement, 32, 0.55).
narrative_ontology:measurement(semi_arian_su_t40, homoousios_christology__semi_arian_reading, suppression_requirement, 40, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_christology__semi_arian_reading, identity_coordination).
narrative_ontology:affects_constraint(homoousios_christology__semi_arian_reading, pro_nicene_reading).

% DUAL FORMULATION NOTE:
% The homoousios_christology kernel decomposes into three structurally distinct constraints: the arian_reading (high extraction on Nicenes, denies shared substance), the semi_arian_reading (moderate extraction on both extremes, coordinates a middle), and the pro_nicene_reading (high extraction on Arians and Semi-Arians, asserts identical substance). Each reading has a distinct epsilon, beneficiary/victim structure, and enforcement profile. This file instantiates the semi_arian_reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
