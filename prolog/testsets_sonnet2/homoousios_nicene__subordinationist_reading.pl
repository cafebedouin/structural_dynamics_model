% ============================================================================
% CONSTRAINT STORY: homoousios_nicene__subordinationist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homoousios_nicene__subordinationist_reading, []).

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
 *   constraint_id: homoousios_nicene__subordinationist_reading
 *   human_readable: Homoousios Read as Compatible with Subordination (Son Derives Being from Father)
 *   domain: historical_theology/ecclesiastical_history/philosophy_of_religion
 *
 * SUMMARY:
 *   This story authors ONE reading of the contested homoousios kernel from
 *   the Nicene creed of 325: the subordinationist reading, under which the
 *   Son's shared divinity (homoousios, 'of the same essence') is held
 *   compatible with the Son deriving being from the Father and standing in
 *   some ontological or functional subordination to the Father. This reading
 *   was institutionally powerful for much of the fourth century — backed at
 *   various points by imperial authority (notably under Constantius II) — and
 *   its remnants persisted for centuries among Germanic Christian populations
 *   converted under Homoian missionary activity (Ulfilas and the Goths). The
 *   sibling readings — metaphysical equality (the eventual
 *   Cappadocian/Chalcedonian settlement) and honorific similarity (the
 *   homoiousian 'like-in-essence' compromise position) — are separate
 *   constraints, not alternative measurements of this one. Each reading has
 *   its own beneficiary/victim structure and its own ε; they are linked here
 *   only via network edges and the shared kernel_id.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_nicene__subordinationist_reading, 0.62).
domain_priors:suppression_score(homoousios_nicene__subordinationist_reading, 0.71).
domain_priors:theater_ratio(homoousios_nicene__subordinationist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_nicene__subordinationist_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_nicene__subordinationist_reading, "Homoousios Read as Compatible with Subordination (Son Derives Being from Father)").
narrative_ontology:topic_domain(homoousios_nicene__subordinationist_reading, "historical_theology/ecclesiastical_history/philosophy_of_religion").

domain_priors:requires_active_enforcement(homoousios_nicene__subordinationist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_nicene__subordinationist_reading, '2e5dcfce-b974-44c7-ade2-e6c78f485f75').
narrative_ontology:cs_kernel_codification('2e5dcfce-b974-44c7-ade2-e6c78f485f75', formalized).
narrative_ontology:cs_authority_grounding('2e5dcfce-b974-44c7-ade2-e6c78f485f75', lineage).
narrative_ontology:cs_interpretation_layer_present('2e5dcfce-b974-44c7-ade2-e6c78f485f75').
narrative_ontology:cs_reading_relation('2e5dcfce-b974-44c7-ade2-e6c78f485f75', homoousios_nicene__metaphysical_equality_reading, forecloses).
narrative_ontology:cs_reading_relation('2e5dcfce-b974-44c7-ade2-e6c78f485f75', homoousios_nicene__honorific_similarity_reading, influences).
narrative_ontology:cs_axiom('2e5dcfce-b974-44c7-ade2-e6c78f485f75', foundational, derivation_of_being_compatible_with_shared_divinity).
narrative_ontology:cs_axiom_status(derivation_of_being_compatible_with_shared_divinity, holdable).
narrative_ontology:cs_axiom_grounding('2e5dcfce-b974-44c7-ade2-e6c78f485f75', derivation_of_being_compatible_with_shared_divinity, conventional).
narrative_ontology:cs_axiom('2e5dcfce-b974-44c7-ade2-e6c78f485f75', foundational, scriptural_asymmetry_passages_govern_ontological_reading).
narrative_ontology:cs_axiom_status(scriptural_asymmetry_passages_govern_ontological_reading, overridden).
narrative_ontology:cs_axiom_grounding('2e5dcfce-b974-44c7-ade2-e6c78f485f75', scriptural_asymmetry_passages_govern_ontological_reading, conventional).
narrative_ontology:cs_reference_frame('2e5dcfce-b974-44c7-ade2-e6c78f485f75', nicene_325_conciliar_formula).
narrative_ontology:cs_drift_state('2e5dcfce-b974-44c7-ade2-e6c78f485f75', post_constantinople_381, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('2e5dcfce-b974-44c7-ade2-e6c78f485f75', '').
narrative_ontology:cs_kernel_id(homoousios_nicene__subordinationist_reading, homoousios_nicene).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_nicene__subordinationist_reading, subordinationist_clergy_networks).
narrative_ontology:constraint_beneficiary(homoousios_nicene__subordinationist_reading, arian_semi_arian_remnant_communities).
narrative_ontology:constraint_beneficiary(homoousios_nicene__subordinationist_reading, regional_bishops_favoring_scriptural_literalism).
narrative_ontology:constraint_victim(homoousios_nicene__subordinationist_reading, nicene_orthodox_communities).
narrative_ontology:constraint_victim(homoousios_nicene__subordinationist_reading, trinitarian_theologians_post_381).
narrative_ontology:constraint_victim(homoousios_nicene__subordinationist_reading, ecumenical_conciliar_authority).
narrative_ontology:constraint_vindicates(homoousios_nicene__subordinationist_reading, scriptural_priority_over_conciliar_formula).
narrative_ontology:constraint_vindicates(homoousios_nicene__subordinationist_reading, derivation_of_being_as_compatible_with_shared_divinity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bishops and presbyters, especially in the eastern provinces after Nicaea (Eusebian and later homoian factions), who read homoousios as permitting the Son's derivation from the Father while retaining shared divinity. They administer sees, ordain clergy, and hold imperial favor intermittently (under Constantius II, for instance), using that leverage to press the subordinationist reading into creeds and synodal statements.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, subordinationist_clergy_networks, agenda_setter,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(homoousios_nicene__subordinationist_reading, subordinationist_clergy_networks, beneficiary).

% Congregations and clergy who retained subordinationist Christology (Arian and Homoian variants, later persisting among Germanic tribes such as the Goths and Vandals) after the conciliar tide turned against them. This reading of homoousios gives their position continued theological legitimacy and a route back into orthodoxy's own vocabulary rather than requiring wholesale doctrinal surrender.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, arian_semi_arian_remnant_communities, beneficiary,
    moderate, generational, constrained, regional).

% Bishops, monastics, and lay communities committed to the Athanasian/Cappadocian reading of homoousios as full ontological equality. Where the subordinationist reading gains ground in a see or province, they face deposition, exile (as Athanasius himself repeatedly experienced), loss of church buildings, and doctrinal marginalization enforced by imperial religious policy under subordinationist-leaning emperors.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, nicene_orthodox_communities, payer,
    organized, civilizational, constrained, continental).

% The Cappadocian Fathers and successors who articulated the settled post-Constantinople-381 formula (one ousia, three hypostases, no subordination in being). This reading's persistence in dissenting communities and revivalist theology forces them into perpetual doctrinal defense, treatise-writing, and conciliar re-litigation of a question they consider closed.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, trinitarian_theologians_post_381, payer,
    institutional, civilizational, constrained, continental).

% The institutional authority of Nicaea (325) and Constantinople (381) as binding doctrinal settlements. Every persistence of the subordinationist reading of homoousios erodes the claim that these councils definitively closed the question, forcing the conciliar tradition to keep re-asserting its own finality rather than resting on it.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, ecumenical_conciliar_authority, payer,
    institutional, civilizational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_nicene__subordinationist_reading, ecumenical_conciliar_authority, agenda_setter).

% Constantine, Constantius II, Valens, and Theodosius I each shifted imperial backing between subordinationist and equality readings for reasons of political unity as much as theology. Their arbitration decided which reading held state enforcement power at a given moment, but their own motives (political stability, not doctrinal conviction) are excluded from the theological record that credits or blames the councils themselves.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, roman_emperors_arbitrating_doctrine, excluded,
    institutional, biographical, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_nicene__subordinationist_reading, roman_emperors_arbitrating_doctrine, agenda_setter).

% Later theological traditions (Unitarian movements, Jehovah's Witnesses' Christology, some strands of academic historical-critical scholarship) that revisit the subordinationist reading as a live textual and historical possibility, treating the fourth-century contest as unresolved rather than settled by conciliar fiat.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, later_christological_movements, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(homoousios_nicene__subordinationist_reading, subordinationist_clergy_networks).
narrative_ontology:fixing_cost_class(homoousios_nicene__subordinationist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The subordinationist reading coordinates a scriptural-literalist interpretive community: it lets clergy and congregations affirm 'the Son is divine' (satisfying homoousios's baptismal and liturgical use) while preserving passages that speak of the Father sending, commanding, or being greater than the Son, without forcing an interpretive leap to co-equal ontology that many found scripturally under-supported.
% TRANSFER_FUNCTION: Where this reading gains institutional power (imperial favor, a see's episcopal succession, a regional synod), it moves ecclesiastical authority, property, and doctrinal legitimacy away from Nicene-committed clergy and communities and toward subordinationist clergy networks — deposing bishops, reassigning churches, and rewriting creedal formulas used in ordination and catechesis.
% ABSENT_VOICES: The laity in mixed or contested sees, whose baptismal and liturgical practice used homoousios language without necessarily holding a technical position on ontological equality versus derivation, are rarely represented in the surviving conciliar and polemical record — the debate is preserved almost entirely through bishops, court theologians, and imperial correspondence.
% DISAPPEARANCE_RATIONALE: If the subordinationist reading were removed from the field of live theological options entirely, the fourth-century doctrinal conflict loses its central axis: imperial religious policy, the depositions of Athanasius, the shifting councils of the 350s-360s, and the eventual Cappadocian settlement at Constantinople 381 all exist because this reading was a genuinely contested, institutionally backed alternative, not a fringe position swiftly dismissed.
% FOUNDING_PROBLEM: How to affirm, using the single word homoousios adopted at Nicaea in 325, that the Son is genuinely divine and shares the Father's being, while remaining faithful to scriptural language (John 14:28, 'the Father is greater than I'; 1 Corinthians 15:28) that appears to describe an asymmetry between Father and Son.
% FOUNDING_PROBLEM_CORROBORATION: Nicene-committed theologians (Athanasius, the Cappadocians) attest the problem as resolved by 381 and treat continued subordinationist readings as heresy rather than live exegesis. Independent historians of early Christianity (outside the confessional commitments of either camp) corroborate that the scriptural texts cited by subordinationists are genuinely present in the canon and that the ontological-equality reading required substantial philosophical elaboration (ousia/hypostasis distinctions) not transparently given by the biblical text itself — supporting the claim that the founding exegetical problem was real and not merely a heretical pretext.
narrative_ontology:disappearance_verdict(homoousios_nicene__subordinationist_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_nicene__subordinationist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_nicene__subordinationist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(homoousios_nicene__subordinationist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_nicene__subordinationist_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_nicene__subordinationist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(homoousios_nicene__subordinationist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(homoousios_nicene__subordinationist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction rises through the mid-fourth century (0.35 to 0.70 by 381) as the subordinationist reading moves from a live minority exegetical position into an instrument of imperially-backed ecclesiastical purges — depositions, exiles, and property transfers away from Nicene-committed clergy. It declines somewhat after 381 as conciliar consolidation strips it of centralized state backing, but persists (0.58-0.62) through its survival among Homoian Germanic Christian populations, where it continued to structure ecclesiastical authority in Ostrogothic and Visigothic territories into the fifth century and beyond. Suppression tracks the same arc but with more persistence, reflecting the active doctrinal policing (synodal condemnations, banishments) required to hold either reading's dominance in a contested see.
 *
 * PERSPECTIVAL GAP:
 *   From the subordinationist clergy seat, this reading is a defensible, scripturally-grounded coordination of the church's Christological language with its scriptural canon — a genuine solution to a real exegetical tension. From the Nicene orthodox seat, the same reading is functionally extractive: it uses the shared vocabulary of homoousios to claim doctrinal legitimacy while denying the substance (co-equality) that the term was adopted at Nicaea specifically to secure, and its institutional victories came through imperial coercion rather than theological persuasion. The engine should compute divergent per-seat types from this same structural data — that divergence is the point, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Subordinationist clergy networks and Arian/Semi-Arian remnant communities are declared beneficiaries: where this reading holds institutional power, they retain sees, ordination authority, and doctrinal legitimacy. Nicene orthodox communities, post-381 trinitarian theologians, and the ecumenical conciliar authority itself are declared victims: the reading's institutional success directly costs them ecclesiastical office, communities, and the claim that Nicaea/Constantinople settled the question. The directionality here is inherently reversible with imperial favor — this is why suppression measures activity, not fixed structural position — but at any given cross-section the beneficiary/victim split is real and asymmetric.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling shared-divinity language with scriptural asymmetry passages) never fully disappeared — it recurs in every subsequent Christological controversy (Nestorian, Monophysite, and later Unitarian revivals) — but the specific institutional form of fourth-century subordinationism as a state-backed ecclesiastical program did dissolve after Theodosius I's Edict of Thessalonica (380) and Constantinople I (381) removed imperial backing. Classifying this as tangled_rope rather than pure snare avoids treating fourth-century subordinationists as mere power-seekers with no genuine exegetical grievance, while classifying it as tangled_rope rather than pure rope avoids treating the imperially-enforced depositions and exiles of Nicene bishops as anything other than real extraction riding on a real interpretive disagreement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy_at_nicaea,
    'Did the bishops who signed the Nicene formula in 325 share a single determinate understanding of homoousios that ruled out subordinationist compatibility, or was the term''s meaning genuinely under-specified and contested from the outset, making the subordinationist reading a live original possibility rather than a later corruption?',
    'Close philological and historical analysis of the conciliar minutes, contemporary correspondence (Eusebius of Caesarea''s letter to his own church explaining his signature), and comparison with how homoousios was used in prior non-Nicene theological contexts (e.g., by Paul of Samosata, whom earlier synods condemned for a different use of the term).',
    'If the term was genuinely indeterminate in 325, the subordinationist reading has a stronger claim to legitimate descent from the original conciliar act, weakening the conciliar authority seat''s claim that Nicaea itself foreclosed subordination. If the term had a determinate anti-subordinationist sense from the start, the subordinationist reading is better characterized as a later reinterpretation riding on ambiguous later transmission, strengthening the case for classifying this reading''s institutional victories as closer to snare than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy_at_nicaea, empirical, 'Whether homoousios was originally determinate against subordination or genuinely open at Nicaea in 325.').

omega_variable(
    committer_structure_scriptural_vs_conciliar_authority,
    'This reading distributes interpretive authority toward direct scriptural exegesis and away from conciliar/traditional authority as the arbiter of doctrinal meaning. Is this authority-relocation itself part of what makes the reading attractive to its beneficiary communities, independent of the exegetical merits of subordination itself?',
    'Compare the argumentative structure used by subordinationist bishops (Eusebius of Nicomedia, Ulfilas) against contemporaneous Nicene polemics: does the subordinationist case rest primarily on textual/exegetical argument, or substantially on delegitimizing conciliar authority as such (i.e., is authority-relocation the load-bearing move)?',
    'If authority-relocation does substantial independent work, the subordinationist reading is partly a vehicle for a broader anti-conciliar authority claim, which would push the classification toward a more extraction-weighted reading (the coordination story is partly cover for an authority contest). If the exegetical argument stands substantially on its own, the reading''s coordination function (solving a genuine scriptural tension) is more load-bearing and independent of the authority question.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_structure_scriptural_vs_conciliar_authority, conceptual, 'Whether the reading''s appeal is separable from its effect of relocating doctrinal authority away from councils.').

omega_variable(
    sibling_reading_boundary_honorific_vs_subordinationist,
    'Where exactly does the honorific_similarity_reading (homoiousios, ''like essence'') end and this subordinationist_reading (homoousios compatible with derivation/subordination) begin? Fourth-century sources themselves show significant sliding between homoian, homoiousian, and strict Arian positions.',
    'A dedicated comparative reading of Basil of Ancyra''s homoiousian formula against the Homoian formula adopted at the Council of Constantinople (360) would locate the structural boundary — specifically whether ''like in essence'' is offered as a weaker ontological claim than ''same essence with derivation'' or as a rhetorically softer version of the same underlying subordinationist commitment.',
    'If the boundary is genuinely fuzzy in the historical record, this story and honorific_similarity_reading may need to be treated as adjacent points on a spectrum rather than two cleanly disjoint constraints, which would affect how their network edges and beneficiary overlaps should be modeled.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_boundary_honorific_vs_subordinationist, conceptual, 'Structural boundary ambiguity between the subordinationist and honorific-similarity kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_nicene__subordinationist_reading, 325, 451).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_tr_t325, homoousios_nicene__subordinationist_reading, theater_ratio, 325, 0.2).
narrative_ontology:measurement(homo_tr_t350, homoousios_nicene__subordinationist_reading, theater_ratio, 350, 0.3).
narrative_ontology:measurement(homo_tr_t360, homoousios_nicene__subordinationist_reading, theater_ratio, 360, 0.38).
narrative_ontology:measurement(homo_tr_t381, homoousios_nicene__subordinationist_reading, theater_ratio, 381, 0.35).
narrative_ontology:measurement(homo_tr_t410, homoousios_nicene__subordinationist_reading, theater_ratio, 410, 0.42).
narrative_ontology:measurement(homo_tr_t451, homoousios_nicene__subordinationist_reading, theater_ratio, 451, 0.4).

% Extraction over time
narrative_ontology:measurement(homo_be_t325, homoousios_nicene__subordinationist_reading, base_extractiveness, 325, 0.35).
narrative_ontology:measurement(homo_be_t350, homoousios_nicene__subordinationist_reading, base_extractiveness, 350, 0.5).
narrative_ontology:measurement(homo_be_t360, homoousios_nicene__subordinationist_reading, base_extractiveness, 360, 0.66).
narrative_ontology:measurement(homo_be_t381, homoousios_nicene__subordinationist_reading, base_extractiveness, 381, 0.7).
narrative_ontology:measurement(homo_be_t410, homoousios_nicene__subordinationist_reading, base_extractiveness, 410, 0.58).
narrative_ontology:measurement(homo_be_t451, homoousios_nicene__subordinationist_reading, base_extractiveness, 451, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(homo_su_t325, homoousios_nicene__subordinationist_reading, suppression_requirement, 325, 0.4).
narrative_ontology:measurement(homo_su_t350, homoousios_nicene__subordinationist_reading, suppression_requirement, 350, 0.55).
narrative_ontology:measurement(homo_su_t360, homoousios_nicene__subordinationist_reading, suppression_requirement, 360, 0.68).
narrative_ontology:measurement(homo_su_t381, homoousios_nicene__subordinationist_reading, suppression_requirement, 381, 0.75).
narrative_ontology:measurement(homo_su_t410, homoousios_nicene__subordinationist_reading, suppression_requirement, 410, 0.6).
narrative_ontology:measurement(homo_su_t451, homoousios_nicene__subordinationist_reading, suppression_requirement, 451, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_nicene__subordinationist_reading, identity_coordination).
narrative_ontology:affects_constraint(homoousios_nicene__subordinationist_reading, homoousios_nicene__metaphysical_equality_reading).
narrative_ontology:affects_constraint(homoousios_nicene__subordinationist_reading, homoousios_nicene__honorific_similarity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling stories decomposing the natural-language label 'the homoousios controversy' per the epsilon-invariance principle: subordinationist_reading (this file), metaphysical_equality_reading, and honorific_similarity_reading. Each reading has a structurally distinct beneficiary/victim set and its own epsilon, because 'what homoousios means' resolves to different real-world winners and losers depending on which reading held institutional power at a given moment. The three form a kernel family under homoousios_nicene; all three must cross-link via affects_constraints. This reading historically influenced the honorific_similarity_reading's rise as a mediating compromise (homoiousian bishops sought a middle path between this reading and the equality reading) and stood in direct tension with the metaphysical_equality_reading that eventually prevailed at Constantinople 381.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(homoousios_nicene__subordinationist_reading, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
