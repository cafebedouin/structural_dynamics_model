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
 *   human_readable: Homoousios as Full Ontological Equality (Nicene-Constantinopolitan Reading)
 *   domain: historical theology / ecclesiastical history / philosophy of religion
 *
 * SUMMARY:
 *   This story authors the metaphysical-equality reading of homoousios: the
 *   claim, ratified at Nicaea (325) and reaffirmed and hardened at
 *   Constantinople (381), that homoousios secures full ontological equality
 *   between Father and Son — same divine essence (ousia), co-eternal, with no
 *   subordination in being. This reading is one of three structurally
 *   distinct claims that shelter under the single label 'the homoousios
 *   controversy' or 'the Nicene settlement'; the other two — that homoousios
 *   signifies mere likeness (honorific_similarity_reading) and that
 *   homoousios is compatible with derivational subordination
 *   (subordinationist_reading) — are separate constraint stories with their
 *   own ε, beneficiaries, and victim sets, linked via
 *   network.affects_constraints. The extraction trajectory here tracks a real
 *   historical arc: the term itself was ambiguous and contested for decades
 *   after 325 (extraction relatively low, contest genuinely live, homoian
 *   formulas held imperial favor under Constantius II), then hardened sharply
 *   after 381 when Theodosius I made Nicene orthodoxy the sole legally
 *   recognized Christianity of the empire and extended anathema and civil
 *   penalty to dissenters — extraction and suppression both jump at that
 *   point and remain elevated through Chalcedon (451), which structurally
 *   presupposes this reading.
 *
 * KEY AGENTS:
 *   - nicene_episcopal_hierarchy: agenda_setter (institutional/arbitrage) — drafts and enforces the equality formula
 *   - imperial_church_authority: agenda_setter/beneficiary (institutional/arbitrage) — backs enforcement with state power
 *   - athanasian_theological_faction: beneficiary (organized/mobile) — builds career and canonical authority on this reading
 *   - arian_clergy: payer (moderate/trapped) — deposed and anathematized
 *   - homoian_congregations: payer (powerless/trapped) — retroactively reclassified as heretical
 *   - subordinationist_theologians: payer (powerless/trapped) — silenced, writings destroyed
 *   - gothic_arian_christians: payer (moderate/constrained) — generational friction until eventual conversion
 *   - later_church_historians: observer (analytical/analytical) — reconstructs the contest behind the settlement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_nicene__metaphysical_equality_reading, 0.62).
domain_priors:suppression_score(homoousios_nicene__metaphysical_equality_reading, 0.81).
domain_priors:theater_ratio(homoousios_nicene__metaphysical_equality_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_nicene__metaphysical_equality_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_nicene__metaphysical_equality_reading, "Homoousios as Full Ontological Equality (Nicene-Constantinopolitan Reading)").
narrative_ontology:topic_domain(homoousios_nicene__metaphysical_equality_reading, "historical theology / ecclesiastical history / philosophy of religion").

domain_priors:requires_active_enforcement(homoousios_nicene__metaphysical_equality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_nicene__metaphysical_equality_reading, 'a0c607cd-eed6-429c-860a-e3bf1a542669').
narrative_ontology:cs_kernel_codification('a0c607cd-eed6-429c-860a-e3bf1a542669', formalized).
narrative_ontology:cs_authority_grounding('a0c607cd-eed6-429c-860a-e3bf1a542669', lineage).
narrative_ontology:cs_interpretation_layer_present('a0c607cd-eed6-429c-860a-e3bf1a542669').
narrative_ontology:cs_reading_relation('a0c607cd-eed6-429c-860a-e3bf1a542669', homoousios_nicene__subordinationist_reading, forecloses).
narrative_ontology:cs_reading_relation('a0c607cd-eed6-429c-860a-e3bf1a542669', homoousios_nicene__honorific_similarity_reading, forecloses).
narrative_ontology:cs_axiom('a0c607cd-eed6-429c-860a-e3bf1a542669', foundational, single_undivided_divine_essence).
narrative_ontology:cs_axiom_status(single_undivided_divine_essence, holdable).
narrative_ontology:cs_axiom_grounding('a0c607cd-eed6-429c-860a-e3bf1a542669', single_undivided_divine_essence, deontological).
narrative_ontology:cs_axiom('a0c607cd-eed6-429c-860a-e3bf1a542669', foundational, no_ontological_subordination_in_being).
narrative_ontology:cs_axiom_status(no_ontological_subordination_in_being, holdable).
narrative_ontology:cs_axiom_grounding('a0c607cd-eed6-429c-860a-e3bf1a542669', no_ontological_subordination_in_being, conventional).
narrative_ontology:cs_reference_frame('a0c607cd-eed6-429c-860a-e3bf1a542669', nicene_constantinopolitan_creedal_settlement).
narrative_ontology:cs_drift_state('a0c607cd-eed6-429c-860a-e3bf1a542669', post_chalcedonian_reception, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('a0c607cd-eed6-429c-860a-e3bf1a542669', '').
narrative_ontology:cs_kernel_id(homoousios_nicene__metaphysical_equality_reading, homoousios_nicene).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_nicene__metaphysical_equality_reading, nicene_episcopal_hierarchy).
narrative_ontology:constraint_beneficiary(homoousios_nicene__metaphysical_equality_reading, imperial_church_authority).
narrative_ontology:constraint_beneficiary(homoousios_nicene__metaphysical_equality_reading, athanasian_theological_faction).
narrative_ontology:constraint_victim(homoousios_nicene__metaphysical_equality_reading, arian_clergy).
narrative_ontology:constraint_victim(homoousios_nicene__metaphysical_equality_reading, homoian_congregations).
narrative_ontology:constraint_victim(homoousios_nicene__metaphysical_equality_reading, subordinationist_theologians).
narrative_ontology:constraint_victim(homoousios_nicene__metaphysical_equality_reading, gothic_arian_christians).
narrative_ontology:constraint_vindicates(homoousios_nicene__metaphysical_equality_reading, trinitarian_co_equality_doctrine).
narrative_ontology:constraint_vindicates(homoousios_nicene__metaphysical_equality_reading, single_divine_essence_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bishops aligned with the Nicene formula (Athanasius and successors) control conciliar proceedings, draft the creedal language, and after 381 secure imperial backing to enforce it as the boundary of orthodox communion. They administer excommunication and exile against dissenting clergy and hold the interpretive keys to what 'same essence' means in practice.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, nicene_episcopal_hierarchy, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Roman and later Byzantine emperors from Constantine through Theodosius I use creedal uniformity as an instrument of imperial cohesion, convening councils, exiling bishops who dissent, and eventually criminalizing Arian worship. Religious unity underwrites political unity; enforcement machinery (councils, decrees, banishment) is state-backed.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, imperial_church_authority, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_nicene__metaphysical_equality_reading, imperial_church_authority, beneficiary).

% Theologians and monastic networks who built their careers, sees, and doctrinal authority on defending strict ontological equality. They gain ecclesiastical office, textual authority, and lasting canonical status; their theological program becomes the only one transmissible as orthodoxy.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, athanasian_theological_faction, beneficiary,
    organized, civilizational, mobile, continental).

% Presbyters and bishops holding that the Son is generated and therefore not co-equal in being are deposed, exiled (Arius himself repeatedly), and anathematized. Continuing to teach the subordinationist position after Nicaea/Constantinople costs them office, communion, and often physical safety; there is no institutional channel left to hold the position openly within the imperial church.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, arian_clergy, payer,
    moderate, biographical, trapped, continental).

% Lay communities and clergy who held the compromise 'like the Father' (homoian) formula, dominant under Constantius II, find themselves reclassified as heretical once the Nicene reading is re-imposed under Theodosius I. Their prior good standing evaporates retroactively; they must recant, convert, or lose access to sacraments and civil legal protections tied to orthodox status.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, homoian_congregations, payer,
    powerless, generational, trapped, regional).

% Individual thinkers who continue to argue the Son derives being from the Father cannot publish, teach, or hold clerical office within imperial Christianity after the ontological-equality reading is codified as law under Theodosius's edicts. Their writings are targeted for destruction; exit means either recantation or exile to communities outside imperial reach.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, subordinationist_theologians, payer,
    powerless, biographical, trapped, regional).

% Germanic peoples converted to Christianity under an Arian/homoian missionary tradition (via Ulfilas) retain a subordinationist Christology for generations after Nicene orthodoxy hardens in the empire. They face religious-political friction with Nicene populations and rulers upon settling within or bordering the former empire, eventually converting to Nicene Christianity under sustained pressure (e.g., Visigothic conversion at the Third Council of Toledo).
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, gothic_arian_christians, payer,
    moderate, generational, constrained, continental).

% Scholars examining the councils' proceedings, letters, and creedal texts to reconstruct which positions were live options before the settlement hardened, and how much of the 'equality' reading's dominance reflects theological argument versus imperial coercion.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, later_church_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(homoousios_nicene__metaphysical_equality_reading, diffuse).
narrative_ontology:fixing_cost_class(homoousios_nicene__metaphysical_equality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the imperial church with a single, non-negotiable Christological formula that stabilizes liturgy, communion boundaries, and doctrinal transmission across a fractious, geographically dispersed set of sees — without a shared creedal boundary, communion fragments along regional and political lines.
% TRANSFER_FUNCTION: Moves interpretive authority, clerical office, and doctrinal legitimacy from subordinationist and compromise factions to the Nicene-Athanasian faction and the episcopal-imperial alliance that backs it; moves the cost of noncompliance (deposition, exile, loss of legal standing) onto clergy and congregations holding alternative Christologies.
% ABSENT_VOICES: Arius and his continuing sympathizers were present at Nicaea itself but were a minority voice overridden by vote and then structurally excluded from all subsequent redrafting; homoian and Gothic Arian communities had no seat at the councils that reclassified them as heretical decades after their own formula had briefly held imperial favor.
% DISAPPEARANCE_RATIONALE: If the metaphysical-equality reading were not enforced as the boundary of communion, the fourth-century church fragments along the Nicene/homoian/subordinationist lines that were live simultaneously for most of the century; clerical office, imperial religious policy, and the shape of later Christian metaphysics (including Chalcedonian Christology, which presupposes Nicene equality) would need to be rebuilt on a different foundation.
% FOUNDING_PROBLEM: The church needed a way to resolve the Arian controversy — is the Son a creature, however exalted, or does he share the Father's own uncreated being? — because the answer determined whether Christ's mediation and worship were coherent, and disagreement was fracturing communion across the empire.
% FOUNDING_PROBLEM_CORROBORATION: Nicene and later Chalcedonian churches attest the problem was genuinely resolved and remains theologically live and settled. Independent historians of late antiquity (outside all confessional beneficiary groups) attest that the 'resolution' tracked imperial political consolidation as much as theological argument — the homoian position held imperial favor for decades before losing it, which is difficult to explain as pure doctrinal discovery; some historians of religion also note the underlying question (how divine unity and plurality relate) remains genuinely unsettled rather than closed by the vote at Constantinople.
narrative_ontology:disappearance_verdict(homoousios_nicene__metaphysical_equality_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_nicene__metaphysical_equality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_nicene__metaphysical_equality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(homoousios_nicene__metaphysical_equality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_nicene__metaphysical_equality_reading, 0.62, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.62 at interval end) reflects that once the equality reading is codified as imperial law, holding an alternative Christology carries real costs — office, communion, legal standing, sometimes safety — that are not symmetric to any cost borne by the beneficiary faction. Suppression (0.81) is authored higher than extraction because the mechanism securing the reading's dominance after 381 is substantially coercive: imperial edict, exile, and eventually criminalization of Arian worship, not merely theological persuasion. Accessibility collapse (0.7) is high but not maximal — subordinationist and homoian Christianity persisted for centuries outside imperial reach (notably among Germanic peoples) even as it collapsed within the empire's legal and ecclesiastical boundaries. Resistance (0.6) is substantial and historically documented: homoian ascendancy under Constantius II, continued Gothic Arianism for generations, and repeated conciliar reversals before 381 show this was not an uncontested natural settlement.
 *
 * DIRECTIONALITY LOGIC:
 *   The Nicene episcopal hierarchy and the imperial authority that backs it sit at the beneficiary end: they set the terms, administer enforcement, and their theological program becomes the transmissible orthodoxy. Arian clergy, homoian congregations, and subordinationist theologians sit at the target end: trapped exit options, biographical-to-generational time horizons that offer no escape from the consequences, and no institutional channel to hold their position within the empire after 381. Gothic Arian Christians are differentiated by an override-worthy nuance — they are moderate power (a settled, organized peoples with their own political structures) but only constrained (not fully trapped) exit, since their Christology persisted for generations outside direct imperial legal reach before eventual conversion.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how to state coherently that Christ's divinity is not less than the Father's, given that Christian worship and soteriology require it — was and remains a genuine theological question with real doctrinal stakes; this is not a manufactured pretext. But the specific mechanism that came to enforce ONE answer (this reading) as the sole legally permissible one is separable from the question it answers: the homoian formula held the same institutional position under Constantius II that Nicene orthodoxy holds after 381, which shows the enforcement apparatus tracks political alignment as much as settled theological consensus. Classifying this as tangled_rope rather than snare or mountain honors both halves: there is a genuine coordination function (a single creedal boundary stabilizes communion across a fractious empire) AND asymmetric extraction (anathematized clergy pay through the same structure that coordinates everyone else). Classifying it as mountain would launder the coercive history into inevitability; classifying it as pure snare would erase the real theological coordination problem it was addressing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    creedal_precision_vs_political_consolidation,
    'Did the metaphysical-equality reading prevail because it was the theologically correct resolution of a genuine ontological question, or because it aligned with the political interests of the imperial factions that eventually consolidated power (Theodosius I in particular)?',
    'Comparative analysis of the theological argumentation quality on each side against the documented political alignments and shifts in imperial favor (Constantius II''s homoian preference vs. Theodosius I''s Nicene enforcement) — if doctrinal argument quality tracks poorly with which position held imperial favor at a given time, the political-consolidation explanation gains support.',
    'If the reading''s dominance is substantially explained by imperial political consolidation rather than theological argument winning on the merits, the constraint is better read as extraction wearing coordination''s clothing (closer to snare); if theological argument substantially drove eventual consensus independent of imperial politics, the coordination function is more genuine (closer to rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creedal_precision_vs_political_consolidation, conceptual, 'Whether the reading''s historical victory tracks theological merit or imperial political alignment.').

omega_variable(
    kernel_ambiguity_of_homoousios_itself,
    'Was the term homoousios itself, as used at Nicaea in 325, actually intended by its framers to secure the strict metaphysical-identity reading later hardened at Constantinople in 381, or did its meaning shift substantially between the two councils?',
    'Close philological and historical-theological analysis of how homoousios was used and glossed in the 325-381 interval, including the Cappadocian fathers'' clarifying work distinguishing ousia from hypostasis, which was necessary precisely because the original term''s implications were unsettled.',
    'If the term''s meaning was substantially clarified/shifted between 325 and 381 (rather than merely restated), then the metaphysical-equality reading is itself a later interpretive layer on an originally more ambiguous kernel — supporting the honorific_similarity_reading''s claim that the kernel was under-determined at its origin.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_ambiguity_of_homoousios_itself, empirical, 'Whether the strict equality reading was present at Nicaea''s origin or developed through the Cappadocian clarification before Constantinople.').

omega_variable(
    cs_framing_text_vs_authority,
    'Should the kernel be read as the creedal text itself (formalized, fixed at each council) or as the standing authority of ecumenical council as an institution (the deeper commitment that the equality reading depends on for its force)?',
    'Compare classification under each framing: text-framing treats drift as interpretive (Cappadocian clarification, later credal restatements); authority-framing treats drift as a question of which councils count as authoritative and why (a question that itself became contested in later schisms, e.g. over the filioque and conciliar reception).',
    'Under the text framing, this constraint remains cs_structure fixed_text/lineage as authored. Under the authority framing, the constraint would need to account for the prior, deeper question of what makes an ecumenical council binding at all — a question this story brackets by treating conciliar authority as already established.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_text_vs_authority, conceptual, 'Alternative framing: creedal text as kernel vs. conciliar authority itself as the deeper kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_nicene__metaphysical_equality_reading, 325, 451).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_tr_t325, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 325, 0.2).
narrative_ontology:measurement_basis(homo_tr_t325, observed).
narrative_ontology:measurement(homo_tr_t350, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 350, 0.22).
narrative_ontology:measurement_basis(homo_tr_t350, observed).
narrative_ontology:measurement(homo_tr_t361, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 361, 0.18).
narrative_ontology:measurement_basis(homo_tr_t361, observed).
narrative_ontology:measurement(homo_tr_t381, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 381, 0.3).
narrative_ontology:measurement_basis(homo_tr_t381, observed).
narrative_ontology:measurement(homo_tr_t400, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 400, 0.28).
narrative_ontology:measurement_basis(homo_tr_t400, observed).
narrative_ontology:measurement(homo_tr_t425, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 425, 0.27).
narrative_ontology:measurement_basis(homo_tr_t425, observed).
narrative_ontology:measurement(homo_tr_t451, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 451, 0.28).
narrative_ontology:measurement_basis(homo_tr_t451, observed).

% Extraction over time
narrative_ontology:measurement(homo_be_t325, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 325, 0.35).
narrative_ontology:measurement_basis(homo_be_t325, observed).
narrative_ontology:measurement(homo_be_t350, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 350, 0.3).
narrative_ontology:measurement_basis(homo_be_t350, observed).
narrative_ontology:measurement(homo_be_t361, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 361, 0.25).
narrative_ontology:measurement_basis(homo_be_t361, observed).
narrative_ontology:measurement(homo_be_t381, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 381, 0.55).
narrative_ontology:measurement_basis(homo_be_t381, observed).
narrative_ontology:measurement(homo_be_t400, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 400, 0.6).
narrative_ontology:measurement_basis(homo_be_t400, observed).
narrative_ontology:measurement(homo_be_t425, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 425, 0.62).
narrative_ontology:measurement_basis(homo_be_t425, observed).
narrative_ontology:measurement(homo_be_t451, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 451, 0.62).
narrative_ontology:measurement_basis(homo_be_t451, observed).

% Suppression requirement over time
narrative_ontology:measurement(homo_su_t325, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 325, 0.4).
narrative_ontology:measurement_basis(homo_su_t325, observed).
narrative_ontology:measurement(homo_su_t350, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 350, 0.45).
narrative_ontology:measurement_basis(homo_su_t350, observed).
narrative_ontology:measurement(homo_su_t361, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 361, 0.3).
narrative_ontology:measurement_basis(homo_su_t361, observed).
narrative_ontology:measurement(homo_su_t381, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 381, 0.75).
narrative_ontology:measurement_basis(homo_su_t381, observed).
narrative_ontology:measurement(homo_su_t400, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 400, 0.8).
narrative_ontology:measurement_basis(homo_su_t400, observed).
narrative_ontology:measurement(homo_su_t425, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 425, 0.81).
narrative_ontology:measurement_basis(homo_su_t425, observed).
narrative_ontology:measurement(homo_su_t451, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 451, 0.81).
narrative_ontology:measurement_basis(homo_su_t451, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_nicene__metaphysical_equality_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(homoousios_nicene__metaphysical_equality_reading, 0.08).
narrative_ontology:affects_constraint(homoousios_nicene__metaphysical_equality_reading, homoousios_nicene__subordinationist_reading).
narrative_ontology:affects_constraint(homoousios_nicene__metaphysical_equality_reading, homoousios_nicene__honorific_similarity_reading).
narrative_ontology:affects_constraint(homoousios_nicene__metaphysical_equality_reading, chalcedonian_definition_hypostatic_union).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language label 'the homoousios controversy' / 'the Nicene settlement' into structurally distinct readings of a single kernel (homoousios_nicene): metaphysical_equality_reading (this story — full ontological equality, no subordination), subordinationist_reading (Son derives being from Father, shares divinity but not equality), and honorific_similarity_reading (mere likeness, collapsing toward homoiousios). Each reading has its own ε, its own beneficiary/victim structure, and its own classification; they are linked here rather than merged because measuring 'the controversy' under different readings yields different extraction values, which by the ε-invariance principle means they are different constraints, not one constraint viewed three ways. This story also links forward to the Chalcedonian settlement, which structurally presupposes the equality reading of homoousios as a premise for its own Christological formula.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
