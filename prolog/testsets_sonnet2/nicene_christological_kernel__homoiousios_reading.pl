% ============================================================================
% CONSTRAINT STORY: nicene_christological_kernel__homoiousios_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_christological_kernel__homoiousios_reading, []).

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
 *   constraint_id: nicene_christological_kernel__homoiousios_reading
 *   human_readable: Homoiousian Christological Reading (Similar-Substance Formula)
 *   domain: religious/ecclesiastical
 *
 * SUMMARY:
 *   This story authors ONLY the homoiousios ('similar substance') reading of
 *   the fourth-century Christological kernel — the position associated with
 *   the 'Homoiousian' or semi-Arian party at councils such as Ancyra (358)
 *   and Seleucia (359), which held that the Son is of like, but not
 *   identical, substance with the Father. This reading gained substantial
 *   imperial backing under Constantius II as a compromise formula intended to
 *   defuse the harder Nicene (homoousios) and Anomoean (heteroousios, 'unlike
 *   substance') extremes. The sibling reading — homoousios, full ontological
 *   identity — is a separate constraint story with its own extractiveness
 *   profile and stakeholder set. The two readings are NOT averaged here; this
 *   file's ε (0.46) describes only the homoiousian arrangement as it actually
 *   operated: a moderate-extraction, actively-enforced doctrinal formula that
 *   benefited regional episcopal autonomy and subordinationist exegesis at
 *   the cost of imperial doctrinal unity and the homoousian party's settled
 *   position.
 *
 * KEY AGENTS:
 *   - regional_eastern_bishops: primary beneficiary/agenda_setter (institutional/constrained) — gains exegetical latitude and local authority
 *   - imperial_unity_project: primary payer (institutional/trapped) — bears the cost of fragmented consensus
 *   - nicene_homoousian_clergy: secondary payer (organized/constrained) — bears renewed controversy and depositions
 *   - laity_under_doctrinal_flux: diffuse payer (powerless/trapped) — bears confusion and instability with no voice
 *   - constantius_ii_and_court_theologians: secondary agenda_setter (institutional/arbitrage) — can shift patronage between formulas
 *   - later_church_historians: analytical observer — sees the full contest retrospectively
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_christological_kernel__homoiousios_reading, 0.46).
domain_priors:suppression_score(nicene_christological_kernel__homoiousios_reading, 0.38).
domain_priors:theater_ratio(nicene_christological_kernel__homoiousios_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_christological_kernel__homoiousios_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoiousios_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoiousios_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_christological_kernel__homoiousios_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoiousios_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_christological_kernel__homoiousios_reading, tangled_rope).
narrative_ontology:human_readable(nicene_christological_kernel__homoiousios_reading, "Homoiousian Christological Reading (Similar-Substance Formula)").
narrative_ontology:topic_domain(nicene_christological_kernel__homoiousios_reading, "religious/ecclesiastical").

domain_priors:requires_active_enforcement(nicene_christological_kernel__homoiousios_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_christological_kernel__homoiousios_reading, '7e8284eb-00f2-4052-a8d7-2821c122d039').
narrative_ontology:cs_kernel_codification('7e8284eb-00f2-4052-a8d7-2821c122d039', formalized).
narrative_ontology:cs_authority_grounding('7e8284eb-00f2-4052-a8d7-2821c122d039', lineage).
narrative_ontology:cs_interpretation_layer_present('7e8284eb-00f2-4052-a8d7-2821c122d039').
narrative_ontology:cs_reading_relation('7e8284eb-00f2-4052-a8d7-2821c122d039', nicene_christological_kernel__homoousios_reading, coexists_with).
narrative_ontology:cs_axiom('7e8284eb-00f2-4052-a8d7-2821c122d039', foundational, ontological_distinction_required_for_monotheistic_clarity).
narrative_ontology:cs_axiom_status(ontological_distinction_required_for_monotheistic_clarity, holdable).
narrative_ontology:cs_axiom_grounding('7e8284eb-00f2-4052-a8d7-2821c122d039', ontological_distinction_required_for_monotheistic_clarity, deontological).
narrative_ontology:cs_axiom('7e8284eb-00f2-4052-a8d7-2821c122d039', secondary, regional_synodal_authority_sufficient_for_credal_ratification).
narrative_ontology:cs_axiom_status(regional_synodal_authority_sufficient_for_credal_ratification, holdable).
narrative_ontology:cs_axiom_grounding('7e8284eb-00f2-4052-a8d7-2821c122d039', regional_synodal_authority_sufficient_for_credal_ratification, conventional).
narrative_ontology:cs_reference_frame('7e8284eb-00f2-4052-a8d7-2821c122d039', nicene_325_homoousios_settlement).
narrative_ontology:cs_drift_state('7e8284eb-00f2-4052-a8d7-2821c122d039', sirmium_ancyra_seleucia_councils_350s, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('7e8284eb-00f2-4052-a8d7-2821c122d039', '').
narrative_ontology:cs_kernel_id(nicene_christological_kernel__homoiousios_reading, nicene_christological_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoiousios_reading, regional_eastern_bishops).
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoiousios_reading, subordinationist_exegetes).
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoiousios_reading, provincial_synods).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoiousios_reading, imperial_unity_project).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoiousios_reading, nicene_homoousian_clergy).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoiousios_reading, laity_under_doctrinal_flux).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convene regional synods (Ancyra, Seleucia) to press the homoiousios formula as a mediating position between full ontological identity and outright subordination. They administer their sees' catechesis and liturgy according to this reading, and gain doctrinal room to preserve a distinction between Father and Son they regard as necessary for coherent monotheism. Their exit from imperial doctrinal consensus is constrained by dependence on imperial patronage and councils, but they retain local liturgical and administrative autonomy.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, regional_eastern_bishops, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(nicene_christological_kernel__homoiousios_reading, regional_eastern_bishops, beneficiary).

% Theologians and catechists whose scriptural readings (emphasizing passages implying the Son's derivation from the Father) are vindicated by the homoiousios formula rather than suppressed as heretical. They gain intellectual and pastoral legitimacy but remain dependent on episcopal patronage for teaching posts and manuscript circulation.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, subordinationist_exegetes, beneficiary,
    moderate, biographical, constrained, regional).

% Bodies of regional clergy who convene to ratify creeds independent of a single universally-binding imperial formula. The homoiousios reading gives them room to issue their own credal statements without being immediately anathematized, at the cost of standing outside a single empire-wide communion.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, provincial_synods, beneficiary,
    organized, generational, constrained, regional).

% The imperial administration (from Constantius II onward) that sought a single creed to bind the church as an instrument of political cohesion across the empire. Every credal fragmentation, including the rise of a homoiousian middle party, undercuts the single-formula strategy the state invested in at Nicaea; the empire cannot simply exit the problem, since religious unity was treated as load-bearing for imperial legitimacy.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, imperial_unity_project, payer,
    institutional, civilizational, trapped, continental).

% Bishops and theologians (Athanasius and allies) committed to the full ontological identity of Father and Son. The homoiousios formula, by preserving real ontological distinction, is read by them as reopening the subordinationist door Nicaea (325) was convened to close; they bear the cost of renewed controversy, repeated depositions, exiles, and the erosion of a settlement they had believed final. Their exit option is limited to continued conciliar and polemical contest, since abandoning the homoousios claim would concede what they regard as the substance of the faith.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, nicene_homoousian_clergy, payer,
    organized, generational, constrained, continental).

% Ordinary believers whose local bishop's creed may shift between homoousian and homoiousian formulas depending on imperial favor and synodal outcome, sometimes within a single lifetime. They bear the confusion, occasional excommunication of familiar clergy, and loss of a stable communal identity, with essentially no capacity to influence which formula governs their diocese.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, laity_under_doctrinal_flux, payer,
    powerless, biographical, trapped, regional).

% The emperor and his ecclesiastical advisors who, for a period, promoted homoiousian and related compromise formulas as instruments of court-manageable unity, convening councils (Sirmium, Seleucia) to engineer consensus. They can shift patronage between formulas as political circumstance changes, giving them far more room to maneuver than any single bishop or lay believer.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, constantius_ii_and_court_theologians, agenda_setter,
    institutional, biographical, arbitrage, continental).

% Scholars examining the fourth-century credal controversies retrospectively, reconstructing which formula served which faction's interests and how the eventual Nicene-Constantinopolitan settlement (381) resolved — provisionally — the contest in favor of the homoousian reading.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, later_church_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nicene_christological_kernel__homoiousios_reading, regional_eastern_bishops).
narrative_ontology:fixing_cost_class(nicene_christological_kernel__homoiousios_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a credal formula that lets regional churches affirm a real, felt distinction between Father and Son — addressing a genuine pastoral and philosophical worry that pure ontological identity collapses into modalism or compromises monotheistic clarity — without requiring wholesale adoption of the more totalizing homoousios formula backed by imperial and Alexandrian authority.
% TRANSFER_FUNCTION: Moves doctrinal authority and exegetical legitimacy from a single imperially-enforced center (Nicaea's homoousios settlement and its Athanasian defenders) toward regional episcopal authorities and provincial synods; correspondingly shifts the cost of unresolved controversy onto imperial unity efforts and onto ordinary believers who must navigate shifting local creeds.
% ABSENT_VOICES: The laity whose communion depends on whichever creed their bishop currently holds are almost entirely absent from the councils that produce and revise the formula; Western Latin-speaking bishops, largely committed to a Tertullian-derived one-substance vocabulary, are also underrepresented in the Greek-language homoiousian debates that produced this reading.
% DISAPPEARANCE_RATIONALE: If the homoiousios reading had never gained synodal traction, the mid-fourth-century councils (Sirmium, Ancyra, Seleucia) that ratified regional compromise creeds would have had no textual anchor, imperial religious policy under Constantius II would have lacked its preferred middle path, and the eventual settlement at Constantinople in 381 would have had one fewer competing formula to define itself against — the shape of the fourth-century controversy, and arguably the eventual wording of the Nicene-Constantinopolitan creed, would differ.
% FOUNDING_PROBLEM: How to preserve a monotheistic ontology — one God, not two — while still explaining scriptural language (the Son 'sent by' the Father, subject to the Father, praying to the Father) that implies some real distinction of being, without collapsing into either strict subordination (the Son as a lesser created being) or an undifferentiated identity that erases the Father-Son relation altogether.
% FOUNDING_PROBLEM_CORROBORATION: Homoiousian bishops themselves (Basil of Ancyra, George of Laodicea) attest the problem as live: monotheistic coherence requires some ontological distinction. Athanasian and later Cappadocian sources — writing from outside the homoiousian party, and eventually vindicated by the 381 settlement — attest that the 'problem' as posed by the homoiousian party was substantially a restatement of subordinationism dressed in moderate vocabulary, and that the genuine philosophical worry (avoiding modalism) had already been addressed by homoousian theologians distinguishing ousia from hypostasis. No fully disinterested corroborating source exists; every attestation comes from a party to the fourth-century contest itself.
narrative_ontology:disappearance_verdict(nicene_christological_kernel__homoiousios_reading, world_rearranges).
narrative_ontology:founding_problem_status(nicene_christological_kernel__homoiousios_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_christological_kernel__homoiousios_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nicene_christological_kernel__homoiousios_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nicene_christological_kernel__homoiousios_reading, 0.46, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_christological_kernel__homoiousios_reading_tests).
:- end_tests(nicene_christological_kernel__homoiousios_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.46 — moderate, not low and not severe — because the homoiousios formula genuinely solves a felt theological problem (preserving Father-Son distinction against modalist collapse) for the regional bishops and exegetes who benefit from it, while imposing real, measurable costs on imperial unity efforts and on the homoousian party who must re-fight settled ground. Suppression (0.38) is lower than extraction because the homoiousian party did not, for most of the interval, possess the coercive machinery of imperial anathema in the way the eventual 381 settlement would deploy against non-Nicene positions — its persistence depended more on synodal and episcopal maneuvering than on organized suppression of dissent. Theater ratio (0.28) reflects that a meaningful share of conciliar activity (Sirmium, Seleucia) was substantive doctrinal contest rather than pure performance, though some council activity was already becoming more about political positioning under Constantius than genuine adjudication by the later years of the interval — hence the measured rise and slight fall in theater_ratio across the grid.
 *
 * PERSPECTIVAL GAP:
 *   From the regional bishops' seat, the homoiousios formula reads as principled theological moderation — coordination around a shared concern (preserving real distinction) rather than extraction. From the homoousian clergy's seat and from the imperial unity project's seat, the same formula reads as a destabilizing wedge that reopens settled controversy and threatens both doctrinal integrity and political cohesion. The engine computes these as structurally different seat-classifications from the same authored data; this story does not adjudicate which seat is 'right' — only the homoousios sibling story would author that contest from the other side.
 *
 * DIRECTIONALITY LOGIC:
 *   Regional eastern bishops and subordinationist exegetes sit near the beneficiary end: the formula validates their preferred exegesis and expands local doctrinal authority relative to a single imperially-enforced homoousian standard. The imperial unity project and nicene homoousian clergy sit near the target end: the former because doctrinal fragmentation directly undercuts the political-religious cohesion the empire invested in since Constantine, the latter because the homoiousios reading reopens a controversy they regarded Nicaea (325) as having closed, forcing renewed polemical and conciliar labor. Laity are the most fully trapped payers — they have no synodal voice and simply inherit whichever formula their bishop currently holds, with no meaningful exit from their local ecclesial community.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how to preserve monotheistic coherence while honoring scriptural language of distinction — was live and genuine at the time the formula emerged (per founding_problem_status: contested). Reading this constraint as tangled_rope rather than pure snare or pure rope reflects that the coordination function (a real philosophical worry, addressed for a real community of believers) coexisted with asymmetric extraction (regional autonomy purchased at the cost of imperial cohesion and the homoousian party's prior settlement). Classifying it as pure extraction would erase the genuine theological stakes the homoiousian party believed it was defending; classifying it as pure coordination would erase the real institutional damage to unity and the coercive synodal maneuvering (Sirmium 357's forced subscriptions) that accompanied its imperial-backed phases.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    distinction_vs_subordination_ambiguity,
    'Does the homoiousios formula represent a coherent, non-heretical attempt to preserve real Trinitarian distinction, or is it structurally indistinguishable from a moderated subordinationism that the broader tradition would eventually reject?',
    'Close textual comparison of homoiousian conciliar statements (Ancyra 358, Sirmium 358) against both Arian/heteroousian formulas and the eventual Nicene-Constantinopolitan (381) settlement''s ousia/hypostasis distinction, assessing whether homoiousios occupies genuinely distinct conceptual space or collapses into a spectrum point on the subordinationist side.',
    'If genuinely distinct and philosophically motivated, the constraint''s coordination function is stronger and extractiveness should be read as lower; if effectively a moderated subordinationism, the coordination story is closer to cover for regional episcopal autonomy-seeking, and extractiveness is understated here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distinction_vs_subordination_ambiguity, conceptual, 'Whether homoiousios is a distinct theological position or a moderated subordinationism.').

omega_variable(
    imperial_instrumentalization_ambiguity,
    'To what extent was the homoiousian formula''s imperial backing (under Constantius II) driven by genuine theological conviction at court versus pure political utility in managing a fractious episcopate?',
    'Comparative analysis of imperial correspondence and conciliar acta (Sirmium, Seleucia) for evidence of doctrinal reasoning versus political calculation in the shifts of imperial religious policy across the 350s.',
    'If primarily instrumental, the imperial_unity_project''s status as victim is complicated — the empire itself created and then was harmed by the fragmentation it had instrumentally encouraged, which would sharpen the tangled_rope reading (self-inflicted extraction) rather than framing the empire as a pure external payer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imperial_instrumentalization_ambiguity, empirical, 'Whether imperial support for the formula was theological or purely political.').

omega_variable(
    kernel_framing_under_determination,
    'Is the nicene_christological_kernel best framed as a contest between fixed doctrinal propositions (homoousios vs. homoiousios as static claims), or as a contest between the authority structures that would enforce each proposition (Alexandrian/imperial center vs. regional Eastern episcopate)?',
    'Compare the doctrinal-content framing against an authority-structure framing by tracing whether shifts in imperial favor changed which bishops held power without changing the underlying theological arguments, versus whether theological argument itself drove the shifts.',
    'Under the doctrinal-content framing (adopted here), this story''s cs_pattern centers on axiom content (distinction vs. identity). Under an authority-structure framing, the same historical episode would be read primarily as a contest over WHO holds interpretive authority, with doctrinal content as secondary — potentially shifting classification toward pure power contest with theological content as post-hoc justification. This story adopts the doctrinal-content framing because the councils'' own acta foreground substance-language disputes, not explicit jurisdictional claims.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_under_determination, conceptual, 'Alternative framings of the kernel: doctrinal content versus authority structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_christological_kernel__homoiousios_reading, 340, 381).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nice_tr_t340, nicene_christological_kernel__homoiousios_reading, theater_ratio, 340, 0.15).
narrative_ontology:measurement(nice_tr_t347, nicene_christological_kernel__homoiousios_reading, theater_ratio, 347, 0.19).
narrative_ontology:measurement(nice_tr_t353, nicene_christological_kernel__homoiousios_reading, theater_ratio, 353, 0.24).
narrative_ontology:measurement(nice_tr_t358, nicene_christological_kernel__homoiousios_reading, theater_ratio, 358, 0.29).
narrative_ontology:measurement(nice_tr_t365, nicene_christological_kernel__homoiousios_reading, theater_ratio, 365, 0.31).
narrative_ontology:measurement(nice_tr_t373, nicene_christological_kernel__homoiousios_reading, theater_ratio, 373, 0.3).
narrative_ontology:measurement(nice_tr_t381, nicene_christological_kernel__homoiousios_reading, theater_ratio, 381, 0.28).

% Extraction over time
narrative_ontology:measurement(nice_be_t340, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 340, 0.3).
narrative_ontology:measurement(nice_be_t347, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 347, 0.34).
narrative_ontology:measurement(nice_be_t353, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 353, 0.39).
narrative_ontology:measurement(nice_be_t358, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 358, 0.44).
narrative_ontology:measurement(nice_be_t365, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 365, 0.48).
narrative_ontology:measurement(nice_be_t373, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 373, 0.47).
narrative_ontology:measurement(nice_be_t381, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 381, 0.46).

% Suppression requirement over time
narrative_ontology:measurement(nice_su_t340, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 340, 0.22).
narrative_ontology:measurement(nice_su_t347, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 347, 0.28).
narrative_ontology:measurement(nice_su_t353, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 353, 0.35).
narrative_ontology:measurement(nice_su_t358, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 358, 0.42).
narrative_ontology:measurement(nice_su_t365, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 365, 0.4).
narrative_ontology:measurement(nice_su_t373, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 373, 0.39).
narrative_ontology:measurement(nice_su_t381, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 381, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_christological_kernel__homoiousios_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(nicene_christological_kernel__homoiousios_reading, 0.1).
narrative_ontology:affects_constraint(nicene_christological_kernel__homoiousios_reading, nicene_christological_kernel__homoousios_reading).

% DUAL FORMULATION NOTE:
% This story and nicene_christological_kernel__homoousios_reading decompose the natural-language label 'the fourth-century Christological controversy' into two structurally distinct readings of a single contested kernel, per the ε-invariance principle. The homoousios reading (full ontological identity) is the eventual Nicene-Constantinopolitan (381) settlement position: lower measured extractiveness in its own file, benefiting imperial unity and settled Alexandrian authority, victimizing regional exegetical pluralism. The homoiousios reading (this file) inverts the beneficiary/victim structure: benefits regional autonomy, victimizes imperial cohesion and the homoousian party's prior settlement. Both readings share the same kernel_id (nicene_christological_kernel) but are authored as separate files with independent ε, stakeholders, and classification, linked via affects_constraints rather than merged or averaged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
