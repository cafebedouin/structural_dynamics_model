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
 *   human_readable: Pro-Nicene Homoousios Doctrine (Consubstantiality of Christ with the Father)
 *   domain: historical_theology/ecclesiastical_politics
 *
 * SUMMARY:
 *   This constraint models the pro-Nicene reading of the Christological
 *   kernel: the claim that Christ is homoousios (of identical substance) with
 *   the Father, as codified at the Council of Nicaea (325) and reaffirmed at
 *   Constantinople (381). This is one of three structurally distinct readings
 *   of a single contested kernel over Christ's relationship to the Father;
 *   the arian_reading (subordinationist, created Christ) and
 *   semi_arian_reading (homoiousios, similar substance) are separate
 *   constraints, not alternative measurements of this one. The pro-Nicene
 *   reading is authored here on its own terms: as it operated once adopted as
 *   imperial-ecclesiastical orthodoxy, with its own enforcement apparatus,
 *   beneficiary structure, and extraction profile — not relative to what the
 *   rival readings would have produced.
 *
 * KEY AGENTS:
 *   - nicene_bishops: Primary agenda-setters (institutional/arbitrage) — draft, ratify, and enforce the formula
 *   - constantinian_imperial_court: Co-agenda-setter and beneficiary (institutional/arbitrage) — provides coercive enforcement and gains political unity
 *   - arian_clergy: Primary target (moderate/trapped) — deposed, exiled, anathematized
 *   - gothic_and_vandal_christian_communities: Secondary target (powerless/trapped) — inherit subordinationist Christianity and are later marginalized as Nicene polities dominate
 *   - modern_historical_theologians: Analytical observer — assesses theological versus political drivers of the outcome
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_christology__pro_nicene_reading, 0.62).
domain_priors:suppression_score(homoousios_christology__pro_nicene_reading, 0.78).
domain_priors:theater_ratio(homoousios_christology__pro_nicene_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_christology__pro_nicene_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_christology__pro_nicene_reading, "Pro-Nicene Homoousios Doctrine (Consubstantiality of Christ with the Father)").
narrative_ontology:topic_domain(homoousios_christology__pro_nicene_reading, "historical_theology/ecclesiastical_politics").

domain_priors:requires_active_enforcement(homoousios_christology__pro_nicene_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_christology__pro_nicene_reading, 'a6c81e1c-8b7c-4f45-b670-51124f9ba538').
narrative_ontology:cs_kernel_codification('a6c81e1c-8b7c-4f45-b670-51124f9ba538', formalized).
narrative_ontology:cs_authority_grounding('a6c81e1c-8b7c-4f45-b670-51124f9ba538', extraction).
narrative_ontology:cs_interpretation_layer_present('a6c81e1c-8b7c-4f45-b670-51124f9ba538').
narrative_ontology:cs_reading_relation('a6c81e1c-8b7c-4f45-b670-51124f9ba538', homoousios_christology__arian_reading, forecloses).
narrative_ontology:cs_reading_relation('a6c81e1c-8b7c-4f45-b670-51124f9ba538', homoousios_christology__semi_arian_reading, influences).
narrative_ontology:cs_axiom('a6c81e1c-8b7c-4f45-b670-51124f9ba538', foundational, christ_identical_substance_with_father).
narrative_ontology:cs_axiom_status(christ_identical_substance_with_father, holdable).
narrative_ontology:cs_axiom_grounding('a6c81e1c-8b7c-4f45-b670-51124f9ba538', christ_identical_substance_with_father, theological).
narrative_ontology:cs_axiom('a6c81e1c-8b7c-4f45-b670-51124f9ba538', secondary, conciliar_anathema_binds_universal_church).
narrative_ontology:cs_axiom_status(conciliar_anathema_binds_universal_church, holdable).
narrative_ontology:cs_axiom_grounding('a6c81e1c-8b7c-4f45-b670-51124f9ba538', conciliar_anathema_binds_universal_church, conventional).
narrative_ontology:cs_reference_frame('a6c81e1c-8b7c-4f45-b670-51124f9ba538', nicene_conciliar_settlement_325).
narrative_ontology:cs_drift_state('a6c81e1c-8b7c-4f45-b670-51124f9ba538', post_constantinople_381_consolidation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a6c81e1c-8b7c-4f45-b670-51124f9ba538', '').
narrative_ontology:cs_kernel_id(homoousios_christology__pro_nicene_reading, homoousios_christology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_christology__pro_nicene_reading, nicene_bishops).
narrative_ontology:constraint_beneficiary(homoousios_christology__pro_nicene_reading, constantinian_imperial_court).
narrative_ontology:constraint_beneficiary(homoousios_christology__pro_nicene_reading, athanasian_alexandrian_see).
narrative_ontology:constraint_victim(homoousios_christology__pro_nicene_reading, arian_clergy).
narrative_ontology:constraint_victim(homoousios_christology__pro_nicene_reading, semi_arian_clergy).
narrative_ontology:constraint_victim(homoousios_christology__pro_nicene_reading, gothic_and_vandal_christian_communities).
narrative_ontology:constraint_victim(homoousios_christology__pro_nicene_reading, theological_dissenters_laity).
narrative_ontology:constraint_vindicates(homoousios_christology__pro_nicene_reading, trinitarian_orthodoxy_doctrine).
narrative_ontology:constraint_vindicates(homoousios_christology__pro_nicene_reading, conciliar_supremacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft and enforce the homoousios formula at Nicaea (325) and its restatements at Constantinople (381), controlling episcopal appointments, conciliar votes, and the anathema clause that excommunicates dissenters. Their doctrinal authority and sees' primacy are consolidated by the formula's adoption as imperial orthodoxy.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, nicene_bishops, agenda_setter,
    institutional, civilizational, arbitrage, continental).

% Convenes and funds the council, enforces its findings through imperial law, exile, and confiscation of church property from dissenting sees. Gains a unified doctrinal basis for imperial legitimacy and administrative control over a fractious church that could otherwise fragment along regional and political lines.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, constantinian_imperial_court, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_christology__pro_nicene_reading, constantinian_imperial_court, beneficiary).

% Alexandria's theological faction becomes the doctrinal center of gravity; its bishops gain enormous influence over the wider church and repeated imperial backing (with intermittent reversals) as the enforcers and articulators of the formula against Arian and semi-Arian rivals.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, athanasian_alexandrian_see, beneficiary,
    powerful, generational, mobile, regional).

% Bishops and priests holding that Christ is a created, subordinate being are deposed, exiled (as with Arius himself), and anathematized. Their sees are seized and redistributed to pro-Nicene appointees; recantation or exile from clerical office are their only paths, and their writings are ordered destroyed.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, arian_clergy, payer,
    moderate, biographical, trapped, continental).

% Hold the homoiousios compromise (similar, not identical substance) and are treated as insufficiently orthodox once pro-Nicene formulations harden after 362-381. Many are pressured into subscription or marginalized from councils; some negotiate partial rehabilitation by accepting Nicene terms under duress.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, semi_arian_clergy, payer,
    moderate, biographical, constrained, continental).

% Converted to Christianity under Arian missionary influence (notably via Ulfilas), these communities carry a homoousios-incompatible Christology for generations. When Nicene Rome and Constantinople gain political dominance, their faith is treated as heretical, feeding centuries of religious-political conflict and forced conversion pressure as Nicene polities absorb their territories.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, gothic_and_vandal_christian_communities, payer,
    powerless, generational, trapped, regional).

% Ordinary believers holding non-Nicene Christological views have no formal voice in conciliar decisions, face exclusion from sacraments and communities under bishops who enforce the formula, and bear social and sometimes legal consequences (loss of civic standing under later imperial edicts) for their beliefs.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, theological_dissenters_laity, payer,
    powerless, biographical, trapped, local).

% Study the councils' proceedings, letters, and political context to assess how much of the outcome reflects theological reasoning versus imperial politics, factional maneuvering, and the personal rivalries of bishops like Athanasius, Eusebius, and Arius.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, modern_historical_theologians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(homoousios_christology__pro_nicene_reading, nicene_bishops).
narrative_ontology:fixing_cost_class(homoousios_christology__pro_nicene_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, non-negotiable Christological formula that allows a geographically vast and administratively fragmented church to speak with one doctrinal voice, resolve competing regional Christologies, and align ecclesiastical unity with imperial political unity.
% TRANSFER_FUNCTION: Moves doctrinal authority, ecclesiastical office, and imperial favor toward bishops and sees that affirm homoousios, while moving clerical office, property, communal standing, and physical safety away from clergy and communities holding subordinationist Christologies.
% ABSENT_VOICES: Arian and semi-Arian laity and lower clergy across the empire (and later Gothic/Vandal converts) had no seat at Nicaea or Constantinople; the councils were assemblies of bishops convened and often stacked by imperial preference, and dissenting voices are known mostly through documents preserved by the winning side.
% DISAPPEARANCE_RATIONALE: Absent the enforced homoousios standard, the fourth-century church would likely have remained doctrinally plural (Arian, semi-Arian, and Nicene sees coexisting or competing regionally), imperial religious policy would lack a single orthodoxy to enforce, and centuries of subsequent doctrinal, political, and military conflict (Gothic Arianism, Christological councils at Ephesus and Chalcedon building on this precedent) would take a substantially different shape.
% FOUNDING_PROBLEM: The early fourth-century church faced a genuine theological crisis: competing accounts of Christ's relationship to God the Father threatened to fracture Christian communities and, once Christianity became politically favored under Constantine, threatened imperial unity built partly on religious cohesion.
% FOUNDING_PROBLEM_CORROBORATION: Pro-Nicene bishops and later orthodox tradition attest the problem was a genuine and dangerous heresy requiring resolution. Independent corroboration is thinner than the tradition suggests: contemporary non-aligned observers (e.g., later Enlightenment and modern historians such as Richard Rubenstein and R.P.C. Hanson) argue from surviving correspondence that the theological dispute was substantially entangled with, and at points subordinate to, imperial political consolidation and personal episcopal rivalry — a reading that does not originate from the beneficiary factions themselves.
narrative_ontology:disappearance_verdict(homoousios_christology__pro_nicene_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_christology__pro_nicene_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_christology__pro_nicene_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(homoousios_christology__pro_nicene_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_christology__pro_nicene_reading, 0.62, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises sharply after 325 (0.15 to 0.45) as the formula moves from theological proposal to enforced imperial policy, then climbs further after 381 (to 0.6) as Constantinople hardens the settlement against semi-Arian compromise and extends enforcement to a wider clerical population. Suppression tracks a steeper and higher curve (reaching 0.78) because the mechanism of enforcement — anathema, deposition, exile, property seizure, imperial edict — is a raw structural feature of how dissent is handled, independent of how extraction is scaled by scope. Theater ratio rises moderately (to 0.4) reflecting genuine ongoing theological argument alongside performative conciliar unanimity that masked continued private dissent and repeated imperial reversals (e.g., under Constantius II's pro-Arian sympathies).
 *
 * PERSPECTIVAL GAP:
 *   From the nicene_bishops and imperial court seats, homoousios resolves a real coordination problem: a single Christological standard prevents doctrinal fragmentation that would undermine both ecclesiastical unity and imperial religious policy. From the arian_clergy, semi_arian_clergy, and Gothic/Vandal community seats, the same formula operates as an enforced exclusion mechanism that transfers clerical office, property, and communal legitimacy toward one faction. The engine should compute divergent seat-level types from this same structural data — the pro-Nicene claim of settling a genuine crisis and the payer seats' experience of coercive extraction are not in tension; they are the expected product of one arrangement viewed from asymmetric structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Nicene bishops and the imperial court are declared beneficiaries: they set the agenda, control enforcement, and gain concentrated doctrinal and political authority — d sits near the beneficiary end. Arian and semi-Arian clergy, Gothic/Vandal converts, and dissenting laity are declared victims: they bear deposition, exile, property loss, or social exclusion with limited or no exit — d sits near the target end, amplified for the powerless and trapped groups (Gothic/Vandal communities, lay dissenters) relative to the moderate-power, still-somewhat-mobile clergy factions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (doctrinal fragmentation threatening ecclesiastical and imperial unity) was arguably live in 325 but its status by the fifth century is contested: the underlying theological question was substantially settled among the dominant faction, yet enforcement infrastructure (anathema, imperial edict, property seizure) continued and in places intensified, suggesting a further apparatus was doing more work maintaining factional dominance than resolving a live theological dispute. Classifying this as tangled_rope rather than pure snare or pure mountain captures both halves: it is not merely coercion dressed as theology (a genuine coordination function — resolving a real doctrinal crisis for a fracturing institution — exists) and it is not costless coordination (concentrated beneficiaries capture office and authority while a clearly identifiable set of payers bears exile, dispossession, and exclusion under active, non-optional enforcement).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_truth_vs_political_consolidation,
    'Was homoousios adopted because it is the theologically correct resolution of a genuine christological crisis, or because it served Constantinian and Alexandrian institutional consolidation, with theological argument as post-hoc justification?',
    'Comparative analysis of conciliar correspondence, the sequence of imperial reversals (Nicene under Constantine, Arian-leaning under Constantius II, Nicene again under Theodosius), and whether doctrinal positions tracked political alignment more than theological argument across these reversals.',
    'If the formula''s adoption tracks imperial political alignment more closely than theological argument, the coordination-function claim weakens substantially and the constraint reads closer to snare (naked institutional consolidation) than tangled_rope; if theological argument was the primary driver and enforcement was a secondary, later-added feature, the coordination function is stronger and the tangled_rope classification is better supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_truth_vs_political_consolidation, conceptual, 'Whether homoousios''s adoption was primarily theological or primarily political-institutional.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly does the pro-Nicene reading''s core premise diverge from the arian and semi-Arian readings — is it a difference in metaphysical vocabulary (ousia terminology was itself contested and shifted meaning across the fourth century), or a substantive difference about Christ''s ontological status?',
    'Philological and historical-theological analysis of how ''ousia'' and ''hypostasis'' were used differently by Eastern and Western theologians before and after Nicaea, and whether homoousios and homoiousios reflect a substantive or largely terminological disagreement that hardened into factional identity markers.',
    'If the disagreement is substantially terminological, the classification of pro-Nicene enforcement against semi-Arians as extraction (rather than defense of a substantive truth) is strengthened, since victims were punished partly for vocabulary rather than doctrine; if the disagreement is substantive, the pro-Nicene enforcement against semi-Arians has a stronger claim to being genuine doctrinal boundary-maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Whether the pro-Nicene/semi-Arian split is substantive or terminological, and what that implies for enforcement legitimacy.').

omega_variable(
    counterfactual_pluralism_stability,
    'Would a fourth-century church that remained doctrinally plural (tolerating Arian, semi-Arian, and Nicene Christologies as coexisting positions) have been institutionally stable, or would fragmentation have produced worse outcomes (schism, civil conflict) than the enforced settlement?',
    'Comparative study of regions and periods where doctrinal pluralism persisted longer (e.g., Gothic Arian kingdoms coexisting with Nicene populations for over a century) to assess whether coexistence was stable or itself a source of chronic conflict.',
    'If pluralism could have been stable, the coordination-function justification for enforced uniformity weakens; if pluralism reliably produced worse conflict, the coordination function is better supported even though the enforcement mechanism remains extractive toward the losing faction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_pluralism_stability, empirical, 'Whether enforced doctrinal uniformity was necessary for stability or an avoidable escalation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_christology__pro_nicene_reading, 300, 451).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_tr_t300, homoousios_christology__pro_nicene_reading, theater_ratio, 300, 0.1).
narrative_ontology:measurement_basis(homo_tr_t300, observed).
narrative_ontology:measurement(homo_tr_t325, homoousios_christology__pro_nicene_reading, theater_ratio, 325, 0.2).
narrative_ontology:measurement_basis(homo_tr_t325, observed).
narrative_ontology:measurement(homo_tr_t340, homoousios_christology__pro_nicene_reading, theater_ratio, 340, 0.28).
narrative_ontology:measurement_basis(homo_tr_t340, observed).
narrative_ontology:measurement(homo_tr_t381, homoousios_christology__pro_nicene_reading, theater_ratio, 381, 0.35).
narrative_ontology:measurement_basis(homo_tr_t381, observed).
narrative_ontology:measurement(homo_tr_t420, homoousios_christology__pro_nicene_reading, theater_ratio, 420, 0.4).
narrative_ontology:measurement_basis(homo_tr_t420, observed).
narrative_ontology:measurement(homo_tr_t451, homoousios_christology__pro_nicene_reading, theater_ratio, 451, 0.4).
narrative_ontology:measurement_basis(homo_tr_t451, observed).

% Extraction over time
narrative_ontology:measurement(homo_be_t300, homoousios_christology__pro_nicene_reading, base_extractiveness, 300, 0.15).
narrative_ontology:measurement_basis(homo_be_t300, observed).
narrative_ontology:measurement(homo_be_t325, homoousios_christology__pro_nicene_reading, base_extractiveness, 325, 0.45).
narrative_ontology:measurement_basis(homo_be_t325, observed).
narrative_ontology:measurement(homo_be_t340, homoousios_christology__pro_nicene_reading, base_extractiveness, 340, 0.5).
narrative_ontology:measurement_basis(homo_be_t340, observed).
narrative_ontology:measurement(homo_be_t381, homoousios_christology__pro_nicene_reading, base_extractiveness, 381, 0.6).
narrative_ontology:measurement_basis(homo_be_t381, observed).
narrative_ontology:measurement(homo_be_t420, homoousios_christology__pro_nicene_reading, base_extractiveness, 420, 0.62).
narrative_ontology:measurement_basis(homo_be_t420, observed).
narrative_ontology:measurement(homo_be_t451, homoousios_christology__pro_nicene_reading, base_extractiveness, 451, 0.62).
narrative_ontology:measurement_basis(homo_be_t451, observed).

% Suppression requirement over time
narrative_ontology:measurement(homo_su_t300, homoousios_christology__pro_nicene_reading, suppression_requirement, 300, 0.2).
narrative_ontology:measurement_basis(homo_su_t300, observed).
narrative_ontology:measurement(homo_su_t325, homoousios_christology__pro_nicene_reading, suppression_requirement, 325, 0.55).
narrative_ontology:measurement_basis(homo_su_t325, observed).
narrative_ontology:measurement(homo_su_t340, homoousios_christology__pro_nicene_reading, suppression_requirement, 340, 0.6).
narrative_ontology:measurement_basis(homo_su_t340, observed).
narrative_ontology:measurement(homo_su_t381, homoousios_christology__pro_nicene_reading, suppression_requirement, 381, 0.75).
narrative_ontology:measurement_basis(homo_su_t381, observed).
narrative_ontology:measurement(homo_su_t420, homoousios_christology__pro_nicene_reading, suppression_requirement, 420, 0.78).
narrative_ontology:measurement_basis(homo_su_t420, observed).
narrative_ontology:measurement(homo_su_t451, homoousios_christology__pro_nicene_reading, suppression_requirement, 451, 0.78).
narrative_ontology:measurement_basis(homo_su_t451, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_christology__pro_nicene_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(homoousios_christology__pro_nicene_reading, 0.1).
narrative_ontology:affects_constraint(homoousios_christology__pro_nicene_reading, arian_reading).
narrative_ontology:affects_constraint(homoousios_christology__pro_nicene_reading, semi_arian_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three structurally distinct readings of the homoousios_christology kernel, decomposed per the epsilon-invariance principle rather than represented as a single constraint with a measurement parameter. pro_nicene_reading (this file) models the enforced, eventually-dominant orthodoxy with high suppression and active exclusion mechanisms. arian_reading models the subordinationist position as its own constraint with its own beneficiary/victim structure (largely inverted once Nicene enforcement dominates). semi_arian_reading models the homoiousios compromise position, which experiences pressure from pro-Nicene consolidation without being simply absorbed into either extreme. All three share the network edge structure to reflect their common kernel origin and mutual causal entanglement across the fourth-century councils.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
