% ============================================================================
% CONSTRAINT STORY: nicene_christological_kernel__homoiousios_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: nicene_christological_kernel__homoiousios_reading
 *   human_readable: Homoiousian Christology: Similar-Substance Reading of the Nicene Kernel
 *   domain: Historical Theology / Christology / Ecclesiastical Authority
 *
 * SUMMARY:
 *   In the decades following Nicaea (325 CE), a substantial bloc of Eastern
 *   bishops found the council's homoousios formula philosophically opaque and
 *   exegetically underdetermined — it seemed, to critics, to risk Sabellian
 *   modalism or to assert an identity of essence unsupported by the plain
 *   sense of texts describing the Son as sent, begotten, and subordinate. The
 *   homoiousian party, especially prominent at the councils of Ancyra (358)
 *   and Seleucia (359) under the patronage of Constantius II, proposed 'of
 *   similar substance' as a formula that preserved real ontological
 *   distinction between Father and Son while still affirming the Son's
 *   genuine, derived divinity — a middle path between strict Arian
 *   subordinationism and Nicene identity-of-essence. For roughly a decade
 *   this reading commanded significant imperial and episcopal support,
 *   displacing Nicene loyalists from major sees. It receded after
 *   Constantius's death (361) and was decisively superseded when the
 *   Cappadocian ousia/hypostasis distinction allowed homoousios to be re-read
 *   in a way that answered the homoiousian objections, ratified at
 *   Constantinople in 381.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_christological_kernel__homoiousios_reading, 0.42).
domain_priors:suppression_score(nicene_christological_kernel__homoiousios_reading, 0.38).
domain_priors:theater_ratio(nicene_christological_kernel__homoiousios_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_christological_kernel__homoiousios_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoiousios_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoiousios_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_christological_kernel__homoiousios_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoiousios_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_christological_kernel__homoiousios_reading, tangled_rope).
narrative_ontology:human_readable(nicene_christological_kernel__homoiousios_reading, "Homoiousian Christology: Similar-Substance Reading of the Nicene Kernel").
narrative_ontology:topic_domain(nicene_christological_kernel__homoiousios_reading, "Historical Theology / Christology / Ecclesiastical Authority").

domain_priors:requires_active_enforcement(nicene_christological_kernel__homoiousios_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_christological_kernel__homoiousios_reading, '73f90825-f467-4583-b702-61e8aa5dbbed').
narrative_ontology:cs_kernel_codification('73f90825-f467-4583-b702-61e8aa5dbbed', formalized).
narrative_ontology:cs_authority_grounding('73f90825-f467-4583-b702-61e8aa5dbbed', lineage).
narrative_ontology:cs_interpretation_layer_present('73f90825-f467-4583-b702-61e8aa5dbbed').
narrative_ontology:cs_reading_relation('73f90825-f467-4583-b702-61e8aa5dbbed', nicene_christological_kernel__homoousios_reading, coexists_with).
narrative_ontology:cs_axiom('73f90825-f467-4583-b702-61e8aa5dbbed', foundational, real_ontological_distinction_required_for_monotheistic_intelligibility).
narrative_ontology:cs_axiom_status(real_ontological_distinction_required_for_monotheistic_intelligibility, overridden).
narrative_ontology:cs_axiom_grounding('73f90825-f467-4583-b702-61e8aa5dbbed', real_ontological_distinction_required_for_monotheistic_intelligibility, deontological).
narrative_ontology:cs_axiom('73f90825-f467-4583-b702-61e8aa5dbbed', secondary, scriptural_subordination_language_entails_substantial_difference).
narrative_ontology:cs_axiom_status(scriptural_subordination_language_entails_substantial_difference, overridden).
narrative_ontology:cs_axiom_grounding('73f90825-f467-4583-b702-61e8aa5dbbed', scriptural_subordination_language_entails_substantial_difference, conventional).
narrative_ontology:cs_reference_frame('73f90825-f467-4583-b702-61e8aa5dbbed', conciliar_subordinationist_intelligibility).
narrative_ontology:cs_drift_state('73f90825-f467-4583-b702-61e8aa5dbbed', council_of_constantinople_381, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('73f90825-f467-4583-b702-61e8aa5dbbed', '').
narrative_ontology:cs_kernel_id(nicene_christological_kernel__homoiousios_reading, nicene_christological_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoiousios_reading, regional_eastern_bishops).
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoiousios_reading, exegetical_traditionalist_clergy).
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoiousios_reading, homoiousian_court_faction).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoiousios_reading, imperial_unity_project).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoiousios_reading, nicene_loyalist_bishops).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoiousios_reading, laity_caught_in_schism).
narrative_ontology:constraint_vindicates(nicene_christological_kernel__homoiousios_reading, subordinationist_intelligibility_of_divine_persons).
narrative_ontology:constraint_vindicates(nicene_christological_kernel__homoiousios_reading, preservation_of_monotheistic_clarity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bishops chiefly in Asia Minor and Syria who promote homoiousios as a mediating formula preserving both the Father's monarchical priority and Christ's derived divinity. They convene regional synods (Ancyra 358, Seleucia 359) to press the formula against both strict Arian subordinationism and Nicene homoousios, gaining doctrinal authority in their sees but depending on imperial favor (chiefly under Constantius II) to enforce it against rivals.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, regional_eastern_bishops, agenda_setter,
    organized, generational, constrained, regional).

% Clergy attached to Origenist exegetical traditions who read Scripture as implying real distinction between Father and Son. The homoiousian formula legitimates their existing hermeneutic and preaching practice without requiring wholesale doctrinal reversal; they gain continuity and local authority at the cost of standing outside the eventually dominant Nicene consensus.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, exegetical_traditionalist_clergy, beneficiary,
    moderate, biographical, constrained, regional).

% Courtiers and bishops with access to Constantius II who use the homoiousian compromise to build a governing coalition against both extreme Arians (anomoeans) and Nicene loyalists (notably Athanasius). They receive imperial patronage, sees, and councils convened in their favor; their exit is mobile because political fortune could shift the formula's favor again.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, homoiousian_court_faction, beneficiary,
    powerful, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(nicene_christological_kernel__homoiousios_reading, homoiousian_court_faction, agenda_setter).

% The imperial project of a single, empire-wide creed enforceable across all sees, which depended on doctrinal uniformity to underwrite political cohesion. The homoiousian compromise fractures this uniformity into competing regional formulas, weakening the emperor's capacity to use a single creed as an instrument of administrative unity; it cannot simply exit the theological dispute since imperial legitimacy is bound up in settling it.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, imperial_unity_project, payer,
    institutional, civilizational, trapped, continental).

% Bishops (Athanasius and allies in Alexandria and the West) who hold homoousios as the only formula preserving full divine equality. Under homoiousian ascendancy at court they face deposition, exile, and loss of sees; their exit options are constrained to endurance, appeal to Rome, or waiting out the political cycle until Nicene orthodoxy is restored at Constantinople (381).
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, nicene_loyalist_bishops, payer,
    organized, generational, constrained, continental).

% Ordinary believers whose local bishop's creed determines which communion, which liturgy, and which sacramental lineage they belong to. When sees change hands between homoiousian and homoousian bishops through imperial appointment or synodical reversal, congregations experience excommunication, competing claims to the same church buildings, and confusion over which baptism or ordination is valid. They have essentially no capacity to exit the jurisdiction of whichever bishop currently holds their see.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, laity_caught_in_schism, payer,
    powerless, biographical, trapped, local).

% Scholars examining the fourth-century controversies retrospectively, reconstructing the political and theological dynamics from conciliar records, letters, and later polemical historiography (much of it written by the eventual Nicene victors, complicating the record).
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, later_church_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mediating formula that lets bishops affirm Christ's real, substantial derivation from the Father — preserving intelligible monotheism and the Father's monarchy — without adopting either the full ontological identity of homoousios or the created-being subordinationism of strict Arianism. This solves a genuine problem: how to hold together scriptural language of distinction (the Son 'sent' by, 'obedient to,' 'less than' the Father) with the conviction that Christ is truly divine, in a way many bishops found more exegetically defensible than homoousios.
% TRANSFER_FUNCTION: Moves doctrinal authority, imperial patronage, and control of sees from Nicene-loyalist bishops to homoiousian bishops during periods of court favor (chiefly under Constantius II, 350s–361 CE), and moves ecclesiastical and administrative coherence away from the imperial unity project toward regional and factional autonomy. Ordinary laity absorb the cost of jurisdictional instability as sees change doctrinal hands.
% ABSENT_VOICES: The laity whose sacramental and communal life is disrupted by episcopal turnover are not party to any council; their objections survive mainly as scattered incidental reports of riots and disputed church buildings, not as a represented interest. Western bishops largely absent from the Eastern synods (Ancyra, Seleucia) that produced the formula would object that a regional Eastern compromise was being pressed toward empire-wide status without their participation.
% DISAPPEARANCE_RATIONALE: From the eventual Nicene-consensus vantage (post-381), the homoiousian reading's disappearance changed nothing of lasting theological substance — homoousios prevailed and homoiousios is remembered as a way-station. From the vantage of the mid-fourth century actors themselves, its disappearance (i.e., its defeat) rearranged everything: sees changed hands, exiled bishops returned, and an entire hermeneutic tradition was pushed to the margins or absorbed into Cappadocian mediation. Whether the reading's vanishing 'rearranges the world' depends on which temporal vantage authors the answer — hence contested rather than settled either way.
% FOUNDING_PROBLEM: How to formulate Christ's relationship to the Father in a way that is exegetically defensible (accounting for scriptural language of the Son's subordination and derivation), philosophically intelligible (avoiding the appearance of two independent gods), and politically workable (capable of commanding assent from a fractured episcopate) — without collapsing into either strict subordinationist Arianism or a formula (homoousios) that many found philosophically opaque and scripturally under-supported at the time it was first proposed (325 CE).
% FOUNDING_PROBLEM_CORROBORATION: The problem of finding a mediating formula between Arian subordinationism and Nicene homoousios was substantially resolved by the Cappadocian Fathers' distinction between ousia and hypostasis, ratified at Constantinople in 381 — a resolution corroborated by both Eastern and Western sees converging on homoousios thereafter and by modern historical-critical scholarship (e.g. R.P.C. Hanson's history of the controversy) written from outside any communion invested in vindicating the homoiousian formula specifically. No living ecclesiastical body today attests the founding problem as still live in the form the homoiousians posed it; the formula survives only as a historical waypoint, not as a defended present position.
narrative_ontology:disappearance_verdict(nicene_christological_kernel__homoiousios_reading, contested).
narrative_ontology:founding_problem_status(nicene_christological_kernel__homoiousios_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_christological_kernel__homoiousios_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nicene_christological_kernel__homoiousios_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nicene_christological_kernel__homoiousios_reading, 0.42, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is moderate and time-varying (peaking near 0.44 at the height of court favor in 358, receding to 0.10 by 381) because the homoiousian reading's costliness is bound to its political ascendancy: when it holds imperial patronage, it extracts sees, authority, and doctrinal legitimacy from Nicene loyalists and destabilizes lay communion; when political favor recedes, so does its extractive force. Suppression tracks the same arc — synodical depositions and exiles cluster in the 350s. Theater ratio is moderate-low throughout: the councils involved genuine theological argument (this was not pure performance), but a growing share of synodical activity in the peak years served factional consolidation rather than doctrinal clarification, hence the rise to 0.30–0.32 at 358–361.
 *
 * DIRECTIONALITY LOGIC:
 *   Regional Eastern bishops and the homoiousian court faction are structural beneficiaries: the formula legitimates their existing exegetical commitments and delivers them sees, councils, and imperial favor. Nicene loyalist bishops and the imperial unity project are targets: loyalists lose sees and face exile under homoiousian ascendancy, and the empire's unity project is undermined by having no single enforceable creed. Laity are the most powerless payers — trapped in whatever jurisdiction their see currently holds, with no capacity to exit the doctrinal dispute despite bearing its sacramental and communal consequences directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is authored as 'dead' — the underlying tension the homoiousian formula tried to resolve (reconciling scriptural subordination language with genuine divinity) was substantially answered by Cappadocian technical vocabulary, and no living tradition defends homoiousios specifically as still necessary. This prevents the classification from treating the reading as a live, ongoing extraction: it was a genuine, historically bounded attempt at coordination (a real theological problem, addressed by real argument) whose extractive costs were concentrated in a specific ~decade-long window of political ascendancy, not a permanent structure. Reading the temporal measurements alongside the mismatch check (dead founding problem + contested disappearance verdict) is the intended signal: this is a resolved historical episode, not a persisting mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    homoiousios_vs_homoousios_framing_choice,
    'Is the choice to treat homoiousios as the ''reading under evaluation'' here (rather than treating homoousios as the baseline and homoiousios as its deviation) itself a framing artifact of which side eventually won at Constantinople (381)?',
    'Compare how a fourth-century Eastern synod (e.g. Ancyra 358) would have described the kernel versus how post-381 conciliar historiography describes it; the framing that treats homoousios as default is itself a retrospective imposition from the eventual victors.',
    'If the framing is retrospectively biased, the ''moderate extractiveness, fragmenting'' character assigned to this reading may partly reflect that it lost rather than any intrinsic structural property distinguishing it from the homoousios reading at the time both were live options.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(homoiousios_vs_homoousios_framing_choice, conceptual, 'Whether treating homoousios as the kernel''s stable center and homoiousios as the contested deviation is itself an artifact of who won.').

omega_variable(
    genuine_theology_vs_court_faction_instrument,
    'Was homoiousios a sincere, independently-motivated theological mediating position, or was it substantially a vocabulary of convenience adopted by a court faction to build a winning anti-Nicene, anti-Arian coalition under Constantius II?',
    'Compare the pre-court-patronage writings of homoiousian theologians (e.g. Basil of Ancyra) with the formula''s usage in imperially-convened councils; sincere independent development predating imperial interest would support the coordination reading, while formula adoption coinciding tightly with patronage would support the instrumental reading.',
    'If substantially instrumental, the extraction and suppression metrics during the 350s peak understate the degree to which the formula functioned as cover for a straightforward power struggle over sees, and the classification would drift further toward extraction-dominant during that window.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_theology_vs_court_faction_instrument, empirical, 'Whether the homoiousian formula was sincere theology or a patronage-driven coalition instrument.').

omega_variable(
    monotheistic_clarity_claim_evaluability,
    'Is ''preserving monotheistic clarity'' a genuinely evaluable coordination benefit, or is it a theological value claim that cannot be adjudicated by the same structural analysis applied to the extraction and suppression metrics?',
    'No empirical resolution mechanism exists; this is a question of whether theological coherence claims are the kind of thing susceptible to structural/extraction analysis at all, or whether they sit outside that frame entirely.',
    'If monotheistic clarity is treated as a genuine value the formula protects, the coordination_function narrative is strengthened and less of the constraint''s persistence should be attributed to pure factional extraction; if treated as an unfalsifiable rationalization, more weight shifts to the political/patronage account.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(monotheistic_clarity_claim_evaluability, preference, 'Whether a claimed theological coherence benefit is evaluable by structural analysis or lies outside its scope.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_christological_kernel__homoiousios_reading, 325, 381).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nice_tr_t325, nicene_christological_kernel__homoiousios_reading, theater_ratio, 325, 0.1).
narrative_ontology:measurement_basis(nice_tr_t325, observed).
narrative_ontology:measurement(nice_tr_t336, nicene_christological_kernel__homoiousios_reading, theater_ratio, 336, 0.15).
narrative_ontology:measurement_basis(nice_tr_t336, observed).
narrative_ontology:measurement(nice_tr_t350, nicene_christological_kernel__homoiousios_reading, theater_ratio, 350, 0.22).
narrative_ontology:measurement_basis(nice_tr_t350, observed).
narrative_ontology:measurement(nice_tr_t358, nicene_christological_kernel__homoiousios_reading, theater_ratio, 358, 0.3).
narrative_ontology:measurement_basis(nice_tr_t358, observed).
narrative_ontology:measurement(nice_tr_t361, nicene_christological_kernel__homoiousios_reading, theater_ratio, 361, 0.32).
narrative_ontology:measurement_basis(nice_tr_t361, observed).
narrative_ontology:measurement(nice_tr_t370, nicene_christological_kernel__homoiousios_reading, theater_ratio, 370, 0.28).
narrative_ontology:measurement_basis(nice_tr_t370, observed).
narrative_ontology:measurement(nice_tr_t381, nicene_christological_kernel__homoiousios_reading, theater_ratio, 381, 0.2).
narrative_ontology:measurement_basis(nice_tr_t381, observed).

% Extraction over time
narrative_ontology:measurement(nice_be_t325, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 325, 0.18).
narrative_ontology:measurement_basis(nice_be_t325, observed).
narrative_ontology:measurement(nice_be_t336, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 336, 0.24).
narrative_ontology:measurement_basis(nice_be_t336, observed).
narrative_ontology:measurement(nice_be_t350, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 350, 0.33).
narrative_ontology:measurement_basis(nice_be_t350, observed).
narrative_ontology:measurement(nice_be_t358, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 358, 0.44).
narrative_ontology:measurement_basis(nice_be_t358, observed).
narrative_ontology:measurement(nice_be_t361, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 361, 0.42).
narrative_ontology:measurement_basis(nice_be_t361, observed).
narrative_ontology:measurement(nice_be_t370, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 370, 0.3).
narrative_ontology:measurement_basis(nice_be_t370, observed).
narrative_ontology:measurement(nice_be_t381, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 381, 0.1).
narrative_ontology:measurement_basis(nice_be_t381, observed).

% Suppression requirement over time
narrative_ontology:measurement(nice_su_t325, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 325, 0.15).
narrative_ontology:measurement_basis(nice_su_t325, observed).
narrative_ontology:measurement(nice_su_t336, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 336, 0.2).
narrative_ontology:measurement_basis(nice_su_t336, observed).
narrative_ontology:measurement(nice_su_t350, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 350, 0.3).
narrative_ontology:measurement_basis(nice_su_t350, observed).
narrative_ontology:measurement(nice_su_t358, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 358, 0.45).
narrative_ontology:measurement_basis(nice_su_t358, observed).
narrative_ontology:measurement(nice_su_t361, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 361, 0.4).
narrative_ontology:measurement_basis(nice_su_t361, observed).
narrative_ontology:measurement(nice_su_t370, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 370, 0.25).
narrative_ontology:measurement_basis(nice_su_t370, observed).
narrative_ontology:measurement(nice_su_t381, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 381, 0.12).
narrative_ontology:measurement_basis(nice_su_t381, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_christological_kernel__homoiousios_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(nicene_christological_kernel__homoiousios_reading, 0.1).
narrative_ontology:affects_constraint(nicene_christological_kernel__homoiousios_reading, homoousios_reading).

% DUAL FORMULATION NOTE:
% This story and nicene_christological_kernel's homoousios_reading are sibling instantiations of the same contested kernel (nicene_christological_kernel), decomposed per the ε-invariance principle because measuring 'the Nicene Christological dispute' as a single constraint would conflate two structurally distinct claims with different beneficiary/victim sets and different persistence trajectories. This file (homoiousios) shows moderate, time-bounded extraction peaking under Constantius II (350s-361) and receding to near-zero by 381; the homoousios reading should show a different extraction profile reflecting its role as the eventually-dominant, empire-unifying formula. Do not average or merge; each carries its own ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
