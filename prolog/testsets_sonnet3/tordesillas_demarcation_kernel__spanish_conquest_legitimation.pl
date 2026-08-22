% ============================================================================
% CONSTRAINT STORY: tordesillas_demarcation_kernel__spanish_conquest_legitimation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tordesillas_demarcation_kernel__spanish_conquest_legitimation, []).

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
 *   constraint_id: tordesillas_demarcation_kernel__spanish_conquest_legitimation
 *   human_readable: Papal Grant as License for Conquest and Subjugation West of the Tordesillas Line
 *   domain: international_law/colonial_history/sovereignty_theory
 *
 * SUMMARY:
 *   This story authors ONLY the Spanish conquest-legitimation reading of the
 *   Tordesillas demarcation kernel: the papal bulls (Inter Caetera and
 *   successors) and the 1494 Treaty of Tordesillas function as a license the
 *   Spanish Crown invokes to conquer, govern, and Christianize territories
 *   west of the demarcation line, subjugating indigenous polities and
 *   individuals in the process. The mechanism of extraction is concrete and
 *   traceable: the papal grant supplies the doctrinal title; the
 *   Requerimiento operationalizes it into a legal precondition for war; the
 *   encomienda system operationalizes conquest into ongoing forced labor and
 *   tribute extraction; and missionary conversion supplies the grant's stated
 *   justifying purpose. Extraction rises sharply across the sixteenth century
 *   as the encomienda system matures from initial conquest violence into an
 *   institutionalized labor-extraction regime, while theater (missionary and
 *   legal-formality activity that provides doctrinal cover without altering
 *   the underlying extraction) rises alongside it as critics within the
 *   Church itself (Montesinos, Las Casas) force increasing performative
 *   justification.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.93).
domain_priors:suppression_score(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.91).
domain_priors:theater_ratio(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, extractiveness, 0.93).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, snare).
narrative_ontology:human_readable(tordesillas_demarcation_kernel__spanish_conquest_legitimation, "Papal Grant as License for Conquest and Subjugation West of the Tordesillas Line").
narrative_ontology:topic_domain(tordesillas_demarcation_kernel__spanish_conquest_legitimation, "international_law/colonial_history/sovereignty_theory").

domain_priors:requires_active_enforcement(tordesillas_demarcation_kernel__spanish_conquest_legitimation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 'c95428ac-76a4-4940-82e4-6fbbe00c71fe').
narrative_ontology:cs_kernel_codification('c95428ac-76a4-4940-82e4-6fbbe00c71fe', formalized).
narrative_ontology:cs_authority_grounding('c95428ac-76a4-4940-82e4-6fbbe00c71fe', lineage).
narrative_ontology:cs_interpretation_layer_present('c95428ac-76a4-4940-82e4-6fbbe00c71fe').
narrative_ontology:cs_reading_relation('c95428ac-76a4-4940-82e4-6fbbe00c71fe', tordesillas_demarcation_kernel__portuguese_exploration_legitimation, coexists_with).
narrative_ontology:cs_axiom('c95428ac-76a4-4940-82e4-6fbbe00c71fe', foundational, papal_grant_confers_valid_title_to_conquer_and_subjugate).
narrative_ontology:cs_axiom_status(papal_grant_confers_valid_title_to_conquer_and_subjugate, overridden).
narrative_ontology:cs_axiom_grounding('c95428ac-76a4-4940-82e4-6fbbe00c71fe', papal_grant_confers_valid_title_to_conquer_and_subjugate, theological).
narrative_ontology:cs_axiom('c95428ac-76a4-4940-82e4-6fbbe00c71fe', foundational, conversion_mandate_justifies_coercive_subjugation).
narrative_ontology:cs_axiom_status(conversion_mandate_justifies_coercive_subjugation, overridden).
narrative_ontology:cs_axiom_grounding('c95428ac-76a4-4940-82e4-6fbbe00c71fe', conversion_mandate_justifies_coercive_subjugation, instrumental).
narrative_ontology:cs_reference_frame('c95428ac-76a4-4940-82e4-6fbbe00c71fe', papal_universal_temporal_jurisdiction).
narrative_ontology:cs_drift_state('c95428ac-76a4-4940-82e4-6fbbe00c71fe', post_valladolid_debate_and_new_laws, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('c95428ac-76a4-4940-82e4-6fbbe00c71fe', '').
narrative_ontology:cs_kernel_id(tordesillas_demarcation_kernel__spanish_conquest_legitimation, tordesillas_demarcation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_crown).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_colonial_administration).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, encomenderos).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, catholic_missionary_orders).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, indigenous_populations_west_of_line).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, enslaved_indigenous_laborers).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, indigenous_polities_dispossessed_of_sovereignty).
narrative_ontology:constraint_vindicates(tordesillas_demarcation_kernel__spanish_conquest_legitimation, papal_donation_doctrine).
narrative_ontology:constraint_vindicates(tordesillas_demarcation_kernel__spanish_conquest_legitimation, doctrine_of_discovery).
narrative_ontology:constraint_vindicates(tordesillas_demarcation_kernel__spanish_conquest_legitimation, requerimiento_legal_fiction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Petitioned the papacy for the bull Inter Caetera and its successors, then negotiated Tordesillas to convert a contested exploration claim into an exclusive license to conquer, govern, and Christianize everything west of the line. Administers the grant through viceroyalties, issues the Requerimiento as a legal precondition for war, and collects tribute, land, and labor rights across the claimed hemisphere.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_crown, agenda_setter,
    institutional, generational, arbitrage, global).

% Implements the grant on the ground through viceroys, encomenderos, and ecclesiastical courts. Distributes encomienda grants, enforces tribute and labor obligations, and adjudicates disputes using the papal donation as the ultimate title-source for all land and jurisdiction claimed.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_colonial_administration, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_colonial_administration, beneficiary).

% Receive grants of indigenous labor and tribute directly traceable to the Crown's papally-derived title. Extract forced labor and goods from assigned indigenous communities under the legal cover that the grant vindicates their claim to lordship over conquered peoples.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, encomenderos, beneficiary,
    powerful, biographical, mobile, regional).

% Granted the spiritual mandate underlying the temporal grant: conversion of indigenous peoples is the papal bull's stated justification for the conquest. Orders gain missions, converts, and institutional standing; some individual friars (Las Casas, Montesinos) later turn the same doctrinal apparatus against the encomienda's worst abuses, but the missionary function as a whole legitimizes the subjugation it accompanies.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, catholic_missionary_orders, beneficiary,
    organized, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(tordesillas_demarcation_kernel__spanish_conquest_legitimation, catholic_missionary_orders, agenda_setter).

% Have no representation in the papal grant, the treaty negotiation, or the Requerimiento read (often in Spanish or Latin, to people who never hear or cannot understand it) that legally authorizes war against them if they do not submit. Bear the demographic collapse, forced conversion, land dispossession, and encomienda labor extraction that follow directly from the grant's operation. No exit exists within the framework the grant establishes.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, indigenous_populations_west_of_line, payer,
    powerless, civilizational, trapped, continental).

% Assigned under encomienda or outright enslavement to extract labor and tribute for encomenderos and the Crown, under a legal architecture whose ultimate title traces to the papal grant. Mortality from overwork, disease, and violence is extreme; flight is punished as rebellion against a sovereignty the grant has declared legitimate.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, enslaved_indigenous_laborers, payer,
    powerless, biographical, trapped, regional).

% Pre-existing states, confederacies, and chiefdoms (Aztec, Inca, and smaller polities) whose independent political existence is unilaterally nullified by a treaty and papal grant they had no part in making. Their own diplomatic and military resistance is recast, under the Requerimiento's logic, as unlawful rebellion against a sovereignty conferred by God through the Pope.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, indigenous_polities_dispossessed_of_sovereignty, payer,
    organized, generational, trapped, continental).

% Party to the same treaty but on the other side of the line; not a stakeholder in the conquest-legitimation function this reading describes. Portugal's interest is exclusion of Castilian rivals east of the line, which is a structurally distinct constraint (see the sibling reading) and is not authored here.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, portuguese_crown, excluded,
    institutional, generational, arbitrage, global).

% Issues the bulls (Inter Caetera and successors) claiming universal jurisdiction to allocate newly encountered lands and peoples between Christian sovereigns, conditioned on evangelization. Provides the doctrinal donation theory that both crowns invoke, and later revises or is invoked against its own grant (Sublimis Deus, 1537) once the human costs become visible to Rome.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, papacy, agenda_setter,
    institutional, civilizational, analytical, universal).

% Study the doctrine of discovery as a foundational and still-cited source of title in settler-colonial jurisprudence (invoked as late as Johnson v. M'Intosh, 1823, and referenced in later indigenous land rights litigation). Assess whether the papal grant functioned as genuine international law or as a self-serving legal fiction retroactively rationalizing conquest already underway.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, modern_international_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tordesillas_demarcation_kernel__spanish_conquest_legitimation, diffuse).
narrative_ontology:fixing_cost_class(tordesillas_demarcation_kernel__spanish_conquest_legitimation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Between the two Iberian Catholic crowns, the grant and treaty solve a real coordination problem: preventing armed conflict between Spain and Portugal over overlapping claims to newly encountered lands, by having a mutually recognized authority (the Pope) draw a boundary both sides accept as binding.
% TRANSFER_FUNCTION: Transfers land, labor, tribute, political sovereignty, and religious self-determination from indigenous polities and individuals west of the line to the Spanish Crown, colonial administrators, encomenderos, and missionary orders — using a Rome-issued legal instrument as the chain of title for every subsequent extraction.
% ABSENT_VOICES: Every indigenous polity and individual west of the line whose land, labor, and sovereignty the grant disposes of. None were consulted in the papal bulls, the treaty negotiation, or the drafting of the Requerimiento that is nominally addressed to them; the document authorizing war against them is frequently read in a language and legal register wholly foreign to its audience, at a distance, or not read to them at all.
% DISAPPEARANCE_RATIONALE: Absent the papal grant's legitimating chain, Spanish conquest would have required an alternative title theory or would have proceeded as naked conquest without the doctrinal cover distinguishing it from piracy under contemporary European legal norms. Indigenous polities' sovereignty claims would have retained whatever standing they held under emerging natural-law and ius gentium theories (as Vitoria himself later argued), materially altering both the legal architecture of colonization and its ideological legitimation for centuries of subsequent settler-colonial jurisprudence.
% FOUNDING_PROBLEM: Two rival Catholic maritime powers were racing to claim newly reached Atlantic and Pacific lands, and their monarchs sought a mutually binding, religiously authoritative mechanism to divide the world and avert war between them.
% FOUNDING_PROBLEM_CORROBORATION: The inter-Iberian war-avoidance problem was substantially resolved by the 1494 treaty itself and has had no operative force between Spain and Portugal for centuries; corroborated by mainstream diplomatic historiography treating Tordesillas as a settled historical episode. However, the grant's legitimating function for indigenous subjugation was NOT attested as solving any problem for the indigenous parties it bound — Dominican friars Antonio de Montesinos and Bartolomé de las Casas, writing from within the Spanish colonial and ecclesiastical apparatus itself but explicitly opposing the encomienda's operation, are the closest available corroboration from partly outside the beneficiary set, and even they operated inside the framework of Spanish sovereignty rather than indigenous self-governance. No corroboration from an indigenous polity's own institutions survives in a form the colonial record preserved as authoritative.
narrative_ontology:disappearance_verdict(tordesillas_demarcation_kernel__spanish_conquest_legitimation, world_rearranges).
narrative_ontology:founding_problem_status(tordesillas_demarcation_kernel__spanish_conquest_legitimation, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 'none', 1).
narrative_ontology:epsilon_provenance(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.93, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tordesillas_demarcation_kernel__spanish_conquest_legitimation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tordesillas_demarcation_kernel__spanish_conquest_legitimation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tordesillas_demarcation_kernel__spanish_conquest_legitimation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored very high (0.93 by 1600) because the grant's operation directly produces catastrophic demographic collapse, forced labor mortality, and total political dispossession for the populations it governs — this is not incidental to the arrangement but its core mechanism. Suppression is comparably high (0.91) because alternatives are actively foreclosed: indigenous polities that resist are, by the Requerimiento's own logic, waging unlawful rebellion against a sovereignty the treaty has already conferred, which licenses further war. Accessibility_collapse is deliberately authored lower (0.35) than extractiveness/suppression because alternative legal and moral framings were never fully foreclosed even at the time — Vitoria's natural-law critique, Las Casas's advocacy, and Sublimis Deus (1537) all persisted as live counter-doctrines within the very tradition that produced the grant, meaning the doctrine never achieved the near-total alternative-collapse a genuine mountain would show. Resistance is authored high (0.78) reflecting both indigenous military and political resistance across the conquest period and internal ecclesiastical resistance to the encomienda's operation.
 *
 * PERSPECTIVAL GAP:
 *   From the Spanish Crown's and colonial administration's seat, the grant is coordination: a legitimate, religiously sanctioned resolution of competing European claims that also serves a genuine evangelizing mission. From the indigenous seats, the identical instrument operates as license for dispossession, forced labor, and demographic catastrophe with no coordination benefit reaching them at all. The engine should compute a stark seat divergence here — near-mountain or rope framing from the agenda-setter seats against near-total-extraction framing from the payer seats — and that divergence IS the historical phenomenon under study, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   The Spanish Crown, colonial administration, encomenderos, and missionary orders are beneficiaries: they collect land, labor, tribute, converts, and jurisdiction directly through the grant's operation, and their exit options (arbitrage/mobile) reflect genuine structural power to walk away from or reshape the arrangement without bearing its costs. Indigenous populations, enslaved laborers, and dispossessed polities are targets: trapped exit options, powerless-to-organized power levels, and the story's entire extractive flow runs from them to the beneficiary seats. The directionality here needs no override — the beneficiary/victim declarations map cleanly onto the structural relationship the historical record documents.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (averting inter-Iberian war) was substantively resolved by the treaty itself within years of its signing, yet the doctrinal apparatus it spawned (donation theory, discovery doctrine, Requerimiento) persisted and expanded in application for a full century-plus after the war-avoidance function had been served, continuing to authorize conquest and labor extraction long after its stated coordination purpose was moot. This is the canonical mandatrophy pattern: a genuinely coordinating founding purpose (dead) sustaining an apparatus whose actual ongoing function (subjugation) was never a coordination function at all — the classification as snare rather than tangled_rope reflects that no coordination function ever existed FOR the victim population; only the two colonizing crowns had a coordination problem to solve, and indigenous peoples were the object of the resulting instrument, never a coordinated party to it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    papal_authority_naturalness_vs_construction,
    'Was the papal donation doctrine treated by contemporaries as a genuine, binding exercise of universal spiritual-and-temporal jurisdiction (a natural extension of accepted medieval canon law), or was it always understood, even at the time, as a self-serving legal fiction constructed to launder conquest already underway or intended?',
    'Comparative analysis of contemporaneous juristic debate — Vitoria''s Relectiones (which explicitly rejected the donation theory as a valid title basis while proposing alternative just-war grounds), Sepúlveda''s defense of natural slavery, and the Valladolid debate (1550-51) record the doctrine''s contested status within the Spanish intellectual establishment itself, providing direct evidence of how seriously the donation theory was actually held even by supporters of conquest.',
    'If the donation theory was widely understood even at the time as a legal fiction rather than genuine natural or divine law, this strengthens the snare classification (extraction dressed as legitimate coordination) over any reading that treats papal authority claims as good-faith mountain-adjacent natural law. If it was genuinely believed binding by the vast majority of contemporary jurists, the constraint''s ideological function was less cynical construction and more sincere (if catastrophically wrong) doctrine — though this would not change the extraction outcome for indigenous victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(papal_authority_naturalness_vs_construction, conceptual, 'Whether the papal donation doctrine was sincere natural-law doctrine or contemporaneously-recognized legal fiction.').

omega_variable(
    requerimiento_functional_versus_theatrical_role,
    'Did the Requerimiento function as a genuine (if grossly inadequate) legal precondition intended to give indigenous polities a real chance to submit peacefully, or was it purely theatrical compliance-cover, read in circumstances (distant, untranslated, before dawn, to empty villages) engineered to guarantee non-compliance and thus ''legally'' authorize war regardless of indigenous response?',
    'Documentary review of specific Requerimiento readings recorded by conquistador chroniclers and by critics like Las Casas, who explicitly satirized the document''s practical impossibility; cross-reference with instances (rare) where indigenous polities did have functioning interpreters present and the subsequent legal/military treatment they received.',
    'If the Requerimiento was functionally theatrical in the overwhelming majority of documented cases, the theater_ratio for this specific sub-mechanism should be authored much higher than the story''s aggregate 0.42 — supporting a reading where the entire legal apparatus was cover for predetermined conquest rather than a genuine (if unjust) legal process.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(requerimiento_functional_versus_theatrical_role, empirical, 'Whether the Requerimiento was a genuine legal precondition or engineered theater guaranteeing war authorization.').

omega_variable(
    internal_ecclesiastical_resistance_causal_weight,
    'Did internal Catholic resistance to the encomienda system (Montesinos, Las Casas, Sublimis Deus, the New Laws of 1542) meaningfully constrain or reduce the extraction the grant enabled, or did it function primarily as reputational management that left the underlying extractive structure intact while individual abuses were selectively curbed?',
    'Compare encomienda labor-mortality and tribute-extraction rates before and after the New Laws'' promulgation and subsequent partial repeal following colonist rebellion (notably in Peru), to assess whether reform pressure produced measurable reduction in extraction or was substantially reversed by colonial resistance.',
    'If reform pressure produced negligible durable reduction in extraction, this supports treating the missionary-order beneficiary role and the internal-critique resistance figure as operating within, not against, the overall snare structure — resistance documented but not efficacious. If reforms produced durable reduction, the resistance metric and theater_ratio trajectory should be revisited for the post-1542 portion of the interval.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internal_ecclesiastical_resistance_causal_weight, empirical, 'Whether ecclesiastical reform efforts durably reduced extraction or functioned as reputational theater alongside continued extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 1493, 1600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tord_tr_t1493, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1493, 0.2).
narrative_ontology:measurement(tord_tr_t1510, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1510, 0.3).
narrative_ontology:measurement(tord_tr_t1525, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1525, 0.38).
narrative_ontology:measurement(tord_tr_t1540, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1540, 0.4).
narrative_ontology:measurement(tord_tr_t1560, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1560, 0.41).
narrative_ontology:measurement(tord_tr_t1600, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1600, 0.42).

% Extraction over time
narrative_ontology:measurement(tord_be_t1493, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1493, 0.55).
narrative_ontology:measurement(tord_be_t1510, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1510, 0.72).
narrative_ontology:measurement(tord_be_t1525, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1525, 0.85).
narrative_ontology:measurement(tord_be_t1540, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1540, 0.9).
narrative_ontology:measurement(tord_be_t1560, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1560, 0.92).
narrative_ontology:measurement(tord_be_t1600, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1600, 0.93).

% Suppression requirement over time
narrative_ontology:measurement(tord_su_t1493, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1493, 0.5).
narrative_ontology:measurement(tord_su_t1510, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1510, 0.68).
narrative_ontology:measurement(tord_su_t1525, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1525, 0.82).
narrative_ontology:measurement(tord_su_t1540, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1540, 0.88).
narrative_ontology:measurement(tord_su_t1560, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1560, 0.9).
narrative_ontology:measurement(tord_su_t1600, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1600, 0.91).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tordesillas_demarcation_kernel__spanish_conquest_legitimation, enforcement_mechanism).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__spanish_conquest_legitimation, tordesillas_demarcation_kernel__portuguese_exploration_legitimation).

% DUAL FORMULATION NOTE:
% This story and tordesillas_demarcation_kernel__portuguese_exploration_legitimation are twin readings of the same 1493-1494 papal-and-treaty kernel (tordesillas_demarcation_kernel). They share a founding instrument but are authored as structurally distinct constraints per the ε-invariance principle: this reading's ε (0.93) reflects catastrophic extraction from indigenous populations via conquest and encomienda; the Portuguese sibling reading's ε is authored much lower, reflecting a coordination/exclusion dispute between two European crowns rather than subjugation of a colonized population. The two stories must be read together to understand the kernel's full contested structure but must never be merged into a single ε value.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
