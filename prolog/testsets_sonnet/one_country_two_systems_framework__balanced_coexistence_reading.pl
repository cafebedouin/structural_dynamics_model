% ============================================================================
% CONSTRAINT STORY: one_country_two_systems_framework__balanced_coexistence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_one_country_two_systems_framework__balanced_coexistence_reading, []).

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
 *   constraint_id: one_country_two_systems_framework__balanced_coexistence_reading
 *   human_readable: One Country, Two Systems — Balanced Coexistence Reading
 *   domain: constitutional_law/political_systems/state_sovereignty
 *
 * SUMMARY:
 *   This story instantiates the balanced-coexistence reading of the One
 *   Country, Two Systems kernel: the boundary between PRC sovereignty and
 *   Hong Kong autonomy is treated as a genuinely negotiated, contested line
 *   rather than as settled by either sovereignty-primacy or autonomy-primacy
 *   premises. On this reading, the framework's periodic crises (2003 Article
 *   23 withdrawal, 2014 Umbrella Movement, 2019 extradition protests, 2020
 *   National Security Law, 2021 electoral overhaul) are each renegotiation
 *   events in an ongoing accommodation, not simple ratchets toward one pole.
 *   The framework coordinates a genuine problem (integrating a distinct
 *   legal-financial system into a sovereign state without destroying its
 *   function) while extracting asymmetrically from the political-liberties
 *   dimension of the bargain, particularly after 2019. This is a
 *   medium-epsilon regime: extraction rises and partially retreats rather
 *   than monotonically escalating, reflecting the fact that both central and
 *   local systems have practical limits they observably respect even as they
 *   also violate the spirit of prior accommodations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__balanced_coexistence_reading, 0.52).
domain_priors:suppression_score(one_country_two_systems_framework__balanced_coexistence_reading, 0.48).
domain_priors:theater_ratio(one_country_two_systems_framework__balanced_coexistence_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__balanced_coexistence_reading, tangled_rope).
narrative_ontology:human_readable(one_country_two_systems_framework__balanced_coexistence_reading, "One Country, Two Systems — Balanced Coexistence Reading").
narrative_ontology:topic_domain(one_country_two_systems_framework__balanced_coexistence_reading, "constitutional_law/political_systems/state_sovereignty").

domain_priors:requires_active_enforcement(one_country_two_systems_framework__balanced_coexistence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__balanced_coexistence_reading, 'bcbaeca7-e720-421d-906a-6364d860cb1b').
narrative_ontology:cs_kernel_codification('bcbaeca7-e720-421d-906a-6364d860cb1b', fixed_text).
narrative_ontology:cs_authority_grounding('bcbaeca7-e720-421d-906a-6364d860cb1b', extraction).
narrative_ontology:cs_interpretation_layer_present('bcbaeca7-e720-421d-906a-6364d860cb1b').
narrative_ontology:cs_reading_relation('bcbaeca7-e720-421d-906a-6364d860cb1b', one_country_two_systems_framework__sovereignty_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('bcbaeca7-e720-421d-906a-6364d860cb1b', one_country_two_systems_framework__autonomy_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('bcbaeca7-e720-421d-906a-6364d860cb1b', foundational, neither_sovereignty_nor_autonomy_is_absolute).
narrative_ontology:cs_axiom_status(neither_sovereignty_nor_autonomy_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('bcbaeca7-e720-421d-906a-6364d860cb1b', neither_sovereignty_nor_autonomy_is_absolute, conventional).
narrative_ontology:cs_axiom('bcbaeca7-e720-421d-906a-6364d860cb1b', foundational, contested_boundaries_resolved_by_political_accommodation_not_legal_supremacy).
narrative_ontology:cs_axiom_status(contested_boundaries_resolved_by_political_accommodation_not_legal_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('bcbaeca7-e720-421d-906a-6364d860cb1b', contested_boundaries_resolved_by_political_accommodation_not_legal_supremacy, instrumental).
narrative_ontology:cs_reference_frame('bcbaeca7-e720-421d-906a-6364d860cb1b', sino_british_joint_declaration_and_basic_law_1997_settlement).
narrative_ontology:cs_drift_state('bcbaeca7-e720-421d-906a-6364d860cb1b', post_2020_national_security_law_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bcbaeca7-e720-421d-906a-6364d860cb1b', '').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__balanced_coexistence_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_business_elite).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, prc_central_government).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_civil_service).
narrative_ontology:constraint_victim(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_pro_democracy_movement).
narrative_ontology:constraint_victim(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_working_residents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds ultimate sovereign authority and interprets the Basic Law through the National People's Congress Standing Committee. Negotiates the boundary of Hong Kong's autonomy case by case — sometimes deferring to local institutions, sometimes overriding them (electoral reform, national security law) — and treats each accommodation as provisional rather than as a permanent cession of authority. Retains the capacity to escalate if political costs of restraint exceed political costs of intervention.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, prc_central_government, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__balanced_coexistence_reading, prc_central_government, beneficiary).

% Administers the functional separation day to day — courts, currency, customs, immigration — under the common-law-derived Basic Law framework. Benefits from the continued existence of a distinct administrative system that preserves its institutional role, salaries, and international standing, but must continually negotiate which decisions require Beijing's sign-off and which remain locally determined.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_civil_service, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_civil_service, beneficiary).

% Profits from Hong Kong's distinct legal and financial system — common law contracts, freely convertible currency, separate customs territory — which functions only as long as the two-systems boundary holds in commercial matters. Can relocate capital and personnel if the accommodation collapses, giving this group leverage in the ongoing bargain, but generally prefers quiet stability to confrontation with Beijing.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_business_elite, beneficiary,
    powerful, biographical, arbitrage, global).

% Organizes for expanded suffrage and institutional checks promised (ambiguously) under the Basic Law. Bears the costs when the boundary of autonomy shifts toward sovereignty — arrests, disqualifications, the 2020 national security law — and has few exit options besides emigration, which forfeits political standing. Retains some bargaining power through international attention and periodic mass mobilization, but that power has visibly diminished across renegotiation cycles.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_pro_democracy_movement, payer,
    organized, generational, trapped, regional).

% Live inside whatever accommodation the two sovereign-and-autonomous systems currently strike — subject to shifting rules on assembly, press, and education without the capital mobility of the business elite or the organizational capacity of the movement. Absorb the diffuse social cost of each renegotiation cycle (protests, crackdowns, emigration waves) without a direct seat at the table.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_working_residents, payer,
    powerless, biographical, constrained, regional).

% The UK and other signatories to the Sino-British Joint Declaration asserted an internationally guaranteed interest in the boundary terms, but have no enforcement mechanism once China has taken the position that the Declaration is a historical document with no continuing legal effect. Their objections are registered diplomatically but do not alter the domestic bargaining structure.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, international_treaty_parties, excluded,
    institutional, generational, analytical, global).

% Study the framework as a case of contested federalism/asymmetric autonomy, documenting how each crisis (2003 Article 23, 2014 Umbrella Movement, 2019 extradition bill, 2020 NSL) resets the practical boundary and testing whether the resulting equilibrium is better modeled as negotiated accommodation or as one-directional erosion.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(one_country_two_systems_framework__balanced_coexistence_reading, diffuse).
narrative_ontology:fixing_cost_class(one_country_two_systems_framework__balanced_coexistence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows a single sovereign state to incorporate a territory with a structurally different economic and legal system without immediately homogenizing it — preserving Hong Kong's role as a distinct financial and legal gateway while integrating it under one national sovereign, and avoiding the costs (capital flight, international backlash, administrative chaos) of forced immediate convergence.
% TRANSFER_FUNCTION: Moves de facto governing authority back and forth across a contested boundary: in ordinary periods, moves administrative and civil-liberties latitude toward Hong Kong institutions and civil society; in crisis periods, moves enforcement and interpretive authority toward Beijing. Also moves economic rents from the persistence of the distinct system toward the business elite and civil service who administer or profit from it.
% ABSENT_VOICES: Hong Kong residents who reject both the sovereignty-primacy and pure-autonomy framings — who want a third option (independence, or a renegotiated treaty status) — have no institutional channel; that position is treated as outside the negotiable space by both central and local authorities. International treaty parties are diplomatically vocal but structurally excluded from enforcement.
% DISAPPEARANCE_RATIONALE: If the negotiated-accommodation framework collapsed entirely into pure sovereignty assertion or pure autonomy guarantee, the business elite and civil service argue the commercial and administrative architecture would rearrange sharply (capital flight or normalization respectively); the pro-democracy movement and central government dispute which direction collapse would go and who would gain — hence contested rather than a clean verdict.
% FOUNDING_PROBLEM: How to reincorporate a territory with an entrenched, internationally entangled common-law capitalist system into a sovereign socialist state without triggering capital flight, administrative collapse, or international rupture at the 1997 handover.
% FOUNDING_PROBLEM_CORROBORATION: PRC officials and Hong Kong civil servants attest the founding problem's administrative core (maintaining Hong Kong's function as a financial and legal gateway) remains live and is being successfully managed. Independent scholars and the pro-democracy movement attest that the political-autonomy dimension of the founding bargain (meaningful local self-government, promised universal suffrage) is functionally dead, citing the 2020 National Security Law and 2021 electoral overhaul as evidence the negotiated character of the arrangement has narrowed sharply in one direction — corroboration from outside both benefiting camps (comparative constitutional scholarship, UN human rights bodies) generally supports the erosion reading on the political dimension while agreeing the commercial/administrative dimension persists.
narrative_ontology:disappearance_verdict(one_country_two_systems_framework__balanced_coexistence_reading, contested).
narrative_ontology:founding_problem_status(one_country_two_systems_framework__balanced_coexistence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__balanced_coexistence_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(one_country_two_systems_framework__balanced_coexistence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(one_country_two_systems_framework__balanced_coexistence_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(one_country_two_systems_framework__balanced_coexistence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(one_country_two_systems_framework__balanced_coexistence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(one_country_two_systems_framework__balanced_coexistence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.52 (moderate, not extreme) because the framework retains a genuine coordination function — Hong Kong's distinct currency, courts, and customs territory persist and are not fully absorbed even after the 2020 NSL. Suppression at 0.48 reflects real but partial coercive enforcement: arrests and disqualifications are real, but broad swaths of commercial and civil life remain outside direct central intervention. Theater ratio at 0.40 captures that a meaningful share of 'autonomy' language now describes administrative continuity rather than genuine self-governance, without claiming the entire arrangement is performative. Accessibility collapse (0.45) is moderate: exit for ordinary residents has narrowed but is not fully closed (emigration schemes exist); exit for capital remains comparatively open. Resistance (0.60) reflects the repeated mass mobilizations documented across the interval — this is not a quiescent constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the PRC central government and civil service seats, the framework is a functioning coordination mechanism under active, legitimate renegotiation — each crisis resolved by an accommodation that preserves the core bargain. From the pro-democracy movement and working-resident seats, the same sequence of events computes as a tangled rope trending toward extraction: each renegotiation cedes more ground on the autonomy side than it recovers. The balanced-coexistence reading is exactly the claim that BOTH partial pictures are real — the engine's per-seat computation should reflect that divergence rather than resolving it in either direction.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are the parties whose institutional or economic position depends on the continued (if narrowing) existence of a distinct system: the PRC central government benefits from incorporating a valuable financial gateway without absorbing its instability; the Hong Kong civil service and business elite benefit from the persistence of the administrative and legal distinctiveness that gives them a functional and economic role. Victims are the parties who bear the cost when the negotiated boundary shifts toward sovereignty: the pro-democracy movement (direct legal and political costs) and working residents (diffuse costs of instability, emigration pressure, narrowing civic space) without commensurate exit capacity.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (integrating a distinct system without destroying its function) remains partially live on the commercial/administrative dimension but is increasingly dead on the political-autonomy dimension, per the R5 corroboration. Classifying this as tangled_rope rather than snare avoids mislabeling the entire framework as pure extraction — the currency board, common-law courts, and customs distinctiveness are real, functioning coordination mechanisms that a pure-extraction reading would miss. Classifying it as tangled_rope rather than rope avoids mislabeling the asymmetric costs borne by the pro-democracy movement and working residents as if the arrangement were a pure mutual benefit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    accommodation_vs_erosion_trajectory,
    'Is the sequence of post-2014 crises (Umbrella Movement, extradition protests, National Security Law, electoral overhaul) evidence of a genuinely negotiated, cyclical accommodation — as the balanced-coexistence reading holds — or is it evidence that the sovereignty-primacy reading has become the operative one in practice, with ''negotiation'' now describing only the pace and manner of a one-directional shift?',
    'Track whether any future crisis produces a genuine concession FROM Beijing back toward Hong Kong autonomy (not merely a pause in escalation). A resumption of competitive Legislative Council elections or judicial independence in politically sensitive cases would support the balanced reading; continued one-directional narrowing without reversal would support reclassifying this story toward the sovereignty_primacy_reading''s structure.',
    'If no reversal occurs across another full crisis cycle, this story''s claimed_type and metrics should converge toward those of sovereignty_primacy_reading, and the balanced-coexistence reading would need to be retired as descriptively false rather than merely contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accommodation_vs_erosion_trajectory, empirical, 'Whether the observed crisis cycle is genuine bidirectional renegotiation or one-directional erosion described in negotiation language.').

omega_variable(
    civil_society_leverage_durability,
    'Does Hong Kong civil society and the business elite retain meaningful bargaining leverage (economic exit threat, international attention) sufficient to constrain central authority, or has that leverage been substantially exhausted since 2020?',
    'Observe capital flow data, corporate relocation decisions, and whether international economic/diplomatic pressure has produced any documented policy concession from Beijing post-2020.',
    'If leverage has been exhausted, the ''negotiation'' framing of this reading becomes primarily rhetorical, strengthening the case for reclassification as a more purely extractive (snare-leaning) arrangement rather than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civil_society_leverage_durability, empirical, 'Whether civil society and business retain real bargaining power or only the appearance of it.').

omega_variable(
    kernel_framing_choice_basis,
    'What justifies treating ''balanced coexistence'' as a distinct, coherent reading rather than a euphemistic middle position invented to avoid choosing between the sovereignty-primacy and autonomy-primacy claims?',
    'Compare this reading''s structural predictions (medium epsilon, cyclical renegotiation, mutual acknowledged limits) against the historical record independently of rhetorical framing — if central authority has never in practice accepted a binding limit on its own power, the ''balanced'' framing may be structurally empty.',
    'If no instance exists of PRC central authority accepting a binding, non-revocable limit on its own intervention, this reading''s core premise (neither side absolute) is unsupported, and the constraint should be re-authored under the sovereignty_primacy_reading instead.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_choice_basis, conceptual, 'Whether balanced-coexistence is a structurally distinct reading or an averaging artifact between the two primacy readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__balanced_coexistence_reading, 1997, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(one__tr_t1997, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 1997, 0.2).
narrative_ontology:measurement(one__tr_t2003, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 2003, 0.25).
narrative_ontology:measurement(one__tr_t2008, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 2008, 0.28).
narrative_ontology:measurement(one__tr_t2014, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 2014, 0.32).
narrative_ontology:measurement(one__tr_t2019, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 2019, 0.35).
narrative_ontology:measurement(one__tr_t2020, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 2020, 0.4).
narrative_ontology:measurement(one__tr_t2024, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(one__be_t1997, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 1997, 0.28).
narrative_ontology:measurement(one__be_t2003, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 2003, 0.35).
narrative_ontology:measurement(one__be_t2008, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 2008, 0.33).
narrative_ontology:measurement(one__be_t2014, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 2014, 0.44).
narrative_ontology:measurement(one__be_t2019, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 2019, 0.55).
narrative_ontology:measurement(one__be_t2020, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 2020, 0.62).
narrative_ontology:measurement(one__be_t2024, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 2024, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(one__su_t1997, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 1997, 0.2).
narrative_ontology:measurement(one__su_t2003, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 2003, 0.28).
narrative_ontology:measurement(one__su_t2008, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 2008, 0.3).
narrative_ontology:measurement(one__su_t2014, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 2014, 0.4).
narrative_ontology:measurement(one__su_t2019, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 2019, 0.5).
narrative_ontology:measurement(one__su_t2020, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 2020, 0.6).
narrative_ontology:measurement(one__su_t2024, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__balanced_coexistence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(one_country_two_systems_framework__balanced_coexistence_reading, sovereignty_primacy_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__balanced_coexistence_reading, autonomy_primacy_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the one_country_two_systems_framework kernel. sovereignty_primacy_reading treats autonomy as fully delegated/revocable by PRC sovereign authority; autonomy_primacy_reading treats autonomy as treaty-guaranteed and internationally enforceable. This balanced_coexistence_reading occupies the structural middle, authored with medium extractiveness (0.52) versus what would be a higher ε under sovereignty_primacy and a lower ε under autonomy_primacy. Each reading has its own stable ε and stakeholder structure per the ε-invariance principle; they are linked here rather than merged into one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
