% ============================================================================
% CONSTRAINT STORY: reformation_composite__political_realignment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_composite__political_realignment_reading, []).

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
 *   constraint_id: reformation_composite__political_realignment_reading
 *   human_readable: Cuius Regio Political Realignment Constraint
 *   domain: historical/political/religious
 *
 * SUMMARY:
 *   This constraint story instantiates the political_realignment_reading of
 *   the reformation_composite kernel. It treats the Reformation not as
 *   primarily a theological event or a technological effect but as a
 *   structural mechanism by which emerging European territorial rulers
 *   leveraged religious differentiation to confiscate ecclesiastical
 *   jurisdiction, wealth, and military loyalty from imperial and papal
 *   authorities. The operative constraint is the cuius regio eius religio
 *   settlement and its enforcement apparatus: a system that coordinates
 *   populations within territorial boundaries while extracting sovereignty
 *   and revenue from supra-territorial authorities and shifting conformity
 *   costs onto dissenting subjects. This reading is structurally distinct
 *   from the theological and technological sibling readings and carries a
 *   different epsilon because its beneficiary and victim sets are
 *   political-territorial rather than soteriological or communicative.
 *
 * KEY AGENTS:
 *   - territorial_rulers (institutional/agenda-setter): determine confessional alignment, confiscate church assets, and enforce territorial sovereignty against imperial and papal claims
 *   - imperial_authority (institutional/payer): loses jurisdictional, fiscal, and military supremacy to territorial princes
 *   - papal_authority (institutional/payer): loses universal spiritual jurisdiction, tithes, and appointment rights
 *   - dissenting_subjects (powerless/payer): bear the costs of confessional exclusion and persecution under state enforcement
 *   - conforming_subjects (moderate/beneficiary): receive stability and protected worship in exchange for loyalty to the state religion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__political_realignment_reading, 0.72).
domain_priors:suppression_score(reformation_composite__political_realignment_reading, 0.72).
domain_priors:theater_ratio(reformation_composite__political_realignment_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__political_realignment_reading, tangled_rope).
narrative_ontology:human_readable(reformation_composite__political_realignment_reading, "Cuius Regio Political Realignment Constraint").
narrative_ontology:topic_domain(reformation_composite__political_realignment_reading, "historical/political/religious").

domain_priors:requires_active_enforcement(reformation_composite__political_realignment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__political_realignment_reading, '59cbcb94-f3b9-461e-89b7-c28bb028a62d').
narrative_ontology:cs_kernel_codification('59cbcb94-f3b9-461e-89b7-c28bb028a62d', distributed).
narrative_ontology:cs_authority_grounding('59cbcb94-f3b9-461e-89b7-c28bb028a62d', expertise).
narrative_ontology:cs_interpretation_layer_present('59cbcb94-f3b9-461e-89b7-c28bb028a62d').
narrative_ontology:cs_reading_relation('59cbcb94-f3b9-461e-89b7-c28bb028a62d', reformation_composite__theological_fragmentation_reading, coexists_with).
narrative_ontology:cs_reading_relation('59cbcb94-f3b9-461e-89b7-c28bb028a62d', reformation_composite__technological_mediation_reading, influences).
narrative_ontology:cs_axiom('59cbcb94-f3b9-461e-89b7-c28bb028a62d', foundational, territorial_sovereignty_supersedes_universal_jurisdiction).
narrative_ontology:cs_axiom_status(territorial_sovereignty_supersedes_universal_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('59cbcb94-f3b9-461e-89b7-c28bb028a62d', territorial_sovereignty_supersedes_universal_jurisdiction, empirically_contingent).
narrative_ontology:cs_axiom('59cbcb94-f3b9-461e-89b7-c28bb028a62d', foundational, rulers_as_agenda_setters_not_doctrine_receivers).
narrative_ontology:cs_axiom_status(rulers_as_agenda_setters_not_doctrine_receivers, holdable).
narrative_ontology:cs_axiom_grounding('59cbcb94-f3b9-461e-89b7-c28bb028a62d', rulers_as_agenda_setters_not_doctrine_receivers, empirically_contingent).
narrative_ontology:cs_reference_frame('59cbcb94-f3b9-461e-89b7-c28bb028a62d', westphalian_territorial_sovereignty).
narrative_ontology:cs_drift_state('59cbcb94-f3b9-461e-89b7-c28bb028a62d', post_westphalian_globalization, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('59cbcb94-f3b9-461e-89b7-c28bb028a62d', '').
narrative_ontology:cs_kernel_id(reformation_composite__political_realignment_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__political_realignment_reading, territorial_rulers).
narrative_ontology:constraint_beneficiary(reformation_composite__political_realignment_reading, conforming_subjects).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, imperial_authority).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, papal_authority).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, dissenting_subjects).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Determines the official religion of their territory under cuius regio eius religio, confiscates ecclesiastical properties and revenues, asserts sovereignty over legal and ecclesiastical appointments, and deploys confessional uniformity as a tool of state consolidation and resistance to imperial or papal jurisdiction.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, territorial_rulers, agenda_setter,
    institutional, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(reformation_composite__political_realignment_reading, territorial_rulers, beneficiary).

% Share the ruler's established religion and receive legal protection, public worship rights, and relative stability within a territorially unified confession; they do not set policy but benefit from the coordinated suppression of cross-border religious warfare.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, conforming_subjects, beneficiary,
    moderate, biographical, constrained, local).

% The Holy Roman Emperor and imperial institutions lose jurisdictional supremacy over religious matters as princes assert autonomy; imperial tax collection, military levies, and legal appeals erode as territorial courts and armies replace imperial structures.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, imperial_authority, payer,
    institutional, generational, constrained, continental).

% The Papacy loses territorial tithes, appointment rights, doctrinal monopoly, and the political leverage of universal spiritual jurisdiction over European rulers; must negotiate with or anathematize secular princes whose military and fiscal power now exceeds papal enforcement capacity.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, papal_authority, payer,
    institutional, civilizational, constrained, global).

% Subjects whose faith does not match the ruler's established religion face criminal penalties, exclusion from public office, fines, exile, or persecution under cuius regio frameworks; their religious practice is outlawed or driven underground within their native territory with no affordable legal exit.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, dissenting_subjects, payer,
    powerless, biographical, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_composite__political_realignment_reading, territorial_rulers).
narrative_ontology:fixing_cost_class(reformation_composite__political_realignment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a territorial mechanism for emerging European rulers to consolidate sovereignty, resolve religious civil conflict within fixed borders, and establish clear jurisdictional authority over ecclesiastical property and appointments without perpetual appeal to imperial or papal courts.
% TRANSFER_FUNCTION: Moves jurisdictional authority, tax revenue from church lands, military loyalty, and legal supremacy from imperial and papal structures to territorial rulers; moves the costs of religious conformity and persecution onto dissenting populations within each territory.
% ABSENT_VOICES: Dissenting theological voices within established territorial churches are excluded from doctrinal determination once cuius regio is enforced; transnational communal and peasant movements seeking non-territorial religious solutions (Anabaptists, radical reformers) are suppressed or expelled from the political settlement and have no seat at the Augsburg or Westphalian tables.
% DISAPPEARANCE_RATIONALE: If cuius regio and its enforcement vanished overnight, emerging territorial rulers would lack the jurisdictional wedge to confiscate church wealth, repudiate papal appointment rights, and centralize legal authority; imperial and papal structures would retain overlapping sovereignty claims across Europe, and the modern state system would not have coalesced along confessional boundaries.
% FOUNDING_PROBLEM: How to end the constant religious civil war and jurisdictional chaos of the early sixteenth century while consolidating nascent state authority against overlapping imperial and papal claims.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary humanist jurists and diplomats around the Peace of Augsburg attest the need for territorial peace; modern historical sociology and political history outside the theological tradition corroborate the sovereignty-consolidation motive. Theological historians dispute that state formation was the primary problem, arguing that doctrinal truth and salvation were the founding imperatives, which means corroboration from outside the benefiting parties exists but is contested by sibling readings of the same kernel.
narrative_ontology:disappearance_verdict(reformation_composite__political_realignment_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_composite__political_realignment_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_composite__political_realignment_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reformation_composite__political_realignment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_composite__political_realignment_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_composite__political_realignment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reformation_composite__political_realignment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reformation_composite__political_realignment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72) because the constraint transfers massive ecclesiastical wealth, legal authority, and military loyalty from imperial and papal structures to territorial rulers, while dissenting subjects bear severe conformity costs. Suppression is equally high (0.72): cuius regio requires active enforcement through state confessional courts, border controls, military campaigns (Schmalkaldic wars, Counter-Reformation interventions), and legal persecution. Theater ratio is moderate-high (0.55): genuine state-building occurs, but religious uniformity becomes partly performative as outward conformity masks private belief and confessional identity is instrumentalized for political allegiance. Accessibility collapse is 0.60 because alternatives such as imperial universalism, papal supranationalism, or autonomous communal Christianity persist only in marginalized pockets after the territorial-state model hardens. Resistance is 0.55 because imperial forces, papal leagues, and dissenting movements mount sustained but ultimately unsuccessful opposition. All metrics are authored on a shared time grid spanning 1517â1648.
 *
 * PERSPECTIVAL GAP:
 *   The territorial ruler seat experiences this constraint as legitimate state-building coordination that ends civil war and secures sovereignty; the imperial and papal seats experience it as jurisdictional theft enforced by military power and heretical repudiation; the dissenting subject seat experiences it as state-imposed religious extraction with no affordable exit. The engine computes this divergence from the structural data rather than from the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Territorial rulers are full beneficiaries (low d), gaining sovereignty and revenue directly from the constraint. Conforming subjects are moderate beneficiaries (moderate-low d), receiving stability at the cost of narrowed religious autonomy. Imperial and papal authorities are full targets (high d), losing scope and income. Dissenting subjects are extreme targets (very high d), legally trapped and stripped of worship rights. The engine amplifies effective extraction for the high-d seats and damps or inverts it for the low-d seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the constraint as pure extraction (snare) by acknowledging the genuine coordination problem solved: without cuius regio, Europe faced perpetual multi-sided religious civil war with no clear sovereignty map. The extraction is asymmetric but rides on a real coordination function (territorial pacification and state consolidation). If the coordination function were absentâif cuius regio produced only war and no sovereign orderâit would compute as snare. Conversely, treating it as rope would ignore the identifiable victims (imperial authority, papal authority, dissenting subjects) and the active enforcement required to suppress alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    political_vs_theological_primacy,
    'Is the cuius regio settlement better explained as an emergent political instrument of state formation, or as an epiphenomenon of prior theological fragmentation?',
    'Comparative archival analysis of the timing and motivation of key territorial breaks (e.g., Henry VIII, Schmalkaldic League) relative to doctrinal publication dates and reception networks.',
    'If theological commitments preceded and compelled political realignment, the constraint''s directionality shifts toward theological actors as agenda-setters, raising the coordination type toward identity_coordination and potentially lowering the political extraction score.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_vs_theological_primacy, conceptual, 'Whether political or theological causation is primary in the kernel.').

omega_variable(
    suppression_ambiguity_cuius_regio,
    'Is the conformity enforced by cuius regio maintained primarily by state coercion (police, borders, military), or by internalized confessional identity making exit psychologically costly?',
    'Post-Westphalian emigration rates, crypto-religious practice records, and comparative analysis of recidivism after legal repeal of confessional restrictions.',
    'If internalized, effective suppression exceeds structural measures and dissenting subjects should be reclassified as identity_locked rather than merely trapped, amplifying their computed extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_ambiguity_cuius_regio, empirical, 'Structural versus internalized suppression mechanism for confessional conformity.').

omega_variable(
    reformation_kernel_framing,
    'Does treating the Reformation as a composite kernel of separable readings obscure the historical reality that political, theological, and technological factors were inseparable in the sixteenth century?',
    'Historiometric analysis of whether the three readings predict disjoint empirical signatures in primary source attribution or overlap in explanatory coverage.',
    'If inseparable, the decomposition into three constraints is an analytical artifice and cross-coupling should be treated as high; if separable, the epsilon-invariance decomposition is structurally sound.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reformation_kernel_framing, conceptual, 'Whether the kernel decomposition is analytically valid or an imposed artifice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__political_realignment_reading, 0, 131).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t0, reformation_composite__political_realignment_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(refo_tr_t22, reformation_composite__political_realignment_reading, theater_ratio, 22, 0.28).
narrative_ontology:measurement(refo_tr_t44, reformation_composite__political_realignment_reading, theater_ratio, 44, 0.38).
narrative_ontology:measurement(refo_tr_t66, reformation_composite__political_realignment_reading, theater_ratio, 66, 0.45).
narrative_ontology:measurement(refo_tr_t88, reformation_composite__political_realignment_reading, theater_ratio, 88, 0.5).
narrative_ontology:measurement(refo_tr_t110, reformation_composite__political_realignment_reading, theater_ratio, 110, 0.52).
narrative_ontology:measurement(refo_tr_t131, reformation_composite__political_realignment_reading, theater_ratio, 131, 0.55).

% Extraction over time
narrative_ontology:measurement(refo_be_t0, reformation_composite__political_realignment_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(refo_be_t22, reformation_composite__political_realignment_reading, base_extractiveness, 22, 0.4).
narrative_ontology:measurement(refo_be_t44, reformation_composite__political_realignment_reading, base_extractiveness, 44, 0.55).
narrative_ontology:measurement(refo_be_t66, reformation_composite__political_realignment_reading, base_extractiveness, 66, 0.62).
narrative_ontology:measurement(refo_be_t88, reformation_composite__political_realignment_reading, base_extractiveness, 88, 0.66).
narrative_ontology:measurement(refo_be_t110, reformation_composite__political_realignment_reading, base_extractiveness, 110, 0.7).
narrative_ontology:measurement(refo_be_t131, reformation_composite__political_realignment_reading, base_extractiveness, 131, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t0, reformation_composite__political_realignment_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(refo_su_t22, reformation_composite__political_realignment_reading, suppression_requirement, 22, 0.55).
narrative_ontology:measurement(refo_su_t44, reformation_composite__political_realignment_reading, suppression_requirement, 44, 0.65).
narrative_ontology:measurement(refo_su_t66, reformation_composite__political_realignment_reading, suppression_requirement, 66, 0.7).
narrative_ontology:measurement(refo_su_t88, reformation_composite__political_realignment_reading, suppression_requirement, 88, 0.74).
narrative_ontology:measurement(refo_su_t110, reformation_composite__political_realignment_reading, suppression_requirement, 110, 0.73).
narrative_ontology:measurement(refo_su_t131, reformation_composite__political_realignment_reading, suppression_requirement, 131, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_composite__political_realignment_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
