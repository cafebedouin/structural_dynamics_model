% ============================================================================
% CONSTRAINT STORY: jati_practice_norm__localized_practice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jati_practice_norm__localized_practice_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: jati_practice_norm__localized_practice_reading
 *   human_readable: Jati Boundary Maintenance as Localized, Renegotiable Coordination Practice
 *   domain: social_anthropology/religious_studies/political_economy
 *
 * SUMMARY:
 *   This story instantiates the localized-practice reading of the
 *   jati_practice_norm kernel: jati boundaries treated as decentralized,
 *   empirically fluid coordination norms subject to continuous local
 *   renegotiation, evidenced by proliferation to well over 3,000 recorded
 *   categories nationally and constant boundary revision by local panchayat
 *   and association bodies. This reading is deliberately narrow — it does not
 *   describe the orthodox textual reading (fixed varna-derived boundaries
 *   with pollution sanctions) or the colonial census reading (external
 *   administrative reification for governance legibility); those are separate
 *   constraints with their own ε values, linked here only as siblings in the
 *   same kernel contest.
 *
 * KEY AGENTS:
 *   - village_panchayat_bodies: local adjudicators and boundary-setters (organized/constrained) — administer recognition and revision
 *   - subcaste_associations: organized beneficiary bodies that both maintain and profit from the coordination structure (organized/constrained)
 *   - local_marriage_networks and occupational_guild_clusters: diffuse beneficiaries of the matching function (moderate/constrained)
 *   - cross_boundary_couples and occupational_mobility_seekers: bear the cost of boundary maintenance when they attempt to cross it (powerless/trapped-constrained)
 *   - field_anthropologists: analytical observers documenting proliferation and renegotiation empirically
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jati_practice_norm__localized_practice_reading, 0.22).
domain_priors:suppression_score(jati_practice_norm__localized_practice_reading, 0.28).
domain_priors:theater_ratio(jati_practice_norm__localized_practice_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_practice_norm__localized_practice_reading, rope).
narrative_ontology:human_readable(jati_practice_norm__localized_practice_reading, "Jati Boundary Maintenance as Localized, Renegotiable Coordination Practice").
narrative_ontology:topic_domain(jati_practice_norm__localized_practice_reading, "social_anthropology/religious_studies/political_economy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__localized_practice_reading, '3419cbc8-b723-42d1-8896-47a9245f504d').
narrative_ontology:cs_kernel_codification('3419cbc8-b723-42d1-8896-47a9245f504d', distributed).
narrative_ontology:cs_authority_grounding('3419cbc8-b723-42d1-8896-47a9245f504d', practice).
narrative_ontology:cs_interpretation_layer_present('3419cbc8-b723-42d1-8896-47a9245f504d').
narrative_ontology:cs_reading_relation('3419cbc8-b723-42d1-8896-47a9245f504d', jati_practice_norm__orthodox_textual_reading, coexists_with).
narrative_ontology:cs_reading_relation('3419cbc8-b723-42d1-8896-47a9245f504d', jati_practice_norm__colonial_census_reading, influences).
narrative_ontology:cs_axiom('3419cbc8-b723-42d1-8896-47a9245f504d', foundational, boundary_legitimacy_derives_from_ongoing_local_consensus).
narrative_ontology:cs_axiom_status(boundary_legitimacy_derives_from_ongoing_local_consensus, holdable).
narrative_ontology:cs_axiom_grounding('3419cbc8-b723-42d1-8896-47a9245f504d', boundary_legitimacy_derives_from_ongoing_local_consensus, conventional).
narrative_ontology:cs_axiom('3419cbc8-b723-42d1-8896-47a9245f504d', secondary, category_proliferation_is_evidence_of_functioning_not_failure).
narrative_ontology:cs_axiom_status(category_proliferation_is_evidence_of_functioning_not_failure, holdable).
narrative_ontology:cs_axiom_grounding('3419cbc8-b723-42d1-8896-47a9245f504d', category_proliferation_is_evidence_of_functioning_not_failure, empirically_contingent).
narrative_ontology:cs_reference_frame('3419cbc8-b723-42d1-8896-47a9245f504d', decentralized_practice_equilibrium).
narrative_ontology:cs_drift_state('3419cbc8-b723-42d1-8896-47a9245f504d', contemporary_urbanization_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3419cbc8-b723-42d1-8896-47a9245f504d', '').
narrative_ontology:cs_kernel_id(jati_practice_norm__localized_practice_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, local_marriage_networks).
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, occupational_guild_clusters).
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, village_panchayat_bodies).
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, subcaste_associations).
narrative_ontology:constraint_victim(jati_practice_norm__localized_practice_reading, cross_boundary_couples).
narrative_ontology:constraint_victim(jati_practice_norm__localized_practice_reading, occupational_mobility_seekers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicate disputes about who belongs to which jati grouping at the local level, recognize new sub-groupings when occupational or migratory circumstances shift, and mediate marriage and commensality questions. Their rulings are locally binding but have no force outside the region and are frequently revised as circumstances change.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, village_panchayat_bodies, agenda_setter,
    organized, generational, constrained, local).

% Formally organize around a jati sub-grouping to pool resources for education, marriage brokering, and mutual aid; they actively lobby for recognition of new sub-divisions or mergers when it serves members' economic or status interests. They both maintain the boundary and profit from its coordination function.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, subcaste_associations, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jati_practice_norm__localized_practice_reading, subcaste_associations, agenda_setter).

% Families use jati boundaries to solve a genuine matching problem: finding spouses with compatible social expectations, ritual practice, and kinship obligations without exhaustive individual vetting. The norm reduces search costs for those operating inside the recognized boundary.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, local_marriage_networks, beneficiary,
    moderate, biographical, constrained, local).

% Historically organized around traditional occupations, these clusters use jati identity to coordinate craft transmission, apprenticeship, and market access among practitioners. New occupational shifts (migration to new trades) generate new sub-jati claims rather than expulsion, illustrating the proliferation dynamic.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, occupational_guild_clusters, beneficiary,
    moderate, generational, constrained, regional).

% Individuals who wish to marry or affiliate across recognized jati lines face social sanction, loss of network support, or family estrangement even where no formal or state-backed rule prohibits the union. Their only real exit is geographic or social relocation away from the network that enforces the boundary.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, cross_boundary_couples, payer,
    powerless, biographical, trapped, local).

% People attempting to move into a different trade or status category associated with another jati sometimes encounter resistance to their claimed new affiliation from both origin and destination groupings, bearing the cost of boundary maintenance during the (often successful, but not costless) transition period.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, occupational_mobility_seekers, payer,
    powerless, biographical, constrained, local).

% Document the empirical proliferation of jati categories (well over 3,000 distinct groupings recorded across regions) and the constant local renegotiation of boundaries, providing the evidentiary basis for reading jati as a live, decentralized coordination practice rather than a fixed hierarchy.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, field_anthropologists, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jati_practice_norm__localized_practice_reading, diffuse).
narrative_ontology:fixing_cost_class(jati_practice_norm__localized_practice_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Jati boundaries solve local matching and coordination problems — who to marry, who to apprentice with, who to extend mutual aid to — without requiring centralized enumeration or state adjudication; the boundary functions as a low-cost heuristic for trust and reciprocity among people who cannot exhaustively vet every relationship.
% TRANSFER_FUNCTION: Primarily coordinates access to marriage networks, craft transmission, and mutual aid within the recognized boundary; the transfer cost falls mainly on individuals who wish to cross boundaries and are denied access to the network's coordination benefits rather than on a redistribution of material wealth between groups.
% ABSENT_VOICES: Individuals who successfully cross jati lines and thrive are underrepresented in the ethnographic record relative to those who remain within recognized boundaries; their experience of the boundary as more porous than institutionally described rarely reaches the panchayat or association bodies that adjudicate recognition.
% DISAPPEARANCE_RATIONALE: If jati-boundary coordination vanished overnight, local marriage markets, craft apprenticeship networks, and mutual aid associations organized around it would lose their existing matching mechanism and would need to reconstitute trust and reciprocity through some other network — kinship, religious congregation, residential proximity, or state-provided alternatives — a genuine reorganization, not a null event.
% FOUNDING_PROBLEM: Coordinating trust, reciprocity, marriage compatibility, and occupational transmission among large, dispersed populations without a centralized verification apparatus.
% FOUNDING_PROBLEM_CORROBORATION: Field anthropologists studying contemporary caste dynamics (a seat outside the beneficiary associations) corroborate that the coordination problem — matching, mutual aid provision, occupational network maintenance — remains actively solved through jati-boundary practice in most regions studied, evidenced by continuous proliferation and local renegotiation rather than institutional fossilization.
narrative_ontology:disappearance_verdict(jati_practice_norm__localized_practice_reading, world_rearranges).
narrative_ontology:founding_problem_status(jati_practice_norm__localized_practice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jati_practice_norm__localized_practice_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jati_practice_norm__localized_practice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jati_practice_norm__localized_practice_reading, 0.22, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jati_practice_norm__localized_practice_reading_tests).
:- end_tests(jati_practice_norm__localized_practice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.22) because this reading's structural claim is that the norm functions predominantly as a genuine, low-overhead coordination mechanism: it reduces matching costs in marriage and occupational networks without requiring centralized enforcement machinery. Suppression is moderate-low (0.28) — social sanction against boundary-crossers exists and is real, but it operates through withdrawal of network benefits rather than coercive apparatus, and the boundary itself proliferates rather than hardens, which is inconsistent with heavy suppression. Theater ratio is low and essentially flat (0.12-0.15) because there is little performative maintenance distinct from the coordination function itself — recognition rituals ARE the coordination mechanism, not a decorative overlay on top of it. The flat, near-static measurement series reflects this reading's core empirical claim: the system is stable in aggregate intensity while proliferating in category count, a different kind of drift than rising extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of subcaste associations and panchayat bodies, the boundary is a functioning, actively-maintained-but-lightly-enforced coordination good that they administer and that continuously adapts to demographic and economic change (evidenced by proliferation). From the seat of a cross-boundary couple, the same structure operates as an involuntary social tax on their choice of partner, imposed without formal coercive backing but with real relational and economic cost. The engine should register this as low aggregate extraction with locally concentrated cost on a small population of boundary-crossers, not uniform extraction across all governed parties.
 *
 * DIRECTIONALITY LOGIC:
 *   Village panchayats and subcaste associations sit near the beneficiary end: they administer the recognition process and capture the coordination value (marriage-matching efficiency, mutual aid pooling, craft transmission) it produces. Local marriage networks and occupational guild clusters are diffuse beneficiaries — genuine coordination gains, no concentrated extraction. Cross-boundary couples and occupational mobility seekers are the structural targets: they bear the switching cost of the boundary without capturing its coordination benefit, and their exit options are constrained-to-trapped because leaving the network entirely (not just the boundary) is usually the only clean exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding coordination problem (matching and mutual-aid provision across dispersed, otherwise-unverifiable populations) remains live per the corroborating anthropological record — this is not an atrophied constraint being defended out of pure inertia. Classifying it as rope rather than snare or tangled_rope prevents mislabeling a functioning, low-suppression coordination mechanism as pure extraction merely because it produces losers at its margins (boundary-crossers); conversely, retaining an omega on beneficiary capture prevents the reading from becoming an uncritical apologetic for a structure that some subcaste associations actively use to extract status rents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_ambiguity,
    'Which reading of jati_practice_norm best characterizes the operative structure at a given place and time — localized practice, orthodox textual, or colonial-census stabilization — and can a single empirical case instantiate more than one simultaneously?',
    'Comparative ethnographic and historical analysis distinguishing regions/periods where boundary practice shows active local renegotiation and proliferation from those where a fixed textual hierarchy or a colonial-era administrative category dominates lived practice.',
    'If a given empirical setting is better described by the orthodox_textual_reading or colonial_census_reading, the extraction and suppression metrics for that setting would be substantially higher than authored here; this story''s low-extraction rope classification is bounded to settings where the localized-practice structure genuinely dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Which kernel reading actually governs a given empirical instance of jati boundary maintenance.').

omega_variable(
    proliferation_as_weak_enforcement_signal,
    'Does the empirical proliferation of jati categories (3,000+) genuinely indicate weak, decentralized enforcement, or does it instead indicate successful fragmentation of resistance into smaller units each individually easier to police?',
    'Analysis of whether new sub-jati categories emerge primarily from bottom-up economic/occupational shifts (supporting the coordination reading) or from top-down administrative/associational status competition (which would support a more extractive reading even within this framing).',
    'If proliferation is driven by status-competition extraction rather than genuine coordination adaptation, effective extraction under this reading should be revised upward and some beneficiary groups reclassified as capturing rents from the boundary-setting process itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proliferation_as_weak_enforcement_signal, empirical, 'Whether category proliferation reflects genuine decentralization or fragmented extraction.').

omega_variable(
    boundary_crosser_suppression_mechanism,
    'Is the suppression experienced by cross-boundary couples and occupational mobility seekers structural (network withdrawal, economic sanction) or internalized (belief that crossing is itself illegitimate, persisting after network sanction is removed)?',
    'Post-exit trajectory analysis: track whether individuals who successfully cross boundaries and relocate away from the enforcing network continue to experience self-imposed constraint or fully normalize the crossing over time.',
    'If suppression is substantially internalized, the effective suppression borne by boundary-crossers is higher than the structural (network-based) measure alone suggests, and the rope classification for their seat specifically would understate their experienced cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(boundary_crosser_suppression_mechanism, empirical, 'Structural versus internalized suppression mechanism affecting boundary-crossers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__localized_practice_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_tr_t0, jati_practice_norm__localized_practice_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(jati_tr_t20, jati_practice_norm__localized_practice_reading, theater_ratio, 20, 0.13).
narrative_ontology:measurement(jati_tr_t40, jati_practice_norm__localized_practice_reading, theater_ratio, 40, 0.13).
narrative_ontology:measurement(jati_tr_t60, jati_practice_norm__localized_practice_reading, theater_ratio, 60, 0.14).
narrative_ontology:measurement(jati_tr_t80, jati_practice_norm__localized_practice_reading, theater_ratio, 80, 0.15).
narrative_ontology:measurement(jati_tr_t100, jati_practice_norm__localized_practice_reading, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(jati_be_t0, jati_practice_norm__localized_practice_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(jati_be_t20, jati_practice_norm__localized_practice_reading, base_extractiveness, 20, 0.2).
narrative_ontology:measurement(jati_be_t40, jati_practice_norm__localized_practice_reading, base_extractiveness, 40, 0.21).
narrative_ontology:measurement(jati_be_t60, jati_practice_norm__localized_practice_reading, base_extractiveness, 60, 0.22).
narrative_ontology:measurement(jati_be_t80, jati_practice_norm__localized_practice_reading, base_extractiveness, 80, 0.22).
narrative_ontology:measurement(jati_be_t100, jati_practice_norm__localized_practice_reading, base_extractiveness, 100, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(jati_practice_norm__localized_practice_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jati_practice_norm__localized_practice_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jati_practice_norm__localized_practice_reading, 0.08).
narrative_ontology:affects_constraint(jati_practice_norm__localized_practice_reading, jati_practice_norm__orthodox_textual_reading).
narrative_ontology:affects_constraint(jati_practice_norm__localized_practice_reading, jati_practice_norm__colonial_census_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the colloquial label 'jati boundaries' per the ε-invariance principle: localized_practice_reading (this file, rope, low ε), orthodox_textual_reading (higher ε, higher suppression, fixed-hierarchy reading), and colonial_census_reading (externally-imposed reification, different beneficiary set — administrative bureaucracies rather than local associations). All three are linked via network edges as siblings in the same contested kernel; none of the three ε values should be treated as convertible into another by changing the observable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
