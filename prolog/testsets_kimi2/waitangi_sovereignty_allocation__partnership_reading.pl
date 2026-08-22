% ============================================================================
% CONSTRAINT STORY: waitangi_sovereignty_allocation__partnership_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_waitangi_sovereignty_allocation__partnership_reading, []).

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
 *   constraint_id: waitangi_sovereignty_allocation__partnership_reading
 *   human_readable: Treaty of Waitangi Partnership Reading
 *   domain: constitutional law / indigenous rights / post-colonial governance
 *
 * SUMMARY:
 *   The partnership reading of the Treaty of Waitangi holds that the Treaty
 *   established an ongoing relationship of good faith partnership between the
 *   Crown and MÄori, requiring the Crown to consult and actively protect
 *   MÄori interests. Emerging from the 1987 Court of Appeal decision and
 *   subsequent Waitangi Tribunal jurisprudence, this reading has become the
 *   dominant framework for Treaty claims and settlements. It coordinates
 *   Crown-MÄori relations by channeling indigenous political claims into
 *   institutional processes, while asymmetrically preserving Crown
 *   sovereignty. The Crown benefits from legitimacy and maintained authority;
 *   MÄori communities bear the cost of subordinated sovereignty while
 *   receiving procedural rights and material redress.
 *
 * KEY AGENTS:
 *   - crown_government: Primary agenda-setter (institutional/constrained) â administers the partnership, retains parliamentary sovereignty, benefits from legitimacy
 *   - maori_iwi_hapu: Dual-positioned payer/beneficiary (organized/constrained) â receives redress and consultation but bears structural subordination
 *   - waitangi_tribunal: Analytical observer (institutional/analytical) â interprets Treaty principles, non-binding recommendations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__partnership_reading, 0.56).
domain_priors:suppression_score(waitangi_sovereignty_allocation__partnership_reading, 0.5).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__partnership_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, extractiveness, 0.56).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__partnership_reading, tangled_rope).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__partnership_reading, "Treaty of Waitangi Partnership Reading").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__partnership_reading, "constitutional law / indigenous rights / post-colonial governance").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__partnership_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__partnership_reading, '2f2ffe0e-a9f6-4781-8711-1a144636e8c7').
narrative_ontology:cs_kernel_codification('2f2ffe0e-a9f6-4781-8711-1a144636e8c7', fixed_text).
narrative_ontology:cs_authority_grounding('2f2ffe0e-a9f6-4781-8711-1a144636e8c7', lineage).
narrative_ontology:cs_interpretation_layer_present('2f2ffe0e-a9f6-4781-8711-1a144636e8c7').
narrative_ontology:cs_reading_relation('2f2ffe0e-a9f6-4781-8711-1a144636e8c7', waitangi_sovereignty_allocation__crown_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('2f2ffe0e-a9f6-4781-8711-1a144636e8c7', waitangi_sovereignty_allocation__rangatiratanga_reading, influences).
narrative_ontology:cs_axiom('2f2ffe0e-a9f6-4781-8711-1a144636e8c7', foundational, ongoing_partnership_reciprocal_duties).
narrative_ontology:cs_axiom_status(ongoing_partnership_reciprocal_duties, holdable).
narrative_ontology:cs_axiom_grounding('2f2ffe0e-a9f6-4781-8711-1a144636e8c7', ongoing_partnership_reciprocal_duties, conventional).
narrative_ontology:cs_axiom('2f2ffe0e-a9f6-4781-8711-1a144636e8c7', foundational, good_faith_consultation_principle).
narrative_ontology:cs_axiom_status(good_faith_consultation_principle, holdable).
narrative_ontology:cs_axiom_grounding('2f2ffe0e-a9f6-4781-8711-1a144636e8c7', good_faith_consultation_principle, conventional).
narrative_ontology:cs_reference_frame('2f2ffe0e-a9f6-4781-8711-1a144636e8c7', reciprocal_partnership_framework).
narrative_ontology:cs_drift_state('2f2ffe0e-a9f6-4781-8711-1a144636e8c7', contemporary_constitutional_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2f2ffe0e-a9f6-4781-8711-1a144636e8c7', '').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__partnership_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__partnership_reading, crown_government).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__partnership_reading, maori_iwi_hapu).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__partnership_reading, maori_iwi_hapu).
narrative_ontology:constraint_vindicates(waitangi_sovereignty_allocation__partnership_reading, treaty_principles_doctrine).
narrative_ontology:constraint_vindicates(waitangi_sovereignty_allocation__partnership_reading, bicultural_partnership_jurisprudence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the Treaty partnership framework through legislation, policy, and the settlement process. Controls parliamentary sovereignty and the Waitangi Tribunal's mandate. Bears consultation obligations and settlement fiscal costs, but retains ultimate constitutional authority and benefits from legitimacy and orderly governance.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, crown_government, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__partnership_reading, crown_government, beneficiary).

% Engage with Crown processes to seek redress for Treaty breaches and protect taonga. Receive settlements and consultation rights, but bear the structural cost of subordinated sovereignty and the resource burden of participating in Crown-designed consultation and settlement processes. Exit to full self-determination is blocked by the Crown's retention of parliamentary sovereignty.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, maori_iwi_hapu, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__partnership_reading, maori_iwi_hapu, beneficiary).

% Investigates Crown actions against Treaty principles and recommends redress. Operates as a quasi-judicial body within the Crown's constitutional structure; its recommendations are not binding on Parliament. Provides analytical legitimacy to the partnership reading but does not control enforcement.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, waitangi_tribunal, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(waitangi_sovereignty_allocation__partnership_reading, crown_government).
narrative_ontology:fixing_cost_class(waitangi_sovereignty_allocation__partnership_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a constitutional mechanism for managing post-colonial governance between a settler state and indigenous peoples, preventing violent conflict by channeling MÄori political claims into institutional processes (courts, Tribunal, settlements) rather than extra-constitutional resistance.
% TRANSFER_FUNCTION: Transfers authority from MÄori communities to Crown institutions in exchange for procedural rights (consultation) and material redress (settlements), while preserving ultimate Crown sovereignty.
% ABSENT_VOICES: MÄori advocates of full tino rangatiratanga who reject Crown sovereignty entirely, and Crown sovereigntists who reject any Treaty constraint on parliamentary supremacy, are both marginalized in the partnership frame; they are present in political discourse but excluded from the partnership's institutional logic, which requires both sides to accept the Crown as senior partner.
% DISAPPEARANCE_RATIONALE: The partnership reading underpins the Waitangi Tribunal, the Treaty settlement process, and statutory consultation requirements. Its disappearance would eliminate the legal obligation to consult and protect, collapsing the current architecture of redress and likely triggering a constitutional crisis as MÄori-Crown relations revert to unilateral Crown authority or open contestation.
% FOUNDING_PROBLEM: How to govern a bicultural polity after colonial annexation without ongoing warfare or complete subjugation, while retaining Crown/imperial authority.
% FOUNDING_PROBLEM_CORROBORATION: Waitangi Tribunal historians and MÄori scholars attest the founding problem persists; Crown government attests it is being resolved through the settlement process. International human rights monitors corroborate that structural inequities remain, supporting a contested status.
narrative_ontology:disappearance_verdict(waitangi_sovereignty_allocation__partnership_reading, world_rearranges).
narrative_ontology:founding_problem_status(waitangi_sovereignty_allocation__partnership_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(waitangi_sovereignty_allocation__partnership_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(waitangi_sovereignty_allocation__partnership_reading, 'none', 1).
narrative_ontology:epsilon_provenance(waitangi_sovereignty_allocation__partnership_reading, 0.56, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(waitangi_sovereignty_allocation__partnership_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(waitangi_sovereignty_allocation__partnership_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(waitangi_sovereignty_allocation__partnership_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.56) is moderate-high: the partnership channels MÄori claims into Crown-controlled processes that preserve ultimate Crown sovereignty, extracting political subordination. Suppression (0.50) is moderate: alternatives like the rangatiratanga reading are marginalized in constitutional practice but persist in political discourse. Theater ratio (0.40) reflects increasing proceduralism where consultation occurs but Crown decisions are rarely altered. Accessibility collapse (0.65) is high because once inside the partnership frame, full sovereignty appears legally impossible. Resistance (0.45) is moderate: MÄori political movements resist tokenism and push for co-governance. The temporal measurements show gradual intensification of extractiveness and theater as the settlement process matures and consultation hardens into ritual.
 *
 * PERSPECTIVAL GAP:
 *   From the Crown seat, the partnership is a generous coordination mechanism that shares power and provides redress within constitutional limits. From the MÄori payer seat, the same structure appears as managed subordination: the Crown sets the rules, controls the Tribunal's mandate, and retains parliamentary sovereignty. The engine computes this divergence from identical structural facts plus opposing beneficiary/victim declarations and constrained exits on both sides.
 *
 * DIRECTIONALITY LOGIC:
 *   The Crown is structurally near the beneficiary end despite being agenda-setter because it captures sovereignty and legitimacy (d low). MÄori iwi/hapu are structurally near the target end because they bear the cost of subordinated political authority (d high), even though they also receive settlements. The Tribunal sits at analytical distance (d near 0.5, non-extractive observer). No overrides are needed because beneficiary/victim declarations and exit options capture the structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â governing a bicultural polity without warfare â remains live in the sense that MÄori-Crown relations require ongoing management. However, the settlement process has been so lengthy and partial that the partnership risks becoming a perpetual mechanism for deferring final resolution. The mandatrophy is not yet resolved (status contested) because the constraint still performs genuine coordination (redress, consultation) even as it extracts. A pure snare classification would miss the real settlements and procedural gains; a pure rope would miss the sovereignty asymmetry. Tangled rope captures both.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consultation_theater_or_genuine,
    'Does Crown consultation under the partnership reading alter substantive Crown decisions, or does it function as procedural compliance theater that legitimates predetermined outcomes?',
    'Systematic outcome-tracking: compare Crown decisions with and without consultation across matched policy domains; measure MÄori signatory satisfaction with consultation influence.',
    'If theater dominates, theater_ratio and extractiveness should be revised upward, strengthening the tangled rope or snare classification. If genuine, coordination function is stronger, supporting a lower extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consultation_theater_or_genuine, empirical, 'Whether consultation is performative or substantive').

omega_variable(
    partnership_rangatiratanga_boundary,
    'Can the partnership reading and the rangatiratanga reading coexist as live normative frameworks for MÄori communities, or does adopting the partnership reading structurally foreclose full tino rangatiratanga?',
    'Survey of MÄori political attitudes and iwi constitutional documents to determine whether partnership and rangatiratanga are held as complementary or competing frames.',
    'If partnership forecloses rangatiratanga, the constraint''s suppression metric is higher than authored; if they coexist, the constraint functions more like a rope with high coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partnership_rangatiratanga_boundary, conceptual, 'Whether partnership and rangatiratanga are mutually exclusive').

omega_variable(
    crown_benefit_legitimacy_vs_extraction,
    'Is the Crown''s benefit from the partnership reading best understood as legitimate coordination rent (stability, order) or as extraction (sovereignty maintained without full consent)?',
    'Comparative constitutional analysis with other settler-indigenous treaty frameworks that have moved further toward shared sovereignty.',
    'If the Crown benefit is reclassified as extraction, the gain_flow tightens and the constraint''s tangled rope classification shifts toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(crown_benefit_legitimacy_vs_extraction, preference, 'Normative framing of Crown benefit').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__partnership_reading, 0, 48).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wait_tr_t0, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(wait_tr_t8, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(wait_tr_t16, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 16, 0.25).
narrative_ontology:measurement(wait_tr_t24, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 24, 0.3).
narrative_ontology:measurement(wait_tr_t32, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 32, 0.34).
narrative_ontology:measurement(wait_tr_t40, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(wait_tr_t48, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 48, 0.4).

% Extraction over time
narrative_ontology:measurement(wait_be_t0, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(wait_be_t8, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(wait_be_t16, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 16, 0.46).
narrative_ontology:measurement(wait_be_t24, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 24, 0.5).
narrative_ontology:measurement(wait_be_t32, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 32, 0.53).
narrative_ontology:measurement(wait_be_t40, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(wait_be_t48, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 48, 0.56).

% Suppression requirement over time
narrative_ontology:measurement(wait_su_t0, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(wait_su_t8, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 8, 0.28).
narrative_ontology:measurement(wait_su_t16, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 16, 0.35).
narrative_ontology:measurement(wait_su_t24, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 24, 0.42).
narrative_ontology:measurement(wait_su_t32, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 32, 0.46).
narrative_ontology:measurement(wait_su_t40, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 40, 0.48).
narrative_ontology:measurement(wait_su_t48, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 48, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__partnership_reading, crown_sovereignty_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__partnership_reading, rangatiratanga_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the waitangi_sovereignty_allocation kernel, decomposed per the Îµ-invariance principle because the English and MÄori texts support structurally distinct sovereignty claims with different Îµ profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
