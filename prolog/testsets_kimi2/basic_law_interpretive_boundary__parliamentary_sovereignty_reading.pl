% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__parliamentary_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_boundary__parliamentary_sovereignty_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: basic_law_interpretive_boundary__parliamentary_sovereignty_reading
 *   human_readable: Knesset Parliamentary Sovereignty over Basic Law Interpretation
 *   domain: constitutional_law/comparative_constitutionalism
 *
 * SUMMARY:
 *   This constraint story instantiates the parliamentary sovereignty reading
 *   of the basic_law_interpretive_boundary kernel in Israeli constitutional
 *   law. Under this reading, the Knesset as the continuously elected
 *   representative body retains ultimate authority to interpret and amend
 *   Basic Laws by simple majority, and judicial review is advisory rather
 *   than binding. The constraint coordinates constitutional authority by
 *   assigning final interpretive power to the democratically elected
 *   legislature, yielding a near-zero extraction profile from the reading's
 *   own perspective. The constraint is claimed as rope â a coordination
 *   mechanism for democratic sovereignty â while the metrics independently
 *   describe a low but non-zero extraction environment with moderate
 *   resistance from institutional rivals. Sibling readings
 *   (judicial_supremacy_reading, balanced_contestation_reading) are modeled
 *   as separate constraints.
 *
 * KEY AGENTS:
 *   - knesset_majority: Primary agenda-setter (institutional/arbitrage) â exercises ultimate interpretive and amendatory authority over Basic Laws
 *   - israeli_electorate: Diffuse beneficiary (organized/constrained) â channels democratic sovereignty through periodic elections
 *   - supreme_court: Institutional observer (institutional/constrained) â retains advisory interpretive capacity but lacks binding veto under this reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.12).
domain_priors:suppression_score(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.2).
domain_priors:theater_ratio(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, rope).
narrative_ontology:human_readable(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, "Knesset Parliamentary Sovereignty over Basic Law Interpretation").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, "constitutional_law/comparative_constitutionalism").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 'c3279747-05d7-42a7-97f0-25f4d7a7c590').
narrative_ontology:cs_kernel_codification('c3279747-05d7-42a7-97f0-25f4d7a7c590', formalized).
narrative_ontology:cs_authority_grounding('c3279747-05d7-42a7-97f0-25f4d7a7c590', lineage).
narrative_ontology:cs_reading_relation('c3279747-05d7-42a7-97f0-25f4d7a7c590', basic_law_interpretive_boundary__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('c3279747-05d7-42a7-97f0-25f4d7a7c590', basic_law_interpretive_boundary__balanced_contestation_reading, coexists_with).
narrative_ontology:cs_axiom('c3279747-05d7-42a7-97f0-25f4d7a7c590', foundational, knesset_constituent_power_continuous).
narrative_ontology:cs_axiom_status(knesset_constituent_power_continuous, holdable).
narrative_ontology:cs_axiom_grounding('c3279747-05d7-42a7-97f0-25f4d7a7c590', knesset_constituent_power_continuous, conventional).
narrative_ontology:cs_axiom('c3279747-05d7-42a7-97f0-25f4d7a7c590', foundational, simple_majority_amendment_sovereignty).
narrative_ontology:cs_axiom_status(simple_majority_amendment_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('c3279747-05d7-42a7-97f0-25f4d7a7c590', simple_majority_amendment_sovereignty, conventional).
narrative_ontology:cs_reference_frame('c3279747-05d7-42a7-97f0-25f4d7a7c590', continuous_democratic_constituency).
narrative_ontology:cs_drift_state('c3279747-05d7-42a7-97f0-25f4d7a7c590', post_constitutional_revolution_1992, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c3279747-05d7-42a7-97f0-25f4d7a7c590', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, knesset_majority).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, israeli_electorate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds elected office and commands the legislative majority required to enact or amend Basic Laws by simple majority vote. Exercises ultimate interpretive authority over the constitutional text and may override judicial review through ordinary legislation. Exit from this position is via electoral loss or coalition collapse, but while in power the majority sets the constitutional agenda without external institutional veto.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, knesset_majority, agenda_setter,
    institutional, generational, arbitrage, national).

% Votes in regular parliamentary elections to determine the composition of the Knesset. Under this reading, democratic sovereignty flows directly from the electorate to its parliamentary representatives, with no unelected filter. Electoral change is the primary mechanism for altering constitutional direction; emigration is the only full exit from the national constitutional order.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, israeli_electorate, beneficiary,
    organized, biographical, constrained, national).

% Continues to hear constitutional challenges and interpret Basic Laws, but under this reading its determinations are advisory rather than binding on the Knesset. The Court may issue reasoned opinions and temporary injunctions, yet the Knesset retains the formal power to override or disregard them by ordinary majority. Judicial independence is maintained in day-to-day adjudication but not in the final constitutional say.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, supreme_court, observer,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the constitutional coordination problem of assigning final interpretive authority in a democratic system lacking a formal constituent assembly or entrenched amendment procedure, by channeling all constitutional power through the continuously elected parliament.
% TRANSFER_FUNCTION: Moves final constitutional interpretive and amendatory authority from the judiciary and any extra-parliamentary body to the Knesset majority; moves political accountability for constitutional choices from insulated judicial reasoning to the electoral process.
% ABSENT_VOICES: Judicial supremacy advocates, minority rights organizations, and international human rights bodies would argue that unchecked parliamentary sovereignty endangers entrenched rights and the rule of law; they are present in public discourse but structurally excluded from the final interpretive boundary under this reading.
% DISAPPEARANCE_RATIONALE: If the Knesset's ultimate authority over Basic Law interpretation disappeared overnight, judicial review would become binding, the Supreme Court would assume final constitutional arbiter status, and the legislative process would operate under judicial oversight â the institutional hierarchy would invert.
% FOUNDING_PROBLEM: The absence of a clear democratic sovereign with continuous constitutional authority in a state founded without a single constitutional moment or ratified constitution; the need to locate constituent power in an ongoing democratic institution rather than an unelected court or frozen text.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and constitutional scholars outside the parliamentary majority attest to the historical ambiguity of constituent power in Israel's transition from the Constituent Assembly to the First Knesset. However, liberal constitutionalists and civil society organizations contest that this founding problem remains best solved by unchecked parliamentary sovereignty, arguing that the historical moment has passed and institutional balance is now required.
narrative_ontology:disappearance_verdict(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.12, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_boundary__parliamentary_sovereignty_reading_tests).
:- end_tests(basic_law_interpretive_boundary__parliamentary_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.12 to reflect the reading's near-zero assessment while acknowledging friction from international treaty obligations and the institutional costs imposed on judicial review advocates. Suppression (0.20) is low-moderate: the constraint's persistence relies primarily on electoral legitimation rather than coercion, though political pressure on the Court increases during constitutional crises. Theater ratio (0.20) captures the growing performative dimension of sovereignty assertions during constitutional overhaul debates without dominating the functional reality of legislative procedure. Accessibility collapse (0.70) is relatively high because, within this reading's framework, the logic of parliamentary sovereignty is self-evident to adherents and alternatives collapse quickly once the majoritarian premise is accepted. Resistance (0.40) reflects the substantial institutional and social pushback from judicial supremacy advocates and civil society.
 *
 * PERSPECTIVAL GAP:
 *   The Knesset majority seat experiences this constraint as the natural order of democratic accountability â low extraction, legitimate coordination. The Supreme Court seat, were it classified as payer, would experience the same arrangement as institutional demotion, but under this reading it is assigned observer status because the reading denies that the Court ever held legitimate final authority. The engine computes this divergence from role declarations: only the Knesset majority and electorate are declared beneficiaries; no victim group is declared, keeping the structural derivation aligned with the reading's self-understanding.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary declarations only: knesset_majority and israeli_electorate are declared beneficiaries, pushing their d values toward the beneficiary end (low d). The Supreme Court is not declared a victim, so its d remains at the canonical fallback for institutional observers rather than being driven toward the target end. This accurately reflects the reading's structural assertion that sovereignty flows from electorate to Knesset without extraction from other state organs.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling this constraint as extraction by requiring declared victims for snare or tangled_rope classification. From the parliamentary sovereignty reading, there are no legitimate victims â the judiciary's advisory role is proper, not confiscatory. The rope classification is gated on beneficiaries without victims, which matches the reading's coordination story. If future events produce identifiable concentrated costs (e.g., specific minority groups systematically harmed by override legislation), the reading would need to be reassessed, potentially as tangled_rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Does the parliamentary sovereignty reading describe an intrinsic feature of Israel''s constitutional order, or is it one contested construction among several equally plausible framings?',
    'Comparative constitutional analysis across jurisdictions with similar unwritten or basic-law structures; examination of which reading better fits the totality of Israel''s constitutional enactments, political practice, and jurisprudential history.',
    'If the reading is one construction among many, its low epsilon claim reflects normative preference rather than structural necessity, and the constraint may compute differently under sibling readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether this reading is the unique correct framing or one contestable construction').

omega_variable(
    international_treaty_sovereignty_gap,
    'How does the exception for international treaty obligations affect the coherence of the parliamentary sovereignty claim â does it acknowledge a higher-order constraint that undermines simple-majority supremacy?',
    'Analysis of Knesset practice regarding treaty override, monist-dualist doctrinal positions, and whether international obligations are treated as self-executing limits or mere policy considerations.',
    'If international obligations function as binding higher-order constraints, the Knesset is not fully sovereign and the constraint''s extraction profile becomes more complex, potentially introducing tangled_rope features.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_treaty_sovereignty_gap, empirical, 'Whether international law creates an extraction stream from the Knesset itself').

omega_variable(
    judicial_advisory_vs_veto_status,
    'Does the Supreme Court''s loss of binding review power under this reading constitute a genuine institutional cost (extraction), or merely the restoration of its proper advisory role?',
    'Comparative historical analysis of the Court''s self-understanding and public legitimacy before and after the constitutional revolution of 1992; examination of whether the Court''s post-1992 review powers were widely understood as provisional or permanent.',
    'If the Court''s veto power was historically contingent, its removal is not extraction and the rope classification holds; if entrenched in institutional expectations, its removal extracts from the Court and raises effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_advisory_vs_veto_status, conceptual, 'Whether judicial subordination is cost-restoration or extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(blip_psr_tr_t0, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(blip_psr_tr_t6, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 6, 0.08).
narrative_ontology:measurement(blip_psr_tr_t12, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 12, 0.1).
narrative_ontology:measurement(blip_psr_tr_t18, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 18, 0.13).
narrative_ontology:measurement(blip_psr_tr_t24, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 24, 0.16).
narrative_ontology:measurement(blip_psr_tr_t30, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 30, 0.2).

% Extraction over time
narrative_ontology:measurement(blip_psr_be_t0, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(blip_psr_be_t6, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 6, 0.09).
narrative_ontology:measurement(blip_psr_be_t12, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 12, 0.1).
narrative_ontology:measurement(blip_psr_be_t18, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 18, 0.1).
narrative_ontology:measurement(blip_psr_be_t24, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 24, 0.11).
narrative_ontology:measurement(blip_psr_be_t30, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 30, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(blip_psr_su_t0, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(blip_psr_su_t6, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 6, 0.08).
narrative_ontology:measurement(blip_psr_su_t12, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 12, 0.11).
narrative_ontology:measurement(blip_psr_su_t18, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 18, 0.14).
narrative_ontology:measurement(blip_psr_su_t24, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 24, 0.17).
narrative_ontology:measurement(blip_psr_su_t30, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 30, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, balanced_contestation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the basic_law_interpretive_boundary kernel, which decomposes into three structurally distinct constraints due to epsilon-invariance failure across readings. Each reading authors a different epsilon, beneficiary/victim structure, and claimed type from the same constitutional text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
