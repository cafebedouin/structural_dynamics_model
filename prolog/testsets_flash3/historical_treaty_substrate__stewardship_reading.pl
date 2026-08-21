% ============================================================================
% CONSTRAINT STORY: historical_treaty_substrate__stewardship_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_historical_treaty_substrate__stewardship_reading, []).

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
 *   constraint_id: historical_treaty_substrate__stewardship_reading
 *   human_readable: Historical Treaty Substrate (Stewardship Reading)
 *   domain: legal_anthropology/indigenous_law/comparative_constitutional_theory
 *
 * SUMMARY:
 *   This constraint represents the 'stewardship reading' of historical
 *   treaties, where they are understood as relational pacts for shared
 *   territorial stewardship, not as instruments of land cession. It
 *   emphasizes mutual obligations for coexistence and joint resource
 *   management, with no extinguishment of Indigenous sovereignty. This
 *   reading is distinct from others that view treaties as property
 *   transactions or purely international agreements, and it is actively
 *   advocated by Indigenous legal traditions and some progressive legal
 *   scholars.
 *
 * KEY AGENTS:
 *   - Indigenous_nations: Primary beneficiary/co-governor (organized/identity_locked)
 *   - settler_state_government: Agenda-setter/obligated party (institutional/constrained)
 *   - resource_extraction_industries: Payer (powerful/constrained)
 *   - legal_scholars_indigenous_law: Analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__stewardship_reading, 0.25).
domain_priors:suppression_score(historical_treaty_substrate__stewardship_reading, 0.15).
domain_priors:theater_ratio(historical_treaty_substrate__stewardship_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__stewardship_reading, rope).
narrative_ontology:human_readable(historical_treaty_substrate__stewardship_reading, "Historical Treaty Substrate (Stewardship Reading)").
narrative_ontology:topic_domain(historical_treaty_substrate__stewardship_reading, "legal_anthropology/indigenous_law/comparative_constitutional_theory").

domain_priors:requires_active_enforcement(historical_treaty_substrate__stewardship_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__stewardship_reading, '77495d03-7166-497c-8537-c5394ec11ae3').
narrative_ontology:cs_kernel_codification('77495d03-7166-497c-8537-c5394ec11ae3', formalized).
narrative_ontology:cs_authority_grounding('77495d03-7166-497c-8537-c5394ec11ae3', lineage).
narrative_ontology:cs_interpretation_layer_present('77495d03-7166-497c-8537-c5394ec11ae3').
narrative_ontology:cs_reading_relation('77495d03-7166-497c-8537-c5394ec11ae3', historical_treaty_substrate__extinguishment_reading, coexists_with).
narrative_ontology:cs_reading_relation('77495d03-7166-497c-8537-c5394ec11ae3', historical_treaty_substrate__nation_to_nation_reading, coexists_with).
narrative_ontology:cs_axiom('77495d03-7166-497c-8537-c5394ec11ae3', foundational, inherent_indigenous_sovereignty_persists).
narrative_ontology:cs_axiom_status(inherent_indigenous_sovereignty_persists, holdable).
narrative_ontology:cs_axiom_grounding('77495d03-7166-497c-8537-c5394ec11ae3', inherent_indigenous_sovereignty_persists, deontological).
narrative_ontology:cs_axiom('77495d03-7166-497c-8537-c5394ec11ae3', foundational, treaties_as_relational_pacts_for_coexistence).
narrative_ontology:cs_axiom_status(treaties_as_relational_pacts_for_coexistence, holdable).
narrative_ontology:cs_axiom_grounding('77495d03-7166-497c-8537-c5394ec11ae3', treaties_as_relational_pacts_for_coexistence, conventional).
narrative_ontology:cs_reference_frame('77495d03-7166-497c-8537-c5394ec11ae3', original_relational_intent).
narrative_ontology:cs_drift_state('77495d03-7166-497c-8537-c5394ec11ae3', contemporary_legal_discourse, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('77495d03-7166-497c-8537-c5394ec11ae3', '').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__stewardship_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__stewardship_reading, indigenous_nations).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__stewardship_reading, settler_state_citizens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(historical_treaty_substrate__stewardship_reading, resource_extraction_industries).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__stewardship_reading, relational_governance_principle).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__stewardship_reading, inherent_indigenous_sovereignty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain inherent jurisdiction over traditional territories, participating in shared governance and resource management. Their identity is deeply tied to the land and treaty relationships, making 'exit' from the treaty framework a form of cultural and political dissolution.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, indigenous_nations, beneficiary,
    organized, generational, identity_locked, regional).

% Has mutual obligations for coexistence and shared stewardship, requiring consent for resource development and joint decision-making. Benefits from stable, legitimate governance across the entire territory. Exit means repudiating foundational legal principles and risking social instability.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, settler_state_government, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from peaceful coexistence and sustainable resource management, contributing to a shared future. Their benefits are diffuse and long-term, tied to the stability of the overall governance framework.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, settler_state_citizens, beneficiary,
    moderate, biographical, mobile, national).

% Must seek consent and adhere to jointly developed stewardship plans, incurring costs for consultation and environmental protection. Their operations are directly impacted by shared governance requirements. Exit means losing access to resources in treaty territories.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, resource_extraction_industries, payer,
    powerful, immediate, constrained, regional).

% Analyze the historical and contemporary application of treaties through the lens of relational stewardship, advocating for legal and policy reforms that align with this reading. Their work influences judicial and legislative discourse.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, legal_scholars_indigenous_law, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for shared territorial stewardship and peaceful coexistence between Indigenous nations and the settler state, ensuring mutual benefit and sustainable resource management.
% TRANSFER_FUNCTION: Facilitates the transfer of responsibilities for shared governance and resource management, from unilateral settler state control to joint decision-making with Indigenous nations. It also implies a transfer of benefits from sustainable practices to all inhabitants.
% ABSENT_VOICES: Those who view treaties solely as historical land sales or as instruments of extinguishment are structurally excluded from this reading's framework; they would argue against shared governance and for unilateral state authority.
% DISAPPEARANCE_RATIONALE: If this reading of treaties vanished, the legal and political landscape would be fundamentally altered. Indigenous claims to jurisdiction and co-management would be undermined, leading to increased conflict over land and resources, and a breakdown of reconciliation efforts. The settler state would lose a crucial basis for its own legitimacy in these territories.
% FOUNDING_PROBLEM: The original problem was how to establish peaceful relations and share vast territories between distinct peoples, ensuring mutual survival and prosperity without one subsuming the other.
% FOUNDING_PROBLEM_CORROBORATION: Indigenous elders and legal scholars consistently attest that the original problem of coexistence and shared stewardship remains live, requiring ongoing relational engagement. Some progressive settler legal scholars and government officials also corroborate this, acknowledging the historical failure to uphold the spirit of these agreements.
narrative_ontology:disappearance_verdict(historical_treaty_substrate__stewardship_reading, world_rearranges).
narrative_ontology:founding_problem_status(historical_treaty_substrate__stewardship_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(historical_treaty_substrate__stewardship_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(historical_treaty_substrate__stewardship_reading, 'none', 1).
narrative_ontology:epsilon_provenance(historical_treaty_substrate__stewardship_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(historical_treaty_substrate__stewardship_reading_tests).
:- end_tests(historical_treaty_substrate__stewardship_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.25) is relatively low because this reading posits a genuine coordination function with mutual benefits, but acknowledges some historical and ongoing friction in implementation. Suppression (0.15) is also low, reflecting that this reading is actively resisted by other interpretations rather than coercively enforced. Theater ratio (0.1) is minimal, as the core tenets of shared stewardship are meant to be genuinely implemented, not merely performed. The metrics reflect the ideal of this reading, not the historical reality of its suppression by other readings.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Indigenous nations, this reading is a vindication of their inherent rights and a pathway to genuine self-determination. From the settler state's perspective, it represents a complex, ongoing obligation that challenges historical assumptions of unilateral authority. Resource industries see it as an added cost and regulatory burden. The engine will compute these divergences based on the declared roles and positional atoms.
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous nations are beneficiaries (d near 0.0) as their inherent jurisdiction is affirmed and they gain co-management authority. The settler state government is an agenda-setter with significant obligations, making its directionality closer to symmetric (d near 0.5) as it gains legitimacy but bears costs of shared governance. Resource industries are payers (d near 1.0) as they face increased regulatory and consent costs. Settler citizens are diffuse beneficiaries (d near 0.0) from long-term stability and sustainability.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading actively counters mandatrophy by re-asserting the original, relational mandate of treaties against interpretations that would render them obsolete or purely extractive. It argues that the 'founding problem' of coexistence is still live and requires ongoing, active coordination, preventing the constraint from decaying into a piton or snare by re-legitimizing its coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_legitimacy_contest,
    'Is the ''stewardship reading'' a legitimate interpretation of historical treaties, or a contemporary re-framing imposed on historical documents?',
    'Extensive historical and legal anthropological research into Indigenous legal traditions and oral histories contemporaneous with treaty making, alongside comparative analysis of treaty interpretation in other jurisdictions.',
    'If deemed a legitimate historical interpretation, it strengthens the legal and moral imperative for shared governance. If seen as a modern imposition, its legal force is weakened, potentially reclassifying it as a preference-based scaffold rather than a foundational rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_legitimacy_contest, conceptual, 'Contest over the historical legitimacy of the stewardship reading.').

omega_variable(
    implementation_gap_empirical,
    'To what extent is the ''stewardship reading'' actually implemented in practice, versus remaining an aspirational legal theory?',
    'Empirical studies of co-management agreements, resource revenue sharing, and Indigenous consent processes in treaty territories, measuring actual shifts in power and decision-making authority.',
    'A significant implementation gap would indicate that the constraint''s effective extractiveness is higher than its base value suggests, as the benefits of shared stewardship are not fully realized, potentially pushing it towards a tangled_rope or even snare in practice, despite its claimed rope status.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(implementation_gap_empirical, empirical, 'Gap between theoretical stewardship reading and practical implementation.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal/institutional barriers) or internalized (cognitive patterns that persist after barrier removal)?',
    'Post-judicial-victory trajectory: if Indigenous nations continue to face resistance to shared governance even after favorable court rulings, reclassify as partially internalized suppression within settler institutions.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the settler state carries the suppression with them after legal challenges, requiring more active resistance from Indigenous nations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in treaty implementation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__stewardship_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hist_tr_t0, historical_treaty_substrate__stewardship_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(hist_tr_t25, historical_treaty_substrate__stewardship_reading, theater_ratio, 25, 0.08).
narrative_ontology:measurement(hist_tr_t50, historical_treaty_substrate__stewardship_reading, theater_ratio, 50, 0.1).
narrative_ontology:measurement(hist_tr_t75, historical_treaty_substrate__stewardship_reading, theater_ratio, 75, 0.09).
narrative_ontology:measurement(hist_tr_t100, historical_treaty_substrate__stewardship_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(hist_be_t0, historical_treaty_substrate__stewardship_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(hist_be_t25, historical_treaty_substrate__stewardship_reading, base_extractiveness, 25, 0.22).
narrative_ontology:measurement(hist_be_t50, historical_treaty_substrate__stewardship_reading, base_extractiveness, 50, 0.25).
narrative_ontology:measurement(hist_be_t75, historical_treaty_substrate__stewardship_reading, base_extractiveness, 75, 0.24).
narrative_ontology:measurement(hist_be_t100, historical_treaty_substrate__stewardship_reading, base_extractiveness, 100, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(hist_su_t0, historical_treaty_substrate__stewardship_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(hist_su_t25, historical_treaty_substrate__stewardship_reading, suppression_requirement, 25, 0.12).
narrative_ontology:measurement(hist_su_t50, historical_treaty_substrate__stewardship_reading, suppression_requirement, 50, 0.15).
narrative_ontology:measurement(hist_su_t75, historical_treaty_substrate__stewardship_reading, suppression_requirement, 75, 0.14).
narrative_ontology:measurement(hist_su_t100, historical_treaty_substrate__stewardship_reading, suppression_requirement, 100, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(historical_treaty_substrate__stewardship_reading, attachment_coordination).
narrative_ontology:affects_constraint(historical_treaty_substrate__stewardship_reading, historical_treaty_substrate__extinguishment_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__stewardship_reading, historical_treaty_substrate__nation_to_nation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'historical_treaty_substrate' kernel. Its ε value and structural properties differ significantly from the 'extinguishment_reading' (high extraction, snare) and 'nation_to_nation_reading' (moderate extraction, tangled_rope), which are modeled as separate constraints. This reading emphasizes relationality and shared stewardship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
