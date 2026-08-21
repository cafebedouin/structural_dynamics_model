% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_reversal__exogenous_override_reading, []).

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
 *   constraint_id: marriage_commitment_reversal__exogenous_override_reading
 *   human_readable: LDS Marriage Commitment Reversal: Exogenous Override Reading
 *   domain: religious_institutional_history/commitment_systems/political_theology
 *
 * SUMMARY:
 *   This constraint describes the cessation of plural marriage in the Church
 *   of Jesus Christ of Latter-day Saints (LDS Church) as a direct result of
 *   overwhelming external coercion from the United States federal government,
 *   without a corresponding internal doctrinal revision. The reading
 *   emphasizes the federal government's extraction of institutional
 *   sovereignty and the church's strategic compliance to preserve its
 *   existence, while Section 132 (the doctrinal basis for plural marriage)
 *   remained unrenounced in scripture. This is one reading of the
 *   'marriage_commitment_reversal' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__exogenous_override_reading, 0.85).
domain_priors:suppression_score(marriage_commitment_reversal__exogenous_override_reading, 0.92).
domain_priors:theater_ratio(marriage_commitment_reversal__exogenous_override_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__exogenous_override_reading, snare).
narrative_ontology:human_readable(marriage_commitment_reversal__exogenous_override_reading, "LDS Marriage Commitment Reversal: Exogenous Override Reading").
narrative_ontology:topic_domain(marriage_commitment_reversal__exogenous_override_reading, "religious_institutional_history/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__exogenous_override_reading, '9bf7a14c-a06c-4545-a2b7-a85fc696fe69').
narrative_ontology:cs_kernel_codification('9bf7a14c-a06c-4545-a2b7-a85fc696fe69', fixed_text).
narrative_ontology:cs_authority_grounding('9bf7a14c-a06c-4545-a2b7-a85fc696fe69', extraction).
narrative_ontology:cs_interpretation_layer_present('9bf7a14c-a06c-4545-a2b7-a85fc696fe69').
narrative_ontology:cs_reading_relation('9bf7a14c-a06c-4545-a2b7-a85fc696fe69', marriage_commitment_reversal__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_reading_relation('9bf7a14c-a06c-4545-a2b7-a85fc696fe69', marriage_commitment_reversal__practice_doctrine_gap, influences).
narrative_ontology:cs_axiom('9bf7a14c-a06c-4545-a2b7-a85fc696fe69', foundational, federal_supremacy_over_religious_practice).
narrative_ontology:cs_axiom_status(federal_supremacy_over_religious_practice, holdable).
narrative_ontology:cs_axiom_grounding('9bf7a14c-a06c-4545-a2b7-a85fc696fe69', federal_supremacy_over_religious_practice, conventional).
narrative_ontology:cs_axiom('9bf7a14c-a06c-4545-a2b7-a85fc696fe69', foundational, divine_command_for_plural_marriage_unrevoked).
narrative_ontology:cs_axiom_status(divine_command_for_plural_marriage_unrevoked, holdable).
narrative_ontology:cs_axiom_grounding('9bf7a14c-a06c-4545-a2b7-a85fc696fe69', divine_command_for_plural_marriage_unrevoked, theological).
narrative_ontology:cs_reference_frame('9bf7a14c-a06c-4545-a2b7-a85fc696fe69', federal_territorial_sovereignty_asserted).
narrative_ontology:cs_drift_state('9bf7a14c-a06c-4545-a2b7-a85fc696fe69', post_manifesto_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('9bf7a14c-a06c-4545-a2b7-a85fc696fe69', '').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__exogenous_override_reading, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__exogenous_override_reading, federal_government_of_the_united_states).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, church_of_jesus_christ_of_latter_day_saints_institutional_sovereignty).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, polygamous_families).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__exogenous_override_reading, latter_day_saints_adherents).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__exogenous_override_reading, us_public_opinion).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, latter_day_saints_adherents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exerted immense legal and military pressure, including confiscation of church property and imprisonment of leaders, to force the cessation of polygamous marriages in US territories. Benefited from establishing federal supremacy over religious practice in civil matters.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, federal_government_of_the_united_states, agenda_setter,
    institutional, generational, arbitrage, national).

% Forced to publicly abandon the practice of plural marriage to avoid complete disincorporation and loss of temporal assets. The institution's autonomy was directly extracted by federal power, leading to a public suspension of practice while retaining the underlying doctrine.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, church_of_jesus_christ_of_latter_day_saints_institutional_sovereignty, payer,
    institutional, civilizational, trapped, regional).

% Individuals and families who had entered into plural marriages faced legal persecution, social ostracization, and the dismantling of their family structures. Their commitment was deeply tied to religious identity, making 'exit' from the practice a profound personal and spiritual crisis.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, polygamous_families, payer,
    powerless, biographical, identity_locked, local).

% Benefited from the church's continued legal existence and the cessation of federal persecution, allowing for the growth and normalization of the faith. However, they bore the cost of adapting to a new social and religious norm, often with internal conflict regarding the doctrinal shift.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, latter_day_saints_adherents, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__exogenous_override_reading, latter_day_saints_adherents, payer).

% Largely viewed polygamy as immoral and uncivilized, supporting federal intervention. Benefited from the perceived triumph of 'American values' and secular law over religious practice, reinforcing a particular vision of national identity and social order.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, us_public_opinion, beneficiary,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint coordinated the cessation of a religiously sanctioned practice (plural marriage) with the demands of federal law and prevailing social norms, allowing the LDS Church to survive as a legal entity within the United States.
% TRANSFER_FUNCTION: Transferred institutional autonomy and the right to practice plural marriage from the LDS Church to the federal government, in exchange for the church's continued legal existence and the cessation of federal persecution.
% ABSENT_VOICES: The voices of those who believed plural marriage was a divinely commanded practice, and who wished to continue living it, were suppressed by federal force and marginalized within the church's public discourse. Their theological and personal justifications for the practice were systematically ignored or criminalized.
% DISAPPEARANCE_RATIONALE: If the federal coercion had never occurred, the LDS Church would likely have continued the practice of plural marriage, leading to a fundamentally different institutional structure, legal status, and relationship with the US government. The social and political landscape of the American West would have been dramatically altered.
% FOUNDING_PROBLEM: The federal government perceived the practice of plural marriage by the LDS Church as a challenge to its sovereignty, a violation of 'civilized' norms, and an obstacle to Utah's statehood.
% FOUNDING_PROBLEM_CORROBORATION: Federal legislative records, Supreme Court rulings (e.g., Reynolds v. United States), and contemporary newspaper accounts from outside the LDS Church consistently corroborate the federal government's view of plural marriage as an intractable problem requiring coercive intervention. The problem was resolved by force, not by internal doctrinal shift.
narrative_ontology:disappearance_verdict(marriage_commitment_reversal__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_reversal__exogenous_override_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__exogenous_override_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(marriage_commitment_reversal__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_reversal__exogenous_override_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_reversal__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_reversal__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_reversal__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the federal government successfully forced the church to abandon a core practice, seizing property and imprisoning leaders. Suppression is extremely high (0.92) due to the full weight of federal legal and military power brought to bear. Theater ratio is high (0.60) because the public cessation of practice was a performance of compliance, while the underlying doctrine (Section 132) was preserved in scripture, creating a significant doctrine-practice gap. Resistance was fierce but ultimately overwhelmed by federal power.
 *
 * PERSPECTIVAL GAP:
 *   From the federal government's perspective, this was a necessary assertion of national sovereignty and moral order. From the LDS Church's perspective, it was a forced capitulation to preserve the institution, a 'snare' imposed by an external power. The engine's classification will reflect the high extraction and suppression from the victim seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal government is the clear beneficiary and agenda-setter, successfully imposing its will. The LDS institutional sovereignty and polygamous families are the primary victims, bearing the full cost of the coercion. Latter-day Saints adherents are beneficiaries in that the church survived, but also payers in adapting to the new reality. US public opinion benefited from the perceived victory of secular law.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_primacy_of_coercion,
    'To what extent was federal coercion the *sole* or *primary* cause of the cessation of plural marriage, as opposed to internal theological developments or a combination of factors?',
    'Counterfactual historical analysis: what would the church''s trajectory have been in the absence of federal pressure? Examination of internal church documents for evidence of theological shifts predating or independent of federal action.',
    'If coercion was less primary, this reading''s extractiveness and suppression metrics would be lower, potentially shifting its classification towards a ''tangled_rope'' or even ''rope'' if internal factors played a larger coordinating role. If coercion was overwhelmingly primary, the ''snare'' classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(causal_primacy_of_coercion, empirical, 'Assessing the causal weight of external coercion versus internal factors in the policy reversal.').

omega_variable(
    doctrine_practice_gap_persistence,
    'How long did the doctrine-practice gap persist, and what were its internal effects on LDS theology and identity?',
    'Textual analysis of subsequent LDS scriptural interpretations, sermons, and historical narratives regarding Section 132 and plural marriage. Ethnographic study of contemporary LDS communities'' understanding of the doctrine.',
    'If the gap persisted for a long time with significant internal tension, it reinforces the ''snare'' aspect of this reading, indicating an unaddressed internal contradiction. If the doctrine was effectively reinterpreted or de-emphasized internally, it would lend more credence to the ''endogenous_reinterpretation_reading'' sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_practice_gap_persistence, conceptual, 'The duration and impact of the gap between unrenounced doctrine and suspended practice.').

omega_variable(
    legitimacy_of_federal_intervention,
    'Was the federal government''s intervention a legitimate exercise of state power to enforce secular law, or an overreach infringing on religious freedom?',
    'Legal and political philosophy analysis of the boundaries of religious freedom and state sovereignty in a pluralistic society. Comparative analysis of similar historical cases of state-religion conflict.',
    'If deemed legitimate, the ''snare'' classification might be reframed as a ''tangled_rope'' from a broader societal perspective, acknowledging a coordination function for national unity. If deemed an overreach, the ''snare'' classification is strengthened, highlighting the unjust nature of the extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_of_federal_intervention, preference, 'Normative assessment of the federal government''s right to intervene in religious practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__exogenous_override_reading, 1862, 1890).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1862, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1862, 0.1).
narrative_ontology:measurement(marr_tr_t1870, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1870, 0.25).
narrative_ontology:measurement(marr_tr_t1880, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1880, 0.4).
narrative_ontology:measurement(marr_tr_t1890, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1890, 0.6).

% Extraction over time
narrative_ontology:measurement(marr_be_t1862, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1862, 0.6).
narrative_ontology:measurement(marr_be_t1870, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1870, 0.7).
narrative_ontology:measurement(marr_be_t1880, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1880, 0.8).
narrative_ontology:measurement(marr_be_t1890, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1890, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1862, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1862, 0.7).
narrative_ontology:measurement(marr_su_t1870, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1870, 0.8).
narrative_ontology:measurement(marr_su_t1880, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1880, 0.9).
narrative_ontology:measurement(marr_su_t1890, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1890, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_commitment_reversal__exogenous_override_reading, marriage_commitment_reversal__endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(marriage_commitment_reversal__exogenous_override_reading, marriage_commitment_reversal__practice_doctrine_gap).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'marriage_commitment_reversal' kernel. This 'exogenous_override_reading' emphasizes external coercion, while 'endogenous_reinterpretation_reading' focuses on internal theological shifts, and 'practice_doctrine_gap' highlights the enduring tension between doctrine and practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
