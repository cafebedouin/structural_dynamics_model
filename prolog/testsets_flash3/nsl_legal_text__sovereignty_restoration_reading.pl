% ============================================================================
% CONSTRAINT STORY: nsl_legal_text__sovereignty_restoration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nsl_legal_text__sovereignty_restoration_reading, []).

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
 *   constraint_id: nsl_legal_text__sovereignty_restoration_reading
 *   human_readable: NSL as Sovereign Security Instrument (Sovereignty Restoration Reading)
 *   domain: constitutional_law/political_sociology/international_relations
 *
 * SUMMARY:
 *   This constraint story represents the 'sovereignty restoration' reading of
 *   the National Security Law (NSL) enacted in 2020. From this perspective,
 *   the NSL is a legitimate instrument for the central government to reassert
 *   its sovereign authority and restore constitutional order following the
 *   widespread unrest and protests of 2019. It is viewed as a necessary
 *   measure to safeguard national security and ensure stability, targeting
 *   political opposition and activists as security threats. The claimed type
 *   is 'tangled_rope' because it purports to coordinate security and
 *   governance while demonstrably extracting from specific groups.
 *
 * KEY AGENTS:
 *   - central_government_authorities: Primary agenda-setter (institutional/arbitrage) — benefits from increased control.
 *   - local_pro_establishment_factions: Beneficiary (organized/mobile) — aligns with central government, gains influence.
 *   - pro_democracy_activists: Primary target (powerless/trapped) — bears direct suppression and legal penalties.
 *   - political_opposition: Payer (moderate/constrained) — experiences narrowing political space and legal risks.
 *   - international_observers: Analytical observer (analytical/analytical) — monitors and critiques the NSL's impact.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__sovereignty_restoration_reading, 0.45).
domain_priors:suppression_score(nsl_legal_text__sovereignty_restoration_reading, 0.7).
domain_priors:theater_ratio(nsl_legal_text__sovereignty_restoration_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__sovereignty_restoration_reading, tangled_rope).
narrative_ontology:human_readable(nsl_legal_text__sovereignty_restoration_reading, "NSL as Sovereign Security Instrument (Sovereignty Restoration Reading)").
narrative_ontology:topic_domain(nsl_legal_text__sovereignty_restoration_reading, "constitutional_law/political_sociology/international_relations").

domain_priors:requires_active_enforcement(nsl_legal_text__sovereignty_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__sovereignty_restoration_reading, '25baca84-9a76-47eb-8acc-2dd92eda7938').
narrative_ontology:cs_kernel_codification('25baca84-9a76-47eb-8acc-2dd92eda7938', formalized).
narrative_ontology:cs_authority_grounding('25baca84-9a76-47eb-8acc-2dd92eda7938', lineage).
narrative_ontology:cs_interpretation_layer_present('25baca84-9a76-47eb-8acc-2dd92eda7938').
narrative_ontology:cs_reading_relation('25baca84-9a76-47eb-8acc-2dd92eda7938', nsl_legal_text__democratic_enclosure_reading, coexists_with).
narrative_ontology:cs_reading_relation('25baca84-9a76-47eb-8acc-2dd92eda7938', nsl_legal_text__jurisdictional_capture_reading, coexists_with).
narrative_ontology:cs_axiom('25baca84-9a76-47eb-8acc-2dd92eda7938', foundational, national_sovereignty_is_paramount).
narrative_ontology:cs_axiom_status(national_sovereignty_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('25baca84-9a76-47eb-8acc-2dd92eda7938', national_sovereignty_is_paramount, deontological).
narrative_ontology:cs_axiom('25baca84-9a76-47eb-8acc-2dd92eda7938', secondary, stability_precedes_liberal_freedoms).
narrative_ontology:cs_axiom_status(stability_precedes_liberal_freedoms, holdable).
narrative_ontology:cs_axiom_grounding('25baca84-9a76-47eb-8acc-2dd92eda7938', stability_precedes_liberal_freedoms, instrumental).
narrative_ontology:cs_reference_frame('25baca84-9a76-47eb-8acc-2dd92eda7938', pre_2019_constitutional_stability).
narrative_ontology:cs_drift_state('25baca84-9a76-47eb-8acc-2dd92eda7938', post_2019_unrest_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('25baca84-9a76-47eb-8acc-2dd92eda7938', '').
narrative_ontology:cs_kernel_id(nsl_legal_text__sovereignty_restoration_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__sovereignty_restoration_reading, central_government_authorities).
narrative_ontology:constraint_beneficiary(nsl_legal_text__sovereignty_restoration_reading, local_pro_establishment_factions).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, pro_democracy_activists).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, political_opposition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts and enforces the NSL, framing it as a necessary measure to restore stability and constitutional order after periods of unrest. Benefits from increased control over political dissent and perceived enhanced national security.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, central_government_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Aligns with the central government's narrative, benefiting from the suppression of opposition and the return to a 'stable' environment. Gains political influence and economic opportunities under the new order.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, local_pro_establishment_factions, beneficiary,
    organized, biographical, mobile, local).

% Are directly targeted by the NSL, facing arrest, prosecution, and severe penalties for activities previously considered legitimate protest. Their ability to organize and express dissent is severely curtailed, leading to self-censorship or exile.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, pro_democracy_activists, payer,
    powerless, immediate, trapped, local).

% Experiences a narrowing of political space, with increased scrutiny and potential legal repercussions for their activities. While not always directly targeted, the chilling effect of the NSL limits their operational capacity and public support.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, political_opposition, payer,
    moderate, biographical, constrained, local).

% Monitor the implementation of the NSL, assessing its impact on human rights, rule of law, and autonomy. Their analysis often contrasts with the official narrative, highlighting concerns about civil liberties and political freedoms.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, international_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate security efforts and legal responses to perceived threats to national unity and constitutional order, ensuring a unified approach to governance and public safety.
% TRANSFER_FUNCTION: Transfers legal authority and enforcement power from local autonomy to central government control, along with the suppression of political dissent and the redefinition of 'security threats'.
% ABSENT_VOICES: Independent legal scholars and human rights advocates, who would argue against the broad scope and vague definitions within the NSL, are largely excluded from the official discourse, their critiques dismissed as external interference.
% DISAPPEARANCE_RATIONALE: If the NSL vanished overnight, the central government's perceived authority over security matters would be challenged, political opposition and activist movements would likely re-emerge, and the legal landscape would revert to a more autonomous, common-law-centric system, leading to significant political and legal reorganization.
% FOUNDING_PROBLEM: The central government perceived a severe threat to national security and constitutional order following widespread social unrest and protests in 2019, which it viewed as undermining stability and sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: Central government authorities and state media consistently attest that the founding problem of national security threats remains live. This is contested by international legal bodies and human rights organizations, who argue the unrest was a symptom of governance issues, not a direct threat to sovereignty, and that the NSL has exacerbated underlying tensions rather than resolved them.
narrative_ontology:disappearance_verdict(nsl_legal_text__sovereignty_restoration_reading, world_rearranges).
narrative_ontology:founding_problem_status(nsl_legal_text__sovereignty_restoration_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nsl_legal_text__sovereignty_restoration_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(nsl_legal_text__sovereignty_restoration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nsl_legal_text__sovereignty_restoration_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nsl_legal_text__sovereignty_restoration_reading_tests).
:- end_tests(nsl_legal_text__sovereignty_restoration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate from this reading's perspective, as it is framed as targeting specific 'threats' rather than the general population, but still imposes significant costs on political dissent. Suppression (0.70) is high, reflecting the active enforcement mechanisms and the chilling effect on opposition. Theater ratio (0.20) is low, as the security function is genuinely pursued, even if its scope is contested. Accessibility collapse (0.60) is substantial, as avenues for protest and political expression are significantly curtailed. Resistance (0.50) is moderate, reflecting ongoing, albeit suppressed, opposition and international criticism.
 *
 * PERSPECTIVAL GAP:
 *   The central government and its allies perceive the NSL as a necessary and legitimate coordination mechanism for national security. In contrast, the targeted activists and political opposition experience it as a purely extractive and suppressive instrument. The engine's per-seat classification will reflect this divergence, with beneficiaries seeing a 'rope' or 'scaffold' and victims experiencing a 'snare' or 'tangled_rope'.
 *
 * DIRECTIONALITY LOGIC:
 *   Central government authorities are clear beneficiaries, as the NSL directly enhances their power and control. Local pro-establishment factions also benefit from the stability and suppression of their rivals. Pro-democracy activists and the political opposition are the primary targets, bearing the direct costs of legal enforcement and curtailed freedoms. International observers maintain an analytical distance, neither directly benefiting nor being targeted by the constraint's internal operation.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading frames the NSL as a response to a live founding problem (national security threats). The classification as 'tangled_rope' acknowledges the claimed coordination function (restoring order) while highlighting the asymmetric extraction from political opposition. This prevents mislabeling it as a pure 'rope' (ignoring extraction) or a pure 'snare' (ignoring the claimed security mandate). The 'live' status of the founding problem, combined with 'world_rearranges' on disappearance, suggests that from this reading's perspective, the constraint is still functionally relevant, even if its methods are contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nsl_necessity_vs_overreach,
    'Is the NSL a proportionate and necessary response to genuine national security threats, or does it represent an overreach of central authority that criminalizes legitimate political expression?',
    'Independent judicial review by an internationally recognized court, or a comprehensive, transparent assessment of the actual threats versus the scope and impact of the law''s enforcement.',
    'If deemed overreach, the constraint''s extractiveness and suppression would be re-evaluated as higher, potentially shifting its classification towards a ''snare'' from a neutral observer''s perspective. If deemed necessary, the ''tangled_rope'' classification would be reinforced, with the coordination function emphasized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nsl_necessity_vs_overreach, empirical, 'Ambiguity regarding the proportionality and necessity of the NSL''s provisions.').

omega_variable(
    constitutional_order_definition,
    'What constitutes ''constitutional order'' in this context? Is it a return to pre-2019 stability, or a redefinition that prioritizes central government control over local autonomy and civil liberties?',
    'Analysis of legal precedents and legislative intent, combined with a comparison of the pre- and post-NSL legal frameworks, focusing on the balance of powers and rights.',
    'If ''constitutional order'' is redefined to prioritize central control, the ''sovereignty restoration'' reading becomes more self-serving, increasing the perceived extraction from local autonomy. If it genuinely restores a prior, balanced order, the coordination aspect is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_order_definition, conceptual, 'The contested definition of ''constitutional order'' itself.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__sovereignty_restoration_reading, 2019, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(nsl__be_t2019, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 2019, 0.3).
narrative_ontology:measurement(nsl__be_t2020, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 2020, 0.4).
narrative_ontology:measurement(nsl__be_t2021, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 2021, 0.45).
narrative_ontology:measurement(nsl__be_t2022, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 2022, 0.45).
narrative_ontology:measurement(nsl__be_t2023, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 2023, 0.45).
narrative_ontology:measurement(nsl__be_t2024, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(nsl__su_t2019, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 2019, 0.5).
narrative_ontology:measurement(nsl__su_t2020, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 2020, 0.65).
narrative_ontology:measurement(nsl__su_t2021, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 2021, 0.7).
narrative_ontology:measurement(nsl__su_t2022, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 2022, 0.7).
narrative_ontology:measurement(nsl__su_t2023, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 2023, 0.7).
narrative_ontology:measurement(nsl__su_t2024, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_legal_text__sovereignty_restoration_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'nsl_legal_text' kernel. It focuses on the NSL as a legitimate sovereign security instrument, distinct from readings that emphasize democratic enclosure or jurisdictional capture.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
