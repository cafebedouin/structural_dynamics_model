% ============================================================================
% CONSTRAINT STORY: balfour_mandate_instruments__dual_obligation_indigenous_rights
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_balfour_mandate_instruments__dual_obligation_indigenous_rights, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: balfour_mandate_instruments__dual_obligation_indigenous_rights
 *   human_readable: Balfour Mandate: Dual Obligation to Indigenous Rights Reading
 *   domain: international_law/colonial_administration/state_formation
 *
 * SUMMARY:
 *   This constraint represents an interpretation of the British Mandate for
 *   Palestine (1920-1948) that emphasizes the equal or superior obligation to
 *   protect the existing civil and political rights, as well as land tenure,
 *   of the Arab population. Under this reading, the 'national home for the
 *   Jewish people' clause was subordinated to the principles of
 *   self-determination for the indigenous majority and minority protection.
 *   This interpretation implies restrictions on land transfers to prevent
 *   displacement and limits on Jewish immigration to avoid demographic
 *   transformation, thereby constraining the Zionist project and British
 *   administrative flexibility.
 *
 * KEY AGENTS:
 *   - palestinian_arab_elites: Primary beneficiary (powerful/constrained) — benefit from land tenure protection and political rights.
 *   - palestinian_arab_communities: Primary beneficiary (organized/constrained) — benefit from land tenure protection and demographic stability.
 *   - zionist_organizations: Primary victim (institutional/constrained) — constrained in land acquisition and immigration goals.
 *   - british_administrators: Primary victim (institutional/constrained) — constrained in satisfying Zionist demands and maintaining order.
 *   - league_of_nations: Observer (institutional/analytical) — nominal oversight body, but with limited enforcement power.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.7).
domain_priors:suppression_score(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.6).
domain_priors:theater_ratio(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, extractiveness, 0.7).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__dual_obligation_indigenous_rights, tangled_rope).
narrative_ontology:human_readable(balfour_mandate_instruments__dual_obligation_indigenous_rights, "Balfour Mandate: Dual Obligation to Indigenous Rights Reading").
narrative_ontology:topic_domain(balfour_mandate_instruments__dual_obligation_indigenous_rights, "international_law/colonial_administration/state_formation").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__dual_obligation_indigenous_rights).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__dual_obligation_indigenous_rights, 'ef7345c8-19af-4d27-a55e-9e629cfd458a').
narrative_ontology:cs_kernel_codification('ef7345c8-19af-4d27-a55e-9e629cfd458a', fixed_text).
narrative_ontology:cs_authority_grounding('ef7345c8-19af-4d27-a55e-9e629cfd458a', lineage).
narrative_ontology:cs_interpretation_layer_present('ef7345c8-19af-4d27-a55e-9e629cfd458a').
narrative_ontology:cs_reading_relation('ef7345c8-19af-4d27-a55e-9e629cfd458a', balfour_mandate_instruments__jewish_national_home_primacy, forecloses).
narrative_ontology:cs_reading_relation('ef7345c8-19af-4d27-a55e-9e629cfd458a', balfour_mandate_instruments__mandatory_interpretive_discretion, coexists_with).
narrative_ontology:cs_axiom('ef7345c8-19af-4d27-a55e-9e629cfd458a', foundational, indigenous_rights_primacy).
narrative_ontology:cs_axiom_status(indigenous_rights_primacy, holdable).
narrative_ontology:cs_axiom_grounding('ef7345c8-19af-4d27-a55e-9e629cfd458a', indigenous_rights_primacy, deontological).
narrative_ontology:cs_axiom('ef7345c8-19af-4d27-a55e-9e629cfd458a', foundational, self_determination_norm).
narrative_ontology:cs_axiom_status(self_determination_norm, holdable).
narrative_ontology:cs_axiom_grounding('ef7345c8-19af-4d27-a55e-9e629cfd458a', self_determination_norm, deontological).
narrative_ontology:cs_reference_frame('ef7345c8-19af-4d27-a55e-9e629cfd458a', dual_obligation_balanced_mandate).
narrative_ontology:cs_drift_state('ef7345c8-19af-4d27-a55e-9e629cfd458a', end_of_mandate_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('ef7345c8-19af-4d27-a55e-9e629cfd458a', '').
narrative_ontology:cs_kernel_id(balfour_mandate_instruments__dual_obligation_indigenous_rights, balfour_mandate_instruments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_elites).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_communities).
narrative_ontology:constraint_victim(balfour_mandate_instruments__dual_obligation_indigenous_rights, zionist_organizations).
narrative_ontology:constraint_victim(balfour_mandate_instruments__dual_obligation_indigenous_rights, british_administrators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Leaders and landowners who benefit from the Mandate's provisions protecting existing land tenure and civil/political rights, which would otherwise be eroded by Zionist expansion. They actively lobby for the enforcement of these protections but are constrained by British power.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_elites, beneficiary,
    powerful, generational, constrained, regional).

% The indigenous population whose land, livelihoods, and political future are theoretically protected by this reading of the Mandate. They are beneficiaries of land transfer restrictions and immigration quotas, but their collective action is often suppressed.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_communities, beneficiary,
    organized, generational, constrained, local).

% Organizations (e.g., Jewish Agency) whose primary goal is to establish a Jewish national home through land acquisition and mass immigration. This reading of the Mandate directly constrains their ability to achieve these goals, forcing them to pay in terms of delayed or denied expansion.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, zionist_organizations, payer,
    institutional, generational, constrained, global).

% The mandatory power tasked with implementing the Mandate. This reading imposes a 'dual obligation' that creates significant administrative and political challenges, forcing them to mediate between conflicting demands and often incurring the wrath of both sides. They pay in terms of administrative burden and political instability.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, british_administrators, payer,
    institutional, biographical, constrained, national).

% The international body that granted the Mandate and was theoretically responsible for its oversight. They observed the implementation and received reports but had limited direct enforcement power, acting primarily as an analytical and legitimizing body.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, league_of_nations, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_elites).
narrative_ontology:fixing_cost_class(balfour_mandate_instruments__dual_obligation_indigenous_rights, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate the establishment of a Jewish national home with the protection of the civil and political rights of the existing non-Jewish population, aiming for a balanced development of the territory under international supervision.
% TRANSFER_FUNCTION: Transfers security of land tenure and political representation to the Palestinian Arab population, and transfers constraints on land acquisition and immigration to Zionist organizations and British administrators.
% ABSENT_VOICES: The full voice of an independent Palestinian Arab state, which would have asserted full sovereignty and self-determination, was absent due to the colonial context. Their demands were mediated through 'non-Jewish communities' clauses.
% DISAPPEARANCE_RATIONALE: If this reading of the Mandate (with its emphasis on indigenous rights) had been consistently and effectively enforced, the demographic and territorial landscape of Palestine would have developed very differently, likely leading to an Arab-majority state with a Jewish minority, rather than the eventual outcome. Its disappearance would have meant the removal of the primary legal basis for protecting Arab rights within the Mandate framework.
% FOUNDING_PROBLEM: The problem of reconciling the promise of a 'national home for the Jewish people' with the existing rights and aspirations of the indigenous Arab population in Palestine, under the framework of post-WWI colonial administration and international law.
% FOUNDING_PROBLEM_CORROBORATION: Historians and international legal scholars, independent of both Israeli and Palestinian national narratives, corroborate that the founding problem of reconciling these two obligations was never effectively resolved by the British administration, leading to its eventual collapse. The problem is 'dead' in the sense that the Mandate itself ended without a resolution, and the subsequent conflict superseded its terms.
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__dual_obligation_indigenous_rights, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__dual_obligation_indigenous_rights, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__dual_obligation_indigenous_rights, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(balfour_mandate_instruments__dual_obligation_indigenous_rights, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(balfour_mandate_instruments__dual_obligation_indigenous_rights_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(balfour_mandate_instruments__dual_obligation_indigenous_rights, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(balfour_mandate_instruments__dual_obligation_indigenous_rights_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely attempts to coordinate the rights of the indigenous population with the establishment of a Jewish national home, but this coordination function is intertwined with significant extraction from Zionist organizations and British administrators who are prevented from pursuing their maximalist goals. Extractiveness is high (0.7) because the limitations on land and immigration directly impede the core objectives of the Zionist project. Suppression (0.6) is present as British enforcement (or lack thereof) actively suppresses Zionist aspirations for rapid demographic and territorial expansion. Resistance (0.8) is high, reflecting the constant political and sometimes violent opposition from Zionist organizations and their international allies against these limitations. Accessibility collapse (0.4) is moderate, as alternatives (e.g., direct land purchases, unrestricted immigration) are partially, but not completely, foreclosed by the Mandate's terms under this reading.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Palestinian Arab elites and communities, this reading of the Mandate represents a legitimate (though often inadequately enforced) protection of their rights and a framework for eventual self-determination. From the perspective of Zionist organizations, it is an illegitimate constraint that undermines the core promise of the Balfour Declaration and the Mandate itself, viewing it as an extraction of their potential national home. British administrators experience it as a constant source of tension and a constraint on their ability to satisfy conflicting demands.
 *
 * DIRECTIONALITY LOGIC:
 *   Palestinian Arab elites and communities are beneficiaries (d near 0.0) as the constraint protects their existing rights and land tenure. Zionist organizations are victims (d near 1.0) as their goals of land acquisition and demographic growth are directly impeded. British administrators are also victims (d near 1.0) because this reading forces them to balance conflicting obligations, often leading to political and administrative difficulties, and preventing them from fully satisfying the powerful Zionist lobby. The League of Nations is an analytical observer (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading of the Mandate, if genuinely enforced, would have prevented the Mandate from becoming a pure Snare for the Arab population. By imposing dual obligations, it attempts to maintain a coordination function (managing competing claims) rather than solely facilitating extraction. However, the high resistance and contested status of its founding problem suggest that the coordination function was severely challenged, and the constraint's persistence was often more performative than effective, leading to a drift towards a more extractive (Snare-like) outcome in practice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine interpretation of the Mandate''s intent, or a counter-reading imposed by external pressure?',
    'Analysis of primary source documents from the Mandate''s drafting, including internal Colonial Office memoranda and League of Nations debates, to ascertain the original intent regarding indigenous rights versus national home provisions.',
    'If a genuine interpretation, it strengthens the claim for Palestinian self-determination based on the Mandate''s own terms. If a counter-reading, it highlights the contestation over the Mandate''s legitimacy and intent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is one reading (dual_obligation_indigenous_rights) of the ''balfour_mandate_instruments'' kernel. A sibling reading, ''jewish_national_home_primacy'', would prioritize Jewish settlement and state-building, directly contradicting the land tenure and political rights protections central to this reading. Another sibling, ''mandatory_interpretive_discretion'', would assert the British administration''s sole authority to interpret the Mandate, making the ''dual obligation'' merely one possible interpretation among others, rather than a binding structural constraint.').

omega_variable(
    enforcement_sincerity,
    'To what extent did British administrators genuinely attempt to enforce the ''dual obligation'' provisions, versus using them as a rhetorical cover for other objectives?',
    'Examination of British administrative records, land transfer policies, immigration quota enforcement, and responses to Arab political demands, cross-referenced with internal communications regarding strategic objectives.',
    'If enforcement was sincere, it suggests a genuine (though perhaps ultimately failed) attempt at balanced administration. If insincere, it reveals the ''dual obligation'' as a performative constraint, masking a de facto ''jewish_national_home_primacy'' reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_sincerity, empirical, 'Assessing the sincerity of British enforcement of indigenous rights protections.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balf_tr_t0, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 0, 0.15).
narrative_ontology:measurement(balf_tr_t5, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 5, 0.18).
narrative_ontology:measurement(balf_tr_t10, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(balf_be_t0, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(balf_be_t5, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(balf_be_t10, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 10, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(balf_su_t0, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(balf_su_t5, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(balf_su_t10, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__dual_obligation_indigenous_rights, enforcement_mechanism).
narrative_ontology:affects_constraint(balfour_mandate_instruments__dual_obligation_indigenous_rights, balfour_mandate_instruments__jewish_national_home_primacy).
narrative_ontology:affects_constraint(balfour_mandate_instruments__dual_obligation_indigenous_rights, balfour_mandate_instruments__mandatory_interpretive_discretion).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'balfour_mandate_instruments' kernel. This reading emphasizes indigenous rights and land tenure, while 'jewish_national_home_primacy' prioritizes Jewish state-building, and 'mandatory_interpretive_discretion' focuses on British administrative authority to interpret the Mandate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
