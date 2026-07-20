% ============================================================================
% CONSTRAINT STORY: balfour_mandate_instruments__mandatory_interpretive_discretion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_balfour_mandate_instruments__mandatory_interpretive_discretion, []).

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
 *   constraint_id: balfour_mandate_instruments__mandatory_interpretive_discretion
 *   human_readable: British Mandatory Interpretive Discretion
 *   domain: international_law/colonial_administration/state_formation
 *
 * SUMMARY:
 *   This constraint story instantiates the
 *   'mandatory_interpretive_discretion' reading of the contested Balfour
 *   Mandate Instruments kernel. Under this reading, the British mandatory
 *   authority's unreviewable power to adjudicate between competing
 *   interpretations of the Mandateâparticularly the meaning of 'national
 *   home' and the scope of Arab safeguardsâis itself the operational
 *   constraint system. The text remains formally fixed while British policy
 *   oscillates (White Papers 1922, 1930, 1939), generating strategic
 *   uncertainty for both Arab and Zionist communities. The sibling readings
 *   are 'jewish_national_home_primacy' (the text directs demographic
 *   transformation toward Jewish sovereignty) and
 *   'dual_obligation_indigenous_rights' (the text subordinates the national
 *   home to Arab self-determination). This reading treats neither national
 *   claim as textually privileged; instead, privilege flows to the
 *   interpreter.
 *
 * KEY AGENTS:
 *   - British Mandatory Authority (institutional/agenda-setter/beneficiary): holds and exercises unreviewable interpretive discretion over Palestine policy
 *   - Palestinian Arab Community (organized/payer): bears costs of land and policy uncertainty without appeal
 *   - Zionist Jewish Community (organized/payer): bears costs of immigration and settlement policy reversals without appeal
 *   - League Permanent Mandates Commission (institutional/observer): nominally supervises but cannot compel interpretation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.62).
domain_priors:suppression_score(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.7).
domain_priors:theater_ratio(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, extractiveness, 0.62).
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__mandatory_interpretive_discretion, snare).
narrative_ontology:human_readable(balfour_mandate_instruments__mandatory_interpretive_discretion, "British Mandatory Interpretive Discretion").
narrative_ontology:topic_domain(balfour_mandate_instruments__mandatory_interpretive_discretion, "international_law/colonial_administration/state_formation").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__mandatory_interpretive_discretion, '8aaec88f-b442-4acf-9148-9b0fea3adb86').
narrative_ontology:cs_kernel_codification('8aaec88f-b442-4acf-9148-9b0fea3adb86', formalized).
narrative_ontology:cs_authority_grounding('8aaec88f-b442-4acf-9148-9b0fea3adb86', lineage).
narrative_ontology:cs_interpretation_layer_present('8aaec88f-b442-4acf-9148-9b0fea3adb86').
narrative_ontology:cs_reading_relation('8aaec88f-b442-4acf-9148-9b0fea3adb86', balfour_mandate_instruments__jewish_national_home_primacy, coexists_with).
narrative_ontology:cs_reading_relation('8aaec88f-b442-4acf-9148-9b0fea3adb86', balfour_mandate_instruments__dual_obligation_indigenous_rights, coexists_with).
narrative_ontology:cs_axiom('8aaec88f-b442-4acf-9148-9b0fea3adb86', foundational, mandatory_interpretive_supremacy).
narrative_ontology:cs_axiom_status(mandatory_interpretive_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('8aaec88f-b442-4acf-9148-9b0fea3adb86', mandatory_interpretive_supremacy, conventional).
narrative_ontology:cs_reference_frame('8aaec88f-b442-4acf-9148-9b0fea3adb86', mandatory_trusteeship_framework).
narrative_ontology:cs_drift_state('8aaec88f-b442-4acf-9148-9b0fea3adb86', late_mandate_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8aaec88f-b442-4acf-9148-9b0fea3adb86', '').
narrative_ontology:cs_kernel_id(balfour_mandate_instruments__mandatory_interpretive_discretion, balfour_mandate_instruments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__mandatory_interpretive_discretion, british_mandatory_authority).
narrative_ontology:constraint_victim(balfour_mandate_instruments__mandatory_interpretive_discretion, palestinian_arab_community).
narrative_ontology:constraint_victim(balfour_mandate_instruments__mandatory_interpretive_discretion, zionist_jewish_community).
narrative_ontology:constraint_vindicates(balfour_mandate_instruments__mandatory_interpretive_discretion, imperial_trusteeship_doctrine).
narrative_ontology:constraint_vindicates(balfour_mandate_instruments__mandatory_interpretive_discretion, mandatory_sovereignty_as_administration).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers Palestine under the League of Nations Mandate, issuing White Papers and land regulations that shift policy between Arab and Zionist claims without external review. Retains sole authority to interpret 'national home' and 'safeguards', using this flexibility to manage imperial interests and prevent either community from achieving autonomous political consolidation.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, british_mandatory_authority, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(balfour_mandate_instruments__mandatory_interpretive_discretion, british_mandatory_authority, beneficiary).

% Lives under British land and immigration policies that oscillate between limited protection and open Zionist settlement. Cannot appeal to fixed textual meaning of the Mandate against British discretion; political organization is met with regulation or repression depending on the policy phase.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, palestinian_arab_community, payer,
    organized, generational, trapped, national).

% Relies on British interpretation of the 'national home' clause to facilitate immigration and land purchase, but faces sudden reversals (1930, 1939 White Papers) that restrict these same activities. Has no judicial or arbitral instance to enforce a pro-Zionist reading of the Mandate against British policy shifts.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, zionist_jewish_community, payer,
    organized, generational, constrained, national).

% Receives British annual reports and hears petitions from both communities, but lacks enforcement power to compel adherence to any particular interpretation of the Mandate. Its recommendations are advisory; it cannot override Colonial Office discretion.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, league_permanent_mandates_commission, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(balfour_mandate_instruments__mandatory_interpretive_discretion, british_mandatory_authority).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The British claimed to coordinate between competing Arab and Jewish national claims under the Mandate, maintaining public order and administering development while the League of Nations supervised.
% TRANSFER_FUNCTION: Moves strategic certainty, territorial control, and policy predictability from both Arab and Zionist communities to the British mandatory authority, which gains imperial flexibility and leverage.
% ABSENT_VOICES: International judicial instances capable of binding review of mandatory discretion were structurally absent; the Permanent Court of International Justice had no compulsory jurisdiction over Palestine Mandate interpretation, and both communities lacked standing to compel arbitration.
% DISAPPEARANCE_RATIONALE: If British interpretive discretion vanishedâreplaced by fixed textual meaning or external arbitrationâthe political field would reorganize: either community could appeal to a stable legal baseline, shifting the locus of authority from colonial improvisation to institutionalized law or direct negotiation.
% FOUNDING_PROBLEM: The collapse of Ottoman imperial authority after World War I and the need to administer Palestine while reconciling the Balfour Declaration's 'national home' promise with existing Arab population and land rights.
% FOUNDING_PROBLEM_CORROBORATION: British official historiography and League of Nations archives attest the administrative vacuum; independent international legal historians and both Arab and Zionist sources confirm the problem existed, though they dispute whether unreviewable British discretion was a necessary or legitimate solution.
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__mandatory_interpretive_discretion, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__mandatory_interpretive_discretion, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__mandatory_interpretive_discretion, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(balfour_mandate_instruments__mandatory_interpretive_discretion, 'none', 1).
narrative_ontology:epsilon_provenance(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(balfour_mandate_instruments__mandatory_interpretive_discretion_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(balfour_mandate_instruments__mandatory_interpretive_discretion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(balfour_mandate_instruments__mandatory_interpretive_discretion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderate because the constraint extracts strategic certainty and political autonomy from both communities, though the British also provide some administrative functions. Suppression (0.70) is high because the constraint persists through colonial legal hierarchy, emergency regulations, and the exclusion of both communities from binding appellate instances. Theater_ratio (0.45) reflects the growing performative quality of League oversight and British claims of trusteeship as actual policy became more arbitrary. Accessibility_collapse (0.60) captures the absence of alternative legal fora; resistance (0.55) reflects the 1936 Arab Revolt and Zionist paramilitary responses. Temporal measurements show cyclical extraction: policy oscillations (1922 pro-Zionist, 1930 balanced, 1939 pro-Arab) created lock-in where each shift raised the baseline of uncertainty. The measurement grid shares time points across all tracked metrics to prevent misalignment.
 *
 * PERSPECTIVAL GAP:
 *   From the British seat, the arrangement appears as necessary imperial flexibility to manage an intractable dual obligation; from both community seats, the same structure appears as arbitrary extraction that prevents either from securing a predictable political future. The engine computes this divergence from structural data: the British have arbitrage-grade exit (they can withdraw or reframe policy) while both communities are trapped or constrained within the mandate territory.
 *
 * DIRECTIONALITY LOGIC:
 *   British mandatory authority is the declared beneficiary (gains policy flexibility, divide-and-rule leverage) and agenda-setter, yielding a low directionality toward subsidy. Both Arab and Zionist communities are declared victims (bear strategic uncertainty, cannot appeal), yielding high directionality toward extraction. The Permanent Mandates Commission sits at near-symmetric directionality as an observer with neither cost nor benefit. No override is needed because structural derivation captures the relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the R5 genealogy interview, this constraint might be misclassified as a Tangled Rope: there is a genuine coordination problem (administering a post-Ottoman territory with competing nationalisms) and the British did provide courts, infrastructure, and order. However, the founding problem status is contested, the coordination story is cover for extraction of discretion, and the beneficiaries are specifically the administrators rather than the coordinated communities. The snare classification captures that the coordination function has been subordinated to extraction. The theater ratio and temporal drift confirm that enforcement increasingly served to maintain British flexibility rather than to solve the underlying coordination problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'Does the Balfour Mandate Instruments kernel structurally support fixed textual meaning, or is interpretive discretion inherent to its text?',
    'Comparative legal analysis of the Mandate text against other League mandates to determine whether ''national home'' and ''safeguards'' language inherently delegate interpretive authority.',
    'If the text inherently delegates discretion, this reading is a Mountain of legal positivism; if the text is fixable, this reading is a constructed Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Whether interpretive discretion is textually inherent or politically constructed.').

omega_variable(
    oscillation_as_extraction,
    'Are the oscillations in British land and immigration policy (1922/1930/1939) a deliberate divide-and-rule mechanism, or reactions to exogenous pressures?',
    'Archival analysis of Colonial Office deliberations to determine whether oscillation was strategically chosen to prevent either community from consolidating autonomous power.',
    'If deliberate, base_extractiveness is higher than structural necessity suggests; if reactive, extraction is incidental to governance failure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oscillation_as_extraction, empirical, 'Whether policy oscillation is strategic extraction or reactive governance.').

omega_variable(
    structural_suppression_confirmation,
    'Is the suppression of appeal pathways purely structural (colonial legal hierarchy) or does it include internalized acceptance of British interpretive supremacy by either community?',
    'Post-mandate legal history: did either community continue to appeal to British legal frameworks or accept British interpretive authority after independence?',
    'If internalized, effective suppression exceeds the structural measure; if purely structural, suppression collapses with British withdrawal.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(structural_suppression_confirmation, empirical, 'Whether suppression is purely structural or includes internalized acceptance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__mandatory_interpretive_discretion, 0, 28).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balf_tr_t0, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 0, 0.25).
narrative_ontology:measurement(balf_tr_t2, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 2, 0.3).
narrative_ontology:measurement(balf_tr_t5, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 5, 0.32).
narrative_ontology:measurement(balf_tr_t10, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 10, 0.38).
narrative_ontology:measurement(balf_tr_t15, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 15, 0.4).
narrative_ontology:measurement(balf_tr_t19, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 19, 0.5).
narrative_ontology:measurement(balf_tr_t24, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 24, 0.48).
narrative_ontology:measurement(balf_tr_t28, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 28, 0.45).

% Extraction over time
narrative_ontology:measurement(balf_be_t0, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(balf_be_t2, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 2, 0.52).
narrative_ontology:measurement(balf_be_t5, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(balf_be_t10, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(balf_be_t15, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(balf_be_t19, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 19, 0.7).
narrative_ontology:measurement(balf_be_t24, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 24, 0.62).
narrative_ontology:measurement(balf_be_t28, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 28, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(balf_su_t0, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(balf_su_t2, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 2, 0.6).
narrative_ontology:measurement(balf_su_t5, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(balf_su_t10, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(balf_su_t15, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(balf_su_t19, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 19, 0.78).
narrative_ontology:measurement(balf_su_t24, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 24, 0.75).
narrative_ontology:measurement(balf_su_t28, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 28, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(balfour_mandate_instruments__mandatory_interpretive_discretion, balfour_mandate_instruments__jewish_national_home_primacy).
narrative_ontology:affects_constraint(balfour_mandate_instruments__mandatory_interpretive_discretion, balfour_mandate_instruments__dual_obligation_indigenous_rights).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested Balfour Mandate Instruments kernel. It focuses on British interpretive discretion as the operational system, while sibling readings focus on the substantive national claims the text is held to direct.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
