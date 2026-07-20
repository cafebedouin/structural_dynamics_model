% ============================================================================
% CONSTRAINT STORY: ai_alignment_priority__integrated_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_priority__integrated_reading, []).

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
 *   constraint_id: ai_alignment_priority__integrated_reading
 *   human_readable: Integrated AI Alignment Priority Reading
 *   domain: ai_governance/technology_ethics/risk_assessment
 *
 * SUMMARY:
 *   This constraint story instantiates the integrated_reading of the
 *   contested kernel ai_alignment_priority. The kernel asks what 'alignment'
 *   means and which harms it should address. The integrated reading claims
 *   that catastrophic capability risks and present discriminatory or
 *   extractive harms are complementary priorities that must be addressed
 *   together within a unified governance and research agenda. This reading is
 *   distinct from the existential_risk_reading (pure catastrophic focus) and
 *   the nearterm_harms_reading (pure present-justice focus). The constraint
 *   is the institutionalized arrangementâfunding protocols, conference
 *   structures, and evaluation normsâthat enforces this complementarity.
 *
 * KEY AGENTS:
 *   - integrative_alignment_institutions (agenda setter / institutional / arbitrage): administers the dual-track frame and captures resources and legitimacy from brokering.
 *   - dual_track_funders (beneficiary / powerful / mobile): prefers unified portfolios and benefits from reduced administrative overhead.
 *   - longterm_safety_researchers (beneficiary / moderate / constrained): gain funding but pay methodological costs.
 *   - nearterm_harms_researchers (beneficiary / moderate / constrained): gain platform but lose autonomous framing.
 *   - present_marginalized_groups (payer / powerless / trapped): bear dilution of targeted remediation.
 *   - future_populations (payer / powerless / trapped): bear potential delay of capability restraint.
 *   - ai_governance_observers (observer / institutional / analytical): track whether integration produces outcomes or rhetoric.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__integrated_reading, 0.55).
domain_priors:suppression_score(ai_alignment_priority__integrated_reading, 0.45).
domain_priors:theater_ratio(ai_alignment_priority__integrated_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__integrated_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_priority__integrated_reading, "Integrated AI Alignment Priority Reading").
narrative_ontology:topic_domain(ai_alignment_priority__integrated_reading, "ai_governance/technology_ethics/risk_assessment").

domain_priors:requires_active_enforcement(ai_alignment_priority__integrated_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__integrated_reading, 'bec27dda-182e-4774-94fd-cb67d52c3913').
narrative_ontology:cs_kernel_codification('bec27dda-182e-4774-94fd-cb67d52c3913', distributed).
narrative_ontology:cs_authority_grounding('bec27dda-182e-4774-94fd-cb67d52c3913', distributed).
narrative_ontology:cs_reading_relation('bec27dda-182e-4774-94fd-cb67d52c3913', ai_alignment_priority__existential_risk_reading, influences).
narrative_ontology:cs_reading_relation('bec27dda-182e-4774-94fd-cb67d52c3913', ai_alignment_priority__nearterm_harms_reading, influences).
narrative_ontology:cs_axiom('bec27dda-182e-4774-94fd-cb67d52c3913', foundational, complementarity_of_harm_types).
narrative_ontology:cs_axiom_status(complementarity_of_harm_types, holdable).
narrative_ontology:cs_axiom_grounding('bec27dda-182e-4774-94fd-cb67d52c3913', complementarity_of_harm_types, empirically_contingent).
narrative_ontology:cs_axiom('bec27dda-182e-4774-94fd-cb67d52c3913', foundational, balanced_resource_allocation).
narrative_ontology:cs_axiom_status(balanced_resource_allocation, holdable).
narrative_ontology:cs_axiom_grounding('bec27dda-182e-4774-94fd-cb67d52c3913', balanced_resource_allocation, conventional).
narrative_ontology:cs_reference_frame('bec27dda-182e-4774-94fd-cb67d52c3913', unified_alignment_agenda).
narrative_ontology:cs_drift_state('bec27dda-182e-4774-94fd-cb67d52c3913', post_integrative_turn_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bec27dda-182e-4774-94fd-cb67d52c3913', '').
narrative_ontology:cs_kernel_id(ai_alignment_priority__integrated_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, integrative_alignment_institutions).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, dual_track_funders).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, longterm_safety_researchers).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, nearterm_harms_researchers).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, present_marginalized_groups).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, future_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer cross-priority research programs, conferences, and funding pools that require grantees to address both catastrophic risk and present harms. They set evaluative criteria that treat single-priority proposals as incomplete, and they capture institutional legitimacy and operating budgets by brokering between the two camps.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, integrative_alignment_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Philanthropic and government funders who prefer unified portfolios covering both long-term safety and near-term justice. They benefit from a single narrative that justifies large, undifferentiated grants and reduces the transaction costs of managing separate program offices.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, dual_track_funders, beneficiary,
    powerful, biographical, mobile, global).

% Receive expanded funding and mainstream legitimacy by adopting the integrative frame, but must divert methodological effort toward present-harm audits and justify capability work in terms of immediate deployment risks. Their exit to a pure long-term agenda is constrained by funding gatekeeping.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, longterm_safety_researchers, beneficiary,
    moderate, biographical, constrained, global).

% Gain access to longtermist funding platforms and policy audiences by framing discriminatory or extractive harms as alignment failures, but must subsume their specific justice claims under a broader risk-management vocabulary that strips activist momentum.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, nearterm_harms_researchers, beneficiary,
    moderate, biographical, constrained, global).

% Communities subject to current algorithmic surveillance, labor extraction, and discriminatory automated decisions. Under the integrated frame, targeted regulatory urgency is diluted into research agendas that treat their harms as one input to a larger alignment calculus, slowing concrete remediation.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, present_marginalized_groups, payer,
    powerless, immediate, trapped, global).

% Hypothetical future beings who would bear the consequences of catastrophic misalignment. The integrative frame may delay or weaken capability-restraint measures by using present-harm integration as a legitimacy shield, leaving them exposed to unabated risk trajectories from which they cannot exit.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, future_populations, payer,
    powerless, civilizational, trapped, universal).

% Independent auditors, science and technology studies scholars, and legislative staff who track whether integrated programs produce measurable, non-substitutable improvements for both present marginalized groups and long-term safety, or whether integration functions primarily as rhetorical portfolio management.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, ai_governance_observers, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_priority__integrated_reading, integrative_alignment_institutions).
narrative_ontology:fixing_cost_class(ai_alignment_priority__integrated_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents destructive schism between AI safety and AI ethics communities by establishing shared vocabulary, joint funding streams, and dual-track research evaluation so that capability risk and deployment harm are addressed within a unified governance architecture rather than competing for zero-sum institutional territory.
% TRANSFER_FUNCTION: Moves institutional legitimacy, research funding, and policy attention from siloed single-priority programs to integrative institutions that allocate across both; moves methodological obligations from pure research traditions toward hybrid audit-and-red-team protocols that serve the integrative narrative.
% ABSENT_VOICES: Pure existential-risk researchers who regard near-term framing as an existential distraction, and grassroots algorithmic justice organizers who regard longtermism as epistemic colonialism, are partially audible in public discourse but structurally underweighted on funding panels, keynote lineups, and agenda-setting committees; their exclusion is the price of presenting a unified field.
% DISAPPEARANCE_RATIONALE: If the integrative frame vanished overnight, the field would likely fracture into separate conferences, funding pools, and policy coalitions; present-harms advocates would push harder for immediate regulatory enforcement, existential-risk advocates for steeper capability restraint, and the current broker institutions would lose their mediating role and consolidated budgets.
% FOUNDING_PROBLEM: Destructive polarization between AI safety and AI ethics communities in the late 2010s, producing duplicated effort, hostile public discourse, and policy paralysis as each camp dismissed the other's priorities and competed for scarce institutional attention.
% FOUNDING_PROBLEM_CORROBORATION: Integrative institutions attest the polarization problem remains live and requires active management. Independent sociologists of science and historians of the field corroborate that polarization was real, but note it was partly amplified by funding scarcity and organizational entrepreneurship rather than purely intellectual disagreement; pure-priority advocates from both sides attest the problem was manufactured by funders seeking manageable portfolios, providing external challenge to the beneficiary narrative.
narrative_ontology:disappearance_verdict(ai_alignment_priority__integrated_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_priority__integrated_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_priority__integrated_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_alignment_priority__integrated_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_priority__integrated_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_priority__integrated_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_priority__integrated_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_priority__integrated_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.55 (moderate) because the constraint genuinely coordinates two camps that would otherwise fragment, but it also extracts by diluting the urgency and specificity of each agenda into a manageable portfolio for integrative institutions. Suppression is 0.45: the frame is maintained by peer pressure, funding gatekeeping, and conference curation that marginalizes pure-priority positions, not by physical coercion. Theater ratio at 0.4 reflects growing performative maintenanceâpanels and reports that announce balance without rebalancing power or resources. Accessibility collapse is 0.5 because pure alternatives (existential-only or justice-only agendas) remain thinkable but are institutionally marginalized. Resistance is 0.5 because both pure camps actively contest the frame. The measurement series run on one shared time grid (0â20) to prevent misaligned substitution.
 *
 * PERSPECTIVAL GAP:
 *   The integrative institutions and dual-track funders experience the constraint as coordination: it stabilizes coalitions, expands addressable audiences, and legitimates large-scale budgets. Present marginalized groups and future populations experience it as extraction: their specific harms are instrumentalized to justify a broader agenda while concrete remedial action is diffused across hybrid programs. The longterm and nearterm researcher seats sit in between, receiving subsidies (funding, legitimacy) while bearing methodological costs; the engine will compute a lower effective extraction for them than for the trapped victim seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (integrative institutions, funders, researchers) receive structural subsidy: their participation in the constraint is rewarded with resources, platforms, or legitimacy, pushing their directionality toward the beneficiary pole. Victims (present marginalized groups, future populations) bear the costs of dilution and delay, with trapped or identity-locked exit profiles that amplify their effective extraction. The observer seat is analytical and does not participate in the transfer. No directionality overrides are needed because the structural derivation from beneficiary/victim declarations plus exit options accurately captures these relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by maintaining a genuine coordination function: integrated programs do produce some hybrid methodologies (red-teaming plus audits) that neither pure camp would generate alone. Mandatrophy would occur if the founding problemâdestructive polarizationâwere fully resolved but the integrative frame persisted purely to protect broker budgets. The founding_problem_status is authored as contested because external observers disagree on whether polarization is still live; if it were clearly dead and the frame persisted with rising theater, the engine would flag piton or snare drift. For now, the dual methodology and real resource splitting keep it in tangled-rope territory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    complementarity_constructed_or_discovered,
    'Is the complementarity between catastrophic and present AI harms a discoverable structural feature of the risk landscape, or an institutional compromise constructed to manage coalition politics?',
    'Empirical tracking of whether integrated programs produce non-substitutable improvements on both fronts compared to siloed programs; sociological analysis of whether the complementarity claim correlates with funding consolidation rather than outcome data.',
    'If constructed, the constraint''s coordination function is cover for resource capture by integrative institutions (snare-ward drift); if discovered, the moderate extraction is genuine coordination overhead (rope-ward).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complementarity_constructed_or_discovered, conceptual, 'Whether the integrated priority framework reflects reality or institutional compromise.').

omega_variable(
    future_populations_as_victims,
    'Can future populations be structurally represented as victims in a constraint whose enforcement is entirely mediated by present institutions and actors?',
    'Intertemporal representation theory and analysis of whether present institutional constraints can bear costs for not-yet-existing agents without proxy capture by present beneficiaries.',
    'If future populations cannot be formal victims here, the victim set collapses to present groups, changing the directionality profile and potentially retyping the constraint toward scaffold or rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_populations_as_victims, conceptual, 'Epistemic status of future populations as constraint victims.').

omega_variable(
    kernel_integrated_vs_siblings,
    'This constraint is the integrated_reading of kernel ai_alignment_priority. Sibling readings (existential_risk_reading, nearterm_harms_reading) treat the priorities as competing or zero-sum. Does adopting the integrated reading foreclose either sibling within a single actor''s framework?',
    'Survey of actor commitments to test whether any institution simultaneously holds the integrated reading and one pure-priority reading without contradiction; logical analysis of whether complementarity entails non-competing allocation.',
    'If the integrated reading forecloses neither, the three readings form a live constraint family; if it forecloses one or both, the engine should compute foreclosure from axiom contradiction, producing a dominance hierarchy rather than a family of coexisting alternatives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_integrated_vs_siblings, conceptual, 'Structural relationship of integrated reading to sibling kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__integrated_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_align_int_tr_t0, ai_alignment_priority__integrated_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ai_align_int_tr_t4, ai_alignment_priority__integrated_reading, theater_ratio, 4, 0.25).
narrative_ontology:measurement(ai_align_int_tr_t8, ai_alignment_priority__integrated_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(ai_align_int_tr_t12, ai_alignment_priority__integrated_reading, theater_ratio, 12, 0.37).
narrative_ontology:measurement(ai_align_int_tr_t16, ai_alignment_priority__integrated_reading, theater_ratio, 16, 0.4).
narrative_ontology:measurement(ai_align_int_tr_t20, ai_alignment_priority__integrated_reading, theater_ratio, 20, 0.42).

% Extraction over time
narrative_ontology:measurement(ai_align_int_be_t0, ai_alignment_priority__integrated_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(ai_align_int_be_t4, ai_alignment_priority__integrated_reading, base_extractiveness, 4, 0.45).
narrative_ontology:measurement(ai_align_int_be_t8, ai_alignment_priority__integrated_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(ai_align_int_be_t12, ai_alignment_priority__integrated_reading, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(ai_align_int_be_t16, ai_alignment_priority__integrated_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(ai_align_int_be_t20, ai_alignment_priority__integrated_reading, base_extractiveness, 20, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(ai_align_int_su_t0, ai_alignment_priority__integrated_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ai_align_int_su_t4, ai_alignment_priority__integrated_reading, suppression_requirement, 4, 0.35).
narrative_ontology:measurement(ai_align_int_su_t8, ai_alignment_priority__integrated_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(ai_align_int_su_t12, ai_alignment_priority__integrated_reading, suppression_requirement, 12, 0.43).
narrative_ontology:measurement(ai_align_int_su_t16, ai_alignment_priority__integrated_reading, suppression_requirement, 16, 0.45).
narrative_ontology:measurement(ai_align_int_su_t20, ai_alignment_priority__integrated_reading, suppression_requirement, 20, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_priority__integrated_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_alignment_priority__integrated_reading, ai_alignment_priority__existential_risk_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__integrated_reading, ai_alignment_priority__nearterm_harms_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'AI alignment priority' conflates three structurally distinct claims. This story decomposes the integrated reading; sibling stories instantiate the existential-risk and near-term-harms readings. Each carries its own epsilon, beneficiary/victim structure, and type. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
