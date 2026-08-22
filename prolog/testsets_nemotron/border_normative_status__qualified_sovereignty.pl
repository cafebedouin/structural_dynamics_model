% ============================================================================
% CONSTRAINT STORY: border_normative_status__qualified_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_normative_status__qualified_sovereignty, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: border_normative_status__qualified_sovereignty
 *   human_readable: Qualified State Sovereignty Over Borders: Proportionality and Human Rights Conditionality
 *   domain: political/legal/migration
 *
 * SUMMARY:
 *   This constraint instantiates the 'qualified sovereignty' reading of the
 *   border_normative_status kernel: states retain authority to control
 *   borders, but that authority is conditional on proportionality to
 *   legitimate state interests and consistency with human rights obligations.
 *   Unlike the sovereignty_primary reading (which treats exclusion as a
 *   foundational prerogative) or the freedom_primary reading (which treats
 *   movement as a near-absolute right), this reading creates a structured
 *   adjudication burden: every exercise of border control must be justified,
 *   necessary, and proportionate. The constraint operates as a tangled rope
 *   because it performs genuine coordination (establishing a shared normative
 *   framework for border governance across the international legal order,
 *   preventing a race to the bottom on rights) while simultaneously
 *   extracting from excluded migrants, asylum seekers, and displaced citizens
 *   through the suppression of entry and the procedural burdens of the
 *   proportionality test itself. The adjudication machinery — supranational
 *   courts, domestic judicial review, UN treaty bodies — is the active
 *   enforcement layer that makes the conditionality real rather than
 *   aspirational.
 *
 * KEY AGENTS:
 *   - receiving_state_institutions: Agenda setter (institutional/biographical/constrained/global) — sets and administers border policy under the proportionality mandate; bears compliance costs but captures sovereignty rents
 *   - citizen_nationals: Beneficiary (organized/biographical/constrained/national) — receive the collective self-determination and security benefits the constraint legitimizes; also bear costs when state overreach rebounds
 *   - international_legal_order: Beneficiary (institutional/civilizational/analytical/universal) — gains a stabilized normative framework that prevents norm fragmentation; collects legitimacy rents
 *   - excluded_migrants: Victim (powerless/biographical/trapped/global) — bear the extraction of denial, detention, removal; exit options structurally collapsed by the very border regime
 *   - displaced_citizens: Victim (moderate/biographical/identity_locked/global) — citizens denied re-entry, rendered stateless, or denaturalized; exit is identity-locked because citizenship is the exit credential
 *   - asylum_seekers_denied_entry: Victim (powerless/immediate/trapped/global) — bear the sharpest extraction: refoulement risk, detention, rights suspension; the proportionality test is often the mechanism of denial
 *   - supranational_courts_treaty_bodies: Agenda setter (institutional/generational/arbitrage/universal) — adjudicate proportionality; their rulings are the enforcement machinery
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__qualified_sovereignty, 0.48).
domain_priors:suppression_score(border_normative_status__qualified_sovereignty, 0.62).
domain_priors:theater_ratio(border_normative_status__qualified_sovereignty, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, extractiveness, 0.48).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__qualified_sovereignty, tangled_rope).
narrative_ontology:human_readable(border_normative_status__qualified_sovereignty, "Qualified State Sovereignty Over Borders: Proportionality and Human Rights Conditionality").
narrative_ontology:topic_domain(border_normative_status__qualified_sovereignty, "political/legal/migration").

domain_priors:requires_active_enforcement(border_normative_status__qualified_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__qualified_sovereignty, '5b263aaa-94fd-4762-ad1b-5be5fc7759d6').
narrative_ontology:cs_kernel_codification('5b263aaa-94fd-4762-ad1b-5be5fc7759d6', fixed_text).
narrative_ontology:cs_authority_grounding('5b263aaa-94fd-4762-ad1b-5be5fc7759d6', lineage).
narrative_ontology:cs_interpretation_layer_present('5b263aaa-94fd-4762-ad1b-5be5fc7759d6').
narrative_ontology:cs_reading_relation('5b263aaa-94fd-4762-ad1b-5be5fc7759d6', border_normative_status__sovereignty_primary, coexists_with).
narrative_ontology:cs_reading_relation('5b263aaa-94fd-4762-ad1b-5be5fc7759d6', border_normative_status__freedom_primary, coexists_with).
narrative_ontology:cs_axiom('5b263aaa-94fd-4762-ad1b-5be5fc7759d6', foundational, sovereignty_conditioned_on_proportionality).
narrative_ontology:cs_axiom_status(sovereignty_conditioned_on_proportionality, holdable).
narrative_ontology:cs_axiom_grounding('5b263aaa-94fd-4762-ad1b-5be5fc7759d6', sovereignty_conditioned_on_proportionality, conventional).
narrative_ontology:cs_axiom('5b263aaa-94fd-4762-ad1b-5be5fc7759d6', foundational, human_rights_as_limit_on_exclusion).
narrative_ontology:cs_axiom_status(human_rights_as_limit_on_exclusion, holdable).
narrative_ontology:cs_axiom_grounding('5b263aaa-94fd-4762-ad1b-5be5fc7759d6', human_rights_as_limit_on_exclusion, deontological).
narrative_ontology:cs_axiom('5b263aaa-94fd-4762-ad1b-5be5fc7759d6', secondary, adjudication_burden_on_state).
narrative_ontology:cs_axiom_status(adjudication_burden_on_state, holdable).
narrative_ontology:cs_axiom_grounding('5b263aaa-94fd-4762-ad1b-5be5fc7759d6', adjudication_burden_on_state, instrumental).
narrative_ontology:cs_reference_frame('5b263aaa-94fd-4762-ad1b-5be5fc7759d6', postwar_human_rights_settlement).
narrative_ontology:cs_drift_state('5b263aaa-94fd-4762-ad1b-5be5fc7759d6', contemporary_migration_governance, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5b263aaa-94fd-4762-ad1b-5be5fc7759d6', '2026-08-03T14:22:17Z').
narrative_ontology:cs_kernel_id(border_normative_status__qualified_sovereignty, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__qualified_sovereignty, receiving_state_institutions).
narrative_ontology:constraint_beneficiary(border_normative_status__qualified_sovereignty, citizen_nationals).
narrative_ontology:constraint_beneficiary(border_normative_status__qualified_sovereignty, international_legal_order).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, excluded_migrants).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, displaced_citizens).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, asylum_seekers_denied_entry).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, receiving_state_institutions).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, citizen_nationals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and administers border policy under the proportionality mandate. Captures sovereignty rents (control over membership, territory, resources) but bears compliance costs: running individualized proportionality assessments, defending decisions in domestic and supranational courts, implementing treaty body views. Exit is constrained — withdrawal from human rights treaties carries reputational and diplomatic costs; the proportionality doctrine is embedded in domestic constitutional law in many jurisdictions.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, receiving_state_institutions, agenda_setter,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(border_normative_status__qualified_sovereignty, receiving_state_institutions, payer).

% Receive collective self-determination, security, and welfare-state sustainability benefits that the constraint's legitimacy framework protects. Also bear costs when state overreach rebounds (erosion of rule of law, diplomatic isolation, moral injury). Exit is constrained — citizenship is the primary exit credential; renouncing it is costly and often unavailable to those most affected by state overreach.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, citizen_nationals, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(border_normative_status__qualified_sovereignty, citizen_nationals, payer).

% Gains a stabilized normative framework that prevents norm fragmentation and a race to the bottom on migrant rights. Collects legitimacy rents: the proportionality doctrine makes the international legal order the authoritative interpreter of state sovereignty. Exit is analytical — this is an observer seat that does not bear extraction but provides the adjudication infrastructure.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, international_legal_order, beneficiary,
    institutional, civilizational, analytical, universal).

% Bear the extraction of denial, detention, and removal. The proportionality test is often the mechanism of denial: states articulate 'legitimate interests' (deterrence, resource preservation, social cohesion) that courts defer to. Exit is trapped — the border regime itself is the barrier; there is no alternative jurisdiction that does not operate under some variant of this kernel. Irregular movement is not exit from the constraint but deeper exposure to its enforcement layer.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, excluded_migrants, payer,
    powerless, biographical, trapped, global).

% Citizens denied re-entry, rendered stateless, or denaturalized under the proportionality doctrine (e.g., 'security' exclusions, deprivation of nationality for 'seriously prejudicial' conduct). Exit is identity-locked: citizenship is the credential that should guarantee entry, but the constraint makes that credential conditional. The self-concept 'I am a citizen, therefore I have a right to return' is fused with the constraint's operation — when the state denies re-entry, the identity frame breaks. This is not mere legal exclusion; it is existential displacement.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, displaced_citizens, payer,
    moderate, biographical, identity_locked, global).

% Bear the sharpest extraction: refoulement risk, detention, suspension of procedural rights. The proportionality test is weaponized — 'legitimate interests' (border integrity, deterrence of irregular migration) outweigh non-refoulement in state arguments, and courts often defer. Exit is trapped: the constraint operates at the moment of arrival; there is no 'outside' from which to challenge it. The adjudication burden falls on them as procedural exhaustion — accelerated procedures, limited appeal, evidentiary barriers.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, asylum_seekers_denied_entry, payer,
    powerless, immediate, trapped, global).

% Adjudicate proportionality: hear cases, issue binding judgments (ECtHR, IACtHR) or authoritative views (UN treaty bodies). Their rulings are the enforcement machinery that makes the conditionality real. They have arbitrage-grade exit — they choose which cases to hear, how to interpret the standard, and when to defer to states. They bear minimal extraction; they capture institutional authority and legitimacy from operating the adjudication layer.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, supranational_courts_treaty_bodies, agenda_setter,
    institutional, generational, arbitrage, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_normative_status__qualified_sovereignty, diffuse).
narrative_ontology:fixing_cost_class(border_normative_status__qualified_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared normative framework (proportionality, legitimate interests, human rights conditionality) that prevents a race to the bottom on migrant rights and stabilizes interstate expectations about border governance. Without it, states would compete on exclusion severity; with it, there is a common adjudicative language and a floor of procedural protection.
% TRANSFER_FUNCTION: Moves three things: (1) admission rights from excluded migrants to receiving states (the core extraction); (2) compliance costs from states to their administrative apparatus (proportionality assessments, litigation, detention infrastructure); (3) legitimacy and interpretive authority from states to supranational courts and the international legal order (the coordination rent).
% ABSENT_VOICES: Irregular migrants who never reach adjudication (intercepted at sea, turned back at land borders, disappeared in transit) — they would object to the proportionality test as a sham but are structurally excluded from the conversation. Stateless persons who fall between state jurisdictions — the constraint assumes a state of origin to return to; for the truly stateless, the proportionality test has no purchase. Future generations who inherit the normalized architecture of conditional rights — they are not present to contest the founding problem's status.
% DISAPPEARANCE_RATIONALE: If the qualified sovereignty constraint vanished overnight, states would revert to sovereignty_primary (unrestricted exclusion) or face pressure toward freedom_primary (open borders). The supranational adjudication layer would lose its core border jurisdiction. Migrants would lose even the procedural floor of proportionality review. The international legal order would lose a major stabilizing doctrine. The world of border governance would reorganize around either raw sovereignty or rights-absolute frameworks.
% FOUNDING_PROBLEM: Post-WWII need to prevent sovereign abuse of border control (denaturalization, mass expulsion, refugee rejection) while preserving the state system's foundational assumption that territorial authority includes membership control. The proportionality doctrine was the compromise: states keep the authority but must justify its exercise.
% FOUNDING_PROBLEM_CORROBORATION: States and UNHCR attest the problem is live: ongoing mass displacement, security threats, and resource pressures make conditional sovereignty necessary. Migrants' rights advocates and critical legal scholars attest the problem is dead: the proportionality standard has become a management tool for exclusion, not a constraint on it; the adjudication machinery legitimizes rather than limits. Independent corroboration: the 2018 Global Compact for Migration negotiations revealed that states treat proportionality as a ceiling, not a floor — they negotiated to weaken, not strengthen, the conditionality.
narrative_ontology:disappearance_verdict(border_normative_status__qualified_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(border_normative_status__qualified_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__qualified_sovereignty, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(border_normative_status__qualified_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(border_normative_status__qualified_sovereignty, 0.48, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_normative_status__qualified_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_normative_status__qualified_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_normative_status__qualified_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48) is substantial but not extreme: the constraint extracts from the excluded through denial of entry, detention, and procedural exhaustion, but the proportionality condition genuinely limits what states can do — it is not a blank check. Suppression (0.62) is higher than extraction because the constraint's persistence depends on active enforcement: states must be compelled to run proportionality analyses, courts must hear cases, treaty bodies must issue views. The theater ratio (0.38) reflects that a significant share of state 'compliance' is performative — proportionality analyses that rubber-stamp predetermined outcomes, procedural hurdles that exhaust claimants. Accessibility collapse (0.45) is moderate: alternatives (open borders, freedom_primary) are not conceptually collapsed but are politically suppressed. Resistance (0.55) is significant: states resist the adjudication burden, migrants resist through litigation and irregular movement, and the sovereignty_primary reading resists as a competing normative frame.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute per-seat classifications from the structural data. From the receiving_state_institutions seat, the constraint may compute as a scaffold (transitional support for rights-based governance) or a piton (degraded sovereignty ritual). From the excluded_migrants seat, it computes as a snare (extraction via denial). From the international_legal_order seat, it computes as a rope (coordination on shared standards). The citizen_nationals seat likely computes as a beneficiary with moderate extraction (they gain security but lose when the state overreaches). The displaced_citizens seat is distinctive: identity_locked exit makes them experience the constraint as a snare despite formal citizenship. The supranational_courts seat sees the coordination function most clearly but also the enforcement costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: receiving_state_institutions (capture sovereignty rents and legitimacy), citizen_nationals (collective self-determination benefit), international_legal_order (normative framework stabilization). Victims declared: excluded_migrants (bear denial/detention/removal), displaced_citizens (bear re-entry denial and status precarity), asylum_seekers_denied_entry (bear refoulement risk and rights suspension). The directionality derivation will assign low d to beneficiaries (subsidy), high d to victims (extraction). The adjudication burden falls on states (compliance cost) and migrants (procedural barrier) — this dual incidence is captured by the omega on adjudication_burden_distribution. The supranational courts are agenda_setters with analytical exit — they enforce but do not bear the extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-WWII need to prevent sovereign abuse while preserving state order) is contested: states claim it is live (ongoing migration pressures, security threats), migrants and rights advocates claim it is dead (the proportionality standard has become a management tool for exclusion). The constraint prevents mislabeling coordination as pure extraction by maintaining a genuine coordination function (the shared proportionality framework), but the extraction from the excluded is real and the adjudication machinery is the enforcement layer that sustains it. Without the coordination function, this would be a snare; without the extraction, it would be a rope. The tangled_rope classification captures the hybrid structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine distinct reading of the border_normative_status kernel, or does it collapse into one of its siblings under scrutiny?',
    'Formal comparison of the three readings'' structural parameters (beneficiary/victim sets, extraction directionality, coordination claims) in the engine; if two readings produce identical per-seat classifications across all power atoms, they are the same constraint under the ε-invariance principle.',
    'If this reading collapses into sovereignty_primary, the ''proportionality'' condition is performative theater; if it collapses into freedom_primary, the ''legitimate state interests'' carve-out is nominal. Distinctness is what makes the tangled_rope classification analytically meaningful.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Commitment that this is a structurally distinct kernel reading, not a rhetorical variant.').

omega_variable(
    proportionality_operationalization,
    'What operational standard counts as ''proportionate'' exercise of border control, and who adjudicates it?',
    'Track adjudicative outcomes across supranational courts (ECtHR, IACtHR, UN treaty bodies) and domestic high courts over 2015-2035; code for convergence/divergence on the proportionality threshold.',
    'If proportionality adjudication converges on a high threshold (near freedom_primary), this reading''s extraction from excluded migrants drops. If it converges on a low threshold (near sovereignty_primary), the human rights condition is decorative. The engine''s effective extraction χ is sensitive to this threshold via the suppression metric.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(proportionality_operationalization, empirical, 'Whether ''proportionality'' has a stable operational meaning that distinguishes this reading from its siblings.').

omega_variable(
    adjudication_burden_distribution,
    'Does the adjudication burden this reading creates fall primarily on states (as compliance cost) or on migrants (as procedural barrier)?',
    'Measure asylum processing times, legal aid availability, and appeal success rates across jurisdictions implementing this standard; compare burden distribution pre/post adoption.',
    'If the burden falls on states as costly compliance, the constraint is genuinely extractive from state institutions (beneficiary→payer shift). If it falls on migrants as procedural exhaustion, the constraint is more extractive from the excluded. This changes the beneficiary/victim structure and thus χ.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adjudication_burden_distribution, empirical, 'Incidence of the adjudication cost this reading''s legitimacy condition imposes.').

omega_variable(
    displaced_citizens_as_victims,
    'Are ''displaced citizens'' (citizens unable to return, stateless nationals, denaturalized persons) a genuine victim class of this constraint, or are they collateral to other constraints (denaturalization, statelessness conventions)?',
    'Isolate cases where border denial directly targets citizens'' re-entry rights under the proportionality doctrine vs. cases where exclusion follows from separate status-determination proceedings.',
    'If displaced citizens are primarily victims of this constraint, the victim set is broader and the constraint''s extraction is more diffuse. If they are victims of adjacent constraints, this reading''s victim set narrows to excluded_migrants and asylum_seekers_denied_entry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displaced_citizens_as_victims, conceptual, 'Boundary of the victim set for this specific reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__qualified_sovereignty, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(border_normative_status__qualified_sovereignty_tr_t0, border_normative_status__qualified_sovereignty, theater_ratio, 0, 0.22).
narrative_ontology:measurement(border_normative_status__qualified_sovereignty_tr_t5, border_normative_status__qualified_sovereignty, theater_ratio, 5, 0.28).
narrative_ontology:measurement(border_normative_status__qualified_sovereignty_tr_t10, border_normative_status__qualified_sovereignty, theater_ratio, 10, 0.33).
narrative_ontology:measurement(border_normative_status__qualified_sovereignty_tr_t15, border_normative_status__qualified_sovereignty, theater_ratio, 15, 0.36).
narrative_ontology:measurement(border_normative_status__qualified_sovereignty_tr_t20, border_normative_status__qualified_sovereignty, theater_ratio, 20, 0.38).

% Extraction over time
narrative_ontology:measurement(border_normative_status__qualified_sovereignty_be_t0, border_normative_status__qualified_sovereignty, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(border_normative_status__qualified_sovereignty_be_t5, border_normative_status__qualified_sovereignty, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(border_normative_status__qualified_sovereignty_be_t10, border_normative_status__qualified_sovereignty, base_extractiveness, 10, 0.43).
narrative_ontology:measurement(border_normative_status__qualified_sovereignty_be_t15, border_normative_status__qualified_sovereignty, base_extractiveness, 15, 0.46).
narrative_ontology:measurement(border_normative_status__qualified_sovereignty_be_t20, border_normative_status__qualified_sovereignty, base_extractiveness, 20, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(border_normative_status__qualified_sovereignty_su_t0, border_normative_status__qualified_sovereignty, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(border_normative_status__qualified_sovereignty_su_t5, border_normative_status__qualified_sovereignty, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(border_normative_status__qualified_sovereignty_su_t10, border_normative_status__qualified_sovereignty, suppression_requirement, 10, 0.57).
narrative_ontology:measurement(border_normative_status__qualified_sovereignty_su_t15, border_normative_status__qualified_sovereignty, suppression_requirement, 15, 0.6).
narrative_ontology:measurement(border_normative_status__qualified_sovereignty_su_t20, border_normative_status__qualified_sovereignty, suppression_requirement, 20, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_normative_status__qualified_sovereignty, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(border_normative_status__qualified_sovereignty, 0.12).
narrative_ontology:affects_constraint(border_normative_status__qualified_sovereignty, border_normative_status__sovereignty_primary).
narrative_ontology:affects_constraint(border_normative_status__qualified_sovereignty, border_normative_status__freedom_primary).
narrative_ontology:affects_constraint(border_normative_status__qualified_sovereignty, refugee_convention_non_refoulement).
narrative_ontology:affects_constraint(border_normative_status__qualified_sovereignty, statelessness_convention_1954).
narrative_ontology:affects_constraint(border_normative_status__qualified_sovereignty, arbitrary_detention_prohibition).

% DUAL FORMULATION NOTE:
% This constraint is one member of the border_normative_status constraint family (kernel_id: border_normative_status). The three readings decompose the colloquial label 'border sovereignty' into structurally distinct constraints with different ε values, different beneficiary/victim sets, and different classifications. The qualified_sovereignty reading has the highest adjudication burden and the most complex victim set. The sovereignty_primary reading has negligible extraction (mountain-claiming) but high suppression of alternatives. The freedom_primary reading has states as payers and migrants as beneficiaries (rope-claiming). All three are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(border_normative_status__qualified_sovereignty, institutional, 0.35).
constraint_indexing:directionality_override(border_normative_status__qualified_sovereignty, powerless, 0.92).
constraint_indexing:directionality_override(border_normative_status__qualified_sovereignty, moderate, 0.78).
constraint_indexing:directionality_override(border_normative_status__qualified_sovereignty, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
