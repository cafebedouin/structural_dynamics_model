% ============================================================================
% CONSTRAINT STORY: one_country_two_systems_framework__sovereignty_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_one_country_two_systems_framework__sovereignty_primacy_reading, []).

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
 *   constraint_id: one_country_two_systems_framework__sovereignty_primacy_reading
 *   human_readable: One Country, Two Systems â Sovereignty Primacy Reading
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   This constraint instantiates the sovereignty-primacy reading of the 'One
 *   Country, Two Systems' kernel. Under this reading, Hong Kong's autonomy is
 *   delegated by and revocable through PRC sovereign authority; national
 *   security and territorial integrity override local autonomy when they
 *   conflict. The National Security Law and mainland enforcement operations
 *   in Hong Kong are the material expression of this reading. The constraint
 *   is structurally hybrid: it coordinates state unity and security
 *   enforcement while extracting autonomy, civil liberties, and judicial
 *   independence from Hong Kong institutions and residents. The claim/metric
 *   independence is preserved â the claimed type is tangled_rope (genuine
 *   coordination function plus asymmetric extraction), while the metrics
 *   describe a heavily extractive, actively enforced arrangement.
 *
 * KEY AGENTS:
 *   - prc_central_government: Primary agenda-setter (institutional/arbitrage) â asserts sovereignty and enforcement authority
 *   - hk_government: Secondary agenda-setter (institutional/constrained) â local administration implementing PRC directives
 *   - mainland_security_apparatus: Primary beneficiary (institutional/mobile) â gains cross-border enforcement jurisdiction
 *   - hk_residents: Primary payer population (organized/constrained) â bear autonomy erosion and civil liberty restrictions
 *   - hk_judiciary: Institutional payer (institutional/constrained) â loses independence on national security matters
 *   - pro_democracy_activists: High-extraction target (powerless/trapped) â direct suppression via National Security Law
 *   - international_community: Analytical observer (institutional/analytical) â monitors treaty compliance without enforcement leverage
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__sovereignty_primacy_reading, 0.78).
domain_priors:suppression_score(one_country_two_systems_framework__sovereignty_primacy_reading, 0.85).
domain_priors:theater_ratio(one_country_two_systems_framework__sovereignty_primacy_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__sovereignty_primacy_reading, tangled_rope).
narrative_ontology:human_readable(one_country_two_systems_framework__sovereignty_primacy_reading, "One Country, Two Systems â Sovereignty Primacy Reading").
narrative_ontology:topic_domain(one_country_two_systems_framework__sovereignty_primacy_reading, "constitutional/political").

domain_priors:requires_active_enforcement(one_country_two_systems_framework__sovereignty_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__sovereignty_primacy_reading, '40d085d6-676a-40e4-91ca-6509cacb5e59').
narrative_ontology:cs_kernel_codification('40d085d6-676a-40e4-91ca-6509cacb5e59', formalized).
narrative_ontology:cs_authority_grounding('40d085d6-676a-40e4-91ca-6509cacb5e59', lineage).
narrative_ontology:cs_interpretation_layer_present('40d085d6-676a-40e4-91ca-6509cacb5e59').
narrative_ontology:cs_reading_relation('40d085d6-676a-40e4-91ca-6509cacb5e59', one_country_two_systems_framework__autonomy_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('40d085d6-676a-40e4-91ca-6509cacb5e59', one_country_two_systems_framework__balanced_coexistence_reading, forecloses).
narrative_ontology:cs_axiom('40d085d6-676a-40e4-91ca-6509cacb5e59', foundational, sovereignty_delegation_revocable).
narrative_ontology:cs_axiom_status(sovereignty_delegation_revocable, holdable).
narrative_ontology:cs_axiom_grounding('40d085d6-676a-40e4-91ca-6509cacb5e59', sovereignty_delegation_revocable, conventional).
narrative_ontology:cs_axiom('40d085d6-676a-40e4-91ca-6509cacb5e59', foundational, national_security_overrides_autonomy).
narrative_ontology:cs_axiom_status(national_security_overrides_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('40d085d6-676a-40e4-91ca-6509cacb5e59', national_security_overrides_autonomy, instrumental).
narrative_ontology:cs_reference_frame('40d085d6-676a-40e4-91ca-6509cacb5e59', unified_state_authority).
narrative_ontology:cs_drift_state('40d085d6-676a-40e4-91ca-6509cacb5e59', post_nsl_imposition, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('40d085d6-676a-40e4-91ca-6509cacb5e59', '').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__sovereignty_primacy_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, prc_central_government).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, mainland_security_apparatus).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, hk_government).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hk_residents).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hk_judiciary).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, pro_democracy_activists).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, civil_society_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the constitutional framework for Hong Kong's status through the Basic Law and National Security Law; asserts ultimate interpretive and enforcement authority over all matters touching on sovereignty and territorial integrity; can revoke or override local autonomy through NPCSC decisions.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, prc_central_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Administers Hong Kong affairs under the sovereignty-primacy framework, implements the National Security Law locally, appoints officials vetted for political loyalty, and cannot refuse mainland enforcement intervention without risking removal or constitutional bypass.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hk_government, agenda_setter,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__sovereignty_primacy_reading, hk_government, beneficiary).

% Gains expanded jurisdictional authority to operate within Hong Kong under the National Security Law; investigates, arrests, and transfers cases across the mainland-Hong Kong boundary; benefits from legal formalization of cross-border enforcement powers previously limited by local consent.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, mainland_security_apparatus, beneficiary,
    institutional, generational, mobile, national).

% Live under a legal system where mainland security law overrides local civil liberties protections; face erosion of freedoms of speech, assembly, and press; emigration is an exit path but carries biographical cost and not all can exercise it.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hk_residents, payer,
    organized, biographical, constrained, regional).

% Retains independence in commercial and ordinary criminal matters but is structurally subordinated on national security cases; mainland security apparatus can bypass or override local judicial process; judges face political vetting and institutional pressure.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hk_judiciary, payer,
    institutional, biographical, constrained, regional).

% Bear the highest direct costs of the sovereignty-primacy framework; subject to arrest, prosecution, and detention under the National Security Law for speech, assembly, or organizational activities previously protected under local law; exile is the only exit and criminalizes return.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, pro_democracy_activists, payer,
    powerless, immediate, trapped, local).

% Operate under tightened funding restrictions, foreign-agent registration pressures, and self-censorship requirements; advocacy on sovereignty or human rights becomes high-risk; organizational survival depends on avoiding politically sensitive issues.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, civil_society_groups, payer,
    moderate, biographical, constrained, local).

% Monitors the erosion of Hong Kong's autonomy against treaty commitments and international human rights standards; issues statements, sanctions, and legal opinions but lacks enforcement leverage over PRC sovereign decisions.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, international_community, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(one_country_two_systems_framework__sovereignty_primacy_reading, prc_central_government).
narrative_ontology:fixing_cost_class(one_country_two_systems_framework__sovereignty_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains territorial integrity and state unity by subordinating Hong Kong's separate legal system to PRC sovereign authority, providing a constitutional mechanism for national security enforcement across the special administrative region.
% TRANSFER_FUNCTION: Transfers autonomy, civil liberties protections, and judicial independence from Hong Kong residents and institutions to PRC central state authority, particularly in national security and territorial integrity domains.
% ABSENT_VOICES: Pro-independence activists, full-autonomy advocates, and international human rights monitors are structurally excluded from the constitutional conversation; their exclusion is enforced by the national security framework itself, which criminalizes the positions they would articulate.
% DISAPPEARANCE_RATIONALE: If the sovereignty-primacy framework vanished overnight, Hong Kong's legal system would revert to fuller autonomy, mainland security agents would withdraw, the National Security Law would cease to operate locally, and the boundary between mainland and Hong Kong jurisdiction would reconstitute itself along pre-2020 fault lines.
% FOUNDING_PROBLEM: How to reintegrate Hong Kong into the People's Republic of China after 156 years of colonial separation while preserving enough of its distinct economic and legal identity to ensure a stable transition and continued international commercial confidence.
% FOUNDING_PROBLEM_CORROBORATION: PRC central government attests the problem is still live, citing ongoing separatist threats. Hong Kong civil society, UK government treaty signatories, and UN human rights bodies attest the reintegration is accomplished and the framework now functions as sovereignty consolidation rather than transitional arrangement; independent international legal scholarship corroborates the autonomy-erosion reading from outside the beneficiary set.
narrative_ontology:disappearance_verdict(one_country_two_systems_framework__sovereignty_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(one_country_two_systems_framework__sovereignty_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__sovereignty_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(one_country_two_systems_framework__sovereignty_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(one_country_two_systems_framework__sovereignty_primacy_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(one_country_two_systems_framework__sovereignty_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(one_country_two_systems_framework__sovereignty_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(one_country_two_systems_framework__sovereignty_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the arrangement systematically transfers autonomy and rights protections from Hong Kong to Beijing; suppression is higher (0.85) because the framework's persistence depends on active enforcement via the National Security Law and mainland agent operations, not on voluntary compliance. Theater is moderate (0.45): the formal structures of separate courts and the Basic Law are maintained performatively while substantive decision-making migrates to mainland security and party organs. Accessibility collapse is high (0.75) because the legal and political alternatives to sovereignty subordination (independence, full autonomy, neutral arbitration) are structurally criminalized or delegitimized. Resistance is moderate (0.60): the 2019 protests demonstrated substantial resistance, but the post-NSL period shows suppression successfully containing it.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats (PRC state, HK administration) compute the constraint as legitimate coordination maintaining territorial integrity. The payer seats (HK residents, judiciary, activists) compute it as extraction of autonomy and rights. The engine produces this divergence from the structural data â identical metrics are scaled oppositely by directionality. The international observer seat sees the divergence itself but lacks power to alter it.
 *
 * DIRECTIONALITY LOGIC:
 *   The PRC central government, mainland security apparatus, and HK government are structural beneficiaries: they collect sovereignty consolidation, expanded jurisdiction, and political survival (low d, damped effective extraction). Hong Kong residents, the judiciary, pro-democracy activists, and civil society groups are structural targets: they bear the costs of eroded autonomy and rights (high d, amplified effective extraction). The divergence is sharp: the beneficiary seats experience the constraint as constitutional order and security provision; the payer seats experience it as coercive subordination.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â post-colonial reintegration â is contested as to whether it remains live. If the problem is dead and the arrangement persists as sovereignty consolidation, the mandatrophy path would push toward snare or piton. However, the coordination function (security enforcement across a sub-unit) is not fully theatrical â it is genuinely exercised. The tangled-rope classification captures this hybridity: the constraint is not pure extraction because state unity is a real coordination problem, but it is not pure coordination because the extraction is asymmetric and enforced. The R5 genealogy interview flags the contested founding-problem status for downstream analysis without forcing a false scaffold or snare classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_obsolescence,
    'Has the founding problem of post-colonial reintegration been solved, rendering the sovereignty-primacy framework a persistent extraction mechanism rather than transitional coordination?',
    'Historical comparison with other post-colonial reintegration processes; assessment of whether separatist threats are genuine or manufactured through state security framing.',
    'If the founding problem is dead, the constraint functions primarily as sovereignty-consolidation extraction; if live, it retains stronger tangled-rope coordination character.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the constraint persists because the problem remains or because the arrangement has become self-sustaining.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression in Hong Kong primarily structural (state coercion, legal penalties, extradition threats) or internalized (self-censorship, political disengagement, anticipatory obedience)?',
    'Post-exit suppression trajectory among emigrants; comparison of speech and organizational behavior inside versus outside the constraint''s reach.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests â the target population carries the suppression with them even where structural barriers are absent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism for a population-level constraint.').

omega_variable(
    judicial_subordination_necessity,
    'Is judicial subordination on national security matters a necessary feature of any sovereignty-over-autonomy framework, or is it extractive of rule-of-law protections beyond what territorial integrity requires?',
    'Comparative analysis of other special administrative regions, federal sub-units, and asymmetric autonomy regimes handling security matters.',
    'If subordination is necessary, the constraint''s base extractiveness is lower; if exceptional among comparable cases, the extraction is higher and more contingent on constructed authority.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_subordination_necessity, conceptual, 'Whether judicial subordination is structurally necessary or extractively excessive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__sovereignty_primacy_reading, 0, 27).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(one__tr_t0, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(one__tr_t9, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 9, 0.15).
narrative_ontology:measurement(one__tr_t18, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 18, 0.25).
narrative_ontology:measurement(one__tr_t22, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 22, 0.35).
narrative_ontology:measurement(one__tr_t23, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 23, 0.4).
narrative_ontology:measurement(one__tr_t27, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 27, 0.45).

% Extraction over time
narrative_ontology:measurement(one__be_t0, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(one__be_t9, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 9, 0.35).
narrative_ontology:measurement(one__be_t18, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 18, 0.45).
narrative_ontology:measurement(one__be_t22, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 22, 0.55).
narrative_ontology:measurement(one__be_t23, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 23, 0.72).
narrative_ontology:measurement(one__be_t27, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 27, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(one__su_t0, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(one__su_t9, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 9, 0.45).
narrative_ontology:measurement(one__su_t18, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 18, 0.5).
narrative_ontology:measurement(one__su_t22, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 22, 0.65).
narrative_ontology:measurement(one__su_t23, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 23, 0.82).
narrative_ontology:measurement(one__su_t27, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 27, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__sovereignty_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, one_country_two_systems_framework__autonomy_primacy_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, one_country_two_systems_framework__balanced_coexistence_reading).

% DUAL FORMULATION NOTE:
% This constraint is the sovereignty-primacy reading of the contested 'One Country, Two Systems' kernel. The kernel decomposes into at least three structurally distinct constraints because the natural-language label conflates claims with different epsilon values, beneficiary structures, and enforcement profiles. This reading treats autonomy as revocable delegation under absolute sovereignty; the autonomy reading treats autonomy as substantive and internationally guaranteed; the balanced reading treats the relationship as a negotiable functional division. They form a constraint family linked by mutual network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
