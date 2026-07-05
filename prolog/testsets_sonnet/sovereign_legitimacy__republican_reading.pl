% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__republican_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_legitimacy__republican_reading, []).

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
 *   constraint_id: sovereign_legitimacy__republican_reading
 *   human_readable: Republican Popular Sovereignty (Consent-of-the-Governed Legitimacy)
 *   domain: political_philosophy/constitutional_theory
 *
 * SUMMARY:
 *   This story instantiates the republican reading of the
 *   sovereign_legitimacy kernel: legitimate authority flows upward from the
 *   governed through delegated consent, validated by periodic electoral
 *   cycles and constitutional adherence. It is written as a clean,
 *   ε-invariant constraint distinct from its sibling readings (monarchical,
 *   constitutional_hybrid) — those are separate constraint files, not
 *   alternative measurements of this one. Historically the franchise has
 *   expanded (property, race, and sex qualifications progressively removed),
 *   which the temporal measurements trace as declining extraction and
 *   suppression from the founding period to the mid-20th century, followed by
 *   a plateau reflecting persistent but narrower exclusions (felony
 *   disenfranchisement, districting effects, non-citizen exclusion) that
 *   remain unresolved into the present.
 *
 * KEY AGENTS:
 *   - enfranchised_citizenry: primary beneficiary and nominal source of authority
 *   - elected_officials: agenda-setters who administer the consent mechanism and benefit from its legitimating effect
 *   - constitutional_courts: agenda-setters who police the boundary of valid consent
 *   - disenfranchised_residents, felony_disenfranchised_persons, non_citizen_residents: bear coercive authority without a seat in the consent mechanism
 *   - structurally_underrepresented_minorities: formally enfranchised but structurally vulnerable to majoritarian outcomes
 *   - political_theorists_and_courts_of_record: external corroborating observers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__republican_reading, 0.42).
domain_priors:suppression_score(sovereign_legitimacy__republican_reading, 0.38).
domain_priors:theater_ratio(sovereign_legitimacy__republican_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__republican_reading, tangled_rope).
narrative_ontology:human_readable(sovereign_legitimacy__republican_reading, "Republican Popular Sovereignty (Consent-of-the-Governed Legitimacy)").
narrative_ontology:topic_domain(sovereign_legitimacy__republican_reading, "political_philosophy/constitutional_theory").

domain_priors:requires_active_enforcement(sovereign_legitimacy__republican_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__republican_reading, '8ebdba6c-2b85-4d8d-a1ad-17a620e77df7').
narrative_ontology:cs_kernel_codification('8ebdba6c-2b85-4d8d-a1ad-17a620e77df7', formalized).
narrative_ontology:cs_authority_grounding('8ebdba6c-2b85-4d8d-a1ad-17a620e77df7', practice).
narrative_ontology:cs_interpretation_layer_present('8ebdba6c-2b85-4d8d-a1ad-17a620e77df7').
narrative_ontology:cs_reading_relation('8ebdba6c-2b85-4d8d-a1ad-17a620e77df7', sovereign_legitimacy__monarchical_reading, forecloses).
narrative_ontology:cs_reading_relation('8ebdba6c-2b85-4d8d-a1ad-17a620e77df7', sovereign_legitimacy__constitutional_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('8ebdba6c-2b85-4d8d-a1ad-17a620e77df7', foundational, authority_derives_from_governed_consent).
narrative_ontology:cs_axiom_status(authority_derives_from_governed_consent, holdable).
narrative_ontology:cs_axiom_grounding('8ebdba6c-2b85-4d8d-a1ad-17a620e77df7', authority_derives_from_governed_consent, deontological).
narrative_ontology:cs_axiom('8ebdba6c-2b85-4d8d-a1ad-17a620e77df7', secondary, periodic_electoral_validation_required_for_legitimacy).
narrative_ontology:cs_axiom_status(periodic_electoral_validation_required_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('8ebdba6c-2b85-4d8d-a1ad-17a620e77df7', periodic_electoral_validation_required_for_legitimacy, conventional).
narrative_ontology:cs_reference_frame('8ebdba6c-2b85-4d8d-a1ad-17a620e77df7', popular_sovereignty_social_contract).
narrative_ontology:cs_drift_state('8ebdba6c-2b85-4d8d-a1ad-17a620e77df7', contemporary_democratic_backsliding_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8ebdba6c-2b85-4d8d-a1ad-17a620e77df7', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__republican_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__republican_reading, enfranchised_citizenry).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__republican_reading, elected_officials).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__republican_reading, constitutional_courts).
narrative_ontology:constraint_victim(sovereign_legitimacy__republican_reading, disenfranchised_residents).
narrative_ontology:constraint_victim(sovereign_legitimacy__republican_reading, felony_disenfranchised_persons).
narrative_ontology:constraint_victim(sovereign_legitimacy__republican_reading, non_citizen_residents).
narrative_ontology:constraint_victim(sovereign_legitimacy__republican_reading, structurally_underrepresented_minorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__republican_reading, structurally_underrepresented_minorities).
narrative_ontology:constraint_victim(sovereign_legitimacy__republican_reading, enfranchised_citizenry).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the vote and participatory rights that constitute the legitimacy claim; periodically ratifies or removes officeholders through elections. Benefits from the coordination function of peaceful succession and accountable government, but also bears costs when majoritarian outcomes disadvantage minority factions within the enfranchised body itself. Exit is constrained: emigration or civil disobedience are the practical alternatives to participation.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, enfranchised_citizenry, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__republican_reading, enfranchised_citizenry, payer).

% Derive authority from periodic electoral validation and administer the machinery of consent — districting, ballot access, eligibility rules. They set the terms of what counts as valid consent and can shape franchise boundaries to their advantage. Their exit option (leaving office, seeking other institutional roles) is far more mobile than that of the governed.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, elected_officials, agenda_setter,
    institutional, biographical, mobile, national).

% Interpret the constitutional text that mediates between popular will and enforceable law, policing the boundary of legitimate consent (e.g., voting rights adjudication, apportionment). They benefit from the legitimacy the arrangement confers on their own interpretive authority.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, constitutional_courts, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__republican_reading, constitutional_courts, observer).

% Live under laws and enforcement they had no formal voice in authorizing — historically through property, race, or gender qualifications, and in the present through age, residency, or documentation status thresholds. Bear the full coercive weight of the state's authority while being structurally outside the consent mechanism that is claimed to legitimate it.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, disenfranchised_residents, payer,
    powerless, biographical, trapped, national).

% Stripped of voting rights through criminal conviction in many jurisdictions, sometimes permanently. Remain subject to taxation, policing, and law but are excluded from the delegation mechanism that is supposed to ground the state's authority over them.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, felony_disenfranchised_persons, payer,
    powerless, biographical, trapped, national).

% Work, pay taxes, and are governed by the same laws as citizens but categorically cannot vote or hold most office. Their interests are represented only indirectly, if at all, through the goodwill of enfranchised advocates.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, non_citizen_residents, excluded,
    powerless, biographical, trapped, national).

% Hold the formal franchise but see it diluted through districting, electoral thresholds, or majoritarian coalition dynamics that can produce durable minority status within a nominally consent-based system. Benefit from the formal right to participate while bearing the practical risk that majoritarian outcomes will not track their interests — the classic vulnerability the republican reading names but does not resolve.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, structurally_underrepresented_minorities, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__republican_reading, structurally_underrepresented_minorities, beneficiary).

% Comparative constitutional scholars and international tribunals assess whether a given republic's consent mechanisms meet the standard the republican reading claims for itself — free and fair elections, meaningful franchise, minority protections — providing external corroboration or critique independent of the domestic beneficiaries.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, political_theorists_and_courts_of_record, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sovereign_legitimacy__republican_reading, diffuse).
narrative_ontology:fixing_cost_class(sovereign_legitimacy__republican_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the succession and legitimacy problem: it provides a peaceful, rule-bound mechanism for authorizing and removing holders of coercive power, replacing dynastic inheritance or naked force with periodic, contestable validation by the governed.
% TRANSFER_FUNCTION: Moves formal authority and its coercive backing from the citizenry (as claimed source) to officeholders for fixed terms, in exchange for periodic accountability; simultaneously moves the practical burdens of governance (taxation, law enforcement, conscription in some systems) onto everyone subject to the jurisdiction, including those outside the franchise.
% ABSENT_VOICES: Disenfranchised residents, felony-disenfranchised persons, and non-citizen residents bear the state's coercive authority without a seat in the consent mechanism that is claimed to legitimate it; historically, propertyless persons, women, and racial minorities occupied this position before franchise expansions, which is direct evidence the excluded seat is real and has moved rather than being a hypothetical edge case.
% DISAPPEARANCE_RATIONALE: If the electoral/consent mechanism vanished overnight, the succession problem it solves would reopen immediately — officeholders would lose their claim to legitimate authority, accountability mechanisms (elections, recall, term limits) would cease to function, and some other legitimation basis (force, inheritance, technocratic claim) would have to fill the vacuum. Enfranchised citizens would lose their primary lever over governance; excluded groups would see no change in their situation, which is itself diagnostic of their exclusion.
% FOUNDING_PROBLEM: Replacing arbitrary, unaccountable, often violent successions of monarchical or oligarchic power with a legitimation basis that does not depend on birth, force, or divine claim — answering 'why should this person's commands bind me' with an answer traceable to the governed's own authorization.
% FOUNDING_PROBLEM_CORROBORATION: Comparative constitutional scholars and international election-monitoring bodies (outside the domestic beneficiary set) corroborate that the founding problem remains partially live in mature republics — genuine turnover and accountability do occur — while also documenting persistent gaps (felony disenfranchisement scope, districting manipulation, voter suppression litigation records) that beneficiaries themselves rarely surface unprompted. Excluded groups and their advocates attest the problem is very much unresolved for them specifically.
narrative_ontology:disappearance_verdict(sovereign_legitimacy__republican_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_legitimacy__republican_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__republican_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sovereign_legitimacy__republican_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sovereign_legitimacy__republican_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__republican_reading_tests).
:- end_tests(sovereign_legitimacy__republican_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) because the coordination function is genuine — the arrangement really does solve the succession/accountability problem for those inside the franchise — but a persistent excluded population bears governance costs without formal voice, which is asymmetric extraction riding on the same structure. Suppression is moderate (0.38) reflecting formal, ongoing barriers to entry for excluded groups (felony disenfranchisement statutes, non-citizen exclusion) rather than the higher suppression a monarchical reading would show. Theater ratio (0.28) is non-trivial: some electoral machinery (safe-seat elections, symbolic constitutional ratification votes) performs consent-validation without live contestation, but the bulk of the mechanism remains functionally load-bearing.
 *
 * PERSPECTIVAL GAP:
 *   From the enfranchised citizenry's seat, the arrangement looks like functioning rope: they vote, they can remove officeholders, the system responds to their preferences over time. From the excluded seats, the identical structure looks like an enforced hierarchy dressed in participatory language — they are governed, taxed, and policed by an authority that claims its legitimacy from 'the people' while defining them out of that category. The engine computes these divergent seat classifications from the same structural data; the story does not resolve which seat is 'correct.'
 *
 * DIRECTIONALITY LOGIC:
 *   Enfranchised citizens and elected officials sit near the beneficiary end — the former collects the coordination benefit and the deciding voice, the latter collects the authority itself. Constitutional courts benefit from the legitimacy their interpretive role is granted. Disenfranchised residents, felony-disenfranchised persons, and non-citizen residents sit at the target end: they are structurally trapped, bear the coercive costs of the arrangement, and have no formal lever within the consent mechanism to alter it. Structurally underrepresented minorities occupy a genuinely mixed position — formally enfranchised (lower d than the excluded groups) but functionally vulnerable to majoritarian coalition dynamics, which is exactly the vulnerability the republican reading's own theory names but does not resolve.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — legitimating succession without force, birth, or divine claim — remains partially live (turnover and accountability genuinely occur in functioning republics), which prevents wholesale reclassification as pure extraction. But the founding_problem_status is authored as contested rather than live, because for the excluded seats the problem is not solved at all; the arrangement's claim to derive authority from 'the people' while excluding defined subpopulations from that people is the structural tension a tangled_rope reading is built to hold, rather than mislabeling the whole arrangement as either pure coordination (ignoring the excluded) or pure extraction (ignoring the genuine accountability function for the enfranchised).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    franchise_boundary_as_kernel_disagreement,
    'Is the boundary of ''the people'' whose consent legitimates authority a fixed feature of the republican reading, or is it itself the site of ongoing renegotiation that changes what constraint is being evaluated?',
    'Track franchise-boundary litigation and legislative expansion/contraction over the interval; if the boundary is being actively renegotiated in a given period, treat that period as a distinct sub-reading rather than folding it into a single continuous ε.',
    'If franchise boundary shifts are severe enough (e.g., a near-total redefinition of the demos), the resulting arrangement may be a structurally distinct constraint rather than a drift within this one — this omega documents where the single-constraint assumption could break down.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(franchise_boundary_as_kernel_disagreement, conceptual, 'Whether franchise-boundary change constitutes drift within this reading or a new constraint').

omega_variable(
    majoritarian_tyranny_vulnerability,
    'Does the republican reading''s own accountability mechanism (majoritarian electoral cycles) structurally produce durable minority disadvantage that the reading''s theory cannot self-correct?',
    'Comparative analysis of minority political outcomes across republics with varying electoral system designs (proportional vs. majoritarian) to test whether the vulnerability is intrinsic to the republican form or contingent on specific electoral mechanics.',
    'If intrinsic, the tangled_rope classification is durable regardless of electoral reform; if contingent, some republics may structurally approach a purer rope classification while others remain closer to tangled_rope or snare for the affected minority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majoritarian_tyranny_vulnerability, empirical, 'Whether majoritarian vulnerability is intrinsic to the republican form or an artifact of specific electoral design').

omega_variable(
    corroboration_independence,
    'How independent are the corroborating comparative-scholarship and election-monitoring sources from the republics whose legitimacy they assess?',
    'Audit funding sources and institutional affiliation of major election-monitoring bodies and comparative constitutional scholarship for structural ties to the republics under assessment.',
    'Weak independence would undermine the founding_problem_corroboration claim and push the founding_problem_status assessment toward being self-asserted by beneficiaries rather than genuinely externally corroborated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(corroboration_independence, empirical, 'Whether corroborating observers are structurally independent of the assessed republics').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__republican_reading, 1789, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sove_tr_t1789, sovereign_legitimacy__republican_reading, theater_ratio, 1789, 0.35).
narrative_ontology:measurement(sove_tr_t1870, sovereign_legitimacy__republican_reading, theater_ratio, 1870, 0.32).
narrative_ontology:measurement(sove_tr_t1920, sovereign_legitimacy__republican_reading, theater_ratio, 1920, 0.3).
narrative_ontology:measurement(sove_tr_t1965, sovereign_legitimacy__republican_reading, theater_ratio, 1965, 0.26).
narrative_ontology:measurement(sove_tr_t2000, sovereign_legitimacy__republican_reading, theater_ratio, 2000, 0.27).
narrative_ontology:measurement(sove_tr_t2024, sovereign_legitimacy__republican_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(sove_be_t1789, sovereign_legitimacy__republican_reading, base_extractiveness, 1789, 0.55).
narrative_ontology:measurement(sove_be_t1870, sovereign_legitimacy__republican_reading, base_extractiveness, 1870, 0.5).
narrative_ontology:measurement(sove_be_t1920, sovereign_legitimacy__republican_reading, base_extractiveness, 1920, 0.46).
narrative_ontology:measurement(sove_be_t1965, sovereign_legitimacy__republican_reading, base_extractiveness, 1965, 0.4).
narrative_ontology:measurement(sove_be_t2000, sovereign_legitimacy__republican_reading, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement(sove_be_t2024, sovereign_legitimacy__republican_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(sove_su_t1789, sovereign_legitimacy__republican_reading, suppression_requirement, 1789, 0.62).
narrative_ontology:measurement(sove_su_t1870, sovereign_legitimacy__republican_reading, suppression_requirement, 1870, 0.55).
narrative_ontology:measurement(sove_su_t1920, sovereign_legitimacy__republican_reading, suppression_requirement, 1920, 0.48).
narrative_ontology:measurement(sove_su_t1965, sovereign_legitimacy__republican_reading, suppression_requirement, 1965, 0.4).
narrative_ontology:measurement(sove_su_t2000, sovereign_legitimacy__republican_reading, suppression_requirement, 2000, 0.38).
narrative_ontology:measurement(sove_su_t2024, sovereign_legitimacy__republican_reading, suppression_requirement, 2024, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__republican_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(sovereign_legitimacy__republican_reading, monarchical_reading).
narrative_ontology:affects_constraint(sovereign_legitimacy__republican_reading, constitutional_hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the sovereign_legitimacy kernel. monarchical_reading grounds authority downward from inherited/divine right (foreclosed by this reading's core premise, since both cannot simultaneously ground legitimate authority in the same political framework). constitutional_hybrid_reading coexists with this reading in dual-authority systems (e.g., constitutional monarchies) where ceremonial and political authority are split — a party could hold the hybrid reading without foreclosing the republican reading's account of the political-authority component. Each sibling is authored as its own ε-invariant file with its own beneficiary/victim structure and metrics; this file does not average over them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
