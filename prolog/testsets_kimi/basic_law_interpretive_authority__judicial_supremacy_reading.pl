% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_authority__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_authority__judicial_supremacy_reading, []).

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
 *   constraint_id: basic_law_interpretive_authority__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy in Constitutional Interpretation
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   This constraint instantiates the judicial_supremacy_reading of the
 *   basic_law_interpretive_authority kernel. In this reading, courts hold
 *   final interpretive authority over constitutional meaning, grounded in
 *   specialized legal expertise and independence from political pressure. The
 *   judiciary enters the beneficiary set through accumulated institutional
 *   authority and prestige, while elected legislatures and electoral
 *   majorities enter the victim set when judicial review blocks legislation
 *   and democratic preferences. The constraint coordinates constitutional
 *   settlement but extracts democratic autonomy from coordinate branches and
 *   popular majorities. It requires active enforcement through judicial
 *   decisions, contempt powers, and executive compliance.
 *
 * KEY AGENTS:
 *   - judiciary: Primary agenda-setter and beneficiary (institutional/generational) â accumulates interpretive authority
 *   - elected_legislatures: Primary payer (institutional/generational) â bears gridlock and invalidation costs
 *   - electoral_majorities: Secondary payer (powerless/biographical) â democratic preferences filtered by judicial review
 *   - popular_constitutionalists: Excluded voice (moderate/generational) â argues for ongoing democratic contestation
 *   - constitutional_scholars: Analytical observer (analytical/generational) â evaluates empirical and normative effects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_authority__judicial_supremacy_reading, 0.72).
domain_priors:suppression_score(basic_law_interpretive_authority__judicial_supremacy_reading, 0.65).
domain_priors:theater_ratio(basic_law_interpretive_authority__judicial_supremacy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_authority__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_authority__judicial_supremacy_reading, "Judicial Supremacy in Constitutional Interpretation").
narrative_ontology:topic_domain(basic_law_interpretive_authority__judicial_supremacy_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(basic_law_interpretive_authority__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_authority__judicial_supremacy_reading, '28cc038b-4546-47ca-9326-3843580585e9').
narrative_ontology:cs_kernel_codification('28cc038b-4546-47ca-9326-3843580585e9', fixed_text).
narrative_ontology:cs_authority_grounding('28cc038b-4546-47ca-9326-3843580585e9', expertise).
narrative_ontology:cs_interpretation_layer_present('28cc038b-4546-47ca-9326-3843580585e9').
narrative_ontology:cs_reading_relation('28cc038b-4546-47ca-9326-3843580585e9', basic_law_interpretive_authority__parliamentary_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('28cc038b-4546-47ca-9326-3843580585e9', basic_law_interpretive_authority__popular_constitutionalism_reading, forecloses).
narrative_ontology:cs_axiom('28cc038b-4546-47ca-9326-3843580585e9', foundational, constitutional_settlement_requires_judicial_finality).
narrative_ontology:cs_axiom_status(constitutional_settlement_requires_judicial_finality, holdable).
narrative_ontology:cs_axiom_grounding('28cc038b-4546-47ca-9326-3843580585e9', constitutional_settlement_requires_judicial_finality, instrumental).
narrative_ontology:cs_axiom('28cc038b-4546-47ca-9326-3843580585e9', foundational, judicial_expertise_as_constitutional_authorship).
narrative_ontology:cs_axiom_status(judicial_expertise_as_constitutional_authorship, holdable).
narrative_ontology:cs_axiom_grounding('28cc038b-4546-47ca-9326-3843580585e9', judicial_expertise_as_constitutional_authorship, conventional).
narrative_ontology:cs_reference_frame('28cc038b-4546-47ca-9326-3843580585e9', constitutional_settlement_expertise).
narrative_ontology:cs_drift_state('28cc038b-4546-47ca-9326-3843580585e9', contemporary_polarized_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('28cc038b-4546-47ca-9326-3843580585e9', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_authority__judicial_supremacy_reading, basic_law_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__judicial_supremacy_reading, judiciary).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__judicial_supremacy_reading, elected_legislatures).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__judicial_supremacy_reading, electoral_majorities).
narrative_ontology:constraint_vindicates(basic_law_interpretive_authority__judicial_supremacy_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(basic_law_interpretive_authority__judicial_supremacy_reading, judicial_review_finality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds final interpretive authority over constitutional text through specialized legal expertise and institutional independence. Decisions are enforced by other branches. Derives institutional prestige, tenure security, and centrality to national political settlement. Professional identity is constituted by this authority; exit would require abandoning the judicial role or the legal framework itself.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, judiciary, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__judicial_supremacy_reading, judiciary, beneficiary).

% Enacts legislation subject to judicial invalidation on constitutional grounds. Bears gridlock costs when anticipating judicial review during drafting. Democratic mandate and policy autonomy are truncated by court interpretation. Exit requires supermajoritarian constitutional amendment or institutional crisis.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, elected_legislatures, payer,
    institutional, generational, constrained, national).

% Their policy preferences are blocked when courts invalidate popular legislation or referenda. Democratic self-rule is filtered through judicial interpretation. Exit via constitutional amendment is prohibitively difficult in most jurisdictions.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, electoral_majorities, payer,
    powerless, biographical, constrained, national).

% Advocate that constitutional meaning should emerge from ongoing democratic contestation rather than terminal judicial adjudication. Their framework is marginalized by the doctrine of judicial supremacy and excluded from final interpretive authority.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, popular_constitutionalists, excluded,
    moderate, generational, constrained, national).

% Analyze and debate the legitimacy, empirical effects, and democratic consequences of judicial supremacy. Produce comparative and theoretical studies on the distribution of constitutional authority.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_authority__judicial_supremacy_reading, judiciary).
narrative_ontology:fixing_cost_class(basic_law_interpretive_authority__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Settles disputes over constitutional meaning among coordinate branches of government, providing a single terminal arbiter to prevent constitutional crises and legal uncertainty.
% TRANSFER_FUNCTION: Moves final interpretive authority and institutional legitimacy from elected legislatures and electoral majorities to the judiciary; transfers a policy veto from democratic majorities to unelected judges.
% ABSENT_VOICES: Popular constitutionalists and direct-democracy advocates, who argue for perpetual democratic contestation over constitutional meaning, are structurally excluded from final interpretive authority; legislative dissenters who reject judicial finality are also marginalized.
% DISAPPEARANCE_RATIONALE: If judicial supremacy vanished, legislatures would regain final interpretive authority, constitutional meaning would become openly contested among branches, and the existing balance of powers would destabilize; political actors would need to construct alternative settlement mechanisms.
% FOUNDING_PROBLEM: How to resolve disputes over the meaning of a written constitution among coordinate branches without descending into institutional deadlock or raw power contests.
% FOUNDING_PROBLEM_CORROBORATION: The Federalist Papers (Hamilton, outside the modern judiciary) and comparative constitutional scholars attest the need for constitutional settlement mechanisms; popular constitutionalists like Larry Kramer corroborate that the problem persists but argue judicial supremacy is not the appropriate solution. Legislative historians and political scientists outside the beneficiary set attest that gridlock and democratic truncation are live costs.
narrative_ontology:disappearance_verdict(basic_law_interpretive_authority__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_authority__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_authority__judicial_supremacy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(basic_law_interpretive_authority__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_authority__judicial_supremacy_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_authority__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_authority__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_authority__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.72) because judicial supremacy concentrates terminal authority in a non-elected branch, decoupling constitutional meaning from democratic majorities. Suppression (0.65) reflects the active exclusion of legislative override and popular constitutionalism through doctrine and institutional design. Theater_ratio (0.28) captures the performative element of legal reasoning that masks policy choices, though courts retain genuine adjudicative function. Accessibility_collapse (0.68) is high because alternatives (legislative supremacy, ongoing contestation) collapse once judicial finality is accepted. Resistance (0.45) is moderate: court-packing threats and jurisdiction-stripping proposals signal ongoing contestation but rarely succeed.
 *
 * PERSPECTIVAL GAP:
 *   The judiciary seat experiences the constraint as legitimate expertise and necessary settlement, while the legislative and electoral majority seats experience it as an externally imposed veto that truncates democratic self-rule. The engine computes this divergence from identical structural data: the agenda_setter/beneficiary has identity_locked exit and institutional power, while payers have constrained exit and bear the democratic cost.
 *
 * DIRECTIONALITY LOGIC:
 *   Judiciary is declared in beneficiaries and holds agenda_setter role with identity_locked exit, producing a low directionality (beneficiary end). Elected_legislatures and electoral_majorities are declared in victims with constrained exit, producing high directionality (target end). Constitutional_scholars as analytical observers sit outside the extraction flow. The derived d values should place the judiciary near the beneficiary end and the legislative/majority seats near the full-target end.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â inter-branch constitutional dispute â remains contested rather than dead, preventing automatic piton classification. The constraint has not atrophied into pure theater; judicial decisions still structure politics. However, the accumulation of extraction over time (see measurements) and the rise in theater_ratio suggest a tangled_rope drifting toward more extractive operation, warranting lifecycle monitoring.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_supremacy_kernel_contest,
    'Does judicial supremacy represent a necessary coordination function for constitutional settlement, or has it evolved into an extractive concentration of democratic authority in the judiciary?',
    'Comparative constitutional analysis of democratic stability and constitutional crisis frequency in systems with judicial supremacy (US), parliamentary sovereignty (UK), and weak-form review (Canada/NZ).',
    'If genuine coordination, classification trends toward rope or scaffold; if extractive capture, classification trends toward snare or tangled_rope with elevated theater_ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_supremacy_kernel_contest, conceptual, 'Kernel contest between coordination and extraction readings of judicial supremacy').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative interpretive authorities structural (Article V difficulty, institutional norms) or internalized (legislative deference to courts as legitimate)?',
    'Observe behavior during periods of court-packing threats or jurisdictional stripping: if legislatures assert interpretive independence when structural barriers loosen, suppression is structural; if deference persists, suppression is internalized.',
    'If internalized, effective suppression exceeds structural measures, increasing extractive severity for payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression of non-judicial constitutional interpretation').

omega_variable(
    scope_amplification_democratic_deficit,
    'Does the national scope of judicial supremacy amplify its extractiveness relative to decentralized or subsidiary interpretive systems?',
    'Cross-jurisdictional comparison of federal systems with centralized constitutional courts versus distributed interpretation.',
    'National scope increases effective extraction for electoral_majorities by removing sub-national exit options.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_amplification_democratic_deficit, empirical, 'Spatial scope amplification of democratic deficit').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_authority__judicial_supremacy_reading, 0, 220).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(basi_tr_t20, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(basi_tr_t40, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement(basi_tr_t80, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 80, 0.2).
narrative_ontology:measurement(basi_tr_t120, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 120, 0.24).
narrative_ontology:measurement(basi_tr_t160, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 160, 0.26).
narrative_ontology:measurement(basi_tr_t200, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 200, 0.27).
narrative_ontology:measurement(basi_tr_t220, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 220, 0.28).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(basi_be_t20, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(basi_be_t40, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 40, 0.46).
narrative_ontology:measurement(basi_be_t80, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 80, 0.54).
narrative_ontology:measurement(basi_be_t120, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 120, 0.62).
narrative_ontology:measurement(basi_be_t160, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 160, 0.68).
narrative_ontology:measurement(basi_be_t200, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 200, 0.71).
narrative_ontology:measurement(basi_be_t220, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 220, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(basi_su_t20, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(basi_su_t40, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(basi_su_t80, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 80, 0.55).
narrative_ontology:measurement(basi_su_t120, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 120, 0.5).
narrative_ontology:measurement(basi_su_t160, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 160, 0.52).
narrative_ontology:measurement(basi_su_t200, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 200, 0.6).
narrative_ontology:measurement(basi_su_t220, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 220, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_authority__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__judicial_supremacy_reading, parliamentary_sovereignty_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__judicial_supremacy_reading, popular_constitutionalism_reading).

% DUAL FORMULATION NOTE:
% This constraint is the judicial_supremacy_reading of the basic_law_interpretive_authority kernel, which decomposes into three readings: judicial supremacy, parliamentary sovereignty, and popular constitutionalism. Each reading instantiates a different distribution of interpretive authority and extracts from different institutional seats. They are linked as a constraint family via network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
