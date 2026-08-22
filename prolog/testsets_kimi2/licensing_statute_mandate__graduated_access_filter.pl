% ============================================================================
% CONSTRAINT STORY: licensing_statute_mandate__graduated_access_filter
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_licensing_statute_mandate__graduated_access_filter, []).

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
    narrative_ontology:suppression_profile/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: licensing_statute_mandate__graduated_access_filter
 *   human_readable: Statutory Licensing as Graduated Access Filter
 *   domain: labor_economics/regulatory_policy/public_administration
 *
 * SUMMARY:
 *   This constraint story instantiates the graduated_access_filter reading of
 *   the contested kernel licensing_statute_mandate. Under this reading,
 *   statutory occupational licensing and credential requirements do not
 *   primarily assure competence or maximize incumbent rents, but instead
 *   operate as a graduated filtration mechanism that sorts potential labor
 *   market entrants by pre-existing class position and access to the time,
 *   money, and social capital required to complete credentialing. The
 *   credentialed class captures restricted-entry rents, while low-wealth
 *   aspirants and unlicensed competent practitioners bear the costs of
 *   exclusion. The constraint is actively enforced through statutory
 *   penalties for unlicensed practice and administrative control of
 *   examination and experience requirements. The sibling
 *   readingsâpublic_safety_coordination and
 *   rent_seeking_suppressionâframe the same statutory apparatus as consumer
 *   protection or incumbent rent extraction, respectively; this reading
 *   emphasizes the class-sorting dimension.
 *
 * KEY AGENTS:
 *   - credentialed_class: Primary beneficiary (organized/mobile) â collects scarcity rents from restricted entry
 *   - licensing_administrators: Agenda-setter (institutional/arbitrage) â designs and enforces barrier structure, collects fees
 *   - low_wealth_labor_entrants: Primary target (powerless/trapped) â excluded by resource requirements for exam prep, tuition, and unpaid experience
 *   - unlicensed_competent_practitioners: Secondary target (powerless/trapped) â faces criminal and civil penalties for practicing without statutory paper
 *   - consumer_advocates: Excluded voice (moderate/analytical) â would argue for lower-cost service access but is marginalized in policy debates
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(licensing_statute_mandate__graduated_access_filter, 0.82).
domain_priors:suppression_score(licensing_statute_mandate__graduated_access_filter, 0.88).
domain_priors:theater_ratio(licensing_statute_mandate__graduated_access_filter, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, extractiveness, 0.82).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(licensing_statute_mandate__graduated_access_filter, snare).
narrative_ontology:human_readable(licensing_statute_mandate__graduated_access_filter, "Statutory Licensing as Graduated Access Filter").
narrative_ontology:topic_domain(licensing_statute_mandate__graduated_access_filter, "labor_economics/regulatory_policy/public_administration").

domain_priors:requires_active_enforcement(licensing_statute_mandate__graduated_access_filter).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(licensing_statute_mandate__graduated_access_filter, '5ce43920-4834-42b2-85aa-dfdd8134d424').
narrative_ontology:cs_kernel_codification('5ce43920-4834-42b2-85aa-dfdd8134d424', formalized).
narrative_ontology:cs_authority_grounding('5ce43920-4834-42b2-85aa-dfdd8134d424', lineage).
narrative_ontology:cs_interpretation_layer_present('5ce43920-4834-42b2-85aa-dfdd8134d424').
narrative_ontology:cs_reading_relation('5ce43920-4834-42b2-85aa-dfdd8134d424', licensing_statute_mandate__public_safety_coordination, influences).
narrative_ontology:cs_reading_relation('5ce43920-4834-42b2-85aa-dfdd8134d424', licensing_statute_mandate__rent_seeking_suppression, coexists_with).
narrative_ontology:cs_axiom('5ce43920-4834-42b2-85aa-dfdd8134d424', foundational, credential_requirements_are_structurally_exclusionary).
narrative_ontology:cs_axiom_status(credential_requirements_are_structurally_exclusionary, holdable).
narrative_ontology:cs_axiom_grounding('5ce43920-4834-42b2-85aa-dfdd8134d424', credential_requirements_are_structurally_exclusionary, empirically_contingent).
narrative_ontology:cs_reference_frame('5ce43920-4834-42b2-85aa-dfdd8134d424', statutory_meritocratic_gate).
narrative_ontology:cs_drift_state('5ce43920-4834-42b2-85aa-dfdd8134d424', post_empirical_challenge_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5ce43920-4834-42b2-85aa-dfdd8134d424', '').
narrative_ontology:cs_kernel_id(licensing_statute_mandate__graduated_access_filter, licensing_statute_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__graduated_access_filter, credentialed_class).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, low_wealth_labor_entrants).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, unlicensed_competent_practitioners).
narrative_ontology:constraint_vindicates(licensing_statute_mandate__graduated_access_filter, credential_competence_equivalence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Licensed practitioners who have cleared statutory barriers and now enjoy restricted-entry wage premia. They benefit from reduced competition and publicly justify the system as protecting professional standards. Exit from the constraint is unnecessary because they are its beneficiaries, though they retain geographic mobility through interstate reciprocity agreements.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, credentialed_class, beneficiary,
    organized, biographical, mobile, national).

% State boards and regulatory agencies that write examination rules, evaluate experience hours, and enforce unlicensed practice statutes. They collect application and renewal fees and derive institutional budgets, staffing, and authority from the gatekeeping function.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, licensing_administrators, agenda_setter,
    institutional, generational, arbitrage, national).

% Aspirants who possess competence or willingness to work but lack the savings, credit, or unpaid time to complete mandated exam preparation, tuition, or experience hours. They are sorted out of licensed occupations before reaching the market and often diverted into lower-wage, unregulated sectors.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, low_wealth_labor_entrants, payer,
    powerless, immediate, trapped, national).

% Workers with demonstrable skills who perform services in the informal sector without statutory credentials. They face fines, criminal penalties, and civil liability if discovered, and cannot convert lived experience into legally recognized practice.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, unlicensed_competent_practitioners, payer,
    powerless, immediate, trapped, local).

% Organizations and researchers who argue that lower-cost, non-credentialed service options would benefit consumers, particularly in low-income communities. They are structurally excluded from licensing board deliberations and outspent by incumbent associations in legislative lobbying.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, consumer_advocates, excluded,
    moderate, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(licensing_statute_mandate__graduated_access_filter, credentialed_class).
narrative_ontology:fixing_cost_class(licensing_statute_mandate__graduated_access_filter, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Purports to solve consumer information asymmetry about practitioner competence, but structurally operates as a class-sorting mechanism that segments labor market entry by pre-existing wealth and resource access.
% TRANSFER_FUNCTION: Moves economic rents from excluded workers and consumers paying inflated service prices to credentialed incumbents, and moves regulatory fees and compliance burdens from aspirants to licensing administrators.
% ABSENT_VOICES: Low-wealth labor entrants and unlicensed competent practitioners are excluded from policy formation; consumer advocates arguing for lower-cost access are marginalized by incumbent professional associations.
% DISAPPEARANCE_RATIONALE: If statutory credential requirements vanished, labor market entry would surge in licensed occupations, incumbent wage premia would compress toward competitive levels, alternative training pathways would re-emerge, and the licensing enforcement apparatus would become obsolete.
% FOUNDING_PROBLEM: Information asymmetry between consumers and service providers, with risk of harm from incompetent practice.
% FOUNDING_PROBLEM_CORROBORATION: Independent labor economists and civil-rights attorneys attest that consumer harm in licensed occupations is rare and addressable through less restrictive means; licensing boards and professional associations attest the problem remains acute. No corroboration from neutral consumer-protection agencies exists without incumbent capture.
narrative_ontology:disappearance_verdict(licensing_statute_mandate__graduated_access_filter, world_rearranges).
narrative_ontology:founding_problem_status(licensing_statute_mandate__graduated_access_filter, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(licensing_statute_mandate__graduated_access_filter, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(licensing_statute_mandate__graduated_access_filter, 'none', 1).
narrative_ontology:epsilon_provenance(licensing_statute_mandate__graduated_access_filter, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(licensing_statute_mandate__graduated_access_filter_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(licensing_statute_mandate__graduated_access_filter, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(licensing_statute_mandate__graduated_access_filter_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82) is high because the barrier structure transfers substantial occupational rents to incumbents and excludes a class of would-be entrants. Suppression (0.88) is higher because the constraint depends on active statutory enforcementâunlicensed practice acts, fines, and imprisonmentâto prevent alternative pathways. Theater ratio (0.45) reflects that the competence-testing apparatus performs real screening at some margin, but a growing share of regulatory activity is devoted to justifying barriers that exceed what risk exposure warrants. Accessibility collapse (0.75) is high because informal apprenticeship, third-party certification, and consumer reputation mechanisms have been legally crowded out. Resistance (0.60) reflects persistent but organizationally weak opposition from marginalized workers and some economists, countered by well-resourced incumbent associations.
 *
 * PERSPECTIVAL GAP:
 *   From the credentialed_class seat, the constraint appears as a legitimate professional standard protecting consumers and rewarding educational investment. From the low-wealth_entrant seat, the same statute operates as a structurally enforced class ceiling. The engine computes this divergence from the power, exit, and role asymmetries rather than from any authored reconciliation.
 *
 * DIRECTIONALITY LOGIC:
 *   The credentialed_class sits near the full-beneficiary end (low d): the constraint subsidizes their wages by restricting supply. Licensing_administrators are agenda-setters with arbitrage-grade exitâthey can move across jurisdictions or into private practiceâbut their institutional revenue and authority depend on the constraint's persistence, placing them in the beneficiary-administrator zone. Low_wealth_labor_entrants and unlicensed_competent_practitioners are full targets (high d): they bear the extraction directly through exclusion and enforcement. Consumer_advocates are analytical and excluded; they do not materially benefit or pay.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by requiring named victims and active enforcement for snare classification, which this story supplies. A rope reading would require that participants be net beneficiaries with minimal coercive overhead; here, the payer class is net harmed and enforcement is substantial. A piton reading would require atrophied function and diffuse capture; here the extraction is concentrated, actively maintained, and vigorously defended by identifiable beneficiaries, so piton is ruled out. The scaffold gate fails because no sunset clause exists and the arrangement is justified as permanent steady-state.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Does the licensing statute kernel function primarily as class-sorting extraction, consumer safety coordination, or incumbent rent extraction?',
    'Comparative cross-jurisdiction analysis of credential barrier height, practitioner class-origin demographics, consumer harm rates, and incumbent wage premia.',
    'Resolution determines whether the constraint is classified as snare (this reading), rope or scaffold (public safety), or snare with different beneficiary structure (rent seeking).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Kernel reading ambiguity for licensing statutes').

omega_variable(
    credential_quality_correlation,
    'Do higher statutory credential barriers correlate with measurable service quality improvements, or primarily with reduced labor supply and higher incumbent wages?',
    'Cross-jurisdiction regression of licensing requirements against consumer complaint rates, harm incidents, and occupational wage premia, controlling for income and education.',
    'If no correlation with quality, the public safety coordination story collapses and extraction dominates the classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credential_quality_correlation, empirical, 'Empirical test of the consumer protection justification').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (statutory penalties and policing of unlicensed practice) or internalized (aspirants and consumers believe formal credentials are the only legitimate marker of competence)?',
    'Compare rates of informal practice and consumer willingness to hire unlicensed providers in jurisdictions with weak versus strong enforcement, holding income constant.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggestsâthe target population carries the suppression with them after statutory removal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(licensing_statute_mandate__graduated_access_filter, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lice_tr_t0, licensing_statute_mandate__graduated_access_filter, theater_ratio, 0, 0.15).
narrative_ontology:measurement(lice_tr_t10, licensing_statute_mandate__graduated_access_filter, theater_ratio, 10, 0.25).
narrative_ontology:measurement(lice_tr_t20, licensing_statute_mandate__graduated_access_filter, theater_ratio, 20, 0.33).
narrative_ontology:measurement(lice_tr_t30, licensing_statute_mandate__graduated_access_filter, theater_ratio, 30, 0.39).
narrative_ontology:measurement(lice_tr_t40, licensing_statute_mandate__graduated_access_filter, theater_ratio, 40, 0.43).
narrative_ontology:measurement(lice_tr_t50, licensing_statute_mandate__graduated_access_filter, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(lice_be_t0, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(lice_be_t10, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(lice_be_t20, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(lice_be_t30, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 30, 0.72).
narrative_ontology:measurement(lice_be_t40, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 40, 0.78).
narrative_ontology:measurement(lice_be_t50, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 50, 0.82).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(licensing_statute_mandate__graduated_access_filter, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(licensing_statute_mandate__graduated_access_filter, public_safety_coordination).
narrative_ontology:affects_constraint(licensing_statute_mandate__graduated_access_filter, rent_seeking_suppression).

% DUAL FORMULATION NOTE:
% This constraint is the graduated_access_filter reading of the licensing_statute_mandate kernel. Sibling readings public_safety_coordination and rent_seeking_suppression decompose the same statutory phenomenon into distinct structural claims with different epsilon values, beneficiary sets, and stakeholder arrangements.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
