% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_balance__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_balance__proportionality_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: vaccine_mandate_balance__proportionality_reading
 *   human_readable: Proportionality Threshold for Vaccine Mandates
 *   domain: public_health_ethics/constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The proportionality reading of vaccine mandate balance holds that
 *   state-compelled vaccination is permissible only when three conditions are
 *   met: (1) disease severity crosses a high threshold, (2) transmission risk
 *   justifies population-level intervention, and (3) vaccine safety and
 *   efficacy data meet rigorous standards. Exemptions (medical, religious,
 *   philosophical) must be substantively available and procedurally
 *   accessible. This reading positions itself between the categorical
 *   rejection of mandates (bodily_autonomy_primary) and the permissive public
 *   health primacy view (public_health_primary). The constraint is the legal
 *   doctrine itself — the proportionality test that courts and legislatures
 *   apply. It operates as a coordination mechanism: it structures the
 *   legitimate exercise of state power, protects individuals from
 *   disproportionate mandates, and enables mandates when genuinely warranted.
 *   Its extraction is conditional: when thresholds are met, the state may
 *   impose mandates (extracting compliance from individuals); when thresholds
 *   are not met, the constraint blocks mandates (protecting individuals). The
 *   state executive branch is the primary payer — it bears the burden of
 *   proof and the political cost of meeting the test. The general public and
 *   vulnerable populations are beneficiaries of both the protection from
 *   disease (when mandates are allowed) and the protection from state
 *   overreach (when mandates are blocked). Individuals subject to mandates
 *   are beneficiaries of the test's limiting function but payers when a
 *   mandate is upheld.
 *
 * KEY AGENTS:
 *   - state_executive_branch: Primary payer (powerful/constrained) — must satisfy the proportionality test to impose mandates; bears evidentiary and political costs
 *   - courts_legislature: Agenda setter (institutional/arbitrage) — articulate and enforce the proportionality standard; set the thresholds
 *   - general_public: Beneficiary (organized/constrained) — gains disease protection when mandates are justified, gains liberty protection when they are not
 *   - vulnerable_populations: Beneficiary (powerless/trapped) — disproportionately benefit from mandates that achieve herd immunity; cannot exit the risk environment
 *   - individuals_subject_to_mandate: Beneficiary/Payer (moderate/constrained) — protected from unjust mandates, but bear compliance costs when mandates pass the test
 *   - public_health_authorities: Observer (institutional/analytical) — provide evidence on severity, transmission, and vaccine safety; shape the factual predicates of the test
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__proportionality_reading, 0.35).
domain_priors:suppression_score(vaccine_mandate_balance__proportionality_reading, 0.25).
domain_priors:theater_ratio(vaccine_mandate_balance__proportionality_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__proportionality_reading, rope).
narrative_ontology:human_readable(vaccine_mandate_balance__proportionality_reading, "Proportionality Threshold for Vaccine Mandates").
narrative_ontology:topic_domain(vaccine_mandate_balance__proportionality_reading, "public_health_ethics/constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__proportionality_reading, '5e95fe05-88bf-4788-afd7-e572e82e3d2b').
narrative_ontology:cs_kernel_codification('5e95fe05-88bf-4788-afd7-e572e82e3d2b', formalized).
narrative_ontology:cs_authority_grounding('5e95fe05-88bf-4788-afd7-e572e82e3d2b', lineage).
narrative_ontology:cs_interpretation_layer_present('5e95fe05-88bf-4788-afd7-e572e82e3d2b').
narrative_ontology:cs_reading_relation('5e95fe05-88bf-4788-afd7-e572e82e3d2b', vaccine_mandate_balance__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('5e95fe05-88bf-4788-afd7-e572e82e3d2b', vaccine_mandate_balance__public_health_primary, coexists_with).
narrative_ontology:cs_axiom('5e95fe05-88bf-4788-afd7-e572e82e3d2b', foundational, state_compulsion_requires_proportional_justification).
narrative_ontology:cs_axiom_status(state_compulsion_requires_proportional_justification, holdable).
narrative_ontology:cs_axiom_grounding('5e95fe05-88bf-4788-afd7-e572e82e3d2b', state_compulsion_requires_proportional_justification, deontological).
narrative_ontology:cs_axiom('5e95fe05-88bf-4788-afd7-e572e82e3d2b', foundational, exemptions_must_be_substantively_accessible).
narrative_ontology:cs_axiom_status(exemptions_must_be_substantively_accessible, holdable).
narrative_ontology:cs_axiom_grounding('5e95fe05-88bf-4788-afd7-e572e82e3d2b', exemptions_must_be_substantively_accessible, conventional).
narrative_ontology:cs_reference_frame('5e95fe05-88bf-4788-afd7-e572e82e3d2b', classical_proportionality_framework).
narrative_ontology:cs_drift_state('5e95fe05-88bf-4788-afd7-e572e82e3d2b', post_covid_emergency_jurisprudence, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5e95fe05-88bf-4788-afd7-e572e82e3d2b', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__proportionality_reading, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__proportionality_reading, general_public).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__proportionality_reading, vulnerable_populations).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__proportionality_reading, individuals_subject_to_mandate).
narrative_ontology:constraint_victim(vaccine_mandate_balance__proportionality_reading, state_executive_branch).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vaccine_mandate_balance__proportionality_reading, individuals_subject_to_mandate).
narrative_ontology:constraint_vindicates(vaccine_mandate_balance__proportionality_reading, proportionality_principle_in_public_health_law).
narrative_ontology:constraint_vindicates(vaccine_mandate_balance__proportionality_reading, least_restrictive_means_doctrine).
narrative_ontology:constraint_vindicates(vaccine_mandate_balance__proportionality_reading, evidence_based_policy_making).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Initiates vaccine mandates during public health emergencies. Must satisfy the proportionality test by presenting evidence on disease severity, transmission risk, and vaccine safety. Bears the evidentiary burden, litigation costs, and political accountability. Cannot easily avoid the test when crises arise; exit would mean forgoing a primary public health tool.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, state_executive_branch, payer,
    powerful, biographical, constrained, national).

% Articulate the proportionality standard (legislatures via statutes, courts via judicial review). Define the thresholds for severity, transmission, and safety. Determine the robustness required for exemptions. Can reform the test through legislation or precedent. Their exit is arbitrage-grade — they control the legal framework itself.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, courts_legislature, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefits from the test in two ways: when mandates are upheld, gains disease protection through herd immunity; when mandates are blocked, gains protection from state overreach. Organized through democratic processes but individually constrained — cannot opt out of the legal system or the disease environment.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, general_public, beneficiary,
    organized, biographical, constrained, national).

% Immunocompromised, elderly, and other high-risk groups who depend on population-level immunity. They benefit disproportionately when mandates are justified and upheld. They are trapped in the risk environment — cannot exit the threat of severe disease, and lack the power to influence the test's application.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, vulnerable_populations, beneficiary,
    powerless, biographical, trapped, national).

% Bear the compliance costs (vaccination, potential adverse effects, loss of bodily autonomy) when a mandate passes the proportionality test. Benefit from the test's limiting function when it blocks disproportionate mandates. Their exit options are constrained — they can seek exemptions (if robust), relocate, or litigate, but cannot easily escape the legal regime.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, individuals_subject_to_mandate, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_balance__proportionality_reading, individuals_subject_to_mandate, beneficiary).

% Provide the epidemiological and safety evidence that feeds the proportionality test (case fatality rates, R0 estimates, vaccine trial data). They shape the factual predicates but do not control the legal thresholds. Their role is analytical — they observe the constraint's operation and supply data, but are not directly constrained by it.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, public_health_authorities, observer,
    institutional, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_balance__proportionality_reading, diffuse).
narrative_ontology:fixing_cost_class(vaccine_mandate_balance__proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective action problem of determining when state compulsion of medical intervention is legitimate. Provides a structured, evidence-based framework that balances collective protection against individual liberty, preventing both arbitrary mandates and categorical paralysis.
% TRANSFER_FUNCTION: When the test is satisfied, transfers compliance costs (vaccination, autonomy intrusion) from individuals subject to mandate to the general public (disease protection). When the test is not satisfied, transfers the risk of disease spread to the public in exchange for preserving individual liberty. The state executive branch always bears the procedural costs of meeting the test.
% ABSENT_VOICES: Future generations (who inherit the precedent set by proportionality jurisprudence), non-citizen residents (who may be subject to mandates but lack political voice), and pathogen evolution itself (which changes the factual predicates unpredictably). These voices are structurally excluded from the current legal process.
% DISAPPEARANCE_RATIONALE: If the proportionality test vanished overnight, two regimes would likely fill the void: either categorical mandates (public_health_primary) leading to routine compulsion for minor diseases, or categorical bans (bodily_autonomy_primary) leading to uncontrolled outbreaks of severe diseases. The legal landscape, public health practice, and the balance of state power vs individual rights would fundamentally reorganize.
% FOUNDING_PROBLEM: The proportionality test was built to solve the problem of arbitrary state power in public health — the historical pattern of either unchecked mandate authority (leading to abuse) or absolute prohibitions (leading to preventable mass death). It emerged from constitutional jurisprudence seeking a middle ground that respects both collective survival and individual dignity.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by constitutional courts across multiple jurisdictions (e.g., Jacobson v. Massachusetts lineage in US, proportionality analysis in EU and Canadian law), by public health ethicists outside the state apparatus (e.g., Childress et al. on public health ethics frameworks), and by historical records of mandate abuse (e.g., forced vaccination campaigns in colonial contexts). No single beneficiary group monopolizes this attestation.
narrative_ontology:disappearance_verdict(vaccine_mandate_balance__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_balance__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vaccine_mandate_balance__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_balance__proportionality_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_balance__proportionality_reading_tests).
:- end_tests(vaccine_mandate_balance__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.35) because the constraint itself does not extract; it gates extraction by mandates. The 0.35 reflects the average extraction potential across pathogens — higher for smallpox-like diseases, near-zero for seasonal flu. Suppression is low (0.25) because the test does not suppress alternatives (voluntary vaccination, NPIs); it only suppresses unjustified mandates. Theater ratio is low (0.15) — the test is genuinely operationalized in courts, though some jurisdictions perform proportionality analysis ritually. Accessibility collapse is moderate (0.40) — alternative legal frameworks exist (strict scrutiny, rational basis) but the proportionality test has become dominant in many systems. Resistance is moderate-high (0.55) — state actors frequently contest the stringency of the test, and judicial deference varies. The claimed type is rope: the test coordinates legitimate state action with minimal coercive overhead, participants (the public) are net beneficiaries, and alternatives (other legal standards) are not suppressed.
 *
 * PERSPECTIVAL GAP:
 *   The state executive branch experiences this constraint as a burden (high directionality toward target) — it must marshal evidence, survive judicial review, and absorb political backlash. The general public and vulnerable populations experience it as a protective coordination mechanism (low directionality toward beneficiary). Individuals subject to mandates experience it bidirectionally: as a shield against disproportionate mandates and as a gateway to mandated compliance. Courts experience it as an analytical framework (directionality near zero). The engine will compute these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The state executive branch is the primary payer: it initiates mandates and must satisfy the test, bearing evidentiary costs and political risk. Its exit options are constrained (it cannot easily avoid the test when public health crises arise). Courts and legislature are agenda_setters: they define the test and enforce it, with arbitrage-grade exit (they can reform the test). The general public and vulnerable populations are beneficiaries: they gain protection without bearing the costs of the test's operation. Individuals subject to mandates are dual-role: beneficiaries of the test's limiting function, payers when a mandate is upheld. Their exit options are constrained (they cannot opt out of the legal system). Public health authorities are observers: they supply the factual predicates but do not control the legal standard.
 *
 * MANDATROPHY ANALYSIS:
 *   The proportionality test was founded to solve the problem of arbitrary state power in public health — preventing both tyranny (unchecked mandates) and paralysis (categorical bans). That founding problem remains live: new pathogens emerge, vaccine technologies evolve, and the balance must be continuously recalibrated. The test has not atrophied into a piton; it is actively litigated and its thresholds are contested in each pandemic. However, there is a risk of mandatrophy if courts apply the test deferentially (rubber-stamping mandates) or if legislatures codify exemptions so broadly that the test becomes a nullity. The omega on exemption robustness captures this.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the vaccine_mandate_balance kernel, or does it collapse into one of the sibling readings under scrutiny?',
    'Compare the structural operation of the proportionality test across jurisdictions and pathologies; if the test''s victim/beneficiary sets and extraction profile remain conditional on disease parameters in a way that cannot be reduced to the categorical premises of the sibling readings, the reading is structurally distinct.',
    'If the reading collapses, the constraint story should be merged with the dominating sibling; if distinct, it stands as a separate constraint with its own ε and classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Structural distinctness of the proportionality reading within the kernel').

omega_variable(
    epsilon_variability_by_pathogen,
    'Does the base extractiveness (ε) of this constraint vary so widely across pathogens (smallpox vs seasonal flu) that it constitutes multiple constraints rather than one?',
    'Author separate constraint stories for high-severity/high-transmission pathogens (where mandates are frequently permitted) and low-severity/low-transmission pathogens (where mandates are rarely permitted) and compare their metric profiles; if ε differs by >0.3, decompose per ε-invariance principle.',
    'If ε is pathogen-dependent, the single constraint story masks a family of constraints; decomposition would yield distinct classifications (e.g., tangled_rope for smallpox, rope for flu).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_variability_by_pathogen, empirical, 'Whether ε-invariance requires pathogen-specific decomposition').

omega_variable(
    exemption_robustness_operationalization,
    'What constitutes a ''robust'' exemption in practice, and does the operationalization of exemptions shift the constraint toward snare or tangled_rope?',
    'Survey exemption grant rates, procedural accessibility, and judicial review standards across jurisdictions; if exemptions are theoretically robust but practically inaccessible, the constraint''s effective suppression and extraction increase.',
    'If exemptions are nominal, the constraint operates as a snare for non-exempt individuals; if genuinely accessible, it remains a rope with a safety valve.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exemption_robustness_operationalization, empirical, 'Operational reality of exemption mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__proportionality_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vmb_prop_tr_t0, vaccine_mandate_balance__proportionality_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(vmb_prop_tr_t0, observed).
narrative_ontology:measurement(vmb_prop_tr_t25, vaccine_mandate_balance__proportionality_reading, theater_ratio, 25, 0.1).
narrative_ontology:measurement_basis(vmb_prop_tr_t25, observed).
narrative_ontology:measurement(vmb_prop_tr_t50, vaccine_mandate_balance__proportionality_reading, theater_ratio, 50, 0.15).
narrative_ontology:measurement_basis(vmb_prop_tr_t50, observed).
narrative_ontology:measurement(vmb_prop_tr_t75, vaccine_mandate_balance__proportionality_reading, theater_ratio, 75, 0.18).
narrative_ontology:measurement_basis(vmb_prop_tr_t75, observed).
narrative_ontology:measurement(vmb_prop_tr_t100, vaccine_mandate_balance__proportionality_reading, theater_ratio, 100, 0.15).
narrative_ontology:measurement_basis(vmb_prop_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(vmb_prop_be_t0, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement_basis(vmb_prop_be_t0, observed).
narrative_ontology:measurement(vmb_prop_be_t25, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 25, 0.3).
narrative_ontology:measurement_basis(vmb_prop_be_t25, observed).
narrative_ontology:measurement(vmb_prop_be_t50, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 50, 0.35).
narrative_ontology:measurement_basis(vmb_prop_be_t50, observed).
narrative_ontology:measurement(vmb_prop_be_t75, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 75, 0.38).
narrative_ontology:measurement_basis(vmb_prop_be_t75, observed).
narrative_ontology:measurement(vmb_prop_be_t100, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 100, 0.35).
narrative_ontology:measurement_basis(vmb_prop_be_t100, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(vaccine_mandate_balance__proportionality_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_balance__proportionality_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vaccine_mandate_balance__proportionality_reading, 0.1).
narrative_ontology:affects_constraint(vaccine_mandate_balance__proportionality_reading, vaccine_mandate_balance__bodily_autonomy_primary).
narrative_ontology:affects_constraint(vaccine_mandate_balance__proportionality_reading, vaccine_mandate_balance__public_health_primary).

% DUAL FORMULATION NOTE:
% This constraint is one member of the vaccine_mandate_balance constraint family. The three readings (proportionality_reading, bodily_autonomy_primary, public_health_primary) decompose the kernel into structurally distinct constraints with different ε profiles, beneficiary/victim sets, and classifications. The proportionality reading is the only one with conditional victim sets and pathogen-dependent ε. The bodily_autonomy_primary reading has near-zero ε (mountain-like) but high suppression of state action. The public_health_primary reading has higher ε (tangled_rope) because it permits mandates under broader conditions. All three are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vaccine_mandate_balance__proportionality_reading, powerful, 0.75).
constraint_indexing:directionality_override(vaccine_mandate_balance__proportionality_reading, institutional, 0.1).
constraint_indexing:directionality_override(vaccine_mandate_balance__proportionality_reading, organized, 0.2).
constraint_indexing:directionality_override(vaccine_mandate_balance__proportionality_reading, powerless, 0.15).
constraint_indexing:directionality_override(vaccine_mandate_balance__proportionality_reading, moderate, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
