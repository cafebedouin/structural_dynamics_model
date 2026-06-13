% ============================================================================
% CONSTRAINT STORY: substance_control_authority__prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_authority__prohibition_reading, []).

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
 *   constraint_id: substance_control_authority__prohibition_reading
 *   human_readable: State Criminalization Authority for Drug Prohibition (Protective Reading)
 *   domain: criminal_justice/public_health/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the PROHIBITION READING of the
 *   substance_control_authority kernel: state authority to criminalize drug
 *   use/possession, justified as protecting third parties from drug-related
 *   crime and social disorder. The reading is one of three contested framings
 *   of the same stabilized commitment (the state's authority to regulate
 *   drugs). Under this reading, users are victims of criminalization, third
 *   parties are protected from harms, and enforcement institutions benefit
 *   from authority and budget allocation. The claim/metric independence is
 *   deliberate: this reading CLAIMS the constraint is tangled_rope (genuine
 *   coordination problem: preventing third-party harm + asymmetric extraction
 *   via criminalization), while the authored metrics describe substantially
 *   extractive, heavily enforced operation with rising theater ratio
 *   (enforcement increasingly divorced from protective function). The engine
 *   measures this gap; do not reconcile the claim to the metrics.
 *
 * KEY AGENTS:
 *   - state_criminal_authority: Sets the criminalization schedule, directs enforcement, administers courts and prisons. Powerful institutional actor with arbitrage exit (can adjust policy at will). Structural beneficiary.
 *   - users_criminalized: Subject to criminalization, arrest, prosecution, incarceration. Powerless, trapped (criminalization forecloses legal alternatives). Primary victim seat.
 *   - third_party_communities: Promised protective benefit from reduced drug-related crime. Organized but constrained (their exit is relocation, expensive and disruptive). Claimed beneficiary seat.
 *   - law_enforcement_institutions: Collect budget, authority, prestige from drug enforcement. Institutional power, mobile exit (can deprioritize drugs). Structural beneficiary, often more directly than third parties.
 *   - families_of_users: Identity-locked to users; bear costs of incarceration and stigma while experiencing mixed protective/extractive effects. Moderate power, generational time horizon.
 *   - communities_overpoliced: Bear enforcement burden disproportionately; not the primary locus of drug use but suffer surveillance and arrest concentration. Organized but constrained; dual victim/targeted seat.
 *   - public_health_researchers: Analytical observers who measure outcomes; their findings contest the protective framing but are marginalized in criminal-justice policy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__prohibition_reading, 0.76).
domain_priors:suppression_score(substance_control_authority__prohibition_reading, 0.81).
domain_priors:theater_ratio(substance_control_authority__prohibition_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, extractiveness, 0.76).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__prohibition_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_authority__prohibition_reading, "State Criminalization Authority for Drug Prohibition (Protective Reading)").
narrative_ontology:topic_domain(substance_control_authority__prohibition_reading, "criminal_justice/public_health/political_economy").

domain_priors:requires_active_enforcement(substance_control_authority__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__prohibition_reading, 'd07b0828-33ab-49e9-b2aa-d69511b5646e').
narrative_ontology:cs_kernel_codification('d07b0828-33ab-49e9-b2aa-d69511b5646e', formalized).
narrative_ontology:cs_authority_grounding('d07b0828-33ab-49e9-b2aa-d69511b5646e', extraction).
narrative_ontology:cs_interpretation_layer_present('d07b0828-33ab-49e9-b2aa-d69511b5646e').
narrative_ontology:cs_reading_relation('d07b0828-33ab-49e9-b2aa-d69511b5646e', substance_control_authority__harm_reduction_reading, coexists_with).
narrative_ontology:cs_reading_relation('d07b0828-33ab-49e9-b2aa-d69511b5646e', substance_control_authority__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('d07b0828-33ab-49e9-b2aa-d69511b5646e', foundational, drug_use_criminally_causative_harm).
narrative_ontology:cs_axiom_status(drug_use_criminally_causative_harm, holdable).
narrative_ontology:cs_axiom_grounding('d07b0828-33ab-49e9-b2aa-d69511b5646e', drug_use_criminally_causative_harm, empirically_contingent).
narrative_ontology:cs_axiom('d07b0828-33ab-49e9-b2aa-d69511b5646e', foundational, criminal_penalty_deters_drug_use).
narrative_ontology:cs_axiom_status(criminal_penalty_deters_drug_use, holdable).
narrative_ontology:cs_axiom_grounding('d07b0828-33ab-49e9-b2aa-d69511b5646e', criminal_penalty_deters_drug_use, empirically_contingent).
narrative_ontology:cs_reference_frame('d07b0828-33ab-49e9-b2aa-d69511b5646e', criminal_deterrence_framework).
narrative_ontology:cs_drift_state('d07b0828-33ab-49e9-b2aa-d69511b5646e', contemporary_institutional_capture_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d07b0828-33ab-49e9-b2aa-d69511b5646e', '').
narrative_ontology:cs_kernel_id(substance_control_authority__prohibition_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, third_party_communities).
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, law_enforcement_institutions).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, users_criminalized).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, communities_overpoliced).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_authority__prohibition_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(substance_control_authority__prohibition_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_authority__prohibition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_authority__prohibition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_authority__prohibition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts moderate (0.48) at interval start and rises monotonically to 0.76 at interval end, plateauing. This trajectory reflects the ratchet dynamic: as criminalization becomes institutionalized, enforcement infrastructure expands (DEA, task forces, surveillance capacity), creating constituencies for continuation. Extractiveness rises because the constraint increasingly extracts value from criminalized populations without proportional reduction in the protective benefit (which itself becomes more theatrical). Suppression is high (0.81 at end) because the constraint's persistence depends fundamentally on excluding alternatives: drug users cannot legally satisfy their substance use, harm-reduction advocates are excluded from policy forums, and decriminalization/legalization reading are treated as criminal permissiveness. The constraint must actively suppress the articulation of alternative frameworks. Theater ratio rises from 0.18 to 0.42: early in the interval, enforcement genuinely reduced some drug-related crime; by interval end, enforcement is increasingly devoted to maintaining the criminalization architecture itself (mandatory minimums, asset seizure, federal scheduling lobbying) rather than solving the third-party harms that justified the original authority. The measurements are authored on a single shared time grid so every metric is valued at every time point the others are examined.
 *
 * PERSPECTIVAL GAP:
 *   The state authority and law enforcement institutions experience this constraint as a coordination success: they have solved the third-party-protection problem and are administering the solution. From the user seats and overpoliced-community seats, the same structure operates as pure extraction: the protective benefit is either marginal or negative (users and communities bear enforcement harms that exceed the protective gains), and the arrangement persists by actively suppressing alternatives. The engine computes these divergent seat classifications from the structural data. The authorization shift from 'we protect third parties' to 'we protect our institutional authority' happens at the individual-agent level (police officer choosing to enforce or not) and the institutional level (law enforcement lobbying against policy alternatives). The perspectival gap is irreducible in the commitment-system framing: the same kernel generates incompatible readings depending on which seat perceives the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Users_criminalized: d approaches 1.0 (full target). Victims by declaration; trapped exit (criminalization is the only path forward short of geographic relocation, which is blocked by poverty and criminal record); powerless; identity of 'user' may be fused with self-concept, creating psychological identity-lock beyond legal/geographic trapping. Effective extraction χ scales upward from high base ε by full target directionality. Communities_overpoliced: d around 0.75–0.85 (strong target). Named victims; constrained exit (police presence is inescapable in their geographic scope); organized power gives them some collective voice, but not enough to shift policy; spatial scope (regional/national) means they cannot vote police enforcement away. Effective extraction χ scales by target directionality plus scope amplification. Third_party_communities: d around 0.25–0.35 (beneficiary end). Named beneficiaries; promised protective benefit; organized power; constrained exit (relocation is expensive and disruptive). The constraint extracts from users to provide them a protective good. Directionality of third-party communities is POSITIVE (they benefit) but not zero because they also bear indirect costs: the constraint narrows their own choices (police presence, restriction on treatment options for family members). Law_enforcement_institutions: d around 0.15–0.25 (beneficiary end). Named beneficiary; institutional power; arbitrage exit (policy can shift, though institutional inertia is high). They collect extraction directly (budget, authority). Effective extraction χ is inverted to subsidy; they are subsidized by the constraint. No overrides are needed; the structural derivation produces accurate directionality at each seat.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is evident in the theater-ratio trajectory. The founding problem was drug-related crime and third-party harm. The original mandate was to deter drug use via criminal penalty, which would reduce crime and disorder. After ~24 time points (mid-interval), the theater ratio shows enforcement effort increasingly devoted to maintaining the criminalization schedule itself rather than achieving the protective outcome. Indicators: (a) drug-war budget increases even as drug-related violent crime declines; (b) federal scheduling decisions driven by pharmaceutical-industry lobbying (protecting market share) rather than public-health evidence; (c) mandatory-minimum laws become harder to reform despite empirical evidence they are ineffective at deterrence; (d) asset-seizure revenue becomes a direct budget source for law enforcement, creating financial incentive to arrest independent of protective benefit. The mandate (protect third parties from drug harms via deterrence) has outlived its primary function (that function is partially achieved or is no longer the actual driver of enforcement decisions). The constraint persists because it benefits institutional actors whose primary incentive is continuation, not outcome. The theater-ratio rise from 0.18 to 0.42 models this: by interval end, 42% of enforcement activity is performative (defending the apparatus) rather than functional (solving the founding problem). The Tangled_Rope claim acknowledges the mandatrophy: there IS a real coordination function (reducing third-party harm), but it is entangled with pure extraction (maintaining institutional power and budget). A pure Snare would have no genuine third-party benefit; a pure Rope would have no extraction. The actual constraint is both, and the theater ratio captures the drift toward the extraction pole.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    protective_benefit_counterfactual,
    'What fraction of the measured reduction in drug-related crime is attributable to the deterrent effect of criminalization, versus to other factors (improved employment, treatment access, neighborhood economic change, demographic shifts)?',
    'Quasi-experimental study comparing jurisdictions with different enforcement intensities controlling for socioeconomic variables; or interrupted time-series analysis of enforcement escalations/de-escalations with crime outcomes.',
    'If criminalization accounts for <20% of crime reduction, the protective-benefit framing collapses and the constraint reclassifies toward pure Snare. If >50%, the Tangled_Rope claim is more defensible. Current econometric literature estimates 15–35%, with high uncertainty.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(protective_benefit_counterfactual, empirical, 'Attribution of crime reduction to criminalization vs. confounders.').

omega_variable(
    racial_disparities_mechanism,
    'Are the documented racial disparities in drug enforcement (arrest rates, sentencing) the result of differential drug use prevalence across racial groups, or of discriminatory enforcement targeting of communities?',
    'Survey data on drug use prevalence by race; comparison of enforcement actions to prevalence; qualitative interviews with police about enforcement priorities; analysis of surveillance technology deployment by neighborhood race/poverty.',
    'If disparities are purely driven by prevalence, the constraint''s asymmetric harm (concentrated on overpoliced communities) is indirect. If driven by enforcement discrimination, the constraint''s structure includes active targeting of powerless communities for extraction, reclassifying it toward Snare. Evidence strongly suggests both prevalence and targeting play a role, with targeting amplifying the disparities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(racial_disparities_mechanism, empirical, 'Whether racial disparities in enforcement reflect usage patterns or discriminatory targeting.').

omega_variable(
    substitution_dynamics,
    'When criminalization reduces supply of one drug, do users and dealers substitute to other substances, or does reduction in supply reduce overall drug use? Does substitution create new harms (more dangerous substances, larger criminal organizations)?',
    'Time-series analysis of enforcement against one drug and market emergence of substitutes; epidemiological tracking of overdose by substance type and purity; DEA seizure data by drug class pre/post enforcement spikes.',
    'If substitution is rapid and harms increase (more potent substances, larger cartels), the constraint''s protective benefit is illusory and the extraction is pure. If substitution is minimal and use genuinely declines, the protective case is stronger. Evidence shows substantial substitution (fentanyl replacing heroin, synthetic drugs replacing plant-based drugs) with increased overdose mortality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(substitution_dynamics, empirical, 'Whether criminalization reduces drug use or triggers harmful substitution.').

omega_variable(
    identity_fusion_suppression_ambiguity,
    'For users and families of users, is the measured suppression (trapped/identity_locked exit) structural (legal/economic barriers that would disappear with policy change) or internalized (the user has internalized criminal-justice framing as deserved punishment and carries the suppression with them even if legal barriers were removed)?',
    'Post-decriminalization trajectory analysis in jurisdictions that shifted policy: do users'' subjective sense of agency and belonging recover, or do internalized shame and criminal identity persist?',
    'If structural, decriminalization would recover agency and exit options. If internalized, the constraint has done iatrogenic identity damage that persists after the mechanism is removed. Likely both are present to some degree; the question is the proportion. If internalization is >60%, the constraint''s damage exceeds its protective benefit even after policy reversal.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_fusion_suppression_ambiguity, empirical, 'Whether suppression of users is structural or internalized via criminalization narratives.').

omega_variable(
    reading_contest_structure,
    'Is the prohibition reading''s core premise (drug users are the source of third-party harm requiring criminalization) logically compatible with the harm-reduction reading''s core premise (drug users are people experiencing a health condition requiring treatment, not punishment)?',
    'Jurisprudential and policy analysis: can the same legal framework simultaneously treat drug use as crime AND as medical condition? How do jurisdictions that hold both achieve consistency? What does the choice reveal about the reading that is actually operative?',
    'If the premises are logically incompatible (they are), then adoption of the harm-reduction reading FORECLOSES the prohibition reading''s core claim within any single coherent framework. However, multiple frameworks can coexist across different jurisdictions and policy domains (DEA treats it as crime; public health treats it as illness). The manifest reading choice is which framework the state allocates primary authority to; currently, criminal authority dominates, making prohibition operative despite medical-model advocacy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_structure, conceptual, 'Whether the prohibition and harm-reduction readings are logically compatible or foreclosed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__prohibition_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_authority__prohibition_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(subs_tr_t8, substance_control_authority__prohibition_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement(subs_tr_t16, substance_control_authority__prohibition_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(subs_tr_t24, substance_control_authority__prohibition_reading, theater_ratio, 24, 0.36).
narrative_ontology:measurement(subs_tr_t32, substance_control_authority__prohibition_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement(subs_tr_t40, substance_control_authority__prohibition_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(subs_tr_t50, substance_control_authority__prohibition_reading, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_authority__prohibition_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(subs_be_t8, substance_control_authority__prohibition_reading, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(subs_be_t16, substance_control_authority__prohibition_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement(subs_be_t24, substance_control_authority__prohibition_reading, base_extractiveness, 24, 0.7).
narrative_ontology:measurement(subs_be_t32, substance_control_authority__prohibition_reading, base_extractiveness, 32, 0.74).
narrative_ontology:measurement(subs_be_t40, substance_control_authority__prohibition_reading, base_extractiveness, 40, 0.76).
narrative_ontology:measurement(subs_be_t50, substance_control_authority__prohibition_reading, base_extractiveness, 50, 0.76).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_authority__prohibition_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(subs_su_t8, substance_control_authority__prohibition_reading, suppression_requirement, 8, 0.68).
narrative_ontology:measurement(subs_su_t16, substance_control_authority__prohibition_reading, suppression_requirement, 16, 0.72).
narrative_ontology:measurement(subs_su_t24, substance_control_authority__prohibition_reading, suppression_requirement, 24, 0.76).
narrative_ontology:measurement(subs_su_t32, substance_control_authority__prohibition_reading, suppression_requirement, 32, 0.79).
narrative_ontology:measurement(subs_su_t40, substance_control_authority__prohibition_reading, suppression_requirement, 40, 0.81).
narrative_ontology:measurement(subs_su_t50, substance_control_authority__prohibition_reading, suppression_requirement, 50, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_authority__prohibition_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(substance_control_authority__prohibition_reading, 0.12).
narrative_ontology:affects_constraint(substance_control_authority__prohibition_reading, substance_control_authority__harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_authority__prohibition_reading, substance_control_authority__legalization_reading).

% DUAL FORMULATION NOTE:
% The substance_control_authority kernel decomposes into three structurally distinct readings: (1) prohibition_reading (this constraint) — criminalization to protect third parties; (2) harm_reduction_reading — decriminalization + treatment + public health interventions; (3) legalization_reading — regulated market with quality/taxation controls. The ε values differ radically: prohibition_reading has high extraction (0.76) due to criminalization costs and incarceration harms; harm_reduction_reading has moderate extraction (0.35–0.45) due to treatment/supervision costs; legalization_reading has low extraction (0.15–0.25) if regulated as a normal market. The beneficiary/victim structures differ: prohibition creates users as victims; harm-reduction treats users as clients; legalization treats users as consumers. Each reading is a separate constraint with its own classification. They are linked via network edges because changing one reading constrains the others: proof that prohibition is ineffective at protective benefit (high theater ratio, low crime-reduction attribution) creates pressure toward harm_reduction or legalization readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
