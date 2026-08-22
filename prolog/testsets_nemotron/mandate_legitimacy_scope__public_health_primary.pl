% ============================================================================
% CONSTRAINT STORY: mandate_legitimacy_scope__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mandate_legitimacy_scope__public_health_primary, []).

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
 *   constraint_id: mandate_legitimacy_scope__public_health_primary
 *   human_readable: State Authority to Compel Vaccination for Vulnerable Population Protection
 *   domain: public_health_ethics/constitutional_law/medical_autonomy
 *
 * SUMMARY:
 *   This constraint story instantiates the public_health_primary reading of
 *   the mandate_legitimacy_scope kernel. The reading holds that state
 *   authority to compel vaccination is legitimate when necessary to protect
 *   vulnerable populations from serious harm. The structural claim:
 *   immunocompromised and high-risk populations enter the victim set when
 *   mandates are absent (they bear externalized infection risk from
 *   unvaccinated others); the unvaccinated bear a duty to protect via
 *   vaccination; and mandate absence carries high extractiveness (ε) from the
 *   vulnerable population's perspective. The constraint coordinates disease
 *   suppression at population scale but extracts bodily autonomy from
 *   non-consenting individuals. Active enforcement (school mandates,
 *   healthcare worker requirements, travel restrictions) is required to
 *   sustain coverage. The claimed_type is tangled_rope: genuine coordination
 *   function (herd immunity protecting the vulnerable) combined with
 *   asymmetric extraction (bodily integrity violation for non-consenting
 *   adults, concentrated on vaccine-hesitant communities).
 *
 * KEY AGENTS:
 *   - immunocompromised_populations: Primary beneficiary (powerless/identity_locked) — protected by mandate-induced herd immunity
 *   - elderly_high_risk: Primary beneficiary (powerless/constrained) — protected by reduced community transmission
 *   - public_health_institutions: Agenda setter (institutional/generational/arbitrage) — administers mandates, collects compliance data, manages exemption processes
 *   - unvaccinated_by_choice: Primary victim (moderate/constrained) — compelled to vaccinate or face exclusion from school/work/travel
 *   - vaccine_hesitant_communities: Primary victim (organized/identity_locked) — bear disproportionate enforcement pressure, distrust medical establishment
 *   - medical_exemption_denied: Primary victim (powerless/trapped) — legitimate medical contraindications rejected by rigid exemption criteria
 *   - constitutional_courts: Observer (analytical/civilizational/analytical) — adjudicate legitimacy challenges, define scope of state power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__public_health_primary, 0.65).
domain_priors:suppression_score(mandate_legitimacy_scope__public_health_primary, 0.55).
domain_priors:theater_ratio(mandate_legitimacy_scope__public_health_primary, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, extractiveness, 0.65).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__public_health_primary, tangled_rope).
narrative_ontology:human_readable(mandate_legitimacy_scope__public_health_primary, "State Authority to Compel Vaccination for Vulnerable Population Protection").
narrative_ontology:topic_domain(mandate_legitimacy_scope__public_health_primary, "public_health_ethics/constitutional_law/medical_autonomy").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__public_health_primary, 'f39ecb2c-75f8-45df-9116-42247b4e8fae').
narrative_ontology:cs_kernel_codification('f39ecb2c-75f8-45df-9116-42247b4e8fae', formalized).
narrative_ontology:cs_authority_grounding('f39ecb2c-75f8-45df-9116-42247b4e8fae', lineage).
narrative_ontology:cs_interpretation_layer_present('f39ecb2c-75f8-45df-9116-42247b4e8fae').
narrative_ontology:cs_reading_relation('f39ecb2c-75f8-45df-9116-42247b4e8fae', mandate_legitimacy_scope__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('f39ecb2c-75f8-45df-9116-42247b4e8fae', mandate_legitimacy_scope__proportionality_reading, influences).
narrative_ontology:cs_axiom('f39ecb2c-75f8-45df-9116-42247b4e8fae', foundational, vulnerable_population_protection_justifies_compulsion).
narrative_ontology:cs_axiom_status(vulnerable_population_protection_justifies_compulsion, holdable).
narrative_ontology:cs_axiom_grounding('f39ecb2c-75f8-45df-9116-42247b4e8fae', vulnerable_population_protection_justifies_compulsion, deontological).
narrative_ontology:cs_axiom('f39ecb2c-75f8-45df-9116-42247b4e8fae', foundational, herd_immunity_as_collective_duty).
narrative_ontology:cs_axiom_status(herd_immunity_as_collective_duty, holdable).
narrative_ontology:cs_axiom_grounding('f39ecb2c-75f8-45df-9116-42247b4e8fae', herd_immunity_as_collective_duty, instrumental).
narrative_ontology:cs_reference_frame('f39ecb2c-75f8-45df-9116-42247b4e8fae', jacobson_police_power_framework).
narrative_ontology:cs_drift_state('f39ecb2c-75f8-45df-9116-42247b4e8fae', post_covid_mandate_expansion, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f39ecb2c-75f8-45df-9116-42247b4e8fae', '').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__public_health_primary, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, immunocompromised_populations).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, elderly_high_risk).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, public_health_institutions).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__public_health_primary, unvaccinated_by_choice).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__public_health_primary, vaccine_hesitant_communities).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__public_health_primary, medical_exemption_denied).
narrative_ontology:constraint_vindicates(mandate_legitimacy_scope__public_health_primary, public_health_emergency_powers_doctrine).
narrative_ontology:constraint_vindicates(mandate_legitimacy_scope__public_health_primary, collective_immunity_threshold_theory).
narrative_ontology:constraint_vindicates(mandate_legitimacy_scope__public_health_primary, parens_patriae_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Cannot safely receive live vaccines or mount adequate immune response. Depend on community-wide vaccination coverage to reduce exposure risk. Their vulnerability is not chosen and cannot be exited — they are structurally dependent on others' vaccination decisions. When mandates are absent, they bear concentrated infection risk externalized by unvaccinated individuals.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, immunocompromised_populations, beneficiary,
    powerless, biographical, identity_locked, national).

% Age-related immune decline and comorbidities create high risk of severe outcomes. Benefit from reduced community transmission via mandates. Can partially self-isolate but at severe cost to quality of life and social participation. Exit from vulnerability is biologically constrained.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, elderly_high_risk, beneficiary,
    powerless, biographical, constrained, national).

% Design, administer, and enforce vaccination mandates (school entry, healthcare employment, travel). Collect compliance data, manage exemption processes, define target diseases and schedules. Institutional legitimacy and funding depend on successful disease control. Can modify mandates (add/remove vaccines, adjust exemptions) through regulatory process. Capture risk: institutional survival may incentivize mandate expansion beyond epidemiological necessity.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, public_health_institutions, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(mandate_legitimacy_scope__public_health_primary, public_health_institutions, beneficiary).

% Decline vaccination for personal, religious, or philosophical reasons. Face escalating exclusion: school denial, employment termination, travel restrictions, social stigma. Can access exemptions in some jurisdictions but process is burdensome. Exit from constraint requires either vaccination (violating conscience) or accepting exclusion from core civic/economic participation.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, unvaccinated_by_choice, payer,
    moderate, biographical, constrained, national).

% Communities with historical distrust of medical establishment (e.g., communities of color with Tuskegee legacy, religious communities with doctrinal objections). Bear disproportionate enforcement pressure. Hesitancy is identity-fused — vaccination feels like betrayal of community identity. Exit requires identity rupture, not just behavior change. Structurally excluded from mandate design process; their objections are treated as misinformation rather than legitimate dissent.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, vaccine_hesitant_communities, payer,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(mandate_legitimacy_scope__public_health_primary, vaccine_hesitant_communities, excluded).

% Individuals with legitimate medical contraindications (allergies, immunodeficiency, prior adverse events) whose exemption requests are denied by rigid criteria or skeptical reviewers. Cannot vaccinate safely, cannot access exemption, face full enforcement penalties. No exit exists — trapped between medical contraindication and state compulsion. Small in number but extreme in extraction intensity.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, medical_exemption_denied, payer,
    powerless, immediate, trapped, local).

% Adjudicate constitutional challenges to mandates (Jacobson v. Massachusetts precedent, religious freedom claims, due process). Define the scope of state police power vs. individual liberty. Their rulings structurally modify the constraint's enforcement boundary. Do not bear extraction or receive benefit directly; their institutional role is interpretive authority.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, constitutional_courts, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(mandate_legitimacy_scope__public_health_primary, public_health_institutions).
narrative_ontology:fixing_cost_class(mandate_legitimacy_scope__public_health_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Achieves population-level immunity sufficient to interrupt transmission of contagious diseases, thereby protecting individuals who cannot be vaccinated or who remain vulnerable despite vaccination. Solves the collective action problem where individual vaccination decisions create externalities for the most vulnerable.
% TRANSFER_FUNCTION: Transfers bodily autonomy and decision-making authority from individuals (unvaccinated_by_choice, vaccine_hesitant_communities, medical_exemption_denied) to public health institutions, which compel vaccination as condition of civic participation. The transfer is justified as protecting immunocompromised_populations and elderly_high_risk from externalized infection risk.
% ABSENT_VOICES: Children (subject to school mandates without consent capacity), future generations (who inherit mandate infrastructure and precedent), and global populations (excluded from domestic mandate benefits but affected by vaccine nationalism). These voices are structurally absent from the legitimacy calculus — mandates are adjudicated by domestic courts and legislatures with no formal representation for these groups.
% DISAPPEARANCE_RATIONALE: If vaccination mandates vanished overnight: childhood vaccination rates would drop (historical precedent: UK pertussis 1970s, Japan pertussis 1970s), leading to disease resurgence; immunocompromised and elderly populations would face sharply increased infection risk; public health institutions would lose primary disease control lever; school/workplace exclusion policies would collapse; the legal framework of state police power for public health would be fundamentally weakened. The world would rearrange around voluntary vaccination with predictable coverage gaps.
% FOUNDING_PROBLEM: Infectious disease control in increasingly dense, mobile populations where voluntary vaccination consistently fails to achieve herd immunity thresholds, leaving vulnerable populations exposed to preventable severe harm and death. Historical trigger: smallpox epidemics in urbanizing 19th/early 20th century populations.
% FOUNDING_PROBLEM_CORROBORATION: Public health institutions and epidemiological literature attest the founding problem remains live (COVID-19, measles resurgence in under-vaccinated pockets). Bodily autonomy advocates and some legal scholars attest the founding problem is substantially solved for many mandated diseases (polio eliminated in Americas, measles near-elimination pre-2019) and the constraint persists as institutional inertia. The proportionality_reading sibling corroborates the 'contested' status — legitimacy depends on disease-specific parameters that have changed since founding.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__public_health_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__public_health_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(mandate_legitimacy_scope__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(mandate_legitimacy_scope__public_health_primary, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mandate_legitimacy_scope__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(mandate_legitimacy_scope__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(mandate_legitimacy_scope__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.65: The constraint compels medical intervention on non-consenting adults. This is a profound bodily integrity intrusion. However, ε is not higher because the intervention (vaccination) carries individual health benefit and the coordination function (protecting vulnerable) is real and empirically grounded. Suppression 0.55: Enforcement is active (school exclusion, employment conditions, travel restrictions) but alternatives exist (home schooling, remote work, exemption processes in most jurisdictions). Theater ratio 0.12: Low — the coordination function (disease control) is genuinely operative and measurable; enforcement activity tracks epidemiological goals, not performative compliance. Accessibility collapse 0.45: Moderate — alternatives to vaccination (masking, distancing, isolation) exist but are partial and burdensome; the constraint narrows but does not eliminate exit. Resistance 0.48: Significant but not overwhelming — vaccine hesitancy is organized and persistent, but majority compliance sustains the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from beneficiary/victim declarations and exit options. Immunocompromised and elderly (beneficiaries) have identity_locked/constrained exit — they cannot opt out of their vulnerability, so the constraint subsidizes them (low d). Unvaccinated_by_choice and vaccine_hesitant (victims) have constrained exit — they can technically refuse but face escalating exclusion from public life (high d). Medical_exemption_denied are trapped — no exit, full target (d ≈ 1.0). Public_health_institutions are agenda_setters with arbitrage exit — they administer the constraint and can modify it (d near 0.0). Constitutional_courts are analytical observers (d = 0.5). The reading's structural delta (high ε from mandate absence) means the vulnerable populations' directionality toward the NO-MANDATE counterfactual is high-target; this reading inverts that by making the mandate the protective constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (infectious disease control in dense populations) remains live — COVID-19 demonstrated ongoing vulnerability. However, the constraint shows mandatrophy risk: childhood vaccine mandates persist for diseases with near-zero circulation (polio in Americas, measles in high-coverage regions) where the coordination function has atrophied but the extraction machinery remains. The theater_ratio rise from 0.08 to 0.12 over the interval suggests creeping performative maintenance for eradicated/near-eradicated diseases. The reading does not declare mandatrophy_resolved — the tension between live founding problem and atrophied sub-constraints is unresolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct structural claim (public_health_primary reading) or merely a policy preference within a single mandate_legitimacy_scope kernel?',
    'Compare ε and victim/beneficiary structures across the three declared readings (public_health_primary, bodily_autonomy_primary, proportionality_reading). If each reading instantiates a constraint with different ε, different victims, and different coordination/extraction balance, they are distinct constraints linked by kernel_id.',
    'If distinct constraints: each gets its own classification and the engine tracks them separately. If single constraint: the readings are observer perspectives on one ε, and the model must not author multiple files.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the three declared readings are structurally distinct constraints or observer perspectives on one constraint').

omega_variable(
    coordination_extraction_boundary,
    'Is the coordination function (protecting vulnerable populations from disease) structurally separable from the extraction function (compelling medical intervention on non-consenting adults), or are they inseparable in practice?',
    'Natural experiments from jurisdictions with robust voluntary vaccination + targeted protection for vulnerable groups vs. universal mandates. If vulnerable protection achieves equivalent outcomes without compulsion, the functions are separable and the mandate''s extraction is not necessary for coordination.',
    'If separable: the constraint is a snare (coordination is cover for extraction). If inseparable: the constraint is a genuine tangled_rope where extraction is the price of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether protection of vulnerable populations requires compulsion of the general population or can be achieved through voluntary means + targeted measures').

omega_variable(
    threshold_legitimacy_boundary,
    'At what threshold of disease severity, vaccine efficacy, and vulnerable population risk does the public_health_primary reading''s legitimacy claim activate? Is there a structural threshold or a continuous gradient?',
    'Analyze historical mandate triggers: smallpox (CFR ~30%, sterilizing vaccine), polio (paralysis risk, high efficacy), COVID-19 (variable severity, non-sterilizing vaccines, evolving variants). Test whether legitimacy tracks a discrete threshold or continuous variables.',
    'If discrete threshold: the constraint has a clear structural boundary. If continuous gradient: legitimacy is a moving target, making the constraint''s extraction profile inherently unstable and susceptible to mission creep.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(threshold_legitimacy_boundary, conceptual, 'Whether mandate legitimacy under this reading has a structural activation threshold or a continuous gradient').

omega_variable(
    mandate_absence_extraction,
    'The reading claims high ε from mandate absence (immunocompromised harmed). But is the absence of a mandate itself a constraint with extractive force, or is it the baseline state from which mandates deviate?',
    'Model the counterfactual: in a no-mandate regime, do immunocompromised populations bear costs imposed by others'' choices (externalized risk), or do they bear the natural risk of a pathogen? Distinguish imposed extraction from background risk.',
    'If mandate absence is extractive: both mandate and no-mandate are extractive constraints with different victim sets — a true dilemma. If mandate absence is baseline: only the mandate extracts, and the reading''s ε claim is overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_absence_extraction, conceptual, 'Whether the absence of a vaccination mandate constitutes an extractive constraint on vulnerable populations or the baseline state of nature').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__public_health_primary, 1905, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mandate_legitimacy_scope__public_health_primary_tr_t1905, mandate_legitimacy_scope__public_health_primary, theater_ratio, 1905, 0.08).
narrative_ontology:measurement(mandate_legitimacy_scope__public_health_primary_tr_t1922, mandate_legitimacy_scope__public_health_primary, theater_ratio, 1922, 0.1).
narrative_ontology:measurement(mandate_legitimacy_scope__public_health_primary_tr_t1955, mandate_legitimacy_scope__public_health_primary, theater_ratio, 1955, 0.09).
narrative_ontology:measurement(mandate_legitimacy_scope__public_health_primary_tr_t1977, mandate_legitimacy_scope__public_health_primary, theater_ratio, 1977, 0.1).
narrative_ontology:measurement(mandate_legitimacy_scope__public_health_primary_tr_t2005, mandate_legitimacy_scope__public_health_primary, theater_ratio, 2005, 0.11).
narrative_ontology:measurement(mandate_legitimacy_scope__public_health_primary_tr_t2020, mandate_legitimacy_scope__public_health_primary, theater_ratio, 2020, 0.12).

% Extraction over time
narrative_ontology:measurement(mandate_legitimacy_scope__public_health_primary_be_t1905, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 1905, 0.35).
narrative_ontology:measurement(mandate_legitimacy_scope__public_health_primary_be_t1922, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 1922, 0.42).
narrative_ontology:measurement(mandate_legitimacy_scope__public_health_primary_be_t1955, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 1955, 0.48).
narrative_ontology:measurement(mandate_legitimacy_scope__public_health_primary_be_t1977, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 1977, 0.52).
narrative_ontology:measurement(mandate_legitimacy_scope__public_health_primary_be_t2005, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 2005, 0.55).
narrative_ontology:measurement(mandate_legitimacy_scope__public_health_primary_be_t2020, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 2020, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(mandate_legitimacy_scope__public_health_primary_su_t1905, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 1905, 0.4).
narrative_ontology:measurement(mandate_legitimacy_scope__public_health_primary_su_t1922, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 1922, 0.45).
narrative_ontology:measurement(mandate_legitimacy_scope__public_health_primary_su_t1955, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 1955, 0.48).
narrative_ontology:measurement(mandate_legitimacy_scope__public_health_primary_su_t1977, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 1977, 0.5).
narrative_ontology:measurement(mandate_legitimacy_scope__public_health_primary_su_t2005, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 2005, 0.52).
narrative_ontology:measurement(mandate_legitimacy_scope__public_health_primary_su_t2020, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 2020, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandate_legitimacy_scope__public_health_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(mandate_legitimacy_scope__public_health_primary, 0.12).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__public_health_primary, mandate_legitimacy_scope__bodily_autonomy_primary).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__public_health_primary, mandate_legitimacy_scope__proportionality_reading).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__public_health_primary, school_vaccine_requirements).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__public_health_primary, healthcare_worker_mandates).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__public_health_primary, travel_vaccination_requirements).

% DUAL FORMULATION NOTE:
% This is one of three constraint stories decomposing the mandate_legitimacy_scope kernel. The public_health_primary reading claims mandates are legitimate coordination protecting vulnerable populations (tangled_rope). The bodily_autonomy_primary reading claims mandates are illegitimate extraction violating bodily integrity (snare). The proportionality_reading claims legitimacy is a continuous function of disease/vaccine parameters (scaffold or tangled_rope depending on parameters). All three share the kernel_id but instantiate distinct constraints with different ε, victims, and beneficiaries.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(mandate_legitimacy_scope__public_health_primary, powerless, 0.15).
constraint_indexing:directionality_override(mandate_legitimacy_scope__public_health_primary, moderate, 0.7).
constraint_indexing:directionality_override(mandate_legitimacy_scope__public_health_primary, organized, 0.75).
constraint_indexing:directionality_override(mandate_legitimacy_scope__public_health_primary, institutional, 0.1).
constraint_indexing:directionality_override(mandate_legitimacy_scope__public_health_primary, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
