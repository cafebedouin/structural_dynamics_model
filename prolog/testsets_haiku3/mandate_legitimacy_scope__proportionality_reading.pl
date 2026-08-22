% ============================================================================
% CONSTRAINT STORY: mandate_legitimacy_scope__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mandate_legitimacy_scope__proportionality_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: mandate_legitimacy_scope__proportionality_reading
 *   human_readable: Vaccine Mandate Legitimacy Under Proportionality Test
 *   domain: public_health/constitutional_law/medical_autonomy
 *
 * SUMMARY:
 *   This constraint embodies one reading of the mandate legitimacy kernel: a
 *   proportionality-based framework that says vaccine mandates are legitimate
 *   ONLY when the disease threat is sufficiently severe, the vaccine is
 *   proven safe and effective, and no less restrictive alternatives exist.
 *   Under this reading, mandates for measles are legitimate (high severity,
 *   proven vaccine, no alternative protection); mandates for seasonal
 *   influenza are dubious (moderate severity, moderate efficacy, alternative
 *   disease management exists). The constraint's extractiveness varies by
 *   pathogen — it is conditional, not categorical. The proportionality
 *   reading coexists with two sibling readings: bodily_autonomy_primary
 *   (mandates violate inviolable individual rights regardless of disease
 *   severity) and public_health_primary (state authority is legitimate when
 *   necessary to protect vulnerable populations, with less emphasis on
 *   proportionality thresholds). This story instantiates ONLY the
 *   proportionality reading.
 *
 * KEY AGENTS:
 *   - disease_control_authorities: Set mandates based on proportionality assessment; hold institutional power and mobile exit options
 *   - vulnerable_populations: Depend on herd immunity; powerless and trapped; benefit from mandates for high-severity pathogens
 *   - vaccine_hesitant_individuals: Face exclusion/job loss; constrained exit; targeted by mandate enforcement
 *   - medical_contraindication_holders: Unable to receive vaccine; constrained exit; pay costs despite medical unsuitability
 *   - vaccine_manufacturers: Benefit from guaranteed demand; institutional power; transparency on safety data conditions legitimacy under this reading
 *   - public_health_researchers: Generate disease severity and safety data that determine legitimacy boundaries; independent observer seat
 *   - civil_liberties_advocates: Excluded from legitimacy assessment; hold bodily-autonomy-primary reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__proportionality_reading, 0.52).
domain_priors:suppression_score(mandate_legitimacy_scope__proportionality_reading, 0.68).
domain_priors:theater_ratio(mandate_legitimacy_scope__proportionality_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(mandate_legitimacy_scope__proportionality_reading, "Vaccine Mandate Legitimacy Under Proportionality Test").
narrative_ontology:topic_domain(mandate_legitimacy_scope__proportionality_reading, "public_health/constitutional_law/medical_autonomy").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__proportionality_reading, '2361d20d-54c6-4192-9be3-1596bf59d1fd').
narrative_ontology:cs_kernel_codification('2361d20d-54c6-4192-9be3-1596bf59d1fd', distributed).
narrative_ontology:cs_authority_grounding('2361d20d-54c6-4192-9be3-1596bf59d1fd', distributed).
narrative_ontology:cs_reading_relation('2361d20d-54c6-4192-9be3-1596bf59d1fd', mandate_legitimacy_scope__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('2361d20d-54c6-4192-9be3-1596bf59d1fd', mandate_legitimacy_scope__public_health_primary, coexists_with).
narrative_ontology:cs_axiom('2361d20d-54c6-4192-9be3-1596bf59d1fd', foundational, mandate_legitimacy_is_conditional_on_disease_severity).
narrative_ontology:cs_axiom_status(mandate_legitimacy_is_conditional_on_disease_severity, holdable).
narrative_ontology:cs_axiom_grounding('2361d20d-54c6-4192-9be3-1596bf59d1fd', mandate_legitimacy_is_conditional_on_disease_severity, empirically_contingent).
narrative_ontology:cs_axiom('2361d20d-54c6-4192-9be3-1596bf59d1fd', foundational, least_restrictive_means_doctrine).
narrative_ontology:cs_axiom_status(least_restrictive_means_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('2361d20d-54c6-4192-9be3-1596bf59d1fd', least_restrictive_means_doctrine, deontological).
narrative_ontology:cs_reference_frame('2361d20d-54c6-4192-9be3-1596bf59d1fd', proportionality_balanced_framework).
narrative_ontology:cs_drift_state('2361d20d-54c6-4192-9be3-1596bf59d1fd', contemporary_mandate_scope_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2361d20d-54c6-4192-9be3-1596bf59d1fd', '').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__proportionality_reading, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, vulnerable_populations).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, disease_control_authorities).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__proportionality_reading, vaccine_hesitant_individuals).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__proportionality_reading, medical_contraindication_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, vaccine_manufacturers).
narrative_ontology:constraint_vindicates(mandate_legitimacy_scope__proportionality_reading, proportionality_doctrine).
narrative_ontology:constraint_vindicates(mandate_legitimacy_scope__proportionality_reading, least_restrictive_means_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Public health agencies set vaccination mandates based on epidemiological assessment of disease severity, vaccine safety/efficacy data, and availability of alternatives. They enforce mandates through school/workplace exclusion, medical license suspension, or employment termination. They operate under constitutional constraints requiring proportionality: the mandate must match the actual disease threat and available countermeasures.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, disease_control_authorities, agenda_setter,
    institutional, generational, mobile, national).

% Immunocompromised individuals, infants too young for vaccination, and others unable to mount immune response depend on herd immunity from mandated vaccination of the broader population. For severe pathogens (measles, polio), this dependence is life-or-death. The mandate directly protects them by reducing disease circulation.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, vulnerable_populations, beneficiary,
    powerless, biographical, trapped, national).

% Individuals who refuse vaccination face exclusion from schools, workplaces, or public spaces, or lose professional licensure. Their options are to comply, relocate to jurisdictions without mandates, or litigate. The constraint's legitimacy, under this reading, depends on whether the disease threat and vaccine risk profile justify forcing their compliance.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, vaccine_hesitant_individuals, payer,
    moderate, biographical, constrained, national).

% Individuals with documented medical contraindications (severe allergy, myocarditis history, etc.) may not be able to receive the vaccine. Under proportionality framing, they should receive exemptions, but enforcement sometimes denies exemptions or applies them inconsistently, forcing compliance despite medical unsuitability.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, medical_contraindication_holders, payer,
    moderate, biographical, constrained, national).

% Mandates guarantee stable, high-volume demand for vaccines. Under proportionality framing, manufacturers' liability is contingent on transparency and ongoing safety monitoring; if safety data deteriorates or alternatives emerge, the mandate's legitimacy shifts.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, vaccine_manufacturers, beneficiary,
    institutional, generational, mobile, global).

% Epidemiologists and vaccine safety researchers generate the disease severity, vaccine efficacy, and safety data on which proportionality determinations rest. They are independent observers whose findings can shift the legitimacy boundary — a new safety signal or altered disease epidemiology changes what mandates pass the proportionality test.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, public_health_researchers, observer,
    organized, generational, analytical, global).

% Argue that mandates, even when proportional by severity/safety metrics, still violate bodily autonomy and should never be imposed. They are not at the table when proportionality thresholds are set; their voice (bodily autonomy primary reading) is structurally excluded from legitimacy assessment under this reading.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, civil_liberties_advocates, excluded,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(mandate_legitimacy_scope__proportionality_reading, disease_control_authorities).
narrative_ontology:fixing_cost_class(mandate_legitimacy_scope__proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a collective action problem: unvaccinated individuals benefit from herd immunity they do not provide; without mandate, vaccination rate falls below the threshold needed to protect vulnerable populations from severe disease. The mandate coordinates vaccination coverage at the level required for vulnerable-population protection.
% TRANSFER_FUNCTION: Moves bodily autonomy from vaccine-hesitant and medically-unsuitable individuals to vulnerable populations (who gain herd immunity protection) and to disease control authorities (who gain enforcement legitimacy and budget). Individuals bearing the mandate bear a real cost: forced medical intervention, exclusion from institutions, or relocation.
% ABSENT_VOICES: Bodily-autonomy-primary advocates (see constraint_mandate_legitimacy_scope__bodily_autonomy_primary) would argue mandates violate inviolable individual rights; they are excluded from legitimacy assessment under this reading because this reading prioritizes proportionality to disease threat, not inviolability of autonomy. Indigenous and historically-harmed communities with documented vaccine mistrust are often structurally excluded from mandate-setting tables.
% DISAPPEARANCE_RATIONALE: If proportionality-based mandate legitimacy dissolved overnight, disease control authorities would lose the legal/institutional framework for enforcement; vaccination rates would drop for low-severity pathogens (flu, RSV) where hesitancy is common and severity is mild; vulnerable populations would face increased disease risk for high-severity pathogens where mandates had driven herd immunity. The institutional arrangement would reorganize around voluntary vaccination campaigns and targeted protection for vulnerable groups.
% FOUNDING_PROBLEM: Vaccine-preventable disease outbreaks in the 1980s-1990s (measles, whooping cough) revealed that voluntary uptake failed to reach herd immunity thresholds, leaving vulnerable populations (infants, immunocompromised) at unacceptable risk from highly lethal pathogens.
% FOUNDING_PROBLEM_CORROBORATION: Public health agencies and disease epidemiologists attest the founding problem remains live for measles, polio, pertussis (high-severity, highly-transmissible). Epidemiological data support this — measles outbreak risk correlates precisely with vaccination rate dropping below herd immunity threshold. Medical autonomy advocates contest the framing: they attest the founding problem is valid but claim mandates are an illegitimate solution and argue transparency + public education can substitute. Independent vaccine safety researchers (outside benefiting parties) attest that for low-severity pathogens (seasonal flu, RSV) the founding problem is substantially attenuated and mandate legitimacy is questionable.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__proportionality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(mandate_legitimacy_scope__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(mandate_legitimacy_scope__proportionality_reading, 0.52, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mandate_legitimacy_scope__proportionality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(mandate_legitimacy_scope__proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(mandate_legitimacy_scope__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score (0.52) reflects that mandates coordinate real protection (vulnerable populations) but also impose real bodily autonomy costs (hesitant individuals face forced intervention or exclusion). Under proportionality framing, extractiveness is conditional on pathogen severity: high for measles (high severity justifies high extraction), moderate to low for flu (lower severity, weaker justification). The temporal arc shows extraction rising from 0.35 (measles-only era) to 0.54 (COVID expansion when mandates applied to moderate-severity respiratory pathogens) then contracting to 0.52 as proportionality constraints reasserted post-COVID. Suppression is high (0.68) because mandate enforcement requires closing exemption pathways and preventing institutional workarounds — hesitant individuals have nowhere to go. Theater ratio (0.41) indicates that while authentic disease control function exists, a growing share of enforcement activity (especially post-COVID) appears to be rehearsal of authority rather than response to disease threat; the high theater_t30 spike during COVID reflects the decoupling of mandate scope from underlying epidemiology. Accessibility_collapse measures how completely individuals can opt out: t0 was moderate (philosophical exemptions existed), t50 is higher (exemptions tightened, alternative institutions fewer). Resistance is high (0.71) because vaccine hesitancy is real, organized, and growing — the constraint meets substantial push-back. The coercion grid shows suppression intensity increases at every level from t0 to t50, but particularly at organizational (vaccine mandates at workplaces) and individual levels (professional licensure suspension).
 *
 * PERSPECTIVAL GAP:
 *   From the disease_control_authorities seat, this constraint is legitimate coordination: it solves a public health coordination problem and saves lives. The metrics should reflect genuine collective action. From the vaccine_hesitant seat, the same constraint is experienced as coercive extraction: they are forced to accept a medical intervention against their will, excluded from employment, and have no viable alternatives. The engine should compute this as a Tangled Rope at the beneficiary seat (coordination dominates) and as a Snare or extractive Tangled Rope at the payer seats (extraction dominates). The proportionality reading itself is the mechanism that BRIDGES this gap: by conditioning legitimacy on disease severity, vaccine efficacy, and availability of alternatives, the reading allows the constraint to be legitimate for high-severity pathogens and illegitimate for low-severity ones — the type classification shifts with the pathogen, not with the observer seat. This is NOT perspectivism; it is conditional legitimacy.
 *
 * DIRECTIONALITY LOGIC:
 *   Vulnerable populations receive high beneficiary directionality (d near 0.0) because the constraint protects them directly and they are trapped by lack of exit — herd immunity is the only protection available to them. Disease control authorities receive moderate beneficiary directionality (d ~0.2-0.3) because they gain institutional legitimacy and enforcement authority, but they are not the primary beneficiaries; they also bear compliance costs (litigation, institutional resistance) and operate under constitutional constraints. Vaccine hesitant individuals and contraindication holders receive high target directionality (d near 1.0) because they bear the full cost of forced compliance: they must accept medical intervention, face exclusion, or relocate. Manufacturers receive moderate beneficiary directionality (d ~0.2) because mandates guarantee demand, but under proportionality framing their legitimacy is contingent on ongoing safety monitoring and transparency — if safety data deteriorates, they become targets rather than beneficiaries. Public health researchers receive analytical directionality (d ~0.5) because their role is to generate the data that determines what mandates are proportional; they are neither extracted from nor benefited by the mandate itself, but their findings reshape the constraint's legitimacy boundary.
 *
 * MANDATROPHY ANALYSIS:
 *   The proportionality reading prevents mandatrophy by explicitly conditioning legitimacy on disease parameters that can become obsolete. A mandate for a pathogen that no longer circulates, or for which safer alternatives have emerged, fails the proportionality test and loses legitimacy. The reading's weakness is susceptibility to scope-creep: disease control authorities can redraw the 'severity threshold' or 'efficacy benchmark' to justify mandates that no longer meet the original proportionality standard (witnessed in COVID-era mandates for mild-severity Omicron variants using pre-Omicron efficacy data). The theater_ratio spike at t30 (COVID era) captures this drift: enforcement activity continued for pathogens that no longer met the proportionality threshold. The counter-mandatrophy mechanism is litigation and legislative override: when mandates exceed proportionality, courts can strike them down (witnessed in Florida's elimination of vaccine mandates for routine childhood vaccines in 2023, overriding the public health agencies' earlier assessment). The constraint remains tangled_rope if proportionality is genuinely applied; it becomes snare if authorities apply it inconsistently or dishonestly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    severity_threshold_ambiguity,
    'What disease severity threshold makes a mandate legitimate under proportionality logic? Is it quantified by mortality rate, hospitalization rate, or subjective assessment of social impact?',
    'Comparative analysis of mandate policies across jurisdictions: which pathogens are mandated and which are not, and what epidemiological metrics were cited in each decision. Documentary analysis of legislative findings and judicial opinions setting severity benchmarks.',
    'If severity is quantified (e.g., >1% mortality → legitimate), the proportionality test is operationalizable and its application can be audited. If severity is qualitative, the test becomes vulnerable to authority drift and scope-creep; authorities can redefine ''severe'' to expand mandate scope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(severity_threshold_ambiguity, empirical, 'Whether the proportionality test''s severity component is quantifiable or subject to reinterpretation.').

omega_variable(
    reading_foreclosure_ambiguity,
    'Do the proportionality reading and bodily_autonomy_primary reading logically foreclose each other, or do they coexist as competing legitimate frameworks?',
    'Examination of constitutional jurisprudence and political philosophy: can a single legal authority (court, legislature) simultaneously hold both that mandates are legitimate when proportional AND that bodily autonomy is inviolable? Or does adopting one reading require rejecting the other?',
    'If they foreclose each other (logically incompatible), the constraint story contains a kernel_contradiction that can be resolved only by courts choosing one reading over the other. If they coexist, the contest is ongoing, and both readings'' constraints should be modeled as live options across different jurisdictions and eras.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_ambiguity, conceptual, 'Whether proportionality and bodily autonomy readings are logically exclusive or coexistable.').

omega_variable(
    suppression_internalization_mechanism,
    'Is the high suppression (0.68) score driven by external enforcement (exclusion from institutions, job loss) or by internalization (vaccine hesitant individuals come to believe mandates are legitimate or unavoidable)?',
    'Post-mandate empirical study: if mandate enforcement is removed, do vaccine hesitant individuals continue to vaccinate, or does vaccination rate drop to pre-mandate levels? If drop = externally suppressed; if plateau above baseline = partially internalized.',
    'If suppression is purely external, removing the mandate removes the suppression. If suppression is internalized, the constraint persists even after formal mandate ends — hesitant individuals remain unable or unwilling to vaccinate. Internalized suppression suggests the constraint''s true extractiveness is higher than the scalar suggests, because the target carries the suppression beyond formal coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_mechanism, empirical, 'Whether mandate compliance is driven by external enforcement or internalized belief.').

omega_variable(
    contraindication_exemption_scope,
    'Are medical contraindications genuinely recognized in mandate enforcement, or are they narrowed or denied in practice?',
    'Audit of exemption grants across jurisdictions: what fraction of medical contraindication claims are approved vs. denied? Are approval rates correlated with political pressure to maximize mandate compliance?',
    'If exemptions are broadly recognized, medical_contraindication_holders remain a small payer class; if exemptions are narrowly denied, this class expands and the constraint becomes more extractive for a wider population with actual medical unsuitability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contraindication_exemption_scope, empirical, 'Whether medical exemptions are operationally accessible or systematically denied.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint truly a reading of the mandate_legitimacy_scope kernel, or is it instantiating a different (unshared) normative commitment?',
    'Clarification of whether the proportionality reading is held by actual political/legal authorities as a legitimate stance, or whether it is an analytical reconstruction that no real actor endorses. If the former, it is a sibling reading; if the latter, it is an external normative frame imposed by the analyst.',
    'If proportionality is a real reading held by courts, legislatures, or public health agencies, the constraint family models an actual contest among live positions. If it is only an analytical frame, the constraint becomes a normative proposal rather than a descriptive account of contested legitimacy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether proportionality is a reading of a shared kernel or an external normative frame.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__proportionality_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_t0_measles, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(theater_t0_measles, observed).
narrative_ontology:measurement(theater_t10_justification_required, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement_basis(theater_t10_justification_required, observed).
narrative_ontology:measurement(theater_t20_mandate_generalization, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement_basis(theater_t20_mandate_generalization, observed).
narrative_ontology:measurement(theater_t30_covid_enforcement_ritual, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement_basis(theater_t30_covid_enforcement_ritual, observed).
narrative_ontology:measurement(theater_t40_proportionality_reemphasis, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(theater_t40_proportionality_reemphasis, observed).
narrative_ontology:measurement(theater_t50_current, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 50, 0.41).
narrative_ontology:measurement_basis(theater_t50_current, observed).

% Extraction over time
narrative_ontology:measurement(extract_t0_measles_mandate, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(extract_t0_measles_mandate, observed).
narrative_ontology:measurement(extract_t10_mandate_expansion_flu, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement_basis(extract_t10_mandate_expansion_flu, observed).
narrative_ontology:measurement(extract_t20_mandate_proliferation, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement_basis(extract_t20_mandate_proliferation, observed).
narrative_ontology:measurement(extract_t30_covid_mandate_scope, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 30, 0.54).
narrative_ontology:measurement_basis(extract_t30_covid_mandate_scope, observed).
narrative_ontology:measurement(extract_t40_post_covid_contraction, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 40, 0.51).
narrative_ontology:measurement_basis(extract_t40_post_covid_contraction, observed).
narrative_ontology:measurement(extract_t50_current, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 50, 0.52).
narrative_ontology:measurement_basis(extract_t50_current, observed).

% Suppression requirement over time
narrative_ontology:measurement(suppress_t0_exemption_acceptance, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(suppress_t0_exemption_acceptance, observed).
narrative_ontology:measurement(suppress_t10_exemption_tightening, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement_basis(suppress_t10_exemption_tightening, observed).
narrative_ontology:measurement(suppress_t20_mandate_scope_broadening, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement_basis(suppress_t20_mandate_scope_broadening, observed).
narrative_ontology:measurement(suppress_t30_covid_enforcement_hardening, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 30, 0.78).
narrative_ontology:measurement_basis(suppress_t30_covid_enforcement_hardening, observed).
narrative_ontology:measurement(suppress_t40_litigation_backlash, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 40, 0.69).
narrative_ontology:measurement_basis(suppress_t40_litigation_backlash, observed).
narrative_ontology:measurement(suppress_t50_current, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 50, 0.68).
narrative_ontology:measurement_basis(suppress_t50_current, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=50
narrative_ontology:measurement(mand_grid_01, mandate_legitimacy_scope__proportionality_reading, accessibility_collapse(class), 0, 0.58).
narrative_ontology:measurement(mand_grid_02, mandate_legitimacy_scope__proportionality_reading, accessibility_collapse(class), 50, 0.71).
narrative_ontology:measurement(mand_grid_03, mandate_legitimacy_scope__proportionality_reading, accessibility_collapse(individual), 0, 0.61).
narrative_ontology:measurement(mand_grid_04, mandate_legitimacy_scope__proportionality_reading, accessibility_collapse(individual), 50, 0.62).
narrative_ontology:measurement(mand_grid_05, mandate_legitimacy_scope__proportionality_reading, accessibility_collapse(organizational), 0, 0.48).
narrative_ontology:measurement(mand_grid_06, mandate_legitimacy_scope__proportionality_reading, accessibility_collapse(organizational), 50, 0.64).
narrative_ontology:measurement(mand_grid_07, mandate_legitimacy_scope__proportionality_reading, accessibility_collapse(structural), 0, 0.55).
narrative_ontology:measurement(mand_grid_08, mandate_legitimacy_scope__proportionality_reading, accessibility_collapse(structural), 50, 0.68).
narrative_ontology:measurement(mand_grid_09, mandate_legitimacy_scope__proportionality_reading, resistance(class), 0, 0.62).
narrative_ontology:measurement(mand_grid_10, mandate_legitimacy_scope__proportionality_reading, resistance(class), 50, 0.73).
narrative_ontology:measurement(mand_grid_11, mandate_legitimacy_scope__proportionality_reading, resistance(individual), 0, 0.71).
narrative_ontology:measurement(mand_grid_12, mandate_legitimacy_scope__proportionality_reading, resistance(individual), 50, 0.7).
narrative_ontology:measurement(mand_grid_13, mandate_legitimacy_scope__proportionality_reading, resistance(organizational), 0, 0.52).
narrative_ontology:measurement(mand_grid_14, mandate_legitimacy_scope__proportionality_reading, resistance(organizational), 50, 0.71).
narrative_ontology:measurement(mand_grid_15, mandate_legitimacy_scope__proportionality_reading, resistance(structural), 0, 0.48).
narrative_ontology:measurement(mand_grid_16, mandate_legitimacy_scope__proportionality_reading, resistance(structural), 50, 0.68).
narrative_ontology:measurement(mand_grid_17, mandate_legitimacy_scope__proportionality_reading, stakes_inflation(class), 0, 0.45).
narrative_ontology:measurement(mand_grid_18, mandate_legitimacy_scope__proportionality_reading, stakes_inflation(class), 50, 0.64).
narrative_ontology:measurement(mand_grid_19, mandate_legitimacy_scope__proportionality_reading, stakes_inflation(individual), 0, 0.51).
narrative_ontology:measurement(mand_grid_20, mandate_legitimacy_scope__proportionality_reading, stakes_inflation(individual), 50, 0.55).
narrative_ontology:measurement(mand_grid_21, mandate_legitimacy_scope__proportionality_reading, stakes_inflation(organizational), 0, 0.38).
narrative_ontology:measurement(mand_grid_22, mandate_legitimacy_scope__proportionality_reading, stakes_inflation(organizational), 50, 0.62).
narrative_ontology:measurement(mand_grid_23, mandate_legitimacy_scope__proportionality_reading, stakes_inflation(structural), 0, 0.42).
narrative_ontology:measurement(mand_grid_24, mandate_legitimacy_scope__proportionality_reading, stakes_inflation(structural), 50, 0.58).
narrative_ontology:measurement(mand_grid_25, mandate_legitimacy_scope__proportionality_reading, suppression(class), 0, 0.48).
narrative_ontology:measurement(mand_grid_26, mandate_legitimacy_scope__proportionality_reading, suppression(class), 50, 0.71).
narrative_ontology:measurement(mand_grid_27, mandate_legitimacy_scope__proportionality_reading, suppression(individual), 0, 0.52).
narrative_ontology:measurement(mand_grid_28, mandate_legitimacy_scope__proportionality_reading, suppression(individual), 50, 0.72).
narrative_ontology:measurement(mand_grid_29, mandate_legitimacy_scope__proportionality_reading, suppression(organizational), 0, 0.42).
narrative_ontology:measurement(mand_grid_30, mandate_legitimacy_scope__proportionality_reading, suppression(organizational), 50, 0.68).
narrative_ontology:measurement(mand_grid_31, mandate_legitimacy_scope__proportionality_reading, suppression(structural), 0, 0.38).
narrative_ontology:measurement(mand_grid_32, mandate_legitimacy_scope__proportionality_reading, suppression(structural), 50, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandate_legitimacy_scope__proportionality_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(mandate_legitimacy_scope__proportionality_reading, 0.18).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__proportionality_reading, mandate_legitimacy_scope__bodily_autonomy_primary).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__proportionality_reading, mandate_legitimacy_scope__public_health_primary).

% DUAL FORMULATION NOTE:
% The mandate_legitimacy_scope kernel decomposes into three constraint stories, each instantiating a different reading of the same legitimacy question. The proportionality_reading (this story) conditions legitimacy on disease severity, vaccine safety, and available alternatives — making mandate legitimacy pathogen-dependent. The bodily_autonomy_primary reading (sibling constraint) rejects ALL mandates as violations of inviolable bodily integrity. The public_health_primary reading (sibling constraint) licenses mandates when necessary to protect vulnerable populations, with weaker emphasis on proportionality thresholds. These three readings coexist across different jurisdictions, political traditions, and legal frameworks. Each reading has its own ε, its own beneficiary/victim structure, and its own classification. They are linked via network.affects_constraints to enable the analysis system to track how doctrine disagreement produces different constraint types from the same underlying institutional arrangement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(mandate_legitimacy_scope__proportionality_reading, powerless, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
