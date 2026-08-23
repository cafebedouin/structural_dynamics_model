% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coercion_legitimacy_boundary__public_health_primary, []).

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
 *   constraint_id: coercion_legitimacy_boundary__public_health_primary
 *   human_readable: Public Health Primacy: State Compulsion for Collective Harm Prevention
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint story instantiates the public_health_primary reading of
 *   the coercion_legitimacy_boundary kernel. The reading holds that state
 *   authority to compel medical intervention is triggered when collective
 *   harm-prevention outweighs individual autonomy — a utilitarian balancing
 *   test rooted in Jacobson v. Massachusetts (1905) and elaborated through
 *   modern public health law. The standing arrangement under contest is the
 *   regime of vaccine mandates, quarantine powers, and compulsory treatment
 *   orders enforced by state public health authorities. The reading's ε
 *   (0.75) reflects the extraction inherent in the enforcement apparatus:
 *   fines, exclusion from public life, loss of employment, and in extreme
 *   cases physical compulsion. Beneficiaries are the immunocompromised (who
 *   cannot vaccinate and rely on herd immunity), the general public
 *   (protected from epidemic spread), and the public health infrastructure
 *   (which gains operational authority). Victims are unvaccinated individuals
 *   coerced into medical procedures and autonomy claimants whose bodily
 *   integrity is overridden. The constraint requires active enforcement —
 *   mandates do not self-execute. The claimed type is tangled_rope: genuine
 *   coordination function (disease prevention) combined with asymmetric
 *   extraction (coercion of non-consenting individuals).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__public_health_primary, 0.75).
domain_priors:suppression_score(coercion_legitimacy_boundary__public_health_primary, 0.78).
domain_priors:theater_ratio(coercion_legitimacy_boundary__public_health_primary, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, extractiveness, 0.75).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__public_health_primary, tangled_rope).
narrative_ontology:human_readable(coercion_legitimacy_boundary__public_health_primary, "Public Health Primacy: State Compulsion for Collective Harm Prevention").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__public_health_primary, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(coercion_legitimacy_boundary__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__public_health_primary, 'c67fff32-cd5d-4309-88a0-27b557ead1de').
narrative_ontology:cs_kernel_codification('c67fff32-cd5d-4309-88a0-27b557ead1de', formalized).
narrative_ontology:cs_authority_grounding('c67fff32-cd5d-4309-88a0-27b557ead1de', lineage).
narrative_ontology:cs_interpretation_layer_present('c67fff32-cd5d-4309-88a0-27b557ead1de').
narrative_ontology:cs_reading_relation('c67fff32-cd5d-4309-88a0-27b557ead1de', coercion_legitimacy_boundary__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('c67fff32-cd5d-4309-88a0-27b557ead1de', coercion_legitimacy_boundary__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('c67fff32-cd5d-4309-88a0-27b557ead1de', foundational, collective_harm_prevention_justifies_coercion).
narrative_ontology:cs_axiom_status(collective_harm_prevention_justifies_coercion, holdable).
narrative_ontology:cs_axiom_grounding('c67fff32-cd5d-4309-88a0-27b557ead1de', collective_harm_prevention_justifies_coercion, empirically_contingent).
narrative_ontology:cs_axiom('c67fff32-cd5d-4309-88a0-27b557ead1de', secondary, individual_autonomy_not_absolute_in_public_health).
narrative_ontology:cs_axiom_status(individual_autonomy_not_absolute_in_public_health, holdable).
narrative_ontology:cs_axiom_grounding('c67fff32-cd5d-4309-88a0-27b557ead1de', individual_autonomy_not_absolute_in_public_health, conventional).
narrative_ontology:cs_reference_frame('c67fff32-cd5d-4309-88a0-27b557ead1de', classical_police_power_authority).
narrative_ontology:cs_drift_state('c67fff32-cd5d-4309-88a0-27b557ead1de', contemporary_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c67fff32-cd5d-4309-88a0-27b557ead1de', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__public_health_primary, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, immunocompromised_individuals).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, general_public).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, public_health_infrastructure).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__public_health_primary, unvaccinated_individuals).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__public_health_primary, autonomy_claimants).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__public_health_primary, herd_immunity_threshold_doctrine).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__public_health_primary, police_power_public_health_exception).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__public_health_primary, collective_welfare_over_individual_liberty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs, administers, and enforces medical compulsion policies (vaccine mandates, quarantine orders, compulsory treatment). Justifies authority through police power doctrine and epidemiological modeling. Controls the enforcement apparatus and collects compliance revenue (fines) and political capital. Can shift policy frameworks across administrations; exit from the role is rotational, not structural.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, state_public_health_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Cannot receive vaccines due to medical contraindications; rely entirely on population-level herd immunity for protection. The constraint's operation reduces their infection risk but transfers no direct resources to them. They have no exit from biological vulnerability and no alternative protection mechanism. Their situation is structurally fixed by biology, not policy choice.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, immunocompromised_individuals, beneficiary,
    powerless, biographical, trapped, national).

% Receives population-level protection from epidemic spread through herd immunity. Bears diffuse costs: tax funding for public health infrastructure, spillover restrictions during outbreaks, and indirect economic impacts of mandates. Exit is constrained — they cannot individually opt out of the disease ecology but can politically contest specific mandates.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, general_public, beneficiary,
    organized, biographical, constrained, national).

% Subject to medical compulsion (vaccine mandates, exclusion from schools/workplaces/public spaces, fines). For many, vaccine refusal is fused with political/religious identity — exit requires identity rupture, not just behavior change. They bear the direct coercive burden of the constraint. Their alternatives (medical exemptions, religious exemptions) are narrowing over the interval.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, unvaccinated_individuals, payer,
    moderate, biographical, identity_locked, national).

% Advocacy organizations and legal actors defending bodily autonomy as a categorical right. Bear litigation costs, political organizing costs, and reputational costs from being framed as anti-science. Exit is constrained — they cannot abandon the constitutional claim without dissolving their organizational purpose. They are payers because they invest resources to resist the constraint's expansion.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, autonomy_claimants, payer,
    organized, generational, constrained, national).

% Adjudicate challenges to medical compulsion under constitutional frameworks (substantive due process, free exercise, equal protection). They do not directly bear coercion or collect benefits but their rulings set the enforcement boundary. Their analytical seat has full exit — they can distinguish, narrow, or expand the doctrine without personal cost.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, constitutional_courts, observer,
    institutional, generational, analytical, national).

% Subject to parental consent regimes for medical intervention; cannot independently consent or refuse. School vaccine mandates compel them indirectly through parents. They would object to both compulsion and non-protection but have no structural voice. Their situation is mediated by parents and state — they are doubly excluded from the coercion_legitimacy conversation.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, children_minors, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(coercion_legitimacy_boundary__public_health_primary, state_public_health_authority).
narrative_ontology:fixing_cost_class(coercion_legitimacy_boundary__public_health_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Achieves population-level disease prevention (herd immunity, outbreak containment) that voluntary compliance cannot reliably produce, protecting vulnerable populations who cannot self-protect.
% TRANSFER_FUNCTION: Moves bodily autonomy and medical decision-making authority from individuals to state public health authorities; moves infection risk from immunocompromised/vulnerable populations to unvaccinated individuals who bear the compulsion; moves enforcement costs (monitoring, compliance, litigation) to taxpayers.
% ABSENT_VOICES: Children/minors (subject to mandates without consent capacity), future generations (bear long-term precedent effects), individuals with medical contraindications who are not immunocompromised (e.g., allergy, pregnancy — they fall between exemption categories), global south populations (bear vaccine inequity exacerbated by mandate-driven demand).
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight, vaccine mandates and quarantine powers would be legally unenforceable. Herd immunity would collapse for diseases requiring >90% coverage (measles), causing epidemics. Immunocompromised individuals would lose population-level protection. Public health infrastructure would lose operational authority. The disease ecology and legal framework would reorganize around voluntary compliance only.
% FOUNDING_PROBLEM: Historical epidemics (smallpox, yellow fever, 1918 influenza) where voluntary compliance failed to achieve herd immunity, causing mass mortality that state compulsion could have prevented. Jacobson v. Massachusetts (1905) codified the police power exception to liberty for smallpox vaccination.
% FOUNDING_PROBLEM_CORROBORATION: Epidemiological historical record corroborates that voluntary compliance alone failed for smallpox and other high-R0 diseases. Autonomy advocates and some legal scholars contest current necessity, arguing modern medicine (antivirals, targeted protection, lower IFR pathogens) has solved the founding problem. Legislative testimony and amicus briefs from non-benefiting parties (civil liberties orgs, religious liberty groups) support the shifted-function reading.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__public_health_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__public_health_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(coercion_legitimacy_boundary__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(coercion_legitimacy_boundary__public_health_primary, 0.75, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coercion_legitimacy_boundary__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(coercion_legitimacy_boundary__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(coercion_legitimacy_boundary__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.75) because the constraint's operation transfers bodily autonomy and bears significant compliance costs on non-consenting individuals. Suppression is high (0.78) because alternatives (voluntary compliance, targeted protection) are structurally suppressed once the harm-prevention threshold is declared met — the state does not offer opt-outs. Theater ratio is moderate (0.38): public health messaging performs care while enforcement machinery operates; the gap widened during COVID-19 when mandates extended beyond the epidemiological justification. Accessibility collapse (0.68) reflects that once a mandate is issued, alternatives (exemptions, alternatives) are narrowly construed. Resistance (0.55) is substantial but channeled through legal challenges rather than mass non-compliance. The measurement grid spans 1905 (Jacobson) to 2024, showing extraction accumulation as enforcement capacity expanded and theater increased during pandemic response.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences the constraint as coordination infrastructure it built and maintains (low effective extraction). The payer seats (unvaccinated, autonomy_claimants) experience it as enforced extraction with suppressed exits (high effective extraction). The beneficiary seats (immunocompromised, general_public) experience it as net subsidy (negative effective extraction). The engine computes this divergence from the structural data; the authored claim (tangled_rope) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The state_public_health_authority (agenda_setter, institutional power, generational horizon, arbitrage exit) sits at d≈0.15 — it designs and benefits from the enforcement apparatus. Immunocompromised_individuals (beneficiary, powerless/moderate power, biographical horizon, trapped exit) sit at d≈0.10 — they receive protection but have no exit from vulnerability. General_public (beneficiary, organized power, biographical horizon, constrained exit) sits at d≈0.30 — they gain population-level protection but bear diffuse costs (tax funding, restriction spillovers). Unvaccinated_individuals (payer, moderate/powerless power, biographical horizon, identity_locked/constrained exit) sit at d≈0.85 — they bear the direct coercion, and for many, vaccine refusal is identity-fused (identity_locked). Autonomy_claimants (payer, organized power, generational horizon, constrained exit) sit at d≈0.70 — they bear legal and political costs of defending the autonomy boundary. Constitutional_courts (observer, institutional power, generational horizon, analytical exit) sit at d≈0.50 — they adjudicate but do not directly bear or collect.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (epidemic control when voluntary compliance fails) remains live for novel pathogens but is contested for endemic diseases. The constraint shows mandatrophy signals: theater_ratio rising from 0.10 to 0.38 suggests performative maintenance; extraction accumulated from 0.45 to 0.75 suggests rent-seeking layered onto coordination. However, the coordination function (herd immunity) remains epidemiologically real — this is not a pure piton. The tangled_rope classification captures the hybrid: the coordination function is genuine but the enforcement apparatus has extracted beyond the coordination floor.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary_ambiguity,
    'Does the public_health_primary reading foreclose the bodily_autonomy_primary reading within a single legal framework, or do they coexist as competing constitutional interpretations?',
    'Constitutional jurisprudence analysis: if a court adopts a categorical bodily autonomy right, public_health_primary is logically foreclosed; if courts apply balancing tests, both readings coexist in different doctrinal niches.',
    'If forecloses, the kernel has a structural fault line where only one reading can be institutionally authoritative at a time. If coexists_with, the kernel sustains permanent doctrinal contestation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary_ambiguity, conceptual, 'Whether the kernel''s readings are mutually exclusive or simultaneously holdable').

omega_variable(
    extraction_referent_ambiguity,
    'Is the high ε (0.75) authored for the standing arrangement of specific mandates (vaccine mandates, quarantine orders) or for the abstract doctrine itself?',
    'Decompose the constraint: author separate stories for the abstract doctrine (low ε, mountain-like) and for specific enforcement regimes (high ε, tangled_rope/snare). The ε-invariance principle requires this decomposition if the referent shifts.',
    'If ε refers to the doctrine, the reading may be a false summit mountain. If ε refers to enforcement regimes, the high ε is honest but the constraint_id should index the regime, not the principle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_referent_ambiguity, conceptual, 'Whether ε''s referent is the abstract principle or the concrete enforcement apparatus').

omega_variable(
    beneficiary_vs_vindicated_proposition,
    'Are immunocompromised_individuals genuine beneficiaries who collect rents from the constraint, or is ''protection of the vulnerable'' a vindicated proposition that collects no rents?',
    'Trace material flows: do immunocompromised individuals receive transfers (resources, priority access, financial compensation) from the constraint''s operation, or does the constraint merely reduce their risk without transferring value to them?',
    'If they are vindicated propositions not beneficiaries, the coordination function lacks a beneficiary seat — weakening the tangled_rope classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_vs_vindicated_proposition, empirical, 'Whether vulnerable populations are material beneficiaries or rhetorical vindications').

omega_variable(
    proportionality_calibration_boundary,
    'Where does the public_health_primary reading''s ''collective harm outweighs autonomy'' threshold structurally diverge from the proportionality_reading''s severity-scaled calibration?',
    'Identify the disease parameters (R0, IFR, transmission mode) at which public_health_primary would authorize compulsion but proportionality_reading would not. If no such parameter space exists, the readings are not structurally distinct.',
    'If the readings occupy identical parameter space, they are the same constraint with different labels — violating ε-invariance. If distinct, each needs its own ε and victim set.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(proportionality_calibration_boundary, conceptual, 'Whether the two readings are empirically distinguishable in their authorization conditions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__public_health_primary, 1905, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clb_php_tr_t1905, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 1905, 0.1).
narrative_ontology:measurement(clb_php_tr_t1950, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 1950, 0.15).
narrative_ontology:measurement(clb_php_tr_t1976, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 1976, 0.22).
narrative_ontology:measurement(clb_php_tr_t2000, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 2000, 0.28).
narrative_ontology:measurement(clb_php_tr_t2020, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 2020, 0.42).
narrative_ontology:measurement(clb_php_tr_t2024, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(clb_php_be_t1905, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 1905, 0.45).
narrative_ontology:measurement(clb_php_be_t1950, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 1950, 0.55).
narrative_ontology:measurement(clb_php_be_t1976, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 1976, 0.62).
narrative_ontology:measurement(clb_php_be_t2000, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(clb_php_be_t2020, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 2020, 0.78).
narrative_ontology:measurement(clb_php_be_t2024, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 2024, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(clb_php_su_t1905, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 1905, 0.55).
narrative_ontology:measurement(clb_php_su_t1950, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 1950, 0.6).
narrative_ontology:measurement(clb_php_su_t1976, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 1976, 0.68).
narrative_ontology:measurement(clb_php_su_t2000, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 2000, 0.72).
narrative_ontology:measurement(clb_php_su_t2020, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 2020, 0.85).
narrative_ontology:measurement(clb_php_su_t2024, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__public_health_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(coercion_legitimacy_boundary__public_health_primary, 0.12).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__public_health_primary, vaccine_mandate_enforcement_regime).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__public_health_primary, quarantine_authority_statute).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__public_health_primary, public_health_emergency_powers_act).

% DUAL FORMULATION NOTE:
% This constraint (public_health_primary reading) decomposes the coercion_legitimacy_boundary kernel alongside bodily_autonomy_primary and proportionality_reading. The public_health_primary reading has higher ε (enforcement apparatus extraction) and names unvaccinated_individuals as victims. The bodily_autonomy_primary reading would have near-zero ε (categorical prohibition = no enforcement) but would name immunocompromised as victims of non-protection. The proportionality_reading would have variable ε keyed to disease parameters.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(coercion_legitimacy_boundary__public_health_primary, moderate, 0.85).
constraint_indexing:directionality_override(coercion_legitimacy_boundary__public_health_primary, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
