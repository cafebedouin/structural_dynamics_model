% ============================================================================
% CONSTRAINT STORY: public_health_mandate_authority__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_health_mandate_authority__public_health_primary, []).

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
 *   constraint_id: public_health_mandate_authority__public_health_primary
 *   human_readable: Public Health Mandate Authority — Vulnerable Commons Protection Reading
 *   domain: public_health_law/constitutional_rights/bioethics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'public_health_primary' reading of
 *   the contested kernel 'public_health_mandate_authority.' The reading
 *   frames mandates as an obligation to protect the vulnerable commons
 *   (immunocompromised population, healthcare infrastructure) through
 *   collective action. Immunocompromised individuals enter the victim set
 *   when mandates fail; unvaccinated/mandate-resistant individuals are
 *   excluded from the victim set and framed as free-riders imposing
 *   externalities. High extractiveness falls on mandate-resistant individuals
 *   through employment and service coercion. The constraint is claimed as
 *   tangled_rope: genuine coordination function (protecting the vulnerable
 *   commons) combined with asymmetric extraction (coercive transfer onto
 *   resisters). This reading coexists with the bodily_autonomy_primary and
 *   proportionality_reading siblings — neither is logically foreclosed within
 *   a single framework, but they occupy different parties' commitments.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__public_health_primary, 0.78).
domain_priors:suppression_score(public_health_mandate_authority__public_health_primary, 0.72).
domain_priors:theater_ratio(public_health_mandate_authority__public_health_primary, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, extractiveness, 0.78).
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__public_health_primary, tangled_rope).
narrative_ontology:human_readable(public_health_mandate_authority__public_health_primary, "Public Health Mandate Authority — Vulnerable Commons Protection Reading").
narrative_ontology:topic_domain(public_health_mandate_authority__public_health_primary, "public_health_law/constitutional_rights/bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__public_health_primary, '05352a31-ea69-4dad-adf2-86470e1cde35').
narrative_ontology:cs_kernel_codification('05352a31-ea69-4dad-adf2-86470e1cde35', formalized).
narrative_ontology:cs_authority_grounding('05352a31-ea69-4dad-adf2-86470e1cde35', lineage).
narrative_ontology:cs_interpretation_layer_present('05352a31-ea69-4dad-adf2-86470e1cde35').
narrative_ontology:cs_reading_relation('05352a31-ea69-4dad-adf2-86470e1cde35', public_health_mandate_authority__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('05352a31-ea69-4dad-adf2-86470e1cde35', public_health_mandate_authority__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('05352a31-ea69-4dad-adf2-86470e1cde35', foundational, vulnerable_commons_protection_obligation).
narrative_ontology:cs_axiom_status(vulnerable_commons_protection_obligation, holdable).
narrative_ontology:cs_axiom_grounding('05352a31-ea69-4dad-adf2-86470e1cde35', vulnerable_commons_protection_obligation, deontological).
narrative_ontology:cs_axiom('05352a31-ea69-4dad-adf2-86470e1cde35', foundational, collective_action_justifies_coercive_transfer).
narrative_ontology:cs_axiom_status(collective_action_justifies_coercive_transfer, holdable).
narrative_ontology:cs_axiom_grounding('05352a31-ea69-4dad-adf2-86470e1cde35', collective_action_justifies_coercive_transfer, instrumental).
narrative_ontology:cs_reference_frame('05352a31-ea69-4dad-adf2-86470e1cde35', classical_police_power_public_health).
narrative_ontology:cs_drift_state('05352a31-ea69-4dad-adf2-86470e1cde35', post_covid_mandate_expansion, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('05352a31-ea69-4dad-adf2-86470e1cde35', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__public_health_primary, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, immunocompromised_population).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, healthcare_infrastructure).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, public_health_authorities).
narrative_ontology:constraint_victim(public_health_mandate_authority__public_health_primary, mandate_resistant_individuals).
narrative_ontology:constraint_victim(public_health_mandate_authority__public_health_primary, employment_coerced_workers).
narrative_ontology:constraint_vindicates(public_health_mandate_authority__public_health_primary, collective_action_obligation_for_vulnerable_commons).
narrative_ontology:constraint_vindicates(public_health_mandate_authority__public_health_primary, public_health_necessity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Cannot safely access public spaces without community-wide mitigation. Gains protection from collective action but has no structural power to enforce it. When mandates fail, they bear the infection risk directly — enters victim set under mandate failure.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, immunocompromised_population, beneficiary,
    powerless, biographical, trapped, national).

% Hospitals and care systems avoid collapse when transmission is suppressed. Their administrators set triage protocols and advocate for mandates as operational necessity. They collect the benefit of capacity preservation but also administer the constraint's enforcement.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, healthcare_infrastructure, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(public_health_mandate_authority__public_health_primary, healthcare_infrastructure, agenda_setter).

% Design, issue, and enforce mandates. Their institutional legitimacy and resource flows depend on the mandate framework. They coordinate the collective action and extract compliance through licensing, funding, and legal authority.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, public_health_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Refuse or resist mandates on grounds of bodily autonomy, religious objection, or risk assessment. Face exclusion from employment, education, travel, and public accommodation. Coerced through loss of livelihood and civic participation — high extractiveness on this seat.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, mandate_resistant_individuals, payer,
    moderate, biographical, constrained, national).

% Comply with mandates solely to retain employment or access to essential services. Have no ideological resistance but no meaningful exit — losing income means immediate material harm. Extraction is experienced as forced transfer rather than chosen contribution.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, employment_coerced_workers, payer,
    powerless, immediate, trapped, national).

% Frame mandates as categorical violation of bodily sovereignty. Organize litigation, legislative challenges, and public campaigns. Their exclusion from the constraint's justification structure is structural — the reading defines them as free-riders imposing externality rather than rights-holders.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, bodily_autonomy_advocates, excluded,
    organized, biographical, mobile, national).

% Evaluate mandates on sliding scale: threat severity, alternatives, coercion magnitude, duration. Neither categorically for nor against; their analysis is excluded from the binary framing of this reading but informs judicial review and policy design.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, proportionality_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of suppressing pathogen transmission to protect those who cannot self-protect (immunocompromised) and prevent healthcare system collapse. Without mandates, individual rationality leads to under-mitigation and the commons is degraded.
% TRANSFER_FUNCTION: Moves compliance burden (vaccination, masking, testing, isolation) from the vulnerable population onto the general population, and moves enforcement costs (employment loss, service denial, legal penalties) onto mandate-resistant individuals. The transfer is justified as externality internalization by this reading.
% ABSENT_VOICES: Bodily autonomy advocates are structurally excluded from the constraint's justification — their objection is framed as free-riding rather than rights-claim. People with medical contraindications to vaccines (distinct from immunocompromised) are also absent; they bear mandate coercion without the immunological benefit.
% DISAPPEARANCE_RATIONALE: If mandates vanished overnight, transmission would rise, immunocompromised people would lose safe access to public life, healthcare systems would face surge risk, and the social contract around collective protection would fracture. The world would reorganize around individualized risk management with substantially higher morbidity for the vulnerable.
% FOUNDING_PROBLEM: Historical pandemics (smallpox, polio, 1918 influenza) demonstrated that voluntary mitigation fails to protect the vulnerable and overwhelms healthcare infrastructure. The mandate framework was built to convert collective-action failure into coordinated protection.
% FOUNDING_PROBLEM_CORROBORATION: Public health historians and epidemiologists outside the benefiting authorities corroborate that mandates achieved eradication/control of specific diseases. Critics from the bodily autonomy and proportionality readings contest whether the founding problem (pandemics of that severity) persists for current mandates, citing lower IFR pathogens and available alternatives. No neutral arbiter has settled this.
narrative_ontology:disappearance_verdict(public_health_mandate_authority__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(public_health_mandate_authority__public_health_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__public_health_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(public_health_mandate_authority__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(public_health_mandate_authority__public_health_primary, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_health_mandate_authority__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(public_health_mandate_authority__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(public_health_mandate_authority__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the constraint transfers substantial compliance costs onto mandate-resistant individuals via employment loss and service denial — coercion, not voluntary contribution. Suppression (0.72) is high because alternatives (remote work, medical exemptions, targeted protection) are actively restricted or made inaccessible to maintain mandate universality. Theater ratio (0.22) is moderate-low: the coordination function (transmission suppression) is real and measured, but a growing share of enforcement activity serves mandate compliance rather than direct infection control. Accessibility collapse (0.48) reflects that alternatives exist but are structurally discouraged. Resistance (0.61) captures sustained legal, political, and non-compliance pushback.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter/beneficiary seats (public health authorities, healthcare infrastructure), the constraint computes as rope: genuine coordination solving a collective-action problem with net benefit to the protected. From the payer seats (mandate-resistant, employment-coerced), it computes as snare: coercive extraction with suppressed alternatives. The engine computes this divergence from the structural data — the claimed_type (tangled_rope) reflects the hybrid structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Immunocompromised and healthcare infrastructure are beneficiaries (d near 0.0) — the constraint subsidizes their protection. Public health authorities are agenda_setters with arbitrage-grade exit (d ~0.15) — they administer and benefit institutionally. Mandate-resistant individuals are primary targets (d near 1.0) — they bear the coercive transfer. Employment-coerced workers are trapped payers (d ~0.85) — they comply under duress with no ideological position. Bodily autonomy advocates are excluded (d undefined — outside the constraint's coordination logic). Proportionality scholars are observers (d = 0.5 analytical).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (pandemic-scale collective action failure) is contested as live vs. resolved. If the founding problem is dead but the mandate apparatus persists and expands to lower-severity pathogens, mandatrophy is unresolved — the constraint has become a piton or snare. This reading's claimed tangled_rope status depends on the founding problem remaining live; if status resolves to 'dead,' the coordination function evaporates and the constraint reclassifies toward snare. The omega variable 'founding_problem_persistence' captures this.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_persistence,
    'Does the founding problem (pandemic-scale collective action failure threatening vulnerable commons) persist for current mandate applications, or has it been resolved by medical countermeasures and changed epidemiology?',
    'Longitudinal epidemiological analysis: if pathogens subject to mandates no longer produce healthcare collapse or disproportionate immunocompromised mortality without mandates, the founding problem is resolved. Requires counterfactual modeling of mandate removal.',
    'If founding problem is resolved, the coordination function evaporates and the constraint reclassifies from tangled_rope toward snare (pure extraction). If contested remains, tangled_rope holds. If live, rope or tangled_rope depending on extraction ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether the original collective-action failure that justified mandates still obtains.').

omega_variable(
    coordination_extraction_separability,
    'Is the transmission-suppression coordination function structurally separable from the coercive enforcement apparatus, or does the coordination require the extraction?',
    'Natural experiment from jurisdictions using voluntary high-uptake strategies (e.g., high-trust societies with transparent communication): if comparable protection of vulnerable commons is achieved without coercive mandates, the functions are separable.',
    'If separable, the measured extraction is avoidable overhead — the constraint is a snare dressed as coordination. If inseparable, part of the extraction is the price of coordination itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the constraint''s coordination and extraction components can be decoupled in practice.').

omega_variable(
    immunocompromised_victim_set_conditionality,
    'Does the immunocompromised population enter the victim set ONLY when mandates fail, or are they also victims of the mandate''s collateral harms (isolation, delayed care, social exclusion)?',
    'Mixed-methods study of immunocompromised outcomes under mandates vs. counterfactual: measure infection risk reduction against psychosocial and healthcare access harms from mandate enforcement.',
    'If immunocompromised are net victims even with mandates, the beneficiary claim weakens and the constraint''s coordination function is undermined. If they are net beneficiaries only when mandates work, the victim-set conditionality holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immunocompromised_victim_set_conditionality, empirical, 'Whether the primary beneficiary group experiences net harm from the constraint''s operation.').

omega_variable(
    kernel_reading_foreclosure_structure,
    'Does the public_health_primary reading logically foreclose the bodily_autonomy_primary reading within any single legal-ethical framework, or do they coexist as competing frameworks?',
    'Jurisprudential analysis: if a court or legislature adopts public_health_primary as the governing framework, does it necessarily invalidate bodily_autonomy_primary claims, or can both be accommodated (e.g., via exemptions)? Historical test: Jacobson v. Massachusetts vs. modern exemption jurisprudence.',
    'If forecloses, the kernel has a winner-take-all structure and the engine''s foreclosure computation will resolve it. If coexists_with, the kernel sustains permanent structural tension and the constraint family persists as competing readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_structure, conceptual, 'Logical relationship between the primary competing readings of the mandate authority kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__public_health_primary, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(publ_tr_t0, public_health_mandate_authority__public_health_primary, theater_ratio, 0, 0.12).
narrative_ontology:measurement(publ_tr_t6, public_health_mandate_authority__public_health_primary, theater_ratio, 6, 0.15).
narrative_ontology:measurement(publ_tr_t12, public_health_mandate_authority__public_health_primary, theater_ratio, 12, 0.19).
narrative_ontology:measurement(publ_tr_t18, public_health_mandate_authority__public_health_primary, theater_ratio, 18, 0.21).
narrative_ontology:measurement(publ_tr_t24, public_health_mandate_authority__public_health_primary, theater_ratio, 24, 0.22).

% Extraction over time
narrative_ontology:measurement(publ_be_t0, public_health_mandate_authority__public_health_primary, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(publ_be_t6, public_health_mandate_authority__public_health_primary, base_extractiveness, 6, 0.61).
narrative_ontology:measurement(publ_be_t12, public_health_mandate_authority__public_health_primary, base_extractiveness, 12, 0.73).
narrative_ontology:measurement(publ_be_t18, public_health_mandate_authority__public_health_primary, base_extractiveness, 18, 0.77).
narrative_ontology:measurement(publ_be_t24, public_health_mandate_authority__public_health_primary, base_extractiveness, 24, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(publ_su_t0, public_health_mandate_authority__public_health_primary, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(publ_su_t6, public_health_mandate_authority__public_health_primary, suppression_requirement, 6, 0.58).
narrative_ontology:measurement(publ_su_t12, public_health_mandate_authority__public_health_primary, suppression_requirement, 12, 0.68).
narrative_ontology:measurement(publ_su_t18, public_health_mandate_authority__public_health_primary, suppression_requirement, 18, 0.71).
narrative_ontology:measurement(publ_su_t24, public_health_mandate_authority__public_health_primary, suppression_requirement, 24, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_mandate_authority__public_health_primary, resource_allocation).
narrative_ontology:boltzmann_floor_override(public_health_mandate_authority__public_health_primary, 0.12).
narrative_ontology:affects_constraint(public_health_mandate_authority__public_health_primary, public_health_mandate_authority__bodily_autonomy_primary).
narrative_ontology:affects_constraint(public_health_mandate_authority__public_health_primary, public_health_mandate_authority__proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the public_health_mandate_authority kernel. The public_health_primary reading frames mandates as obligation to protect vulnerable commons (tangled_rope). The bodily_autonomy_primary reading frames mandates as categorical bodily sovereignty violation (snare). The proportionality_reading frames mandates as sliding-scale legitimacy (rope or scaffold depending on context). All three share the same mandate apparatus but differ in ε referent, victim/beneficiary sets, and claimed_type. The ε values differ substantially: public_health_primary ε=0.78 (high extraction on resisters), bodily_autonomy_primary ε≈0.9 (extraction on all mandated), proportionality_reading ε varies by threat level.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(public_health_mandate_authority__public_health_primary, moderate, 0.88).
constraint_indexing:directionality_override(public_health_mandate_authority__public_health_primary, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
