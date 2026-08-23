% ============================================================================
% CONSTRAINT STORY: public_health_mandate_authority__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
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
 *   human_readable: Public Health Mandate Authority (Public Health Primary Reading)
 *   domain: public_health_law/constitutional_rights/bioethics
 *
 * SUMMARY:
 *   This constraint story captures the public_health_primary reading of the
 *   contested kernel 'public_health_mandate_authority.' The reading frames
 *   mandates as obligatory collective action to protect the vulnerable
 *   commons — immunocompromised individuals and healthcare infrastructure —
 *   from the negative externality of non-compliance. The mandate-resistant
 *   are structurally positioned as free-riders whose refusal imposes costs on
 *   the vulnerable; the coercion they face (employment loss, service denial)
 *   is the enforcement price of the coordination function. The claim/metric
 *   independence is deliberate: the reading CLAIMS tangled_rope (genuine
 *   coordination + asymmetric extraction + active enforcement) while the
 *   authored metrics describe the operational reality of that structure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__public_health_primary, 0.65).
domain_priors:suppression_score(public_health_mandate_authority__public_health_primary, 0.75).
domain_priors:theater_ratio(public_health_mandate_authority__public_health_primary, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, extractiveness, 0.65).
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__public_health_primary, tangled_rope).
narrative_ontology:human_readable(public_health_mandate_authority__public_health_primary, "Public Health Mandate Authority (Public Health Primary Reading)").
narrative_ontology:topic_domain(public_health_mandate_authority__public_health_primary, "public_health_law/constitutional_rights/bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__public_health_primary, '7f8fe83f-9738-4709-9bbb-8cf92b67a0e6').
narrative_ontology:cs_kernel_codification('7f8fe83f-9738-4709-9bbb-8cf92b67a0e6', formalized).
narrative_ontology:cs_authority_grounding('7f8fe83f-9738-4709-9bbb-8cf92b67a0e6', expertise).
narrative_ontology:cs_interpretation_layer_present('7f8fe83f-9738-4709-9bbb-8cf92b67a0e6').
narrative_ontology:cs_reading_relation('7f8fe83f-9738-4709-9bbb-8cf92b67a0e6', public_health_mandate_authority__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('7f8fe83f-9738-4709-9bbb-8cf92b67a0e6', public_health_mandate_authority__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('7f8fe83f-9738-4709-9bbb-8cf92b67a0e6', foundational, collective_obligation_to_protect_vulnerable_commons).
narrative_ontology:cs_axiom_status(collective_obligation_to_protect_vulnerable_commons, holdable).
narrative_ontology:cs_axiom_grounding('7f8fe83f-9738-4709-9bbb-8cf92b67a0e6', collective_obligation_to_protect_vulnerable_commons, deontological).
narrative_ontology:cs_axiom('7f8fe83f-9738-4709-9bbb-8cf92b67a0e6', secondary, healthcare_infrastructure_preservation_as_public_good).
narrative_ontology:cs_axiom_status(healthcare_infrastructure_preservation_as_public_good, holdable).
narrative_ontology:cs_axiom_grounding('7f8fe83f-9738-4709-9bbb-8cf92b67a0e6', healthcare_infrastructure_preservation_as_public_good, instrumental).
narrative_ontology:cs_reference_frame('7f8fe83f-9738-4709-9bbb-8cf92b67a0e6', collective_duty_framework).
narrative_ontology:cs_drift_state('7f8fe83f-9738-4709-9bbb-8cf92b67a0e6', post_pandemic_rights_challenges, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7f8fe83f-9738-4709-9bbb-8cf92b67a0e6', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__public_health_primary, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, immunocompromised_individuals).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, healthcare_infrastructure).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, general_public).
narrative_ontology:constraint_victim(public_health_mandate_authority__public_health_primary, mandate_resistant_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(public_health_mandate_authority__public_health_primary, healthcare_infrastructure).
narrative_ontology:constraint_victim(public_health_mandate_authority__public_health_primary, general_public).
narrative_ontology:constraint_victim(public_health_mandate_authority__public_health_primary, employers_businesses).
narrative_ontology:constraint_vindicates(public_health_mandate_authority__public_health_primary, collective_obligation_to_vulnerable_commons).
narrative_ontology:constraint_vindicates(public_health_mandate_authority__public_health_primary, healthcare_system_preservation_as_public_good).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issue and enforce mandates (vaccination, masking, distancing) under statutory authority. Justify mandates as necessary to protect immunocompromised and prevent healthcare collapse. Control exemption criteria and enforcement mechanisms. Bear political costs of mandate resistance but hold institutional authority to sustain mandates.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, public_health_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Depend on collective compliance for protection from severe outcomes. Cannot safely exit shared spaces without mandate-backed community immunity. Face disproportionate risk when mandates lapse. Have no structural power to enforce mandates themselves; protection is entirely derivative of others' compliance.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, immunocompromised_individuals, beneficiary,
    powerless, biographical, trapped, national).

% Hospitals and health systems benefit from mandate-driven surge prevention but bear operational burden of mandate enforcement (screening, vaccination verification, surge capacity). Staff face burnout from both disease surges and enforcement duties. Cannot exit the system without collapsing care delivery.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, healthcare_infrastructure, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(public_health_mandate_authority__public_health_primary, healthcare_infrastructure, payer).

% Face employment termination, service denial, educational exclusion, or movement restrictions for non-compliance. Framed by this reading as free-riders imposing negative externality on vulnerable commons. Exit options limited: compliance, exemption (narrow), relocation, or bearing coercive penalties. Organize politically and legally but lack institutional power to overturn mandates.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, mandate_resistant_individuals, payer,
    moderate, biographical, constrained, national).

% Benefit from healthcare system stability and reduced transmission. Bear compliance costs (time, discomfort, minor risk) and indirect economic costs. Most comply voluntarily; exit via non-compliance is socially and legally constrained but individually feasible. Political voice expressed through elections and advocacy.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, general_public, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(public_health_mandate_authority__public_health_primary, general_public, payer).

% Bear enforcement costs (verification, termination, litigation risk) and productivity losses. Some large employers support mandates for workforce stability; others resist as operational burden. Can relocate operations or lobby for policy change. Not directly health-vulnerable but structurally positioned as enforcement intermediaries.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, employers_businesses, payer,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(public_health_mandate_authority__public_health_primary, employers_businesses, agenda_setter).

% Adjudicate challenges to mandate authority (religious liberty, due process, statutory authority). Shape the practical scope of mandates through precedent. Do not bear health risks or enforcement costs directly; legitimacy derives from constitutional interpretation, not public health expertise.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, courts_legal_system, observer,
    institutional, generational, analytical, national).

% Produce normative frameworks for mandate justification (least restrictive means, proportionality, reciprocity). Influence policy discourse but hold no enforcement power. Their authority is epistemic, not institutional.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, bioethics_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents healthcare system collapse and protects immunocompromised individuals by aligning population behavior (vaccination, masking, distancing) around a collective shield that no individual can provide alone.
% TRANSFER_FUNCTION: Transfers autonomy and liberty interests from mandate-resistant individuals (who bear coercion via employment/service loss) to collective risk reduction for immunocompromised and healthcare infrastructure. Transfers surge risk from healthcare system to non-compliant individuals via exclusion mechanisms.
% ABSENT_VOICES: Medically exempt individuals who cannot access formal exemptions due to bureaucratic barriers; children and dependents of mandate-resistant caregivers; future generations inheriting the precedent of state-compelled medical intervention; undocumented populations excluded from both mandate protections and exemption pathways.
% DISAPPEARANCE_RATIONALE: If mandates vanished overnight, vaccination rates would drop sharply in resistant populations, healthcare surge capacity would be tested within weeks, immunocompromised individuals would lose their collective shield and face exclusion from public life, and the legal framework for public health authority would face existential challenge.
% FOUNDING_PROBLEM: Recurrent infectious disease crises (smallpox, polio, 1918 influenza, HIV/AIDS, COVID-19) that overwhelmed healthcare infrastructure and disproportionately killed vulnerable populations, demonstrating that voluntary measures alone cannot achieve population-level protection.
% FOUNDING_PROBLEM_CORROBORATION: Epidemiological history documents recurring surges when collective measures lapse; historical public health literature (e.g., Jacobson v. Massachusetts, smallpox eradication) attests to the necessity of mandates; independent commissions (e.g., COVID-19 origin investigations, pandemic preparedness reviews) corroborate ongoing threat; not solely asserted by mandate beneficiaries.
narrative_ontology:disappearance_verdict(public_health_mandate_authority__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(public_health_mandate_authority__public_health_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__public_health_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(public_health_mandate_authority__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(public_health_mandate_authority__public_health_primary, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.65) reflects substantial but not total extraction: mandate-resistant bear high coercive costs (employment, services), but the mandate also delivers real coordination value (healthcare preservation, vulnerable protection). Suppression (0.75) is high because mandate persistence depends on active exclusion of non-compliant individuals from employment, education, and public accommodation — not on voluntary adherence. Theater ratio (0.25) is low-moderate: the public health function is genuine, but a growing fraction of enforcement activity serves compliance signaling over marginal epidemiological benefit. Accessibility collapse (0.55) is moderate: alternatives exist (exemptions, relocation, remote participation) but are structurally constrained. Resistance (0.70) is high: organized legal, political, and cultural opposition sustains contestation.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seat (public health authorities), the constraint is a necessary coordination mechanism with justified enforcement. From the payer seat (mandate-resistant), it is experienced as coercive extraction with no reciprocal benefit — they are framed as free-riders but experience the mandate as a penalty for non-conformity. From the trapped beneficiary seat (immunocompromised), the mandate is the only barrier to exclusion from public life. The engine computes these seat-specific classifications from the structural data; the authored claim does not adjudicate the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities are agenda_setters with institutional power and arbitrage-grade exit (they administer the constraint). Immunocompromised are beneficiaries with trapped exit — they cannot protect themselves without the mandate. Healthcare infrastructure is a dual beneficiary/payer: it gains system stability but bears enforcement burden. Mandate-resistant are payers with constrained exit — they bear coercion costs but cannot easily escape the jurisdiction. General public are beneficiaries with mobile exit — they gain protection but can comply at modest cost. Employers are payers with agenda_setter secondary role — they enforce mandates but can lobby or relocate. Courts and scholars are observers with analytical exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate authority's founding problem (recurrent healthcare collapse from infectious disease) remains contested — not dead (new pathogens emerge) but not universally accepted as live (vaccines/treatments reduce severity). The constraint persists partly through institutional inertia (public health statutes rarely sunset) and partly through genuine coordination need. The classification as tangled_rope (not snare) hinges on the coordination function being real and substantial: without mandates, the vulnerable commons suffers measurable harm. The risk of false summit (mountain claim) is low because no party claims mandates are natural law; the risk of misclassifying as pure snare is mitigated by the documented beneficiary structure (immunocompromised, healthcare infrastructure).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing_ambiguity,
    'Does the public_health_primary reading''s framing of mandate-resistant individuals as ''free-riders imposing externality'' accurately capture the structural relationship, or does it obscure legitimate liberty interests that the proportionality_reading would protect?',
    'Comparative analysis of mandate outcomes in jurisdictions with robust exemption frameworks vs. those without: if health outcomes are similar but liberty costs differ, the free-rider framing is partially a cover for extractive overreach.',
    'If the free-rider framing is empirically unsupported, the constraint shifts toward snare (pure extraction) from the mandate-resistant seat; if supported, tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing_ambiguity, conceptual, 'Whether the free-rider framing is a genuine structural description or a normative cover for asymmetric extraction.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the high suppression (0.75) experienced by mandate-resistant individuals primarily structural (employment termination, service denial) or partially internalized (moral stigma, social ostracism that persists after formal mandates lift)?',
    'Longitudinal study of mandate-resistant populations after mandate removal: if employment/social penalties persist, internalized suppression is significant.',
    'If internalized suppression is substantial, the constraint''s effective suppression exceeds the structural measure — the target carries suppression after formal exit, supporting snare classification from that seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for mandate-resistant individuals.').

omega_variable(
    coordination_extraction_boundary,
    'At what threshold does the coordination function (protecting vulnerable commons) become a cover for extraction (expanding mandates beyond epidemiological justification)?',
    'Counterfactual modeling: simulate mandate removal with and without alternative protections (improved ventilation, targeted prophylaxis, healthcare surge capacity). If alternatives achieve comparable vulnerable protection at lower coercion, the mandate''s coordination claim is inflated.',
    'If coordination is achievable with lower coercion, the constraint''s extractiveness is over-authored and the tangled_rope classification masks a snare core.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether the mandate''s coordination function is structurally necessary or contingently inflated.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__public_health_primary, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(publ_tr_t0, public_health_mandate_authority__public_health_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(publ_tr_t5, public_health_mandate_authority__public_health_primary, theater_ratio, 5, 0.12).
narrative_ontology:measurement(publ_tr_t10, public_health_mandate_authority__public_health_primary, theater_ratio, 10, 0.18).
narrative_ontology:measurement(publ_tr_t15, public_health_mandate_authority__public_health_primary, theater_ratio, 15, 0.22).
narrative_ontology:measurement(publ_tr_t20, public_health_mandate_authority__public_health_primary, theater_ratio, 20, 0.25).
narrative_ontology:measurement(publ_tr_t25, public_health_mandate_authority__public_health_primary, theater_ratio, 25, 0.25).

% Extraction over time
narrative_ontology:measurement(publ_be_t0, public_health_mandate_authority__public_health_primary, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(publ_be_t5, public_health_mandate_authority__public_health_primary, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(publ_be_t10, public_health_mandate_authority__public_health_primary, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(publ_be_t15, public_health_mandate_authority__public_health_primary, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(publ_be_t20, public_health_mandate_authority__public_health_primary, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(publ_be_t25, public_health_mandate_authority__public_health_primary, base_extractiveness, 25, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(publ_su_t0, public_health_mandate_authority__public_health_primary, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(publ_su_t5, public_health_mandate_authority__public_health_primary, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(publ_su_t10, public_health_mandate_authority__public_health_primary, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(publ_su_t15, public_health_mandate_authority__public_health_primary, suppression_requirement, 15, 0.72).
narrative_ontology:measurement(publ_su_t20, public_health_mandate_authority__public_health_primary, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(publ_su_t25, public_health_mandate_authority__public_health_primary, suppression_requirement, 25, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_mandate_authority__public_health_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(public_health_mandate_authority__public_health_primary, 0.1).
narrative_ontology:affects_constraint(public_health_mandate_authority__public_health_primary, public_health_mandate_authority__bodily_autonomy_primary).
narrative_ontology:affects_constraint(public_health_mandate_authority__public_health_primary, public_health_mandate_authority__proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint (public_health_primary) and its siblings (bodily_autonomy_primary, proportionality_reading) form a constraint family decomposing the kernel 'public_health_mandate_authority.' The ε values differ substantially: public_health_primary authors ε=0.65 (substantial extraction on mandate-resistant); bodily_autonomy_primary would author ε≈0.9 (categorical violation); proportionality_reading would author ε variable (0.3-0.8 depending on sliding scale). They share the referent (mandate authority) but instantiate different constraints per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(public_health_mandate_authority__public_health_primary, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
