% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading
 *   human_readable: Bodily Autonomy Primacy: Absolute Prohibition on Vaccine Mandates
 *   domain: public_health_policy/constitutional_law/bioethics
 *
 * SUMMARY:
 *   This constraint story instantiates the bodily_autonomy_primacy_reading of
 *   the vaccine_mandate_legitimacy kernel. The reading asserts that medical
 *   self-sovereignty is absolute and state coercion for vaccination is
 *   categorically impermissible regardless of epidemiological outcome. It
 *   claims the status of a natural law / mountain constraint
 *   (emerges_naturally: true), but declares identifiable beneficiaries
 *   (liberty advocacy movements, bodily autonomy organizations,
 *   vaccine-hesitant individuals) and identifiable victims (immunocompromised
 *   individuals, medically vulnerable populations, public health
 *   infrastructure). This beneficiary/victim structure makes it a False
 *   Summit Mountain candidate — the FSM signature will evaluate whether the
 *   claimed natural-law immunity masks a constructed constraint that benefits
 *   organized advocacy movements at the expense of the vulnerable. The ε
 *   referent is the standing arrangement of state vaccine mandate authority,
 *   assessed by this reading's lights as highly extractive (0.82 at interval
 *   end).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.82).
domain_priors:suppression_score(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.88).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, mountain).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, "Bodily Autonomy Primacy: Absolute Prohibition on Vaccine Mandates").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, "public_health_policy/constitutional_law/bioethics").

domain_priors:emerges_naturally(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 'fcd48535-ec3e-4ddd-a1ce-7cd4454730a7').
narrative_ontology:cs_kernel_codification('fcd48535-ec3e-4ddd-a1ce-7cd4454730a7', distributed).
narrative_ontology:cs_authority_grounding('fcd48535-ec3e-4ddd-a1ce-7cd4454730a7', distributed).
narrative_ontology:cs_reading_relation('fcd48535-ec3e-4ddd-a1ce-7cd4454730a7', vaccine_mandate_legitimacy__public_health_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('fcd48535-ec3e-4ddd-a1ce-7cd4454730a7', vaccine_mandate_legitimacy__risk_stratification_reading, influences).
narrative_ontology:cs_axiom('fcd48535-ec3e-4ddd-a1ce-7cd4454730a7', foundational, bodily_integrity_absolute_against_state_coercion).
narrative_ontology:cs_axiom_status(bodily_integrity_absolute_against_state_coercion, holdable).
narrative_ontology:cs_axiom_grounding('fcd48535-ec3e-4ddd-a1ce-7cd4454730a7', bodily_integrity_absolute_against_state_coercion, deontological).
narrative_ontology:cs_axiom('fcd48535-ec3e-4ddd-a1ce-7cd4454730a7', secondary, informed_consent_non_derogable).
narrative_ontology:cs_axiom_status(informed_consent_non_derogable, holdable).
narrative_ontology:cs_axiom_grounding('fcd48535-ec3e-4ddd-a1ce-7cd4454730a7', informed_consent_non_derogable, deontological).
narrative_ontology:cs_reference_frame('fcd48535-ec3e-4ddd-a1ce-7cd4454730a7', pre_jacobson_natural_rights_framework).
narrative_ontology:cs_drift_state('fcd48535-ec3e-4ddd-a1ce-7cd4454730a7', post_covid_mandate_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('fcd48535-ec3e-4ddd-a1ce-7cd4454730a7', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, liberty_advocacy_movements).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, bodily_autonomy_organizations).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_hesitant_individuals).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, immunocompromised_individuals).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, medically_vulnerable_populations).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, public_health_infrastructure).
narrative_ontology:constraint_vindicates(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, bodily_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, informed_consent_absolutism).
narrative_ontology:constraint_vindicates(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, anti_coercion_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organize litigation, legislation, and public campaigns to establish and defend absolute bodily autonomy against vaccine mandates. Gain legitimacy, funding, and membership from the principle's adoption. Can shift strategy across courts, legislatures, and public opinion.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, liberty_advocacy_movements, beneficiary,
    organized, generational, mobile, national).

% Specialized NGOs (e.g., ICAN, Children's Health Defense) that set the legal and rhetorical agenda for bodily autonomy absolutism. They draft model legislation, fund test cases, and define the intellectual boundaries of the reading. Benefit directly from the constraint's normative force.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, bodily_autonomy_organizations, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, bodily_autonomy_organizations, agenda_setter).

% Individuals who decline vaccines for religious, philosophical, or safety reasons. Directly benefit from the prohibition on mandates — they retain access to schools, workplaces, and public life without vaccination. Exit is constrained by social stigma and employment conditions, but the principle protects their choice.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_hesitant_individuals, beneficiary,
    moderate, biographical, constrained, national).

% Cannot mount adequate vaccine response; depend on high community vaccination for indirect protection. When mandates are prohibited, vaccination rates drop in schools and workplaces, directly increasing their exposure risk. Cannot exit the risk — immunosuppression is not chosen, and isolation from society is the only alternative.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, immunocompromised_individuals, payer,
    powerless, biographical, trapped, national).

% Includes infants too young for vaccination, elderly with waning immunity, and people with conditions contraindicating vaccination. Bear disproportionate morbidity and mortality when mandates are absent and community coverage falls. Structural vulnerability is not escapable; the constraint removes the primary policy tool that protected them.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, medically_vulnerable_populations, payer,
    powerless, biographical, trapped, national).

% State and local health departments that would impose mandates during outbreaks. The absolute prohibition constrains their toolkit — they lose the most effective containment lever. They are not mere victims; they actively contest the reading in courts and legislatures, but their institutional role is structurally limited by it.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Adjudicate clashes between bodily autonomy claims and police power. Their rulings determine whether the absolute prohibition becomes binding law or remains aspirational. They do not collect rents from either side but their doctrinal choices shape the constraint's effective force.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% Provide the empirical foundation for mandate efficacy and herd immunity thresholds. Their consensus that mandates increase coverage and protect the vulnerable is the primary evidence against the reading's claim that mandates are unnecessary. They hold no enforcement power but their authority is cited by all sides.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, epidemiologists_infectious_disease_experts, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a normative boundary that treats bodily integrity as non-negotiable, preventing state instrumentalization of individual bodies for collective ends regardless of epidemiological circumstances.
% TRANSFER_FUNCTION: Moves the burden of disease exposure from the unvaccinated (who retain choice) to the immunocompromised and medically vulnerable (who bear disproportionate infection risk when community coverage falls), while transferring legitimacy capital and policy influence to anti-mandate movements.
% ABSENT_VOICES: Immunocompromised individuals and families of medically vulnerable children who cannot safely participate in public life when vaccination rates drop; public health practitioners who lose a primary outbreak containment tool; parents of children too young to vaccinate who rely on community protection. These voices are structurally excluded from the liberty-movement framing because their vulnerability is treated as an externality, not a stake.
% DISAPPEARANCE_RATIONALE: If the absolute prohibition on vaccine mandates disappeared overnight, states would regain authority to impose mandates during outbreaks, vaccination rates would likely rise in mandated contexts (schools, healthcare, military), and immunocompromised individuals would regain indirect protection — the public health architecture would reorganize around collective protection as the default, with opt-outs as exceptions rather than the reverse.
% FOUNDING_PROBLEM: State overreach into medical decision-making established in Jacobson v. Massachusetts (1905) and accelerated by COVID-era mandates created precedent for unbounded state access to bodies during declared emergencies, with no principled stopping point.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars outside the liberty movement (e.g., Lawrence Gostin, Wendy Parmet, historians of Jacobson) attest the founding problem is contested: Jacobson was always narrow (smallpox, ~30% mortality, no less restrictive means), but COVID mandates expanded the precedent to low-mortality pathogens with leaky vaccines. No consensus exists outside the benefiting parties on whether the 'unbounded precedent' claim reflects actual doctrine or advocacy framing.
narrative_ontology:disappearance_verdict(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, ExtMetricName, E),
    domain_priors:suppression_score(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading),
    narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because from this reading's perspective, the standing arrangement (state mandate authority) extracts bodily autonomy from individuals without consent. Suppression is very high (0.88) because the reading demands complete suppression of mandate authority — no exceptions, no balancing. Theater is low-moderate (0.28) because the principle is genuinely held, but performative elements grew during COVID (rhetorical escalation, fundraising on mandate opposition). Accessibility_collapse (0.75) reflects the reading's claim that no legitimate alternative exists — but descriptively, the risk_stratification_reading demonstrates a coherent middle ground. Resistance (0.72) is high because public health authorities, courts, and medical establishments actively contest the absolutist frame.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (immunocompromised, vulnerable) and beneficiary seats (liberty movements) should compute radically different types: from the vulnerable's position, the constraint is a snare (pure extraction of their protection); from the liberty movement's position, it is a mountain (absolute right). The engine computes this divergence from the structural data — the authored claim (mountain) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Liberty advocacy movements and bodily autonomy organizations are structural beneficiaries (d near 0.0) — they gain legitimacy, funding, and policy influence from the principle. Vaccine-hesitant individuals are direct beneficiaries but with constrained exit (d ~0.2). Immunocompromised and medically vulnerable are full targets (d near 1.0) — they bear the epidemiological externality with no exit. Public health authorities are constrained agenda_setters (d ~0.6) — they lose their primary containment tool but retain institutional voice. Courts and epidemiologists are analytical observers (d ~0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The reading presents itself as solving a live founding problem (unbounded state precedent), but the corroboration field shows this is contested outside the benefiting parties. If the founding problem is dead (Jacobson was always narrow, COVID was exceptional), the absolute prohibition persists as a piton — a degraded coordination mechanism (protecting bodily autonomy) maintained theatrically after its justification evaporated. If live, it remains a tangled_rope (genuine coordination against state overreach, but with asymmetric extraction on the vulnerable). The mandatrophy_resolved flag is not set — the status is contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_right_vs_constructed_immunity,
    'Is bodily autonomy against vaccination a genuine natural law (mountain) or a constructed constraint that benefits identifiable liberty advocacy movements (false summit)?',
    'Historical-philosophical analysis: does the absolute prohibition have pre-legal, cross-cultural recognition as a natural right, or does it emerge from specific 20th/21st-century libertarian legal theory? Corpus analysis of judicial and scholarly citation networks.',
    'If natural law, the mountain claim holds and FSM does not fire. If constructed, FSM reclassifies to tangled_rope (coordination against state overreach + asymmetric extraction on vulnerable) or snare (if coordination function is pretextual).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_right_vs_constructed_immunity, conceptual, 'Natural-law vs constructed-status ambiguity triggering False Summit Mountain detection.').

omega_variable(
    immunocompromised_risk_attribution,
    'Is the excess exposure risk borne by immunocompromised individuals actually caused by mandate absence, or by other factors (vaccine efficacy against transmission, variant evolution, behavioral adaptation)?',
    'Epidemiological counterfactual modeling: compare infection outcomes for immunocompromised populations in jurisdictions with vs without mandates, controlling for vaccine efficacy, NPIs, and variant waves. Natural experiments from mandate bans (e.g., Florida, Texas) vs mandate states.',
    'If mandate absence is not the primary driver of vulnerable risk, the victim declaration weakens and the extraction asymmetry decreases. If strongly causal, the snare/tangled_rope character intensifies.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(immunocompromised_risk_attribution, empirical, 'Causal attribution of vulnerable-population harm to mandate policy vs other variables.').

omega_variable(
    reading_foreclosure_boundary,
    'Does this reading logically foreclose the public_health_primacy_reading within a single legal framework, or do they coexist as competing frameworks held by different institutional coalitions?',
    'Doctrinal analysis: can a single constitutional doctrine simultaneously hold that (a) bodily autonomy is absolute against vaccination and (b) state police power justifies mandates for collective harm prevention? Test via Supreme Court opinion mapping — do any justices endorse both in different contexts?',
    'If forecloses, the kernel has a genuine logical fracture (rare). If coexists_with, the kernel is a standard distributed commitment system with competing readings. The reading_relations declaration uses coexists_with; this omega tests that judgment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_boundary, conceptual, 'Whether the absolutist and primacy readings are logically incompatible or politically competing.').

omega_variable(
    mandate_suppression_mechanism,
    'Is the suppression of mandate authority structural (court rulings, legislative bans) or internalized (political reluctance, institutional self-censorship)?',
    'Post-exit suppression trajectory: in jurisdictions where mandate bans were enacted and later repealed or enjoined, does mandate authority return immediately (structural) or remain suppressed (internalized)? Track legislative and executive behavior after legal constraints lift.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the prohibition carries itself forward even without active enforcement. This would increase the constraint''s persistence score and piton-likeness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_suppression_mechanism, empirical, 'Structural vs internalized suppression mechanism for mandate authority prohibition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vml_bap_tr_t0, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(vml_bap_tr_t3, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 3, 0.15).
narrative_ontology:measurement(vml_bap_tr_t6, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 6, 0.2).
narrative_ontology:measurement(vml_bap_tr_t9, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 9, 0.25).
narrative_ontology:measurement(vml_bap_tr_t12, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 12, 0.32).
narrative_ontology:measurement(vml_bap_tr_t14, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 14, 0.35).

% Extraction over time
narrative_ontology:measurement(vml_bap_be_t0, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(vml_bap_be_t3, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 3, 0.35).
narrative_ontology:measurement(vml_bap_be_t6, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 6, 0.42).
narrative_ontology:measurement(vml_bap_be_t9, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 9, 0.48).
narrative_ontology:measurement(vml_bap_be_t12, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 12, 0.85).
narrative_ontology:measurement(vml_bap_be_t14, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 14, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(vml_bap_su_t0, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(vml_bap_su_t3, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 3, 0.25).
narrative_ontology:measurement(vml_bap_su_t6, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 6, 0.3).
narrative_ontology:measurement(vml_bap_su_t9, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 9, 0.38).
narrative_ontology:measurement(vml_bap_su_t12, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 12, 0.92).
narrative_ontology:measurement(vml_bap_su_t14, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 14, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_mandate_legitimacy__risk_stratification_reading).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_mandate_legitimacy__public_health_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the 'vaccine mandate legitimacy' label into three structurally distinct readings with different ε, beneficiaries, and victims. The bodily_autonomy_primacy_reading claims mountain status with high ε (0.82); public_health_primacy_reading would claim rope/tangled_rope with lower ε; risk_stratification_reading claims scaffold with sunset logic. Linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
