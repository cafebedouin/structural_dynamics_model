% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_balance__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_balance__bodily_autonomy_primary, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: vaccine_mandate_balance__bodily_autonomy_primary
 *   human_readable: Bodily Autonomy Primary: State Cannot Compel Medical Intervention
 *   domain: public_health_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint instantiates the bodily-autonomy-primary reading of the
 *   vaccine-mandate-balance kernel. The reading asserts that individual
 *   consent is inviolable and that the state cannot compel medical
 *   intervention regardless of collective benefit calculations or
 *   epidemiological severity. Under this reading, unvaccinated individuals
 *   who refuse mandates are victims of coercion; the constraint is
 *   snare-shaped because it persists through enforcement (employment loss,
 *   school exclusion, travel restrictions) and suppresses alternatives
 *   (refusing the injection means accepting material loss, not exiting the
 *   constraint). The vulnerably-positioned immunocompromised are NOT
 *   automatically victims of mandate-rejection under this reading—they are
 *   treated as accepting risk as part of living in a society where bodily
 *   autonomy is supreme. This reading forecloses the public_health_primary
 *   reading (which treats collective immunity as overriding) and coexists
 *   with the proportionality_reading (which attempts to mediate by setting
 *   high thresholds for mandate justification).
 *
 * KEY AGENTS:
 *   - unvaccinated_coerced_individuals: Powerless agents bearing direct coercive costs (employment, education, travel loss) when they refuse vaccination. Their exit is not to escape the constraint but to accept material deprivation.
 *   - medical_freedom_dissenters: Identity-locked movement actors defending bodily autonomy on principle. Their opposition persists regardless of epidemiological evidence; liberty-fusion makes biological facts secondary to the principle.
 *   - public_health_authorities: Institutional agenda-setters who design and enforce mandates. They frame coercion as proportional emergency response; they control the enforcement machinery and benefit from state authority expansion.
 *   - immunocompromised_vulnerable: Structurally dependent on population immunity but—under bodily-autonomy-primary framing—NOT positioned as victims when mandates are rejected, because accepting risk is intrinsic to respecting others' bodily integrity.
 *   - vaccine_manufacturers: Beneficiaries capturing guaranteed demand and liability shields from mandates. They collect rents from coercive uptake.
 *   - medical_ethics_bodies: Analytical observers documenting the tensions but unable to enforce policy constraints.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__bodily_autonomy_primary, 0.82).
domain_priors:suppression_score(vaccine_mandate_balance__bodily_autonomy_primary, 0.78).
domain_priors:theater_ratio(vaccine_mandate_balance__bodily_autonomy_primary, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, extractiveness, 0.82).
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__bodily_autonomy_primary, snare).
narrative_ontology:human_readable(vaccine_mandate_balance__bodily_autonomy_primary, "Bodily Autonomy Primary: State Cannot Compel Medical Intervention").
narrative_ontology:topic_domain(vaccine_mandate_balance__bodily_autonomy_primary, "public_health_ethics/constitutional_law").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__bodily_autonomy_primary, '20b28b23-103f-43e4-84cc-8f5de7065b8d').
narrative_ontology:cs_kernel_codification('20b28b23-103f-43e4-84cc-8f5de7065b8d', formalized).
narrative_ontology:cs_authority_grounding('20b28b23-103f-43e4-84cc-8f5de7065b8d', lineage).
narrative_ontology:cs_interpretation_layer_present('20b28b23-103f-43e4-84cc-8f5de7065b8d').
narrative_ontology:cs_reading_relation('20b28b23-103f-43e4-84cc-8f5de7065b8d', vaccine_mandate_balance__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('20b28b23-103f-43e4-84cc-8f5de7065b8d', vaccine_mandate_balance__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('20b28b23-103f-43e4-84cc-8f5de7065b8d', foundational, bodily_integrity_inviolable).
narrative_ontology:cs_axiom_status(bodily_integrity_inviolable, holdable).
narrative_ontology:cs_axiom_grounding('20b28b23-103f-43e4-84cc-8f5de7065b8d', bodily_integrity_inviolable, deontological).
narrative_ontology:cs_axiom('20b28b23-103f-43e4-84cc-8f5de7065b8d', foundational, state_authority_ends_at_body).
narrative_ontology:cs_axiom_status(state_authority_ends_at_body, holdable).
narrative_ontology:cs_axiom_grounding('20b28b23-103f-43e4-84cc-8f5de7065b8d', state_authority_ends_at_body, deontological).
narrative_ontology:cs_reference_frame('20b28b23-103f-43e4-84cc-8f5de7065b8d', bodily_integrity_inviolable).
narrative_ontology:cs_drift_state('20b28b23-103f-43e4-84cc-8f5de7065b8d', contemporary_pandemic_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('20b28b23-103f-43e4-84cc-8f5de7065b8d', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_victim(vaccine_mandate_balance__bodily_autonomy_primary, unvaccinated_coerced_individuals).
narrative_ontology:constraint_victim(vaccine_mandate_balance__bodily_autonomy_primary, medical_freedom_dissenters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__bodily_autonomy_primary, immunocompromised_vulnerable_populations).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_manufacturers).
narrative_ontology:constraint_vindicates(vaccine_mandate_balance__bodily_autonomy_primary, bodily_integrity_doctrine).
narrative_ontology:constraint_vindicates(vaccine_mandate_balance__bodily_autonomy_primary, informed_consent_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face employment loss, school exclusion, travel restrictions, and social stigma if they do not submit to vaccination mandates. Many cite conscience, religious belief, prior infection, or distrust of regulatory processes. Their refusal carries material consequences (job loss, educational access denial) enforced by institutional rules they did not consent to. Exit means accepting the loss, not refusing the injection.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, unvaccinated_coerced_individuals, payer,
    powerless, biographical, trapped, national).

% Comprise a vocal political movement centered on the principle that bodily autonomy is non-negotiable. They object to mandates on principle even when the disease or vaccine profile might support voluntary uptake. Their identity as defenders of liberty and bodily integrity is fused with their vaccine stance; they would remain opposed to coercion mechanisms even if efficacy and safety were scientifically uncontested.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, medical_freedom_dissenters, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_balance__bodily_autonomy_primary, medical_freedom_dissenters, excluded).

% Design and enforce vaccination mandate policies during epidemiological emergencies. They justify mandates as necessary to achieve population immunity thresholds that protect those who cannot be vaccinated. They administer the rules, select exemption criteria, and enforce consequences. They frame mandates as proportional responses to existential threat.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, public_health_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Depend on collective immunity to avoid lethal infection. They cannot be vaccinated themselves or vaccination provides minimal protection. Under this reading, they do NOT become victims if infection risk increases when mandates are rejected—risk acceptance is treated as intrinsic to the liberty principle. However, they benefit structurally from high population immunity when it exists.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, immunocompromised_vulnerable_populations, beneficiary,
    powerless, biographical, trapped, national).

% Capture substantial revenue and liability protection from mandated uptake. Mandates guarantee demand and reduce market uncertainty. They benefit from enforcement mechanisms that compel purchase and shield them from post-hoc liability claims arising from rare adverse events.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_manufacturers, beneficiary,
    powerful, biographical, arbitrage, global).

% Document and deliberate the ethical tensions between individual autonomy and collective protection. They produce statements on proportionality and informed consent but cannot enforce policy. They witness the constraint's operation from an analytical seat.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, medical_ethics_bodies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_balance__bodily_autonomy_primary, public_health_authorities).
narrative_ontology:fixing_cost_class(vaccine_mandate_balance__bodily_autonomy_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: At the collective level, mandates attempt to solve free-riding: without coercion, voluntary uptake may fall short of herd immunity, leaving vulnerable populations exposed. From the bodily-autonomy-primary reading, this framing misconstrues the problem—the real coordination problem is how to protect vulnerable populations WITHOUT violating bodily integrity (e.g., through focused protection, resource investment in treatment, optional vaccination with transparent data). The mandate solves the public health problem by eliminating consent, not by solving a genuine coordination failure.
% TRANSFER_FUNCTION: Transfers bodily autonomy, medical decision-making authority, and personal risk assessment from individuals to state agents. Individuals lose the right to refuse; the state gains the authority to override refusal via employment, educational, and social sanctions. Vaccine manufacturers transfer liability and demand risk to the public authority and gain guaranteed market access.
% ABSENT_VOICES: Alternative public health strategies (focused protection, treatment infrastructure, transparent risk communication without coercion) are not represented in the mandate-or-nothing framing. Individuals from communities with historical medical mistrust, past harm from coercive medical experiments, or prior infection/natural immunity are structurally excluded from the conversation about exemption grounds—their objections are treated as ignorance rather than as legitimate dissent.
% DISAPPEARANCE_RATIONALE: If bodily-autonomy-primary constraint disappeared—if coercive mandates were legally prohibited—public health authorities would shift to voluntary communication, risk stratification, resource allocation to treatment and protection, and resource-intensive outreach to high-risk populations. Vulnerability protection would require different mechanisms (targeted support, treatment availability, voluntary vaccination with transparent incentives). The institutional authority to compel medical intervention would evaporate, forcing reorganization around consent-based strategies.
% FOUNDING_PROBLEM: The constraint asserts that bodily integrity is a foundational principle that cannot be overridden by collective benefit calculations, regardless of epidemiological severity. The founding problem is not a medical fact—it is a constitutional and ethical principle: the state's legitimate authority ends at the boundary of the body. Mandates arise when voluntary uptake falls short of public health targets, but the constraint rejects the premise that collective targets override individual refusal.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars and medical ethicists working in autonomy-centered traditions (from sources outside the mandate-benefiting authorities) attest that bodily integrity is a foundational principle with historical weight. However, competing ethical frameworks (consequentialist, utilitarian, public-health-centered) contest whether autonomy is absolute or whether it admits proportionate limits. Legislative-hearing testimony from disability advocates, informed-consent organizations, and bodily-autonomy theorists support the principle; public-health officials contest it by appeal to emergency and collective protection. No neutral external authority definitively resolves which principle takes precedence.
narrative_ontology:disappearance_verdict(vaccine_mandate_balance__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_balance__bodily_autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__bodily_autonomy_primary, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(vaccine_mandate_balance__bodily_autonomy_primary, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_balance__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_balance__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_balance__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the constraint operates through coercive mechanisms that override individual choice: employment termination, educational exclusion, travel restrictions. These are not soft incentives—they carry material loss. The constraint persists despite substantial resistance (0.71) from vaccine-hesitant populations and medical-freedom movements, indicating suppression is active. Theater is low (0.22) at interval end: the public-health rationale for mandates exists, but the coercive enforcement machinery—the core of what keeps mandates in place—is not disguised as something else. Accessibility_collapse is moderate (0.68): alternatives (refusing the mandate, accepting the consequences) do exist, but they are costly; the collapse measures how far the choice set has shrunk, not whether it disappeared entirely. The measurement series shows extractiveness rising steeply in the early interval (0.68 to 0.76 in the first 12 time units) as mandates are implemented and enforcement machinery hardens, then flattening (0.76 to 0.82 by t=24), indicating the coercive infrastructure stabilizes. Theater rises more slowly and plateaus earlier (0.08 to 0.22 by t=30), suggesting public justification is offered upfront but enforcement then runs on its own institutional momentum. Suppression_requirement mirrors extractiveness, indicating enforcement must remain active to hold the constraint.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (public_health_authorities) and the payer seats (unvaccinated_coerced_individuals, medical_freedom_dissenters) should compute to dramatically different types from their own positions. From the public-health seat, the mandate is a coordinated, proportional response to a collective threat—possibly rope or tangled_rope in their framing (genuine coordination problem, justified asymmetry). From the coerced-individual seat, it is pure extraction enforced through threats—snare from their position. The engine computes per-seat classification from the structural data; this divergence is the measurement the corpus takes. The bodily-autonomy-primary reading establishes a structural premise (autonomy is inviolable) that drives the snare classification globally, but individual seats will perceive different types based on their power, exit options, and beneficiary/victim status.
 *
 * DIRECTIONALITY LOGIC:
 *   Unvaccinated_coerced_individuals and medical_freedom_dissenters are full targets (d near 1.0): they bear the costs of refusal (employment loss, education denial) without collecting benefits; they are powerless or identity-locked in exit (refusing the injection means accepting loss). Public_health_authorities are beneficiaries (d near 0.0): they expand state authority, justify emergency powers, and face minimal personal cost. Vaccine_manufacturers are beneficiaries (d toward 0.0): they capture demand and liability shields. Immunocompromised_vulnerable sit in an analytically complex position under this reading: they benefit from population immunity but are NOT treated as having suffered a loss when mandates are rejected because respecting bodily autonomy is sovereign over their protection interest. The constraint's structural claim is that autonomy overrides vulnerability protection, which inverts the vulnerable from beneficiary to accepting-residual-risk.
 *
 * MANDATROPHY ANALYSIS:
 *   The bodily-autonomy-primary reading prevents misclassifying the constraint as pure coordination or as a legitimate proportional response. If the constraint were classified as rope or tangled_rope, it would suggest the mandate solves a genuine coordination problem that voluntary mechanisms cannot. Under the autonomy-primary reading, the framing is rejected: the real problem is how to protect vulnerable populations without violating bodily integrity, and mandates solve the public-health problem by eliminating the autonomy problem, not by solving coordination. This classification forces the choice between the readings into the open: is autonomy absolute or does it admit limits? The constraint's snare classification under this reading is not a mathematical artifact—it reflects the reading's core premise that coercion on this object (medical intervention) is illegitimate regardless of outcomes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_absolutism_vs_qualified,
    'Is bodily autonomy an absolute principle that cannot be overridden, or can it be overridden only under strict proportionality conditions (disease severity, vaccine safety, vulnerable-population risk)?',
    'Legal precedent examination (Jacobson v. Massachusetts, more recent jurisprudence on medical autonomy) and philosophical analysis of whether autonomy-based rights have internal limits or external overrides. Jurisdictional variation provides a natural experiment: compare outcomes in jurisdictions with absolute autonomy protections vs. those with proportionality-gated mandates.',
    'If autonomy is absolute, this reading''s snare classification holds globally; if autonomy admits proportional limits, the constraint''s type becomes reading-dependent (snare under bodily_autonomy_primary, tangled_rope or rope under proportionality_reading). The classification depends on which reading is true.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_absolutism_vs_qualified, conceptual, 'Whether bodily autonomy is inviolable or subject to proportionate override.').

omega_variable(
    coercion_vs_incentive_boundary,
    'At what point do employment and educational exclusions become coercive (inviolable-autonomy violation) vs. legitimate institutional rules that do not coerce but exclude on policy grounds?',
    'Philosophical analysis of coercion (threat-based vs. institutional exclusion), legal doctrine on constitutional limits to state action, comparative study of how different jurisdictions treat employment-condition vs. direct-medical-coercion.',
    'A narrow coercion definition (direct medical force only) would lower the extractiveness of employment-based mandates and could shift classification toward rope or tangled_rope. A broad coercion definition (including institutional exclusions leveraging dependence) supports snare classification by treating employment/education loss as coercive threat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_vs_incentive_boundary, conceptual, 'Whether institutional exclusions count as coercion or legitimate conditionality.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the suppression of vaccine-hesitant dissent structural (institutional rules enforcing mandates) or internalized (social stigma, professional identity-fusion with acceptance)?',
    'Post-mandate trajectory analysis: if suppression persists after legal mandates are lifted, it indicates internalized suppression (identity fusion, trust loss); if suppression collapses when mandates end, it indicates structural suppression. Survey and longitudinal studies of vaccine hesitancy persistence.',
    'If internalized, the actual suppression on vaccine-hesitant populations exceeds the structural measure (0.78); targets carry suppression with them after mandate removal. If structural, the suppression is coterminous with institutional enforcement. Internalized suppression implies deeper constraint roots and higher victim identification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether suppression is enforced institutionally or internalized through identity and trust damage.').

omega_variable(
    vulnerable_population_framing_dependency,
    'Does the claim that immunocompromised populations benefit from mandates depend on reading vulnerability as a collective responsibility (overriding autonomy) vs. reading vulnerability as a risk that individuals accept in an autonomy-respecting society?',
    'Philosophical argument: are vulnerable populations'' protection interests grounds for overriding others'' autonomy (public_health_primary reading), or is their protection a separate problem to be solved without coercion (bodily_autonomy_primary reading)? Care-ethics frameworks vs. autonomy-centered frameworks will produce different answers.',
    'Under this reading (bodily_autonomy_primary), vulnerability does NOT activate a duty to override others'' autonomy; vulnerables are not harmed by mandate-rejection because they live in a society respecting autonomy. Under public_health_primary reading, vulnerability creates a duty to compel immunity. The classification difference is reading-dependent, not fact-dependent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vulnerable_population_framing_dependency, conceptual, 'Whether vulnerable populations'' protection grounds autonomy override or represents a separate policy problem.').

omega_variable(
    kernel_reading_contest_unresolved,
    'Which reading of the vaccine-mandate-balance kernel is true: bodily_autonomy_primary (this one), public_health_primary (collective immunity overrides), or proportionality_reading (thresholds mediate)?',
    'This cannot be resolved within any single reading; it is the kernel contest itself. Resolution would require normative judgment about what principles should govern state authority over medical intervention—a constitutional and ethical question, not an empirical one.',
    'Each reading instantiates a different constraint with a different ε, different victim set, and different classification. The corpus registers all three; the contest lives in the readings'' structural divergence, not in one reading being ''right'' and others ''wrong''.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_unresolved, preference, 'The kernel contest: which reading of vaccine-mandate authority is legitimate?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__bodily_autonomy_primary, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(vacc_tr_t0, observed).
narrative_ontology:measurement(vacc_tr_t6, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 6, 0.11).
narrative_ontology:measurement_basis(vacc_tr_t6, observed).
narrative_ontology:measurement(vacc_tr_t12, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 12, 0.14).
narrative_ontology:measurement_basis(vacc_tr_t12, observed).
narrative_ontology:measurement(vacc_tr_t18, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 18, 0.17).
narrative_ontology:measurement_basis(vacc_tr_t18, observed).
narrative_ontology:measurement(vacc_tr_t24, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 24, 0.2).
narrative_ontology:measurement_basis(vacc_tr_t24, observed).
narrative_ontology:measurement(vacc_tr_t30, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 30, 0.22).
narrative_ontology:measurement_basis(vacc_tr_t30, observed).
narrative_ontology:measurement(vacc_tr_t36, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 36, 0.22).
narrative_ontology:measurement_basis(vacc_tr_t36, observed).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 0, 0.68).
narrative_ontology:measurement_basis(vacc_be_t0, observed).
narrative_ontology:measurement(vacc_be_t6, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 6, 0.72).
narrative_ontology:measurement_basis(vacc_be_t6, observed).
narrative_ontology:measurement(vacc_be_t12, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 12, 0.76).
narrative_ontology:measurement_basis(vacc_be_t12, observed).
narrative_ontology:measurement(vacc_be_t18, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 18, 0.79).
narrative_ontology:measurement_basis(vacc_be_t18, observed).
narrative_ontology:measurement(vacc_be_t24, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 24, 0.81).
narrative_ontology:measurement_basis(vacc_be_t24, observed).
narrative_ontology:measurement(vacc_be_t30, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 30, 0.82).
narrative_ontology:measurement_basis(vacc_be_t30, observed).
narrative_ontology:measurement(vacc_be_t36, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 36, 0.82).
narrative_ontology:measurement_basis(vacc_be_t36, observed).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 0, 0.64).
narrative_ontology:measurement_basis(vacc_su_t0, observed).
narrative_ontology:measurement(vacc_su_t6, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 6, 0.68).
narrative_ontology:measurement_basis(vacc_su_t6, observed).
narrative_ontology:measurement(vacc_su_t12, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 12, 0.72).
narrative_ontology:measurement_basis(vacc_su_t12, observed).
narrative_ontology:measurement(vacc_su_t18, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 18, 0.75).
narrative_ontology:measurement_basis(vacc_su_t18, observed).
narrative_ontology:measurement(vacc_su_t24, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 24, 0.77).
narrative_ontology:measurement_basis(vacc_su_t24, observed).
narrative_ontology:measurement(vacc_su_t30, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 30, 0.78).
narrative_ontology:measurement_basis(vacc_su_t30, observed).
narrative_ontology:measurement(vacc_su_t36, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 36, 0.78).
narrative_ontology:measurement_basis(vacc_su_t36, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_balance__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vaccine_mandate_balance__bodily_autonomy_primary, 0.12).
narrative_ontology:affects_constraint(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_mandate_balance__public_health_primary).
narrative_ontology:affects_constraint(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_mandate_balance__proportionality_reading).

% DUAL FORMULATION NOTE:
% The vaccine-mandate-balance kernel decomposes into three structurally distinct constraints: bodily_autonomy_primary (this constraint, snare-shaped, ε=0.82, autonomy is inviolable), public_health_primary (collective immunity overrides autonomy, rope/tangled_rope-shaped, ε varies with disease severity and uptake), and proportionality_reading (autonomy admits proportional limits, tangled_rope-shaped, ε depends on threshold-setting and exemption robustness). Each reading has different ε-invariant properties: victim sets differ (unvaccinated_coerced under autonomy_primary, immunocompromised_exposed under public_health_primary), beneficiary structures differ (authorities + manufacturers under autonomy_primary; broader population under public_health_primary), and foundational axioms differ (autonomy is absolute vs. autonomy is qualified). The readings coexist as live positions held by different political and ethical constituencies; no reading logically forecloses the others, but they imply radically different policies. All three are linked via network.affects_constraints because the contest over which reading governs shapes which constraint structure operates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vaccine_mandate_balance__bodily_autonomy_primary, powerless, 0.95).
constraint_indexing:directionality_override(vaccine_mandate_balance__bodily_autonomy_primary, moderate, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
