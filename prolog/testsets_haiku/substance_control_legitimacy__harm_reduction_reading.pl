% ============================================================================
% CONSTRAINT STORY: substance_control_legitimacy__harm_reduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_legitimacy__harm_reduction_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: substance_control_legitimacy__harm_reduction_reading
 *   human_readable: Harm Reduction Public Health Authority
 *   domain: public health / criminal justice / political economy
 *
 * SUMMARY:
 *   The harm reduction reading of substance control legitimacy frames drug
 *   use as a public health issue requiring medical intervention rather than
 *   criminal punishment. State authority is derived from a duty to minimize
 *   harm: overdose mortality, disease transmission, and incarceration costs.
 *   The constraint coordinates treatment access, harm reduction supply
 *   chains, and medical supervision while extracting compliance costs from
 *   users through mandates and monitoring. It exists in contest with two
 *   sibling readings: legalization (which grants autonomy over substance use
 *   and limits state authority to third-party harm) and prohibition (which
 *   treats substance use as inherently immoral and derives authority from
 *   duty to prevent it through criminalization). The harm reduction reading
 *   is distinct because it medicalizes use while maintaining state control of
 *   the terms of legitimacy. Extraction arises not from criminalization but
 *   from the imposition of a medicalization framework that users may not have
 *   chosen, coupled with selective access to treatment (treatment slots
 *   limited by public funding, compliance requirements gate access, informal
 *   supply alternatives are suppressed). The claim of tangled_rope reflects
 *   genuine coordination (treatment access improved, overdose kits
 *   distributed, disease surveillance improved) AND asymmetric extraction
 *   (users bear the cost of medicalization, informal suppliers are targeted,
 *   treatment becomes a condition of social legitimacy). The reading is
 *   contested: prohibition advocates claim harm reduction enables addiction;
 *   legalization advocates claim it preserves unnecessary state control.
 *
 * KEY AGENTS:
 *   - public_health_authorities: institutional agenda-setter, derives authority from epidemiological evidence and public health mandate
 *   - substance_users_in_treatment_mandate: powerless payers/beneficiaries, trapped between treatment access (contingent on compliance) and informal autonomy (legally precarious)
 *   - treatment_infrastructure_providers: organized beneficiaries, economically dependent on sustained demand and public funding
 *   - informal_economy_participants: powerless payers, displaced by official harm reduction supply
 *   - communities_with_enforcement_density: organized payers, experience treatment as external imposition rather than community integration
 *   - criminal_justice_system_actors: excluded institutional actors, structurally delegitimized by the harm reduction frame
 *   - observer_epidemiologist: analytical seat, measures outcomes against alternatives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__harm_reduction_reading, 0.52).
domain_priors:suppression_score(substance_control_legitimacy__harm_reduction_reading, 0.41).
domain_priors:theater_ratio(substance_control_legitimacy__harm_reduction_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, resistance, 0.56).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_legitimacy__harm_reduction_reading, "Harm Reduction Public Health Authority").
narrative_ontology:topic_domain(substance_control_legitimacy__harm_reduction_reading, "public health / criminal justice / political economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__harm_reduction_reading, '9b775e76-f1f1-41cc-b0d3-9d87e59c177c').
narrative_ontology:cs_kernel_codification('9b775e76-f1f1-41cc-b0d3-9d87e59c177c', fixed_text).
narrative_ontology:cs_authority_grounding('9b775e76-f1f1-41cc-b0d3-9d87e59c177c', extraction).
narrative_ontology:cs_interpretation_layer_present('9b775e76-f1f1-41cc-b0d3-9d87e59c177c').
narrative_ontology:cs_reading_relation('9b775e76-f1f1-41cc-b0d3-9d87e59c177c', substance_control_legitimacy__prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('9b775e76-f1f1-41cc-b0d3-9d87e59c177c', substance_control_legitimacy__legalization_reading, influences).
narrative_ontology:cs_axiom('9b775e76-f1f1-41cc-b0d3-9d87e59c177c', foundational, substance_use_is_medical_disorder).
narrative_ontology:cs_axiom_status(substance_use_is_medical_disorder, holdable).
narrative_ontology:cs_axiom_grounding('9b775e76-f1f1-41cc-b0d3-9d87e59c177c', substance_use_is_medical_disorder, empirically_contingent).
narrative_ontology:cs_axiom('9b775e76-f1f1-41cc-b0d3-9d87e59c177c', foundational, state_authority_derived_from_harm_minimization_duty).
narrative_ontology:cs_axiom_status(state_authority_derived_from_harm_minimization_duty, holdable).
narrative_ontology:cs_axiom_grounding('9b775e76-f1f1-41cc-b0d3-9d87e59c177c', state_authority_derived_from_harm_minimization_duty, deontological).
narrative_ontology:cs_axiom('9b775e76-f1f1-41cc-b0d3-9d87e59c177c', secondary, criminalization_is_counterproductive_to_harm_reduction).
narrative_ontology:cs_axiom_status(criminalization_is_counterproductive_to_harm_reduction, holdable).
narrative_ontology:cs_axiom_grounding('9b775e76-f1f1-41cc-b0d3-9d87e59c177c', criminalization_is_counterproductive_to_harm_reduction, empirically_contingent).
narrative_ontology:cs_reference_frame('9b775e76-f1f1-41cc-b0d3-9d87e59c177c', epidemiologically_informed_harm_reduction_authority).
narrative_ontology:cs_drift_state('9b775e76-f1f1-41cc-b0d3-9d87e59c177c', contemporary_post_opioid_crisis_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9b775e76-f1f1-41cc-b0d3-9d87e59c177c', '').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__harm_reduction_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, public_health_authorities).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, treatment_infrastructure_providers).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, harm_reduction_advocates).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, substance_users_in_treatment_mandate).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, informal_economy_participants).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, communities_with_enforcement_density).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__harm_reduction_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(substance_control_legitimacy__harm_reduction_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_legitimacy__harm_reduction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_legitimacy__harm_reduction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_legitimacy__harm_reduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52 at interval end, rising from 0.35 at t0) because the medicalization frame imposes real compliance costs on users (surveillance, mandatory treatment milestones, medication regimes, counseling sessions, periodic testing) that go beyond the genuine harm reduction benefit they receive. The rise over the interval reflects the tightening of treatment gate conditions and the normalization of medical monitoring as users become enrolled in systems. Suppression is moderate (0.41) because users retain some alternatives (informal use remains available, though legally precarious), but the suppression is rising as informal supply chains are disrupted by harm reduction initiatives and as incarceration risk remains a latent enforcement mechanism for non-compliance. Theater is rising (0.22 to 0.38) because an increasing share of enforcement activity goes to monitoring and compliance-checking rather than actual overdose response or disease prevention. The accessibility collapse (0.48) is below the mountain threshold because alternatives to medicalized treatment remain available, though costly and risky; resistance (0.56) is moderate-high because users who privilege autonomy and informal supply communities actively resist the medicalization frame. The measurements are authored on a shared grid so every metric is authored at every time point. The temporal pattern reflects the lifecycle of the harm reduction frame: early rapid rise as infrastructure scales up and treatment mandates tighten, then plateau as the system stabilizes and users adapt.
 *
 * PERSPECTIVAL GAP:
 *   The public health authorities and the treatment infrastructure providers should compute as cooperatively benefiting from the constraint; the substance users in treatment mandate should compute as moderately extractive-paying seats despite the coordination benefit, because the extraction is the cost of accessing the benefit (gated access, monitoring, medicalization). The informal economy participants compute as pure targets with no offsetting benefit. Communities with enforcement density should compute as somewhat extractive depending on whether their local legitimacy structures accept or resist medical authority. The criminal justice exclusion is the reading-specific characteristic: in the prohibition reading, law enforcement is the agenda-setter; in the legalization reading, criminal justice is explicitly delegitimized as an enforcement mechanism; in the harm reduction reading, it is both delegitimized AND remains as a latent suppressive force (users can still be prosecuted for non-compliance, distribution, or informal use).
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities have low directionality (0.15–0.25): they benefit from the constraint by deriving authority, setting terms, and allocating resources. Treatment providers have similar low d (0.20–0.30): they collect rents from the treatment economy. Substance users have high directionality (0.65–0.75): they bear the extraction (compliance, surveillance, medicalization) even though they also benefit from harm reduction. This asymmetry is the tangled_rope signature. Informal suppliers have maximum d (0.85): pure targets, no coordination benefit. Criminal justice actors are excluded from the authority structure, so their d is derived from their latent enforcement role: they remain powerful enough to suppress deviation but are delegitimized, putting them in an ambiguous structural position—perhaps d = 0.70 (still enforcement power, but residual and contested). The derivation respects beneficiary/victim declarations and the identity_locked exit option for users (the medicalization frame becomes identity-fused once treatment is initiated; exit means losing legitimacy, social services, and medical access).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint has not (yet) experienced mandatrophy in the harm reduction reading, but it carries the risk of it. The founding problem (overdose mortality, disease transmission from criminalization) remains live, and the harm reduction frame directly addresses it. However, if alternative treatments or social structures were to emerge that reduced overdose and disease without medicalization, the treatment infrastructure might persist through inertia even as its founding mandate eroded. The measurement of theater_ratio rising from 0.22 to 0.38 is a signal of incipient mandatrophy: if the theater continues to rise toward 0.5+, it would indicate that compliance monitoring and surveillance have become decoupled from actual harm reduction outcomes. The legalization reading provides a structural threat to the mandate: if autonomy-based substance policy were adopted, the medical authority structure would lose its foundational justification. The present reading maintains mandate vitality by the empirical claim that medicalization reduces harm better than criminalization; the empirical contestability (prohibition advocates claim it enables addiction, some public health researchers question long-term outcomes of medication-assisted therapy compared to abstinence) is captured in the omegas rather than triggering mandatrophy classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    medicalization_as_internalized_suppression,
    'To what extent is the suppression and compliance burden imposed by the medicalization framework structural (external monitoring, treatment slot scarcity, legal risk for non-compliance) versus internalized (users have absorbed the medicalized identity and no longer experience exit as a real option)?',
    'Post-exit follow-up: if substance users who exit the formal treatment system report that suppression and self-monitoring persist (internalized) versus disappear (structural), the mechanism is revealed. Comparative jurisdictions with legalization could also show whether exit-stage suppression remains after legal criminalization is removed.',
    'If suppression is primarily internalized, the effective extraction is higher than the structural measure suggests, and the constraint''s persistence depends on identity-fusion rather than enforcement capacity. This would move the classification toward snare (persistent extraction through internalized control) rather than tangled_rope (coordination with asymmetric extraction). If structural, the suppression is likely sustainable only as long as enforcement capacity is maintained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medicalization_as_internalized_suppression, empirical, 'Structural versus internalized suppression mechanism in medicalization framework').

omega_variable(
    founding_problem_empirical_contestability,
    'Is harm reduction empirically more effective than prohibition at reducing overdose mortality and disease transmission, and how does this compare to legalization-framework outcomes where available?',
    'Meta-analysis of outcome data from jurisdictions with different substance policy regimes: harm reduction (Portugal, Swiss Zurich, U.S. harm reduction cities), prohibition (traditional criminalization regimes), legalization (limited data; some U.S. cannabis legalization metrics available). Compare overdose mortality, disease incidence, incarceration rates, and quality-of-life measures.',
    'If harm reduction empirically outperforms both prohibition and legalization on the public health metrics it claims to optimize, the founding problem remains live and the constraint retains mandate vitality. If legalization produces superior outcomes (lower overdose mortality, higher quality of life) while avoiding medicalization costs, the constraint would face mandatrophy risk: the problem is solved by a sibling reading, not by this one. If prohibition produces comparable outcomes with lower surveillance burden, the empirical foundation for this reading''s authority weakens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_empirical_contestability, empirical, 'Comparative effectiveness of harm reduction versus sibling policy readings').

omega_variable(
    informal_supply_replacement_by_fentanyl,
    'Does harm reduction suppress informal supply chains, or does it create a market niche into which high-potency drugs (fentanyl, xylazine) move, enabling more dangerous informal use?',
    'Time-series analysis of drug-market composition before and after harm reduction implementation: track the share of pharmaceutical (prescribed methadone, buprenorphine) versus illicit supply; track the potency distribution of illicit drugs; conduct supply-chain ethnography to determine whether harm reduction supply channels (sterile equipment, naloxone, supervised consumption sites) substitute for or coexist with illicit supply.',
    'If harm reduction substitutes for informal supply, the suppression is working and the extraction cost is primarily the medicalization burden. If informal supply adapts toward more dangerous products, the harm reduction frame may be misattributing harm causation (treating medicalization access as the harm-reduction input when the real input is the prevention of criminalization-driven supply escalation). This would affect the theoretical coherence of the constraint and suggest the sibling legalization reading might prevent the escalation better.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informal_supply_replacement_by_fentanyl, empirical, 'Whether harm reduction suppresses or displaces informal drug markets').

omega_variable(
    community_legitimacy_of_external_authority,
    'In communities with high historical criminalization and enforcement density, does the medical authority structure gain legitimacy and consent, or is it experienced as replacement colonialism—different authority, same external imposition?',
    'Community ethnography and surveys in jurisdictions with different substance policy histories: measure whether medical authority is experienced as legitimate, trusted, and helpful versus experienced as another form of state control targeting the same communities. Distinguish between explicit policy adoption (harm reduction framework) and community acceptance of the authority structure.',
    'If medical authority gains legitimacy, the constraint''s extraction is lower because it is accepted voluntarily. If medical authority is experienced as replacement colonialism, the extraction is higher because it maintains external control while changing only the form of intervention. This would affect whether the constraint is sustainable and whether legalization or indigenous-authority-based models might be more effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(community_legitimacy_of_external_authority, empirical, 'Whether medicalization gains community legitimacy or is experienced as replacement colonialism').

omega_variable(
    autonomous_substance_use_outside_medicalization,
    'Is there a significant population whose substance use is self-managed, low-harm (occasional, moderate dose, integrated with other life domains) and for whom the medicalization framework is misapplied?',
    'Population surveys of substance use patterns: estimate the fraction of users who meet criteria for disorder (compulsive use, harm, loss of control) versus casual/recreational use. If a large fraction is non-disordered, this reading''s application of medical authority might be overbroad.',
    'If most substance use is disordered and harmful, medicalization is correctly targeted. If a large fraction is autonomous, the extraction is higher because the medicalization frame is imposed on populations who do not need it, and the constraint becomes more snare-like (medicalization as suppression of autonomous choice, not just treatment access). This would strengthen the legalization reading''s case that state authority should be limited to third-party harm rather than applied universally.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(autonomous_substance_use_outside_medicalization, empirical, 'Proportion of substance use that is autonomous versus disorder-level').

omega_variable(
    constraint_reading_contest_mechanism,
    'This constraint is one reading of a contested kernel (substance_control_legitimacy). The contest between harm_reduction, prohibition, and legalization readings depends on deep disagreements about whether substance use is fundamentally a moral failing (prohibition), a medical condition (harm reduction), or an autonomy right (legalization). Which of these readings, if empirically validated or defeated, would actually change political support for the constraint?',
    'Policy history analysis: track whether empirical evidence (e.g., Portuguese harm reduction outcomes) actually shifted support, or whether support tracks other axes (cost, political coalition composition, international pressure). Track whether evidence against prohibition (e.g., high incarceration without reduced use) shifts advocates away from prohibition or whether they double down. Test whether actual legalization experience (cannabis legalization outcomes) shifts support for legalization reading.',
    'If empirical evidence can shift the reading in use, the constraint is contestable and mandatrophy-vulnerable. If readings are sticky regardless of evidence (political coalition constraints, identity-fusion, ideological commitment), the constraint is more stable but its empirical claim is decoupled from its persistence. This affects whether the constraint should be analyzed as an empirical question (and thus subject to refutation) or as a political choice masked as empirical.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(constraint_reading_contest_mechanism, conceptual, 'Whether substance control reading is empirically contestable or politically determinate').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__harm_reduction_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(subs_tr_t5, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(subs_tr_t10, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement(subs_tr_t15, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement(subs_tr_t20, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(subs_tr_t25, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 25, 0.38).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(subs_be_t5, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(subs_be_t10, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(subs_be_t15, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 15, 0.51).
narrative_ontology:measurement(subs_be_t20, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(subs_be_t25, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 25, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(subs_su_t5, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 5, 0.33).
narrative_ontology:measurement(subs_su_t10, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 10, 0.37).
narrative_ontology:measurement(subs_su_t15, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 15, 0.4).
narrative_ontology:measurement(subs_su_t20, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 20, 0.41).
narrative_ontology:measurement(subs_su_t25, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 25, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__harm_reduction_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(substance_control_legitimacy__harm_reduction_reading, 0.12).
narrative_ontology:affects_constraint(substance_control_legitimacy__harm_reduction_reading, substance_control_legitimacy__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__harm_reduction_reading, substance_control_legitimacy__legalization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel substance_control_legitimacy. The kernel is the state's authority to regulate substance use. Three readings decompose the kernel based on different foundational axioms: (1) harm_reduction_reading (this file) — state authority derives from duty to minimize harm via medicalization; (2) prohibition_reading — state authority derives from moral duty to prevent use via criminalization; (3) legalization_reading — state authority limited to preventing third-party harm; adults have autonomy. Each reading has a distinct ε-value: harm reduction has moderate extractiveness (0.52) because medicalization imposes compliance costs; prohibition has high extractiveness (0.75+) because criminalization extracts through incarceration and legal penalties; legalization has low extractiveness (0.20–0.30) because autonomy requires minimal state extraction beyond enforcement of third-party-harm boundaries. The ε-invariance principle requires separate constraints rather than one constraint with measurement-parameter freedom. Sibling readings are linked via network.affects_constraints. The shared founding problem (substance use harms) is instantiated differently in each reading, and the effectiveness contest between them is empirical (captured in omegas).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_legitimacy__harm_reduction_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
