% ============================================================================
% CONSTRAINT STORY: public_health_mandate_authority__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: public_health_mandate_authority__public_health_primary
 *   human_readable: Public Health Mandate Authority (Public Health Primary Reading)
 *   domain: public_health/constitutional_law/bioethics
 *
 * SUMMARY:
 *   Under the public-health-primary reading, public health mandate authority
 *   is framed as an obligation to protect populations who cannot protect
 *   themselves against disease transmission — immunocompromised individuals,
 *   healthcare infrastructure, elderly and infirm. The mandate requires
 *   coordinated participation in preventive measures (vaccination,
 *   prophylaxis, quarantine) because voluntary uptake leaves vulnerable
 *   populations hostage to others' choices. The enforcement mechanism is
 *   high: employment loss, credential suspension, access denial. The reading
 *   treats those who refuse the mandate as free-riders imposing an
 *   externality on the vulnerable commons, not as victims of coercion. This
 *   is ONE READING of a contested kernel: bodily_autonomy_primary holds that
 *   no collective benefit justifies non-consensual medical intervention;
 *   proportionality_reading holds that legitimacy depends on sliding-scale
 *   analysis of threat, alternatives, coercion magnitude, and duration. This
 *   story generates the public-health-primary reading's structural data
 *   without hedging across the contest.
 *
 * KEY AGENTS:
 *   - Immunocompromised populations: powerless, trapped, depend on surrounding compliance; beneficiaries of mandate protection.
 *   - Healthcare system capacity: institutional beneficiary; surge prevention is the coordination function.
 *   - Mandate-resistant workers: moderate power, constrained exit (employment loss), bearing high enforcement costs.
 *   - Unvaccinated populations: framed as free-riders under this reading; identity-locked exit (coupled to ideological position on bodily sovereignty).
 *   - Public health authorities: institutional agenda-setter; delegated police powers; set mandate scope and exemptions.
 *   - Bodily autonomy and proportionality advocates: excluded from decision structure; their objections are pre-judged as insufficiently weighty by the reading's foundational axiom.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__public_health_primary, 0.68).
domain_priors:suppression_score(public_health_mandate_authority__public_health_primary, 0.72).
domain_priors:theater_ratio(public_health_mandate_authority__public_health_primary, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, resistance, 0.69).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__public_health_primary, tangled_rope).
narrative_ontology:human_readable(public_health_mandate_authority__public_health_primary, "Public Health Mandate Authority (Public Health Primary Reading)").
narrative_ontology:topic_domain(public_health_mandate_authority__public_health_primary, "public_health/constitutional_law/bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__public_health_primary, '8417cde4-f65c-4314-8ac6-c71274043a5e').
narrative_ontology:cs_kernel_codification('8417cde4-f65c-4314-8ac6-c71274043a5e', formalized).
narrative_ontology:cs_authority_grounding('8417cde4-f65c-4314-8ac6-c71274043a5e', lineage).
narrative_ontology:cs_interpretation_layer_present('8417cde4-f65c-4314-8ac6-c71274043a5e').
narrative_ontology:cs_reading_relation('8417cde4-f65c-4314-8ac6-c71274043a5e', public_health_mandate_authority__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('8417cde4-f65c-4314-8ac6-c71274043a5e', public_health_mandate_authority__proportionality_reading, influences).
narrative_ontology:cs_axiom('8417cde4-f65c-4314-8ac6-c71274043a5e', foundational, collective_welfare_obligates_medical_compliance).
narrative_ontology:cs_axiom_status(collective_welfare_obligates_medical_compliance, holdable).
narrative_ontology:cs_axiom_grounding('8417cde4-f65c-4314-8ac6-c71274043a5e', collective_welfare_obligates_medical_compliance, deontological).
narrative_ontology:cs_axiom('8417cde4-f65c-4314-8ac6-c71274043a5e', foundational, police_powers_authority_presumed_valid_absent_exigent_limits).
narrative_ontology:cs_axiom_status(police_powers_authority_presumed_valid_absent_exigent_limits, holdable).
narrative_ontology:cs_axiom_grounding('8417cde4-f65c-4314-8ac6-c71274043a5e', police_powers_authority_presumed_valid_absent_exigent_limits, conventional).
narrative_ontology:cs_reference_frame('8417cde4-f65c-4314-8ac6-c71274043a5e', police_powers_constitutional_emergency).
narrative_ontology:cs_drift_state('8417cde4-f65c-4314-8ac6-c71274043a5e', endemic_phase_post_acute_crisis, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8417cde4-f65c-4314-8ac6-c71274043a5e', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__public_health_primary, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, immunocompromised_populations).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, healthcare_system_capacity).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, elderly_and_infirm).
narrative_ontology:constraint_victim(public_health_mandate_authority__public_health_primary, mandate_resistant_workers).
narrative_ontology:constraint_victim(public_health_mandate_authority__public_health_primary, unvaccinated_excluded_populations).
narrative_ontology:constraint_victim(public_health_mandate_authority__public_health_primary, bodily_autonomy_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, healthcare_workers_and_staff).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, hospital_and_medical_infrastructure).
narrative_ontology:constraint_victim(public_health_mandate_authority__public_health_primary, healthcare_workers_and_staff).
narrative_ontology:constraint_victim(public_health_mandate_authority__public_health_primary, unvaccinated_populations).
narrative_ontology:constraint_vindicates(public_health_mandate_authority__public_health_primary, police_powers_doctrine).
narrative_ontology:constraint_vindicates(public_health_mandate_authority__public_health_primary, individual_sacrifice_for_collective_welfare).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Depend on surrounding compliance with vaccination and prophylaxis mandates for access to public spaces, healthcare, employment, and daily life. They cannot mount independent protection; their safety is contingent on others' mandated cooperation. Without collective mandates, they face effective confinement or high infection risk.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, immunocompromised_populations, beneficiary,
    powerless, biographical, trapped, national).

% Benefit from reduced workplace transmission and patient load surge when system-wide vaccination is mandated. They also bear enforcement costs when employers discharge unvaccinated colleagues and face workplace conflict. They are protected, but at the cost of administering or witnessing coercion.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, healthcare_workers_and_staff, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(public_health_mandate_authority__public_health_primary, healthcare_workers_and_staff, payer).

% System capacity is protected when population-level vaccination reduces peak patient surges. Mandates prevent the infrastructure collapse scenario that forces triage and resource rationing. Healthcare systems can maintain elective care and specialty services rather than being entirely consumed by surging acute cases.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, hospital_and_medical_infrastructure, beneficiary,
    institutional, generational, arbitrage, national).

% Face employment loss, credential suspension, and service access denial if they refuse the mandated medical intervention. Their options are: comply, lose livelihood and social participation, or litigate against state/employer enforcement. The cost of noncompliance is high and structurally asymmetric — they do not face fines, they face exclusion from entire sectors.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, mandate_resistant_workers, payer,
    moderate, biographical, constrained, national).

% Framed under this reading as free-riders imposing externality on the vulnerable commons. They bear the mandate's enforcement costs: denied access to employment, healthcare, education, and public assembly. This reading does not position them as victims of coercion but as agents whose refusal to contribute makes them liable to exclusion. Their exit is theoretically available but practically coupled to ideological identity regarding bodily sovereignty and medical skepticism.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, unvaccinated_populations, payer,
    powerless, biographical, identity_locked, national).

% Declare, enforce, and modify public health mandates. They operate under delegated police powers and constitutional emergency authority. They set the mandate's scope, duration, and exemptions, and decide which occupations and spaces fall under it. Their authority is constrained by constitutional limits and public tolerance, but within those bounds they exercise broad discretion.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, public_health_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Hold that mandatory medical intervention violates intrinsic bodily sovereignty regardless of public benefit. They are excluded from the decision-making structure under this reading because the public-health-primary framework pre-judges their objection as insufficiently weighty. Their voice would challenge the foundational axiom of the reading.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, bodily_autonomy_advocates, excluded,
    moderate, biographical, constrained, national).

% Argue that mandate legitimacy depends on proportional fit between threat severity, coercion magnitude, and duration. Under the public-health-primary reading, proportionality is subordinate to the protective obligation itself. They would frame the same mandate differently — as justified only if the narrowest means necessary — and are excluded from that framing.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, proportionality_advocates, excluded,
    moderate, biographical, constrained, national).

% Provide evidence on transmission rates, variant emergence, vaccine effectiveness, and breakthrough infection patterns. Their data informs the threshold question: does the commons face genuine threat requiring mandates? Under this reading they are analytical observers, not decision-makers; the decision is political and constitutional, not technical.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, empirical_epidemiologists, observer,
    institutional, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(public_health_mandate_authority__public_health_primary, hospital_and_medical_infrastructure).
narrative_ontology:fixing_cost_class(public_health_mandate_authority__public_health_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of protecting those who cannot protect themselves: when vaccination (or other mandated prevention) is voluntary and private-choice based, immunocompromised and vulnerable populations face exposure imposed by others' free-rider behavior. A mandate forces all actors into a coordinated prevention regime that is only stable if universal or near-universal.
% TRANSFER_FUNCTION: Moves bodily autonomy claims (from those who resist the mandate) to protective outcomes for vulnerable populations. The transfer is not monetary but affects freedom of movement, employment access, and medical decision-making authority. Those subject to mandates transfer control over certain health decisions to public health authorities; the beneficiary transfer flows to immunocompromised populations who gain access to shared spaces.
% ABSENT_VOICES: Bodily autonomy absolutists and proportionality-constrained frameworks are structurally excluded under this reading's terms — their voices would object to the foundational premise that collective welfare can obligate individual medical compliance. Legislative testimony and public comment reveal substantial populations holding these views, but they are not seated at the decision point. Also absent: communities with low vaccine confidence based on historical medical racism and government medical coercion, whose experience-grounded skepticism is overridden by the aggregate public-health calculation.
% DISAPPEARANCE_RATIONALE: If the mandate authority vanished, vaccination rates would decline, disease prevalence would rise, healthcare system capacity would compress during surges, and immunocompromised populations would face renewed confinement or infection risk. The vulnerable commons would lose the institutional protection the mandate provides. Participation would become voluntary, and collective prevention would degrade below the threshold needed for system stability.
% FOUNDING_PROBLEM: Vaccination (or other prevention measures) as a public health good exhibits classic free-rider dynamics: individuals benefit from others' vaccination without bearing the cost themselves, so voluntary uptake falls below the threshold needed to protect those who cannot be vaccinated or for whom vaccines fail. The vulnerable commons — immunocompromised, chronically ill, young infants — have no mechanism to protect themselves against others' refusal. Without mandate authority, their welfare becomes hostage to others' choices.
% FOUNDING_PROBLEM_CORROBORATION: Immunocompromised and disabled advocacy organizations, infectious disease specialists, and healthcare capacity analysts attest the problem is live: voluntary vaccination rates remain below herd immunity thresholds, variants continue to emerge, and breakthrough infections in vulnerable populations continue. Public health authorities cite ongoing circulation and emergence as justification. The problem statement is corroborated from outside the beneficiary class by epidemiologists and healthcare system administrators managing actual capacity constraints.
narrative_ontology:disappearance_verdict(public_health_mandate_authority__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(public_health_mandate_authority__public_health_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__public_health_primary, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(public_health_mandate_authority__public_health_primary, 'none', 1).

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
 *   Extractiveness is 0.68 at interval end. This reflects high coercion on mandate-resistant populations (employment loss, access denial) coupled with genuine coordination benefit to vulnerable populations. The constraint is NOT pure extraction because immunocompromised populations genuinely benefit from collective mandates — their protection is not incidental. Suppression is 0.72: enforcement requires active machinery (employer checks, credential verification, access control at entry points) and suppresses alternatives (refusal becomes legally and occupationally non-viable for regulated sectors). Theater is moderate (0.41): the protective function is real, but a growing share of enforcement activity defends mandate compliance per se rather than demonstrating benefit to the vulnerable commons. Accessibility collapse is high (0.78): alternatives to participation are structurally constrained; once the mandate is enacted, refusal costs are prohibitive for most. The measurement series shows extractiveness rising from 0.48 to 0.68 as enforcement machinery matures (time 0–24) and then plateaus (time 24–48), suggesting the initial rapid buildout of enforcement infrastructure gives way to stable operational suppression. Suppression shows similar trajectory. Theater rises steadily, indicating a drift toward performative enforcement (compliance monitoring becomes an end in itself) as the beneficiary populations' threat perception declines or becomes contested. This is characteristic of constraints that begin as responsive coordination and drift toward routinized extraction.
 *
 * PERSPECTIVAL GAP:
 *   The public-health-authority seat and the mandate-resistant-worker seat should compute very differently. From the authority position, the mandate is protective coordination: a necessary remedy for a free-rider problem, justified by data on vulnerability and transmission. From the mandate-resistant position, the same structure is coercive exclusion: employment loss and access denial imposed in the name of 'collective welfare' by an authority that pre-judges refusal as illegitimate. The authority can frame vaccine-hesitant populations as irresponsible free-riders; mandate-resistant workers can frame the authority as exercising police powers in ways that violate bodily autonomy. Both frames are internally coherent from their respective seats. The engine computes directionality separately for each seat: authority gets low d (benefits from the arrangement, controls it); resistant workers get high d (targets of coercion, excluded from decision-making). The measurement series shows suppression rising more steeply than extractiveness, which is consistent with a constraint whose primary mechanism shifts from generating benefit to enforcing compliance.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: voluntary vaccination uptake remains below thresholds needed to protect immunocompromised populations, and disease circulation continues. However, the mandatrophy test flags a potential zombie constraint: as the acute crisis phase recedes, the mandate persists through institutional inertia and legal precedent rather than continuous threat reassessment. The theater_ratio rising from 0.22 to 0.41 is diagnostic. Initial enforcement (time 0–16) justified itself by reference to active threat. Later enforcement (time 36–48) increasingly defends the mandate's persistence rather than demonstrating ongoing threat. This is the classic pattern of mandatrophy: the founding problem remains technically unsolved (disease circulation continues) but no longer acute enough to justify the enforcement costs being borne. The constraint avoids pure mandatrophy because immunocompromised populations continue to benefit from lower transmission rates, but it drifts toward a hybrid state where benefit to the beneficiary class no longer clearly justifies the coercion on the payer class. A true mandatrophy declaration would require the founding problem to be dead (solved or irrelevant) while the constraint persists; here the problem remains live but its salience has declined relative to enforcement costs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vulnerability_externality_attribution,
    'Does the unvaccinated population''s presence genuinely create an externality for the immunocompromised (measurable excess transmission risk), or does the externality narrative serve primarily to frame mandate-resistance as illegitimate free-riding?',
    'Epidemiological evidence isolating breakthrough infection rates attributable to community transmission versus alternative exposure pathways (healthcare settings, import from international travel, healthcare worker sources). Controlled analysis of transmission risk to immunocompromised in high-vaccination vs. low-vaccination contexts, net of other variables.',
    'If the externality is substantial and isolable, the public-health-primary reading''s coordination framing is empirically grounded; if the externality is diffuse or marginal relative to other sources, the reading''s beneficiary structure is overstated and the constraint drifts toward pure extraction on the resistant population.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vulnerability_externality_attribution, empirical, 'Whether unvaccinated populations create measurable externality or are narratively blamed for diffuse risk.').

omega_variable(
    foundational_axiom_contest,
    'Does the public-health-primary reading''s core premise — that collective welfare can obligate medical compliance — foreclose the bodily-autonomy-primary reading, coexist with it, or influence but not rule out its coherence?',
    'Jurisprudential analysis of whether the two readings could both be true in a single legal or moral framework, or whether one''s fundamental commitments logically exclude the other. This is a conceptual/constitutive question rather than an empirical one.',
    'If foreclosure: the kernel contest is actually a choice between mutually exclusive framings, and the authority''s selection of this reading is a framing victory for the public-health seat. If coexistence: both readings remain live options, and the mandate''s legitimacy is genuinely contested. If influence: this reading constrains but does not eliminate the autonomy reading''s possibility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(foundational_axiom_contest, conceptual, 'Whether the core axioms of this reading logically foreclose or coexist with bodily autonomy primacy.').

omega_variable(
    identity_lock_mechanism_in_refusal,
    'Is the identity_locked exit attributed to mandate-resistant populations a structural feature of the constraint''s design, or a contingent feature of how ideological identity has become fused with vaccination stance?',
    'Comparative analysis of mandate resistance across populations with different ideological baselines; survey evidence on whether vaccination refusal is identity-constitutive (part of how people understand themselves) or instrumental (a specific choice about this medical intervention). If identity-fusion is contingent rather than structural, the exit is constrained but not identity-locked.',
    'If identity-fused: the constraint''s suppression is higher than the structural measures indicate because people cannot leave without abandoning core identity. If contingent: the suppression is more like ordinary constrained exit and could be reduced by decoupling vaccination stance from identity narratives. This affects whether the constraint is sustainable long-term or prone to backlash.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_in_refusal, empirical, 'Whether refusal is identity-constitutive or contingently politicized.').

omega_variable(
    bodily_autonomy_foreclosure_vs_coexistence,
    'Is the public-health-primary reading FORECLOSING the bodily-autonomy-primary reading (these are mutually exclusive frameworks for the same kernel), or do they COEXIST as competing readings held by different parties?',
    'Examine whether a single legal or moral framework could coherently hold both: (1) collective welfare obligates individual medical compliance (public-health-primary axiom) AND (2) bodily sovereignty is categorically inviolable (bodily-autonomy-primary axiom). If internally contradictory, this reading forecloses its sibling. If both can be held within a framework that assigns them different domains or weights, they coexist.',
    'Foreclosure: the mandate authority''s selection of this reading is a constitutive victory; alternative readings are logically impossible under this framework. Coexistence: the mandate authority has made a political choice among live options; the sibling reading remains a coherent counter-claim. The field''s dispute is about which reading correctly applies to the kernel, not about which reading is logically possible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bodily_autonomy_foreclosure_vs_coexistence, conceptual, 'Whether this reading''s foundational axiom logically excludes bodily autonomy primacy or both remain live options.').

omega_variable(
    proportionality_framework_subsumption,
    'Does the public-health-primary reading acknowledge proportionality constraints (narrow tailoring, duration limits, alternative consideration), or does it treat the protective obligation as absolute, independent of proportional fit?',
    'Textual analysis of mandate legislation, enforcement guidance, and judicial decisions applying the public-health-primary framework. Do they require narrow tailoring, sunset clauses, exemption procedures, and burden-balancing? Or do they allow indefinite, broad-scope enforcement on the basis of public health necessity alone?',
    'If proportionality is integrated: this reading influences the proportionality_reading rather than foreclosing it; both agree on the legitimacy of mandates, differing on constraints. If proportionality is absent: this reading may foreclose the proportionality reading''s core claim that legitimacy depends on proportional fit. Alternatively, proportionality advocates may reject this reading''s foundational axiom entirely (feeding the coexistence vs. foreclosure question).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_framework_subsumption, conceptual, 'Whether this reading subsumes or excludes proportional-fit requirements.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__public_health_primary, 0, 48).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(publ_tr_t0, public_health_mandate_authority__public_health_primary, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(publ_tr_t0, observed).
narrative_ontology:measurement(publ_tr_t8, public_health_mandate_authority__public_health_primary, theater_ratio, 8, 0.28).
narrative_ontology:measurement_basis(publ_tr_t8, observed).
narrative_ontology:measurement(publ_tr_t16, public_health_mandate_authority__public_health_primary, theater_ratio, 16, 0.35).
narrative_ontology:measurement_basis(publ_tr_t16, observed).
narrative_ontology:measurement(publ_tr_t24, public_health_mandate_authority__public_health_primary, theater_ratio, 24, 0.39).
narrative_ontology:measurement_basis(publ_tr_t24, observed).
narrative_ontology:measurement(publ_tr_t36, public_health_mandate_authority__public_health_primary, theater_ratio, 36, 0.4).
narrative_ontology:measurement_basis(publ_tr_t36, observed).
narrative_ontology:measurement(publ_tr_t48, public_health_mandate_authority__public_health_primary, theater_ratio, 48, 0.41).
narrative_ontology:measurement_basis(publ_tr_t48, observed).

% Extraction over time
narrative_ontology:measurement(publ_be_t0, public_health_mandate_authority__public_health_primary, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(publ_be_t0, observed).
narrative_ontology:measurement(publ_be_t8, public_health_mandate_authority__public_health_primary, base_extractiveness, 8, 0.54).
narrative_ontology:measurement_basis(publ_be_t8, observed).
narrative_ontology:measurement(publ_be_t16, public_health_mandate_authority__public_health_primary, base_extractiveness, 16, 0.61).
narrative_ontology:measurement_basis(publ_be_t16, observed).
narrative_ontology:measurement(publ_be_t24, public_health_mandate_authority__public_health_primary, base_extractiveness, 24, 0.66).
narrative_ontology:measurement_basis(publ_be_t24, observed).
narrative_ontology:measurement(publ_be_t36, public_health_mandate_authority__public_health_primary, base_extractiveness, 36, 0.67).
narrative_ontology:measurement_basis(publ_be_t36, observed).
narrative_ontology:measurement(publ_be_t48, public_health_mandate_authority__public_health_primary, base_extractiveness, 48, 0.68).
narrative_ontology:measurement_basis(publ_be_t48, observed).

% Suppression requirement over time
narrative_ontology:measurement(publ_su_t0, public_health_mandate_authority__public_health_primary, suppression_requirement, 0, 0.54).
narrative_ontology:measurement_basis(publ_su_t0, observed).
narrative_ontology:measurement(publ_su_t8, public_health_mandate_authority__public_health_primary, suppression_requirement, 8, 0.62).
narrative_ontology:measurement_basis(publ_su_t8, observed).
narrative_ontology:measurement(publ_su_t16, public_health_mandate_authority__public_health_primary, suppression_requirement, 16, 0.68).
narrative_ontology:measurement_basis(publ_su_t16, observed).
narrative_ontology:measurement(publ_su_t24, public_health_mandate_authority__public_health_primary, suppression_requirement, 24, 0.71).
narrative_ontology:measurement_basis(publ_su_t24, observed).
narrative_ontology:measurement(publ_su_t36, public_health_mandate_authority__public_health_primary, suppression_requirement, 36, 0.72).
narrative_ontology:measurement_basis(publ_su_t36, observed).
narrative_ontology:measurement(publ_su_t48, public_health_mandate_authority__public_health_primary, suppression_requirement, 48, 0.72).
narrative_ontology:measurement_basis(publ_su_t48, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=48
narrative_ontology:measurement(publ_grid_01, public_health_mandate_authority__public_health_primary, accessibility_collapse(class), 0, 0.68).
narrative_ontology:measurement(publ_grid_02, public_health_mandate_authority__public_health_primary, accessibility_collapse(class), 48, 0.79).
narrative_ontology:measurement(publ_grid_03, public_health_mandate_authority__public_health_primary, accessibility_collapse(individual), 0, 0.62).
narrative_ontology:measurement(publ_grid_04, public_health_mandate_authority__public_health_primary, accessibility_collapse(individual), 48, 0.74).
narrative_ontology:measurement(publ_grid_05, public_health_mandate_authority__public_health_primary, accessibility_collapse(organizational), 0, 0.71).
narrative_ontology:measurement(publ_grid_06, public_health_mandate_authority__public_health_primary, accessibility_collapse(organizational), 48, 0.81).
narrative_ontology:measurement(publ_grid_07, public_health_mandate_authority__public_health_primary, accessibility_collapse(structural), 0, 0.75).
narrative_ontology:measurement(publ_grid_08, public_health_mandate_authority__public_health_primary, accessibility_collapse(structural), 48, 0.84).
narrative_ontology:measurement(publ_grid_09, public_health_mandate_authority__public_health_primary, resistance(class), 0, 0.73).
narrative_ontology:measurement(publ_grid_10, public_health_mandate_authority__public_health_primary, resistance(class), 48, 0.68).
narrative_ontology:measurement(publ_grid_11, public_health_mandate_authority__public_health_primary, resistance(individual), 0, 0.71).
narrative_ontology:measurement(publ_grid_12, public_health_mandate_authority__public_health_primary, resistance(individual), 48, 0.64).
narrative_ontology:measurement(publ_grid_13, public_health_mandate_authority__public_health_primary, resistance(organizational), 0, 0.68).
narrative_ontology:measurement(publ_grid_14, public_health_mandate_authority__public_health_primary, resistance(organizational), 48, 0.58).
narrative_ontology:measurement(publ_grid_15, public_health_mandate_authority__public_health_primary, resistance(structural), 0, 0.65).
narrative_ontology:measurement(publ_grid_16, public_health_mandate_authority__public_health_primary, resistance(structural), 48, 0.62).
narrative_ontology:measurement(publ_grid_17, public_health_mandate_authority__public_health_primary, stakes_inflation(class), 0, 0.54).
narrative_ontology:measurement(publ_grid_18, public_health_mandate_authority__public_health_primary, stakes_inflation(class), 48, 0.66).
narrative_ontology:measurement(publ_grid_19, public_health_mandate_authority__public_health_primary, stakes_inflation(individual), 0, 0.51).
narrative_ontology:measurement(publ_grid_20, public_health_mandate_authority__public_health_primary, stakes_inflation(individual), 48, 0.69).
narrative_ontology:measurement(publ_grid_21, public_health_mandate_authority__public_health_primary, stakes_inflation(organizational), 0, 0.58).
narrative_ontology:measurement(publ_grid_22, public_health_mandate_authority__public_health_primary, stakes_inflation(organizational), 48, 0.72).
narrative_ontology:measurement(publ_grid_23, public_health_mandate_authority__public_health_primary, stakes_inflation(structural), 0, 0.61).
narrative_ontology:measurement(publ_grid_24, public_health_mandate_authority__public_health_primary, stakes_inflation(structural), 48, 0.71).
narrative_ontology:measurement(publ_grid_25, public_health_mandate_authority__public_health_primary, suppression(class), 0, 0.52).
narrative_ontology:measurement(publ_grid_26, public_health_mandate_authority__public_health_primary, suppression(class), 48, 0.74).
narrative_ontology:measurement(publ_grid_27, public_health_mandate_authority__public_health_primary, suppression(individual), 0, 0.48).
narrative_ontology:measurement(publ_grid_28, public_health_mandate_authority__public_health_primary, suppression(individual), 48, 0.68).
narrative_ontology:measurement(publ_grid_29, public_health_mandate_authority__public_health_primary, suppression(organizational), 0, 0.54).
narrative_ontology:measurement(publ_grid_30, public_health_mandate_authority__public_health_primary, suppression(organizational), 48, 0.71).
narrative_ontology:measurement(publ_grid_31, public_health_mandate_authority__public_health_primary, suppression(structural), 0, 0.59).
narrative_ontology:measurement(publ_grid_32, public_health_mandate_authority__public_health_primary, suppression(structural), 48, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_mandate_authority__public_health_primary, attachment_coordination).
narrative_ontology:boltzmann_floor_override(public_health_mandate_authority__public_health_primary, 0.11).
narrative_ontology:affects_constraint(public_health_mandate_authority__public_health_primary, public_health_mandate_authority__bodily_autonomy_primary).
narrative_ontology:affects_constraint(public_health_mandate_authority__public_health_primary, public_health_mandate_authority__proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of a three-constraint family decomposing the public_health_mandate_authority kernel. Each reading instantiates a different structural relationship to the underlying commitment system. The public_health_primary reading frames mandate authority as an obligation to protect vulnerable populations; bodily_autonomy_primary treats it as a categorical violation; proportionality_reading makes legitimacy conditional on sliding-scale analysis. These are not three measurements of one constraint — they are three structurally distinct constraints differing in their ε values (extractiveness), beneficiary/victim declarations, and founding axioms. The upstream reading (public_health_primary, this story) influences downstream readings by providing the policy and precedent framework within which alternatives must operate. All three stories are linked via network.affects_constraints to enable contamination and coupling analysis across the kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(public_health_mandate_authority__public_health_primary, powerless, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
