% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_balance__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: vaccine_mandate_balance__bodily_autonomy_primary
 *   human_readable: Vaccine Mandate as Bodily Autonomy Violation (Individual Consent Primary)
 *   domain: public_health_ethics/constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint is ONE READING of the contested kernel
 *   vaccine_mandate_balance. It instantiates the bodily_autonomy_primary
 *   reading: individual consent to medical intervention is inviolable and the
 *   state cannot override it regardless of collective health benefit. Under
 *   this reading, vaccine mandates are classified as snares — coercive
 *   extraction of bodily autonomy, enforced by employment loss, education
 *   exclusion, and social penalty, with the coordination function (disease
 *   prevention) serving as cover for the primary extractive mechanism (state
 *   control over medical decisions). The measurement trajectory shows rising
 *   extractiveness as mandates persist and broaden despite changed
 *   epidemiological conditions (variants less severe, vaccines widely
 *   available, voluntary uptake stabilizing); suppression requirement remains
 *   high as enforcement must expand to maintain compliance; theater ratio
 *   rises modestly as public health messaging shifts from acute-crisis
 *   framing to routine-policy maintenance. This reading assigns the
 *   vaccinated collective as beneficiaries (protective benefit through herd
 *   immunity) and vaccine hesitant, medically exempted, and conscientious
 *   objectors as victims (coerced compliance). Immunocompromised populations
 *   are excluded from the victim category in this reading — their
 *   vulnerability is reframed as an unavoidable risk inherent to liberty
 *   rather than as harm requiring remedy.
 *
 * KEY AGENTS:
 *   - vaccine_hesitant_individuals: Primary targets of coercion — powerless, trapped, bearing employment and social exclusion
 *   - medical_exemption_deniers: Victims of categorical mandate scope — identity-locked, medical facts overridden by policy
 *   - conscientious_objectors: Victims of conscience violation — identity-locked, ideological convictions overridden by state authority
 *   - public_health_authorities: Agenda-setter — institutional power, control mandate scope and enforcement, arbitrage exit (can change policy)
 *   - vaccinated_population_collective: Beneficiaries of herd immunity — organized power, high exit mobility, benefit without running the system
 *   - democratic_legislatures: Observers — institutional power, arena where readings contest, analytical exit
 *   - immunocompromised_population: Excluded from victim classification — vulnerability reframed as risk acceptance inherent to freedom
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__bodily_autonomy_primary, 0.82).
domain_priors:suppression_score(vaccine_mandate_balance__bodily_autonomy_primary, 0.79).
domain_priors:theater_ratio(vaccine_mandate_balance__bodily_autonomy_primary, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, extractiveness, 0.82).
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__bodily_autonomy_primary, snare).
narrative_ontology:human_readable(vaccine_mandate_balance__bodily_autonomy_primary, "Vaccine Mandate as Bodily Autonomy Violation (Individual Consent Primary)").
narrative_ontology:topic_domain(vaccine_mandate_balance__bodily_autonomy_primary, "public_health_ethics/constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__bodily_autonomy_primary, 'ace27874-98e3-41be-b3a6-011929c8843d').
narrative_ontology:cs_kernel_codification('ace27874-98e3-41be-b3a6-011929c8843d', formalized).
narrative_ontology:cs_authority_grounding('ace27874-98e3-41be-b3a6-011929c8843d', extraction).
narrative_ontology:cs_interpretation_layer_present('ace27874-98e3-41be-b3a6-011929c8843d').
narrative_ontology:cs_reading_relation('ace27874-98e3-41be-b3a6-011929c8843d', vaccine_mandate_balance__proportionality_reading, coexists_with).
narrative_ontology:cs_reading_relation('ace27874-98e3-41be-b3a6-011929c8843d', vaccine_mandate_balance__public_health_primary, coexists_with).
narrative_ontology:cs_axiom('ace27874-98e3-41be-b3a6-011929c8843d', foundational, bodily_autonomy_categorical_inviolable).
narrative_ontology:cs_axiom_status(bodily_autonomy_categorical_inviolable, holdable).
narrative_ontology:cs_axiom_grounding('ace27874-98e3-41be-b3a6-011929c8843d', bodily_autonomy_categorical_inviolable, deontological).
narrative_ontology:cs_axiom('ace27874-98e3-41be-b3a6-011929c8843d', foundational, medical_choice_not_subject_to_collective_override).
narrative_ontology:cs_axiom_status(medical_choice_not_subject_to_collective_override, holdable).
narrative_ontology:cs_axiom_grounding('ace27874-98e3-41be-b3a6-011929c8843d', medical_choice_not_subject_to_collective_override, deontological).
narrative_ontology:cs_reference_frame('ace27874-98e3-41be-b3a6-011929c8843d', individual_medical_autonomy_framework).
narrative_ontology:cs_drift_state('ace27874-98e3-41be-b3a6-011929c8843d', post_acute_pandemic_phase, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ace27874-98e3-41be-b3a6-011929c8843d', '2026-06-19T14:32:00Z').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__bodily_autonomy_primary, public_health_authorities).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__bodily_autonomy_primary, vaccinated_population_collective).
narrative_ontology:constraint_victim(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_hesitant_individuals).
narrative_ontology:constraint_victim(vaccine_mandate_balance__bodily_autonomy_primary, medical_exemption_deniers).
narrative_ontology:constraint_victim(vaccine_mandate_balance__bodily_autonomy_primary, conscientious_objectors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face mandated vaccination as condition of employment, education, or public participation. Their bodily autonomy is overridden by state coercion backed by employment loss, school exclusion, or social exclusion. Exit options are severely constrained: they cannot leave the jurisdiction without extraordinary cost, and cannot exercise their stated preference against vaccination within the jurisdiction.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_hesitant_individuals, payer,
    powerless, biographical, trapped, national).

% Individuals with genuine medical contraindications to vaccination (severe allergies, prior myocarditis, immunocompromised status requiring live-attenuated vaccines) who are denied exemptions under narrow statutory criteria. Their medical reality conflicts with the mandate's categorical scope; they are forced into either non-compliance with legal consequence or medical harm. Identity-lock arises from the medical fact that cannot be changed without becoming a different person.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, medical_exemption_deniers, payer,
    powerless, biographical, identity_locked, national).

% Hold religious, philosophical, or deeply-rooted personal convictions against vaccination or state medical coercion that conflict with the mandate. Complying requires violating their conscience; refusing requires accepting employment loss or legal penalty. The identity-lock is the fusion of their worldview with their refusal — exit would require apostasy or apostasy-equivalent ideological reversal.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, conscientious_objectors, payer,
    powerless, biographical, identity_locked, national).

% Design and enforce vaccine mandates, defining scope, exemptions, enforcement mechanisms, and penalties. They justify mandates as necessary to achieve population immunity thresholds and protect vulnerable populations from epidemic risk. They control the administrative apparatus that determines who is exempt, who is compliant, and who faces penalty.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, public_health_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Receive protective benefit (through herd immunity if threshold is reached) and avoid individual risk of vaccine hesitancy spreading. They do not run the mandate system and do not collectively decide its scope; they are beneficiaries of the coercive structure without being its architects. Their individual mobility is high — they can move through society without mandate penalty — but as a collective their benefit depends on the mandate persisting.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, vaccinated_population_collective, beneficiary,
    organized, biographical, mobile, national).

% Are not named as victims under this reading because their vulnerability is reframed as a risk they must accept as the price of others' liberty. They are exposed to unvaccinated individuals and to breakthrough infections but are outside the victim category in this reading's frame — their risk is categorized as inherent to a free society rather than as extraction requiring remedy. This exclusion is structurally distinct from consent-violation and is the reading's treatment of unavoidable collective risk.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, immunocompromised_population, excluded,
    powerless, immediate, trapped, national).

% Enact enabling legislation for public health mandates, setting the legal authority and scope. They serve as a check on executive power and an arena where competing readings contest which principles guide public health law. Their decisions reflect (or are contested) the kernel dispute over whether bodily autonomy or collective benefit is primary.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, democratic_legislatures, observer,
    institutional, generational, analytical, national).

% Mobilize legal and political opposition to mandates on autonomy grounds, arguing that state-compelled medical intervention violates fundamental rights regardless of collective benefit. They contest the mandate's legitimacy from outside the enforcement apparatus and seek to narrow its scope or overturn it entirely.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, civil_liberties_advocates, observer,
    organized, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_balance__bodily_autonomy_primary, public_health_authorities).
narrative_ontology:fixing_cost_class(vaccine_mandate_balance__bodily_autonomy_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Achieve population-level disease prevention by ensuring sufficient vaccination coverage to interrupt disease transmission and protect those who cannot be vaccinated. Solves the collective-action problem that voluntary vaccination alone may not reach sufficient coverage.
% TRANSFER_FUNCTION: Extracts bodily autonomy and decision-making authority over medical intervention from hesitant, exempted, and conscientious individuals and transfers authority to public health institutions to enforce vaccination as a condition of social participation.
% ABSENT_VOICES: Individuals subject to mandates who are not yet identified as hesitant or objecting, whose compliance is compelled before their preferences are known. Also: future generations who will live under precedents this mandate establishes about state power over bodily integrity. Also: epidemiological expertise questioning whether mandates (as opposed to education, incentives, or voluntary programs) are necessary to reach protective thresholds.
% DISAPPEARANCE_RATIONALE: If vaccine mandates disappeared overnight, vaccination rates would decline substantially (historical precedent from mandate removal). Public health authorities would need alternative mechanisms (education, incentives, voluntary programs, or acceptance of higher disease circulation). Disease outbreak dynamics would shift; vulnerable populations would face increased exposure. Employment, education, and social participation would no longer require vaccination. The entire structure of coercive enforcement would collapse and be replaced by voluntary or incentive-based coordination.
% FOUNDING_PROBLEM: Early COVID-19 pandemic: novel pathogen with significant mortality and hospitalization risk, rapidly spreading, vaccines developed and authorized but voluntary uptake insufficient to reach herd immunity thresholds estimated at 70-90%, healthcare systems at capacity, vulnerable populations (elderly, immunocompromised) facing acute exposure risk.
% FOUNDING_PROBLEM_CORROBORATION: Public health authorities attest the founding problem remains live (new variants, waning immunity, global coverage shortfalls). Epidemiologists and public health researchers provide independent analysis of transmission dynamics and herd immunity thresholds. Vaccine hesitant populations and autonomy advocates attest that the founding problem has substantially resolved (vaccines widely available, high-risk populations largely protected, disease severity declined) and that mandates now persist as coercive policy without proportional justification. Legislative inquiries and independent health agencies from other jurisdictions have documented mandate persistence despite changed epidemiological conditions.
narrative_ontology:disappearance_verdict(vaccine_mandate_balance__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_balance__bodily_autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__bodily_autonomy_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vaccine_mandate_balance__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_balance__bodily_autonomy_primary, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Base extractiveness is high (0.82) because the constraint overrides individual medical decision-making through state coercion backed by severe consequences (employment loss, education exclusion). From the bodily-autonomy-primary reading, the extraction is the authority transfer itself, independent of vaccine efficacy or disease severity — the state takes control over medical choices that are presented as individual. Suppression is high (0.79) because alternatives are structurally unavailable: vaccine hesitant individuals cannot exit without extraordinary cost; those with genuine medical contraindications cannot appeal exemptions; conscientious objectors cannot practice their conscience. Theater ratio is low-moderate (0.28) because public health messaging maintains genuine disease-prevention content (not pure performance), but measurement drift shows theater increasing as epidemiological justification weakens — mandates persist despite falling disease severity, suggesting enforcement is increasingly theatrical. Accessibility collapse is moderate (0.71) because alternatives exist rhetorically (other disease-prevention approaches: education, incentives, voluntary programs, targeted protection) but are foreclosed by policy choice rather than natural law. Resistance is high (0.68) because mandates meet sustained opposition from multiple constituencies (medical autonomy advocates, conscientious objectors, public health skeptics, state-power skeptics). The measured metrics describe the constraint as substantially extractive and enforced; the claimed type (snare) reflects this reading's classification that the constraint is primarily extractive with coordination function serving as justification.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (public health authorities) and the payer seats (vaccine hesitant, exempted, conscientious) compute dramatically different types from the same structural data. From the authority seat, the mandate is coordination (disease prevention is a genuine collective problem, coercion is justified by severity and population protection). From the payer seats, the same structure operates as coercive extraction (bodily autonomy is violated, alternatives are suppressed, the disease severity that may have justified early mandates has declined). The engine computes this divergence from the structural facts: the authorities have institutional power and arbitrage-grade exit (they can change the policy); payers have powerless or moderate power and trapped or identity-locked exit. Directionality reflects this asymmetry: authorities are near the beneficiary end (d low), payers are near the target end (d high). What looks like coordination from the high-power, high-exit seat looks like extraction from the low-power, no-exit seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities: d ~0.15 (beneficiary end). They set mandate scope, define exemptions, control enforcement, and can change policy. They are not forced to participate; they choose to maintain the constraint. Vaccine hesitant individuals: d ~0.88 (target end). Employment and education participation are non-optional; they face coerced vaccination or exclusion; no exit that preserves their livelihood or social integration. Medical exemption deniers: d ~0.85 (target end). Medical facts are overridden by policy; they cannot exit by appealing to legitimate medical reality. Conscientious objectors: d ~0.87 (target end). Their ideological frameworks are overridden; exit requires apostasy. Vaccinated population: d ~0.35 (moderate beneficiary). They benefit from herd immunity without running the system; they chose vaccination (mostly without coercion); they can move through society without penalty. Immunocompromised (excluded category): d is not computed in this reading because their risk is categorized outside the extraction frame — their vulnerability is categorized as inherent risk in a free society, not as harm the constraint causes. This is the reading's controversial framing: autonomy for the hesitant is valued higher than protection for the vulnerable.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is not mandatrophic in the technical sense (the mandate still serves its founding function: preventing disease circulation), but it is mandatrophic in the reading's frame: the founding problem (acute pandemic, overwhelmed healthcare, high disease severity) has substantially resolved, yet the mandate persists and has broadened. The R5 analysis shows founding_problem_status=contested: authorities attest the problem remains live (new variants, persistent transmission); external observers attest the problem has materially changed (disease severity lower, vaccines widely available, voluntary uptake stabilizing). The measurement trajectory confirms mandatrophy dynamics: extractiveness rises even as epidemiological justification declines; theater ratio increases as public messaging shifts from acute-crisis to routine-maintenance framing. This is exactly the pattern that signals a constraint whose primary function has atrophied but whose enforcement apparatus has become self-perpetuating. However, under this reading's frame, that atrophy is secondary — the reading's primary claim is that bodily autonomy is inviolable regardless of disease severity. Even if COVID remained acutely severe, this reading would classify mandates as snares. The mandatrophy analysis adds another axis of concern (are we enforcing outdated policy?) but does not drive the classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_vs_vulnerability_frame,
    'Is bodily autonomy a higher-order value than protection of vulnerable populations, or are they incommensurable values that cannot be ranked?',
    'This is a foundational normative question that cannot be resolved by empirical data alone. Resolution requires adoption of a normative framework (liberal individualism, communitarianism, capabilities approach, etc.). Different philosophical traditions yield different answers.',
    'If autonomy is ranked higher, mandates are unjustified extraction and this reading''s classification holds across all epidemiological conditions. If vulnerability is ranked higher and collective protection is a higher-order value, the reading inverts to public_health_primary. If incommensurable, the constraint is contested and proportionality_reading becomes the operative frame.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(autonomy_vs_vulnerability_frame, preference, 'Whether individual bodily autonomy or collective health protection is the foundational normative priority.').

omega_variable(
    structural_vs_internalized_suppression,
    'What proportion of the measured suppression (0.79) is structural (external barriers: employment loss, education exclusion) versus internalized (psychological internalization of mandate framing)?',
    'Post-mandate removal observation: if suppression persists (vaccine hesitancy remains high after coercive mechanisms are lifted), the suppression was substantially internalized. If suppression collapses when coercion is removed, the suppression was primarily structural.',
    'If primarily structural, the constraint''s effective suppression could be substantially reduced by alternative coordination mechanisms (education, incentives). If substantially internalized, the constraint has altered preferences themselves and cannot be unmade by removing external barriers alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_internalized_suppression, empirical, 'Decomposition of suppression into structural and internalized components.').

omega_variable(
    exemption_legitimacy_scope,
    'Are medical exemptions and conscientious exemptions legitimate constraints on mandate scope, or does legitimate public health authority include power to override both?',
    'This question divides the reading from proportionality_reading and public_health_primary. Comparative institutional analysis across jurisdictions with different exemption scopes; empirical measurement of health outcomes and mandate persistence under different exemption rules.',
    'If exemptions are legitimate constraints, the current mandate (narrow exemption criteria) is extractive and medical exemption deniers are unjustly victims. If public health authority can override exemptions, the classification shifts toward public_health_primary frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exemption_legitimacy_scope, conceptual, 'Whether exemption criteria are normatively constrained or within legitimate public health discretion.').

omega_variable(
    alternative_coordination_sufficiency,
    'Could herd immunity thresholds be reached through non-coercive means (education, incentives, targeted protection, voluntary programs) without mandate-based coercion?',
    'Natural experiments from jurisdictions using non-coercive approaches; modeling studies comparing vaccination trajectories; comparative analysis of disease control outcomes under different policy mixes.',
    'If non-coercive alternatives were sufficient, the mandate represents unnecessary extraction. If non-coercive approaches provably fall short of protective thresholds, the mandate moves toward justified coordination in the public_health_primary frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_coordination_sufficiency, empirical, 'Whether alternative coordination mechanisms could achieve public health objectives without coercion.').

omega_variable(
    kernel_reading_identity_fusion,
    'Is the bodily_autonomy_primary reading a fixed philosophical commitment, or is it identity-fused with resistance to authority (such that as institutional authority changes, the reading persists by inverting its content)?',
    'Longitudinal observation of the reading''s application to different institutional contexts; analysis of whether autonomy advocates consistently defend bodily autonomy (including of vaccine-accepting individuals against other state medical coercion) or selectively deploy it when opposing this particular mandate.',
    'If the reading is fused with anti-authority identity rather than with stable autonomy commitment, the classification may shift when institutional context changes. This would expose the reading as partially extractive (instrumentalizing autonomy language for identity resistance) rather than consistently principled.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_identity_fusion, conceptual, 'Whether the bodily_autonomy_primary reading is a stable principle or identity-fused anti-authority position.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__bodily_autonomy_primary, 0, 48).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 0, 0.12).
narrative_ontology:measurement(vacc_tr_t6, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 6, 0.16).
narrative_ontology:measurement(vacc_tr_t12, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 12, 0.2).
narrative_ontology:measurement(vacc_tr_t24, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 24, 0.25).
narrative_ontology:measurement(vacc_tr_t36, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 36, 0.27).
narrative_ontology:measurement(vacc_tr_t48, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 48, 0.28).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(vacc_be_t6, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 6, 0.62).
narrative_ontology:measurement(vacc_be_t12, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 12, 0.68).
narrative_ontology:measurement(vacc_be_t24, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 24, 0.75).
narrative_ontology:measurement(vacc_be_t36, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 36, 0.79).
narrative_ontology:measurement(vacc_be_t48, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 48, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(vacc_su_t6, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 6, 0.64).
narrative_ontology:measurement(vacc_su_t12, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 12, 0.7).
narrative_ontology:measurement(vacc_su_t24, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 24, 0.76).
narrative_ontology:measurement(vacc_su_t36, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 36, 0.78).
narrative_ontology:measurement(vacc_su_t48, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 48, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_balance__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vaccine_mandate_balance__bodily_autonomy_primary, 0.12).
narrative_ontology:affects_constraint(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_mandate_balance__proportionality_reading).
narrative_ontology:affects_constraint(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_mandate_balance__public_health_primary).

% DUAL FORMULATION NOTE:
% The vaccine_mandate_balance kernel decomposes into three constraint stories, each a different reading instantiating a different ε and beneficiary/victim structure. bodily_autonomy_primary (THIS story) treats mandates as extractive coercion (ε=0.82, victims=unvaccinated coerced individuals). proportionality_reading treats mandates as justified only when disease/transmission meet severity thresholds (ε varies with disease severity; victims depend on whether thresholds are met). public_health_primary treats mandates as justified coordination when collective protection demands it (ε low when necessary, victims reframed from unvaccinated to those whose protection is compromised). All three share the kernel (commitment to vaccination as policy) but differ on what makes it legitimate. Sibling stories must be authored as separate constraint files, each with its own ε, stakeholder structure, and six_questions answers. This story links to siblings via network.affects_constraints; reading_relations in cs_structure establish the logical structure among readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vaccine_mandate_balance__bodily_autonomy_primary, powerless, 0.87).
constraint_indexing:directionality_override(vaccine_mandate_balance__bodily_autonomy_primary, organized, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
