% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: coercion_legitimacy_boundary__public_health_primary
 *   human_readable: State Compulsion for Collective Harm Prevention (Public Health Primary Reading)
 *   domain: public_health/constitutional_law/medical_ethics
 *
 * SUMMARY:
 *   This constraint instantiates the PUBLIC_HEALTH_PRIMARY reading of the
 *   coercion-legitimacy kernel: the state's authority to compel medical
 *   intervention when collective harm-prevention outweighs individual bodily
 *   autonomy. The reading is one voice in a contested kernel
 *   (bodily_autonomy_primary and proportionality_reading are sibling
 *   readings, instantiated as separate constraints). In THIS reading, the
 *   state's power to define necessity, the epidemiological justification for
 *   coercion thresholds, and the actual enforcement machinery are the
 *   constraint under examination. Unvaccinated vaccine-hesitant individuals
 *   enter the victim set as coerced subjects; immunocompromised populations
 *   exit the victim set and become the primary beneficiary class (they cannot
 *   be protected by market choice and depend on others' coercion). The
 *   extractiveness is high (0.82 at interval end) because the constraint
 *   persists by active suppression of non-compliance and by progressively
 *   narrowing the exit options available to targets. The theater ratio rises
 *   gradually (0.28 to 0.42 over the interval) as enforcement machinery
 *   focuses increasingly on compliance theater (verification, documentation,
 *   social enforcement) relative to actual epidemiological harm-prevention.
 *
 * KEY AGENTS:
 *   - state_public_health_authority (agenda_setter, institutional) — sets coercion boundaries, enforces mandates, collects compliance data
 *   - vaccine_hesitant_individuals (payer, powerless) — subject to coercion, face multi-layered penalties, trapped in jurisdiction
 *   - immunocompromised_populations (beneficiary, powerless) — depend on herd immunity, structurally unable to protect themselves, identity-locked to protective regime
 *   - medically_contraindicated_persons (payer/beneficiary, moderate) — nominally exempt but bear exemption burden, benefit from others' compliance
 *   - public_health_epidemiologists (agenda_setter/observer, institutional) — provide scientific authority legitimizing coercion thresholds
 *   - constitutional_courts (observer, institutional) — review mandate validity, can invalidate but cannot set agenda
 *   - bodily_autonomy_advocates (excluded, moderate) — structurally barred from agenda-setting, reframed as anti-coordination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__public_health_primary, 0.82).
domain_priors:suppression_score(coercion_legitimacy_boundary__public_health_primary, 0.88).
domain_priors:theater_ratio(coercion_legitimacy_boundary__public_health_primary, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, extractiveness, 0.82).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__public_health_primary, tangled_rope).
narrative_ontology:human_readable(coercion_legitimacy_boundary__public_health_primary, "State Compulsion for Collective Harm Prevention (Public Health Primary Reading)").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__public_health_primary, "public_health/constitutional_law/medical_ethics").

domain_priors:requires_active_enforcement(coercion_legitimacy_boundary__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__public_health_primary, '77589f84-f71d-4142-b2fa-7011abc28d94').
narrative_ontology:cs_kernel_codification('77589f84-f71d-4142-b2fa-7011abc28d94', formalized).
narrative_ontology:cs_authority_grounding('77589f84-f71d-4142-b2fa-7011abc28d94', extraction).
narrative_ontology:cs_interpretation_layer_present('77589f84-f71d-4142-b2fa-7011abc28d94').
narrative_ontology:cs_reading_relation('77589f84-f71d-4142-b2fa-7011abc28d94', coercion_legitimacy_boundary__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('77589f84-f71d-4142-b2fa-7011abc28d94', coercion_legitimacy_boundary__proportionality_reading, influences).
narrative_ontology:cs_axiom('77589f84-f71d-4142-b2fa-7011abc28d94', foundational, collective_harm_overrides_autonomy).
narrative_ontology:cs_axiom_status(collective_harm_overrides_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('77589f84-f71d-4142-b2fa-7011abc28d94', collective_harm_overrides_autonomy, instrumental).
narrative_ontology:cs_axiom('77589f84-f71d-4142-b2fa-7011abc28d94', foundational, epidemiological_necessity_legitimates_coercion).
narrative_ontology:cs_axiom_status(epidemiological_necessity_legitimates_coercion, holdable).
narrative_ontology:cs_axiom_grounding('77589f84-f71d-4142-b2fa-7011abc28d94', epidemiological_necessity_legitimates_coercion, empirically_contingent).
narrative_ontology:cs_reference_frame('77589f84-f71d-4142-b2fa-7011abc28d94', state_public_health_authority_framework).
narrative_ontology:cs_drift_state('77589f84-f71d-4142-b2fa-7011abc28d94', contemporary_rights_expansion_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('77589f84-f71d-4142-b2fa-7011abc28d94', '').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__public_health_primary, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, immunocompromised_populations).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, public_health_infrastructure).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, disease_free_communities).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__public_health_primary, vaccine_hesitant_individuals).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__public_health_primary, medically_contraindicated_persons).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__public_health_primary, autonomy_bearing_subjects).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, medically_contraindicated_persons).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__public_health_primary, medical_professionals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces disease control mandates through legislation, regulation, and administrative action. Justifies coercion as necessary to prevent harm to vulnerable populations and maintain population immunity thresholds. Collects compliance data, administers exemption processes, and deploys enforcement mechanisms (fines, employment restrictions, school exclusion) against non-compliers. Bears responsibility for public health outcomes and defends mandates through epidemiological justification.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, state_public_health_authority, agenda_setter,
    institutional, generational, analytical, national).

% Subject to state compulsion to receive medical intervention (vaccination, quarantine, treatment) against their stated preferences. Face cascading penalties: employment termination, school exclusion for children, social stigma, and legal liability. Their objections—whether based on religious belief, medical concern, or autonomy claim—are overridden by collective harm rationale. Exit from the jurisdiction is the only meaningful opt-out, and geographic mobility is constrained by similar policies in neighboring jurisdictions.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, vaccine_hesitant_individuals, payer,
    powerless, biographical, trapped, national).

% Depend on herd immunity for survival; cannot receive live-attenuated vaccines and have limited protection from inactivated ones. Benefit directly from vaccination mandates on others, which raise the immunized fraction above their personal protection threshold. Their vulnerability is the moral justification for the coercion regime; without their existence as a benefit-receiving class, the public health primary reading loses its coordination story.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, immunocompromised_populations, beneficiary,
    powerless, immediate, identity_locked, national).

% Have genuine medical contraindications (severe allergies, prior adverse reactions) and should not receive specific interventions. Are nominally exempt from mandates but face burden of proof: obtaining medical certification, navigating exemption processes, and bearing social suspicion. They benefit from others' compliance (herd immunity) but are expected to absorb the authentication and social cost of exemption. The exemption pathway is theoretically available but administratively and socially hostile.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, medically_contraindicated_persons, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(coercion_legitimacy_boundary__public_health_primary, medically_contraindicated_persons, beneficiary).

% Benefit from maintained low disease prevalence and reduced transmission risk. Externally protected by mandates on others. Bear diffuse costs through taxation (public health infrastructure), mild compliance burden (scheduled interventions when mandated), and reduced choice set (only approved interventions available). The benefit is diffuse, non-rivalrous collective goods; the cost is absorbed into the background of governance.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, disease_free_communities, beneficiary,
    organized, biographical, constrained, national).

% Provide the scientific authority on which mandate legitimacy rests. Produce epidemiological models and risk assessments that justify coercion thresholds. Sit at the boundary between technical expertise (assessing harm, transmission) and normative choice (threshold at which harm justifies coercion). Their authority is invoked to de-politicize the coercion decision, framing it as technical necessity rather than value choice.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, public_health_epidemiologists, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(coercion_legitimacy_boundary__public_health_primary, public_health_epidemiologists, observer).

% Are obligated to administer mandated interventions and enforce compliance reporting; their medical judgment is subordinated to public health mandate. Bear professional and legal liability for non-compliance and for harms from administered interventions. Face ethical tension between fiduciary duty to individual patients and state compulsion to override patient consent. Their professional autonomy is compressed by the mandate regime.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, medical_professionals, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(coercion_legitimacy_boundary__public_health_primary, medical_professionals, observer).

% Would argue that consent-without-coercion is foundational and that collective benefit does not override individual bodily integrity. Are excluded from the agenda-setting process in the public health primary reading because the reading's core premise is that harm-prevention CAN outweigh autonomy. Their arguments are treated as anti-coordination narratives rather than as legitimate competing values.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, bodily_autonomy_advocates, excluded,
    moderate, biographical, constrained, national).

% Adjudicate whether mandates comply with constitutional protections (bodily integrity, religious freedom, due process). They review the state's claimed necessity and proportionality. Their role is contestatory: they can rule mandates unconstitutional, forcing the state to restructure the enforcement regime. They remain formally neutral but are positioned to validate or invalidate the coercion legitimacy boundary.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(coercion_legitimacy_boundary__public_health_primary, state_public_health_authority).
narrative_ontology:fixing_cost_class(coercion_legitimacy_boundary__public_health_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of achieving herd immunity: individual incentive to free-ride (refuse intervention, benefit from others' compliance) is misaligned with the public-health outcome that requires sufficient population coverage. The mandate internalizes the collective cost, converting free-riding into non-compliance carrying penalties.
% TRANSFER_FUNCTION: Moves bodily autonomy and medical choice from vaccine-hesitant individuals to the state apparatus (which exercises compulsion), and moves health/safety protection to immunocompromised and disease-free populations. The state apparatus collects enforcement authority and epidemiological control; the coerced bear medical administration and compliance cost.
% ABSENT_VOICES: Bodily autonomy advocates and those who hold consent-without-coercion as foundational are structurally excluded from agenda-setting in this reading. Their objections are reframed as anti-health-coordination rather than as competing legitimate values. Constitutional courts participate but only in a review/validation role, not agenda-setting; their capacity to invalidate is marginal within the reading's own authority structure.
% DISAPPEARANCE_RATIONALE: If the coercion-legitimacy boundary and its enforcement machinery disappeared, vaccination rates would drop below herd immunity thresholds in most populations; immunocompromised individuals would face elevated risk; disease prevalence would rise; and the organizational infrastructure (mandate tracking, exemption processing, enforcement) would dissolve. The system reorganizes around voluntary-choice and market-based incentives rather than state compulsion.
% FOUNDING_PROBLEM: Infectious disease transmission creates a genuine externality: one person's decision to refuse intervention harms others who cannot be protected or who face elevated risk. Free-market choice and voluntary uptake leave the population below the immunity threshold needed to protect the vulnerable. The founding problem is the coordination failure that individual choice cannot solve.
% FOUNDING_PROBLEM_CORROBORATION: Public health epidemiologists attest the founding problem is live: disease elimination and low transmission require sustained population immunity above voluntary-uptake levels, and vulnerable populations would face preventable harm without mandates. Constitutional scholars, patient-autonomy advocates, and some ethicists attest the founding problem is overstated or the remedy is disproportionate: voluntary uptake with incentives and education can achieve adequate protection without coercion; the actual practice shows mandates are maintained beyond epidemiological necessity and used to enforce behavioral conformity beyond legitimate harm-prevention. Independent policy analysis and comparative jurisdictional data (jurisdictions with high voluntary uptake vs. jurisdictions with mandates) show mixed outcomes: some mandate regimes achieve higher coverage, others show diminishing returns and rising non-compliance/exit as enforcement intensifies.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__public_health_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__public_health_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(coercion_legitimacy_boundary__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(coercion_legitimacy_boundary__public_health_primary, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness measured at 0.82 because: (1) the constraint compels a medical intervention without informed consent from a defined target class (vaccine-hesitant individuals), (2) the targets bear direct bodily administration cost and penalty cascades (employment, school, legal), (3) the beneficiaries (immunocompromised) cannot opt out of dependency, and (4) the state retains unilateral power to redefine the mandate scope and coercion thresholds. Suppression at 0.88 because: (1) non-compliance is detected and penalized through employment verification, school enrollment, travel documentation, (2) alternative vaccine sources and medical routes are eliminated (single-source supply chains, restricted distribution), (3) social suppression (stigmatization of non-compliance) reinforces structural suppression, and (4) geographic exit is blocked by similar mandates across jurisdictions. Theater ratio rises from 0.28 to 0.42 because enforcement increasingly focuses on documenting compliance (vaccination cards, batch-tracking, digital passports) relative to direct measurement of epidemiological outcomes (disease prevalence, immunity thresholds). The shared time grid ensures all measurements are authored at every examined point; the progressive rise in theater ratio signals that, as herd immunity thresholds are crossed and disease prevalence drops, enforcement machinery persists and expands—a piton-side indicator. Suppression_requirement rises faster than base_extractiveness, indicating that maintaining the mandate requires escalating enforcement intensity even as the coordinate benefit (preventing harm to immunocompromised) stabilizes.
 *
 * PERSPECTIVAL GAP:
 *   From the state's and epidemiologists' seats, this is a rope: genuine collective-action problem (herd immunity requires coordinated high-coverage), voluntary uptake fails, mandates solve it. From the vaccine-hesitant and medically-contraindicated seats, this is a snare: the state compels bodily intervention without consent, uses scientific authority to foreclose objections, and escalates enforcement beyond epidemiological necessity as theater. The engine computes per-seat classification from directionality + suppression + enforcement: powerless targets with high suppression compute as snare-seat; institutional beneficiaries with low suppression compute as rope-seat. The authored claim (tangled_rope) reflects the structural reality: the same constraint is both genuine coordination and extractive coercion, depending on which seat reads it. The seat divergence is the measurement the framework captures.
 *
 * DIRECTIONALITY LOGIC:
 *   Vaccine-hesitant individuals: d ≈ 1.0 (full target). They are powerless, trapped in jurisdiction, face direct coercion, bear bodily administration cost, and have no arbitrage exit. Immunocompromised populations: d ≈ 0.0 (full beneficiary). They benefit from others' coercion without running the coercion machinery, have near-zero exit options (not caused by the constraint but deepened by it), and receive subsidy (protection) from the constraint's operation. Medically_contraindicated_persons: d ≈ 0.65 (asymmetric target). They benefit from herd immunity (coercive regime on others) but face exemption-pathway costs and social suspicion; they are neither fully coerced nor freely protected. State_public_health_authority: d ≈ 0.15 (beneficiary, though structural). They collect authority and control; the constraint expands their institutional reach and generates compliance data and epidemiological legitimacy. The directionality spread is wide and asymmetric—this is the hallmark of tangled rope (genuine coordination function + asymmetric extraction + active enforcement).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint shows rising theater_ratio (0.28 → 0.42) while base_extractiveness plateaus (0.80 → 0.82). This pattern is mandatrophy-adjacent but not a full mandate-obsolescence: the founding problem (achieving herd immunity) remains live and epidemiologically valid, but the measurement pattern suggests enforcement focus is shifting from harm-prevention (epidemiological necessity) to compliance-documentation (theater). The interpretation: the state may be using the legitimate coordination function (herd immunity) as cover for behavioral conformity enforcement that exceeds epidemiological need. The mandatrophy question ('Is the founding problem still live?') is exactly what the six_questions.founding_problem_status disputes—'contested' reflects this divergence. If disease prevalence dropped to zero and herd immunity thresholds were exceeded, but mandates persisted with rising enforcement/theater, mandatrophy would be resolved as 'yes, the founding problem is dead and the constraint is inertial.'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_internalization_vs_structural,
    'What fraction of the measured suppression (0.88) is structural (external barriers: employment, school, travel) versus internalized (individual beliefs, identity fusion, social norm internalization)?',
    'Post-removal observation: if structural enforcement barriers (employment verification, school requirements) were removed while social/identity pressure remained, measure compliance and acceptance in the relaxed regime. If compliance drops sharply, suppression is mostly structural; if it persists, internalization is substantial.',
    'If suppression is >0.5 internalized, the constraint''s effective extractiveness persists even after enforcement machinery is removed—targets carry the suppression themselves. This would lower the responsiveness of the system to regime change and suggest the constraint has identity-fused targets (deeper lock-in than trapped+structural). If mostly structural, removal of enforcement would rapidly de-suppress and restore choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_vs_structural, empirical, 'Decomposition of suppression into structural versus internalized mechanisms').

omega_variable(
    founding_problem_epidemiological_necessity,
    'What is the actual epidemiological threshold at which herd immunity requires state mandate versus voluntary uptake? Is the claimed threshold (the state''s assertion) congruent with independent epidemiological modeling?',
    'Independent peer-reviewed modeling of voluntary-uptake scenarios controlling for education, incentives, and communication; comparison against historical data from voluntary-uptake eras; analysis of jurisdictions with high voluntary uptake (e.g., Nordic countries) versus mandate regimes.',
    'If actual necessary threshold is substantially lower than claimed threshold, the mandate is over-enforcing and using epidemiological legitimacy as cover for behavioral conformity. If thresholds align, the coordination story is empirically grounded. Divergence would support the mandatrophy reading: the founding problem exists but enforcement exceeds necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_epidemiological_necessity, empirical, 'Whether mandated thresholds match epidemiological necessity or exceed it').

omega_variable(
    constitutional_boundary_contest,
    'Is the coercion-legitimacy boundary set by epidemiological fact (harm-prevention necessity) or by constitutional law (state authority limits)? Which regime adjudicates the boundary?',
    'Constitutional court rulings; legislative action; cross-jurisdictional comparative analysis of legal versus epidemiological mandate thresholds.',
    'If epidemiological agencies set the boundary (de facto), the constraint is a tangled_rope with state-authority asymmetry. If constitutional courts set the boundary, the constraint is a snare with extractive authority behind a coordinating mask (courts validate, but can reverse). If legislatures set the boundary, the constraint is more transparent but still coercive. The locus of authority determines how responsive the constraint is to challenge and revision.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_boundary_contest, conceptual, 'Which authority structure (epidemiological, constitutional, legislative) adjudicates coercion legitimacy').

omega_variable(
    bodily_autonomy_foreclosure_risk,
    'Does the public_health_primary reading logically foreclose the bodily_autonomy_primary reading (medical intervention without consent is categorically impermissible), or do they coexist as competing normative frameworks?',
    'Philosophical/constitutional analysis: can one hold that state CAN compel medical intervention in some cases AND that bodily autonomy is categorically inviolable? Or are these mutually exclusive premises?',
    'If FORECLOSED: the two readings cannot coexist in any single legal framework; accepting public_health_primary requires rejecting bodily_autonomy_primary as a foundational claim. This is a zero-sum contest. If COEXISTS: both readings remain live options and the contest is political, not logical. The reading-relations array in cs_structure will be populated based on this resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bodily_autonomy_foreclosure_risk, conceptual, 'Whether public_health_primary and bodily_autonomy_primary are logically foreclosing or coexisting').

omega_variable(
    consent_waiver_scope_creep,
    'Does the medical intervention mandate authorized under public_health_primary evolve to justify coercion in adjacent domains (genetic screening, mental health, nutrition, reproductive decisions)? Is the boundary drift systematic?',
    'Historical tracking of mandate scope expansion; comparative analysis of jurisdictions that adopted public_health_primary coercion frameworks; documentation of subsequent mandate proposals and justifications.',
    'If scope creep is systematic and accelerating, the constraint is functionally a snare whose legitimacy is parasitic on epidemiological emergency but whose actual function is expanding state authority over bodily autonomy. The constraint''s type would not change but its mandatrophy risk would rise: the founding problem (infectious disease harm) stays constant but enforcement scope expands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consent_waiver_scope_creep, empirical, 'Whether public_health_primary coercion authority is contained to epidemiological necessity or drifts into adjacent behavioral control').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__public_health_primary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coer_tr_t0, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 0, 0.28).
narrative_ontology:measurement(coer_tr_t5, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 5, 0.31).
narrative_ontology:measurement(coer_tr_t10, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 10, 0.34).
narrative_ontology:measurement(coer_tr_t15, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 15, 0.37).
narrative_ontology:measurement(coer_tr_t20, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 20, 0.39).
narrative_ontology:measurement(coer_tr_t25, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 25, 0.4).
narrative_ontology:measurement(coer_tr_t30, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 30, 0.41).
narrative_ontology:measurement(coer_tr_t40, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(coer_be_t0, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(coer_be_t5, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 5, 0.68).
narrative_ontology:measurement(coer_be_t10, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 10, 0.73).
narrative_ontology:measurement(coer_be_t15, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 15, 0.78).
narrative_ontology:measurement(coer_be_t20, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 20, 0.8).
narrative_ontology:measurement(coer_be_t25, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 25, 0.81).
narrative_ontology:measurement(coer_be_t30, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 30, 0.82).
narrative_ontology:measurement(coer_be_t40, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 40, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(coer_su_t0, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 0, 0.71).
narrative_ontology:measurement(coer_su_t5, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 5, 0.76).
narrative_ontology:measurement(coer_su_t10, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 10, 0.8).
narrative_ontology:measurement(coer_su_t15, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 15, 0.83).
narrative_ontology:measurement(coer_su_t20, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 20, 0.85).
narrative_ontology:measurement(coer_su_t25, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 25, 0.86).
narrative_ontology:measurement(coer_su_t30, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 30, 0.87).
narrative_ontology:measurement(coer_su_t40, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 40, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__public_health_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(coercion_legitimacy_boundary__public_health_primary, 0.12).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__public_health_primary, coercion_legitimacy_boundary__bodily_autonomy_primary).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__public_health_primary, coercion_legitimacy_boundary__proportionality_reading).

% DUAL FORMULATION NOTE:
% The coercion_legitimacy_boundary kernel decomposes into three structurally distinct constraints, each instantiating a different reading of the same standing question (when may state compel medical intervention?). This story (public_health_primary) frames collective harm-prevention as the dominant legitimacy claim and sets unvaccinated individuals as coerced targets. The bodily_autonomy_primary reading treats bodily integrity as foundational and reframes the same intervention as rights-violating. The proportionality_reading scales coercion legitimacy with disease severity/transmission. All three share a referent (state medical mandates) but exhibit different ε values, beneficiary/victim structures, and suppression mechanisms. The readings are linked by network.affects_constraints; pairwise reading_relations are declared in cs_structure per Rule 4.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(coercion_legitimacy_boundary__public_health_primary, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
