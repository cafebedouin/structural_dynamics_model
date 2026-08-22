% ============================================================================
% CONSTRAINT STORY: public_health_mandate_authority__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_health_mandate_authority__proportionality_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: public_health_mandate_authority__proportionality_reading
 *   human_readable: Public Health Mandate Authority (Proportionality Reading)
 *   domain: public_health/constitutional_rights/bioethics
 *
 * SUMMARY:
 *   A public health authority claiming mandate legitimacy under a
 *   proportionality framework asserts that coercive medical intervention is
 *   justified only when four conditions hold jointly: (1) the pathogenic
 *   threat is severe enough to risk serious population harm, (2) no
 *   less-restrictive alternatives exist, (3) the mandate's coercive scope is
 *   limited to what the threat requires, and (4) the mandate's duration is
 *   tied to the emergency condition. This reading sits between categorical
 *   bodily autonomy (which rejects mandates entirely) and categorical public
 *   health (which treats mandate authority as essentially unconstrained by
 *   threat severity). The constraint story models the proportionality
 *   reading's structural implications: extractiveness is threat-contingent,
 *   victim boundaries shift with available alternatives, and compliance
 *   pressures vary across levels. The claim/metric divergence is
 *   deliberate—the authority frames this as genuine coordination, while
 *   measured extractiveness and suppression suggest asymmetry.
 *
 * KEY AGENTS:
 *   - public_health_authority: Institutional agenda-setter holding mandate power and interpretive discretion over proportionality standards.
 *   - immunocompromised_population: Powerless beneficiaries dependent on collective immunity thresholds.
 *   - vaccine_hesitant_unvaccinated: Moderate-power payers bearing mandate costs with constrained exit options.
 *   - alternative_treatment_excluded_groups: Payers excluded from recognized alternative pathways despite documented efficacy.
 *   - courts_and_constitutional_review: Institutional observer adjudicating whether specific mandates meet proportionality criteria.
 *   - competing_autonomy_advocates: Organized excluded stakeholders rejecting the entire proportionality framework.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__proportionality_reading, 0.58).
domain_priors:suppression_score(public_health_mandate_authority__proportionality_reading, 0.62).
domain_priors:theater_ratio(public_health_mandate_authority__proportionality_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(public_health_mandate_authority__proportionality_reading, "Public Health Mandate Authority (Proportionality Reading)").
narrative_ontology:topic_domain(public_health_mandate_authority__proportionality_reading, "public_health/constitutional_rights/bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__proportionality_reading, '3a7ab05a-4a80-4253-935c-7a3c27a3faa5').
narrative_ontology:cs_kernel_codification('3a7ab05a-4a80-4253-935c-7a3c27a3faa5', formalized).
narrative_ontology:cs_authority_grounding('3a7ab05a-4a80-4253-935c-7a3c27a3faa5', lineage).
narrative_ontology:cs_interpretation_layer_present('3a7ab05a-4a80-4253-935c-7a3c27a3faa5').
narrative_ontology:cs_reading_relation('3a7ab05a-4a80-4253-935c-7a3c27a3faa5', public_health_mandate_authority__bodily_autonomy_reading, coexists_with).
narrative_ontology:cs_reading_relation('3a7ab05a-4a80-4253-935c-7a3c27a3faa5', public_health_mandate_authority__public_health_primary, coexists_with).
narrative_ontology:cs_axiom('3a7ab05a-4a80-4253-935c-7a3c27a3faa5', foundational, threat_severity_calibrates_mandate_legitimacy).
narrative_ontology:cs_axiom_status(threat_severity_calibrates_mandate_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('3a7ab05a-4a80-4253-935c-7a3c27a3faa5', threat_severity_calibrates_mandate_legitimacy, empirically_contingent).
narrative_ontology:cs_axiom('3a7ab05a-4a80-4253-935c-7a3c27a3faa5', foundational, alternatives_constrain_coercion_scope).
narrative_ontology:cs_axiom_status(alternatives_constrain_coercion_scope, holdable).
narrative_ontology:cs_axiom_grounding('3a7ab05a-4a80-4253-935c-7a3c27a3faa5', alternatives_constrain_coercion_scope, deontological).
narrative_ontology:cs_reference_frame('3a7ab05a-4a80-4253-935c-7a3c27a3faa5', proportionality_as_constitutional_constraint).
narrative_ontology:cs_drift_state('3a7ab05a-4a80-4253-935c-7a3c27a3faa5', contemporary_post_mandate_review_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3a7ab05a-4a80-4253-935c-7a3c27a3faa5', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__proportionality_reading, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, immunocompromised_population).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, healthcare_system_capacity).
narrative_ontology:constraint_victim(public_health_mandate_authority__proportionality_reading, vaccine_hesitant_unvaccinated).
narrative_ontology:constraint_victim(public_health_mandate_authority__proportionality_reading, alternative_treatment_excluded_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Declares the necessity and scope of mandates based on epidemiological assessment. Justifies the mandate as protecting collective vulnerability. Possesses the enforcement machinery (licensure revocation, employment restrictions, school exclusions) and interprets what proportionality requires at a given threat level. Maintains discretion to modify or lift mandates as threat conditions change.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, public_health_authority, agenda_setter,
    institutional, generational, analytical, national).

% Depend on collective immunity thresholds to avoid severe or fatal infection when their own immune systems cannot respond adequately to vaccine or infection. A mandate increasing vaccination coverage raises the herd immunity threshold and reduces their exposure risk. They benefit without running the system and cannot exit the requirement—their protection depends entirely on others' compliance.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, immunocompromised_population, beneficiary,
    powerless, biographical, trapped, universal).

% Bear the costs of the mandate: loss of employment, exclusion from schools or public venues, restrictions on professional licensure. Their objections range from bodily autonomy concerns to distrust of public health institutions to preference for alternative prophylaxis. Exit options are limited (relocation to non-mandating jurisdiction, career change, homeschooling) and carry substantial costs. The mandate's enforcement does not track individual risk assessment or availability of alternatives like testing or treatment.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, vaccine_hesitant_unvaccinated, payer,
    moderate, biographical, constrained, national).

% Operates under surge capacity constraints. A mandate reducing infection rates lowers ICU demand, preserves staffing availability, and prevents system collapse. This is treated as a beneficiary (the infrastructure is a 'party' only in the sense that mandate legitimacy explicitly invokes system protection as justification). It does not actively benefit but is the structural object the coordination function protects.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, healthcare_system_capacity, beneficiary,
    institutional, biographical, analytical, national).
narrative_ontology:stakeholder_non_agent(public_health_mandate_authority__proportionality_reading, healthcare_system_capacity).

% Individuals who cite documented prior infection, early treatment protocols, or monoclonal antibody prophylaxis as alternatives to vaccination but are excluded from mandate exemptions by the public health authority's narrow legal definition. They bear mandate costs (employment restrictions, venue exclusion) without recourse to recognized alternatives. Their exclusion depends on the authority's interpretation of what alternatives count.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, alternative_treatment_excluded_groups, payer,
    moderate, biographical, constrained, regional).

% Adjudicate whether a specific mandate meets the proportionality standard. They read the constraint story and ask: Is the threat severe enough to justify this coercion? Have alternatives been tried and found insufficient? Is the duration limited to the emergency condition? Their verdicts determine whether mandates persist or are struck down, but they do not run the mandate themselves.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, courts_and_constitutional_review, observer,
    institutional, generational, analytical, national).

% Would argue that bodily autonomy is inviolable regardless of collective benefit—that the proportionality reading still smuggles in extraction because it permits any mandate above a certain threat threshold. They are excluded from the decision-making process for what the proportionality scale looks like and would reject the entire framework as a cover for state coercion.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, competing_autonomy_advocates, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(public_health_mandate_authority__proportionality_reading, public_health_authority).
narrative_ontology:fixing_cost_class(public_health_mandate_authority__proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Raises population immunity thresholds via mandated intervention to prevent healthcare system collapse and protect those who cannot generate their own immune response. The coordination solves a collective action problem: individual vaccination decisions do not account for externalities (protection of the immunocompromised), so a mandate internalizes those externalities.
% TRANSFER_FUNCTION: Transfers bodily autonomy costs (forced medical intervention, employment restrictions, venue exclusion) from the collective to unvaccinated or hesitant individuals, justified by the benefit of collective immunity. In return, the beneficiary set (immunocompromised, healthcare infrastructure) receives protection they could not secure through voluntary action alone.
% ABSENT_VOICES: Bodily autonomy advocates who reject proportionality frameworks altogether are structurally excluded—they would argue the reading itself is extraction dressed as precision. Alternative-treatment practitioners and early-intervention proponents are excluded from the exemption-definition process. Individuals in constituencies with historical medical discrimination are excluded from framing whether they trust the authority conducting the mandate.
% DISAPPEARANCE_RATIONALE: If the mandate authority and its enforcement vanished, unvaccinated individuals would regain employment and venue access; immunocompromised populations would lose the collective immunity buffer and face heightened exposure risk; the healthcare system would face surge capacity pressure. The population's immunity profile would reflect voluntary vaccination plus natural infection, likely lower than mandate-maintained levels during high-threat phases.
% FOUNDING_PROBLEM: Pathogenic threats severe enough to risk healthcare system collapse and threaten populations unable to mount immune responses require rapid population-level interventions that voluntary individual decisions cannot achieve. The founding problem is collective-action failure in epidemic response.
% FOUNDING_PROBLEM_CORROBORATION: Public health epidemiologists attest that severe pathogens (Ebola, early COVID-19 variants) create authentic collective-action problems and documented system-collapse risk. Bodily autonomy advocates attest that the framing of the 'problem' as requiring non-consensual intervention is itself a choice and treats collective benefit as overriding individual sovereignty. Courts in multiple jurisdictions have validated that epidemic response powers exist but diverge on proportionality standards; no consensus corroboration exists outside the benefiting institutional seats.
narrative_ontology:disappearance_verdict(public_health_mandate_authority__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(public_health_mandate_authority__proportionality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(public_health_mandate_authority__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(public_health_mandate_authority__proportionality_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_health_mandate_authority__proportionality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(public_health_mandate_authority__proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(public_health_mandate_authority__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness sits at 0.58 (mid-range, threat-contingent) because extractiveness depends on the threat level and duration—at interval start (mild threat, early phase) it is 0.28; at peak (severe threat, active emergency) it reaches 0.62; then declines as threat recedes. Suppression is higher (0.62) than extractiveness because mandate compliance is enforced through employment restrictions, licensure revocation, and venue exclusion, not merely through persuasion—these are active coercive mechanisms. Theater is moderate (0.28) because the security/public-health function is genuine (immunocompromised protection is real) but grows as the threat recedes (the same enforcement machinery persists even as the justifying condition weakens). Accessibility_collapse is high (0.72) because once a mandate is in effect, alternatives (relocation, career change, homeschooling) carry enormous costs and are effectively unavailable to most individuals. Resistance is also high (0.71) because the constraint meets sustained organizational and class-level pushback from autonomy advocates and hesitant communities. The measurements track a cyclical arc: extractiveness and suppression rise with threat severity, peak at months 24-36, then decline as the emergency condition recedes—but theater_ratio rises monotonically, indicating the constraint's enforcement outlasts its functional justification.
 *
 * PERSPECTIVAL GAP:
 *   From the public_health_authority seat, the mandate is a proportionality-constrained coordination mechanism that protects the immunocompromised and healthcare system—extractiveness is low and suppression is justifiable. From the vaccine_hesitant seat, the same structure is enforced coercion whose justification (proportionality calibration) remains opaque and subject to authority discretion—extractiveness is high and suppression is experienced as arbitrary. From the courts seat, the structure is a test case: does the authority's specific mandate meet the proportionality standard, or has it exceeded the threat? From the autonomy advocates seat, the entire reading is a legitimation device—proportionality is extraction dressed as precision. The engine computes these divergences from the structural data: beneficiary/victim declarations, power atoms, exit options, and directionality overrides.
 *
 * DIRECTIONALITY LOGIC:
 *   The public_health_authority is the agenda-setter (d near 0.5, symmetric position—it coordinates a real function and extracts enforcement discretion). The immunocompromised_population are beneficiaries with trapped exit (d near 0.0); they benefit from the mandate and cannot exit. The vaccine_hesitant_unvaccinated are payers with constrained exit (d near 0.8); they bear the mandate's costs and have limited options to exit. The alternative_treatment_excluded_groups are also payers (d near 0.85); they have documented alternatives available in other jurisdictions but are excluded by the authority's narrow definition, effectively forced into compliance. Healthcare_system_capacity is treated as a beneficiary (institutional, non-agent entry) because mandate legitimacy explicitly invokes system protection. The threat_level (pathogenic severity) is a non-agent entry—it is the scale on which the reading's proportionality pivots. No directionality override is needed; structural derivation from beneficiary/victim + exit captures the relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (collective-action failure in epidemic response) is live during high-threat phases (CFR > 1%, healthcare surge risk documented) and contested once threat recedes. At month 48, the constraint's functional justification has faded—threat severity is low, alternatives exist, and system capacity is adequate—but the constraint persists due to institutional inertia (licensing restrictions remain in place, venue exclusions continue, employment penalties do not reverse). Theater_ratio rises from 0.12 to 0.28 over the interval, indicating that enforcement activity increasingly defends the constraint's existence rather than the emergency condition that justified it. This is mandatrophy on the way—a constraint whose founding problem is dead or contested but whose machinery persists, increasingly performative. The proportionality reading is supposed to prevent mandatrophy by tying mandate legitimacy to threat severity; if the reading is operationalized (thresholds established, alternatives recognized), mandatrophy is arrested; if not (proportionality remains opaque), the reading collapses and mandatrophy persists. The omegas document this risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_calibration_ambiguity,
    'What threat severity threshold justifies what magnitude of coercion and duration of mandate under the proportionality reading? Where does a mild respiratory virus (CFR 0.1%) sit on the sliding scale relative to an Ebola-like pathogen (CFR > 50%)?',
    'Comparative analysis across jurisdictions with explicit proportionality frameworks (e.g., EU Charter proportionality tests, Canadian Charter s.1); case-by-case adjudication establishing precedent thresholds; public health guidelines articulating threat-to-coercion mappings.',
    'If no clear threshold exists, the proportionality reading collapses into subjective judgment by the authority, making it difficult to distinguish from public-health-primary (the beneficiary reading). A well-calibrated scale makes the reading operationally distinct—and potentially constrains authority discretion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_calibration_ambiguity, conceptual, 'Whether proportionality can be operationalized or remains a vacuous legitimacy claim.').

omega_variable(
    alternative_availability_definition,
    'What counts as an available alternative to mandate-mandated vaccination? Do prior infection, early treatment access, regular testing, or monoclonal antibody prophylaxis sufficiently reduce mandate necessity?',
    'Epidemiological modeling of alternative pathways'' effectiveness; regulatory decisions on exemption criteria; court verdicts on whether alternatives have been adequately explored before coercion.',
    'If alternatives are recognized broadly, mandate extractiveness drops (coercion becomes avoidable); if narrowly, extractiveness stays high (alternatives are de-facto unavailable to the payer set). Victim boundary shifts—those with access to recognized alternatives move closer to the beneficiary side; those with access denied stay as payers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_availability_definition, empirical, 'Which alternatives displace mandate necessity and how they are legally recognized.').

omega_variable(
    structural_identity_lock_hesitant,
    'For vaccine-hesitant individuals, is non-compliance a constrained exit or an identity-locked choice? To what extent is hesitancy rooted in structural barriers to trust, and to what extent in identity-fusion with anti-mandate positions?',
    'Post-mandate exit trajectory: if hesitancy persists after mandate removal and alternative information access, identity-lock is partial; if it reverses, hesitancy was structurally suppressed. Qualitative research on reasons for non-compliance.',
    'If identity-locked, suppression measurement is higher than structural barriers alone suggest—the constraint carries its suppression with it past exit. If structurally constrained (barriers to vaccine access, provider distrust from historical medical discrimination), removal of the mandate but not the barriers leaves suppression intact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_identity_lock_hesitant, empirical, 'Whether vaccine hesitancy is identity-locked or structurally constrained, and thus what suppression persists beyond mandate removal.').

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the public_health_mandate_authority kernel. Is the proportionality reading a genuine third position, or does it collapse into one of the sibling readings (bodily_autonomy_primary or public_health_primary) under scrutiny?',
    'Test cases where proportionality would permit mandates that autonomy rejects and prohibit mandates that public-health endorses. If such cases exist and are adjudicated, the reading is structurally distinct. If real-world application converges on one sibling''s conclusion, the proportionality framework is performative rather than constraining.',
    'If proportionality collapses, mandates persist not because they are legitimated by a sliding scale but because the authority interprets proportionality to match its public-health goal (reading_relations=''coexists_with'' → ''influences'' → functionally ''forecloses'' through institutional capture). This omega documents the reading''s operational status.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether proportionality is a binding constraint on mandate authority or a cover story.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__proportionality_reading, 0, 48).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(publ_tr_t0, public_health_mandate_authority__proportionality_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(publ_tr_t6, public_health_mandate_authority__proportionality_reading, theater_ratio, 6, 0.15).
narrative_ontology:measurement(publ_tr_t12, public_health_mandate_authority__proportionality_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(publ_tr_t24, public_health_mandate_authority__proportionality_reading, theater_ratio, 24, 0.28).
narrative_ontology:measurement(publ_tr_t36, public_health_mandate_authority__proportionality_reading, theater_ratio, 36, 0.32).
narrative_ontology:measurement(publ_tr_t48, public_health_mandate_authority__proportionality_reading, theater_ratio, 48, 0.28).

% Extraction over time
narrative_ontology:measurement(publ_be_t0, public_health_mandate_authority__proportionality_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(publ_be_t6, public_health_mandate_authority__proportionality_reading, base_extractiveness, 6, 0.35).
narrative_ontology:measurement(publ_be_t12, public_health_mandate_authority__proportionality_reading, base_extractiveness, 12, 0.48).
narrative_ontology:measurement(publ_be_t24, public_health_mandate_authority__proportionality_reading, base_extractiveness, 24, 0.58).
narrative_ontology:measurement(publ_be_t36, public_health_mandate_authority__proportionality_reading, base_extractiveness, 36, 0.62).
narrative_ontology:measurement(publ_be_t48, public_health_mandate_authority__proportionality_reading, base_extractiveness, 48, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(publ_su_t0, public_health_mandate_authority__proportionality_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(publ_su_t6, public_health_mandate_authority__proportionality_reading, suppression_requirement, 6, 0.45).
narrative_ontology:measurement(publ_su_t12, public_health_mandate_authority__proportionality_reading, suppression_requirement, 12, 0.58).
narrative_ontology:measurement(publ_su_t24, public_health_mandate_authority__proportionality_reading, suppression_requirement, 24, 0.68).
narrative_ontology:measurement(publ_su_t36, public_health_mandate_authority__proportionality_reading, suppression_requirement, 36, 0.65).
narrative_ontology:measurement(publ_su_t48, public_health_mandate_authority__proportionality_reading, suppression_requirement, 48, 0.62).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=48
narrative_ontology:measurement(publ_grid_01, public_health_mandate_authority__proportionality_reading, accessibility_collapse(class), 0, 0.62).
narrative_ontology:measurement(publ_grid_02, public_health_mandate_authority__proportionality_reading, accessibility_collapse(class), 48, 0.75).
narrative_ontology:measurement(publ_grid_03, public_health_mandate_authority__proportionality_reading, accessibility_collapse(individual), 0, 0.55).
narrative_ontology:measurement(publ_grid_04, public_health_mandate_authority__proportionality_reading, accessibility_collapse(individual), 48, 0.68).
narrative_ontology:measurement(publ_grid_05, public_health_mandate_authority__proportionality_reading, accessibility_collapse(organizational), 0, 0.58).
narrative_ontology:measurement(publ_grid_06, public_health_mandate_authority__proportionality_reading, accessibility_collapse(organizational), 48, 0.72).
narrative_ontology:measurement(publ_grid_07, public_health_mandate_authority__proportionality_reading, accessibility_collapse(structural), 0, 0.68).
narrative_ontology:measurement(publ_grid_08, public_health_mandate_authority__proportionality_reading, accessibility_collapse(structural), 48, 0.78).
narrative_ontology:measurement(publ_grid_09, public_health_mandate_authority__proportionality_reading, resistance(class), 0, 0.65).
narrative_ontology:measurement(publ_grid_10, public_health_mandate_authority__proportionality_reading, resistance(class), 48, 0.74).
narrative_ontology:measurement(publ_grid_11, public_health_mandate_authority__proportionality_reading, resistance(individual), 0, 0.62).
narrative_ontology:measurement(publ_grid_12, public_health_mandate_authority__proportionality_reading, resistance(individual), 48, 0.68).
narrative_ontology:measurement(publ_grid_13, public_health_mandate_authority__proportionality_reading, resistance(organizational), 0, 0.58).
narrative_ontology:measurement(publ_grid_14, public_health_mandate_authority__proportionality_reading, resistance(organizational), 48, 0.72).
narrative_ontology:measurement(publ_grid_15, public_health_mandate_authority__proportionality_reading, resistance(structural), 0, 0.72).
narrative_ontology:measurement(publ_grid_16, public_health_mandate_authority__proportionality_reading, resistance(structural), 48, 0.78).
narrative_ontology:measurement(publ_grid_17, public_health_mandate_authority__proportionality_reading, stakes_inflation(class), 0, 0.32).
narrative_ontology:measurement(publ_grid_18, public_health_mandate_authority__proportionality_reading, stakes_inflation(class), 48, 0.52).
narrative_ontology:measurement(publ_grid_19, public_health_mandate_authority__proportionality_reading, stakes_inflation(individual), 0, 0.22).
narrative_ontology:measurement(publ_grid_20, public_health_mandate_authority__proportionality_reading, stakes_inflation(individual), 48, 0.38).
narrative_ontology:measurement(publ_grid_21, public_health_mandate_authority__proportionality_reading, stakes_inflation(organizational), 0, 0.28).
narrative_ontology:measurement(publ_grid_22, public_health_mandate_authority__proportionality_reading, stakes_inflation(organizational), 48, 0.45).
narrative_ontology:measurement(publ_grid_23, public_health_mandate_authority__proportionality_reading, stakes_inflation(structural), 0, 0.35).
narrative_ontology:measurement(publ_grid_24, public_health_mandate_authority__proportionality_reading, stakes_inflation(structural), 48, 0.58).
narrative_ontology:measurement(publ_grid_25, public_health_mandate_authority__proportionality_reading, suppression(class), 0, 0.42).
narrative_ontology:measurement(publ_grid_26, public_health_mandate_authority__proportionality_reading, suppression(class), 48, 0.65).
narrative_ontology:measurement(publ_grid_27, public_health_mandate_authority__proportionality_reading, suppression(individual), 0, 0.32).
narrative_ontology:measurement(publ_grid_28, public_health_mandate_authority__proportionality_reading, suppression(individual), 48, 0.58).
narrative_ontology:measurement(publ_grid_29, public_health_mandate_authority__proportionality_reading, suppression(organizational), 0, 0.38).
narrative_ontology:measurement(publ_grid_30, public_health_mandate_authority__proportionality_reading, suppression(organizational), 48, 0.62).
narrative_ontology:measurement(publ_grid_31, public_health_mandate_authority__proportionality_reading, suppression(structural), 0, 0.48).
narrative_ontology:measurement(publ_grid_32, public_health_mandate_authority__proportionality_reading, suppression(structural), 48, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_mandate_authority__proportionality_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(public_health_mandate_authority__proportionality_reading, 0.12).
narrative_ontology:affects_constraint(public_health_mandate_authority__proportionality_reading, public_health_mandate_authority__bodily_autonomy_reading).
narrative_ontology:affects_constraint(public_health_mandate_authority__proportionality_reading, public_health_mandate_authority__public_health_primary_reading).
narrative_ontology:affects_constraint(public_health_mandate_authority__proportionality_reading, vaccine_licensing_restrictions).
narrative_ontology:affects_constraint(public_health_mandate_authority__proportionality_reading, employment_based_mandate_enforcement).

% DUAL FORMULATION NOTE:
% The public_health_mandate_authority kernel decomposes into three structurally distinct constraint stories. The proportionality_reading instantiates a middle position that constrains both categorical readings. All three stories share the same kernel_codification (formalized—constitutional authority) and authority_grounding (lineage—interpreted from constitutional text and precedent) but diverge in reference_frame and drift_state. The proportionality reading's distinctiveness lies in its axioms (threat-contingency and alternative-availability constraints) and reading_relations (coexists_with, influences both siblings). Network edges link the reading to its siblings and to downstream constraints (employment enforcement, licensing restrictions) that operationalize mandates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(public_health_mandate_authority__proportionality_reading, institutional, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
