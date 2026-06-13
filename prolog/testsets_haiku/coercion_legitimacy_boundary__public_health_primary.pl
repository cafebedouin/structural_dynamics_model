% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: coercion_legitimacy_boundary__public_health_primary
 *   human_readable: State Compulsion for Collective Health Protection (Public Health Primary Reading)
 *   domain: public_health/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint instantiates the public-health-primary reading of the
 *   coercion-legitimacy-boundary kernel. Under this reading, the state is
 *   authorized to compel medical intervention (vaccination, quarantine,
 *   treatment) when public health epidemiology demonstrates that the
 *   collective harm of disease transmission outweighs the individual autonomy
 *   cost of coercion. Unvaccinated individuals are positioned as targets of
 *   enforcement; immunocompromised populations are positioned as
 *   beneficiaries protected by herd immunity. This reading directly contests
 *   the bodily-autonomy-primary reading (which treats consent as
 *   categorically inviolable) and is influenced by but structurally distinct
 *   from the proportionality reading (which permits coercion only when
 *   disease severity justifies it, not categorically). The constraint's
 *   measured extractiveness (0.72 at interval end) reflects the high autonomy
 *   cost borne by unvaccinated individuals; suppression intensity (0.81)
 *   reflects active institutional enforcement through occupational and
 *   educational exclusion. The constraint is CLAIMED as tangled rope (genuine
 *   herd immunity coordination problem + asymmetric extraction of autonomy
 *   from unvaccinated to immunocompromised) — the measurement series track
 *   enforcement intensification and slow consolidation as hesitancy clusters
 *   are brought into compliance.
 *
 * KEY AGENTS:
 *   - public_health_authority: Institutional agenda-setter; sets mandates based on epidemiological thresholds; claims scientific necessity.
 *   - immunocompromised_populations: Powerless beneficiaries; depend entirely on herd immunity for protection; cannot exit or object.
 *   - unvaccinated_individuals_subject_to_mandate: Moderate-power payers; face occupational and educational exclusion; constrained exit (relocation, homeschooling, retraining).
 *   - vaccine_hesitant_populations: Identity-locked payers; experience mandate as coercion of conscience and bodily ownership; exit would require abandoning core commitments.
 *   - civil_liberties_advocates: Excluded from mandate deliberation; argue bodily autonomy is inviolable regardless of collective benefit; their objections surface only through litigation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__public_health_primary, 0.72).
domain_priors:suppression_score(coercion_legitimacy_boundary__public_health_primary, 0.81).
domain_priors:theater_ratio(coercion_legitimacy_boundary__public_health_primary, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, extractiveness, 0.72).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__public_health_primary, tangled_rope).
narrative_ontology:human_readable(coercion_legitimacy_boundary__public_health_primary, "State Compulsion for Collective Health Protection (Public Health Primary Reading)").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__public_health_primary, "public_health/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(coercion_legitimacy_boundary__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__public_health_primary, '2401cb86-4df6-4183-bd9c-c280d035cd16').
narrative_ontology:cs_kernel_codification('2401cb86-4df6-4183-bd9c-c280d035cd16', formalized).
narrative_ontology:cs_authority_grounding('2401cb86-4df6-4183-bd9c-c280d035cd16', lineage).
narrative_ontology:cs_interpretation_layer_present('2401cb86-4df6-4183-bd9c-c280d035cd16').
narrative_ontology:cs_reading_relation('2401cb86-4df6-4183-bd9c-c280d035cd16', coercion_legitimacy_boundary__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('2401cb86-4df6-4183-bd9c-c280d035cd16', coercion_legitimacy_boundary__proportionality_reading, influences).
narrative_ontology:cs_axiom('2401cb86-4df6-4183-bd9c-c280d035cd16', foundational, collective_harm_prevention_outweighs_individual_autonomy).
narrative_ontology:cs_axiom_status(collective_harm_prevention_outweighs_individual_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('2401cb86-4df6-4183-bd9c-c280d035cd16', collective_harm_prevention_outweighs_individual_autonomy, empirically_contingent).
narrative_ontology:cs_axiom('2401cb86-4df6-4183-bd9c-c280d035cd16', foundational, epidemiological_necessity_grounds_coercion_legitimacy).
narrative_ontology:cs_axiom_status(epidemiological_necessity_grounds_coercion_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('2401cb86-4df6-4183-bd9c-c280d035cd16', epidemiological_necessity_grounds_coercion_legitimacy, empirically_contingent).
narrative_ontology:cs_reference_frame('2401cb86-4df6-4183-bd9c-c280d035cd16', state_police_power_medical_necessity).
narrative_ontology:cs_drift_state('2401cb86-4df6-4183-bd9c-c280d035cd16', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2401cb86-4df6-4183-bd9c-c280d035cd16', '').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__public_health_primary, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, immunocompromised_populations).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, infants_ineligible_for_vaccination).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, collective_herd_immunity_threshold).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__public_health_primary, unvaccinated_individuals_subject_to_mandate).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__public_health_primary, vaccine_hesitant_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, infants_and_unvaccinated_ineligible).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, vaccine_hesitant_populations).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, healthcare_providers).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__public_health_primary, unvaccinated_disease_transmitters).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__public_health_primary, state_police_power_doctrine).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__public_health_primary, collective_benefit_supremacy_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces vaccination mandates for disease control, justified by epidemiological modeling showing transmission thresholds below which herd immunity breaks down. Administers the mandate through licensing boards, school enrollment requirements, and occupational health codes. Claims scientific necessity; faces litigation and resistance from bodily autonomy advocates.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, public_health_authority, agenda_setter,
    institutional, generational, analytical, national).

% Cannot receive vaccines due to medical contraindication (immunosuppressive medication, organ transplants, certain genetic conditions). Depend entirely on surrounding vaccination rates to avoid exposure to preventable diseases. A mandate protecting herd immunity is the only mechanism by which they access protection; without it, they face isolation or disease risk.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, immunocompromised_populations, beneficiary,
    powerless, biographical, trapped, national).

% Infants too young for vaccination and individuals with genuine medical contraindications depend on herd immunity thresholds. Cannot advocate for themselves; protection is wholly passive, contingent on surrounding vaccination rates maintained by mandate.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, infants_and_unvaccinated_ineligible, beneficiary,
    powerless, immediate, trapped, national).

% Subject to vaccination requirements for school enrollment, employment in healthcare/education, or public institution access. Face penalties: school exclusion, occupational disqualification, loss of licensure, or fines. Exit options are limited: geographic relocation to non-mandate jurisdictions, homeschooling, or occupational retrain. The constraint treats vaccine refusal as a coercive target regardless of individual risk calculus or medical history.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, unvaccinated_individuals_subject_to_mandate, payer,
    moderate, biographical, constrained, national).

% Hold religious, philosophical, or distrust-based objections to vaccination. Mandates override their decision-making autonomy and are experienced as state coercion of conscience and bodily integrity. Identity-locked because medical autonomy is integrated into their worldview; exit would require repudiating core commitments about body ownership and informed consent. Some derive secondary benefit from herd immunity but experience the mandate as illegitimate.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, vaccine_hesitant_populations, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(coercion_legitimacy_boundary__public_health_primary, vaccine_hesitant_populations, beneficiary).

% Implement vaccination requirements for staff and patients as public health policy. Benefit from reduced disease transmission in clinical settings and protection of vulnerable patients. Also face ethical tension: the mandate constrains informed-consent protocols they traditionally rely on, creating institutional dissonance.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, healthcare_providers, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(coercion_legitimacy_boundary__public_health_primary, healthcare_providers, beneficiary).

% Provide the scientific case for herd immunity thresholds and disease transmission dynamics. Their models justify the mandate's necessity but do not resolve the normative question of whether science-based necessity licenses state coercion. Occupy an ambiguous seat: they produce the technical premises but are not the authority that invokes those premises to justify coercion.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, public_health_epidemiologists, observer,
    institutional, generational, analytical, global).

% Argue that bodily autonomy is inviolable regardless of collective benefit and that proportionality safeguards (severity-based mandates, sunset clauses, least-restrictive alternatives) are required. Are systematically excluded from mandate-setting deliberations; policy is set by public health and institutional authority without regular consultation with autonomy advocates. Their objections surface only through litigation and legislative testimony.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, civil_liberties_advocates, excluded,
    organized, generational, analytical, national).

% Below herd immunity threshold, unvaccinated individuals become the vector through which preventable disease circulates. From the public health frame, they are targets of intervention; from the bodily autonomy frame, they are autonomous agents. The mandate treats the role as involuntary: they neither choose to transmit nor benefit from transmission, but bear the coercive cost of being positioned in the transmission chain.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, unvaccinated_disease_transmitters, payer,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(coercion_legitimacy_boundary__public_health_primary, public_health_authority).
narrative_ontology:fixing_cost_class(coercion_legitimacy_boundary__public_health_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains herd immunity thresholds (typically 85-95% coverage depending on disease) such that vulnerable populations who cannot be vaccinated (immunocompromised, infants, contraindication-bearing individuals) are protected through passive immunity. Solves a genuine coordination problem: voluntary vaccination alone under-supplies herd immunity because rational individuals face low personal disease risk relative to perceived vaccine risk and free-ride on others' vaccination.
% TRANSFER_FUNCTION: Moves coercive burden from the state onto unvaccinated individuals (via exclusion, occupational disqualification, fines) and transfers protection to immunocompromised and too-young populations who cannot self-protect. The constraint extracts bodily autonomy and decision-making freedom from unvaccinated persons and deposits protective benefit into immunocompromised beneficiaries.
% ABSENT_VOICES: Individuals with genuine medical contraindications who support vaccination on principle but cannot receive vaccines occupy an ambiguous seat (neither full beneficiary nor full victim). Civil liberties advocates are structurally excluded from mandate deliberation; they argue bodily autonomy is categorically inviolable and that less-restrictive alternatives (infection-risk disclosure, proportionality gates, sunset provisions) should mediate collective benefit. Vaccine-hesitant populations holding philosophical objections are also excluded from standard deliberation; their objections are treated as misinformation rather than legitimate dissent.
% DISAPPEARANCE_RATIONALE: If the mandate apparatus vanished, vaccination coverage would drop below herd immunity thresholds within 2-3 seasons (historical precedent: vaccine-hesitant clusters show rapid reversion without institutional pressure). Disease transmission would increase in vulnerable populations; immunocompromised individuals would face heightened isolation or illness risk; infants would lose passive protection. The organizational landscape would reorganize: schools would face disease outbreaks, healthcare systems would see increased complications, and policy would likely reinstate mandates in response to visible harm. The constraint's disappearance would trigger institutional reorganization around disease control.
% FOUNDING_PROBLEM: Measles and polio eliminated from most developed populations by mid-20th century through near-universal vaccination, enabling a naive generation to question whether vaccine-preventable diseases remain genuine threats. Rising vaccine hesitancy due to distrust narratives, alternative medicine movements, and internet misinformation reduced coverage below herd immunity in clusters (California, Somali American communities, UK MMR declines). Disease re-emergence (measles outbreaks 2019, rubella in unvaccinated populations) demonstrated the founding problem remained live: absent institutional enforcement, voluntary coverage falls below herd immunity and vulnerable populations face preventable disease.
% FOUNDING_PROBLEM_CORROBORATION: Public health authorities and epidemiological bodies (CDC, WHO, national public health agencies) attest the founding problem is live: ongoing disease circulation in unvaccinated populations demonstrates the coordination failure. Civil liberties and bodily autonomy advocates contest the framing: they argue the founding problem is overstated (disease risk is low in developed nations) and that institutional policy should track proportionality (mandate measles, not influenza) rather than categorical coercion. Medical ethicists and disability advocates outside the public health authority add nuance: the problem is real but the solution's scope is contested — they suggest sunset clauses, severity-based proportionality, and explicit harm-prevention thresholds. Legislative testimony from both seats (public health testimony on disease transmission; civil liberties testimony on autonomy costs) represents the genuine dispute.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__public_health_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__public_health_primary, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(coercion_legitimacy_boundary__public_health_primary, 'none', 1).

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
 *   Extractiveness is high (0.72) and rising through the interval (0.58 → 0.72) because the enforcement apparatus expands: initial mandates for healthcare workers and school enrollment (lower coverage ≈ lower coercive scope) expand to occupational licensing, public institution access, and eventually employment-conditional vaccination (higher coverage ≈ higher coercive scope). The measurement series shows extractiveness plateauing around year 25 when near-universal mandates are achieved and enforcement effort stabilizes. Suppression is higher than extractiveness (0.81) because it represents the active machinery of exclusion (licensing board review, school enrollment denial, occupational disqualification) that sustains the mandate against resistance. Theater ratio is low (0.28) and grows modestly: the coordination function (herd immunity maintenance) is genuine, but the ratio rises over time as disease risk declines in vaccinated populations and enforcement becomes more about maintaining institutional authority than preventing visible outbreaks. One shared time grid across all three metrics; every metric authored at every examined time point (0, 5, 10, 15, 20, 25, 30, 40).
 *
 * PERSPECTIVAL GAP:
 *   The public health authority computes this constraint as rope (genuine coordination of herd immunity + coercive overhead necessary to supply coordination). Unvaccinated individuals compute it as snare (no genuine coordination benefit for them; extraction of autonomy without reciprocal protection). Immunocompromised populations compute it as pure benefit (rope-to-them, with them as passive beneficiaries). Vaccine-hesitant populations compute it as snare-with-identity-fusion (coercion of conscience, internalized suppression that persists after mandate removal). The engine computes per-seat types from the structural data; the seat divergence is the measurement the framework exists to capture — this is WHY the per-seat computation is essential. From one seat, coordination; from another, extraction; from a third, protection.
 *
 * DIRECTIONALITY LOGIC:
 *   The public health authority is the structural agenda-setter with institutional power and analytical exit — high agency, low constraint. Immunocompromised beneficiaries are powerless, trapped, and entirely passive recipients of protection — low agency, full dependence. Unvaccinated individuals are moderately powerful (some have professional credentials, social capital) but face constrained exit (relocation or occupational retrain are costly but possible) — they sit at moderate d. Vaccine-hesitant populations are identity-locked: medical autonomy is fused with their identity framework such that accepting the mandate requires abandoning core commitments. This identity-lock amplifies suppression: they carry the suppression internally even after a mandate ends (a post-exit trajectory signal for reclassification). The constraint's directionality diverges sharply across seats: from the public health authority, d ≈ 0.1 (this constraint subsidizes their institutional authority); from unvaccinated individuals, d ≈ 0.8 (extraction of autonomy). From immunocompromised populations, d ≈ 0.0 (pure beneficiary; they bear no coercive cost, only reap protection).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids the false tangled-rope trap (claiming coordination when it is pure extraction) by explicitly naming both the beneficiaries (immunocompromised, infants, vulnerable populations) AND the victims (unvaccinated individuals with constrained exit, identity-locked populations). The coordination function is real — unvaccinated individuals do free-ride on herd immunity in one sense — but the coercive machinery is disproportionate: the mandate treats all unvaccinated individuals as vectors rather than as agents with heterogeneous risk profiles, medical histories, or autonomous medical judgment. Mandatrophy would arise if the founding problem (disease re-emergence) became dead (disease eradicated, herd immunity sustained through voluntary uptake) while the enforcement apparatus persisted unchanged. Currently founding_problem_status is contested: public health authorities argue disease remains live; civil liberties advocates argue it is overstated and that proportionality (mandate for severe diseases, not influenza) should mediate scope. This contestation is the sign that the constraint has not yet achieved mandatrophy — the founding problem is still live for one major stakeholder set, even if disputed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_internalization_mechanism,
    'In vaccine-hesitant populations, is the measured suppression (0.81) structural (external penalties: occupational exclusion, school denial, fines) or internalized (identity-fusion that persists after mandate removal: belief that accepting coercion violates bodily autonomy)?',
    'Post-mandate trajectory analysis: if suppression persists after the mandate is removed (populations remaining vaccine-hesitant despite lifted institutional barriers), reclassify as substantially internalized. If suppression drops sharply when barriers are removed, reclassify as primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — targets carry the suppression with them post-exit, reducing their ability to reassess risk independently. This would support reclassification from tangled-rope to snare (pure extraction with internalized suppression preventing exit recalculation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_mechanism, empirical, 'Whether vaccine hesitancy suppression is structural or identity-fused.').

omega_variable(
    herd_immunity_threshold_as_natural_law,
    'Is the herd immunity threshold (typically 85-95% for measles) a natural law of disease transmission, or a constructed epidemiological estimate dependent on modeling assumptions and disease parameters?',
    'Meta-analysis of herd immunity threshold estimates across diseases and populations; sensitivity analysis of threshold estimates to parameter variation; comparison of predicted vs. observed outbreak patterns.',
    'If natural law (threshold is invariant across populations and modeling frameworks), the mandate gains framing as defending a natural boundary, reducing measured extractiveness. If constructed (threshold is parameter-dependent and subject to revision), the mandate appears more discretionary and extractive — the authority is choosing a threshold, not enforcing a physical limit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(herd_immunity_threshold_as_natural_law, empirical, 'Whether herd immunity thresholds are discovered natural facts or constructed epidemiological estimates.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the public_health_primary reading logically foreclose the bodily_autonomy_primary reading, or do the two readings represent genuinely irreconcilable normative commitments held by different parties to an ongoing dispute?',
    'Jurisprudential analysis: examine whether any legal framework has attempted to hold both readings simultaneously (e.g., permit coercion for catastrophic diseases, prohibit coercion for minor diseases). If sustained coexistence is coherent, the readings coexist; if legal frameworks consistently choose one over the other, they foreclose.',
    'If the readings foreclose (one must be true if the other is true), the engine would mark them as mutually incompatible and flag any authority holding both as incoherent. If the readings coexist, they represent a genuinely contested boundary that different parties defend simultaneously, and no foreclosure gate triggers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether the two readings are logically incompatible or represent a contested boundary.').

omega_variable(
    proportionality_scale_vs_categorical_permission,
    'Under the public_health_primary reading, is there a severity floor below which coercion is impermissible (e.g., vaccination mandates are legitimate for measles but not influenza), or is the reading fully categorical (coercion is permitted whenever epidemiology justifies herd immunity)?',
    'Policy analysis of actual mandate scope: do jurisdictions mandate vaccination only for severe diseases, or for all vaccine-preventable diseases including low-severity ones? If scope tracks severity, a proportionality floor is operative even within public_health_primary; if scope is categorical regardless of severity, the reading is fully categorical.',
    'If a floor exists, the public_health_primary reading is constrained by proportionality and does not fully foreclose proportionality_reading; the two readings would coexist rather than the first influencing the second. If no floor exists, public_health_primary is categorically permissive and the proportionality reading must argue for a floor that public_health_primary denies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_scale_vs_categorical_permission, empirical, 'Whether public_health_primary is subject to a severity-based proportionality floor.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__public_health_primary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coer_tr_t0, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(coer_tr_t0, observed).
narrative_ontology:measurement(coer_tr_t5, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 5, 0.15).
narrative_ontology:measurement_basis(coer_tr_t5, observed).
narrative_ontology:measurement(coer_tr_t10, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 10, 0.18).
narrative_ontology:measurement_basis(coer_tr_t10, observed).
narrative_ontology:measurement(coer_tr_t15, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 15, 0.21).
narrative_ontology:measurement_basis(coer_tr_t15, observed).
narrative_ontology:measurement(coer_tr_t20, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 20, 0.24).
narrative_ontology:measurement_basis(coer_tr_t20, observed).
narrative_ontology:measurement(coer_tr_t25, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 25, 0.27).
narrative_ontology:measurement_basis(coer_tr_t25, observed).
narrative_ontology:measurement(coer_tr_t30, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(coer_tr_t30, observed).
narrative_ontology:measurement(coer_tr_t40, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(coer_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(coer_be_t0, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(coer_be_t0, observed).
narrative_ontology:measurement(coer_be_t5, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 5, 0.62).
narrative_ontology:measurement_basis(coer_be_t5, observed).
narrative_ontology:measurement(coer_be_t10, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 10, 0.66).
narrative_ontology:measurement_basis(coer_be_t10, observed).
narrative_ontology:measurement(coer_be_t15, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 15, 0.69).
narrative_ontology:measurement_basis(coer_be_t15, observed).
narrative_ontology:measurement(coer_be_t20, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 20, 0.71).
narrative_ontology:measurement_basis(coer_be_t20, observed).
narrative_ontology:measurement(coer_be_t25, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 25, 0.72).
narrative_ontology:measurement_basis(coer_be_t25, observed).
narrative_ontology:measurement(coer_be_t30, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 30, 0.72).
narrative_ontology:measurement_basis(coer_be_t30, observed).
narrative_ontology:measurement(coer_be_t40, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 40, 0.72).
narrative_ontology:measurement_basis(coer_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(coer_su_t0, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 0, 0.65).
narrative_ontology:measurement_basis(coer_su_t0, observed).
narrative_ontology:measurement(coer_su_t5, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 5, 0.7).
narrative_ontology:measurement_basis(coer_su_t5, observed).
narrative_ontology:measurement(coer_su_t10, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 10, 0.74).
narrative_ontology:measurement_basis(coer_su_t10, observed).
narrative_ontology:measurement(coer_su_t15, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 15, 0.77).
narrative_ontology:measurement_basis(coer_su_t15, observed).
narrative_ontology:measurement(coer_su_t20, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 20, 0.79).
narrative_ontology:measurement_basis(coer_su_t20, observed).
narrative_ontology:measurement(coer_su_t25, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 25, 0.8).
narrative_ontology:measurement_basis(coer_su_t25, observed).
narrative_ontology:measurement(coer_su_t30, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 30, 0.81).
narrative_ontology:measurement_basis(coer_su_t30, observed).
narrative_ontology:measurement(coer_su_t40, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 40, 0.81).
narrative_ontology:measurement_basis(coer_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__public_health_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(coercion_legitimacy_boundary__public_health_primary, 0.14).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__public_health_primary, coercion_legitimacy_boundary__bodily_autonomy_primary).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__public_health_primary, coercion_legitimacy_boundary__proportionality_reading).

% DUAL FORMULATION NOTE:
% The coercion_legitimacy_boundary kernel is formalized in constitutional law and instantiates three structurally distinct constraint readings. public_health_primary (this story) positions collective harm-prevention as the ground for legitimacy; bodily_autonomy_primary positions consent as categorically inviolable; proportionality_reading positions disease severity as the scaling parameter. The three readings have different beneficiary/victim structures, different ε values, and different authority-grounding commitments. They are not alternative measurements of one constraint — they are three different constraints that contest the same kernel. public_health_primary influences both siblings by establishing that coercion is legitimately grounded in public health (structural pressure on proportionality to defend its floor-setting logic) and forecloses bodily_autonomy_primary by treating coercion's legitimacy as an open question (directly contradicting the axiom of categorical inviolability).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(coercion_legitimacy_boundary__public_health_primary, moderate, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
