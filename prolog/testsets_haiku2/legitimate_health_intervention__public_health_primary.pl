% ============================================================================
% CONSTRAINT STORY: legitimate_health_intervention__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_health_intervention__public_health_primary, []).

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
 *   constraint_id: legitimate_health_intervention__public_health_primary
 *   human_readable: Public Health Intervention Legitimacy (Population-Primary Reading)
 *   domain: public_health_policy / medical_ethics / constitutional_law
 *
 * SUMMARY:
 *   This constraint instantiates the PUBLIC-HEALTH-PRIMARY reading of the
 *   contested kernel 'legitimate_health_intervention.' Under this reading,
 *   state authority to mandate medical interventions derives from
 *   demonstrable population-level harm reduction (morbidity/mortality
 *   decrease). Individual refusal is reframed as externality imposition —
 *   unvaccinated persons are treated as disease vectors whose bodily choices
 *   create harm spillovers onto vulnerable populations. Enforcement follows:
 *   employment termination, licensing restrictions, school exclusion, medical
 *   access denial. The constraint is CLAIMED as tangled_rope (genuine
 *   coordination function + asymmetric extraction + active enforcement) while
 *   the metrics describe substantial extractiveness (0.68 at interval end)
 *   driven by enforcement intensity and the dual classification of refusers
 *   as both beneficiaries (in the coordinated outcome) and payers (bearing
 *   enforcement costs). This reading contests two sibling readings:
 *   bodily_autonomy_primary (which forecloses this reading's externality
 *   logic) and proportionality_reading (which modulates both population
 *   benefit and individual burden). The kernel is formalized (fixed texts:
 *   public health statutes, epidemiological guidelines), authority grounds
 *   itself in extraction (public health agencies extract institutional
 *   authority and mandate legitimacy from epidemiological consensus), and an
 *   interpretation layer is present (epidemiologists translate raw population
 *   data into policy guidance).
 *
 * KEY AGENTS:
 *   - public_health_authority: agenda-setter (sets mandates, enforces via employment/licensing/access)
 *   - immunocompromised_populations: trapped beneficiaries (depend on mandate compliance for survival)
 *   - vaccine_mandate_refusers: constrained payers (face employment loss, license revocation, exclusion)
 *   - employment_displaced_by_mandate: powerless payers (economic devastation from job termination)
 *   - unvaccinated_disease_vectors: dual-role payers/beneficiaries (classified as externality-creators but benefit from herd immunity once compliance threshold reached)
 *   - epidemiological_consensus_body: non-agent beneficiary (vindicated proposition whose authority is amplified by mandate legitimacy)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__public_health_primary, 0.68).
domain_priors:suppression_score(legitimate_health_intervention__public_health_primary, 0.71).
domain_priors:theater_ratio(legitimate_health_intervention__public_health_primary, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__public_health_primary, tangled_rope).
narrative_ontology:human_readable(legitimate_health_intervention__public_health_primary, "Public Health Intervention Legitimacy (Population-Primary Reading)").
narrative_ontology:topic_domain(legitimate_health_intervention__public_health_primary, "public_health_policy / medical_ethics / constitutional_law").

domain_priors:requires_active_enforcement(legitimate_health_intervention__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__public_health_primary, '4b01c3a5-ffce-4c1d-87bb-10cb1fb31324').
narrative_ontology:cs_kernel_codification('4b01c3a5-ffce-4c1d-87bb-10cb1fb31324', formalized).
narrative_ontology:cs_authority_grounding('4b01c3a5-ffce-4c1d-87bb-10cb1fb31324', extraction).
narrative_ontology:cs_interpretation_layer_present('4b01c3a5-ffce-4c1d-87bb-10cb1fb31324').
narrative_ontology:cs_reading_relation('4b01c3a5-ffce-4c1d-87bb-10cb1fb31324', legitimate_health_intervention__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('4b01c3a5-ffce-4c1d-87bb-10cb1fb31324', legitimate_health_intervention__proportionality_reading, influences).
narrative_ontology:cs_axiom('4b01c3a5-ffce-4c1d-87bb-10cb1fb31324', foundational, externality_imposition_by_refusal).
narrative_ontology:cs_axiom_status(externality_imposition_by_refusal, holdable).
narrative_ontology:cs_axiom_grounding('4b01c3a5-ffce-4c1d-87bb-10cb1fb31324', externality_imposition_by_refusal, deontological).
narrative_ontology:cs_axiom('4b01c3a5-ffce-4c1d-87bb-10cb1fb31324', foundational, population_benefit_overrides_individual_autonomy).
narrative_ontology:cs_axiom_status(population_benefit_overrides_individual_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('4b01c3a5-ffce-4c1d-87bb-10cb1fb31324', population_benefit_overrides_individual_autonomy, deontological).
narrative_ontology:cs_reference_frame('4b01c3a5-ffce-4c1d-87bb-10cb1fb31324', epidemiological_necessity_maximizes_public_benefit).
narrative_ontology:cs_drift_state('4b01c3a5-ffce-4c1d-87bb-10cb1fb31324', contemporary_mandate_contestation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4b01c3a5-ffce-4c1d-87bb-10cb1fb31324', '').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__public_health_primary, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, immunocompromised_populations).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, unvaccinated_disease_vectors).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, vaccine_mandate_refusers).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, employment_displaced_by_mandate).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, immunocompetent_vaccinated).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, unvaccinated_disease_vectors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets vaccine or treatment mandates justified by population-level epidemiological modeling; implements enforcement through employment conditions, licensing restrictions, school attendance rules. Measures legitimacy by morbidity/mortality reduction in the target population. Bears responsibility for communicable disease control but experiences political pressure from multiple sides.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, public_health_authority, agenda_setter,
    institutional, generational, analytical, national).

% Depend directly on high community vaccination/treatment rates for protection, since their immune systems cannot mount effective responses to disease or vaccines. High transmission in the unvaccinated community creates direct mortality risk. They cannot exit the constraint — they are medically dependent on others' compliance.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, immunocompromised_populations, beneficiary,
    powerless, biographical, trapped, national).

% Face employment termination, loss of professional licenses, exclusion from public gatherings, school attendance denial, or medical treatment denial as enforcement for refusing vaccination or treatment. They assert bodily autonomy and medical freedom; the reading asserts they impose externality (disease transmission risk) on others. Exit options: comply, migrate to jurisdictions without mandates, or organize political resistance.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, vaccine_mandate_refusers, payer,
    moderate, biographical, constrained, national).

% Healthcare workers, public employees, military personnel whose livelihoods were terminated for vaccine refusal. Economic devastation compounds the autonomy violation. Their objection — that mandates destroy careers and economic security without consent — is framed as externality imposition in this reading but treated as individual cost, not collective concern.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, employment_displaced_by_mandate, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(legitimate_health_intervention__public_health_primary, employment_displaced_by_mandate, excluded).

% In the public-health-primary reading, the unvaccinated are simultaneously classified as beneficiaries (they benefit from the constraint through reduced disease spread in the overall population once compliance reaches threshold) AND payers (they bear enforcement costs for non-compliance). This dual role reflects the reading's logic: external enforcement is justified because individuals' choices have spillover effects on collective outcomes.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, unvaccinated_disease_vectors, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimate_health_intervention__public_health_primary, unvaccinated_disease_vectors, payer).

% Receive benefit from reduced disease circulation and herd immunity effects. Compliance is rewarded with access and freedom; they are not characterized as victims in this reading because they align with the mandate. Their exit options are effectively closed — not because they are trapped, but because the constraint defines aligned compliance as normal and costless.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, immunocompetent_vaccinated, beneficiary,
    organized, biographical, constrained, national).

% Legal scholars, bioethicists, and disability advocates who argue bodily autonomy is foundational and cannot be overridden by aggregate population benefit. They are structurally excluded from the decision-making apparatus that implements this reading; their presence would argue for alternative framings or proportionality thresholds the reading does not recognize.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, competing_autonomy_advocates, excluded,
    powerful, generational, analytical, national).

% Not a human actor but a vindicated proposition: the epidemiological establishment benefits institutionally and reputationally from public mandates justified by their models. Their epistemic authority is amplified when their risk estimates drive policy; their professional standing depends on those estimates being endorsed as legitimate.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, epidemiological_consensus_body, beneficiary,
    powerful, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(legitimate_health_intervention__public_health_primary, epidemiological_consensus_body).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_health_intervention__public_health_primary, public_health_authority).
narrative_ontology:fixing_cost_class(legitimate_health_intervention__public_health_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Achieves high compliance with disease-prevention interventions (vaccination, testing, treatment) by making non-compliance costly through employment, licensing, or access restrictions. Solves the collective-action problem: without enforcement, individuals face private incentives to free-ride on herd immunity while avoiding intervention risks.
% TRANSFER_FUNCTION: Moves bodily autonomy and economic security from mandate refusers to a diffuse population benefit (disease reduction). Also transfers authority to define medical necessity from individuals to epidemiological experts and state power. The transfer is from identifiable payers (those terminated or restricted) to a diffuse beneficiary set (everyone protected by reduced transmission).
% ABSENT_VOICES: Bodily-autonomy advocates, medical freedom proponents, and disability scholars who argue that state-coerced medical intervention is a fundamental rights violation. They would argue for informed-consent primacy and proportionality thresholds this reading does not recognize. They are structurally excluded from the epidemiological consensus that grounds this reading's legitimacy.
% DISAPPEARANCE_RATIONALE: If mandates and their enforcement mechanisms disappeared, vaccination rates would fall (evidence from mandate-drop periods shows 5-15% compliance declines), disease circulation would increase, and immunocompromised populations would face elevated mortality. The reading treats this rearrangement as proof that the constraint solves a real coordination problem and is therefore not arbitrary extraction — but the rearrangement also reveals that the constraint's persistence depends on active enforcement, not on voluntary recognition of the externality logic.
% FOUNDING_PROBLEM: Insufficient vaccination/treatment compliance to achieve herd immunity thresholds required to protect immunocompromised and medically fragile populations. Without enforcement, voluntary uptake leaves gaps that allow preventable disease circulation.
% FOUNDING_PROBLEM_CORROBORATION: Public health authorities and epidemiological bodies attest the problem is live and that mandates are necessary. However, independent studies from autonomy-focused bioethicists and disability-rights organizations attest that (1) voluntary uptake often reaches adequate thresholds without mandates in many jurisdictions, (2) mandates create health harms for those who lose employment and economic security, and (3) the founding problem may be overstated to justify enforcement that exceeds epidemiological necessity. Legislative testimony from medical freedom advocates disputes both the problem's severity and the framing of individual refusal as pure externality.
narrative_ontology:disappearance_verdict(legitimate_health_intervention__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_health_intervention__public_health_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__public_health_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimate_health_intervention__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_health_intervention__public_health_primary, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_health_intervention__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_health_intervention__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_health_intervention__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and rising (0.45 → 0.68) because enforcement mechanisms (job loss, license revocation) impose severe costs on mandate refusers that are not reciprocated; the constraint is not voluntary coordination but coerced compliance. The rise models enforcement intensification and policy hardening over the interval as resistance to mandates met with more aggressive enforcement (workplace requirements expanded, state sector mandates tightened). Suppression is also high (0.71 final) and rising because the constraint persists by making non-compliance economically and professionally impossible, not by convincing refusers of the externality logic. Theater is moderate-low (0.28) because a genuine coordination function exists (vulnerable populations do benefit from high vaccination rates) but enforcement activity increasingly defends mandate persistence against growing political and legal challenge, not primarily against disease spread. The dual classification of refusers as beneficiaries reveals the reading's internal logic: in the aggregate outcome (high compliance), refusers benefit from herd immunity, but they bear the cost of enforced compliance. This is structurally different from rope (where beneficiaries welcome the coordination) or snare (where there is no coordination function). The tangled-rope claim reflects the reading's commitment to genuine coordination grounded in collective benefit, coupled with asymmetric enforcement to achieve it.
 *
 * PERSPECTIVAL GAP:
 *   From the public_health_authority seat, the constraint appears as necessary coordination: without mandates, vaccination rates fall, and vulnerable populations face preventable mortality — a genuine externality. From the vaccine_mandate_refusers seat, the constraint appears as state coercion justified by epidemiological claims but operating to eliminate bodily autonomy and economic security. From the immunocompromised seat, mandates are protective necessity, not extraction. From the bodily_autonomy_primary reading's seat, this entire framing is illegitimate — individual refusal is not externality imposition but the exercise of rights that public health cannot override. The engine computes these divergent directionalities from the structural data: public health authority gets low d (agenda-setter, benefits from enforcement legitimacy); mandate refusers get high d (payers bearing enforcement costs); immunocompromised get low d (beneficiaries depending on compliance). The perspectival gap emerges from the underlying kernel contest, not from measurement error.
 *
 * DIRECTIONALITY LOGIC:
 *   Public_health_authority: d ≈ 0.2 (agenda-setter, powerful, exits into analytical observation; sets the rules and benefits institutionally from mandate legitimacy; no external pressure forces compliance). Vaccine_mandate_refusers: d ≈ 0.85 (payers bearing enforcement costs, moderate power but facing employment loss and professional exclusion, identity-locked in many cases because refusal is tied to medical philosophy, religious conviction, or political identity; exit would require capitulation to the constraint itself). Employment_displaced_by_mandate: d ≈ 0.9 (powerless, trapped by economic necessity, suffer career termination and cannot re-enter the sector; highest extraction of any seat). Immunocompromised_populations: d ≈ 0.15 (trapped beneficiaries; they benefit from mandate compliance but have no power to enforce it — their survival depends on others' compliance, a dependent benefit, not active benefit-capture). Unvaccinated_disease_vectors: d ≈ 0.7 (moderate power, constrained exit, classified as externality-creators and therefore legitimate targets of enforcement). The directionality override for this last group is necessary: they are mathematically beneficiaries (herd immunity benefits them once compliance threshold is reached) but structurally payers (they bear the enforcement cost of achieving that threshold). The override reflects the reading's reframing of individual bodily choice as externality imposition, which shifts their directionality from low (beneficiary of herd immunity) to high (target of enforcement to produce that immunity).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is 'contested' — public health authorities attest the problem (insufficient voluntary compliance) remains live, while epidemiological critics attest that voluntary uptake often reaches adequate thresholds and that mandates overstay their necessity. The disappearance_verdict is 'world_rearranges' because mandate removal would demonstrably affect vaccination rates and disease circulation. These are not contradictory: a constraint can rearrange the world when removed (satisfy the disappearance verdict) while its founding problem is contested (parties disagree whether the problem still justifies the constraint). The mandatrophy signal emerges from theater_ratio (0.28, moderate-low) and the rising extraction over time. If theater were rising toward 0.5+, the signal would be stronger (theatrical maintenance of a solved problem). Instead, theater rises modestly while extraction plateaus, suggesting the constraint has moved from active response to endemic policy — the founding problem may be contested, but the constraint is not yet purely theatrical. Mandatrophy is NOT resolved; the constraint occupies the tangled-rope zone where both coordination and extraction coexist. Resolution would require either (1) the founding problem to become unambiguously dead (disappearance verdict = world_unchanged), and theater to rise sharply (purely performative maintenance), or (2) the constraint to shift to snare (coordination function eliminated, pure extraction remains), or (3) political resolution of the kernel contest via the bodily_autonomy_primary or proportionality_reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    externality_classification_ambiguity,
    'Is unvaccinated status legitimately classified as ''externality imposition'' (a spillover harm that justifies state intervention), or is it a private medical choice that imposes risk but not a category of harm the state can mandate away?',
    'This is the core kernel contest: bodily-autonomy advocates argue that disease transmission risk is a natural consequence of living in a population, not an externality the individual causes (just as car accidents from driving are internalized by drivers, not imposed on all pedestrians collectively). Public-health advocates argue that communicable disease transmission is structurally different — an individual''s choice directly causes harm to others. Resolution would require foundational normative agreement on what counts as ''externality imposition,'' not empirical measurement.',
    'If externality classification is rejected, the entire legitimacy scaffold of the mandate collapses, and the constraint reclassifies to snare (coerced compliance justified by disputed claims about individual harm-creation). If externality classification is accepted, the tangled-rope classification holds and the constraint''s enforcement is legitimate coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(externality_classification_ambiguity, conceptual, 'Whether individual refusal to vaccinate is validly classified as externality imposition.').

omega_variable(
    epidemiological_necessity_threshold,
    'At what vaccination rate does herd immunity adequately protect vulnerable populations? Below what rate does mandate enforcement become epidemiologically justified versus politically convenient?',
    'Comparative analysis across jurisdictions with different mandate intensities, controlling for disease characteristics, population density, and healthcare capacity. Time-series of vaccination rates and disease outcomes in jurisdictions before, during, and after mandate removal.',
    'High threshold (e.g., 95%+) justifies aggressive enforcement and supports the public-health-primary reading. Low threshold (e.g., 60-70%) suggests mandates exceed epidemiological necessity and are operating as political extraction. At very low thresholds, the constraint shifts from tangled-rope to snare (protection function achieved with low coercion; remaining enforcement is pure extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epidemiological_necessity_threshold, empirical, 'Whether mandate intensity is epidemiologically proportionate.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the high suppression (0.71) primarily structural (employment law, licensing rules, access denial make non-compliance impossible) or internalized (individuals have internalized the public-health framing and accept mandates as legitimate)?',
    'Post-mandate surveys measuring (1) willingness to comply if mandates were removed, (2) acceptance of the externality-imposition framing, (3) belief that vaccination protects others. Rising acceptance would signal internalization; stable or declining acceptance would signal suppression persists structurally (the constraint carries itself only through enforcement, not through belief).',
    'If internalized, the constraint has achieved consensus and the suppression measure understates actual legitimacy. If structural, the constraint is maintained by coercion and the externality framing has not convinced its targets. High structural suppression with low internalization suggests the constraint is a tangled-rope boundary case between coordinate extraction and pure snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether suppression is structural coercion or internalized belief.').

omega_variable(
    kernel_reading_under_determination,
    'Is the public-health-primary reading genuinely the alternative to bodily-autonomy-primary, or do the two readings operate at different levels (one making an institutional/legal claim about legitimate state power, the other making a philosophical claim about individual rights) such that they could coexist in principle?',
    'Analytical unpacking of what each reading claims: does public-health-primary assert that bodily autonomy has NO moral weight, or that it can be overridden by sufficiently large population benefits? Does bodily-autonomy-primary assert that NO state power can override consent, or that state power requires proportionality? If both readings admit that the OTHER consideration has weight but they disagree on weighting, they coexist (proportionality_reading). If one denies the other consideration has weight, they foreclose.',
    'Foreclosure implies that adoption of one reading requires rejecting the core premise of the other — this reading commits us to saying individual consent is not foundational. Coexistence implies that this reading weighs population benefit heavily but does not deny that bodily autonomy matters; the disagreement is over tradeoffs, not premises. This is the underlying kernel contest and determines which sibling reading relation to use.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_under_determination, conceptual, 'Whether public-health-primary forecloses bodily-autonomy-primary or coexists with it.').

omega_variable(
    dual_role_classification_coherence,
    'Is the dual classification of unvaccinated_disease_vectors as both beneficiaries (they benefit from herd immunity outcomes) and payers (they bear enforcement costs) coherent, or does it conflate two different time scales or counterfactuals?',
    'Clarify the counterfactual: in the outcome where compliance reaches herd-immunity threshold, refusers benefit from reduced disease circulation. But they only reach that outcome BECAUSE they are forced to comply. If they could exit while others maintained compliance, they would receive the benefit for free. The dual classification captures that they are forced-beneficiaries (benefit extracted from them via coercion), but the classification risks obscuring the extraction by labeling them beneficiaries at all.',
    'If the dual classification is coherent, it supports the tangled-rope claim (coordination + enforcement for asymmetric reasons). If it is incoherent, then unvaccinated_disease_vectors should be classified as pure payers (not beneficiaries), and the constraint is closer to snare (extraction with enforced participation in a coordinated outcome that benefits others, not them). Resolution clarifies whether forced-beneficiary is a meaningful category or a rhetorical cover for extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_role_classification_coherence, conceptual, 'Whether forced beneficiaries count as true beneficiaries for constraint classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__public_health_primary, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(health_pub_primary_tr_t0, legitimate_health_intervention__public_health_primary, theater_ratio, 0, 0.15).
narrative_ontology:measurement(health_pub_primary_tr_t6, legitimate_health_intervention__public_health_primary, theater_ratio, 6, 0.18).
narrative_ontology:measurement(health_pub_primary_tr_t12, legitimate_health_intervention__public_health_primary, theater_ratio, 12, 0.22).
narrative_ontology:measurement(health_pub_primary_tr_t18, legitimate_health_intervention__public_health_primary, theater_ratio, 18, 0.25).
narrative_ontology:measurement(health_pub_primary_tr_t24, legitimate_health_intervention__public_health_primary, theater_ratio, 24, 0.27).
narrative_ontology:measurement(health_pub_primary_tr_t30, legitimate_health_intervention__public_health_primary, theater_ratio, 30, 0.28).
narrative_ontology:measurement(health_pub_primary_tr_t36, legitimate_health_intervention__public_health_primary, theater_ratio, 36, 0.28).

% Extraction over time
narrative_ontology:measurement(health_pub_primary_be_t0, legitimate_health_intervention__public_health_primary, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(health_pub_primary_be_t6, legitimate_health_intervention__public_health_primary, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(health_pub_primary_be_t12, legitimate_health_intervention__public_health_primary, base_extractiveness, 12, 0.59).
narrative_ontology:measurement(health_pub_primary_be_t18, legitimate_health_intervention__public_health_primary, base_extractiveness, 18, 0.64).
narrative_ontology:measurement(health_pub_primary_be_t24, legitimate_health_intervention__public_health_primary, base_extractiveness, 24, 0.67).
narrative_ontology:measurement(health_pub_primary_be_t30, legitimate_health_intervention__public_health_primary, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(health_pub_primary_be_t36, legitimate_health_intervention__public_health_primary, base_extractiveness, 36, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(health_pub_primary_su_t0, legitimate_health_intervention__public_health_primary, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(health_pub_primary_su_t6, legitimate_health_intervention__public_health_primary, suppression_requirement, 6, 0.56).
narrative_ontology:measurement(health_pub_primary_su_t12, legitimate_health_intervention__public_health_primary, suppression_requirement, 12, 0.64).
narrative_ontology:measurement(health_pub_primary_su_t18, legitimate_health_intervention__public_health_primary, suppression_requirement, 18, 0.68).
narrative_ontology:measurement(health_pub_primary_su_t24, legitimate_health_intervention__public_health_primary, suppression_requirement, 24, 0.71).
narrative_ontology:measurement(health_pub_primary_su_t30, legitimate_health_intervention__public_health_primary, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(health_pub_primary_su_t36, legitimate_health_intervention__public_health_primary, suppression_requirement, 36, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_health_intervention__public_health_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(legitimate_health_intervention__public_health_primary, 0.18).
narrative_ontology:affects_constraint(legitimate_health_intervention__public_health_primary, legitimate_health_intervention__bodily_autonomy_primary).
narrative_ontology:affects_constraint(legitimate_health_intervention__public_health_primary, legitimate_health_intervention__proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'legitimate_health_intervention.' The constraint family comprises three structurally distinct claims about what makes health-intervention mandates legitimate: public_health_primary (population morbidity/mortality reduction justifies enforcement); bodily_autonomy_primary (informed consent cannot be overridden, regardless of public benefit); proportionality_reading (both population benefit and individual autonomy matter, weighted by disease severity). Each reading instantiates a different constraint with different ε values, beneficiary/victim sets, and classification. The three are linked by network.affects_constraints edges reflecting the kernel contest: this reading (public-health-primary) FORECLOSES bodily-autonomy-primary's core premise and INFLUENCES proportionality-reading by establishing that population benefit can override consent (proportionality then asks: at what threshold?). See commentary.kernel_context for full reading analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimate_health_intervention__public_health_primary, moderate, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
