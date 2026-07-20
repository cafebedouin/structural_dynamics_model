% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__symbolic_transformation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__symbolic_transformation, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: temple_sacrifice_commitment__symbolic_transformation
 *   human_readable: Temple Sacrifice Commitment â Symbolic Transformation Reading
 *   domain: religious_law/halakhic_tradition/commitment_system_theory
 *
 * SUMMARY:
 *   This constraint instantiates the symbolic_transformation reading of the
 *   temple_sacrifice_commitment kernel. After the destruction of the Second
 *   Temple in 70 CE, the rabbinic authority structure redefined the biblical
 *   commandment to offer sacrifices, claiming that prayer and Torah study
 *   constitute the new, authorized instantiation of that commitment rather
 *   than temporary substitutes for a suspended practice. The constraint
 *   operates across two millennia of Jewish history, organizing synagogue
 *   liturgy, halakhic education, and communal identity around the proposition
 *   that verbal and intellectual worship fulfill the same covenantal function
 *   as the material altar service. The reading is contested by sibling
 *   readings that treat study as direct performance, as preparatory exercise,
 *   or that insist on material instantiation as non-negotiable.
 *
 * KEY AGENTS:
 *   - rabbinic_authority: Agenda-setter and beneficiary (institutional/constrained) â administers the transformed framework and collects interpretive legitimacy
 *   - diaspora_communities: Primary beneficiary (moderate/identity_locked) â receive continuous practice through the transformation
 *   - kohanim: Payer (moderate/identity_locked) â hereditary priesthood structurally displaced by interpretive shift
 *   - temple_movement_activists: Payer/excluded (powerless/trapped) â bear marginalization costs for holding literal performance view
 *   - critical_religious_historians: Analytical observer (analytical) â documents the power shift without stake in the constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__symbolic_transformation, 0.85).
domain_priors:suppression_score(temple_sacrifice_commitment__symbolic_transformation, 0.68).
domain_priors:theater_ratio(temple_sacrifice_commitment__symbolic_transformation, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, extractiveness, 0.85).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__symbolic_transformation, tangled_rope).
narrative_ontology:human_readable(temple_sacrifice_commitment__symbolic_transformation, "Temple Sacrifice Commitment â Symbolic Transformation Reading").
narrative_ontology:topic_domain(temple_sacrifice_commitment__symbolic_transformation, "religious_law/halakhic_tradition/commitment_system_theory").

domain_priors:requires_active_enforcement(temple_sacrifice_commitment__symbolic_transformation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__symbolic_transformation, 'a5fc8229-65bc-4452-8bdc-ab8613193446').
narrative_ontology:cs_kernel_codification('a5fc8229-65bc-4452-8bdc-ab8613193446', fixed_text).
narrative_ontology:cs_authority_grounding('a5fc8229-65bc-4452-8bdc-ab8613193446', lineage).
narrative_ontology:cs_interpretation_layer_present('a5fc8229-65bc-4452-8bdc-ab8613193446').
narrative_ontology:cs_reading_relation('a5fc8229-65bc-4452-8bdc-ab8613193446', temple_sacrifice_commitment__study_as_exercise, coexists_with).
narrative_ontology:cs_reading_relation('a5fc8229-65bc-4452-8bdc-ab8613193446', temple_sacrifice_commitment__performance_only, forecloses).
narrative_ontology:cs_reading_relation('a5fc8229-65bc-4452-8bdc-ab8613193446', temple_sacrifice_commitment__hybrid_preparatory, coexists_with).
narrative_ontology:cs_axiom('a5fc8229-65bc-4452-8bdc-ab8613193446', foundational, divinely_authorized_transformation).
narrative_ontology:cs_axiom_status(divinely_authorized_transformation, holdable).
narrative_ontology:cs_axiom_grounding('a5fc8229-65bc-4452-8bdc-ab8613193446', divinely_authorized_transformation, theological).
narrative_ontology:cs_axiom('a5fc8229-65bc-4452-8bdc-ab8613193446', secondary, prayer_as_cultic_equivalent).
narrative_ontology:cs_axiom_status(prayer_as_cultic_equivalent, holdable).
narrative_ontology:cs_axiom_grounding('a5fc8229-65bc-4452-8bdc-ab8613193446', prayer_as_cultic_equivalent, conventional).
narrative_ontology:cs_reference_frame('a5fc8229-65bc-4452-8bdc-ab8613193446', sacrifice_command_revealed).
narrative_ontology:cs_drift_state('a5fc8229-65bc-4452-8bdc-ab8613193446', rabbinic_ascendancy, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('a5fc8229-65bc-4452-8bdc-ab8613193446', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__symbolic_transformation, rabbinic_authority).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__symbolic_transformation, diaspora_communities).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__symbolic_transformation, kohanim).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__symbolic_transformation, temple_movement_activists).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__symbolic_transformation, rabbinic_oral_law_authority).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__symbolic_transformation, continuity_of_worship_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the halakhic framework that redefines sacrificial commandments into prayer and Torah study. Derives institutional legitimacy from the claim that this transformation is authorized by the original divine command. Cannot abandon the framework without dissolving rabbinic Judaism's continuity claim, yet retains interpretive flexibility within the tradition.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, rabbinic_authority, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_commitment__symbolic_transformation, rabbinic_authority, beneficiary).

% Receive a viable, continuous religious practice that maintains covenantal relationship without a functioning Temple. Jewish identity is fused with rabbinic normativity; exiting the framework means exiting recognized Jewish communal boundaries in most historical and contemporary contexts.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, diaspora_communities, beneficiary,
    moderate, biographical, identity_locked, global).

% Hereditary priesthood stripped of its central cultic function by the interpretive transformation. Retain ritual remnants such as the priestly blessing and Torah precedence, but the core sacrificial role is declared fulfilled by other agents. Cannot exit their kohanic identity; structurally displaced by the shift from material to verbal worship.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, kohanim, payer,
    moderate, generational, identity_locked, national).

% Advocate for literal sacrificial restoration on the Temple Mount. Regarded by the rabbinic mainstream as dangerous or heretical and structurally excluded from halakhic legitimacy. Bear costs of marginalization and legal or state suppression in Israel; trapped between messianic conviction and rabbinic hegemony.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, temple_movement_activists, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_commitment__symbolic_transformation, temple_movement_activists, excluded).

% Academic observers who analyze the rabbinic transformation as a historical and sociological process. Neither benefit from nor pay into the constraint's operation; document how interpretive authority and ritual power shifted after 70 CE.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, critical_religious_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_commitment__symbolic_transformation, rabbinic_authority).
narrative_ontology:fixing_cost_class(temple_sacrifice_commitment__symbolic_transformation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintaining covenantal continuity and collective Jewish worship after the destruction of the Temple by reconstituting sacrificial commandments into prayer, Torah study, and synagogue ritual.
% TRANSFER_FUNCTION: Transfers cultic authority and ritual legitimacy from the hereditary priesthood and altar-based service to the rabbinic interpretive class and communal verbal worship; transfers compliance obligation from the community to the transformed framework.
% ABSENT_VOICES: Temple movement activists and priestly literalists who regard the transformation as unauthorized usurpation rather than legitimate adaptation; they are excluded from halakhic discourse and, in the State of Israel, from legal access to the Temple Mount for sacrificial purposes.
% DISAPPEARANCE_RATIONALE: If the authority to symbolically transform sacrifice vanished, the rabbinic framework's continuity claim would fracture, synagogue liturgy would lose its claimed connection to Temple worship, the priesthood would reassert its structural role, and the boundary between legitimate Judaism and marginal sectarianism would reorganize.
% FOUNDING_PROBLEM: The destruction of the Second Temple in 70 CE, which eliminated the material conditions for fulfilling central Torah commandments concerning sacrifice and priestly service.
% FOUNDING_PROBLEM_CORROBORATION: Historical and archaeological sources outside the rabbinic tradition corroborate the Temple destruction as a factual crisis. Corroboration that symbolic transformation was the necessary or authorized solution is contested: historians note the rise of Rabbinic Judaism as one competing response among several, not the inevitable resolution. Corroboration from within the beneficiary structure alone would be self-serving.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__symbolic_transformation, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__symbolic_transformation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__symbolic_transformation, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(temple_sacrifice_commitment__symbolic_transformation, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__symbolic_transformation, 0.85, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__symbolic_transformation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(temple_sacrifice_commitment__symbolic_transformation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(temple_sacrifice_commitment__symbolic_transformation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the transformation concentrates interpretive authority in the rabbinic class while structurally displacing the priesthood and marginalizing literalists; the coordination function (communal survival) is real but asymmetrically distributed. Suppression (0.68) reflects active, ongoing enforcement against temple movements and alternative readings. Theater_ratio (0.5) captures the performative dimension of synagogue ritual that echoes the Temple without material sacrifice. Accessibility_collapse (0.72) is high because alternatives such as literal sacrifice or Karaite-style rejection are nearly inaccessible within normative Rabbinic Judaism. Resistance (0.45) is moderate, coming from messianic and temple activist movements. The measurement series run on a single shared time grid.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter/beneficiary seat (rabbinic authority) experiences the constraint as legitimate continuity and necessary adaptation; from this seat the coordination function dominates and the transformation appears authorized. The payer seats (kohanim, temple activists) experience the same structure as usurpation of their role and suppression of their reading. The engine computes this divergence from the structural asymmetry in power, exit options, and beneficiary/victim declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic_authority sits near the beneficiary end: it collects interpretive legitimacy, institutional continuity, and communal obedience. Diaspora_communities sit low-to-moderate: they benefit from identity continuity but pay through identity-locked compliance. Kohanim sit near the target end: they bear the cost of displaced hereditary function with identity-locked exit. Temple movement activists sit at the high-target extreme: they are explicitly excluded, trapped by messianic conviction and political barriers, and bear the full extraction of marginalization.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as pure coordination (Rope) by insisting on declared victims and active enforcement, which exposes the asymmetric extraction beneath the continuity narrative. Conversely, it prevents mislabeling as pure extraction (Snare) by acknowledging the genuine coordination function: Jewish communal survival and covenantal practice were materially preserved by the transformation. The Tangled Rope classification captures that both are structurally true simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transformation_legitimacy,
    'Is the symbolic transformation of sacrifice into prayer genuinely authorized by the original kernel, or is it an institutional innovation retrospectively legitimized as divine continuity?',
    'Text-critical and historical analysis of Second Temple and tannaitic literature to determine whether the transformation doctrine emerges from the kernel''s internal logic or from post-crisis rabbinic power consolidation.',
    'If unauthorized innovation, the constraint''s extractiveness is higher than its coordination function suggests, shifting classification toward snare; if genuinely authorized, the coordination function is primary and extraction is the cost of legitimate adaptation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformation_legitimacy, conceptual, 'Whether transformation is kernel-authorized or institutional cover.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of performance-only readings structural (enforced by rabbinic courts, state power, communal excommunication) or internalized (theological conviction that rabbinic authority is binding)?',
    'Comparative analysis of communities with weak rabbinic coercion but strong adherence versus communities with strong coercion but contested adherence.',
    'If primarily internalized, effective suppression exceeds structural measures and the constraint functions more as identity_coordination; if primarily structural, the constraint relies on enforcement mechanisms typical of tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism.').

omega_variable(
    rabbinic_power_concentration,
    'Does the benefit of interpretive authority accrue primarily to the rabbinic class as a power concentration, or diffusely to Jewish communal survival?',
    'Sociological analysis of resource and status flows within Jewish communities across different historical periods and geographies.',
    'If concentrated, gain_flow should name rabbinic_authority; if diffuse, gain_flow should be diffuse, altering the receipt-surface classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rabbinic_power_concentration, empirical, 'Whether extraction concentrates in rabbinic authority or diffuses to communal survival.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__symbolic_transformation, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temple_sac_sym_tr_t0, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 0, 0.1).
narrative_ontology:measurement(temple_sac_sym_tr_t400, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 400, 0.3).
narrative_ontology:measurement(temple_sac_sym_tr_t800, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 800, 0.42).
narrative_ontology:measurement(temple_sac_sym_tr_t1200, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 1200, 0.46).
narrative_ontology:measurement(temple_sac_sym_tr_t1600, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 1600, 0.48).
narrative_ontology:measurement(temple_sac_sym_tr_t2000, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 2000, 0.5).

% Extraction over time
narrative_ontology:measurement(temple_sac_sym_be_t0, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(temple_sac_sym_be_t400, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 400, 0.55).
narrative_ontology:measurement(temple_sac_sym_be_t800, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 800, 0.72).
narrative_ontology:measurement(temple_sac_sym_be_t1200, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 1200, 0.78).
narrative_ontology:measurement(temple_sac_sym_be_t1600, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 1600, 0.82).
narrative_ontology:measurement(temple_sac_sym_be_t2000, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 2000, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(temple_sac_sym_su_t0, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(temple_sac_sym_su_t400, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 400, 0.7).
narrative_ontology:measurement(temple_sac_sym_su_t800, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 800, 0.65).
narrative_ontology:measurement(temple_sac_sym_su_t1200, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 1200, 0.6).
narrative_ontology:measurement(temple_sac_sym_su_t1600, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 1600, 0.55).
narrative_ontology:measurement(temple_sac_sym_su_t2000, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 2000, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__symbolic_transformation, identity_coordination).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment__study_as_exercise).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment__hybrid_preparatory).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment__performance_only).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the temple_sacrifice_commitment kernel. The kernel decomposes into four structurally distinct constraints because the label temple_sacrifice_commitment conflates claims with different epsilon values, beneficiary structures, and failure modes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
