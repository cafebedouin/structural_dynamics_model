% ============================================================================
% CONSTRAINT STORY: end_of_life_decision_authority__autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_decision_authority__autonomy_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: end_of_life_decision_authority__autonomy_reading
 *   human_readable: Autonomy-Based End-of-Life Decision Authority
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   This constraint story represents the autonomy reading of the end-of-life
 *   decision authority kernel: the claim that competent individuals possess
 *   sovereign authority over their own death. Under this reading, the
 *   constraint is the legal/ethical arrangement that recognizes and protects
 *   this authority — permitting assisted death for competent adults who
 *   request it, with clinical safeguards but without requiring external
 *   approval of the person's reasons. The constraint has evolved from
 *   near-total prohibition (high extraction from the suffering-prolonged,
 *   high suppression of the option) toward increasing legal recognition in
 *   multiple jurisdictions (Oregon 1997, Netherlands 2001, Canada 2016,
 *   etc.), reducing both extractiveness and suppression over the interval.
 *   The autonomy reading frames this as a coordination solution: it solves
 *   the problem of conflicting authorities by assigning final say to the
 *   person whose life it is.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_decision_authority__autonomy_reading, 0.15).
domain_priors:suppression_score(end_of_life_decision_authority__autonomy_reading, 0.25).
domain_priors:theater_ratio(end_of_life_decision_authority__autonomy_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_decision_authority__autonomy_reading, rope).
narrative_ontology:human_readable(end_of_life_decision_authority__autonomy_reading, "Autonomy-Based End-of-Life Decision Authority").
narrative_ontology:topic_domain(end_of_life_decision_authority__autonomy_reading, "medical_ethics/bioethics/end_of_life_policy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_decision_authority__autonomy_reading, '8964121f-0469-48e4-a98a-8d6dd2c55768').
narrative_ontology:cs_kernel_codification('8964121f-0469-48e4-a98a-8d6dd2c55768', formalized).
narrative_ontology:cs_authority_grounding('8964121f-0469-48e4-a98a-8d6dd2c55768', lineage).
narrative_ontology:cs_interpretation_layer_present('8964121f-0469-48e4-a98a-8d6dd2c55768').
narrative_ontology:cs_reading_relation('8964121f-0469-48e4-a98a-8d6dd2c55768', end_of_life_decision_authority__sanctity_reading, forecloses).
narrative_ontology:cs_reading_relation('8964121f-0469-48e4-a98a-8d6dd2c55768', end_of_life_decision_authority__vulnerability_protection_reading, coexists_with).
narrative_ontology:cs_axiom('8964121f-0469-48e4-a98a-8d6dd2c55768', foundational, competent_individual_sovereign_authority_over_death).
narrative_ontology:cs_axiom_status(competent_individual_sovereign_authority_over_death, holdable).
narrative_ontology:cs_axiom_grounding('8964121f-0469-48e4-a98a-8d6dd2c55768', competent_individual_sovereign_authority_over_death, deontological).
narrative_ontology:cs_axiom('8964121f-0469-48e4-a98a-8d6dd2c55768', secondary, bodily_self_ownership_includes_death_timing).
narrative_ontology:cs_axiom_status(bodily_self_ownership_includes_death_timing, holdable).
narrative_ontology:cs_axiom_grounding('8964121f-0469-48e4-a98a-8d6dd2c55768', bodily_self_ownership_includes_death_timing, deontological).
narrative_ontology:cs_reference_frame('8964121f-0469-48e4-a98a-8d6dd2c55768', classical_medical_paternalism).
narrative_ontology:cs_drift_state('8964121f-0469-48e4-a98a-8d6dd2c55768', contemporary_permissive_jurisdictions, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('8964121f-0469-48e4-a98a-8d6dd2c55768', '').
narrative_ontology:cs_kernel_id(end_of_life_decision_authority__autonomy_reading, end_of_life_decision_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__autonomy_reading, competent_adults_seeking_death).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__autonomy_reading, healthcare_professionals_facilitating).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, suffering_prolonged_denied_access).
narrative_ontology:constraint_vindicates(end_of_life_decision_authority__autonomy_reading, bodily_autonomy_principle).
narrative_ontology:constraint_vindicates(end_of_life_decision_authority__autonomy_reading, informed_consent_doctrine).
narrative_ontology:constraint_vindicates(end_of_life_decision_authority__autonomy_reading, self_ownership_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Competent adults who seek to end their suffering through assisted death. They benefit from sovereign authority by avoiding unwanted prolongation of suffering, maintaining dignity, and exercising control over their final days. Their exit options are constrained by legal prohibition in most jurisdictions — they cannot legally access the option without traveling to permissive jurisdictions (expensive, logistically difficult) or resorting to unassisted methods (riskier, more painful).
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, competent_adults_seeking_death, beneficiary,
    moderate, biographical, constrained, national).

% Competent adults who would choose assisted death if legally available but are denied access by current law. They bear the full cost of the constraint: prolonged suffering, loss of dignity, forced dependence, and the psychological burden of knowing a peaceful alternative exists but is legally blocked. Their exit options are effectively trapped — they cannot end their suffering on their terms, cannot legally travel for access (too ill, no resources), and unassisted methods may be beyond their physical capacity.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, suffering_prolonged_denied_access, payer,
    powerless, immediate, trapped, national).

% Physicians and other clinicians who would provide assisted death under an autonomy framework. They benefit professionally by being able to honor patient requests without legal jeopardy, aligning practice with their understanding of beneficence and autonomy. They also function as agenda-setters by defining clinical criteria, establishing safeguards, and shaping implementation. Their exit options are mobile — they can refuse participation (conscientious objection) or relocate to permissive jurisdictions without losing professional standing.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, healthcare_professionals_facilitating, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_decision_authority__autonomy_reading, healthcare_professionals_facilitating, agenda_setter).

% Hospitals, hospices, and palliative care organizations that would implement assisted death protocols. They set institutional policy, establish oversight committees, and bear implementation costs. They are constrained by legal environment, professional guidelines, and community values — cannot easily opt out of legal frameworks but can shape how they operationalize them.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, healthcare_institutions_hospice_palliative, agenda_setter,
    institutional, generational, constrained, national).

% Religious organizations and ethical traditions that oppose assisted death on sanctity-of-life grounds. They are structurally excluded from the autonomy reading's framework — their objection is not a parameter within the model but a competing framework. They can mobilize politically, influence legislation, and provide alternative care, but cannot veto individual decisions under an autonomy regime.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, religious_ethical_opponents, excluded,
    organized, generational, mobile, national).

% Disability rights organizations, elder advocacy groups, and bioethicists who warn that autonomy-based frameworks create subtle coercion for vulnerable populations (pressure to choose death to avoid burdening families, inadequate palliative alternatives). They are excluded from the autonomy reading's beneficiary/payer calculus — their concern is systemic risk, not individual denial. Their exit is constrained: they must engage the political/legal process to institutionalize safeguards.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, vulnerable_populations_advocates, excluded,
    organized, generational, constrained, national).

% Scholars in the autonomy-based bioethics tradition (Beauchamp & Childress, Dworkin, Brock) who provide the theoretical architecture for this reading. They analyze conceptual coherence, track empirical outcomes in permissive jurisdictions, and refine the normative framework. Their seat is analytical — they neither collect rents nor bear costs from the constraint's operation.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, bioethics_scholars_autonomy_tradition, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the coordination problem of who has final authority over the timing and manner of death when a competent person's values conflict with medical technology's capacity to prolong biological life. Without a clear authority rule, decisions default to clinicians, families, or institutional protocols — none of which reliably track the person's own values.
% TRANSFER_FUNCTION: Transfers decision-authority over life-ending interventions from medical gatekeepers (physicians, institutions, state) to the competent individual. The transfer moves: (1) the legal right to request and receive assisted death, (2) the professional obligation to assess eligibility and provide the intervention, (3) the risk of error from institutional denial to individual choice. The flow is from collective/medical control to individual sovereignty.
% ABSENT_VOICES: Future competent adults who will face end-of-life decisions under whatever regime is established — they cannot object to the framework they will inherit. Also absent: those who died under prohibition without ever knowing assisted death was a conceptual possibility (historical silence). Vulnerable populations advocates are partially present as excluded stakeholders but their systemic risk argument is not a veto within the autonomy framework.
% DISAPPEARANCE_RATIONALE: If the autonomy-based authority constraint vanished overnight, decision-making would revert to medical paternalism, family consensus, or state prohibition in most jurisdictions. Competent adults would lose legal access to assisted death; healthcare professionals would lose legal protection for providing it; suffering-prolonged individuals would remain trapped. The world of end-of-life practice would rearrange substantially — the constraint is not a natural fact but a constructed legal/ethical arrangement.
% FOUNDING_PROBLEM: The historical problem was medical technology's capacity to prolong biological life indefinitely without regard for the person's values, creating a new category of suffering: competent individuals trapped in bodies they experience as sources of unrelievable torment, with no legal exit. The autonomy reading was built to solve this by establishing the person as the sovereign authority over the boundary between life and death.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the autonomy tradition itself (Dworkin's 'Life's Dominion', Brock's 'Voluntary Active Euthanasia', Oregon/Washington/Canada/Netherlands legislative findings). Outside corroboration: palliative care clinicians who witness the suffering-prolonged cohort (Chapple et al. BMJ 2006; Ganzini et al. JAMA 2009), disability rights advocates who acknowledge the reality of unrelievable suffering while disputing the autonomy solution (Not Dead Yet testimony), and the European Court of Human Rights (Pretty v UK, Lambert v France) recognizing the Article 8 engagement even while deferring to states.
narrative_ontology:disappearance_verdict(end_of_life_decision_authority__autonomy_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_decision_authority__autonomy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_decision_authority__autonomy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(end_of_life_decision_authority__autonomy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_decision_authority__autonomy_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_decision_authority__autonomy_reading_tests).
:- end_tests(end_of_life_decision_authority__autonomy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15 at interval end) because the constraint primarily *removes* extraction — it stops the state/medical establishment from extracting continued life from competent adults who do not want it. The residual 0.15 reflects implementation costs (safeguards, assessments, oversight) that fall partly on the healthcare system and partly on requesters (waiting periods, multiple assessments). Suppression is low-moderate (0.25) because while the constraint itself is not suppressive, the residual prohibition in non-permissive jurisdictions still suppresses access for many. Theater ratio is low (0.10) because the safeguards (capacity assessment, voluntariness verification, waiting periods) are functional — they serve the genuine coordination need of preventing error/coercion, not performative compliance. Accessibility collapse is moderate (0.35) because alternatives (palliative sedation, VSED, travel) exist but are imperfect substitutes. Resistance is moderate (0.55) reflecting ongoing political, religious, and professional opposition even in permissive jurisdictions.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (suffering_prolonged_denied_access) experiences the *absence* of this constraint as high extraction and high suppression — they are trapped in suffering the constraint would relieve. The beneficiary seats (competent_adults_seeking_death, healthcare_professionals_facilitating) experience the constraint's presence as coordination: it solves the authority problem cleanly. The engine computes this divergence from the structural data: the same legal arrangement is extractive from the denied-access seat and coordinative from the granted-access seat. The claimed_type 'rope' reflects the autonomy reading's self-understanding; the engine may compute different per-seat types.
 *
 * DIRECTIONALITY LOGIC:
 *   The autonomy reading structurally positions competent adults seeking death as beneficiaries (they gain legal access to a peaceful death aligned with their values) and the suffering-prolonged denied access as payers under the *counterfactual* prohibition regime (they bear the cost of the constraint's absence). In permissive jurisdictions, the suffering-prolonged become beneficiaries too — the constraint's expansion converts them from payers to beneficiaries. Healthcare professionals facilitating are dual-positioned: beneficiaries (professional integrity, legal protection) and agenda-setters (they define and implement the clinical pathway). Religious/ethical opponents and vulnerable-population advocates are excluded — their objections operate at the kernel level, not within this reading's framework. The analytical observer seat sees the full structural field.
 *
 * MANDATROPHY ANALYSIS:
 *   The autonomy reading's founding problem (unwanted life prolongation by medical technology) remains live — if anything, technology has expanded the space of prolonged dying. The constraint has not atrophied; its domain has expanded. Mandatrophy is not resolved; the mandate is arguably strengthening as more jurisdictions adopt autonomy-based frameworks. The vulnerability_protection_reading's systemic risk concerns (coercion, inadequate palliative care) represent a genuine contested successor-problem, not mandatrophy of the original.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_vs_vulnerability_boundary,
    'Where does the autonomy reading''s sovereign authority boundary dissolve into the vulnerability protection reading''s distributed checkpoint model? At what point do safeguards become vetoes?',
    'Empirical analysis of jurisdictions with autonomy-based laws: track rates of denied requests, reasons for denial, time-to-access, and whether safeguards function as quality filters or de facto prohibitions for specific populations (cognitive impairment, psychiatric diagnosis, disability).',
    'If safeguards function as vetoes for vulnerable populations, the autonomy reading de facto converges on the vulnerability_protection_reading''s distributed authority model. If safeguards are near-universally passable for competent requesters, the autonomy boundary holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_vs_vulnerability_boundary, empirical, 'Whether autonomy-based safeguards operate as functional filters or structural vetoes for vulnerable populations.').

omega_variable(
    sanctity_foreclosure_mechanism,
    'Does the autonomy reading''s legal recognition *require* the sanctity reading''s foreclosure, or can a pluralistic framework accommodate both (e.g., conscientious objection for clinicians, institutional opt-out for religious hospitals) without logical contradiction?',
    'Legal-philosophical analysis of conscience clauses in permissive jurisdictions: do they create a dual-track system that preserves sanctity commitments for objectors while securing autonomy for requesters, or do they create access gaps that undermine the autonomy right?',
    'If pluralistic accommodation is stable, the forecloses relation is too strong — the readings coexist_with at the institutional level. If accommodation creates systematic access denial, forecloses stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sanctity_foreclosure_mechanism, conceptual, 'Whether sanctity-based objection can be institutionally accommodated without negating autonomy-based access.').

omega_variable(
    slippery_slope_empirical_status,
    'Does the autonomy reading''s externalization of slippery-slope risk (to the vulnerability_protection_reading) hold empirically? Have permissive jurisdictions seen expansion beyond competent adults (to minors, psychiatric-only, dementia advance directives) that validates the vulnerability concern?',
    'Longitudinal tracking of eligibility expansions in Netherlands, Belgium, Canada, Oregon: statutory changes, court rulings, reported cases. Compare expansion trajectory to vulnerability_protection_reading''s predicted failure modes.',
    'If expansions track vulnerability predictions, the autonomy reading''s externalization is falsified — the slippery slope is internal to the autonomy framework''s logic. If expansions are absent or constrained, the externalization holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(slippery_slope_empirical_status, empirical, 'Whether empirical expansion patterns in autonomy-based regimes validate vulnerability_protection_reading''s risk model.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_decision_authority__autonomy_reading, 1990, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t1990, end_of_life_decision_authority__autonomy_reading, theater_ratio, 1990, 0.4).
narrative_ontology:measurement(end__tr_t1997, end_of_life_decision_authority__autonomy_reading, theater_ratio, 1997, 0.35).
narrative_ontology:measurement(end__tr_t2001, end_of_life_decision_authority__autonomy_reading, theater_ratio, 2001, 0.25).
narrative_ontology:measurement(end__tr_t2009, end_of_life_decision_authority__autonomy_reading, theater_ratio, 2009, 0.18).
narrative_ontology:measurement(end__tr_t2016, end_of_life_decision_authority__autonomy_reading, theater_ratio, 2016, 0.12).
narrative_ontology:measurement(end__tr_t2025, end_of_life_decision_authority__autonomy_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(end__be_t1990, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 1990, 0.65).
narrative_ontology:measurement(end__be_t1997, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 1997, 0.55).
narrative_ontology:measurement(end__be_t2001, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 2001, 0.4).
narrative_ontology:measurement(end__be_t2009, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 2009, 0.3).
narrative_ontology:measurement(end__be_t2016, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 2016, 0.22).
narrative_ontology:measurement(end__be_t2025, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 2025, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t1990, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(end__su_t1997, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 1997, 0.55).
narrative_ontology:measurement(end__su_t2001, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 2001, 0.4).
narrative_ontology:measurement(end__su_t2009, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 2009, 0.3).
narrative_ontology:measurement(end__su_t2016, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 2016, 0.25).
narrative_ontology:measurement(end__su_t2025, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 2025, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_decision_authority__autonomy_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(end_of_life_decision_authority__autonomy_reading, 0.08).
narrative_ontology:affects_constraint(end_of_life_decision_authority__autonomy_reading, end_of_life_decision_authority__sanctity_reading).
narrative_ontology:affects_constraint(end_of_life_decision_authority__autonomy_reading, end_of_life_decision_authority__vulnerability_protection_reading).

% DUAL FORMULATION NOTE:
% This constraint is the autonomy_reading of the end_of_life_decision_authority kernel. The kernel decomposes into three structurally distinct constraints with different ε values, beneficiary/victim structures, and claimed types. The autonomy reading (this file) claims rope with low extractiveness; sanctity_reading claims mountain with near-zero extractiveness (from its own frame); vulnerability_protection_reading claims scaffold with moderate extractiveness (institutional checkpoint costs). They are linked via affects_constraints and reading_relations in cs_structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(end_of_life_decision_authority__autonomy_reading, powerless, 0.9).
constraint_indexing:directionality_override(end_of_life_decision_authority__autonomy_reading, moderate, 0.2).
constraint_indexing:directionality_override(end_of_life_decision_authority__autonomy_reading, organized, 0.15).
constraint_indexing:directionality_override(end_of_life_decision_authority__autonomy_reading, institutional, 0.3).
constraint_indexing:directionality_override(end_of_life_decision_authority__autonomy_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
