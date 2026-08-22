% ============================================================================
% CONSTRAINT STORY: end_of_life_authority__slippery_slope_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_authority__slippery_slope_mechanism, []).

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
 *   constraint_id: end_of_life_authority__slippery_slope_mechanism
 *   human_readable: Autonomy Framework Scope Expansion in End-of-Life Authority
 *   domain: medical_ethics/bioethics
 *
 * SUMMARY:
 *   This constraint is ONE READING of a contested kernel: end-of-life
 *   authority. The kernel is the persisting commitment to how individual
 *   choice and institutional decision-making interact in determining who dies
 *   and when. The autonomy_reading grounds legitimacy in individual
 *   self-determination; the sanctity_reading grounds it in intrinsic human
 *   dignity; the slippery_slope_mechanism reading claims that autonomy
 *   frameworks, once established, empirically expand beyond their original
 *   scope (competent terminal patients) to encompass incompetent patients and
 *   chronic non-terminal populations. This reading instantiates the
 *   slippery-slope mechanism as a constraint in its own right: the expansion
 *   process itself becomes extractive, progressively expanding institutional
 *   authority to determine death while reframing the expansion as consistent
 *   with autonomy. The incompetent and non-terminal populations enter the
 *   victim set in this reading because the expansion mechanisms pull them
 *   into end-of-life scope without their consent and often against
 *   disability-based objections. The foundational autonomy principle becomes
 *   the vehicle for sanctity-like concerns (removing socially burdensome
 *   populations) while maintaining the autonomy framing as cover.
 *
 * KEY AGENTS:
 *   - autonomy_advocates: institutional organizers who control legislative and professional framing; push for scope expansion
 *   - death_facilitation_practitioners: physicians and institutional operators who gain discretionary authority as scope expands; benefit from professional legitimacy
 *   - incompetent_patients: powerless, trapped; subject to best-interest determinations under expanded frameworks
 *   - chronic_non_terminal_populations: moderate power but constrained exit; vulnerable to reframing of disability/suffering as death-worthy condition
 *   - disability_rights_advocates: excluded from policy-setting; object to disability conflation with unbearable suffering
 *   - sanctity_advocates: excluded from professional framing; their moral objections relegated to conscience clauses
 *   - medical_professional_bodies: institutional agenda-setters; write guidelines that operationalize scope expansion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__slippery_slope_mechanism, 0.68).
domain_priors:suppression_score(end_of_life_authority__slippery_slope_mechanism, 0.71).
domain_priors:theater_ratio(end_of_life_authority__slippery_slope_mechanism, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, extractiveness, 0.68).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__slippery_slope_mechanism, tangled_rope).
narrative_ontology:human_readable(end_of_life_authority__slippery_slope_mechanism, "Autonomy Framework Scope Expansion in End-of-Life Authority").
narrative_ontology:topic_domain(end_of_life_authority__slippery_slope_mechanism, "medical_ethics/bioethics").

domain_priors:requires_active_enforcement(end_of_life_authority__slippery_slope_mechanism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__slippery_slope_mechanism, '97934ece-d9c2-489b-a1b9-b699de9dc6c7').
narrative_ontology:cs_kernel_codification('97934ece-d9c2-489b-a1b9-b699de9dc6c7', formalized).
narrative_ontology:cs_authority_grounding('97934ece-d9c2-489b-a1b9-b699de9dc6c7', extraction).
narrative_ontology:cs_interpretation_layer_present('97934ece-d9c2-489b-a1b9-b699de9dc6c7').
narrative_ontology:cs_reading_relation('97934ece-d9c2-489b-a1b9-b699de9dc6c7', end_of_life_authority__autonomy_reading, coexists_with).
narrative_ontology:cs_reading_relation('97934ece-d9c2-489b-a1b9-b699de9dc6c7', end_of_life_authority__sanctity_reading, influences).
narrative_ontology:cs_axiom('97934ece-d9c2-489b-a1b9-b699de9dc6c7', foundational, autonomy_framework_empirically_expands).
narrative_ontology:cs_axiom_status(autonomy_framework_empirically_expands, holdable).
narrative_ontology:cs_axiom_grounding('97934ece-d9c2-489b-a1b9-b699de9dc6c7', autonomy_framework_empirically_expands, empirically_contingent).
narrative_ontology:cs_axiom('97934ece-d9c2-489b-a1b9-b699de9dc6c7', secondary, scope_expansion_obscured_by_autonomy_cover_story).
narrative_ontology:cs_axiom_status(scope_expansion_obscured_by_autonomy_cover_story, holdable).
narrative_ontology:cs_axiom_grounding('97934ece-d9c2-489b-a1b9-b699de9dc6c7', scope_expansion_obscured_by_autonomy_cover_story, instrumental).
narrative_ontology:cs_reference_frame('97934ece-d9c2-489b-a1b9-b699de9dc6c7', terminal_illness_autonomy_protection).
narrative_ontology:cs_drift_state('97934ece-d9c2-489b-a1b9-b699de9dc6c7', contemporary_expanded_eligibility, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('97934ece-d9c2-489b-a1b9-b699de9dc6c7', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__slippery_slope_mechanism, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, death_facilitation_practitioners).
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, institutional_death_administrators).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, incompetent_patients).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, chronic_non_terminal_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, family_surrogate_decision_makers).
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, medical_professional_bodies).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, death_facilitation_practitioners).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, family_surrogate_decision_makers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Push for expanding end-of-life decision authority based on individual autonomy and self-determination. Frame eligibility criteria permissively: competent patients with unbearable suffering, then competent chronic patients, then incompetent patients via advance directives, then incompetent patients lacking directives via best-interest determinations. Control legislative framing and professional guideline development. Justify each expansion as consistent with the foundational autonomy principle.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, autonomy_advocates, agenda_setter,
    organized, generational, mobile, national).

% Gain institutional authority and professional legitimacy from operating within autonomy-based frameworks. Operate end-of-life protocols; expand their scope of eligible patients increases their functional authority and reduces external oversight (earlier stages of the slippery slope involve clearer legal constraints; later stages involve ambiguous best-interest determinations where medical judgment controls). Simultaneously bear moral and legal liability for end-of-life decisions, particularly as eligibility criteria become contested.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, death_facilitation_practitioners, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__slippery_slope_mechanism, death_facilitation_practitioners, payer).

% Cannot express preferences; cannot refuse or consent to end-of-life decisions. Lack advance directives or their directives are ambiguous. Subject to best-interest determinations made by medical professionals, family members, and proxy decision-makers operating under expanded autonomy frameworks. The framework's expansion retroactively includes them as eligible for death without explicit consent.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, incompetent_patients, payer,
    powerless, immediate, trapped, local).

% Competent but vulnerable: chronic pain, mental illness, severe disability, social isolation. Originally outside end-of-life scope (not terminal). Progressively included as autonomy framework expands to cover 'unbearable suffering' rather than terminal condition. Experience pressure to frame their continued living as burden-on-others; their autonomy claim becomes entangled with institutional interests in managing chronic populations economically.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, chronic_non_terminal_populations, payer,
    moderate, biographical, constrained, national).

% Given formal decision-making authority for incompetent patients; benefit from reduced long-term caregiving burden. Simultaneously bear emotional and moral weight of life-death decisions. Experience institutional and social pressure to choose death as the compassionate option, particularly as expanded scope includes chronic non-terminal cases.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, family_surrogate_decision_makers, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__slippery_slope_mechanism, family_surrogate_decision_makers, payer).

% Object to the slippery-slope expansion as conflating disability with unbearable suffering and treating disability-based life as candidate for death. Argue that the autonomy framework, applied to socially marginalized populations, becomes vehicle for removing people the society has failed to support adequately. Excluded from professional guideline development and policy-setting; their testimony is treated as special interest rather than structural analysis.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, disability_rights_advocates, excluded,
    organized, generational, mobile, national).

% Object to autonomy-based frameworks on the grounds that human life has intrinsic value and intentional killing violates human dignity. Their position is increasingly isolated from policy-setting as the autonomy reading dominates legal and professional framing. Excluded from institutional decision-making; their moral claims are relegated to conscience-clause protections rather than authority over scope.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, sanctity_advocates, excluded,
    moderate, generational, mobile, national).

% Write and revise professional guidelines; each revision expands eligible populations and reduces explicit criteria, shifting decision-making from rules to professional judgment. Benefit from discretionary authority; constrained by legal liability and public controversy.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, medical_professional_bodies, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__slippery_slope_mechanism, medical_professional_bodies, beneficiary).

% Set legal frameworks; successive legislative and judicial decisions expand the autonomy principle from terminal illness to broader suffering, from explicitly competent patients to surrogate decision-making for incompetent patients. Each expansion is defended as consistent with the foundational autonomy principle while progressively shifting who benefits from the decision-making structure.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, legislatures_and_courts, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(end_of_life_authority__slippery_slope_mechanism, medical_professional_bodies).
narrative_ontology:fixing_cost_class(end_of_life_authority__slippery_slope_mechanism, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified decision-making framework for end-of-life authority: rather than ad-hoc, secretive, or legally ambiguous end-of-life decisions, autonomy-based frameworks create transparent, formalized authority structures in which medical professionals, legal frameworks, and proxies operate. The coordination function is to make end-of-life practices explicit, rule-governed, and publicly defensible.
% TRANSFER_FUNCTION: Transfers decision-making authority from individual patients (when competent) to medical professionals, legal guardians, and best-interest determinations (when patients are incompetent). The expansion progressively transfers the meaning of 'autonomy' from actual individual choice to surrogate-inferred choice, and then to institutional determinations of what choice the incompetent person would have made. Authority to determine who lives and dies migrates from explicit consent to professional judgment operating within increasingly broad frameworks.
% ABSENT_VOICES: Disabled people whose non-terminal conditions are reframed as 'unbearable suffering' by medical authorities; incompetent patients themselves (by definition, cannot speak); populations experiencing social marginalization (poverty, isolation, mental illness) whose vulnerability to death-choice pressure is not represented in professional guideline bodies; philosophical objectors to the autonomy-supremacy framing who are excluded from legal and professional policy-setting.
% DISAPPEARANCE_RATIONALE: If the slippery-slope mechanism and the autonomy framework that drives it vanished, end-of-life decisions would revert to case-by-case, legally ambiguous, informal arrangements (as they were pre-1990s) OR to strict terminal-illness-only rules that would exclude chronic non-terminal populations entirely. The expanded scope of end-of-life authority — now encompassing incompetent patients and chronic populations — would collapse. Institutional death-facilitation programs would lose their scope expansion and legal cover.
% FOUNDING_PROBLEM: Initial autonomy-based end-of-life frameworks (1970s–1990s) targeted a specific, narrow problem: competent terminal patients trapped in undignified, unbearably painful final days with no legal authority to control their death. The frameworks aimed to restore agency to that population by recognizing their right to refuse unwanted treatment and, subsequently, to seek physician-assisted death.
% FOUNDING_PROBLEM_CORROBORATION: The narrow problem — competent terminal patients facing undignified death — is acknowledged solved by all parties. Legal authority for competent patients to refuse unwanted treatment is now established in all jurisdictions. Physician-assisted death is legal in multiple jurisdictions. However, the autonomy framework has expanded FAR BEYOND the original problem: incompetent patients, chronic non-terminal populations, and patients with primarily psychiatric suffering now fall within eligibility. Disability rights organizations, sanctity advocates, and empirical researchers on scope expansion all attest that the expanding framework has decoupled from the founding problem. The founding problem is dead; the mechanism persists and expands.
narrative_ontology:disappearance_verdict(end_of_life_authority__slippery_slope_mechanism, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_authority__slippery_slope_mechanism, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__slippery_slope_mechanism, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(end_of_life_authority__slippery_slope_mechanism, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_authority__slippery_slope_mechanism, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_authority__slippery_slope_mechanism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_authority__slippery_slope_mechanism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(end_of_life_authority__slippery_slope_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.32 (t=0: narrow terminal scope, high friction, explicit consent) to 0.68 (t=35: expanded to incompetent and chronic, low friction for institutional expansion, proxy-determined consent). The acceleration is steepest t=0 to t=10 (competent terminal → competent non-terminal), then moderates as the scope plateau is reached. Suppression is high throughout (0.45→0.71) because the mechanism depends on silencing disability-rights objections and constraining sanctity-based challenge. Theater ratio rises from 0.15 (early stage: clear medical criteria for terminal status) to 0.42 (later stage: best-interest determinations and ambiguous suffering-worthiness dominate; the criteria become increasingly performative as institutional judgment expands). The constraint is CLAIMED as tangled_rope (genuine coordination function for terminal patients + asymmetric extraction targeting incompetent/chronic populations) while measured metrics show progressively higher extractiveness and theater, indicating the coordination function has increasingly become a cover story for expansion. One shared time grid: all three metrics are measured at every time point, enabling the compiler to detect coordinated drift and avoid mismeasurement artifacts.
 *
 * PERSPECTIVAL GAP:
 *   From the autonomy-advocates and institutional seats, the expansion is legitimate: each step is presented as extending the autonomy principle consistently to new populations (advance directives extend autonomy to future incompetent selves; best-interest determinations extend autonomy to incompetent others; chronic-suffering inclusion extends autonomy beyond terminal cases). From the incompetent-patient and disability-rights seats, the same expansion is experienced as loss of protection: the framework that supposedly protects individual choice progressively removes individuals from the decision-making process. The engine will compute these seats differently: autonomy-advocates occupy a beneficiary/agenda-setter position (d near 0.0); incompetent patients occupy a powerless-trapped position with no exit (d near 1.0). The divergence is the empirical finding the slippery-slope reading exists to capture.
 *
 * DIRECTIONALITY LOGIC:
 *   Autonomy-advocates and institutional professionals benefit from scope expansion (low d, negative effective extraction). Incompetent patients and chronic populations bear the extraction costs: they are progressively included in death-eligible scope without consent, subject to best-interest determinations that increasingly defer to institutional judgment rather than explicit individual preference. Disability advocates and sanctity advocates are excluded — their voices are suppressed from policy-setting — which concentrates the definition of 'unbearable suffering' among institutional and pro-expansion seats. The directionality override is unnecessary: the structural data (powerless-trapped for incompetent patients; organized-mobile for advocates; institutional-generational for professional bodies) drives the correct d-values without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading identifies a specific form of mandate creep: the founding mandate was to restore agency to competent terminal patients facing undignified death. That mandate is empirically accomplished (legal authority for competent refusal and, in many jurisdictions, physician-assisted death). However, the autonomy framework persists and expands far beyond the accomplished mandate, progressively redefining who is eligible for death-facilitation. The slippery-slope mechanism is the constraint that captures this mandatrophy: the expansion process itself becomes institutionalized, and the foundational autonomy principle becomes the vehicle for decisions that may violate individual autonomy (best-interest determinations for incompetent patients) or that pathologize disability and chronic suffering in ways the original mandate did not contemplate. The theater_ratio rise (0.15→0.42) models the progressive substitution of explicit criteria (terminal + unbearable pain + competent consent) with implicit criteria (institutional judgment about quality of life, burden on family, social utility). The framework's legitimacy no longer rests on individual autonomy per se but on institutional trust that professionals will make good life-death decisions. Mandatrophy is RESOLVED here: the founding problem (competent terminal patients) is dead; the mechanism (autonomy framework as expansionary institutional tool) persists and is operationalized as the constraint itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_threshold_drift,
    'What counts as ''competent'' to make end-of-life decisions, and does the threshold remain stable as frameworks expand, or does it shift downward to include borderline-competent or episodically-incompetent patients?',
    'Systematic review of jurisdiction-by-jurisdiction changes in competence standards (psychological, cognitive, decisional capacity); comparison of early and late guideline language; analysis of clinical denial/approval patterns for borderline cases over time.',
    'If the competence threshold shifts downward (e.g., accepting psychiatric patients'' end-of-life choices, accepting confusion episodes as compatible with decision-making authority), the slippery-slope mechanism is operationalized: the framework expands by redefining who is eligible, not merely by extending eligibility to previously-excluded-but-clearly-competent populations. This would indicate the extraction is higher than measured because the definitional capture is more complete.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competence_threshold_drift, empirical, 'Whether the competence standard drifts as eligibility expands').

omega_variable(
    best_interest_vs_substituted_judgment,
    'When frameworks shift from requiring explicit individual consent to permitting surrogate decision-making, does the standard revert to substituted judgment (what would the incompetent person have chosen if competent) or best-interest determination (what is objectively best for the incompetent person)?',
    'Analysis of legal and professional language shifts; qualitative research on actual surrogate decision-making rationales; tracking which standard is invoked when incompetent patients lack advance directives.',
    'Substituted judgment preserves a thin autonomy fiction (the incompetent person''s hypothetical choice). Best-interest determination is pure paternalism (the institution judges what is best). The shift from the former to the latter would indicate the expansion is not merely scope-increasing but is fundamentally changing the justificatory basis from autonomy to institutional judgment. Extractiveness would be even higher because the autonomy cover story is dropped and institutional authority becomes primary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(best_interest_vs_substituted_judgment, empirical, 'Whether the surrogate decision standard shifts from substituted judgment to best-interest paternalism as eligibility expands').

omega_variable(
    unbearable_suffering_definition,
    'How is ''unbearable suffering'' defined as the framework expands to chronic non-terminal populations? Does the definition remain tied to objective medical characteristics (terminal diagnosis, pain level), or does it progressively include social suffering (isolation, burden on family, social marginalization)?',
    'Systematic analysis of guideline language and court decisions over time; qualitative research on how practitioners and surrogate decision-makers operationalize ''unbearable suffering'' in practice; comparison of suffering definitions across jurisdictions with different scope expansions.',
    'If ''suffering'' progressively includes social marginalization and caregiver burden, the framework becomes a vehicle for removing socially burdensome populations regardless of individual autonomy or medical prognosis. This would indicate the extraction is not a side effect of scope expansion but the primary function being disguised as autonomy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unbearable_suffering_definition, empirical, 'Whether the definition of ''unbearable suffering'' drifts from objective medical to subjective social criteria').

omega_variable(
    disability_conflation_mechanism,
    'Are disability-based populations (severe disability, chronic illness, mental illness, neurodivergence) systematically overrepresented in end-of-life decision uptake compared to their population proportions? Are they subject to systematic differential pressure from medical professionals, family, or institutional actors?',
    'Epidemiological analysis of who receives end-of-life interventions; demographic comparison of uptake rates by disability status; qualitative interviews with disabled people about perceived pressure; analysis of medical provider recommendations by patient disability status.',
    'If disabled populations are systematically overrepresented (either relative to prevalence or relative to explicitly chosen preferences), the constraint operates as a hidden mechanism for removing socially marginalized populations. This would suggest the slippery-slope mechanism is not incidental but engineered — the autonomy framework is the tool by which institutional interests (cost reduction, social burden minimization) are operationalized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disability_conflation_mechanism, empirical, 'Whether disability-based populations are systematically targeted by end-of-life expansion mechanisms').

omega_variable(
    autonomy_reading_vs_slippery_slope_reading_foreclosure,
    'Does the slippery-slope mechanism reading''s core claim (that autonomy frameworks empirically expand beyond their scope, progressively redefining eligible populations) logically foreclose the autonomy_reading''s core claim (that individual autonomy grounds the right to control death), or do the two readings describe different institutional dynamics that could coexist in the same legal/professional framework?',
    'Philosophical analysis of whether demonstrating scope expansion constitutes a logical refutation of autonomy-grounding (it does not — the readings could coexist: autonomy might ground the right to death while the institutional implementation has become corrupted by scope expansion). Both readings remain theoretically live.',
    'This reading does NOT foreclose autonomy_reading; the two readings coexist_with each other. The slippery-slope mechanism reading is compatible with respecting autonomy IF the institutional implementation were reformed to prevent the scope expansion. This clarifies that the readings differ on institutional dynamics, not on foundational normative claims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(autonomy_reading_vs_slippery_slope_reading_foreclosure, conceptual, 'Clarification that slippery_slope_mechanism reading coexists with autonomy_reading rather than foreclosing it').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__slippery_slope_mechanism, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 0, 0.15).
narrative_ontology:measurement(end__tr_t5, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 5, 0.22).
narrative_ontology:measurement(end__tr_t10, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 10, 0.28).
narrative_ontology:measurement(end__tr_t15, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 15, 0.34).
narrative_ontology:measurement(end__tr_t20, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 20, 0.38).
narrative_ontology:measurement(end__tr_t25, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 25, 0.41).
narrative_ontology:measurement(end__tr_t30, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 30, 0.42).
narrative_ontology:measurement(end__tr_t35, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 35, 0.42).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(end__be_t5, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(end__be_t10, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 10, 0.51).
narrative_ontology:measurement(end__be_t15, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(end__be_t20, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(end__be_t25, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(end__be_t30, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(end__be_t35, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 35, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(end__su_t5, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(end__su_t10, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 10, 0.61).
narrative_ontology:measurement(end__su_t15, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 15, 0.66).
narrative_ontology:measurement(end__su_t20, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 20, 0.69).
narrative_ontology:measurement(end__su_t25, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(end__su_t30, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(end__su_t35, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 35, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__slippery_slope_mechanism, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(end_of_life_authority__slippery_slope_mechanism, 0.18).
narrative_ontology:affects_constraint(end_of_life_authority__slippery_slope_mechanism, end_of_life_authority__autonomy_reading).
narrative_ontology:affects_constraint(end_of_life_authority__slippery_slope_mechanism, end_of_life_authority__sanctity_reading).

% DUAL FORMULATION NOTE:
% The end_of_life_authority kernel decomposes into three structurally distinct constraints (three readings): autonomy_reading grounds legitimacy in individual self-determination; sanctity_reading grounds it in intrinsic human dignity; slippery_slope_mechanism (this story) describes the empirical institutional dynamic by which autonomy frameworks expand beyond their foundational scope, progressively redefining eligible populations and shifting decision-making from explicit consent to professional judgment. The readings are not the same constraint viewed from different angles — they have different ε values (autonomy_reading: lower extraction because it preserves individual agency; slippery_slope_mechanism: higher extraction because it captures institutional expansion; sanctity_reading: highest extraction because it excludes individual choice entirely). Each reading is compiled to a separate .pl file with its own stakeholder structure. They are linked here to enable contamination analysis: if the slippery_slope_mechanism reading's empirical claims are correct (scope expansion is documented), downstream analysis can assess whether that expansion strengthens or weakens the autonomy_reading's normative claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
