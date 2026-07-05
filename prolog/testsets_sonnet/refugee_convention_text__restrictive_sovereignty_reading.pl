% ============================================================================
% CONSTRAINT STORY: refugee_convention_text__restrictive_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_refugee_convention_text__restrictive_sovereignty_reading, []).

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
 *   constraint_id: refugee_convention_text__restrictive_sovereignty_reading
 *   human_readable: 1951 Refugee Convention — Restrictive Sovereignty Reading (Narrow Persecution Standard)
 *   domain: international_law/migration_governance/human_rights
 *
 * SUMMARY:
 *   This story instantiates the restrictive sovereignty reading of the 1951
 *   Refugee Convention kernel: the treaty text as a minimum floor that
 *   preserves maximum sovereign discretion over admission, requiring
 *   individualized proof of persecution for 'well-founded fear' and confining
 *   'particular social group' to immutable characteristics the persecuting
 *   state is aware of. This is a distinct constraint from the expansive
 *   humanitarian reading and the procedural integrity reading of the same
 *   kernel — those are separate stories with separate ε values, linked here
 *   only through the kernel and cs_structure fields, per the ε-invariance
 *   principle. Under this reading, the coordination function (a workable,
 *   bounded admission standard) is real, but it operates alongside asymmetric
 *   extraction: destination states and their enforcement and contracting
 *   apparatus retain discretion and revenue, while claimants who cannot
 *   satisfy the narrow individualized-nexus and immutable-characteristic
 *   tests are excluded from protection regardless of the danger they face.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__restrictive_sovereignty_reading, 0.66).
domain_priors:suppression_score(refugee_convention_text__restrictive_sovereignty_reading, 0.71).
domain_priors:theater_ratio(refugee_convention_text__restrictive_sovereignty_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__restrictive_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(refugee_convention_text__restrictive_sovereignty_reading, "1951 Refugee Convention — Restrictive Sovereignty Reading (Narrow Persecution Standard)").
narrative_ontology:topic_domain(refugee_convention_text__restrictive_sovereignty_reading, "international_law/migration_governance/human_rights").

domain_priors:requires_active_enforcement(refugee_convention_text__restrictive_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__restrictive_sovereignty_reading, 'adee2131-2ada-44c4-88c7-ffcd7e50d3c7').
narrative_ontology:cs_kernel_codification('adee2131-2ada-44c4-88c7-ffcd7e50d3c7', fixed_text).
narrative_ontology:cs_authority_grounding('adee2131-2ada-44c4-88c7-ffcd7e50d3c7', lineage).
narrative_ontology:cs_interpretation_layer_present('adee2131-2ada-44c4-88c7-ffcd7e50d3c7').
narrative_ontology:cs_reading_relation('adee2131-2ada-44c4-88c7-ffcd7e50d3c7', refugee_convention_text__expansive_humanitarian_reading, coexists_with).
narrative_ontology:cs_reading_relation('adee2131-2ada-44c4-88c7-ffcd7e50d3c7', refugee_convention_text__procedural_integrity_reading, influences).
narrative_ontology:cs_axiom('adee2131-2ada-44c4-88c7-ffcd7e50d3c7', foundational, sovereignty_retains_primary_admission_control).
narrative_ontology:cs_axiom_status(sovereignty_retains_primary_admission_control, holdable).
narrative_ontology:cs_axiom_grounding('adee2131-2ada-44c4-88c7-ffcd7e50d3c7', sovereignty_retains_primary_admission_control, conventional).
narrative_ontology:cs_axiom('adee2131-2ada-44c4-88c7-ffcd7e50d3c7', foundational, persecution_requires_individualized_state_directed_targeting).
narrative_ontology:cs_axiom_status(persecution_requires_individualized_state_directed_targeting, holdable).
narrative_ontology:cs_axiom_grounding('adee2131-2ada-44c4-88c7-ffcd7e50d3c7', persecution_requires_individualized_state_directed_targeting, empirically_contingent).
narrative_ontology:cs_reference_frame('adee2131-2ada-44c4-88c7-ffcd7e50d3c7', sovereign_discretion_baseline).
narrative_ontology:cs_drift_state('adee2131-2ada-44c4-88c7-ffcd7e50d3c7', contemporary_mass_displacement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('adee2131-2ada-44c4-88c7-ffcd7e50d3c7', '').
narrative_ontology:cs_kernel_id(refugee_convention_text__restrictive_sovereignty_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__restrictive_sovereignty_reading, destination_state_governments).
narrative_ontology:constraint_beneficiary(refugee_convention_text__restrictive_sovereignty_reading, border_enforcement_agencies).
narrative_ontology:constraint_beneficiary(refugee_convention_text__restrictive_sovereignty_reading, offshore_processing_contractors).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, asylum_seekers_fleeing_generalized_violence).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, gender_and_lgbtq_persecuted_claimants).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, non_state_persecution_claimants).
narrative_ontology:constraint_vindicates(refugee_convention_text__restrictive_sovereignty_reading, state_sovereignty_over_admission).
narrative_ontology:constraint_vindicates(refugee_convention_text__restrictive_sovereignty_reading, convention_as_minimum_floor_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ratify and domesticate the Convention through implementing legislation and adjudicative guidance that construes 'well-founded fear' and 'particular social group' narrowly. Sets admissibility screening thresholds, designates safe third countries, and authorizes offshore processing arrangements. Retains full discretion over how narrowly to read the treaty text and bears no binding external correction when it construes ambiguity restrictively.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, destination_state_governments, agenda_setter,
    institutional, generational, arbitrage, national).

% Operationalize the restrictive reading through credibility assessments, individualized-nexus interviews, and rapid inadmissibility determinations. Their institutional mandate and budget justification depend on the narrow standard remaining in force; a broader reading would expand caseload and reduce their gatekeeping function.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, border_enforcement_agencies, agenda_setter,
    institutional, immediate, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__restrictive_sovereignty_reading, border_enforcement_agencies, beneficiary).

% Operate third-country detention and processing facilities under contracts that exist because the restrictive reading legitimizes extraterritorial screening as compliant with non-refoulement. Revenue depends directly on the volume of claims diverted offshore under this reading; a humanitarian or procedural reading that curtailed offshore processing would eliminate their business model.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, offshore_processing_contractors, beneficiary,
    organized, biographical, mobile, regional).

% Flee civil conflict, gang control, or state collapse without being able to identify a specific persecutor targeting them individually. Under this reading, generalized violence does not establish 'well-founded fear' absent individualized targeting, so their claims are systematically found inadmissible or rejected regardless of the danger they actually face. They have no forum to contest the interpretive choice itself.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, asylum_seekers_fleeing_generalized_violence, payer,
    powerless, immediate, trapped, global).

% Face persecution rooted in gender identity, sexual orientation, or gender-based violence. Under the immutable-characteristics-plus-state-awareness test, adjudicators frequently find their group either too socially constructed to qualify or insufficiently 'known' to the persecuting state, closing off protection that a broader social-group reading would recognize.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, gender_and_lgbtq_persecuted_claimants, payer,
    powerless, immediate, trapped, global).

% Are persecuted by cartels, militias, or family/community actors rather than the state itself. The restrictive reading's requirement of state involvement or state awareness of persecution excludes claims where the state is merely unable, rather than unwilling, to protect them — leaving them without a Convention remedy even where the danger is severe and documented.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, non_state_persecution_claimants, payer,
    powerless, immediate, trapped, global).

% Publish interpretive guidance favoring a broader reading of both 'well-founded fear' and 'particular social group,' but have no binding enforcement power over sovereign treaty interpretation. Their guidance is cited in litigation and diplomatic pressure but does not bind states that elect the restrictive construction; they are consulted rhetorically but excluded from the actual determination process.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, unhcr_and_treaty_monitoring_bodies, excluded,
    organized, generational, constrained, global).

% Apply the restrictive standard case by case under statutory and precedential constraint, sometimes straining against it in individual dispositions but bound by appellate doctrine that entrenches the narrow reading. Individually they can dissent in reasoning but cannot alter the interpretive floor set above them.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, domestic_asylum_adjudicators, observer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__restrictive_sovereignty_reading, domestic_asylum_adjudicators, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(refugee_convention_text__restrictive_sovereignty_reading, destination_state_governments).
narrative_ontology:fixing_cost_class(refugee_convention_text__restrictive_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides destination states a textually defensible, administrable screening standard that allows them to distinguish a bounded category of individually-targeted persecution claims from the much larger universe of displacement, enabling predictable admission caseloads and coordinated interstate burden allocation (safe-third-country and offshore arrangements) without requiring case-by-case renegotiation of what the treaty means.
% TRANSFER_FUNCTION: Moves the cost and risk of protection away from destination states and onto asylum seekers who cannot satisfy the individualized-nexus and immutable-characteristic tests: claimants fleeing generalized or non-state violence absorb the risk of return, while destination states retain full discretion over admission numbers and offshore contractors capture the processing budget that would otherwise fund direct in-country adjudication.
% ABSENT_VOICES: Claimants themselves have no interpretive voice in how 'well-founded fear' or 'particular social group' is construed — the reading is set by domestic courts and executive guidance in destination states. UNHCR's broader interpretive guidance is cited but not binding. Sending-region civil society and refugee-led organizations that document generalized and non-state violence are absent from the treaty-interpretation forum entirely.
% DISAPPEARANCE_RATIONALE: If the restrictive reading were abandoned in favor of the expansive humanitarian reading, admissibility rates for generalized-violence, gender-based, and non-state persecution claims would rise substantially, offshore processing arrangements would lose their doctrinal justification for diverting claims, and border enforcement caseloads would expand — the current low-admission equilibrium in several major destination states depends structurally on this specific interpretive choice, not merely on Convention text that would support multiple readings.
% FOUNDING_PROBLEM: The 1951 Convention was drafted post-WWII to give states a workable, sovereignty-respecting standard for identifying genuine political refugees while preventing indefinite, undifferentiated obligations to admit any displaced person — a problem of distinguishing bounded persecution from generalized upheaval in an era of mass displacement.
% FOUNDING_PROBLEM_CORROBORATION: Destination-state governments and their courts attest the narrow reading is the Convention's original and continuing bargain. UNHCR, refugee law scholars outside government service, and dissenting judicial opinions in asylum appellate courts attest that the drafting history and subsequent state practice support a broader reading, and that the restrictive construction has hardened over decades specifically to manage admission volume rather than to track the treaty's textual commitments — corroboration exists on both sides, which is why the founding-problem status is authored as contested rather than resolved.
narrative_ontology:disappearance_verdict(refugee_convention_text__restrictive_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(refugee_convention_text__restrictive_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(refugee_convention_text__restrictive_sovereignty_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(refugee_convention_text__restrictive_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(refugee_convention_text__restrictive_sovereignty_reading, 0.66, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(refugee_convention_text__restrictive_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(refugee_convention_text__restrictive_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(refugee_convention_text__restrictive_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate-high and rising level (0.42 to 0.66) because the interpretive narrowing has hardened over decades of appellate precedent and executive guidance, increasingly diverting claims into inadmissibility rather than merits review. Suppression is high and rising (0.48 to 0.71) because the mechanism depends on active enforcement — offshore processing, safe-third-country removals, and admissibility screening — that forecloses alternative adjudicative pathways for claimants who cannot reframe their claim to fit the narrow test. Theater ratio is moderate and rising (0.22 to 0.42): a genuine screening function persists, but a growing share of process (credibility interviews structured to elicit individualized-nexus narratives, offshore facility operations justified as compliant with non-refoulement) increasingly performs legal compliance while the substantive admission rate for at-risk populations falls.
 *
 * PERSPECTIVAL GAP:
 *   From the destination-state seat, this reading is coordination: a workable, sovereignty-preserving standard that lets states plan admission and burden-sharing predictably. From the excluded-claimant seat, the identical textual provisions operate as an engineered exclusion mechanism — the treaty's protective language is real, but the interpretive gloss determines who can ever invoke it. The engine's per-seat computation should register this divergence structurally rather than resolve it toward either seat's self-account.
 *
 * DIRECTIONALITY LOGIC:
 *   Destination-state governments and enforcement agencies sit near the beneficiary end: they set and administer the restrictive interpretation and retain full discretion with no binding external correction. Offshore processing contractors are structural beneficiaries whose business model depends on the reading's legitimation of extraterritorial screening. Asylum seekers fleeing generalized violence, gender/LGBTQ+ claimants, and non-state persecution claimants sit at the full-target end: trapped exit options, powerless structural position, and a legal test specifically constructed to exclude their fact patterns from protection. UNHCR and monitoring bodies are excluded rather than coordinated — consulted rhetorically, bound not at all.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (distinguishing bounded, genuine persecution from generalized post-war displacement, in a treaty regime designed to be administrable) has arguably shifted: contemporary displacement is disproportionately driven by generalized conflict, climate-linked collapse, and non-state armed actors — exactly the categories this reading excludes. If the founding problem is understood as identifying genuine need for international protection, it remains live but the restrictive reading no longer tracks it; if understood narrowly as identifying individually-targeted political persecution by state actors, the problem framing itself may be dated. This ambiguity is why founding_problem_status is authored as contested rather than dead — corroboration diverges by seat, which is itself the signal the mandatrophy question exists to surface.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_indeterminacy_vs_deliberate_narrowing,
    'Is the restrictive reading a good-faith textual interpretation of genuinely ambiguous Convention language, or a deliberately constructed narrowing designed to minimize admission obligations while retaining formal treaty compliance?',
    'Comparative analysis of travaux préparatoires alongside a longitudinal study of whether interpretive narrowing correlates with rising displacement pressure and public admission-reduction mandates in destination states, controlling for genuine legal doctrinal development.',
    'If deliberate narrowing under compliance cover, the coordination function is substantially pretextual and the constraint sits closer to a snare than a tangled rope; if good-faith interpretation of real ambiguity, the coordination function is more substantial and the tangled-rope classification is better supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_indeterminacy_vs_deliberate_narrowing, conceptual, 'Whether restrictive interpretation is genuine ambiguity-resolution or engineered exclusion.').

omega_variable(
    sovereignty_floor_vs_humanitarian_ceiling,
    'Does the Convention''s text and drafting history support ''minimum floor, maximum discretion'' as the operative default, or does state practice since 1951 establish a customary international law obligation toward the broader reading that displaces the floor framing?',
    'Systematic review of state practice, UNHCR Executive Committee conclusions, and regional human rights court jurisprudence (ECtHR, IACtHR) for evidence of an emerging customary norm that would override the restrictive default.',
    'If customary law has moved toward the broader reading, this restrictive-reading constraint''s claimed legitimacy erodes even on its own terms — it would be applying an interpretation increasingly at odds with binding custom rather than merely one permissible reading among several.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_floor_vs_humanitarian_ceiling, empirical, 'Whether customary international law has superseded the sovereignty-floor default this reading relies on.').

omega_variable(
    framing_choice_kernel_vs_authority,
    'Should this constraint be framed as a reading of the Convention TEXT (kernel = treaty language) or as a reading of the JUDICIAL AUTHORITY that has accreted around it (kernel = domestic appellate precedent construing the text)? The two framings could produce different cs_pattern classifications: text-as-kernel suggests fixed_text/practice; precedent-as-kernel suggests formalized/lineage with a much thicker interpretation layer.',
    'Track whether restrictive outcomes trace more directly to Convention drafting history/text or to accumulated domestic case law that has drifted from the text''s plain meaning — a jurisdiction-by-jurisdiction doctrinal history would distinguish these.',
    'If precedent-as-kernel is the better framing, the interpretation_layer_present flag and authority_grounding value would shift toward a thicker practice/lineage structure with more absorption capacity for drift, changing how much of the observed extraction should be attributed to text versus accreted doctrine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_choice_kernel_vs_authority, conceptual, 'Alternative framing of the kernel as treaty text versus accreted domestic precedent, and its effect on cs_structure classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__restrictive_sovereignty_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refu_tr_t0, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(refu_tr_t8, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 8, 0.27).
narrative_ontology:measurement(refu_tr_t16, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 16, 0.32).
narrative_ontology:measurement(refu_tr_t24, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 24, 0.36).
narrative_ontology:measurement(refu_tr_t32, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 32, 0.39).
narrative_ontology:measurement(refu_tr_t40, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(refu_be_t0, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(refu_be_t8, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 8, 0.49).
narrative_ontology:measurement(refu_be_t16, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(refu_be_t24, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(refu_be_t32, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 32, 0.63).
narrative_ontology:measurement(refu_be_t40, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 40, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(refu_su_t0, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(refu_su_t8, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(refu_su_t16, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 16, 0.61).
narrative_ontology:measurement(refu_su_t24, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 24, 0.65).
narrative_ontology:measurement(refu_su_t32, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 32, 0.68).
narrative_ontology:measurement(refu_su_t40, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_text__restrictive_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(refugee_convention_text__restrictive_sovereignty_reading, 0.1).
narrative_ontology:affects_constraint(refugee_convention_text__restrictive_sovereignty_reading, expansive_humanitarian_reading).
narrative_ontology:affects_constraint(refugee_convention_text__restrictive_sovereignty_reading, procedural_integrity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the refugee_convention_text kernel (restrictive_sovereignty_reading, expansive_humanitarian_reading, procedural_integrity_reading). Each reading is authored as its own ε-invariant constraint with its own stakeholders, metrics, and classification, per the ε-invariance principle — they are not one constraint measured three ways. The network edges here record that the readings compete for institutional and doctrinal dominance within the same treaty regime: legitimation gained by one reading (e.g., a landmark appellate adoption of the restrictive standard) creates downstream pressure on the others' viability without logically foreclosing them, since different courts and states adopt different readings simultaneously.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
