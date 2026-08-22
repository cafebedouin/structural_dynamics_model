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
 *   constraint_id: end_of_life_authority__slippery_slope_mechanism
 *   human_readable: End-of-Life Authority Slippery Slope: Autonomy Framework Expansion to Incompetent Populations
 *   domain: medical_ethics/bioethics
 *
 * SUMMARY:
 *   This constraint models the empirical mechanism by which autonomy-based
 *   end-of-life frameworks, initially justified as protecting the autonomous
 *   choices of competent terminal patients, systematically expand to
 *   encompass incompetent patients and non-terminal populations with chronic
 *   suffering. The reading instantiates the 'slippery slope' claim: that the
 *   autonomy rationale itself drives the expansion by reinterpreting what
 *   counts as 'unbearable suffering' (from terminal trajectory to subjective
 *   suffering), what counts as 'autonomy' (from explicit consent to proxy
 *   judgment to 'best interests'), and who counts as a victim entitled to
 *   life-ending intervention (from competent terminal to incompetent
 *   chronic-suffering to non-terminal suffering). This is one reading of the
 *   contested end-of-life-authority kernel; sibling readings
 *   (autonomy_reading, sanctity_reading) offer alternative structurings of
 *   the same institutional domain. The slippery-slope reading's distinctive
 *   claim is that the expansion is not a deviation from autonomy principle
 *   but an internal consequence of applying it.
 *
 * KEY AGENTS:
 *   - Competent terminal patients: paradigm beneficiaries initially; remain rhetorical center even as expansion proceeds beyond them
 *   - Medical authorities: agenda-setters who interpret and expand eligibility criteria; gain discretionary power as the framework expands
 *   - Incompetent patients: enter victim set as expansion occurs; powerless to resist proxy decisions framed as autonomous choice
 *   - Chronic-suffering non-terminal patients: enter victim set as 'unbearable suffering' is reinterpreted beyond terminal imminence
 *   - Patient guardians: bear decision-making burden transferred from medical institutions; constrained by asymmetric information and institutional pressure
 *   - Disability advocates: excluded from deliberation; argue expansion conflates disability with pathology
 *   - Bioethics oversight: analytical seats monitoring expansion but with limited enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__slippery_slope_mechanism, 0.68).
domain_priors:suppression_score(end_of_life_authority__slippery_slope_mechanism, 0.72).
domain_priors:theater_ratio(end_of_life_authority__slippery_slope_mechanism, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, extractiveness, 0.68).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, accessibility_collapse, 0.63).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__slippery_slope_mechanism, tangled_rope).
narrative_ontology:human_readable(end_of_life_authority__slippery_slope_mechanism, "End-of-Life Authority Slippery Slope: Autonomy Framework Expansion to Incompetent Populations").
narrative_ontology:topic_domain(end_of_life_authority__slippery_slope_mechanism, "medical_ethics/bioethics").

domain_priors:requires_active_enforcement(end_of_life_authority__slippery_slope_mechanism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__slippery_slope_mechanism, 'fc478f57-9a68-4daf-b5be-3e86c8c4afe4').
narrative_ontology:cs_kernel_codification('fc478f57-9a68-4daf-b5be-3e86c8c4afe4', formalized).
narrative_ontology:cs_authority_grounding('fc478f57-9a68-4daf-b5be-3e86c8c4afe4', extraction).
narrative_ontology:cs_interpretation_layer_present('fc478f57-9a68-4daf-b5be-3e86c8c4afe4').
narrative_ontology:cs_reading_relation('fc478f57-9a68-4daf-b5be-3e86c8c4afe4', end_of_life_authority__autonomy_reading, coexists_with).
narrative_ontology:cs_reading_relation('fc478f57-9a68-4daf-b5be-3e86c8c4afe4', end_of_life_authority__sanctity_reading, coexists_with).
narrative_ontology:cs_axiom('fc478f57-9a68-4daf-b5be-3e86c8c4afe4', foundational, autonomy_framework_inevitably_expands).
narrative_ontology:cs_axiom_status(autonomy_framework_inevitably_expands, holdable).
narrative_ontology:cs_axiom_grounding('fc478f57-9a68-4daf-b5be-3e86c8c4afe4', autonomy_framework_inevitably_expands, empirically_contingent).
narrative_ontology:cs_axiom('fc478f57-9a68-4daf-b5be-3e86c8c4afe4', secondary, expansion_mechanism_institutional_reinterpretation).
narrative_ontology:cs_axiom_status(expansion_mechanism_institutional_reinterpretation, holdable).
narrative_ontology:cs_axiom_grounding('fc478f57-9a68-4daf-b5be-3e86c8c4afe4', expansion_mechanism_institutional_reinterpretation, empirically_contingent).
narrative_ontology:cs_reference_frame('fc478f57-9a68-4daf-b5be-3e86c8c4afe4', autonomy_protection_terminal_competent_explicit).
narrative_ontology:cs_drift_state('fc478f57-9a68-4daf-b5be-3e86c8c4afe4', contemporary_expanded_criteria, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('fc478f57-9a68-4daf-b5be-3e86c8c4afe4', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__slippery_slope_mechanism, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, medical_authorities).
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, legal_jurisdictions_adopting_framework).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, incompetent_patients).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, chronic_suffering_non_terminal_patients).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, patient_guardians_under_pressure).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, competent_terminal_patients).
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, patient_guardians_under_pressure).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Framed as the paradigm case: individuals with decisional capacity facing imminent death from irreversible disease who seek to control the timing and manner of death. Their autonomy interest grounds the initial justification for the framework. As the reading progresses, they remain rhetorically central even as the actual expansion occurs beyond them.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, competent_terminal_patients, beneficiary,
    moderate, immediate, identity_locked, national).

% Implement, interpret, and expand the autonomy-based framework. They gain discretionary authority over which patients qualify, what constitutes 'unbearable suffering,' and how to assess decisional capacity. The framework's institutional machinery (physician-assisted mechanisms, assessments, documentation) is theirs to administer and refine. Their power increases as the framework expands because borderline cases require greater interpretive discretion.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, medical_authorities, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__slippery_slope_mechanism, medical_authorities, beneficiary).

% Individuals lacking decisional capacity (advanced dementia, severe brain injury, persistent vegetative state, profound intellectual disability) who enter the victim set as the autonomy framework expands. They cannot articulate their own preferences, yet the framework's logic migrates to permit decisions about them via proxy judgments, substituted judgment, or 'best interests' interpretations. Their powerlessness makes them vulnerable to family pressure, resource constraints, and institutional interpretations of their 'best interests.'
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, incompetent_patients, payer,
    powerless, immediate, trapped, national).

% Patients with decisional capacity but chronic, non-terminal conditions causing persistent psychological or physical suffering (severe treatment-resistant depression, intractable pain syndromes, locked-in syndrome, advanced neurological conditions without imminent death). As the framework expands beyond terminal-only criteria, they become eligible subjects. The framework now interprets their autonomy interest as grounds for life-ending, which redefines 'unbearable suffering' from terminal trajectory to subjective suffering metric. Their constrained exit reflects that most have lived years under these conditions and face high barriers to relocation or alternative care access.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, chronic_suffering_non_terminal_patients, payer,
    moderate, biographical, constrained, national).

% Family members or appointed guardians of incompetent patients gain formal or informal authority to consent to end-of-life interventions. They encounter institutional pressure (financial, emotional, medical) to make decisions framed as 'what the patient would have wanted' or 'best interests.' Once the framework establishes that autonomy interests can be exercised by proxy, guardians become the decision-makers, but they operate under asymmetric information (they do not know what the patient actually prefers and may face institutional or family incentives to choose termination). They also shoulder emotional burden that the medical system transfers to them.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, patient_guardians_under_pressure, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__slippery_slope_mechanism, patient_guardians_under_pressure, beneficiary).

% Communities of people with disabilities and their advocates raise structural concerns about the framework's expansion: that it conflates 'unbearable suffering' with disability itself, that competent disabled people are pressured to exit, and that the framework treats life with disability as a tragedy warranting intervention. They would argue that the expansion mechanism reflects ableism embedded in medical authority's judgment of quality-of-life, not neutral application of autonomy principle. They are structurally excluded from the decision-making processes that determine eligibility expansion and from the physician-patient dyads where the framework operates.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, disability_advocates, excluded,
    organized, generational, constrained, national).

% Ethics committees, regulatory authorities, and academic bioethicists monitor and authorize the framework's implementation. They face the task of maintaining the nominal boundary between terminal and non-terminal, competent and incompetent, while the framework's internal logic pushes expansion. They are analytical seats that can propose remedies (narrower criteria, enhanced safeguards) but have limited enforcement power once the framework is embedded in medical practice.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, bioethics_oversight_bodies, observer,
    institutional, generational, analytical, national).

% The formal legislative authorization (if present) for autonomy-based end-of-life frameworks typically specifies terminal illness, decisional capacity, and explicit consent as eligibility gates. The constraint's expansion mechanism violates this written intent while formally preserving the autonomy rationale. Legislative intent is a non-agent entity — a doctrine rather than an actor — but is mentioned because the framework's expansion occurs by reinterpreting the intent, not by legal amendment.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, legislative_intent, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(end_of_life_authority__slippery_slope_mechanism, legislative_intent).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(end_of_life_authority__slippery_slope_mechanism, medical_authorities).
narrative_ontology:fixing_cost_class(end_of_life_authority__slippery_slope_mechanism, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Substitutes individual autonomous choice (or proxy judgment of individual preference) for institutional or familial imposition of life continuation, solving the problem of imposing death on a person against their expressed or inferred wishes. Initial framing: respects competent terminally ill patients' authority over their own end-of-life narrative.
% TRANSFER_FUNCTION: Moves decision-making authority from institutional gatekeeping (physicians make life-or-death calls unilaterally) to a nominally autonomy-based framework. In practice, transfers discretionary authority to medical professionals who assess suffering, capacity, and best interests; transfers burden to guardians who must make life-or-death proxy judgments; and transfers risk to patients (especially incompetent ones) whose lives become subject to expanded eligibility interpretations.
% ABSENT_VOICES: Disability advocates, incompetent patients themselves (by definition unable to participate), and patients whose suffering does not fit the medical model (existential suffering, isolation, loss of social role) are structurally excluded from the framework's deliberation. Their objection — that the framework conflates disability with pathology warranting termination, and that 'unbearable suffering' is a construct shaped by ableist medical judgment — is not represented in the physician-patient dyad where decisions are made.
% DISAPPEARANCE_RATIONALE: If this constraint and its expansion mechanism vanished, end-of-life decision-making would revert to terminal-only criteria (if legislative intent were restored) or to institutional gatekeeping (if no framework replaced it). Incompetent and non-terminal patients would lose access to life-ending interventions framed as autonomous choice; guardians would lose formal authority to consent to termination; medical authorities would lose the discretion the expansion grants them. The institutional arrangement would reorganize around whichever end-of-life paradigm replaced this one.
% FOUNDING_PROBLEM: Competent terminally ill patients faced the problem of having their wishes for timing and manner of death overridden by institutional or familial gatekeeping. The autonomy-based framework was built to solve this: give individuals (or those authorized to represent them) control over a death they face imminently anyway.
% FOUNDING_PROBLEM_CORROBORATION: Medical authorities and disability-rights jurisdictions attest the founding problem (overriding competent terminal patients' wishes) is substantially solved for the paradigm case. Bioethicists outside beneficiary institutions attest the framework has expanded far beyond the founding problem: that incompetent patients now enter the victim set not because they faced the original gatekeeping problem, but because medical authorities reinterpreted 'autonomy' to permit decisions about them. Legislative testimony and policy analysis from outside the authorizing jurisdictions document the empirical expansion pattern. The founding problem (gatekeeping over competent terminal patients) is functionally dead; the framework persists to address an expanded, institutionally-constructed problem.
narrative_ontology:disappearance_verdict(end_of_life_authority__slippery_slope_mechanism, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_authority__slippery_slope_mechanism, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__slippery_slope_mechanism, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
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
 *   Extractiveness rises over time (0.35 → 0.68) because the expansion mechanism progressively includes populations who cannot assert autonomous choice (incompetent patients, non-terminal suffering cases) and redefines 'autonomy' to permit decisions about them via proxy or best-interests judgments. At t=0, the framework is minimally extractive: competent terminal patients control decisions, medical gatekeeping is replaced by autonomy-based authority. By t=40, the framework has expanded to populations who cannot exercise autonomy, so the extraction is the institutional and guardian authority to decide life-or-death for people without their explicit consent. Suppression requirement rises in parallel (0.38 → 0.72) because maintaining the fiction that the expanded framework is autonomy-based requires active suppression of disability-advocate objections, critical bioethics voices, and alternative interpretations of suffering. Theater rises (0.12 → 0.41) as the gap widens between the nominal justification (autonomy) and the actual operation (institutional discretion over powerless populations): the framework must increasingly perform autonomy-protection while actually expanding institutional authority. Accessibility collapse is moderate (0.63) because the framework is formalized in law and medical guidelines, but alternative framings (disability-rights critiques, sanctity-based objections) remain available even if institutionally suppressed. Resistance remains moderate-high (0.58) because disability-rights movements and conscience-based medical objectors maintain vocal opposition despite their structural exclusion from decision-making.
 *
 * PERSPECTIVAL GAP:
 *   The medical-authority seat should compute as a beneficiary-side rope or even a beneficiary-biased tangled rope: they gain interpretive discretion, institutional authority, and a framework that delegates difficult decisions to families while preserving medical authority over eligibility. The powerless-incompetent-patient seat should compute as a victim-side tangled rope or snare: they lose autonomy protections because the framework redefines 'autonomy' to exclude their own voice, and suppression is structural (they cannot communicate their preferences) plus internalized (their 'best interests' are defined by others). The guardian seat should compute as near-symmetric but with drift toward payer: they gain formal authority but bear emotional and ethical burden that institutional gatekeeping once held. The disability-advocate seat should compute as a victim-side snare: they are completely excluded from decision-making, and the framework's expansion directly harms the population they advocate for.
 *
 * DIRECTIONALITY LOGIC:
 *   Medical authorities derive d near 0.0 (full beneficiary): they set the agenda, interpret the criteria, and gain discretionary authority as the framework expands. Competent terminal patients derive d near 0.3 (mostly beneficiary): they were the founding beneficiaries and retain nominal paradigm status, but the expansion's benefit accrues primarily to authorities. Incompetent patients derive d near 0.95 (nearly full target): they cannot consent, cannot resist, and their 'autonomy interests' are determined by others. Chronic-suffering non-terminal patients derive d near 0.75 (high target): they have nominal autonomy (they are competent) but face institutional pressure once the framework expands to include suffering (not terminal imminence) as criterion. Guardians derive d near 0.65 (moderate-high target): they gain authority but are burdened with responsibility and face institutional pressure. Disability advocates derive d near 1.0 (full target in a functional sense): they are excluded from the framework's deliberation, and its expansion directly harms the population they represent. The engine's computation should show marked divergence between the beneficiary seat (medical authorities, d~0.0 → low extractiveness for them) and the target seats (incompetent patients d~0.95, disability advocates structurally excluded).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint risks mandatrophy: the founding problem (overriding competent terminal patients' wishes) becomes increasingly absent as the framework expands to populations (incompetent patients) who never posed the original problem. By t=40, the constraint persists not because it solves its founding problem but because the institutional machinery (assessment protocols, guardian authority, medical discretion) has become self-perpetuating. The theater-ratio rise (0.12 → 0.41) signals performative maintenance: the framework continues to invoke autonomy language even as actual decision-making power concentrates in medical and guardian authorities. The founding-problem status is 'dead' (competent terminal patients' gatekeeping problem is solved; future expansion addresses a manufactured problem of 'unbearable suffering' as medical authorities define it), yet the disappearance verdict is 'world rearranges' (significant institutional infrastructure now depends on the framework). This mismatch flags mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_reinterpretation_mechanism,
    'Is the expansion from competent-terminal-explicit to incompetent-non-terminal-proxy an inevitable internal consequence of the autonomy rationale, or a policy choice that could have been constrained?',
    'Comparative policy analysis: jurisdictions that adopted autonomy frameworks with explicit legal constraints (terminal-only, competence-required) versus those with permissive reinterpretation. Document which constraints actually prevented expansion and what enforcement mechanisms held them in place.',
    'If expansion is inevitable (consequence of rationale), the autonomy reading itself structurally produces the slippery slope; if preventable (policy choice), the expansion reflects institutional choice to benefit medical authorities and guardians at the cost of incompetent patients. The constraint type and remedial options diverge sharply on this distinction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_reinterpretation_mechanism, empirical, 'Whether autonomy-framework expansion is rationale-driven or discretionary.').

omega_variable(
    incompetent_patient_interests_representation,
    'When the framework expands to include incompetent patients, who actually represents their interests in the decision-making process — and whose interests does that representation actually serve?',
    'Qualitative research on guardian decision-making: interview guardians about the reasoning process, institutional pressures faced, and information available. Compare their stated reasoning to post-mortem assessments of whether the decision aligned with the patient''s previously expressed values or ''best interests'' as external observers define them.',
    'If guardians genuinely represent incompetent patients'' interests, the framework remains a coordination mechanism (albeit asymmetric and burdened). If guardians'' decisions systematically diverge from the incompetent patient''s interests and align with institutional or family benefits, the framework is extractive (a snare, not a tangled rope) and the ''autonomy'' rationale is a cover story.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(incompetent_patient_interests_representation, empirical, 'Whether proxy decision-making represents incompetent patients'' actual interests or serves other parties'' interests.').

omega_variable(
    suffering_definition_construction,
    'What criteria define ''unbearable suffering'' such that it justifies life-ending intervention? Is this definition medical-objective (measurable, anchored in physiology), patient-subjective (determined by the individual''s experienced suffering), or institutional-constructed (determined by medical, legal, or social judgment)?',
    'Doctrinal analysis of actual cases and their justifications. Identify the metrics used to assess suffering; compare cases where identical conditions were judged ''unbearable'' in one jurisdiction and ''manageable'' in another.',
    'If suffering is constructed (varies by jurisdiction and medical judgment), the expansion mechanism is not neutral application of autonomy principle but institutional imposition of a suffering threshold. This relocates the constraint from tangled rope (mixed coordination/extraction) to snare (pure institutional extraction disguised as autonomy). It also relocates incompetent patients from victim-set to captured-population.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suffering_definition_construction, empirical, 'Whether ''unbearable suffering'' is objective-medical or institutional-constructed.').

omega_variable(
    disability_conflation_mechanism,
    'Does the framework''s expanded use treat ''unbearable suffering'' as arising from disability itself, or only from suffering superadded to disability? Are disabled people who refuse life-ending interventions systematized as irrational, in denial, or failing to understand their own interests?',
    'Policy analysis of guidelines and case law: extract the language used to describe disabled patients'' refusal of life-ending options. Conduct cohort comparison: do disabled patients decline life-ending interventions at higher rates than non-disabled patients with comparable medical conditions? If yes, interview disabled patients about the institutional pressures and framings they encountered.',
    'If the framework conflates disability with suffering-warranting-termination, and systematizes disabled refusal as irrational, the framework is not autonomy-respecting (it overrides disabled autonomy) but ableist extraction disguised as respecting choice. This would support disability-advocate claims and indicate the framework''s expansion is extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disability_conflation_mechanism, empirical, 'Whether the framework conflates disability with pathology-warranting-termination.').

omega_variable(
    autonomy_versus_sanctity_kernel_contestation,
    'Which kernel reading — autonomy or sanctity — better explains the actual operation of end-of-life frameworks as they have expanded?',
    'The slippery_slope_mechanism reading claims that autonomy-framework expansion reveals the mechanism by which medical authority substitutes for individual choice, and that this mechanism works by reinterpreting ''autonomy'' to include proxy judgment and best-interests decisions. The sanctity reading would claim that the framework''s expansion reflects a return to sanctity concerns (treating life as sacred) disguised in autonomy language. Examine whether the expansion primarily (a) narrows the range of individual choice (sanctity effect), or (b) expands institutional discretion framed as respecting expanded forms of autonomy.',
    'If the mechanism is sanctity-like (institutional gatekeeping under autonomy cover), the framework is a snare using autonomy language; the autonomy reading is overridden (or rather, revealed as never truly implemented). If the mechanism is genuine autonomy expansion (proxy decision-making that genuinely respects incompetent patients'' interests), the framework remains a coordination mechanism; the sanctity reading is foreclosed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(autonomy_versus_sanctity_kernel_contestation, conceptual, 'Whether expansion reveals autonomy mechanism or sanctity substitution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__slippery_slope_mechanism, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 0, 0.12).
narrative_ontology:measurement(end__tr_t5, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 5, 0.16).
narrative_ontology:measurement(end__tr_t10, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 10, 0.21).
narrative_ontology:measurement(end__tr_t15, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 15, 0.27).
narrative_ontology:measurement(end__tr_t20, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 20, 0.33).
narrative_ontology:measurement(end__tr_t25, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 25, 0.37).
narrative_ontology:measurement(end__tr_t30, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 30, 0.39).
narrative_ontology:measurement(end__tr_t40, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(end__be_t5, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(end__be_t10, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(end__be_t15, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(end__be_t20, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(end__be_t25, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 25, 0.65).
narrative_ontology:measurement(end__be_t30, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 30, 0.67).
narrative_ontology:measurement(end__be_t40, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(end__su_t5, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 5, 0.46).
narrative_ontology:measurement(end__su_t10, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(end__su_t15, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 15, 0.59).
narrative_ontology:measurement(end__su_t20, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 20, 0.64).
narrative_ontology:measurement(end__su_t25, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 25, 0.68).
narrative_ontology:measurement(end__su_t30, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(end__su_t40, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__slippery_slope_mechanism, resource_allocation).
narrative_ontology:boltzmann_floor_override(end_of_life_authority__slippery_slope_mechanism, 0.18).
narrative_ontology:affects_constraint(end_of_life_authority__slippery_slope_mechanism, end_of_life_authority__autonomy_reading).
narrative_ontology:affects_constraint(end_of_life_authority__slippery_slope_mechanism, end_of_life_authority__sanctity_reading).

% DUAL FORMULATION NOTE:
% The end_of_life_authority kernel decomposes into three structurally distinct constraint readings: (1) autonomy_reading, which grounds end-of-life authority in individual autonomous choice and makes competent terminal patients the paradigm case; (2) sanctity_reading, which grounds authority in intrinsic human worth and prohibits intentional life-ending regardless of individual preference; (3) slippery_slope_mechanism, which models the empirical expansion of autonomy frameworks to incompetent and non-terminal populations. These are not three positions on a single axis but three different structurings of the same institutional domain. The slippery_slope_mechanism reading's distinctive claim is that autonomy-based frameworks, once implemented, necessarily expand beyond competent-terminal-explicit to incompetent-proxy-best-interests due to the internal reinterpretability of 'autonomy.' This expansion is the constraint's primary operation; the reading documents it as a structural feature of applying autonomy rationale, not as a policy deviation. The autonomy and sanctity readings offer alternative foundational groundings for end-of-life authority that would prevent or have prevented the expansion this reading documents.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
