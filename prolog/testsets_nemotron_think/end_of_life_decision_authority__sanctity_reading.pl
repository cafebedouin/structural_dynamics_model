% ============================================================================
% CONSTRAINT STORY: end_of_life_decision_authority__sanctity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_decision_authority__sanctity_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: end_of_life_decision_authority__sanctity_reading
 *   human_readable: Sanctity-of-Life Absolute Prohibition on Intentional Life-Ending
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   The sanctity reading of end-of-life decision authority asserts that human
 *   life possesses intrinsic value independent of individual will, making
 *   intentional life-ending a violation of natural law. This reading claims
 *   Mountain status — a fixed, natural limit. However, identifiable
 *   beneficiaries (religious institutions, pro-life organizations,
 *   traditional medical establishment) derive moral authority, political
 *   capital, and professional coherence from the prohibition, while
 *   identifiable victims (pressured-vulnerable populations, suffering
 *   individuals denied autonomy) bear the extraction of forced endurance. The
 *   constraint requires active enforcement (criminal law, medical licensing,
 *   professional sanctions) and its persistence depends on suppressing the
 *   autonomy_reading and vulnerability_protection_reading alternatives. The
 *   authored metrics describe a constraint that operates as extractive and
 *   enforced, while the claimed_type asserts natural law — the engine
 *   measures this divergence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_decision_authority__sanctity_reading, 0.68).
domain_priors:suppression_score(end_of_life_decision_authority__sanctity_reading, 0.82).
domain_priors:theater_ratio(end_of_life_decision_authority__sanctity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_decision_authority__sanctity_reading, mountain).
narrative_ontology:human_readable(end_of_life_decision_authority__sanctity_reading, "Sanctity-of-Life Absolute Prohibition on Intentional Life-Ending").
narrative_ontology:topic_domain(end_of_life_decision_authority__sanctity_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_decision_authority__sanctity_reading).
domain_priors:emerges_naturally(end_of_life_decision_authority__sanctity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_decision_authority__sanctity_reading, '5346c113-1f96-43cb-b00b-08752e8ca16c').
narrative_ontology:cs_kernel_codification('5346c113-1f96-43cb-b00b-08752e8ca16c', formalized).
narrative_ontology:cs_authority_grounding('5346c113-1f96-43cb-b00b-08752e8ca16c', lineage).
narrative_ontology:cs_interpretation_layer_present('5346c113-1f96-43cb-b00b-08752e8ca16c').
narrative_ontology:cs_reading_relation('5346c113-1f96-43cb-b00b-08752e8ca16c', end_of_life_decision_authority__autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('5346c113-1f96-43cb-b00b-08752e8ca16c', end_of_life_decision_authority__vulnerability_protection_reading, forecloses).
narrative_ontology:cs_axiom('5346c113-1f96-43cb-b00b-08752e8ca16c', foundational, human_life_intrinsic_value_independent_of_will).
narrative_ontology:cs_axiom_status(human_life_intrinsic_value_independent_of_will, holdable).
narrative_ontology:cs_axiom_grounding('5346c113-1f96-43cb-b00b-08752e8ca16c', human_life_intrinsic_value_independent_of_will, deontological).
narrative_ontology:cs_axiom('5346c113-1f96-43cb-b00b-08752e8ca16c', foundational, intentional_life_ending_always_violates_intrinsic_value).
narrative_ontology:cs_axiom_status(intentional_life_ending_always_violates_intrinsic_value, holdable).
narrative_ontology:cs_axiom_grounding('5346c113-1f96-43cb-b00b-08752e8ca16c', intentional_life_ending_always_violates_intrinsic_value, deontological).
narrative_ontology:cs_reference_frame('5346c113-1f96-43cb-b00b-08752e8ca16c', sanctity_of_life_absolute_prohibition).
narrative_ontology:cs_drift_state('5346c113-1f96-43cb-b00b-08752e8ca16c', contemporary_bioethics_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5346c113-1f96-43cb-b00b-08752e8ca16c', '').
narrative_ontology:cs_kernel_id(end_of_life_decision_authority__sanctity_reading, end_of_life_decision_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, religious_institutions).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, pro_life_advocacy_organizations).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, traditional_medical_establishment).
narrative_ontology:constraint_victim(end_of_life_decision_authority__sanctity_reading, pressured_vulnerable_populations).
narrative_ontology:constraint_victim(end_of_life_decision_authority__sanctity_reading, suffering_individuals_denied_autonomy).
narrative_ontology:constraint_vindicates(end_of_life_decision_authority__sanctity_reading, human_life_intrinsic_value_independent_of_will).
narrative_ontology:constraint_vindicates(end_of_life_decision_authority__sanctity_reading, intentional_killing_always_morally_prohibited).
narrative_ontology:constraint_vindicates(end_of_life_decision_authority__sanctity_reading, slippery_slope_from_voluntary_to_involuntary_euthanasia).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain doctrinal authority over life-and-death boundaries; the absolute prohibition reinforces institutional teaching authority and moral leadership. Operate across jurisdictions with varying legal regimes, adapting strategy while holding the absolute line. Collect moral authority and adherent loyalty from maintaining the prohibition.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, religious_institutions, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(end_of_life_decision_authority__sanctity_reading, religious_institutions, beneficiary).

% Build organizational identity, funding, and political capital around defending the absolute prohibition. Mobilize voters, litigate, and lobby against any liberalization. Their existence and relevance depend on the constraint remaining contested; total victory would dissolve their purpose, total defeat would marginalize them.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, pro_life_advocacy_organizations, beneficiary,
    organized, generational, mobile, national).

% Uphold the healer-only role as the core of professional identity; the prohibition protects physicians from being asked to kill, preserving trust and moral clarity. Professional licensing bodies enforce the constraint through sanctions. Individual physicians who dissent face career risk; the establishment benefits from a unified professional norm.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, traditional_medical_establishment, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_decision_authority__sanctity_reading, traditional_medical_establishment, beneficiary).

% Elderly, disabled, poor, and chronically ill people who experience subtle and overt pressure to continue living when they would choose death if permitted. The constraint removes the legal option of assisted death, forcing them to endure suffering or pursue dangerous extra-legal means. They cannot exit the constraint of being alive; their vulnerability is amplified by the prohibition's removal of a regulated, supported pathway.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, pressured_vulnerable_populations, payer,
    powerless, biographical, trapped, national).

% Competent individuals with terminal or intractable suffering who experience the prohibition as a denial of their sovereign authority over their own death. Their self-concept is fused with the conviction that they — not the state, church, or profession — own the decision. The constraint externalizes their suffering as morally irrelevant; they are trapped in a body and legal regime they reject, with identity-locked exit (death is the only exit, and the constraint blocks the supported path to it).
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, suffering_individuals_denied_autonomy, payer,
    powerless, immediate, identity_locked, local).

% Hold a complex position: many support the prohibition as protection against coercion and devaluation of disabled lives, but some critique it as paternalistic denial of autonomy. They are not beneficiaries of the religious framing but align on the vulnerability protection argument. Their testimony is sought by legislatures; they occupy a structural observer seat with moral authority but no direct extraction.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, disability_rights_organizations, observer,
    organized, generational, constrained, national).

% Adjudicate hard cases, draft guidelines, and legitimate policy compromises. They interpret the constraint in practice, creating the 'interpretation layer' that absorbs pressure for liberalization without changing the absolute rule. Their authority derives from being the designated interpreters; they benefit professionally from the constraint's complexity.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, bioethics_committees, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_decision_authority__sanctity_reading, bioethics_committees, agenda_setter).

% Argue for sovereign individual authority over death; their framing is structurally excluded from the sanctity reading's foundational premise. They would object that the constraint imposes a specific metaphysical view on pluralistic societies. They operate in legislatures, courts, and public discourse but cannot gain traction within the sanctity framework itself.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, autonomy_advocates, excluded,
    organized, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a societal consensus that human life is inviolable, preventing a slide from voluntary to involuntary killing and protecting the vulnerable from being treated as burdens. Provides a bright-line rule for medical professionals: heal, never kill.
% TRANSFER_FUNCTION: Transfers decision authority over life-ending from the individual to the collective (state, profession, religious tradition). Transfers the burden of suffering from the collective (which would bear the cost of supported dying) to the individual who must endure it. Transfers moral authority to institutions that define and enforce the boundary.
% ABSENT_VOICES: The pressured-vulnerable who would choose assisted death if it were legal and supported are structurally silenced — the constraint defines their protection as the removal of the option they would choose. Dying individuals who experience the prohibition as violence cannot testify after death; their pre-death testimony is dismissed as 'depression' or 'coercion' within the sanctity framework. Jurisdictions that have legalized assisted dying (Oregon, Netherlands, Canada) demonstrate that the absent voices exist and choose differently when permitted.
% DISAPPEARANCE_RATIONALE: If the absolute prohibition vanished overnight, multiple jurisdictions would legalize assisted dying within months (following existing models). Medical practice would reorganize around new protocols. Religious institutions would lose a central doctrinal enforcement point. The pressured-vulnerable would gain a legal option but face new coercion risks — the vulnerability_protection_reading predicts this rearrangement. The world does not stay the same; the constraint actively structures the legal and medical landscape.
% FOUNDING_PROBLEM: Preventing the slide from voluntary euthanasia to involuntary killing of the disabled, elderly, and socially devalued — the historical memory of eugenics and Nazi 'euthanasia' programs. Establishing a bright-line rule that protects physicians from complicity in killing and preserves medicine's healing identity.
% FOUNDING_PROBLEM_CORROBORATION: Disability rights organizations (Not Dead Yet, Disability Rights Education & Defense Fund) corroborate the vulnerability concern from outside the religious beneficiary set, citing Oregon and Netherlands data on pressure and expansion. Bioethicists outside religious institutions (e.g., Helga Kuhse, Peter Singer) contest the slippery slope, citing empirical evidence from regulated jurisdictions. The founding problem is live for some, dead for others — the contestation is structural.
narrative_ontology:disappearance_verdict(end_of_life_decision_authority__sanctity_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_decision_authority__sanctity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_decision_authority__sanctity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(end_of_life_decision_authority__sanctity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_decision_authority__sanctity_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_decision_authority__sanctity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_decision_authority__sanctity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, ExtMetricName, E),
    domain_priors:suppression_score(end_of_life_decision_authority__sanctity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(end_of_life_decision_authority__sanctity_reading),
    narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(end_of_life_decision_authority__sanctity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68) reflects the substantial burden transferred to suffering individuals and pressured-vulnerable populations who cannot access a supported death. Suppression (0.82) is high because the constraint's persistence depends on criminal prohibition, professional sanctions, and the exclusion of alternative frameworks from legal recognition. Theater ratio (0.42) has risen over the interval as the 'protection of the vulnerable' rhetoric increasingly covers the maintenance of institutional authority — the coordination function (bright line for physicians) is real but a declining share of enforcement activity. Accessibility collapse (0.78) is high for the Mountain claim but lower than a genuine natural law because regulated alternatives exist in other jurisdictions and function. Resistance (0.71) is high and rising as autonomy movements gain legal traction.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seats (religious institutions, medical establishment), the constraint is genuine coordination: a bright line that protects the healing profession and prevents abuse. From the payer seats (pressured-vulnerable, suffering individuals), the same structure operates as enforced extraction: the bright line is a wall that traps them in suffering. The engine computes this divergence from the structural data — the authored claim (mountain) does not adjudicate it. The False Summit Mountain signature will evaluate whether the natural-law claim survives beneficiary presence.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious institutions and traditional medical establishment are structural beneficiaries (d near 0.0-0.2): they collect moral authority, professional coherence, and institutional relevance from the constraint. Pro-life organizations are beneficiaries with mobile exit (d ~0.2). Pressured-vulnerable populations are full targets (d ~0.9-1.0): trapped, powerless, bearing the extraction of forced endurance. Suffering individuals denied autonomy are identity-locked targets (d ~0.85): their self-concept fuses with the conviction of sovereign authority, making the constraint a violation of identity, not just preference. Disability rights organizations and bioethics committees sit near symmetric (d ~0.4-0.5) as observers who shape interpretation. Autonomy advocates are excluded (d not computed) — their framing is structurally incompatible with the sanctity premise.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing slide to involuntary killing) is contested, not dead. The sanctity reading's absolute prohibition persists partly because the vulnerability_protection_reading's regulated alternative is contested — disability rights organizations split on whether safeguards work. The constraint has not atrophied into pure performance (piton) because active enforcement remains high and the coordination function (physician role protection) remains live. But the rising theater ratio and the existence of functioning regulated alternatives in other jurisdictions suggest the constraint is in a mandatrophy transition zone: the original justification is contested, the extraction is measurable, and the enforcement is intensifying to maintain the line.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_prohibition,
    'Is the sanctity-of-life absolute prohibition a genuine natural law (Mountain) or a constructed constraint that benefits identifiable institutions (False Summit Mountain)?',
    'Cross-jurisdictional comparison: if jurisdictions that have removed the prohibition (Netherlands, Belgium, Canada, Oregon, etc.) show no collapse of medical trust or slide to involuntary killing, the natural-law claim is falsified. If such jurisdictions show measurable harm to vulnerable populations, the protection claim gains empirical support.',
    'If natural law, the constraint is a genuine Mountain with zero extraction from any seat. If constructed, it is a False Summit Mountain (reclassified as Tangled Rope via FSM signature) with substantial extraction from pressured-vulnerable and suffering individuals, benefiting religious and medical institutions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_prohibition, empirical, 'Whether the Mountain claim survives empirical test of jurisdictions that have liberalized.').

omega_variable(
    committer_structure_victim_set_delta,
    'How does this reading''s victim set (pressured-vulnerable when euthanasia available, suffering individuals denied autonomy) structurally differ from the victim sets of sibling readings?',
    'Structural comparison of victim declarations across the three kernel readings: sanctity_reading victims = pressured_vulnerable + suffering_denied_autonomy; autonomy_reading victims = those denied sovereign choice (suffering individuals under prohibition); vulnerability_protection_reading victims = those coerced OR denied under either extreme. The delta is the reading-specific construction of who is harmed by what.',
    'Clarifies that ''victim'' is reading-indexed, not kernel-indexed. The same individual (e.g., a disabled person pressured to die) is a victim in the sanctity reading (of legalized euthanasia) but a victim in the autonomy reading (of prohibition). The kernel does not have a single victim set; each reading constructs its own.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_victim_set_delta, conceptual, 'Reading-indexed victim construction across the end_of_life_decision_authority kernel.').

omega_variable(
    suppression_mechanism_ambiguity_vulnerable,
    'For pressured-vulnerable populations, is the measured suppression structural (legal prohibition, lack of access) or internalized (believing they are burdens, having fused identity with the ''life is sacred'' narrative)?',
    'Post-legalization suppression trajectory: in jurisdictions that legalize assisted dying, does the pressure on vulnerable populations persist, diminish, or transform? If suppression persists internally after structural removal, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the vulnerable carry the suppression with them even after legal exit becomes available. This would increase effective extraction for the powerless seat beyond the base ε × directionality calculation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity_vulnerable, empirical, 'Structural vs. internalized suppression for pressured-vulnerable populations under the sanctity prohibition.').

omega_variable(
    healer_only_role_as_extraction_cover,
    'Does the ''physician as healer-only'' role genuinely coordinate trust, or does it function as a cover story for professional control over the life-death boundary that extracts status and authority?',
    'Compare physician attitudes and public trust in jurisdictions with and without assisted dying. If trust holds and physicians participate willingly in regulated assisted dying, the healer-only claim is a cover. If trust collapses or physicians refuse participation en masse, the coordination function is genuine.',
    'If cover story, the traditional_medical_establishment is a concentrated beneficiary extracting professional authority, making the constraint a Snare or Tangled Rope rather than a Mountain. If genuine coordination, the beneficiary claim is weaker and the constraint may be a Scaffold (transitional) or genuine Mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(healer_only_role_as_extraction_cover, empirical, 'Whether the professional identity claim is coordination or extraction cover.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_decision_authority__sanctity_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eol_sanctity_tr_t0, end_of_life_decision_authority__sanctity_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(eol_sanctity_tr_t10, end_of_life_decision_authority__sanctity_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement(eol_sanctity_tr_t20, end_of_life_decision_authority__sanctity_reading, theater_ratio, 20, 0.31).
narrative_ontology:measurement(eol_sanctity_tr_t30, end_of_life_decision_authority__sanctity_reading, theater_ratio, 30, 0.37).
narrative_ontology:measurement(eol_sanctity_tr_t40, end_of_life_decision_authority__sanctity_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement(eol_sanctity_tr_t50, end_of_life_decision_authority__sanctity_reading, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(eol_sanctity_be_t0, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(eol_sanctity_be_t10, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(eol_sanctity_be_t20, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(eol_sanctity_be_t30, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(eol_sanctity_be_t40, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 40, 0.66).
narrative_ontology:measurement(eol_sanctity_be_t50, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(eol_sanctity_su_t0, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(eol_sanctity_su_t10, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 10, 0.78).
narrative_ontology:measurement(eol_sanctity_su_t20, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 20, 0.8).
narrative_ontology:measurement(eol_sanctity_su_t30, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 30, 0.81).
narrative_ontology:measurement(eol_sanctity_su_t40, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 40, 0.82).
narrative_ontology:measurement(eol_sanctity_su_t50, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 50, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_decision_authority__sanctity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(end_of_life_decision_authority__sanctity_reading, 0.08).
narrative_ontology:affects_constraint(end_of_life_decision_authority__sanctity_reading, end_of_life_decision_authority__autonomy_reading).
narrative_ontology:affects_constraint(end_of_life_decision_authority__sanctity_reading, end_of_life_decision_authority__vulnerability_protection_reading).
narrative_ontology:affects_constraint(end_of_life_decision_authority__sanctity_reading, medical_professional_autonomy_boundary).
narrative_ontology:affects_constraint(end_of_life_decision_authority__sanctity_reading, disability_rights_protection_framework).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the end_of_life_decision_authority kernel. The autonomy_reading and vulnerability_protection_reading are sibling constraints with different ε, different beneficiary/victim structures, and different claimed types. The three stories form a constraint family linked by kernel_id. This reading claims Mountain; the autonomy_reading likely claims Snare (for the prohibition) or Rope (for the regulated alternative); the vulnerability_protection_reading likely claims Scaffold or Tangled Rope. Their ε values differ substantially because they assess different standing arrangements under contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(end_of_life_decision_authority__sanctity_reading, institutional, 0.15).
constraint_indexing:directionality_override(end_of_life_decision_authority__sanctity_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
