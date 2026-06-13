% ============================================================================
% CONSTRAINT STORY: dignified_death__autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dignified_death__autonomy_primary, []).

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
 *   constraint_id: dignified_death__autonomy_primary
 *   human_readable: Autonomy-Primary Dignified Death Constraint
 *   domain: bioethics/medical_law/political_philosophy
 *
 * SUMMARY:
 *   The autonomy-primary reading of dignified death asserts that dignity
 *   resides in self-determination: a suffering individual with capacity
 *   possesses final moral authority over the timing and method of their own
 *   death. This reading is one of three structurally distinct constraints
 *   within the contested dignified_death kernel. The autonomy-primary reading
 *   claims that state prohibition and medical gatekeeping extract from those
 *   they deny exit to (prolonged suffering against stated will), while
 *   vindicating the autonomy norm. The constraint is a tangled rope: it
 *   coordinates around respect for individual choice (genuine coordination
 *   function) while simultaneously enforcing medical gatekeeping and
 *   state-legitimated eligibility criteria that asymmetrically extract from
 *   the powerless (those whose autonomy is disputed or whose suffering
 *   continues unrelieved). The claim/metric divergence is deliberate—the
 *   reading is CLAIMED as rope (autonomy-respecting, non-extractive) by its
 *   proponents, but the authored metrics describe extractive operation due to
 *   gatekeeping enforcement, disputed capacity assessment, and differential
 *   access by social position.
 *
 * KEY AGENTS:
 *   - suffering_individual_denied_exit: Powerless, trapped, immediate horizon—bears extraction of continued suffering against stated will
 *   - autonomous_agent: Moderate power, mobile exit—theoretical beneficiary of autonomy norm; actual benefit contingent on gatekeeping approval
 *   - medical_gatekeeper: Institutional power, constrained exit—administers and enforces eligibility criteria; bears liability; simultaneously empowered and constrained
 *   - sanctity_advocate: Organized, constrained exit—excluded from decision authority, contests the constraint's foundational premise
 *   - vulnerable_population: Powerless, identity-locked exit—structurally excluded from capacity assessment discourse; bears extraction risk from either permissiveness or restriction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignified_death__autonomy_primary, 0.52).
domain_priors:suppression_score(dignified_death__autonomy_primary, 0.68).
domain_priors:theater_ratio(dignified_death__autonomy_primary, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, extractiveness, 0.52).
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__autonomy_primary, tangled_rope).
narrative_ontology:human_readable(dignified_death__autonomy_primary, "Autonomy-Primary Dignified Death Constraint").
narrative_ontology:topic_domain(dignified_death__autonomy_primary, "bioethics/medical_law/political_philosophy").

domain_priors:requires_active_enforcement(dignified_death__autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__autonomy_primary, '41d50c3e-105e-4a40-97d4-b96c849c0bbb').
narrative_ontology:cs_kernel_codification('41d50c3e-105e-4a40-97d4-b96c849c0bbb', formalized).
narrative_ontology:cs_authority_grounding('41d50c3e-105e-4a40-97d4-b96c849c0bbb', lineage).
narrative_ontology:cs_interpretation_layer_present('41d50c3e-105e-4a40-97d4-b96c849c0bbb').
narrative_ontology:cs_reading_relation('41d50c3e-105e-4a40-97d4-b96c849c0bbb', dignified_death__sanctity_primary, coexists_with).
narrative_ontology:cs_reading_relation('41d50c3e-105e-4a40-97d4-b96c849c0bbb', dignified_death__relational_autonomy, influences).
narrative_ontology:cs_axiom('41d50c3e-105e-4a40-97d4-b96c849c0bbb', foundational, self_determination_constitutes_dignity).
narrative_ontology:cs_axiom_status(self_determination_constitutes_dignity, holdable).
narrative_ontology:cs_axiom_grounding('41d50c3e-105e-4a40-97d4-b96c849c0bbb', self_determination_constitutes_dignity, deontological).
narrative_ontology:cs_axiom('41d50c3e-105e-4a40-97d4-b96c849c0bbb', foundational, individual_holds_final_authority_over_own_death).
narrative_ontology:cs_axiom_status(individual_holds_final_authority_over_own_death, holdable).
narrative_ontology:cs_axiom_grounding('41d50c3e-105e-4a40-97d4-b96c849c0bbb', individual_holds_final_authority_over_own_death, deontological).
narrative_ontology:cs_reference_frame('41d50c3e-105e-4a40-97d4-b96c849c0bbb', individual_autonomy_as_dignity).
narrative_ontology:cs_drift_state('41d50c3e-105e-4a40-97d4-b96c849c0bbb', contemporary_sanctity_pressure_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('41d50c3e-105e-4a40-97d4-b96c849c0bbb', '').
narrative_ontology:cs_kernel_id(dignified_death__autonomy_primary, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__autonomy_primary, autonomous_agent).
narrative_ontology:constraint_victim(dignified_death__autonomy_primary, suffering_individual_denied_exit).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dignified_death__autonomy_primary, medical_gatekeeper).
narrative_ontology:constraint_victim(dignified_death__autonomy_primary, sanctity_advocate).
narrative_ontology:constraint_vindicates(dignified_death__autonomy_primary, self_determination_as_dignity).
narrative_ontology:constraint_vindicates(dignified_death__autonomy_primary, individual_bodily_autonomy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Person experiencing terminal suffering (unrelenting pain, loss of function, or existential degradation) who seeks to end their life on their own timeline and terms. The constraint denies them access to means and medical cooperation unless specific, bureaucratically-verified eligibility criteria are met. They experience the constraint as coercive postponement of death against their will—prolonged suffering as the price of not meeting gatekeeping standards.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, suffering_individual_denied_exit, payer,
    powerless, immediate, trapped, local).

% Person framed as the rights-holder under autonomy-primary dignity: they possess the moral authority to make final decisions about their own death. The constraint, when it functions, vindicates their self-determination capacity. They are the theoretical beneficiary of the autonomy norm, though whether actual benefit accrues depends on whether they meet eligibility thresholds or navigate medical gatekeeping successfully.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, autonomous_agent, beneficiary,
    moderate, biographical, mobile, national).

% Physicians and medical institutions that determine who qualifies for assisted death and oversee its administration. They administer eligibility criteria (diagnosis confirmation, capacity assessment, waiting periods, repeat consultation), implement the constraint, and bear legal liability for error. Their position is asymmetric: they execute the autonomy norm but also filter it—they are simultaneously constrained by the requirement to honor autonomy and empowered to adjudicate who counts as autonomous.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, medical_gatekeeper, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(dignified_death__autonomy_primary, medical_gatekeeper, payer).

% Groups (religious, philosophical, medical) that hold life's intrinsic moral value as inviolable regardless of suffering or consent. They experience the autonomy-primary constraint as a violation they must live in the presence of—they are excluded from decision authority (the suffering individual holds it) and they bear the cost of witnessing sanctity violations they cannot prevent. They contest the constraint's foundational premise.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, sanctity_advocate, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(dignified_death__autonomy_primary, sanctity_advocate, excluded).

% State and medical licensing bodies that set eligibility criteria, oversight procedures, and liability rules for the constraint's administration. They formalize the gatekeeping function and bear political accountability for both autonomy protection and prevention of coercion/abuse. Their position is deliberately neutral (by design) but structurally constrains what autonomy can mean in practice.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, regulatory_authority, observer,
    institutional, generational, analytical, national).

% Persons with cognitive disabilities, severe depression, or social isolation who might seek death but whose capacity to exercise autonomy is contested or whose choice is shaped by vulnerability to coercion. They are structurally excluded from the conversation about how autonomy is assessed and verified—gatekeepers decide whether they count as autonomous agents. They bear the extraction cost if gatekeeping is either too permissive (enabling death rooted in treatable depression) or too restrictive (imposing prolonged suffering).
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, vulnerable_population, excluded,
    powerless, immediate, identity_locked, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dignified_death__autonomy_primary, medical_gatekeeper).
narrative_ontology:fixing_cost_class(dignified_death__autonomy_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a procedural framework recognizing the suffering individual's final authority over their own death, implemented through medical assessment and administration. Solves the coordination problem of how to honor self-determination while managing risk of coercion, misdiagnosis, or impulsive choice—the problem is: how can a pluralist society respect radical autonomy while preventing harm?
% TRANSFER_FUNCTION: Transfers final decision authority from medical paternalism (physicians choose based on medical judgment) to individual autonomy (the person chooses their own endpoint). Also transfers execution risk from the individual (who cannot reliably self-administer) to medical institutions (who become liable for both administering and certifying eligibility).
% ABSENT_VOICES: Vulnerable populations (cognitively disabled, depressed, socially isolated) are structurally excluded from the conversation about capacity assessment standards. They would argue the criteria are applied in ways that either deny them autonomy they possess or grant it when they are being coerced by circumstance. Also absent: future selves of persons who might later regret the decision—they cannot testify to the gatekeeping process that eliminates them.
% DISAPPEARANCE_RATIONALE: If this autonomy-primary constraint vanished, the default would revert to medical paternalism and state prohibition: physicians would decide based on medical judgment, individuals would lose formal authority over their own death, and suffering would be prolonged against stated will as state/medical default. The world reorganizes because the locus of final authority shifts fundamentally.
% FOUNDING_PROBLEM: Terminal suffering and the loss of bodily autonomy at end-of-life leave persons unable to determine the timing and manner of their own death. Medical institutions default to prolonging life regardless of the individual's stated wishes. The founding problem is the mismatch between the person's experienced dignity (self-determination) and the institutional constraint (medical authority and state prohibition).
% FOUNDING_PROBLEM_CORROBORATION: Attested by: dying patients and disability-rights advocates who testify to experiences of forced prolongation; palliative-care researchers who document cases where pain management fails and autonomy becomes the only remaining dignity; jurisdictions that have legalized medical assistance in dying (Belgium, Netherlands, Canada) report ongoing requests motivated by loss of autonomy rather than pain alone. Contested by: sanctity advocates and some disability-rights groups who argue the founding problem is misframed—that autonomy in the context of depression or social isolation is not genuine, and that the 'solution' creates new harms.
narrative_ontology:disappearance_verdict(dignified_death__autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(dignified_death__autonomy_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignified_death__autonomy_primary, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(dignified_death__autonomy_primary, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dignified_death__autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dignified_death__autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dignified_death__autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.38 to 0.52 over the interval as the constraint becomes institutionalized and gatekeeping procedures solidify—the measurement captures how the initial autonomy norm becomes entangled with medical administrative burden and eligibility criteria that restrict actual access. Theater ratio rises from 0.25 to 0.41, indicating increasing performative activity: consultation processes, capacity assessments, waiting periods, and documentation that serve both protective and restrictive functions. The constraint hardens in practice as sanctity and relational-autonomy advocates press for more stringent capacity verification, gatekeeping intensifies in response, and the gap between nominal autonomy (the right to choose) and practical autonomy (actual access to choice) widens. Suppression rises from 0.55 to 0.68 as enforcement machinery activates: state prohibition, medical liability rules, social stigma against providers, and vulnerability-assessment criteria all increase active suppressive force. The plateau at t=25+ indicates the constraint reaches a stabilized enforced state where extraction and theater settle at elevated levels—the constraint becomes entrenched rather than continuing to intensify.
 *
 * PERSPECTIVAL GAP:
 *   The medical gatekeeper and the suffering individual denied exit should compute fundamentally different types from the same constraint. From the gatekeeper's perspective, the constraint is genuine rope—they are implementing autonomy respect, managing risk, and coordinating medical safeguards. From the suffering individual's perspective, the same constraint is snare-like—they are trapped, denied exit, and subjected to administrative gatekeeping whose criteria they cannot control. The engine computes this divergence from the structural data: the gatekeeper is institutional power with constrained exit (partly captured by the constraint's enforcement function) while the suffering individual is powerless with trapped exit (wholly subordinated). The same constraint substrate produces opposite classifications at different seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The autonomy_primary reading creates an unusual directionality profile. Nominally, the autonomous agent (the one whose self-determination is vindicated) is the beneficiary—they hold final authority, they can choose death. But actual directionality depends on whether they meet gatekeeping criteria: if they do, they benefit (d near 0.0, low extraction); if they don't or are suspected of not being truly autonomous, they are trapped (d near 1.0, high extraction). The suffering_individual_denied_exit bears the extraction of prolonged unwanted suffering—they are trapped and have no arbitrage options (d = 1.0). The medical_gatekeeper sits at moderate d (~0.5): they are partly constrained by liability rules and partly empowered by gatekeeping authority. The sanctity_advocate and vulnerable_population are excluded from authority but bear moral or practical costs—they sit at high d (extraction of moral witness, or risk of either false-permission or false-restriction). The directive override would be unnecessary here because the structural data derive the correct directionalities—but the commentary documents why different seats compute differently.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint's mandatrophy status is CONTESTED and carries high uncertainty. The founding problem (loss of autonomy at end-of-life, inability to self-determine timing and manner of death) is LIVE in the jurisdictions where the autonomy-primary reading holds authority (Belgium, Netherlands, Canada, some US states). In jurisdictions where the sanctity-primary reading dominates (much of the US, most religious institutions), the founding problem is treated as DEAD or MISCONSTRUED—the real problem is framed as preventing wrongful death, not enabling autonomous choice. The tangled_rope classification pins on the fact that autonomy is genuinely coordinated (the norm respects choice) WHILE gatekeeping systematically restricts access (the extraction). This is not mandatropic—the founding problem remains active and the constraint is doing work in response to it. However, the theater-ratio rise and the gatekeeping intensification suggest possible drift toward piton: if sanctity advocates successfully pressurize gatekeepers to apply eligibility criteria so strictly that almost no one qualifies, the constraint would become mostly performative (appears to honor autonomy while functionally denying it). The omega variables below capture this drift risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_vs_vulnerability,
    'When a suffering person seeks death, how do we distinguish autonomous choice from choice shaped by depression, social isolation, or coercive circumstances? What assessment standard is structurally sufficient?',
    'Longitudinal studies tracking persons denied death on capacity grounds: do those reclassified as ''more autonomous'' after treatment survive and report changed preferences? Do those approved despite capacity concerns show patterns of regret or coercion afterward?',
    'If autonomy assessments prove unreliable (many false negatives—genuinely autonomous persons denied; many false positives—coerced persons approved), the constraint''s gatekeeping becomes performative theater masking underlying vulnerability extraction. If reliable, the gatekeeping is protective coordination. The measurement would determine whether suppression/theater metrics increase as the constraint matures—unreliability would drive higher theater as gatekeeping activity increases without reducing harm.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(autonomy_vs_vulnerability, empirical, 'The structural reliability of capacity assessment in distinguishing autonomous choice from vulnerability-shaped choice.').

omega_variable(
    sanctity_vs_autonomy_foreclosure,
    'Does the autonomy-primary reading logically foreclose the sanctity-primary reading within a single moral or legal framework, or do they coexist as genuinely alternative readings held by different parties?',
    'Examine institutional configurations where both readings hold authority: does one systematically subordinate the other, or do they partition domains? In pluralist jurisdictions, do autonomy and sanctity advocates occupy genuinely separate evaluative frameworks, or is one trying to overcome the other?',
    'If autonomy FORECLOSES sanctity (one reading rules out the other), then the constraint is the victorious side of a zero-sum contest and the sanctity advocates are simply excluded losers. If they COEXIST, then the constraint is one reading among live alternatives and the contest is ongoing—sanctity pressure on gatekeeping intensity would be evidence of institutional tension, not constraint degradation. The relation type (forecloses vs. coexists_with) affects how we interpret theater-ratio rise and suppression intensification: is it enforcement of a decided question, or active suppression of a live contest?',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sanctity_vs_autonomy_foreclosure, conceptual, 'Whether autonomy-primary and sanctity-primary readings can coexist in one framework or one logically eliminates the other.').

omega_variable(
    gatekeeping_capture_by_sanctity,
    'As the constraint matures and gatekeeping procedures formalize, will institutional gatekeepers (physicians, ethics committees) be systematically pressurized by sanctity advocates to apply eligibility criteria so restrictively that practical autonomy collapses even as nominal autonomy persists on paper?',
    'Trend analysis on approval rates over time: if gatekeepers become more restrictive while case characteristics remain constant, the constraint is drifting toward piton (autonomy norm on paper, gatekeeping theater in practice). Interviews with gatekeepers on sanctity-advocate pressure and liability concerns.',
    'If gatekeeping becomes captured by sanctity pressure, the constraint would transition from tangled_rope (genuine autonomy norm + asymmetric gatekeeping) to piton (vestigial autonomy rhetoric + performative gatekeeping + actual sanctity enforcement). This would be a reading-level drift where the autonomy-primary constraint is replaced in practice by sanctity-primary enforcement, even though the written law nominally instantiates autonomy-primary. The measurement series already shows theater-ratio rise; continued rise to >0.6 with stagnant or declining approval rates would confirm piton trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeping_capture_by_sanctity, empirical, 'Risk of institutional capture of gatekeeping toward sanctity-primary enforcement despite autonomy-primary legal framing.').

omega_variable(
    reading_kernel_contest,
    'Which reading of the dignified_death kernel will achieve institutional dominance: autonomy-primary, sanctity-primary, or relational-autonomy? Or will the contest remain unresolved with different jurisdictions instantiating different readings?',
    'Follow legislative and judicial trends across jurisdictions over the next 20 years. Track whether autonomy-primary spreads (more jurisdictions legalize MAiD), sanctity-primary consolidates (pushback and restriction), or relational-autonomy emerges as a compromise institutional arrangement.',
    'If autonomy-primary achieves dominance, this constraint''s ε would stabilize at current levels and the tangled-rope classification would hold. If sanctity-primary pushes back successfully, the gatekeeping would intensify further (suppression rises above 0.8, theater rises above 0.6, extractiveness eventually exceeds 0.65) and the constraint would transition toward snare-like extraction of prolonged suffering. If relational-autonomy emerges as institutional compromise, this autonomy-primary constraint would coexist with a new sibling constraint that distributes authority across the triad—two readings both instantiated in mixed jurisdictions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_contest, empirical, 'Long-term institutional resolution of the dignified_death kernel contest among three readings.').

omega_variable(
    vulnerable_population_exclusion,
    'The constraint structurally excludes vulnerable populations (cognitively disabled, depressed, socially isolated) from the conversation about capacity assessment standards. Does this exclusion operate as a protective filter or as a suppression mechanism that denies autonomy to those who actually possess it?',
    'Track outcomes for persons with cognitive disability or depression in jurisdictions with autonomy-primary constraints: are they systematically denied access to MAID, and do they report this as either protective or as extractive denial of autonomy? Do advocacy organizations for disabled persons endorse or contest the constraint?',
    'If exclusion is experienced primarily as protective (vulnerable persons endorse it, disability advocates see it as harm-prevention), the suppression is genuine coordination cost. If experienced as extractive (denials reported as autonomy violation, advocates contest the criteria as biased), the constraint is extracting from vulnerable populations through gatekeeping that is not neutrally protective but selectively restrictive. This would increase effective extraction (χ) for those at the excluded/vulnerable seats and might justify classifying the constraint as snare rather than tangled_rope from their directionality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vulnerable_population_exclusion, empirical, 'Whether structural exclusion of vulnerable populations from capacity-assessment conversation is protective or extractive suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__autonomy_primary, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dign_tr_t0, dignified_death__autonomy_primary, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(dign_tr_t0, observed).
narrative_ontology:measurement(dign_tr_t5, dignified_death__autonomy_primary, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(dign_tr_t5, observed).
narrative_ontology:measurement(dign_tr_t10, dignified_death__autonomy_primary, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(dign_tr_t10, observed).
narrative_ontology:measurement(dign_tr_t15, dignified_death__autonomy_primary, theater_ratio, 15, 0.37).
narrative_ontology:measurement_basis(dign_tr_t15, observed).
narrative_ontology:measurement(dign_tr_t20, dignified_death__autonomy_primary, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(dign_tr_t20, observed).
narrative_ontology:measurement(dign_tr_t25, dignified_death__autonomy_primary, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(dign_tr_t25, observed).
narrative_ontology:measurement(dign_tr_t30, dignified_death__autonomy_primary, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(dign_tr_t30, observed).
narrative_ontology:measurement(dign_tr_t35, dignified_death__autonomy_primary, theater_ratio, 35, 0.41).
narrative_ontology:measurement_basis(dign_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(dign_be_t0, dignified_death__autonomy_primary, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(dign_be_t0, observed).
narrative_ontology:measurement(dign_be_t5, dignified_death__autonomy_primary, base_extractiveness, 5, 0.42).
narrative_ontology:measurement_basis(dign_be_t5, observed).
narrative_ontology:measurement(dign_be_t10, dignified_death__autonomy_primary, base_extractiveness, 10, 0.47).
narrative_ontology:measurement_basis(dign_be_t10, observed).
narrative_ontology:measurement(dign_be_t15, dignified_death__autonomy_primary, base_extractiveness, 15, 0.5).
narrative_ontology:measurement_basis(dign_be_t15, observed).
narrative_ontology:measurement(dign_be_t20, dignified_death__autonomy_primary, base_extractiveness, 20, 0.51).
narrative_ontology:measurement_basis(dign_be_t20, observed).
narrative_ontology:measurement(dign_be_t25, dignified_death__autonomy_primary, base_extractiveness, 25, 0.52).
narrative_ontology:measurement_basis(dign_be_t25, observed).
narrative_ontology:measurement(dign_be_t30, dignified_death__autonomy_primary, base_extractiveness, 30, 0.52).
narrative_ontology:measurement_basis(dign_be_t30, observed).
narrative_ontology:measurement(dign_be_t35, dignified_death__autonomy_primary, base_extractiveness, 35, 0.52).
narrative_ontology:measurement_basis(dign_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(dign_su_t0, dignified_death__autonomy_primary, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(dign_su_t0, observed).
narrative_ontology:measurement(dign_su_t5, dignified_death__autonomy_primary, suppression_requirement, 5, 0.59).
narrative_ontology:measurement_basis(dign_su_t5, observed).
narrative_ontology:measurement(dign_su_t10, dignified_death__autonomy_primary, suppression_requirement, 10, 0.62).
narrative_ontology:measurement_basis(dign_su_t10, observed).
narrative_ontology:measurement(dign_su_t15, dignified_death__autonomy_primary, suppression_requirement, 15, 0.65).
narrative_ontology:measurement_basis(dign_su_t15, observed).
narrative_ontology:measurement(dign_su_t20, dignified_death__autonomy_primary, suppression_requirement, 20, 0.67).
narrative_ontology:measurement_basis(dign_su_t20, observed).
narrative_ontology:measurement(dign_su_t25, dignified_death__autonomy_primary, suppression_requirement, 25, 0.68).
narrative_ontology:measurement_basis(dign_su_t25, observed).
narrative_ontology:measurement(dign_su_t30, dignified_death__autonomy_primary, suppression_requirement, 30, 0.68).
narrative_ontology:measurement_basis(dign_su_t30, observed).
narrative_ontology:measurement(dign_su_t35, dignified_death__autonomy_primary, suppression_requirement, 35, 0.68).
narrative_ontology:measurement_basis(dign_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignified_death__autonomy_primary, attachment_coordination).
narrative_ontology:boltzmann_floor_override(dignified_death__autonomy_primary, 0.12).
narrative_ontology:affects_constraint(dignified_death__autonomy_primary, dignified_death__sanctity_primary).
narrative_ontology:affects_constraint(dignified_death__autonomy_primary, dignified_death__relational_autonomy).

% DUAL FORMULATION NOTE:
% The dignified_death kernel decomposes into three structurally distinct constraints, each instantiating a different reading of what dignity means and where final authority resides. The autonomy_primary reading (this constraint) asserts dignity = self-determination, final authority = individual choice. It stands in structural tension with sanctity_primary (dignity = life's intrinsic value, final authority = transcendent moral law) and partial tension with relational_autonomy (dignity = relational, final authority = distributed across triad). Each reading has distinct ε, distinct beneficiary/victim structure, and distinct type classification. All three readings are live in contemporary bioethics; different jurisdictions instantiate different readings. The three constraints are linked via network.affects_constraints to model how institutional pressure on one reading affects the operation and gatekeeping intensity of the others. The readings are not alternative measurements of one constraint—they are genuinely different constraints arising from different foundations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
