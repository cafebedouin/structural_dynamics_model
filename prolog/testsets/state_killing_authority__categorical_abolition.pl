% ============================================================================
% CONSTRAINT STORY: state_killing_authority__categorical_abolition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_authority__categorical_abolition, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: state_killing_authority__categorical_abolition
 *   human_readable: Categorical Abolition of State Killing (Inalienable Life Reading)
 *   domain: political/philosophical/constitutional
 *
 * SUMMARY:
 *   The categorical abolition reading holds that state killing is inherently
 *   impermissible regardless of the crime committed or consequences
 *   prevented, grounded in the axiom that human life is inalienable—cannot be
 *   forfeited or transferred, even by the state. This reading emerged in
 *   Enlightenment philosophy and was institutionalized in international human
 *   rights law post-WWII. It competes with two sibling readings of the same
 *   kernel (state killing authority): the retributive reading (murderers
 *   forfeit their right to life; proportional punishment requires death for
 *   death) and the deterrence reading (execution is justified if and only if
 *   it prevents future murders at acceptable cost). The categorical abolition
 *   reading is claimed as a mountain—a natural law of human dignity—but the
 *   presence of identified beneficiaries (abolished persons, abolition
 *   movement) triggers false-summit evaluation. The authoring deliberate
 *   divergence between claimed type (mountain) and metrics (low-moderate
 *   extraction, moderate suppression, low theater) models the exact
 *   ambiguity: is inalienable life a discovered natural law, or a constructed
 *   axiom that benefits specific parties? The answer determines whether the
 *   constraint is a natural law or a false summit masquerading as one.
 *
 * KEY AGENTS:
 *   - Condemned persons: powerless beneficiaries structurally protected by the reading; their exit is escape from the regime itself, not movement within it.
 *   - Abolition movement: organized beneficiaries with mobile exit options; they mobilize politically to advance the reading.
 *   - Victims' families (abolitionist): moderate-power beneficiaries whose voices are structurally marginalized in prosecutorial and courtroom settings; the reading validates their position but may not center their agency.
 *   - State execution apparatus: institutional payer; the reading reframes execution as state violation rather than state right.
 *   - Victims' families (retributive): excluded from the reading's framework; their demand for proportional punishment is incompatible with inalienability axiom.
 *   - Retributive and deterrence jurists: excluded; the reading forecloses their premises on the ground that consequences and desert cannot override inalienable life.
 *   - International human rights bodies: observers and institutional validators of the reading; they produce jurisprudence grounding abolition in human dignity.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__categorical_abolition, 0.31).
domain_priors:suppression_score(state_killing_authority__categorical_abolition, 0.42).
domain_priors:theater_ratio(state_killing_authority__categorical_abolition, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, extractiveness, 0.31).
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__categorical_abolition, mountain).
narrative_ontology:human_readable(state_killing_authority__categorical_abolition, "Categorical Abolition of State Killing (Inalienable Life Reading)").
narrative_ontology:topic_domain(state_killing_authority__categorical_abolition, "political/philosophical/constitutional").

domain_priors:emerges_naturally(state_killing_authority__categorical_abolition).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__categorical_abolition, '21be4494-61f5-4c9d-a557-ff34e6cca747').
narrative_ontology:cs_kernel_codification('21be4494-61f5-4c9d-a557-ff34e6cca747', fixed_text).
narrative_ontology:cs_authority_grounding('21be4494-61f5-4c9d-a557-ff34e6cca747', lineage).
narrative_ontology:cs_interpretation_layer_present('21be4494-61f5-4c9d-a557-ff34e6cca747').
narrative_ontology:cs_reading_relation('21be4494-61f5-4c9d-a557-ff34e6cca747', state_killing_authority__retributive_desert, forecloses).
narrative_ontology:cs_reading_relation('21be4494-61f5-4c9d-a557-ff34e6cca747', state_killing_authority__deterrence_instrument, forecloses).
narrative_ontology:cs_axiom('21be4494-61f5-4c9d-a557-ff34e6cca747', foundational, life_is_inalienable).
narrative_ontology:cs_axiom_status(life_is_inalienable, holdable).
narrative_ontology:cs_axiom_grounding('21be4494-61f5-4c9d-a557-ff34e6cca747', life_is_inalienable, deontological).
narrative_ontology:cs_axiom('21be4494-61f5-4c9d-a557-ff34e6cca747', foundational, state_killing_authority_foreclosed).
narrative_ontology:cs_axiom_status(state_killing_authority_foreclosed, holdable).
narrative_ontology:cs_axiom_grounding('21be4494-61f5-4c9d-a557-ff34e6cca747', state_killing_authority_foreclosed, deontological).
narrative_ontology:cs_reference_frame('21be4494-61f5-4c9d-a557-ff34e6cca747', enlightenment_natural_rights_framework).
narrative_ontology:cs_drift_state('21be4494-61f5-4c9d-a557-ff34e6cca747', contemporary_capital_punishment_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('21be4494-61f5-4c9d-a557-ff34e6cca747', '').
narrative_ontology:cs_kernel_id(state_killing_authority__categorical_abolition, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, condemned_persons).
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, abolition_movement).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, victims_families_abolitionist).
narrative_ontology:constraint_victim(state_killing_authority__categorical_abolition, state_execution_apparatus).
narrative_ontology:constraint_vindicates(state_killing_authority__categorical_abolition, human_dignity_intrinsic).
narrative_ontology:constraint_vindicates(state_killing_authority__categorical_abolition, rights_inalienability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals sentenced to death under capital punishment regimes. Under the categorical abolition reading, they remain in the set of rights-holders whose inalienable right to life cannot be forfeited by any crime or state action. Their structural position: the reading posits that execution is impermissible regardless of the crime committed, making their legal status as living persons non-negotiable. Exit consists of commutation, exoneration, or abolition of the regime itself; within the current system they are trapped.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, condemned_persons, beneficiary,
    powerless, biographical, trapped, national).

% Organized networks of advocates, legal organizations, faith communities, and families opposing capital punishment on grounds of inalienable human dignity. They mobilize politically and legally to advance the reading that state killing is categorically impermissible. They have exit options (working in different jurisdictions, focusing on other causes) and can move resources between campaigns.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, abolition_movement, beneficiary,
    organized, generational, mobile, global).

% Families of murder victims who oppose capital punishment, either on principle or through conviction that execution does not serve justice or healing. Under the categorical abolition reading, they align with the inalienable-life doctrine. Their voices are structurally marginalized in prosecutorial and courtroom narratives that center victims' families as automatic demanders of death; their opposition to execution often goes unheard or is treated as betrayal of their murdered relatives.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, victims_families_abolitionist, beneficiary,
    moderate, biographical, constrained, national).

% The legal, prosecutorial, and correctional apparatus that administers capital punishment: legislatures enacting death statutes, prosecutors seeking death sentences, courts issuing execution orders, prisons carrying out executions. Under the categorical abolition reading, this apparatus becomes a potential violator of an inalienable right whenever it executes. The constraint reframes execution from lawful state action to state violation of fundamental human rights.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, state_execution_apparatus, payer,
    institutional, generational, mobile, national).

% Families of murder victims who believe the murderer deserves death or that execution serves justice or closure. The categorical abolition reading excludes them from the set of voices defining what justice requires; their demand for proportional punishment (death for death) is structurally incompatible with the axiom of inalienable life. They are present in courtrooms and victim-impact testimony but are absent from the philosophical framework the reading constructs.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, victims_families_retributive, excluded,
    moderate, biographical, constrained, national).

% Judges, legal scholars, and legislators who ground state killing authority in the doctrine of deserved punishment—the principle that murderers forfeit their right to life and that proportional punishment requires death for death. The categorical abolition reading rejects the premise that any crime can sever the right to life; these jurists are excluded from its legitimacy framework.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, retributive_desert_jurists, excluded,
    institutional, generational, constrained, national).

% Criminologists, policy makers, and legal theorists who justify capital punishment on empirical-consequentialist grounds—that execution deters future murders at acceptable cost. The categorical abolition reading rejects consequences entirely as a basis for making killing permissible; deterrence justification is structurally foreclosed by the axiom of inalienable life, regardless of empirical evidence.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, deterrence_empiricists, excluded,
    institutional, generational, constrained, national).

% UN bodies, regional human rights courts, and international treaty frameworks (e.g., European Convention on Human Rights, International Covenant on Civil and Political Rights Protocol 13) that have adopted the categorical abolition reading as international human rights law. They observe enforcement of the reading across signatory nations and produce jurisprudence grounding inalienable life in human dignity norms.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% Courts and constitutional bodies tasked with interpreting whether capital punishment violates constitutional protections (e.g., Eighth Amendment prohibitions on cruel and unusual punishment, Due Process Clause, or explicit constitutional abolition in some jurisdictions). They adjudicate whether the categorical abolition reading is constitutionally mandated, permitted, or foreclosed by the founding document's text and intent.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, constitutional_interpreters, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_authority__categorical_abolition, abolition_movement).
narrative_ontology:fixing_cost_class(state_killing_authority__categorical_abolition, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The categorical abolition reading coordinates a normative framework in which all legal actors—prosecutors, courts, corrections officials, legislators—are bound by the principle that human life cannot be lawfully forfeited by the state, regardless of crime or social consequence. It solves the coordination problem of what state power over life is legitimate: the answer is none, categorically.
% TRANSFER_FUNCTION: The reading transfers moral and legal authority away from state punishment logic (retributive desert, deterrence calculation) and toward the axiom of inalienable human dignity. No material transfer; what moves is the locus of legitimacy for state action over the condemned.
% ABSENT_VOICES: Victims' families who believe in retributive desert or who demand execution are structurally absent from the categorical abolition framework—their voices appear in courtroom victim-impact testimony but are treated as outside the philosophical bounds of human rights law. Deterrence empiricists are also absent from the framework: whatever their data shows, consequences cannot override the inalienable-life axiom. Condemned persons themselves are often absent from abolition discourse, which centers on abstract principles rather than the lived experience and agency of those facing death.
% DISAPPEARANCE_RATIONALE: If the categorical abolition reading disappeared, the constitutional and international framework grounding state action would revert to contested ground between retributive and deterrence justifications. The world would not rearrange structurally (executions could resume immediately), but the legitimacy framework would shift: state killing would no longer be categorically prohibited but would require justification on desert or deterrence grounds. Some jurisdictions have abolished execution; others retain it; the reading's disappearance would remove the international human rights consensus that makes abolition the default norm. The verdict is contested because some parties (abolitionists, international bodies) would experience the reading's disappearance as catastrophic, while others (retributivists, some victims' families) would experience it as liberation from unjust constraint.
% FOUNDING_PROBLEM: The founding problem was the recognition that state killing, even when legally authorized and imposed through due process, rests on a contestable premise: that human life can be forfeited through crime or can be lawfully taken to prevent future harms. The categorical abolition reading emerged to address this by asserting that the premise is false—that human life is inalienable and that the state's power to take life, unlike its power to imprison, cannot be justified.
% FOUNDING_PROBLEM_CORROBORATION: The categorical abolition reading is attested to by international human rights bodies (UN abolition protocols, regional courts), constitutional courts that have banned capital punishment (South Africa, Germany, Canada), and the abolition movement itself. Outside the benefiting parties: empirical criminologists largely dispute that execution serves deterrence (contradicting one retributive premise), and many secular legal scholars accept the inalienable-life axiom on grounds independent of religious doctrine. However, retributive legal theorists and some victims' families deny the founding problem exists at all, arguing that murderers DO forfeit their rights and that state killing is therefore legitimate. The reading's corroboration is substantial among international authorities and in jurisdictions that have abolished capital punishment, but is actively contested by retributive and deterrence theorists.
narrative_ontology:disappearance_verdict(state_killing_authority__categorical_abolition, contested).
narrative_ontology:founding_problem_status(state_killing_authority__categorical_abolition, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__categorical_abolition, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(state_killing_authority__categorical_abolition, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_authority__categorical_abolition_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, ExtMetricName, E),
    domain_priors:suppression_score(state_killing_authority__categorical_abolition, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(state_killing_authority__categorical_abolition),
    narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(state_killing_authority__categorical_abolition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness metric (0.31, rising to 0.31 over the interval) is low because the constraint does not extract material rents from those it governs; it removes a state capacity rather than redistributing value. However, it is non-zero because the reading excludes alternative readings (retributive, deterrence) from legitimacy—a form of epistemic extraction: the monopoly on what counts as permissible justification. The suppression metric (0.42) is moderate because the reading's enforcement depends partly on legal prohibition (treaties, constitutional bans) and partly on persuading legal actors to adopt the axiom; the reading is neither universally embraced nor violently suppressed. Theater (0.18) is low because the constraint's function (defining what state killing is permissible) is real and remains constant; the measured theater reflects some institutional theatricality (courts and prosecutors perform due-process rituals while the axiom itself is non-negotiable in abolitionist jurisdictions), but the core function is not degraded. Accessibility collapse (0.72) is moderately high because, once the inalienable-life axiom is understood and adopted, alternatives (retributive, deterrence justifications) become logically unavailable within that framework—the reading makes other readings unthinkable for those who accept it. Resistance (0.68) is high because the retributive and deterrence readings persist in many contemporary legal systems and are defended by substantial constituencies (prosecutors, some victims' families, retributive jurists); the categorical abolition reading meets real resistance in courtrooms, legislatures, and public opinion. The measurements show extractiveness and suppression rising very slightly over the 40-year interval (0.18→0.31 and 0.35→0.42), suggesting the reading's institutional dominance is expanding (more jurisdictions adopt abolition; international pressure increases), but the expansion is gentle because the reading faces entrenched retributive sentiment in capital-punishment jurisdictions.
 *
 * PERSPECTIVAL GAP:
 *   Condemned persons experience the constraint as pure protection (beneficiary, trapped, powerless → should classify as mountain from their seat). The state execution apparatus experiences it as pure constraint (payer, powerful, institutional → should classify as snare or mountain-as-foreclose from their seat, depending on how strongly they resist the axiom). Abolition movement experiences it as coordination + power (beneficiary, organized, mobile → should classify as rope or legitimate tangled_rope). Retributive jurists experience it as illegitimate constraint (excluded from framework, powerful institutional seat → should classify as snare, with the reading as the extractive mechanism). The divergence across seats is the measurement the framework exists to take: the same constraint (inalienable-life axiom) produces opposite classifications depending on structural position relative to it.
 *
 * DIRECTIONALITY LOGIC:
 *   Condemned persons: d near 1.0 (full target of the regime, beneficiary of the abolition reading = low d from execution apparatus perspective, high d from their own). The reading protects them from state killing, making them full beneficiaries; they have trapped exit (no choice but to accept the constraint); their d from the abolition reading's perspective is 0.0 (pure subsidy). From the execution apparatus perspective, condemned persons are targets of a protective constraint, so d from that perspective is inverted: the apparatus experiences the reading as blocking its capacity. Abolition movement: d near 0.3 (moderate beneficiary end, organized power, mobile exit). They benefit from the reading but can exit (work in other domains, change careers); they are not extracted from. State execution apparatus: d near 0.7 (moderate target end, institutional power, but constrained by the axiom). The apparatus is prevented from executing; it bears the cost of the constraint (loss of capacity). However, it has institutional exit (can redirect resources to non-lethal punishment, can migrate to capital-punishment jurisdictions), so d is not at 1.0 (not fully trapped). Victims' families (retributive): excluded from the framework, so their d is not directly computed; they experience the constraint as preventing access to their desired form of justice, which maps to high d if we were to compute it, but they are excluded rather than coordinated. The override mechanisms here would not be needed; the structural data already differentiates seats adequately.
 *
 * MANDATROPHY ANALYSIS:
 *   The categorical abolition reading's mandate is clear: abolish state killing because life is inalienable. The question is whether this mandate has outlived its founding problem. The founding problem was identified as the recognition that state killing rests on a contestable premise (forfeiture); the categorical abolition reading emerged to assert the opposite premise (inalienability). But retributive and deterrence readings still persist in many jurisdictions, suggesting the founding problem is not solved—the reading has not convinced all parties. However, in abolitionist jurisdictions, the reading has become default legal doctrine, and its mandate persists not as a solution to a live coordination problem but as a fixed constitutional principle. The question of mandatrophy becomes: has abolition become mere doctrine-assertion in jurisdictions that have adopted it, no longer solving any problem? The measurements show slight increases in suppression and theater over time, which could indicate institutionalization and theatricality creeping in—the reading becoming a fixed norm that requires less active moral commitment and more institutional enforcement. A mandatrophy verdict requires comparing founding problem status (contested: some say the problem is solved by abolition, others say it is live because retributive sentiment persists) against disappearance verdict (contested: some would experience disappearance as catastrophic, others as liberation). The strong contestation suggests the reading has not resolved mandatrophy and cannot; it persists as a live framework precisely because the underlying dispute about human dignity and forfeitability remains unresolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_axiom,
    'Is the categorical abolition reading grounded in a discovered natural law (human dignity is inherent and cannot be alienated) or is it a constructed normative commitment that benefits identifiable parties (abolition movement, condemned persons, international human rights institutions)?',
    'Genealogical analysis: trace the reading''s emergence in 18th-century Enlightenment philosophy (Beccaria, Kant) and in post-WWII human rights frameworks; examine whether the axiom predates the institutions that now benefit from it, or whether the institutions developed the axiom to serve their interests. Structural comparison: if the axiom were truly a natural law, it should hold invariantly across cultures and time; the fact that retributive and deterrence readings persist in contemporary legal systems suggests the axiom is a contested construction, not a discovered fact.',
    'If the reading is constructed (not a natural law), the constraint may be reclassified from mountain to tangled_rope (coordination + extraction) or snare, depending on whether it coordinates genuine moral consensus or imposes a reading that benefits abolition institutions. If the reading is a discovered natural law, it remains mountain, but the beneficiaries listed here would be false positives (a natural law has no beneficiaries, only believers).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_axiom, conceptual, 'Whether inalienable life is a natural law or a constructed axiom that benefits specific parties.').

omega_variable(
    inalienability_vs_forfeiture_premise,
    'Can the axiom that ''life is inalienable'' logically coexist with the retributive premise that ''murderers forfeit their right to life,'' or does one definitionally foreclose the other?',
    'Philosophical logic: examine whether ''inalienable'' means ''cannot be forfeited under any circumstances'' (strict interpretation) or ''can be alienated only by specific procedures or grounds'' (permissive interpretation). The categorical abolition reading takes the strict interpretation; the retributive reading interprets inalienability as context-dependent. Determine whether both readings can be held within a single coherent framework (coexists_with) or whether one definitionally eliminates the other (forecloses).',
    'If the readings foreclose each other (strict interpretation correct), capital punishment law is fundamentally incoherent and cannot be reformed—abolition becomes mandatory. If the readings coexist (permissive interpretation correct), both can be held as live options in different jurisdictions, and the contest is political rather than logical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inalienability_vs_forfeiture_premise, conceptual, 'Whether inalienability is absolute or conditional, and whether this determines the logical relationship between abolition and retributive readings.').

omega_variable(
    marginalization_of_victims_families,
    'Are victims'' families who oppose execution structurally marginalized by the categorical abolition reading, or are they its primary beneficiaries?',
    'Institutional analysis: document how prosecutors use victim-impact testimony to demand execution, whether abolitionist victims'' families have equal voice in courtrooms, and whether abolition campaigns center or silence the testimony of families who oppose death. Interview or survey abolitionist family members about whether the reading validates their position or treats their voice as secondary to abstract human rights claims. Measure whether abolition rhetoric uses victims'' families as spokespersons or marginalizes them in favor of broader human-rights arguments.',
    'If abolitionist families are genuinely empowered by the reading, it delivers coordination + protection (rope or legitimate tangled_rope). If they are marginalized—their voices used instrumentally but not centered—the reading may extract legitimacy from their trauma while sidelining their agency (snare component). The structural position of these families is crucial to whether the constraint is experienced as liberatory or extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginalization_of_victims_families, empirical, 'Whether abolitionist victims'' families are centered or marginalized in abolition discourse and institutions.').

omega_variable(
    enforcement_cost_vs_moral_foundation,
    'Is the categorical abolition reading enforced primarily through moral consensus (belief in inalienable life), legal prohibition (constitutional bans, international treaties), or suppression of retributive voices (marginalization of desert advocates)?',
    'Historical and contemporary analysis: measure the proportion of enforcement via (a) voluntary adoption of the axiom by legal systems and publics, (b) legal prohibition and international pressure, and (c) active suppression of alternative readings (e.g., exclusion of retributive voices from policy-making bodies, prosecution of capital punishment advocates). Examine whether abolition persists in jurisdictions with genuine moral consensus or whether it requires ongoing legal and institutional enforcement against retributive sentiment.',
    'High moral consensus with low enforcement cost = genuine mountain (natural law). High enforcement cost despite persistent retributive sentiment = the reading is constructed and maintained through institutional power, suggesting tangled_rope or snare characteristics. The suppression metric (0.42) is moderate, suggesting the reading is neither universally accepted nor violently suppressed—but the trend matters: if enforcement cost is rising, the reading is losing moral consensus and becoming more dependent on coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_cost_vs_moral_foundation, empirical, 'Whether the categorical abolition reading is self-sustaining through moral consensus or requires ongoing institutional enforcement against retributive sentiment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__categorical_abolition, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_authority__categorical_abolition, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(stat_tr_t0, observed).
narrative_ontology:measurement(stat_tr_t5, state_killing_authority__categorical_abolition, theater_ratio, 5, 0.1).
narrative_ontology:measurement_basis(stat_tr_t5, observed).
narrative_ontology:measurement(stat_tr_t10, state_killing_authority__categorical_abolition, theater_ratio, 10, 0.12).
narrative_ontology:measurement_basis(stat_tr_t10, observed).
narrative_ontology:measurement(stat_tr_t15, state_killing_authority__categorical_abolition, theater_ratio, 15, 0.14).
narrative_ontology:measurement_basis(stat_tr_t15, observed).
narrative_ontology:measurement(stat_tr_t25, state_killing_authority__categorical_abolition, theater_ratio, 25, 0.16).
narrative_ontology:measurement_basis(stat_tr_t25, observed).
narrative_ontology:measurement(stat_tr_t40, state_killing_authority__categorical_abolition, theater_ratio, 40, 0.18).
narrative_ontology:measurement_basis(stat_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_authority__categorical_abolition, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(stat_be_t0, observed).
narrative_ontology:measurement(stat_be_t5, state_killing_authority__categorical_abolition, base_extractiveness, 5, 0.22).
narrative_ontology:measurement_basis(stat_be_t5, observed).
narrative_ontology:measurement(stat_be_t10, state_killing_authority__categorical_abolition, base_extractiveness, 10, 0.25).
narrative_ontology:measurement_basis(stat_be_t10, observed).
narrative_ontology:measurement(stat_be_t15, state_killing_authority__categorical_abolition, base_extractiveness, 15, 0.28).
narrative_ontology:measurement_basis(stat_be_t15, observed).
narrative_ontology:measurement(stat_be_t25, state_killing_authority__categorical_abolition, base_extractiveness, 25, 0.3).
narrative_ontology:measurement_basis(stat_be_t25, observed).
narrative_ontology:measurement(stat_be_t40, state_killing_authority__categorical_abolition, base_extractiveness, 40, 0.31).
narrative_ontology:measurement_basis(stat_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_authority__categorical_abolition, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(stat_su_t0, observed).
narrative_ontology:measurement(stat_su_t5, state_killing_authority__categorical_abolition, suppression_requirement, 5, 0.37).
narrative_ontology:measurement_basis(stat_su_t5, observed).
narrative_ontology:measurement(stat_su_t10, state_killing_authority__categorical_abolition, suppression_requirement, 10, 0.39).
narrative_ontology:measurement_basis(stat_su_t10, observed).
narrative_ontology:measurement(stat_su_t15, state_killing_authority__categorical_abolition, suppression_requirement, 15, 0.4).
narrative_ontology:measurement_basis(stat_su_t15, observed).
narrative_ontology:measurement(stat_su_t25, state_killing_authority__categorical_abolition, suppression_requirement, 25, 0.41).
narrative_ontology:measurement_basis(stat_su_t25, observed).
narrative_ontology:measurement(stat_su_t40, state_killing_authority__categorical_abolition, suppression_requirement, 40, 0.42).
narrative_ontology:measurement_basis(stat_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__categorical_abolition, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(state_killing_authority__categorical_abolition, 0.12).
narrative_ontology:affects_constraint(state_killing_authority__categorical_abolition, state_killing_authority__retributive_desert).
narrative_ontology:affects_constraint(state_killing_authority__categorical_abolition, state_killing_authority__deterrence_instrument).

% DUAL FORMULATION NOTE:
% The state_killing_authority kernel comprises three distinct constraint stories, one per reading. All three are linked by network.affects_constraints to model their contestation and mutual influence. Categorical abolition structurally forecloses retributive and deterrence readings (if life is inalienable, neither desert nor consequences can justify killing). Retributive and deterrence readings coexist with each other but foreclose abolition. Each reading has its own epsilon value, beneficiary/victim structure, and type classification. Decomposition follows ε-invariance principle: changing the reading (observable) changes the structural claim (ε), so each reading is a separate constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
