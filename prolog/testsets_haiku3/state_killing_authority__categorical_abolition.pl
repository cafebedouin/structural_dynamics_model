% ============================================================================
% CONSTRAINT STORY: state_killing_authority__categorical_abolition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: state_killing_authority__categorical_abolition
 *   human_readable: State Killing Authority: Categorical Abolition Reading
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   The categorical abolition reading of state killing authority asserts that
 *   execution is inherently impermissible regardless of crime committed or
 *   future consequences prevented, because human life is inalienable and the
 *   state cannot legitimately take it. This reading competes with two sibling
 *   readings: the retributive reading (murderers forfeit their right to life;
 *   death is a proportional punishment) and the deterrence reading (capital
 *   punishment is justified if it prevents future murders at acceptable
 *   cost). The constraint story you are reading instantiates only the
 *   categorical abolition reading—a clean, ε-invariant account of how this
 *   particular axiom structures authority, benefit, extraction, and
 *   resistance. The other readings are separate constraint stories (not
 *   authored here); they are linked via network.affects_constraints and are
 *   analyzed by the committer frame in the cs_structure section.
 *
 * KEY AGENTS:
 *   - condemned_persons (powerless, trapped exit) — bear the ultimate extraction: their life and future are transferred to the state apparatus, which this reading declares violates their inalienable rights
 *   - death_penalty_abolition_movement (organized, mobile) — benefits from normative authority the reading confers on abolition; advocates its instantiation in law
 *   - human_rights_advocates (institutional, mobile) — benefit institutionally from treating capital punishment as per-se rights violation, which amplifies their monitoring and advocacy standing
 *   - murder_victims_families_favoring_execution (moderate, constrained) — bear a cost: their preferred remedy (offender's death) is delegitimized even as their loss is acknowledged
 *   - prosecutors_enforcing_capital_statute (institutional, constrained) — positioned as potential rights violators; their authority to enforce is categorically constrained
 *   - state_execution_apparatus (institutional, constrained) — administers condemned persons' deaths; under this reading, every act is framed as a violation of inalienable rights
 *   - retributive_justice_advocates (organized, excluded) — excluded from the framework; their core axiom (rights can be forfeited proportionally) is denied
 *   - deterrence_theorists (institutional, excluded) — excluded from normative conversation; consequences are not treated as legitimate grounds for overriding inalienable rights
 *   - analytical_observer (analytical) — records how this reading distributes authority, benefit, and extraction across seats
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__categorical_abolition, 0.82).
domain_priors:suppression_score(state_killing_authority__categorical_abolition, 0.71).
domain_priors:theater_ratio(state_killing_authority__categorical_abolition, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, extractiveness, 0.82).
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__categorical_abolition, snare).
narrative_ontology:human_readable(state_killing_authority__categorical_abolition, "State Killing Authority: Categorical Abolition Reading").
narrative_ontology:topic_domain(state_killing_authority__categorical_abolition, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_killing_authority__categorical_abolition).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__categorical_abolition, '6227b981-f997-44c2-866e-737bd8fa7fc1').
narrative_ontology:cs_kernel_codification('6227b981-f997-44c2-866e-737bd8fa7fc1', fixed_text).
narrative_ontology:cs_authority_grounding('6227b981-f997-44c2-866e-737bd8fa7fc1', lineage).
narrative_ontology:cs_interpretation_layer_present('6227b981-f997-44c2-866e-737bd8fa7fc1').
narrative_ontology:cs_reading_relation('6227b981-f997-44c2-866e-737bd8fa7fc1', state_killing_authority__retributive_desert, forecloses).
narrative_ontology:cs_reading_relation('6227b981-f997-44c2-866e-737bd8fa7fc1', state_killing_authority__deterrence_instrument, influences).
narrative_ontology:cs_axiom('6227b981-f997-44c2-866e-737bd8fa7fc1', foundational, life_is_inalienable).
narrative_ontology:cs_axiom_status(life_is_inalienable, holdable).
narrative_ontology:cs_axiom_grounding('6227b981-f997-44c2-866e-737bd8fa7fc1', life_is_inalienable, deontological).
narrative_ontology:cs_axiom('6227b981-f997-44c2-866e-737bd8fa7fc1', foundational, state_cannot_violate_inalienable_rights).
narrative_ontology:cs_axiom_status(state_cannot_violate_inalienable_rights, holdable).
narrative_ontology:cs_axiom_grounding('6227b981-f997-44c2-866e-737bd8fa7fc1', state_cannot_violate_inalienable_rights, deontological).
narrative_ontology:cs_axiom('6227b981-f997-44c2-866e-737bd8fa7fc1', secondary, consequences_cannot_override_inalienable_rights).
narrative_ontology:cs_axiom_status(consequences_cannot_override_inalienable_rights, holdable).
narrative_ontology:cs_axiom_grounding('6227b981-f997-44c2-866e-737bd8fa7fc1', consequences_cannot_override_inalienable_rights, deontological).
narrative_ontology:cs_reference_frame('6227b981-f997-44c2-866e-737bd8fa7fc1', inalienable_universal_human_rights).
narrative_ontology:cs_drift_state('6227b981-f997-44c2-866e-737bd8fa7fc1', contemporary_capital_punishment_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6227b981-f997-44c2-866e-737bd8fa7fc1', '').
narrative_ontology:cs_kernel_id(state_killing_authority__categorical_abolition, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, death_penalty_abolition_movement).
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, condemned_persons).
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, human_rights_advocates).
narrative_ontology:constraint_victim(state_killing_authority__categorical_abolition, murder_victims_families_favoring_execution).
narrative_ontology:constraint_victim(state_killing_authority__categorical_abolition, prosecutors_enforcing_capital_statute).
narrative_ontology:constraint_victim(state_killing_authority__categorical_abolition, state_execution_apparatus).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, religious_and_secular_inalienable_rights_communities).
narrative_ontology:constraint_victim(state_killing_authority__categorical_abolition, condemned_persons).
narrative_ontology:constraint_vindicates(state_killing_authority__categorical_abolition, universal_inalienable_human_rights_doctrine).
narrative_ontology:constraint_vindicates(state_killing_authority__categorical_abolition, bodily_autonomy_as_non_transferable_right).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Awaiting execution under a capital sentence. Under this reading, they retain inalienable rights to life despite their crime; the reading treats the state's authority to execute as a violation of those rights, not a legitimate punishment. They bear the ultimate extraction: the constraint legitimizes their death and transfers their potential future (which they have no exit from) to the state apparatus.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, condemned_persons, payer,
    powerless, immediate, trapped, national).

% Mobilizes around the principle that state killing violates inalienable human rights. They benefit from the normative authority this reading confers on their political agenda. They do not run the constraint; they advocate for its instantiation in law and practice. Their exit is to move between jurisdictions or historical moments where the reading is or is not dominant.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, death_penalty_abolition_movement, beneficiary,
    organized, generational, mobile, global).

% Operate human rights monitoring bodies and treaty regimes (International Criminal Court, UN bodies, regional human rights courts). They benefit from the categorical abolition reading because it frames capital punishment as a per-se human rights violation, which gives their advocacy and monitoring standing and amplifies their institutional authority.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, human_rights_advocates, beneficiary,
    institutional, generational, mobile, global).

% Seek execution as a form of justice or closure. Under this reading, their desire for the offender's death is treated as understandable but not as a basis for the state to violate inalienable rights. They are marginalized by this reading's framing; prosecutors cite their preferences, but the reading categorically forecloses that as a legitimate ground. They bear a cost: their preferred remedy is delegitimized even as their loss is acknowledged.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, murder_victims_families_favoring_execution, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(state_killing_authority__categorical_abolition, murder_victims_families_favoring_execution, excluded).

% Seek death sentences within existing capital statutes. Under this reading, they are cast as potential violators of inalienable rights. Their enforcement authority is constrained by the reading's premise that capital punishment is inherently impermissible. They extract from condemned persons (removal of the alternative of life imprisonment) but the reading treats that extraction as illegitimate state violence, not valid punishment.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, prosecutors_enforcing_capital_statute, agenda_setter,
    institutional, biographical, constrained, national).

% Administers the machinery, procedure, and ceremony of execution. Under this reading, every act the apparatus performs is framed as a violation of inalienable rights. The apparatus's functional legitimacy is categorically denied; it persists only through coercive state authority, not through normative acceptance of its purpose.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, state_execution_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Hold the reading that proportional punishment requires death for death (lex talionis / retributive desert). This reading excludes them from the legitimate discourse: it denies the core premise they rely on (that rights can be forfeited). They would argue for a different framing of the constraint but are not seated at the table where this reading adjudicates.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, retributive_justice_advocates, excluded,
    organized, generational, mobile, national).

% Argue that capital punishment is justified if it prevents future murders at acceptable cost. This reading categorically denies consequentialist grounds for execution: even if execution prevented future murders, inalienable rights remain non-negotiable. Deterrence theorists' empirical claims are not consumed as relevant to the core question; they are excluded from the normative framework.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, deterrence_theorists, excluded,
    institutional, generational, mobile, global).

% Communities whose ethical commitments (whether theological or philosophical) affirm the inalienable nature of human life. They benefit from the institutional and legal authority this reading confers on their worldview. Their doctrines are vindicated by treating inalienable rights as non-negotiable.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, religious_and_secular_inalienable_rights_communities, beneficiary,
    organized, generational, mobile, global).

% Diverse groups: some seek execution, some oppose it on abolition grounds, some pursue restorative justice or alternative remedies. This reading marginalizes the pro-execution subset, positioning their preferences as emotionally valid but normatively illegitimate grounds for state action. The divided coalition finds itself with internal conflict over which reading of the constraint to endorse.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, victim_advocacy_coalitions, excluded,
    moderate, biographical, constrained, national).

% Records and analyzes the constraint as a kernel reading, tracking how this particular instantiation (categorical abolition) competes with sibling readings (deterrence, retributive desert) and how the structural positions of agents shift across readings.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_authority__categorical_abolition, state_execution_apparatus).
narrative_ontology:fixing_cost_class(state_killing_authority__categorical_abolition, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: No genuine coordination problem is solved by this reading. It is a deontological constraint, not a coordination mechanism. The reading does not solve a collective-action problem; it asserts a normative boundary that forecloses certain state actions regardless of consequences or collective preference.
% TRANSFER_FUNCTION: The reading does not transfer resources between parties; it forbids a transfer (of the condemned person's life to the state). The analysis moves in the opposite direction: it identifies an attempted extraction (life, liberty, future) that the reading declares impermissible.
% ABSENT_VOICES: Murder victims' families who favor execution are structurally present but normatively marginalized by this reading. Retributive justice advocates and deterrence theorists are absent from the framework's legitimate conversation; their theoretical commitments are excluded by the reading's core axiom. Prosecutors and execution-apparatus personnel are present but positioned as potential rights violators, not as legitimate parties whose preferences should shape policy.
% DISAPPEARANCE_RATIONALE: If this reading and its enforcement vanished (replaced by one of the sibling readings), the entire justificatory structure for capital punishment would shift. Under the retributive reading, executions would resume on grounds of proportional desert. Under the deterrence reading, executions would be calibrated to empirical prevention data. The legal, moral, and institutional landscape would reorganize around a different framing of state authority over life.
% FOUNDING_PROBLEM: The founding problem (from this reading's perspective) is the claim that human life is alienable—that rights to existence can be forfeited through crime or transferred to the state as punishment. The reading treats this claim as a foundational error that subsequent capital punishment regimes have built upon. The problem is conceptual/normative, not practical: it is the legitimacy structure that permits state killing.
% FOUNDING_PROBLEM_CORROBORATION: International human rights bodies (UN, ICC, regional courts) affirm inalienable life as a foundational principle, providing corroboration from outside the pro-execution constituency. Philosophical and theological traditions spanning secular and religious frameworks (natural law, human rights law, some religious doctrines) corroborate the inalienable-rights premise. However, retributive justice scholars and deterrence researchers corroborate the opposing premise—that rights can be proportionally forfeited or that consequences justify execution. The corroboration splits along reading lines: each reading cites authorities that share its axioms.
narrative_ontology:disappearance_verdict(state_killing_authority__categorical_abolition, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_authority__categorical_abolition, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__categorical_abolition, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_killing_authority__categorical_abolition, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_authority__categorical_abolition, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_authority__categorical_abolition_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_authority__categorical_abolition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_authority__categorical_abolition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the constraint's operation transfers the condemned person's life to the state apparatus—the ultimate, non-restorable extraction. The condemned person has no exit (trapped) and no alternative (identity_locked to the sentence imposed). Suppression is substantial (0.71) because enforcing the constraint requires active state machinery: legal procedures, appeals systems, execution protocols. Theater is moderate (0.42): the legal procedure is partly genuine (rights arguments are heard), but a substantial portion of enforcement is theatrical—due-process rituals that ultimately proceed to a predetermined conclusion. Accessibility_collapse is moderate (0.68): condemned persons have no meaningful alternatives once sentenced, but the legal system still admits their rights claims even if the final verdict denies them. Resistance is high (0.73): substantial organized opposition to capital punishment, from abolitionist movements, human rights bodies, and families of victims who oppose execution. The measurement series shows extractiveness, suppression, and theater all rising over the interval, suggesting the enforcement machinery has hardened and the justificatory performance has grown more elaborate as resistance has intensified.
 *
 * PERSPECTIVAL GAP:
 *   This reading generates the widest seat divergence in the constraint family. From the prosecutor's perspective, capital punishment is authorized punishment administered through legitimate legal process. From the condemned person's perspective, the same process is a rights violation. From the abolition movement's perspective, it is a delegitimized state violence that should be prohibited. From victims' families favoring execution, it is an impossible remedy—their loss cannot be restored, but execution at least matches the wrong's magnitude. From retributive justice advocates, execution is required by proportional desert. From deterrence theorists, it is contingent on empirical evidence of prevention. The categorical abolition reading forecloses most of these perspectives from the legitimate framework—it does not negotiate with competing grounds, it asserts a categorical boundary.
 *
 * DIRECTIONALITY LOGIC:
 *   Condemned persons: d = 1.0 (full target). They are powerless, trapped, have no benefit from execution, and bear the ultimate cost (loss of life and future). Prosecutors and execution apparatus: d = 0.7 (near-target). They have institutional power but are trapped within the legal system; they administer extraction on behalf of the state. They do not collect the extraction themselves; they enforce it as an act of delegated state violence. Abolition movement: d = 0.1 (near-beneficiary). They have organized power, mobile exit, and benefit from the reading's normative authority. They do not bear extraction; they oppose it. Victims' families favoring execution: d = 0.55 (symmetric). They have moderate power and constrained exit; they bear a cost (their preferred remedy is delegitimized) but also a benefit (the reading acknowledges their loss and frames execution in response to it, even while denying its legitimacy). The reading creates complex asymmetry: it protects the condemned person's inalienable rights, which inverts the standard victim/perpetrator axis. The abolished punishment removes a traditional avenue for victims' families to seek closure, making them structurally similar to constrained payers.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy check: does the constraint's mandate outlive its function? The founding problem (from this reading's perspective) is the legitimacy structure that permits state killing—the claim that rights can be forfeited or that consequences justify execution. If the reading is adopted institutionally, the mandate is live: it continuously enforces the boundary that inalienable rights are non-negotiable. If the reading is NOT adopted institutionally (capital punishment persists under retributive or deterrence justification), then the abolition reading's mandate is denied—not mandatrophied, but defeated. Mandatrophy would arise if both: (1) the reading were institutionalized and capital punishment were abolished (mandate is achieved), AND (2) the abolition enforcement machinery persisted long after executions ceased, becoming purely theatrical. This is not the current state: the reading is contested, not institutionalized, so mandatrophy is not yet a diagnosis. However, the rising theater_ratio over the interval (0.28 to 0.42) suggests that if the reading were to win institutional adoption, enforcement might become increasingly performative—rehearsing inalienable-rights arguments long after the last execution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inalienable_life_vs_forfeiture_premise,
    'Is human life structurally inalienable (no action or crime can legitimately remove the right to exist), or can rights to life be proportionally forfeited through grave wrongs (retributive premise) or overridden by compelling consequences (deterrence premise)?',
    'Philosophical discourse and jurisprudential precedent. Natural law traditions and human rights law affirm inalienability; retributive and deterrence traditions affirm forfeiture or override. No empirical fact can resolve this—it is a foundational normative commitment that precedes any factual analysis.',
    'If inalienability is accepted as a logical or moral necessity, the categorical abolition reading becomes mandatory; all competing readings are foreclosed within that framework. If inalienability is rejected, the reading loses its core axiom and collapses into one of the competing readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(inalienable_life_vs_forfeiture_premise, conceptual, 'Whether life is inalienable by logical necessity, moral principle, or contingent axiom choice.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the high suppression (0.71) in this constraint primarily structural (legal machinery, execution apparatus, state coercion) or internalized (condemned persons internalize the verdict and accept its legitimacy)?',
    'Post-abolition trajectory: if suppression persists after the legal machinery is removed (condemned persons carry shame or self-perceived illegitimacy after release from death row), suppression is partially internalized. If suppression terminates when legal machinery is dismantled, it is purely structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests—the target carries suppression psychologically. If purely structural, remedies focus on dismantling apparatus rather than cognitive reprogramming.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression in capital punishment regimes is structural coercion or partially internalized.').

omega_variable(
    victim_families_excluded_or_constrained,
    'Are murder victims'' families who favor execution genuinely excluded from the categorical abolition framework (their preference is not even heard), or are they constrained payers (their preference is heard but delegitimized)?',
    'Examine whether abolition advocacy includes or excludes victims'' voices, and whether legal systems provide forums for victims to testify despite abolition rules. Constrained = their testimony is heard but the reading overrides it; excluded = their testimony is not solicited or is formally barred.',
    'If excluded, the reading has a structural absence (violated minorities). If constrained, the reading accommodates their voices while denying their normative claim—a different structural relationship.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(victim_families_excluded_or_constrained, empirical, 'Whether victims'' families are excluded from discourse or constrained within it.').

omega_variable(
    kernel_contest_empirical_vs_normative,
    'To what extent does the outcome of the kernel contest (which reading wins institutional adoption) depend on empirical facts (does capital punishment actually deter? do executions actually satisfy retributive principles?) versus pure normative commitment (is inalienable life a non-negotiable axiom)?',
    'Track how empirical research (deterrence studies, recidivism data) influences institutional adoption of competing readings. If empirical evidence shifts institutional verdicts, normative commitment is contingent. If institutional verdicts remain stable despite empirical challenge, normative commitment is prior.',
    'If empirical, the categorical abolition reading is vulnerable to evidence that execution prevents future murders (a deterrence victory). If normative, the reading remains stable regardless of empirical data.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_empirical_vs_normative, preference, 'Whether the kernel contest is decided by empirical evidence or prior normative axioms.').

omega_variable(
    reading_family_interdependence,
    'Are the three readings (categorical abolition, retributive desert, deterrence instrument) genuinely independent constraint families, or does institutional adoption of one reading suppress the others through legal and cultural mechanisms?',
    'Compare jurisdictions that have adopted categorical abolition (EU, many others) with those that retain capital punishment (US, others). Track whether abolition jurisdictions suppress retributive and deterrence discourse, or allow it to persist as minority academic/philosophical positions.',
    'If independent, each reading can coexist with others in pluralistic discourse. If interdependent, adoption of abolition requires suppression of competing readings as illegitimate, making the abolition reading itself extractive of the freedom to hold alternative normative frameworks.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_family_interdependence, empirical, 'Whether the constraint family readings coexist or suppress each other institutionally.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__categorical_abolition, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_authority__categorical_abolition, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(stat_tr_t0, observed).
narrative_ontology:measurement(stat_tr_t8, state_killing_authority__categorical_abolition, theater_ratio, 8, 0.32).
narrative_ontology:measurement_basis(stat_tr_t8, observed).
narrative_ontology:measurement(stat_tr_t16, state_killing_authority__categorical_abolition, theater_ratio, 16, 0.36).
narrative_ontology:measurement_basis(stat_tr_t16, observed).
narrative_ontology:measurement(stat_tr_t24, state_killing_authority__categorical_abolition, theater_ratio, 24, 0.39).
narrative_ontology:measurement_basis(stat_tr_t24, observed).
narrative_ontology:measurement(stat_tr_t32, state_killing_authority__categorical_abolition, theater_ratio, 32, 0.41).
narrative_ontology:measurement_basis(stat_tr_t32, observed).
narrative_ontology:measurement(stat_tr_t40, state_killing_authority__categorical_abolition, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(stat_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_authority__categorical_abolition, base_extractiveness, 0, 0.68).
narrative_ontology:measurement_basis(stat_be_t0, observed).
narrative_ontology:measurement(stat_be_t8, state_killing_authority__categorical_abolition, base_extractiveness, 8, 0.72).
narrative_ontology:measurement_basis(stat_be_t8, observed).
narrative_ontology:measurement(stat_be_t16, state_killing_authority__categorical_abolition, base_extractiveness, 16, 0.77).
narrative_ontology:measurement_basis(stat_be_t16, observed).
narrative_ontology:measurement(stat_be_t24, state_killing_authority__categorical_abolition, base_extractiveness, 24, 0.8).
narrative_ontology:measurement_basis(stat_be_t24, observed).
narrative_ontology:measurement(stat_be_t32, state_killing_authority__categorical_abolition, base_extractiveness, 32, 0.81).
narrative_ontology:measurement_basis(stat_be_t32, observed).
narrative_ontology:measurement(stat_be_t40, state_killing_authority__categorical_abolition, base_extractiveness, 40, 0.82).
narrative_ontology:measurement_basis(stat_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_authority__categorical_abolition, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(stat_su_t0, observed).
narrative_ontology:measurement(stat_su_t8, state_killing_authority__categorical_abolition, suppression_requirement, 8, 0.62).
narrative_ontology:measurement_basis(stat_su_t8, observed).
narrative_ontology:measurement(stat_su_t16, state_killing_authority__categorical_abolition, suppression_requirement, 16, 0.66).
narrative_ontology:measurement_basis(stat_su_t16, observed).
narrative_ontology:measurement(stat_su_t24, state_killing_authority__categorical_abolition, suppression_requirement, 24, 0.69).
narrative_ontology:measurement_basis(stat_su_t24, observed).
narrative_ontology:measurement(stat_su_t32, state_killing_authority__categorical_abolition, suppression_requirement, 32, 0.7).
narrative_ontology:measurement_basis(stat_su_t32, observed).
narrative_ontology:measurement(stat_su_t40, state_killing_authority__categorical_abolition, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(stat_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__categorical_abolition, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(state_killing_authority__categorical_abolition, 0.18).
narrative_ontology:affects_constraint(state_killing_authority__categorical_abolition, state_killing_authority__retributive_desert).
narrative_ontology:affects_constraint(state_killing_authority__categorical_abolition, state_killing_authority__deterrence_instrument).

% DUAL FORMULATION NOTE:
% The state_killing_authority kernel generates three constraint stories, each instantiating a different reading. This story (categorical_abolition) asserts inalienable life. The retributive_desert sibling asserts forfeitable rights and proportional punishment. The deterrence_instrument sibling asserts consequence-justified execution. All three share the same kernel (state authority over capital punishment) but instantiate different ε values, beneficiary structures, and victim sets. The constraint family models how one persistent kernel (the contested legitimacy of state killing) grounds multiple, structurally distinct constraints depending on which axiom is adopted.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_killing_authority__categorical_abolition, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
