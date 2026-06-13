% ============================================================================
% CONSTRAINT STORY: state_killing_authority__retributive_desert
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_authority__retributive_desert, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: state_killing_authority__retributive_desert
 *   human_readable: State Killing Authority (Retributive Desert Reading)
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This constraint instantiates the RETRIBUTIVE DESERT reading of the state
 *   killing authority kernel: murderers forfeit their right to life, and
 *   proportional punishment (lex talionis / eye-for-an-eye principle)
 *   requires death for death. This reading grounds state execution authority
 *   in the objective proportionality norm rather than in outcomes
 *   (deterrence) or consequences. The state claims it has the right to
 *   execute those who have murdered because the act of murder severs the
 *   murderer's claim to the same protection (life itself) they violated. The
 *   victim is vindicated posthumously through the symbolic restoration of
 *   proportional balance via state action. This reading COEXISTS with two
 *   sibling readings held by different political and philosophical factions:
 *   deterrence_instrument (capital punishment justified only if it prevents
 *   future murders) and categorical_abolition (state killing is always
 *   impermissible). The claim/metric divergence is deliberate: the constraint
 *   is CLAIMED as tangled_rope (coordination + extraction), authored metrics
 *   show high extractiveness and suppression, and the engine measures what
 *   that divergence reveals about the reading's actual structural position.
 *
 * KEY AGENTS:
 *   - state_sovereign_authority (institutional, analytical exit): Sets and enforces the capital statutes; claims authority to execute based on proportionality principle; administers trials and executions; experiences the constraint as a legitimate expression of sovereign order.
 *   - condemned_prisoners (powerless, trapped exit): Face execution; bear the ultimate extraction cost (their life); experience maximum suppression and accessibility collapse; have no meaningful exit or leverage to contest the forfeiture premise at the individual level.
 *   - murdered_victims_vindicated (powerless, trapped exit): Posthumously vindicated by the proportional execution of their murderer; benefit narratively and symbolically; receive no material compensation; their status as beneficiary is the reading's distinctive claim—in deterrence or abolition readings, this benefit structure does not arise.
 *   - families_of_victims (moderate, constrained exit): Positioned as beneficiaries (vindication through proportional punishment); some accept the frame, others find it hollow; constrained by the legal system's judgment; cannot override or negotiate the proportionality claim.
 *   - categorical_abolitionists (organized, constrained exit): Excluded from the decision structure because the retributive reading's axiom (forfeiture) directly contradicts their foundational premise (inalienability); they would argue state killing is categorically impermissible regardless of the crime.
 *   - constitutional_courts (institutional, analytical exit): Interpret whether the forfeiture and proportionality principles are constitutionally permissible; assess procedural safeguards; occupied as both agenda-setters (they constrain what the state can do) and observers (they measure alignment between principle and practice).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__retributive_desert, 0.68).
domain_priors:suppression_score(state_killing_authority__retributive_desert, 0.72).
domain_priors:theater_ratio(state_killing_authority__retributive_desert, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, extractiveness, 0.68).
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, resistance, 0.69).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__retributive_desert, tangled_rope).
narrative_ontology:human_readable(state_killing_authority__retributive_desert, "State Killing Authority (Retributive Desert Reading)").
narrative_ontology:topic_domain(state_killing_authority__retributive_desert, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_killing_authority__retributive_desert).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__retributive_desert, '08353b51-5b58-447d-9058-e6c9548c97ee').
narrative_ontology:cs_kernel_codification('08353b51-5b58-447d-9058-e6c9548c97ee', formalized).
narrative_ontology:cs_authority_grounding('08353b51-5b58-447d-9058-e6c9548c97ee', lineage).
narrative_ontology:cs_interpretation_layer_present('08353b51-5b58-447d-9058-e6c9548c97ee').
narrative_ontology:cs_reading_relation('08353b51-5b58-447d-9058-e6c9548c97ee', state_killing_authority__deterrence_instrument, coexists_with).
narrative_ontology:cs_reading_relation('08353b51-5b58-447d-9058-e6c9548c97ee', state_killing_authority__categorical_abolition, coexists_with).
narrative_ontology:cs_axiom('08353b51-5b58-447d-9058-e6c9548c97ee', foundational, murder_entails_forfeiture_of_life).
narrative_ontology:cs_axiom_status(murder_entails_forfeiture_of_life, holdable).
narrative_ontology:cs_axiom_grounding('08353b51-5b58-447d-9058-e6c9548c97ee', murder_entails_forfeiture_of_life, deontological).
narrative_ontology:cs_axiom('08353b51-5b58-447d-9058-e6c9548c97ee', foundational, proportional_lex_talionis_self_justifying).
narrative_ontology:cs_axiom_status(proportional_lex_talionis_self_justifying, holdable).
narrative_ontology:cs_axiom_grounding('08353b51-5b58-447d-9058-e6c9548c97ee', proportional_lex_talionis_self_justifying, deontological).
narrative_ontology:cs_reference_frame('08353b51-5b58-447d-9058-e6c9548c97ee', natural_law_proportional_justice).
narrative_ontology:cs_drift_state('08353b51-5b58-447d-9058-e6c9548c97ee', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('08353b51-5b58-447d-9058-e6c9548c97ee', '').
narrative_ontology:cs_kernel_id(state_killing_authority__retributive_desert, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, murdered_victims_vindicated).
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, state_sovereign_authority).
narrative_ontology:constraint_victim(state_killing_authority__retributive_desert, condemned_prisoners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, families_of_condemned).
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, families_of_victims).
narrative_ontology:constraint_victim(state_killing_authority__retributive_desert, families_of_condemned).
narrative_ontology:constraint_vindicates(state_killing_authority__retributive_desert, proportionality_doctrine).
narrative_ontology:constraint_vindicates(state_killing_authority__retributive_desert, forfeiture_theory_of_rights).
narrative_ontology:constraint_vindicates(state_killing_authority__retributive_desert, lex_talionis_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The state claims exclusive authority to identify murderers, conduct trials, and execute proportional punishment (death) based on the proportionality principle. The state administers capital statutes, enforces appeals processes, and carries out executions. In this reading, state authority is grounded in an objective principle (proportionality/forfeiture) that the state does not create but adjudicates. The state's interest is to maintain sovereignty, to express the legal order's commitment to the proportionality principle, and to vindicate victims through the ritual and reality of execution.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, state_sovereign_authority, agenda_setter,
    institutional, civilizational, analytical, national).

% The deceased victim is posthumously vindicated when the state executes the murderer based on the proportionality principle. The victim cannot act, negotiate, or benefit materially; they are treated as benefiting symbolically and objectively through the restoration of proportional justice. This beneficiary status is unique to the retributive reading: in deterrence or abolition readings, the victim does not appear as a beneficiary of the constraint.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, murdered_victims_vindicated, beneficiary,
    powerless, biographical, trapped, national).

% Face execution on the finding that they committed murder and therefore forfeited their right to life under the proportionality principle. They are treated as having no legitimate claim to contest the foundational principle (though they may contest the trial's fairness or the fact-finding). They bear the ultimate extraction cost: their life. Suppression is maximized at this seat: they cannot exit, escape, seek reprieve on the grounds that the principle is illegitimate, or negotiate. They experience the constraint at full force.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, condemned_prisoners, payer,
    powerless, immediate, trapped, national).

% Carry emotional, social, and sometimes economic costs of having a family member condemned to death and executed. Some may accept the forfeiture principle and experience the execution as justice; others reject the principle entirely and experience it as state killing. They are constrained by the legal system's judgment and cannot override it; they can only appeal within its processes (clemency petitions, executive review). Their dual role reflects the ambiguity in how they experience the constraint: as bearers of cost (payer) or as participants in the restoration of justice (beneficiary).
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, families_of_condemned, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(state_killing_authority__retributive_desert, families_of_condemned, beneficiary).

% Are presented as beneficiaries of the retributive proportionality principle: the execution of the murderer is framed as fulfilling their vindication and their loved one's vindication. Some experience closure and accept the frame; others find the frame hollow, prefer alternatives (life imprisonment allowing potential reconciliation, restorative processes, restitution), or reject capital punishment on principle. They are constrained by the legal system's judgment and cannot negotiate alternative sentences, though they may have limited victim-impact testimony rights.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, families_of_victims, beneficiary,
    moderate, biographical, constrained, national).

% Argue that state killing is categorically impermissible regardless of the crime or the proportionality claim, grounding their position in human rights doctrine and the inalienability of life. They are excluded from the retributive reading's decision structure because the reading's foundational axiom (forfeiture via murder) directly contradicts their foundational axiom (inalienability). They remain outside the constraint's frame, neither coordinated nor extracted from—they are excluded as a structural matter because their premise is foreclosed.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, categorical_abolitionists, excluded,
    organized, generational, constrained, global).

% Argue that capital punishment's legitimacy depends entirely on empirical evidence that it prevents future murders at acceptable cost compared to alternatives. They are excluded from the retributive reading's frame because the reading grounds authority in proportionality principle, not in deterrent outcomes; deterrence evidence is treated as irrelevant to whether the principle justifies execution. They remain outside because their evidential ground (outcome) is displaced by the reading's normative ground (principle).
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, deterrence_advocates, excluded,
    organized, generational, constrained, global).

% Document exonerations and systemic error in capital trials. They are analytical observers: they do not attack the proportionality principle directly but present evidence that the constraint's operation (identifying who has actually committed murder) is fallible. Their work supplies data for evaluating whether the constraint's suppression mechanism (limited appeals, irreversibility, confined clemency) is compatible with the underlying forfeiture premise if guilt identification is uncertain.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, innocence_project_advocates, observer,
    organized, generational, analytical, national).

% Interpret whether the forfeiture and proportionality principles are constitutionally permissible, whether cruel-and-unusual clauses or equal-protection doctrines constrain capital punishment, and whether procedural safeguards (appellate review, DNA testing, jury composition, jury instructions) adequately protect against misidentification. They set the bounds of permissible state action (agenda-setter role) and assess whether the constraint's operation aligns with its foundational premises (observer role).
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, constitutional_courts, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(state_killing_authority__retributive_desert, constitutional_courts, observer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_authority__retributive_desert, state_sovereign_authority).
narrative_ontology:fixing_cost_class(state_killing_authority__retributive_desert, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a rule-based, state-enforced system for identifying murderers, adjudicating culpability, and executing proportional punishment. Solves the coordination problem of preventing private revenge, unequal justice, and vendetta cycles by centralizing punishment under a fixed principle (proportionality/forfeiture) rather than delegating it to victims' families or allowing summary execution.
% TRANSFER_FUNCTION: Transfers the condemned murderer's fundamental claim to life (their right to bodily existence and continued existence) from the individual to the state's authority to execute that extraction. The victim's vindication flows symbolically and narratively from the restoration of proportional balance via state execution of the murderer. No material goods are transferred; the extraction is of the condemned's life itself.
% ABSENT_VOICES: Categorical abolitionists and deterrence advocates are structurally excluded from the retributive reading's decision frame. They would argue that the foundational premises (forfeiture, proportionality as self-justifying) are not legitimate starting points. The voices of condemned prisoners are also minimized: they are treated as having forfeited standing to object to the proportionality principle itself (they may contest the trial's fairness, but not the principle's validity). Innocence project advocates are largely absent from the retributive framing because the principle assumes perfect or near-perfect guilt identification; their evidence of fallibility challenges the frame but is not treated as part of the retributive reading's own logic.
% DISAPPEARANCE_RATIONALE: If this constraint (state authority to execute murderers based on forfeiture/proportionality) vanished overnight, multiple systems would reorganize: criminal justice would shift to alternative sanctions (life imprisonment, restitution, restorative models); the state's authority to inflict death would disappear; international human rights norms against capital punishment would shift from abolitionist exhortation to universal law; private revenge and vigilante justice might resurface in some communities where state punishment authority had been displaced. The removal would be a civilizational shift in the moral grammar of punishment.
% FOUNDING_PROBLEM: The need to establish a rule-based system of punishment that (1) prevents private revenge and vendetta cycles, (2) ensures punishment is proportional to the crime rather than arbitrary or power-driven, and (3) restores the victim's dignity by proportionally vindicating the harm done to them through an objective principle (lex talionis) rather than mere state power. The retributive reading asserts that murder severs the murderer's claim to life, and proportional execution restores the natural order of justice.
% FOUNDING_PROBLEM_CORROBORATION: The state apparatus and retributive legal theorists (philosophers, judicial figures, traditional criminal law doctrines) attest the founding problem is still live. International human rights bodies (UN, European Court of Human Rights, organizations tracking capital punishment), abolitionist jurisdictions, and empirical criminologists attest the founding problem has been substantially solved by alternative systems (life imprisonment without parole, rule-of-law trial procedures without capital punishment) and that the constraint now persists as ideological commitment. Legislative testimony from abolitionist jurisdictions (without reversion to vigilantism), comparative data on crime rates, and victim-family surveys showing diverse attitudes toward capital punishment (some finding closure through execution, others through alternatives) provide corroboration outside the state apparatus.
narrative_ontology:disappearance_verdict(state_killing_authority__retributive_desert, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_authority__retributive_desert, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__retributive_desert, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(state_killing_authority__retributive_desert, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_authority__retributive_desert_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_authority__retributive_desert, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_authority__retributive_desert_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.68 at interval end) because the constraint transfers the condemned prisoner's most fundamental claim (to life itself) from the individual to the state, grounded in a principle (proportionality) that is self-justifying rather than empirically contestable. Suppression is high (0.72) because the constraint depends on preventing condemned prisoners from exercising exit options (escape, reprieve) and on suppressing alternative readings (deterrence, abolition) that would reground authority elsewhere. Theater is moderate (0.41) because ritual elements are present (formal trial, pronouncement of sentence, ceremonial aspects of execution) but a substantial share of enforcement effort is genuinely devoted to identifying murderers and enforcing the proportionality principle—the constraint is not purely performative. Accessibility collapse is high (0.78) because once condemned under the forfeiture principle, the condemned prisoner has essentially no exit: appeals are constrained to procedural errors, clemency is discretionary, and the principle itself (not the application) is treated as non-negotiable. The time series shows extraction and theater rising gradually over the interval as procedural safeguards become more elaborate (increasing theater cost) and as the application of capital punishment becomes more selective and defended (increasing extraction per remaining execution). The coercion grid shows acute asymmetry across levels: individual-level accessibility collapse and stakes inflation are near-maximal (0.88, 0.98) for the condemned, but organizational and class-level resistance grow substantially (0.72–0.75 organizational at t60) as abolitionist movements and international human rights bodies mount pressure. This pattern is distinctive to the retributive reading: the constraint operates with maximum force on individuals deemed to have forfeited their rights, but faces rising organizational and structural resistance from those who reject the forfeiture axiom.
 *
 * PERSPECTIVAL GAP:
 *   The state and retributive theorists experience this constraint as the legitimate expression of a fundamental principle of justice (proportionality). From the condemned prisoner's seat, it is an irreversible extraction of their life based on a principle they may contest. From the abolitionist's seat, it is not a constraint at all but a violation of an inalienable right—the constraint's very existence violates their foundational premise. From the victim's family's seat, the vindication may feel genuine or hollow depending on whether they accept the proportionality framing. The engine computes per-seat classification from the structural data: the state's seat (powerful, analytical exit, agenda-setter role) derives low effective extraction (the state gains authority and symbolic vindication); the condemned prisoner's seat (powerless, trapped exit, payer role) derives high effective extraction (they lose their life); the abolitionist's seat (organized, constrained exit, excluded role) derives a different classification altogether (the constraint forecloses their foundational axiom, making it not a constraint they can negotiate but a violation they must resist). This perspectival divergence is structural, not a matter of opinion: it follows from the different exits, powers, and foundational premises the seats occupy.
 *
 * DIRECTIONALITY LOGIC:
 *   The state (institutional power, analytical exit, agenda-setter) is the structural beneficiary: it collects sovereign authority, procedural legitimacy, and the symbolic vindication of proportional order. Directionality d is low (beneficiary end ~0.15–0.25) because the state's role is to administer, not to suffer the constraint. The condemned prisoner (powerless, trapped exit, payer) is the structural target: they bear extraction directly and maximally. Directionality d is high (target end ~0.85–0.95) because their exit is closed and their cost is irreversible. The murdered victim (posthumously beneficiary) occupies a unique position: they do not negotiate or exit because they are dead; their benefit is narrative and symbolic, restored by the state's execution of the murderer. The families of victims sit near symmetric (~0.50) or moderately toward target (~0.55–0.65): they receive symbolic vindication (beneficiary element) but also carry emotional and sometimes social cost, and they cannot override the system's judgment. The abolitionist (organized, constrained exit, excluded) is not part of the constraint's beneficiary/victim structure—they are outside the reading entirely, and their exclusion is itself the suppression mechanism. No directionality override is needed: the structural derivation from beneficiary/victim + exit captures the seats' positions accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing private revenge, establishing rule-based punishment, grounding authority in an objective principle) was real and has been substantially addressed. However, the retributive reading asserts that the problem remains live: murderers still violate the victim's fundamental right to life, and proportional punishment still restores the victim's vindication. Abolitionist jurisdictions counter that the founding problem has been solved by alternative systems (life imprisonment without release, restorative justice, rule-of-law trials without capital punishment) and that the constraint now persists as ideological commitment rather than necessity. The mandatrophy divergence sits here: founding_problem_status = contested. The constraint shows no signs of atrophying (execution numbers in the US have plateaued rather than declining, though international abolitionism is rising); the theater ratio is rising (more elaborate appeals and procedural safeguards) but not dominantly (0.41 is not piton-range, ~0.55+). The constraint is not resolved mandatrophy (base_properties.mandatrophy_resolved would be true only if the problem were acknowledged as dead and the constraint maintained purely performatively, which the retributive reading rejects). Instead, it exhibits contested mandatrophy: whether the founding problem is live or dead depends entirely on which reading (retributive, deterrence, or abolitionist) is adopted. This is routed to omega variables rather than treated as a classification failure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    forfeiture_premise_validity,
    'Does the act of murder logically entail the forfeiture of the murderer''s right to life, or is this a normative choice that could be rejected without internal contradiction?',
    'Conceptual analysis and genealogical study: trace whether forfeiture doctrine is grounded in a deontological axiom (rights lost by violating rights) or in a conventional rule that could be otherwise. Compare with non-forfeiture alternative systems (life imprisonment, restitution, restorative justice) that operate without the forfeiture premise and ask whether they face logical inconsistency or merely political opposition.',
    'If forfeiture is purely conventional (not logically entailed), the retributive reading becomes a choice rather than a necessity, and abolitionist readings are not foreclosed but merely opposed. If forfeiture is deontological, the retributive reading forecloses abolition at the level of principle. Classification could shift from tangled_rope (hybrid coordination + extraction) to snare (extraction masked as principle) depending on the resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(forfeiture_premise_validity, conceptual, 'Whether forfeiture is logically entailed by murder or is a normative choice.').

omega_variable(
    procedural_fallibility_and_principle,
    'If the retributive principle (death for death) is self-justifying, how does systemic error in identifying murderers (wrongful convictions, exonerations) interact with the principle''s legitimacy?',
    'Case study of innocence project exonerations in capital cases: does the state''s authority persist when exercised on the innocent? If yes, is the authority grounded in the principle or in the state''s power? If no, what procedural floor is necessary (DNA-grade certainty? Unanimous juries? Eyewitness corroboration?) for the principle to hold?',
    'If the principle is truly self-justifying (proportionality requires death), then executing the innocent would be a misapplication of the principle, not a refutation of it. If the principle requires perfect guilt identification, then the constraint''s actual operation (fallible trials) diverges from its foundational claim, suggesting the constraint is actually snare (extraction masked as principle) rather than tangled_rope (genuine coordination + asymmetric extraction). Could shift classification toward snare or lead to authored restrictions (narrower victim set, procedural gatekeeping, mandate for exonerations).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(procedural_fallibility_and_principle, empirical, 'Whether the proportionality principle can survive procedural error in guilt identification.').

omega_variable(
    victim_vindication_mechanism,
    'What mechanism of vindication operates for the dead victim? Are they genuinely benefited by the execution of their murderer, or is the benefit transferred to the state and the victim''s family, with the victim narrated as beneficiary for legitimacy?',
    'Philosophical analysis: examine whether a deceased individual can be harmed or benefited, and whether proportional punishment on their behalf is restoration of their right or projection of the state''s/ family''s need for retribution onto the victim''s behalf. Interview families of victims and draw distinctions between those who experience vindication and those who do not, correlating with acceptance/rejection of the retributive frame.',
    'If the victim is genuinely benefited (deontological axiom: proportional punishment restores their dignity), the retributive reading''s beneficiary structure is structurally sound. If vindication is a retrospective narrative applied to placate the family and legitimize the state''s execution, the true beneficiary is the state (not the victim), and the constraint is snare-like (extraction framed as coordination). Could alter the beneficiaries array and the characterization of whether the constraint is tangled_rope or snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_vindication_mechanism, preference, 'Whether the murdered victim is a genuine beneficiary or a narrativized post-hoc justification.').

omega_variable(
    kernel_reading_uncertainty,
    'This constraint is one reading of the state_killing_authority kernel. What are the committer-axis grounds for having chosen this reading over the deterrence_instrument or categorical_abolition readings?',
    'Document the signals and context that guided the choice: Is this reading the historically dominant one in the relevant jurisdiction? Does the evidence support it? Does the question-asker''s own framework commit them to this reading, or are they exploring what it would mean to adopt it? This is a meta-question about the authoring process itself.',
    'This omega records that the constraint-as-authored is one committer choice among three structurally distinct alternatives. It does not change classification but documents the contingency of having chosen this reading. For the corpus: constrains comparison across readings (they are not contradictory observations of one constraint; they are structurally different constraints of one kernel).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_uncertainty, conceptual, 'Committer-axis framing uncertainty: why this reading rather than the alternatives.').

omega_variable(
    suppression_internalization_in_condemned,
    'The measured suppression (0.72) is structural: legal barriers, appellate constraints, irreversibility of execution. But condemned prisoners may also internalize the forfeiture claim (accepting that they have forfeited their right to live). Is the suppression partly internalized, and if so, does it persist after exit (if a condemned prisoner were reprieved, would they continue to accept the forfeiture premise)?',
    'Case study analysis of reprieved and exonerated individuals: do they reject the forfeiture premise after reprieve, or do they retain acceptance of it? Interview data from condemned prisoners'' statements, clemency petitions, and final words: do they contest the principle or only the application?',
    'If suppression is partly internalized, the constraint''s effective suppression on the condemned is higher than the structural measure (0.72) suggests—they carry the suppression with them psychologically. If suppression is purely structural, the structural measure is complete. This affects per-seat classification: internalized suppression intensifies the target-seat''s extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_in_condemned, empirical, 'Internalization of suppression in the condemned: structural vs. internalized mechanisms.').

omega_variable(
    lex_talionis_commensurability,
    'The proportionality principle asserts that death for death is commensurable. But is the state''s infliction of death truly proportional to the murderer''s infliction of death, or does the state''s authority, process, and ritual element add dimensions that make state killing qualitatively different from murder?',
    'Comparative philosophy and theology: trace versions of lex talionis across traditions (biblical, Islamic, Roman, modern retributivism). Assess whether proportionality is asserted to be commensurable-in-fact or merely as a normative claim (it should be treated as proportional). If merely normative, the principle is a choice rather than a discovery.',
    'If proportionality is a discovered fact of equivalence, the retributive principle is ontologically grounded (death = death). If it is a normative choice, the retributive reading''s claim to objectivity is weakened, and alternative grounds (deterrence, restorative justice, abolition) become viable on different normative premises. Could shift the characterization of whether the constraint forecloses alternatives or merely proposes one reading among incommensurable options.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lex_talionis_commensurability, conceptual, 'Whether lex talionis asserts commensurability-in-fact or as a normative principle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__retributive_desert, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_authority__retributive_desert, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(stat_tr_t0, observed).
narrative_ontology:measurement(stat_tr_t10, state_killing_authority__retributive_desert, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(stat_tr_t10, observed).
narrative_ontology:measurement(stat_tr_t20, state_killing_authority__retributive_desert, theater_ratio, 20, 0.36).
narrative_ontology:measurement_basis(stat_tr_t20, observed).
narrative_ontology:measurement(stat_tr_t30, state_killing_authority__retributive_desert, theater_ratio, 30, 0.39).
narrative_ontology:measurement_basis(stat_tr_t30, observed).
narrative_ontology:measurement(stat_tr_t45, state_killing_authority__retributive_desert, theater_ratio, 45, 0.41).
narrative_ontology:measurement_basis(stat_tr_t45, observed).
narrative_ontology:measurement(stat_tr_t60, state_killing_authority__retributive_desert, theater_ratio, 60, 0.41).
narrative_ontology:measurement_basis(stat_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_authority__retributive_desert, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(stat_be_t0, observed).
narrative_ontology:measurement(stat_be_t10, state_killing_authority__retributive_desert, base_extractiveness, 10, 0.59).
narrative_ontology:measurement_basis(stat_be_t10, observed).
narrative_ontology:measurement(stat_be_t20, state_killing_authority__retributive_desert, base_extractiveness, 20, 0.63).
narrative_ontology:measurement_basis(stat_be_t20, observed).
narrative_ontology:measurement(stat_be_t30, state_killing_authority__retributive_desert, base_extractiveness, 30, 0.67).
narrative_ontology:measurement_basis(stat_be_t30, observed).
narrative_ontology:measurement(stat_be_t45, state_killing_authority__retributive_desert, base_extractiveness, 45, 0.68).
narrative_ontology:measurement_basis(stat_be_t45, observed).
narrative_ontology:measurement(stat_be_t60, state_killing_authority__retributive_desert, base_extractiveness, 60, 0.68).
narrative_ontology:measurement_basis(stat_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_authority__retributive_desert, suppression_requirement, 0, 0.65).
narrative_ontology:measurement_basis(stat_su_t0, observed).
narrative_ontology:measurement(stat_su_t10, state_killing_authority__retributive_desert, suppression_requirement, 10, 0.68).
narrative_ontology:measurement_basis(stat_su_t10, observed).
narrative_ontology:measurement(stat_su_t20, state_killing_authority__retributive_desert, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(stat_su_t20, observed).
narrative_ontology:measurement(stat_su_t30, state_killing_authority__retributive_desert, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(stat_su_t30, observed).
narrative_ontology:measurement(stat_su_t45, state_killing_authority__retributive_desert, suppression_requirement, 45, 0.72).
narrative_ontology:measurement_basis(stat_su_t45, observed).
narrative_ontology:measurement(stat_su_t60, state_killing_authority__retributive_desert, suppression_requirement, 60, 0.72).
narrative_ontology:measurement_basis(stat_su_t60, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=60
narrative_ontology:measurement(stat_grid_01, state_killing_authority__retributive_desert, accessibility_collapse(class), 0, 0.68).
narrative_ontology:measurement(stat_grid_02, state_killing_authority__retributive_desert, accessibility_collapse(class), 60, 0.71).
narrative_ontology:measurement(stat_grid_03, state_killing_authority__retributive_desert, accessibility_collapse(individual), 0, 0.88).
narrative_ontology:measurement(stat_grid_04, state_killing_authority__retributive_desert, accessibility_collapse(individual), 60, 0.88).
narrative_ontology:measurement(stat_grid_05, state_killing_authority__retributive_desert, accessibility_collapse(organizational), 0, 0.72).
narrative_ontology:measurement(stat_grid_06, state_killing_authority__retributive_desert, accessibility_collapse(organizational), 60, 0.74).
narrative_ontology:measurement(stat_grid_07, state_killing_authority__retributive_desert, accessibility_collapse(structural), 0, 0.62).
narrative_ontology:measurement(stat_grid_08, state_killing_authority__retributive_desert, accessibility_collapse(structural), 60, 0.65).
narrative_ontology:measurement(stat_grid_09, state_killing_authority__retributive_desert, resistance(class), 0, 0.68).
narrative_ontology:measurement(stat_grid_10, state_killing_authority__retributive_desert, resistance(class), 60, 0.7).
narrative_ontology:measurement(stat_grid_11, state_killing_authority__retributive_desert, resistance(individual), 0, 0.15).
narrative_ontology:measurement(stat_grid_12, state_killing_authority__retributive_desert, resistance(individual), 60, 0.15).
narrative_ontology:measurement(stat_grid_13, state_killing_authority__retributive_desert, resistance(organizational), 0, 0.72).
narrative_ontology:measurement(stat_grid_14, state_killing_authority__retributive_desert, resistance(organizational), 60, 0.75).
narrative_ontology:measurement(stat_grid_15, state_killing_authority__retributive_desert, resistance(structural), 0, 0.62).
narrative_ontology:measurement(stat_grid_16, state_killing_authority__retributive_desert, resistance(structural), 60, 0.65).
narrative_ontology:measurement(stat_grid_17, state_killing_authority__retributive_desert, stakes_inflation(class), 0, 0.42).
narrative_ontology:measurement(stat_grid_18, state_killing_authority__retributive_desert, stakes_inflation(class), 60, 0.45).
narrative_ontology:measurement(stat_grid_19, state_killing_authority__retributive_desert, stakes_inflation(individual), 0, 0.98).
narrative_ontology:measurement(stat_grid_20, state_killing_authority__retributive_desert, stakes_inflation(individual), 60, 0.98).
narrative_ontology:measurement(stat_grid_21, state_killing_authority__retributive_desert, stakes_inflation(organizational), 0, 0.55).
narrative_ontology:measurement(stat_grid_22, state_killing_authority__retributive_desert, stakes_inflation(organizational), 60, 0.58).
narrative_ontology:measurement(stat_grid_23, state_killing_authority__retributive_desert, stakes_inflation(structural), 0, 0.38).
narrative_ontology:measurement(stat_grid_24, state_killing_authority__retributive_desert, stakes_inflation(structural), 60, 0.4).
narrative_ontology:measurement(stat_grid_25, state_killing_authority__retributive_desert, suppression(class), 0, 0.48).
narrative_ontology:measurement(stat_grid_26, state_killing_authority__retributive_desert, suppression(class), 60, 0.5).
narrative_ontology:measurement(stat_grid_27, state_killing_authority__retributive_desert, suppression(individual), 0, 0.85).
narrative_ontology:measurement(stat_grid_28, state_killing_authority__retributive_desert, suppression(individual), 60, 0.85).
narrative_ontology:measurement(stat_grid_29, state_killing_authority__retributive_desert, suppression(organizational), 0, 0.62).
narrative_ontology:measurement(stat_grid_30, state_killing_authority__retributive_desert, suppression(organizational), 60, 0.65).
narrative_ontology:measurement(stat_grid_31, state_killing_authority__retributive_desert, suppression(structural), 0, 0.52).
narrative_ontology:measurement(stat_grid_32, state_killing_authority__retributive_desert, suppression(structural), 60, 0.54).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__retributive_desert, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(state_killing_authority__retributive_desert, 0.12).
narrative_ontology:affects_constraint(state_killing_authority__retributive_desert, state_killing_authority__deterrence_instrument).
narrative_ontology:affects_constraint(state_killing_authority__retributive_desert, state_killing_authority__categorical_abolition).
narrative_ontology:affects_constraint(state_killing_authority__retributive_desert, constitutional_cruel_unusual_clause).
narrative_ontology:affects_constraint(state_killing_authority__retributive_desert, procedural_due_process_capital_trials).

% DUAL FORMULATION NOTE:
% This constraint is one of three structurally distinct readings of the kernel STATE_KILLING_AUTHORITY. The other readings are: (1) deterrence_instrument—capital punishment justified if and only if it prevents future murders; (2) categorical_abolition—state killing is categorically impermissible. Each reading has different ε, different beneficiary/victim sets, and different axioms. They are not contradictory observations of one constraint; they are three separate constraints instantiated from one contested kernel. All three are linked via network.affects_constraints to document the constraint family. The retributive reading grounds authority in proportionality principle (self-justifying); the deterrence reading grounds it in empirical outcomes; the abolition reading forecloses state killing authority entirely.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_killing_authority__retributive_desert, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
