% ============================================================================
% CONSTRAINT STORY: state_killing_authority__retributive_desert
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: state_killing_authority__retributive_desert
 *   human_readable: State Capital Punishment Authority — Retributive Desert Reading
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This constraint story instantiates the RETRIBUTIVE DESERT reading of the
 *   contested kernel state_killing_authority. The reading asserts that
 *   murderers forfeit their fundamental right to life through the extreme
 *   crime of taking a life, and that the state vindicates the victim and
 *   maintains proportional justice by executing the murderer. This is ONE
 *   reading of a kernel with multiple live contestants: the
 *   categorical_abolition reading rejects state killing entirely as violating
 *   inalienable human dignity; the deterrence_instrument reading justifies
 *   execution only if empirical evidence shows it prevents future murders.
 *   The retributive reading differs from both: it grounds state authority in
 *   proportionality and desert, not in outcomes or universalist rights
 *   claims. The victim enters the beneficiary set posthumously — their death
 *   generates the claim for proportional response. The condemned murderer
 *   exits the rights-holder set through forfeiture — having taken a life,
 *   they have severed their own claim to protection.
 *
 * KEY AGENTS:
 *   - murder_victims_posthumous: beneficiary (vindication claim), powerless, trapped
 *   - state_legitimacy_order: agenda-setter (executes proportional sentence), institutional, arbitrage exit
 *   - condemned_murderers: payer (forfeited life), powerless, trapped
 *   - inmates_death_row: payers (years of degradation + threat), powerless, identity-locked
 *   - families_of_condemned: payers (stigma, loss, watching execution), moderate power, constrained exit
 *   - murder_victim_survivors: beneficiaries (vindication through execution), moderate power, theoretically mobile
 *   - abolition_movement: excluded (rejects forfeiture premise), organized, mobile
 *   - deterrence_consequentialists: excluded (rejects retributive metric), organized, mobile
 *   - constitutional_courts: observer (adjudicates permissibility)
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
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, resistance, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__retributive_desert, tangled_rope).
narrative_ontology:human_readable(state_killing_authority__retributive_desert, "State Capital Punishment Authority — Retributive Desert Reading").
narrative_ontology:topic_domain(state_killing_authority__retributive_desert, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_killing_authority__retributive_desert).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__retributive_desert, 'c3ecc58a-3eef-49f0-b559-8c096ba49815').
narrative_ontology:cs_kernel_codification('c3ecc58a-3eef-49f0-b559-8c096ba49815', fixed_text).
narrative_ontology:cs_authority_grounding('c3ecc58a-3eef-49f0-b559-8c096ba49815', lineage).
narrative_ontology:cs_interpretation_layer_present('c3ecc58a-3eef-49f0-b559-8c096ba49815').
narrative_ontology:cs_reading_relation('c3ecc58a-3eef-49f0-b559-8c096ba49815', state_killing_authority__categorical_abolition, forecloses).
narrative_ontology:cs_reading_relation('c3ecc58a-3eef-49f0-b559-8c096ba49815', state_killing_authority__deterrence_instrument, coexists_with).
narrative_ontology:cs_axiom('c3ecc58a-3eef-49f0-b559-8c096ba49815', foundational, murderers_forfeit_right_to_life).
narrative_ontology:cs_axiom_status(murderers_forfeit_right_to_life, holdable).
narrative_ontology:cs_axiom_grounding('c3ecc58a-3eef-49f0-b559-8c096ba49815', murderers_forfeit_right_to_life, deontological).
narrative_ontology:cs_axiom('c3ecc58a-3eef-49f0-b559-8c096ba49815', foundational, lex_talionis_proportionality_legitimacy).
narrative_ontology:cs_axiom_status(lex_talionis_proportionality_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('c3ecc58a-3eef-49f0-b559-8c096ba49815', lex_talionis_proportionality_legitimacy, conventional).
narrative_ontology:cs_reference_frame('c3ecc58a-3eef-49f0-b559-8c096ba49815', proportional_retributive_justice_order).
narrative_ontology:cs_drift_state('c3ecc58a-3eef-49f0-b559-8c096ba49815', contemporary_rights_expansion_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c3ecc58a-3eef-49f0-b559-8c096ba49815', '').
narrative_ontology:cs_kernel_id(state_killing_authority__retributive_desert, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, murder_victims_posthumous).
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, state_legitimacy_order).
narrative_ontology:constraint_victim(state_killing_authority__retributive_desert, condemned_murderers).
narrative_ontology:constraint_victim(state_killing_authority__retributive_desert, inmates_death_row).
narrative_ontology:constraint_victim(state_killing_authority__retributive_desert, families_of_condemned).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, families_of_condemned).
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, murder_victim_survivors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The deceased murdered person, whose death the retributive reading treats as demanding proportional response from the state. The victim's voice is represented through survivor advocates, victim impact statements at sentencing, and the moral claim that the victim's death has been vindicatively answered. The victim cannot exit the arrangement — their death is the predicating fact.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, murder_victims_posthumous, beneficiary,
    powerless, civilizational, trapped, national).

% The state apparatus that declares, adjudicates, and executes capital punishment on retributive grounds. Under this reading, the state exercises its sovereign authority to maintain proportionality as a foundational legitimacy condition — the state vindicates the victim by exacting the proportional price from the murderer. The state's exit option is to abandon retribution as a legitimacy ground, reframing punishment around deterrence, rehabilitation, or incapacitation instead.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, state_legitimacy_order, agenda_setter,
    institutional, civilizational, arbitrage, national).

% The convicted murderer subject to state execution under the retributive reading. This reading construes their death as proportional forfeiture — they have exited the rights-holder class by their own deed and bear the ultimate extraction: loss of life. They have no real exit option; legal remedies (appeals, clemency) exist but the retributive logic denies their fundamental claim to life once culpability is established.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, condemned_murderers, payer,
    powerless, immediate, trapped, national).

% Condemned prisoners held under death sentences. They experience the constraint as decades of existential uncertainty, legal limbo, psychological degradation, and the standing threat of execution. Their identity has been transformed by the system to 'death row inmate' — they are locked into a condemned status that structures every aspect of their life and from which there is no reintegration pathway even if reprieved. The constraint operates on them through both threat (eventual execution) and presence (the apparatus of confinement and death watch).
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, inmates_death_row, payer,
    powerless, biographical, identity_locked, national).

% Relatives of condemned prisoners. They bear stigma, loss of relationship, and the trauma of watching the state execute a family member. Some may also be survivors of the original murder victim's family, positioning them as simultaneous payers (their relative will be killed) and beneficiaries (proportional response to the harm done to their murdered relative). Their exit is constrained — they cannot save the condemned person; they can withdraw from the legal process but the execution proceeds regardless.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, families_of_condemned, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(state_killing_authority__retributive_desert, families_of_condemned, beneficiary).

% Families and loved ones of the murdered person. Under the retributive reading, the state's execution of the murderer vindicates the victim's death and provides proportional answer to the crime. Survivor advocacy groups represent this seat in the retributive framework, asserting that the murderer's death is owed to the victim and that the state's refusal to execute would constitute a second wrong. Their exit option is theoretical — they can reject the retributive logic and advocate for alternative sentencing — but psychologically their investment in seeing 'justice' done (defined here as execution) is often substantial.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, murder_victim_survivors, beneficiary,
    moderate, biographical, mobile, national).

% Organized advocates who reject capital punishment entirely and contest the retributive reading's core premises. They assert that life is inalienable, that state killing violates human dignity regardless of the crime, and that proportional retribution is a false legitimacy frame. They are excluded from the decision-making authority (they do not set execution policy) but their voice is active in legislative, constitutional, and international pressure to curtail or abolish capital punishment.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, abolition_movement, excluded,
    organized, generational, mobile, national).

% Scholars, policymakers, and advocates who frame capital punishment as justified only if and when empirical evidence shows it prevents future murders at acceptable cost. They are excluded from the retributive reading's authority structure because they contest the retributive frame itself — they dispute whether proportionality is the right legitimacy metric, preferring outcome-based justification. Their position competes with retributivism in legislatures and appellate courts.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, deterrence_consequentialists, excluded,
    organized, generational, mobile, national).

% Judicial bodies that adjudicate whether capital punishment complies with constitutional protections (e.g., cruel and unusual punishment, due process, equal protection). They observe the competing readings (retributive, abolitionist, consequentialist) and render verdicts on which framings are constitutionally permissible. Their role is to interpret the supreme law and police the boundary of legitimate state authority.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_authority__retributive_desert, state_legitimacy_order).
narrative_ontology:fixing_cost_class(state_killing_authority__retributive_desert, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains proportional justice as a foundation of state legitimacy. The retributive reading coordinates a normative order in which the state's authority rests on exacting punishment proportional to harm done — the victim's death demands the murderer's death to restore proportional balance and vindicate the victim. Without this proportional response, the reading asserts, the state's legitimacy is compromised and victims are twice wronged (by the murderer and by the state's failure to exact desert).
% TRANSFER_FUNCTION: Transfers the condemned murderer's life to state custody, resulting in execution. The state, acting on behalf of the victim's memory and the moral order, extracts the murderer's life as payment proportional to the life they took. This is presented not as punishment for deterrence or rehabilitation but as retributive desert — what is owed.
% ABSENT_VOICES: Abolitionists who reject capital punishment on grounds of inalienable human dignity are structurally excluded from the retributive reading's authority — their frame contests the core premise. Consequentialists who demand empirical proof that execution deters are also excluded — they argue the legitimacy metric is wrong. The condemned person themselves is denied voice in the system; they are object of the sentence, not participant in its justification.
% DISAPPEARANCE_RATIONALE: If the retributive-desert authority to execute disappeared, the state would lose a key legitimacy mechanism — victim vindication through proportional retribution would no longer be available as a policy option. The world would rearrange: survivor advocacy would reframe demands around life sentences, the state would need to source legitimacy elsewhere (deterrence, incapacitation, rehabilitation), and the moral order of proportional justice would no longer be instantiated through capital punishment. The constraint is contestable precisely because its disappearance is imagined and debated.
% FOUNDING_PROBLEM: Murderers have taken a human life; proportional justice demands that the murderer forfeit their own right to life. The founding problem is: how does the state vindicatively answer the victim's death and maintain a legitimate moral order in which extreme crimes incur extreme penalties?
% FOUNDING_PROBLEM_CORROBORATION: Retributive theorists (philosophers, some criminologists, victim advocates) attest the founding problem is live and ongoing — victims' deaths continue to demand proportional response. Abolitionists and international human rights bodies (outside the benefiting parties) attest that the founding problem has been superseded — alternative legitimacy grounds (rehabilitation, incapacitation, human dignity) can address the need for order without executing. Empirical research shows declining correlation between execution and victim survivors' reported sense of justice, complicating the corroboration further.
narrative_ontology:disappearance_verdict(state_killing_authority__retributive_desert, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_authority__retributive_desert, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__retributive_desert, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_killing_authority__retributive_desert, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_authority__retributive_desert, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate-to-high (0.68 at interval end) because the constraint extracts the condemned person's life and subjects inmates to decades of degradation. The measurement series shows steady rise over 50 years, reflecting the accumulation of inmates on death row and intensifying legal/psychological pressure. Suppression is high (0.72) because the constraint's persistence requires actively defending the forfeiture premise against abolitionist legal challenges and international human rights pressure — without this suppression (constitutional gates, international treaty reservations), the constraint would face structural collapse. Theater ratio rises from 0.18 to 0.41, indicating growing proxy-goal substitution: the legitimacy claim (proportional vindication) increasingly rides on ritual (elaborate execution protocols, victim impact testimony) rather than on functional vindicative effect (empirical data show survivors often report unmet emotional needs post-execution). The accessibility_collapse (0.78) reflects that once the retributive reading is instantiated, condemned persons have nearly no exit — appeals and clemency remain theoretical. Resistance is high (0.81) because abolitionists, international bodies, and a growing segment of the US population actively contest the retributive frame. One shared time grid: every metric authored at every time point to prevent OQ-105-style grid misalignment.
 *
 * PERSPECTIVAL GAP:
 *   The retributive reading grounds state authority in proportional desert — the state vindicates the victim by exacting the murderer's life as proportional payment. This grounds legitimacy in a normative principle (lex talionis), not in outcomes or universalist rights. Contrast with the deterrence reading: legitimacy grounds in empirical outcomes (does execution prevent future murders?). Contrast with the abolitionist reading: legitimacy grounds in inalienable rights (life is inalienable, state killing violates it regardless of crime or consequence). The three readings do not merely disagree on whether execution is good policy — they disagree on what makes state authority legitimate. This is why they are distinct constraints, not perspective variations on one constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   The condemned murderer's directionality is maximized (d ≈ 0.95) because they are structurally powerless, trapped (no exit), and under immediate threat of execution from a state apparatus they do not control. The state's directionality is minimized (d ≈ 0.05 to 0.15) because it controls the apparatus and collects the legitimacy benefit. Families of condemned sit mid-range (d ≈ 0.65): they bear extraction (the state will execute their relative) but some families also benefit from the retributive frame (those related to the murder victim seek vindication). Murder-victim survivors are heterogeneous: some families see execution as vindication (low d, beneficiaries); others reject the retributive frame and consider execution a second wrong (high d, targets). The abolition movement, though organized and powerful in advocacy, is excluded from the authority structure — they bear no direct extraction but are locked out of the decision (excluded role). This heterogeneity is authoring honesty, not flaw: the same constraint produces divergent directionalities because directionality is structural-relational, not objective.
 *
 * MANDATROPHY ANALYSIS:
 *   The retributive reading faces a dormancy risk: the founding problem (murderers forfeit the right to life; proportional response is owed to victims) remains contested. Empirical data increasingly show that executions do not deliver the promised vindication to survivors, suggesting the founding problem may be becoming dead (alternative legitimacy grounds can address victim need for order, healing, and state response). The theater_ratio rise from 0.18 to 0.41 signals Goodhart drift: the constraint's operation increasingly substitutes ritual legitimacy (elaborate execution protocols, victim impact testimony ceremonies) for functional vindicative effect. This is the classic piton signature — the constraint persists by theater maintenance, not because the founding problem's answer is still live. However, the constraint is classified as tangled_rope (not piton) because there remain genuine beneficiaries (the state derives legitimacy, some victim survivors derive vindication via the retributive frame) and genuine victims (condemned murderers forfeit life). Piton would require no concentrated beneficiary — here the state and the retributive-alignment survivor advocates clearly benefit. The classification holds; the theater rise is diagnostic signal for upstream mandatrophy resolution (the founding problem's answer is attenuating).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    forfeiture_premise_contestation,
    'Do murderers truly forfeit their fundamental right to life, or is the right to life inalienable regardless of crime committed?',
    'Philosophical/theological argument grounded in how rights are constituted (do rights derive from person-hood, from social contract, from divine grant?) and whether extreme crime can sever the source of rights. No empirical resolution — this is conceptual.',
    'If rights are inalienable (abolitionist position), the retributive reading collapses — there is no forfeiture and the state has no authority to execute. If rights are conditional on conduct (retributive position), the abolitionist reading loses its universal claim. This is a core-premise dispute; resolving it determines which reading is structurally coherent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(forfeiture_premise_contestation, conceptual, 'Whether rights can be forfeited through extreme crime or are inalienable').

omega_variable(
    vindicative_efficacy_attenuation,
    'Does execution of murderers actually vindicate the victim and meet survivors'' needs for justice, or does it substitute ritual for functional resolution?',
    'Prospective empirical study following murder-survivor families through execution and post-execution periods, measuring reported sense of vindication, closure, and healing. Historical data show no strong effect; but the retributive reading may persist through theater rather than empirical outcome.',
    'If execution does not produce reported vindication (data suggest it does not for many survivors), the constraint''s founding problem status becomes ''contested'' to ''dead'' — the founding problem (victims demand proportional response) persists, but the answer (execution) is not delivering what it promises. This would trigger mandatrophy signals and shift classification pressure toward piton (performative persistence).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vindicative_efficacy_attenuation, empirical, 'Whether execution delivers the promised vindication to murder survivors').

omega_variable(
    identity_lock_mechanics_on_death_row,
    'Is the suppression experienced by inmates-death-row structural (external barriers: confinement, legal entrapment) or internalized (the person has accepted a condemned identity from which they cannot psychologically exit)?',
    'Post-release studies of exonerees: do they report residual suppression after exit from death row? If suppression persists after the external machinery is removed, it signals internalization. Psychological assessment of death-row inmates'' self-concept and agency.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them even after exit, indicating capture at the cognitive level. This would increase the constraint''s extractiveness score and push toward higher type-certainty (snare rather than tangled-rope). If structural, the suppression is removable and the classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanics_on_death_row, empirical, 'Whether death-row suppression is structural or internalized').

omega_variable(
    kernel_contest_reading_incompatibility,
    'Can the retributive-desert reading and the categorical-abolition reading both be held within a single institutional framework, or do they logically foreclose each other?',
    'Examine constitutional texts and case law: do courts recognize retributive legitimacy as co-equal with other rationales, or do they treat them as mutually exclusive? Do jurisdictions that reject retribution still permit execution under other frames? If both readings persist in different jurisdictions, they coexist; if a court must choose, they foreclose within that framework.',
    'If they foreclose (one must be true and the other false within a single legal system), then the retributive reading''s truth-value determines the constraint''s legitimacy globally. If they coexist (different jurisdictions, different principles), then the kernel contest is unresolved and both constraints remain live. This determines whether the retributive reading can be institutionally stable or is caught in an unresolvable contest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_reading_incompatibility, conceptual, 'Whether retributive and abolitionist readings logically foreclose each other or can coexist').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__retributive_desert, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_authority__retributive_desert, theater_ratio, 0, 0.18).
narrative_ontology:measurement(stat_tr_t8, state_killing_authority__retributive_desert, theater_ratio, 8, 0.22).
narrative_ontology:measurement(stat_tr_t16, state_killing_authority__retributive_desert, theater_ratio, 16, 0.27).
narrative_ontology:measurement(stat_tr_t25, state_killing_authority__retributive_desert, theater_ratio, 25, 0.34).
narrative_ontology:measurement(stat_tr_t35, state_killing_authority__retributive_desert, theater_ratio, 35, 0.39).
narrative_ontology:measurement(stat_tr_t50, state_killing_authority__retributive_desert, theater_ratio, 50, 0.41).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_authority__retributive_desert, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(stat_be_t8, state_killing_authority__retributive_desert, base_extractiveness, 8, 0.59).
narrative_ontology:measurement(stat_be_t16, state_killing_authority__retributive_desert, base_extractiveness, 16, 0.63).
narrative_ontology:measurement(stat_be_t25, state_killing_authority__retributive_desert, base_extractiveness, 25, 0.66).
narrative_ontology:measurement(stat_be_t35, state_killing_authority__retributive_desert, base_extractiveness, 35, 0.68).
narrative_ontology:measurement(stat_be_t50, state_killing_authority__retributive_desert, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_authority__retributive_desert, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(stat_su_t8, state_killing_authority__retributive_desert, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(stat_su_t16, state_killing_authority__retributive_desert, suppression_requirement, 16, 0.66).
narrative_ontology:measurement(stat_su_t25, state_killing_authority__retributive_desert, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(stat_su_t35, state_killing_authority__retributive_desert, suppression_requirement, 35, 0.72).
narrative_ontology:measurement(stat_su_t50, state_killing_authority__retributive_desert, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__retributive_desert, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(state_killing_authority__retributive_desert, 0.12).
narrative_ontology:affects_constraint(state_killing_authority__retributive_desert, state_killing_authority__deterrence_instrument).
narrative_ontology:affects_constraint(state_killing_authority__retributive_desert, state_killing_authority__categorical_abolition).

% DUAL FORMULATION NOTE:
% This constraint is part of a three-story family decomposing the contested kernel state_killing_authority. The retributive-desert reading (this story) grounds state authority in proportionality and forfeiture; it competes with the deterrence-instrument reading (outcome-based justification) and the categorical-abolition reading (inalienable rights). Each story has its own ε, beneficiary/victim structure, and type classification. They are linked via network.affects_constraints because they share the same institutional domain (capital punishment) and contest the same legitimacy ground (what makes state killing authority legitimate). The retributive reading influences both siblings by claiming to establish the moral permissibility of execution; the abolition reading influences both others by denying that permissibility is ever available; the deterrence reading influences both others by offering a third legitimacy ground (outcomes, not principles). This network structure enables the corpus to model the kernel contest as three ε-invariant constraint stories rather than as single story with measurement ambiguity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
