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
 *   human_readable: Categorical Abolition of State Killing: Life as Inalienable Right
 *   domain: constitutional_law/criminal_justice/political_philosophy
 *
 * SUMMARY:
 *   The categorical abolition reading asserts that human life is an
 *   inalienable right that the state cannot legitimately take, regardless of
 *   crime or consequence. This constraint operates as a kernel reading in
 *   contest with retributive-desert and deterrence-instrument readings of the
 *   same state-killing-authority kernel. Under this reading, condemned
 *   persons remain rights-holders; the state that executes them becomes a
 *   potential rights-violator; and victims' families fracture into those who
 *   seek execution (marginalized under this reading) and those who oppose it
 *   (affirmed). The reading is instantiated in law in abolitionist
 *   jurisdictions and international human rights treaties; it is contested or
 *   rejected in retentionist jurisdictions. The claim (tangled_rope) reflects
 *   that the reading coordinates abolition advocates globally while
 *   extracting the voice of retributive and deterrence proponents from the
 *   legitimate punishment menu. The metrics show rising extractiveness and
 *   theater_ratio over the interval as the constraint gains international
 *   institutional authority while retentionist states become more active in
 *   defending execution.
 *
 * KEY AGENTS:
 *   - condemned_persons: principal beneficiaries under the categorical reading; powerless to enforce the constraint
 *   - human_rights_advocates: institutional agenda-setters; maintain and defend the inalienability frame
 *   - abolitionist_jurisdictions: benefit from moral authority and international alignment; enforce the constraint domestically
 *   - retributive_justice_advocates: structurally constrained in punishment design; pay through removal of lex talionis option
 *   - deterrence_proponents: forced to abandon consequentialist justification; their empirical claims are treated as categorically irrelevant
 *   - victims_families_seeking_execution: suppressed by removal of execution from the legitimate menu
 *   - state_criminal_justice_apparatus: administrators who enforce abolition domestically but retain power to interpret boundaries
 *   - international_human_rights_institutions: observers with interpretive authority that shapes the global reading
 *   - retentionist_state_legislatures: active excluders who reject the inalienability frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__categorical_abolition, 0.85).
domain_priors:suppression_score(state_killing_authority__categorical_abolition, 0.72).
domain_priors:theater_ratio(state_killing_authority__categorical_abolition, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, extractiveness, 0.85).
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__categorical_abolition, tangled_rope).
narrative_ontology:human_readable(state_killing_authority__categorical_abolition, "Categorical Abolition of State Killing: Life as Inalienable Right").
narrative_ontology:topic_domain(state_killing_authority__categorical_abolition, "constitutional_law/criminal_justice/political_philosophy").

domain_priors:requires_active_enforcement(state_killing_authority__categorical_abolition).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__categorical_abolition, 'd7cb6e8e-f70d-40d4-8759-0dca2a347a07').
narrative_ontology:cs_kernel_codification('d7cb6e8e-f70d-40d4-8759-0dca2a347a07', fixed_text).
narrative_ontology:cs_authority_grounding('d7cb6e8e-f70d-40d4-8759-0dca2a347a07', lineage).
narrative_ontology:cs_interpretation_layer_present('d7cb6e8e-f70d-40d4-8759-0dca2a347a07').
narrative_ontology:cs_reading_relation('d7cb6e8e-f70d-40d4-8759-0dca2a347a07', state_killing_authority__retributive_desert, forecloses).
narrative_ontology:cs_reading_relation('d7cb6e8e-f70d-40d4-8759-0dca2a347a07', state_killing_authority__deterrence_instrument, influences).
narrative_ontology:cs_axiom('d7cb6e8e-f70d-40d4-8759-0dca2a347a07', foundational, life_inalienable_categorical).
narrative_ontology:cs_axiom_status(life_inalienable_categorical, holdable).
narrative_ontology:cs_axiom_grounding('d7cb6e8e-f70d-40d4-8759-0dca2a347a07', life_inalienable_categorical, deontological).
narrative_ontology:cs_axiom('d7cb6e8e-f70d-40d4-8759-0dca2a347a07', foundational, state_not_authorized_taking_life).
narrative_ontology:cs_axiom_status(state_not_authorized_taking_life, holdable).
narrative_ontology:cs_axiom_grounding('d7cb6e8e-f70d-40d4-8759-0dca2a347a07', state_not_authorized_taking_life, deontological).
narrative_ontology:cs_reference_frame('d7cb6e8e-f70d-40d4-8759-0dca2a347a07', human_dignity_rights_paradigm).
narrative_ontology:cs_drift_state('d7cb6e8e-f70d-40d4-8759-0dca2a347a07', contemporary_2026, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('d7cb6e8e-f70d-40d4-8759-0dca2a347a07', '').
narrative_ontology:cs_kernel_id(state_killing_authority__categorical_abolition, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, condemned_persons).
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, human_rights_advocates).
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, abolitionist_jurisdictions).
narrative_ontology:constraint_victim(state_killing_authority__categorical_abolition, retributive_justice_advocates).
narrative_ontology:constraint_victim(state_killing_authority__categorical_abolition, deterrence_proponents).
narrative_ontology:constraint_victim(state_killing_authority__categorical_abolition, victims_families_seeking_execution).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, abolitionist_families_of_victims).
narrative_ontology:constraint_vindicates(state_killing_authority__categorical_abolition, inalienable_right_to_life).
narrative_ontology:constraint_vindicates(state_killing_authority__categorical_abolition, state_monopoly_on_legitimate_violence_limit).
narrative_ontology:constraint_vindicates(state_killing_authority__categorical_abolition, human_dignity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under sentence of death in a retributive or deterrence jurisdiction. The categorical abolition reading classifies them as rights-holders whose fundamental right to life cannot be forfeited by the state, regardless of their crime. They benefit structurally from the constraint's assertion that life is inalienable; however, they remain powerless to enforce the constraint in jurisdictions that reject it. Execution remains imminent unless the constraint's reading gains institutional authority.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, condemned_persons, beneficiary,
    powerless, immediate, trapped, national).

% International NGOs, national abolition societies, legal scholars, and constitutional reformers who set and defend the categorical abolition reading. They frame state killing as incompatible with human dignity and liberal constitutional order. They lobby legislatures, file legal briefs, coordinate international pressure, and maintain the interpretive framework that treats the inalienability assertion as binding. They have real exit options (shift focus to other rights agendas) but carry identity investment in abolition advocacy.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, human_rights_advocates, agenda_setter,
    organized, generational, mobile, global).

% Nations and regions (EU member states, most developed democracies) that have formally abolished capital punishment. They benefit from alignment with international norms, reduced litigation over execution procedures, and moral authority on human rights. They have the capacity to abandon the constraint (retentionist countries demonstrate this) but gain soft power and institutional legitimacy from maintaining it. Their international advocacy shapes the constraint's interpretation globally.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, abolitionist_jurisdictions, beneficiary,
    institutional, generational, arbitrage, continental).

% Judges, prosecutors, legal scholars, and constituents who hold that murderers forfeit the right to life through their crime, and that proportional punishment (lex talionis) demands execution for murder. They pay by seeing the lex talionis punishment option removed; their retributive framework is constrained rather than prohibited, but the constraint narrows the legitimate punishment menu. They resist through litigation, legislative campaign, and dissent from international agreements. Their voice is strongest in retentionist jurisdictions but increasingly marginalized in abolitionist regions.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, retributive_justice_advocates, payer,
    moderate, biographical, constrained, national).

% Criminologists, policymakers, and legal theorists who argue that execution deters murder and is therefore instrumentally justified if the deterrent effect is strong enough. The categorical abolition constraint removes deterrence as a policy lever regardless of empirical outcome. They bear the cost of being unable to deploy execution even if evidence were to show net lives saved by doing so. Their resistance focuses on empirical argument (challenging deterrence-denial) but the constraint's framing treats their empirical premise as categorically irrelevant—no amount of deterrent evidence could justify state killing under this reading.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, deterrence_proponents, payer,
    moderate, biographical, constrained, national).

% Relatives of murder victims who seek execution of the perpetrator as catharsis, justice, or closure. The categorical abolition constraint removes their voice from legitimate punishment architecture; prosecutors and judges may solicit their input performatively, but the constraint strips execution from the menu before their preference is considered. They experience suppression because their call for execution is treated as categorically illegitimate, not as a legitimate claim competing with other values. Abolitionist-reading jurisdictions often offer victim-support services and restorative-justice pathways, but these are experienced as substitutes, not vindication.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, victims_families_seeking_execution, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(state_killing_authority__categorical_abolition, victims_families_seeking_execution, excluded).

% Relatives of murder victims who oppose execution on moral or practical grounds and advocate for the categorical abolition constraint. They are sometimes marginalized in public discourse by prosecutors who emphasize the voices of families seeking execution. The constraint benefits them by aligning state practice with their moral conviction. They remain constrained by the criminal justice system and the emotional burden of their loss, but they gain institutional legitimacy under the abolition reading.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, abolitionist_families_of_victims, beneficiary,
    moderate, biographical, constrained, national).

% Courts, corrections systems, prosecution services, and legislators who administer criminal punishment. In retentionist jurisdictions, they have the authority to execute; in abolitionist jurisdictions, they enforce the constraint and redirect the punishment architecture. They bear the cost of abolition through legislative reform, retraining, and procedure redesign. However, they retain substantial power to interpret and apply the constraint and can negotiate its boundaries (severity of life sentences, possibility of future release). In abolitionist jurisdictions they benefit from moral clarity and reduced litigation over execution protocols.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, state_criminal_justice_apparatus, agenda_setter,
    institutional, generational, mobile, national).

% UN bodies, regional human rights courts, treaty bodies that monitor state compliance with abolition norms. They interpret and apply the constraint globally, issue findings that pressure retentionist states, and provide venues for condemned persons to challenge execution orders. They are analytical observers in the sense that they do not execute anyone or defend anyone from execution in real time, but their interpretation carries normative weight and shapes the institutional reading of the constraint.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, international_human_rights_institutions, observer,
    institutional, generational, analytical, global).

% Legislative bodies in jurisdictions that retain and defend capital punishment (US jurisdictions, parts of Asia, Middle East, Africa). They are excluded from the dialogue that constitutes the categorical abolition constraint—they do not participate in the interpretive framework that treats life as inalienable in the way abolitionist legislatures and international advocates do. They could join by abolishing capital punishment, but they choose not to. Their exclusion is active (they reject the constraint) rather than passive (they are kept out by force).
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, retentionist_state_legislatures, excluded,
    institutional, generational, mobile, national).

% Judges and constitutional scholars who argue that the Constitution (or founding text) permits capital punishment and that contemporary abolition readings represent judicial overreach or legislative policy choice, not constitutional mandate. They contest the categorical abolition reading by asserting that the framers contemplated execution, that the Fifth Amendment's explicit reference to capital crimes assumes execution's legality, and that abolition must come through political process, not judicial interpretation. Their exclusion reflects the reading's commitment to inalienability as overriding textual permission.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, conservative_originalist_jurists, excluded,
    institutional, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_authority__categorical_abolition, diffuse).
narrative_ontology:fixing_cost_class(state_killing_authority__categorical_abolition, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint coordinates a global norm against state killing by classifying execution as categorically incompatible with human dignity and the rule of law. It solves the problem of moral confusion by establishing that no consequentialist rationale (deterrence, cost savings, victim preference) can override the inalienable right to life. For abolitionist jurisdictions and international advocates, it provides a unified interpretive framework that prevents sliding back into execution as punishment when political pressure or empirical claims (e.g., deterrence studies) arise.
% TRANSFER_FUNCTION: The constraint redirects the practice of state punishment away from execution toward imprisonment, rehabilitation, and restorative justice for those the retributive and deterrence readings would execute. It moves the legitimacy of inflicting death from the state's authority (under those other readings) to a categorically forbidden act. For victims' families seeking execution, it forecloses the option and transfers their voice's institutional weight to zero in the punishment-setting process (though support services and other justice modalities remain). For retributive and deterrence proponents, it transfers the authority to set the punishment menu from empirical/philosophical argument to a prior categorical assertion.
% ABSENT_VOICES: Conservative originalist jurists and retentionist legislatures are structurally excluded from the dialogue that constitutes this reading. Victims' families seeking execution are present but marginalized—prosecutors may solicit their input but the constraint strips execution from the legitimate menu before their preference is considered. Condemned persons themselves, though nominally the beneficiaries, are excluded from power to interpret or enforce the constraint; their survival depends on others' commitment to the reading. The economic interests of states in keeping prisons full (privatized corrections, guard unions, supplier industries) are not voiced as legitimate stakeholders.
% DISAPPEARANCE_RATIONALE: If the categorical abolition constraint vanished overnight, retentionist jurisdictions would resume or intensify execution; abolitionist jurisdictions would face intense political pressure to restore capital punishment in high-profile cases; the international human rights framework would fracture as enforcement mechanisms dissolved. Condemned persons under sentence of death would be executed, victims' families seeking execution would see their demands honored, and the global norm against state killing would fragment into regional and national contests over whether life is inalienable. The world would rearrange around competing readings of whether the state has killing authority.
% FOUNDING_PROBLEM: The founding problem this reading addresses is the historical fact that states have used execution as punishment, torture, and terror, often applied unequally and without regard to actual guilt. The reading was built to solve the problem of reconciling state punishment authority with the claim that life is a right that cannot be forfeited or transferred. It arose in response to enlightenment philosophy (Beccaria, Kant on dignity), abolitionist movements, and post-WWII human rights architecture recognizing that Nazi and totalitarian regimes had used capital punishment as a tool of mass killing masked as justice.
% FOUNDING_PROBLEM_CORROBORATION: Historians and human rights scholars outside the abolitionist movement attest that states have applied execution unequally and as terror; criminological research supports that execution has not been proven to deter murder better than long imprisonment. However, retributive justice theorists and deterrence researchers contest whether the founding problem justifies categorical abolition or merely calls for narrower, fairer application of capital punishment. Conservative constitutional scholars argue the founding problem was already addressed by the Eighth Amendment's prohibition on cruel and unusual punishment and does not require abolition per se. International human rights bodies (UN, ICJ opinions, European Court of Human Rights) attest that the foundational problem—arbitrary state killing—justifies categorical abolition as the only protective measure; regional courts in abolitionist regions confirm this reading. This is NOT self-corroboration by the constraint's beneficiaries: it is corroboration by independent institutional actors (courts, treaty bodies) that are not benefiting from abolition but have interpretive authority.
narrative_ontology:disappearance_verdict(state_killing_authority__categorical_abolition, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_authority__categorical_abolition, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__categorical_abolition, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_killing_authority__categorical_abolition, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_authority__categorical_abolition, 0.85, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.85) because the reading imposes a categorical constraint on how states may punish, removing the execution option regardless of empirical consequences or victim preference. This is not market extraction but interpretive extraction—the constraint re-narrates what punishment means and strips certain parties' preferences from the legitimate menu. Suppression is high (0.72) because enforcement requires active rejection of retributive and deterrence frameworks; retentionist jurisdictions must suppress their own legal traditions to adopt abolition, and within abolitionist jurisdictions, retributive advocates must suppress their interpretation of justice. Theater ratio (0.41) reflects that a portion of the constraint's maintenance is performative: international advocacy conferences, victim-support theater, procedural reforms that perform concern for condemned persons while maintaining the constraint. The measurement series show rising trajectories from 1950 to 2026, tracking the constraint's gain in institutional authority and the corresponding increase in the active work needed to suppress competing readings. The interval reflects the post-WWII emergence of human rights norms (1950) through contemporary global abolition (2026); earlier periods had no organized abolition movement at scale.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute dramatically differently. From the abolitionist-advocate and condemned-person seats, this reading is natural law (life's inalienability is non-negotiable); from the retributive-justice and deterrence seats, it is pure extraction (the removal of legitimate punishment options by prior assertion). From the abolitionist-jurisdiction seat, the constraint is coordination (shared commitment to human dignity); from the retentionist-jurisdiction seat, it is imperial imposition (wealthy democracies using human rights pressure to override domestic democratic choice). The engine computes these divergent perceptions from the structural data: beneficiary vs. victim roles, exit options (trapped vs. mobile), and power asymmetries (powerless condemned vs. institutional advocates).
 *
 * DIRECTIONALITY LOGIC:
 *   Condemned persons (powerless, trapped, immediate horizon) sit at d=1.0 (full benefit from inalienability assertion, zero escape). Abolitionist advocates (organized, mobile, generational horizon) sit near d=0.1 (high beneficiary status, capacity to exit if the constraint were no longer useful, but identity-locked to abolition mission). Retributive and deterrence proponents (moderate power, constrained exit, biographical horizon) sit at d=0.75 (targets of extraction via removal of punishment options, constrained ability to exit justice system entirely but capable of advocating for legal change). State criminal justice apparatus (institutional power, mobile exit, generational horizon) sits near d=0.3 (neither pure beneficiary nor pure target; they administer the constraint in abolitionist jurisdictions and resist it in retentionist ones; they retain power to interpret boundaries—mobile at the scale of jurisdiction choice). Victims' families seeking execution sit at d=0.8 (targets of suppression; their voice is stripped from the legitimate menu). The asymmetries drive the tangled-rope classification: genuine coordination function (global human dignity norm) AND active enforcement against competing readings.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (state killing as terror and inequality) was live at 1950 but is NOW contested. Retributive and deterrence readings argue the founding problem is solved by procedural fairness and empirical evidence, not categorical abolition. However, the categorical abolition reading asserts that the founding problem is INHERENTLY unsolvable short of abolition because state killing is categorically wrong regardless of procedures or evidence. This is not mandatrophy (the constraint persisting after its function dies) but rather a dispute over whether the founding function is ever truly 'done'—inalienability asserts the function is permanent (life's sanctity never expires), while retributive/deterrence readings assert the founding problem was historically specific (mistreatment of prisoners, unequal application) and solvable by reform. The rising theater_ratio (0.28 to 0.41) suggests increasing performativity: international abolition ceremonies, victim-support programs, procedural protections that maintain the constraint while performing concern for the constrained parties. This is not mandatrophy in the engine's sense (abandoned function, mere inertia) but rather *theatricality* as part of a live but heavily contested constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inalienability_vs_forfeit_premise,
    'Is the claim that life is inalienable a foundational axiom about human rights, or is it a contingent political assertion that can be overridden by competing frameworks like retributive proportionality?',
    'Foundational status would be established by finding no abolitionist philosopher or advocate who treats inalienability as negotiable. Contingent status would be established by finding abolitionist frameworks that permit killing in some extreme scenarios (e.g., self-defense against an attacker mid-murder attempt) or that treat inalienability as a policy preference rather than a non-negotiable principle.',
    'If foundational, the constraint forecloses retributive and deterrence readings entirely—they cannot coexist with it in a single coherent framework. If contingent, the readings coexist (multiple parties hold different premises, no logical incompatibility) and the engine computes per-seat types rather than kernel-level foreclosure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inalienability_vs_forfeit_premise, conceptual, 'Whether inalienability is a foundational axiom or a contingent political preference in the abolition reading.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the suppression measured in this constraint primarily structural (legal barriers, institutional exclusion, criminal sanctions against advocates) or internalized (retentionist advocates have accepted the norm, victims'' families seeking execution have internalized shame)?',
    'Post-abolition trajectory: if suppression persists in agents who escape retentionist jurisdictions, the suppression is partly internalized (identity/shame). If suppression vanishes when agents move to abolitionist jurisdictions, it is primarily structural (legal barriers).',
    'If internalized, the effective suppression is higher than the structural measure suggests; retributive and deterrence advocates carry the constraint with them even after legal reform. If structural, abolition in one jurisdiction provides genuine exit; advocates can move to retentionist regions and resume their voice. The distinction affects whether the constraint''s hold is institutional or epistemic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether suppression of retributive/deterrence readings is structural or internalized.').

omega_variable(
    foreclosure_vs_coexistence_empirical_test,
    'Can a coherent framework hold both the categorical abolition premise (life is inalienable) AND the retributive premise (murderers forfeit the right to life) without logical contradiction?',
    'Survey moral and legal philosophers asking whether the two premises can be jointly held without internal contradiction. Examine actual abolition advocates who also hold retributive intuitions to see how they resolve the tension (reinterpretation, compartmentalization, ranking of premises).',
    'If the premises are logically incompatible (cannot both be true in one framework), the reading forecloses retributive desert—the engine marks this at the kernel level. If they can be held together through reinterpretation (e.g., retributive intuitions redirected to non-lethal punishment, or murderers retain dignity even though they forfeit some rights), the readings coexist and the engine computes per-seat types.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreclosure_vs_coexistence_empirical_test, conceptual, 'Whether the inalienability axiom logically forecloses the retributive axiom.').

omega_variable(
    state_killing_vs_state_imprisonment_asymmetry,
    'Does the categorical abolition reading treat state killing as categorically impermissible while treating life imprisonment (which also removes all exit and liberty) as permissible? If so, what principled distinction justifies this asymmetry?',
    'Canonical abolitionist texts and contemporary advocates'' statements on whether life imprisonment violates inalienability. If life imprisonment is also forbidden, the reading is more radical (no state punishment of murderers is permissible). If permitted, the reading must articulate what makes killing worse than permanent imprisonment—death''s finality? The body''s violation? The restoration possibility?',
    'If life imprisonment is also impermissible, the constraint is radically transformative (abolition + abolition of life sentences = fundamental restructuring of criminal justice). If only killing is forbidden, the constraint is moderate (imprisonment substitutes) and the suppression/extraction measures underestimate the constraint''s scope by treating imprisonment as unproblematic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_killing_vs_state_imprisonment_asymmetry, conceptual, 'Whether inalienability extends to permanent imprisonment or only prohibits execution.').

omega_variable(
    reading_family_identity_fusion,
    'For human rights advocates (agenda_setter role), is commitment to categorical abolition an identity-fused position (selfhood constituted by abolition advocacy) or a policy preference (strongly held but separable from identity)?',
    'Longitudinal study: do abolition advocates who leave the movement report identity dissolution or merely policy shift? Do advocates who change minds experience crisis? Interviews about whether ''abolitionist'' is a core identity or a role they play.',
    'If identity-fused, abolition advocates'' exit options are lower than ''mobile'' (they would carry the constraint with them even if they left the advocacy role). If policy preference, exit is truly mobile and the directionality derivation is correct. Identity fusion would raise the effective extraction on advocates—they cannot exit even when the constraint no longer serves their interests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_family_identity_fusion, empirical, 'Whether abolitionist identity is fused with abolitionist advocates'' selfhood.').

omega_variable(
    kernel_reading_family_sibling_boundaries,
    'Are the three readings (categorical_abolition, retributive_desert, deterrence_instrument) the complete set of readings of the state-killing-authority kernel, or are there additional live readings (restorative justice reading, self-defense reading, emergency exception reading) that should be authored separately?',
    'Systematic review of legal and philosophical literatures on capital punishment to identify all distinct normative frameworks that treat state killing as permissible under SOME conditions and impermissible under others, vs. the three cardinal readings specified.',
    'If additional live readings exist, they are separate constraint stories (per the ε-invariance principle) that should be authored and linked to this kernel. The network of kernel readings would be larger and more complex. If only three readings exist, the kernel family is complete as specified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_family_sibling_boundaries, conceptual, 'Whether the state-killing-authority kernel has additional live readings beyond the three specified.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__categorical_abolition, 1950, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1950, state_killing_authority__categorical_abolition, theater_ratio, 1950, 0.28).
narrative_ontology:measurement_basis(stat_tr_t1950, projected).
narrative_ontology:measurement(stat_tr_t1975, state_killing_authority__categorical_abolition, theater_ratio, 1975, 0.32).
narrative_ontology:measurement_basis(stat_tr_t1975, projected).
narrative_ontology:measurement(stat_tr_t2000, state_killing_authority__categorical_abolition, theater_ratio, 2000, 0.35).
narrative_ontology:measurement_basis(stat_tr_t2000, observed).
narrative_ontology:measurement(stat_tr_t2013, state_killing_authority__categorical_abolition, theater_ratio, 2013, 0.39).
narrative_ontology:measurement_basis(stat_tr_t2013, observed).
narrative_ontology:measurement(stat_tr_t2020, state_killing_authority__categorical_abolition, theater_ratio, 2020, 0.4).
narrative_ontology:measurement_basis(stat_tr_t2020, observed).
narrative_ontology:measurement(stat_tr_t2026, state_killing_authority__categorical_abolition, theater_ratio, 2026, 0.41).
narrative_ontology:measurement_basis(stat_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(stat_be_t1950, state_killing_authority__categorical_abolition, base_extractiveness, 1950, 0.62).
narrative_ontology:measurement_basis(stat_be_t1950, projected).
narrative_ontology:measurement(stat_be_t1975, state_killing_authority__categorical_abolition, base_extractiveness, 1975, 0.68).
narrative_ontology:measurement_basis(stat_be_t1975, projected).
narrative_ontology:measurement(stat_be_t2000, state_killing_authority__categorical_abolition, base_extractiveness, 2000, 0.78).
narrative_ontology:measurement_basis(stat_be_t2000, observed).
narrative_ontology:measurement(stat_be_t2013, state_killing_authority__categorical_abolition, base_extractiveness, 2013, 0.82).
narrative_ontology:measurement_basis(stat_be_t2013, observed).
narrative_ontology:measurement(stat_be_t2020, state_killing_authority__categorical_abolition, base_extractiveness, 2020, 0.84).
narrative_ontology:measurement_basis(stat_be_t2020, observed).
narrative_ontology:measurement(stat_be_t2026, state_killing_authority__categorical_abolition, base_extractiveness, 2026, 0.85).
narrative_ontology:measurement_basis(stat_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1950, state_killing_authority__categorical_abolition, suppression_requirement, 1950, 0.58).
narrative_ontology:measurement_basis(stat_su_t1950, projected).
narrative_ontology:measurement(stat_su_t1975, state_killing_authority__categorical_abolition, suppression_requirement, 1975, 0.63).
narrative_ontology:measurement_basis(stat_su_t1975, projected).
narrative_ontology:measurement(stat_su_t2000, state_killing_authority__categorical_abolition, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement_basis(stat_su_t2000, observed).
narrative_ontology:measurement(stat_su_t2013, state_killing_authority__categorical_abolition, suppression_requirement, 2013, 0.71).
narrative_ontology:measurement_basis(stat_su_t2013, observed).
narrative_ontology:measurement(stat_su_t2020, state_killing_authority__categorical_abolition, suppression_requirement, 2020, 0.72).
narrative_ontology:measurement_basis(stat_su_t2020, observed).
narrative_ontology:measurement(stat_su_t2026, state_killing_authority__categorical_abolition, suppression_requirement, 2026, 0.72).
narrative_ontology:measurement_basis(stat_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__categorical_abolition, identity_coordination).
narrative_ontology:boltzmann_floor_override(state_killing_authority__categorical_abolition, 0.12).
narrative_ontology:affects_constraint(state_killing_authority__categorical_abolition, state_killing_authority__retributive_desert).
narrative_ontology:affects_constraint(state_killing_authority__categorical_abolition, state_killing_authority__deterrence_instrument).
narrative_ontology:affects_constraint(state_killing_authority__categorical_abolition, lex_talionis_proportionality).
narrative_ontology:affects_constraint(state_killing_authority__categorical_abolition, victim_family_voice_in_sentencing).

% DUAL FORMULATION NOTE:
% The state-killing-authority kernel has three readings, each a separate constraint story with its own ε, stakeholders, and classification. This file (categorical_abolition) asserts ε=0.85 (high extraction from retributive/deterrence proponents via removal of options) and claimed_type=tangled_rope (genuine coordination of abolition norm + active suppression of competing readings). The sibling retributive_desert reading asserts ε≈0.3 (low extraction; natural law of proportionality) and claimed_type=mountain. The sibling deterrence_instrument reading asserts ε≈0.4 (empirical claim, dependent on evidence) and claimed_type=rope or tangled_rope depending on suppression of dissenting criminology. These are not three measurements of one constraint; they are three structurally distinct constraints sharing a kernel. The ε-invariance test: measuring execution-justice one way (inalienability focus) gives high extraction; measuring it another way (proportionality focus) gives low extraction. Two constraints, therefore two stories, linked by kernel_id.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_killing_authority__categorical_abolition, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
