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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: state_killing_authority__retributive_desert
 *   human_readable: State Killing Authority: Retributive Desert (Lex Talionis)
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   Under the retributive desert reading of state killing authority,
 *   execution is justified as a proportional response to murder grounded in
 *   the normative principle that taking a life incurs a debt that can only be
 *   repaid through death. The murdered victim enters the beneficiary set
 *   posthumously as the one vindicated; the state collects legitimacy for
 *   fulfilling the proportionality norm; the condemned murderer and their
 *   family bear the extraction cost. This reading is ONE among three:
 *   deterrence_instrument reads the same institutional arrangement as
 *   justified by future crime prevention; categorical_abolition reads it as
 *   inherently impermissible. All three readings describe the state's
 *   authority to execute, but from different normative grounds, producing
 *   different stakeholder benefits, different victim/payer structures, and
 *   ultimately different constraint classifications. The retributive reading
 *   is authored here as a tangled_rope: genuine coordination function (public
 *   rule linking crime to consequence), asymmetric extraction (the condemned
 *   bears the cost without benefits), and active enforcement (appeals
 *   processes, clemency decisions, judicial review all work to sustain the
 *   constraint despite high resistance). The claim/metric independence rule
 *   applies: the retributive reading CLAIMS proportional desert as the
 *   legitimacy ground; the metrics describe the extractiveness and
 *   suppression empirically observed; the divergence is the corpus's
 *   measurement of whether the framing matches the structure.
 *
 * KEY AGENTS:
 *   - murdered_victim — beneficiary posthumously via vindication; cannot advocate
 *   - state_retributive_authority — agenda-setter; administers the constraint and claims proportionality as its ground
 *   - condemned_murderer — payer; loses life as forfeit for taking a life
 *   - condemned_murderer_family — payer; bears collateral costs (grief, stigma, loss)
 *   - murder_victim_family — beneficiary-payer; gains symbolic vindication but often reports execution does not heal
 *   - retributive_jurisprudence_community — beneficiary; the reading vindicates their theoretical framework
 *   - death_penalty_abolitionists — excluded; object to the forfeiture premise and would read the same institutional arrangement as snare or impermissible
 *   - judicial_decider — observer; constrained by the retributive framework to accept proportional desert as legitimacy criterion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__retributive_desert, 0.58).
domain_priors:suppression_score(state_killing_authority__retributive_desert, 0.72).
domain_priors:theater_ratio(state_killing_authority__retributive_desert, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, extractiveness, 0.58).
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__retributive_desert, tangled_rope).
narrative_ontology:human_readable(state_killing_authority__retributive_desert, "State Killing Authority: Retributive Desert (Lex Talionis)").
narrative_ontology:topic_domain(state_killing_authority__retributive_desert, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_killing_authority__retributive_desert).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__retributive_desert, '09134c00-958f-479f-9405-5fcc88b1006c').
narrative_ontology:cs_kernel_codification('09134c00-958f-479f-9405-5fcc88b1006c', fixed_text).
narrative_ontology:cs_authority_grounding('09134c00-958f-479f-9405-5fcc88b1006c', lineage).
narrative_ontology:cs_interpretation_layer_present('09134c00-958f-479f-9405-5fcc88b1006c').
narrative_ontology:cs_reading_relation('09134c00-958f-479f-9405-5fcc88b1006c', state_killing_authority__deterrence_instrument, coexists_with).
narrative_ontology:cs_reading_relation('09134c00-958f-479f-9405-5fcc88b1006c', state_killing_authority__categorical_abolition, coexists_with).
narrative_ontology:cs_axiom('09134c00-958f-479f-9405-5fcc88b1006c', foundational, proportional_desert_duty).
narrative_ontology:cs_axiom_status(proportional_desert_duty, holdable).
narrative_ontology:cs_axiom_grounding('09134c00-958f-479f-9405-5fcc88b1006c', proportional_desert_duty, deontological).
narrative_ontology:cs_axiom('09134c00-958f-479f-9405-5fcc88b1006c', foundational, murderer_forfeiture_doctrine).
narrative_ontology:cs_axiom_status(murderer_forfeiture_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('09134c00-958f-479f-9405-5fcc88b1006c', murderer_forfeiture_doctrine, deontological).
narrative_ontology:cs_reference_frame('09134c00-958f-479f-9405-5fcc88b1006c', proportional_desert_state_authority).
narrative_ontology:cs_drift_state('09134c00-958f-479f-9405-5fcc88b1006c', contemporary_abolitionist_pressure, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('09134c00-958f-479f-9405-5fcc88b1006c', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(state_killing_authority__retributive_desert, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, murdered_victim_vindication).
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, state_retributive_authority).
narrative_ontology:constraint_victim(state_killing_authority__retributive_desert, condemned_murderer).
narrative_ontology:constraint_victim(state_killing_authority__retributive_desert, condemned_murderer_family).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, murder_victim_family).
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, retributive_jurisprudence_community).
narrative_ontology:constraint_victim(state_killing_authority__retributive_desert, murder_victim_family).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The deceased person whose murder the retributive framework vindicates. Under this reading, the victim's dignity and rights are restored posthumously through proportional state killing of the murderer. The victim cannot advocate but is invoked as the beneficiary of the proportionality norm.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, murdered_victim, beneficiary,
    powerless, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(state_killing_authority__retributive_desert, murdered_victim).

% The state's institutional capacity to adjudicate guilt, pronounce proportional sentence, and execute it. Under this reading, the state's authority is grounded in the duty to vindicate the murdered victim through proportional punishment, not in deterrent efficacy or rehabilitation potential. The state administers the constraint and collects the legitimacy of fulfilling the proportionality norm.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, state_retributive_authority, agenda_setter,
    institutional, civilizational, analytical, national).

% The person adjudicated to have committed murder. Under this reading, the murderer has forfeited their right to life through their own act; the constraint treats death as the proportional consequence of taking a life. No exit from this determination save clemency or appellate reversal. The condemned person bears the ultimate cost under the retributive framework.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, condemned_murderer, payer,
    powerless, immediate, trapped, national).

% The relatives of the condemned person. They bear costs including witness to state execution, social stigma, loss of kinship relationship, and psychological harm. The retributive framework does not enumerate these costs as relevant to desert calculation; they are collateral effects, not the constraint's direct object.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, condemned_murderer_family, payer,
    powerless, biographical, constrained, national).

% The relatives of the murdered person. Under this reading, they benefit from the vindication of the victim's death through proportional punishment; the state's killing of the murderer restores symbolic balance. However, they also bear ongoing grief, may carry witness responsibility (testifying at trial and clemency hearings), and often report that execution does not resolve their trauma or bring closure.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, murder_victim_family, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(state_killing_authority__retributive_desert, murder_victim_family, payer).

% Philosophers, legal theorists, and judicial actors who hold or defend retributive desert theory. The constraint's operation vindicates their normative framework; the practice of proportional punishment reifies the theory, making it self-referential. They benefit from institutional authority accepting their premises.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, retributive_jurisprudence_community, beneficiary,
    organized, civilizational, mobile, global).

% Actors and movements that reject capital punishment on categorical or consequentialist grounds. They are excluded from the decision-making authority under the retributive reading; their objections are treated as external to the constraint's legitimacy structure rather than as internal disagreement about desert. They would argue for different readings of the kernel (abolition, deterrence minimization).
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, death_penalty_abolitionists, excluded,
    organized, generational, constrained, global).

% International human rights treaties, courts, and oversight bodies that increasingly treat capital punishment as incompatible with human dignity norms. Under the retributive reading, the state's authority to execute is domestically grounded; international human rights authority is treated as a separate forum that may impose costs but does not negate the state's retributive legitimacy. The excluded status is contentious: some states defer to international override; others assert retributive authority against it.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, international_human_rights_regime, excluded,
    institutional, generational, constrained, global).

% The court system tasked with adjudicating guilt, proportionality of sentence, and constitutional adequacy of procedure. The judicial observer sees the full constraint structure but is constrained by the retributive framework to accept proportional desert as the legitimacy criterion, even if other criteria (deterrence, rehabilitation, victim preference) might shift if weighted differently.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, judicial_decider, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_authority__retributive_desert, state_retributive_authority).
narrative_ontology:fixing_cost_class(state_killing_authority__retributive_desert, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable, publicly-articulated rule linking severity of crime (taking a life) to severity of punishment (death), enabling citizens to know the consequence of capital murder and enabling society to treat crime response as principled rather than arbitrary or vengeful.
% TRANSFER_FUNCTION: Transfers the condemned murderer from the set of rights-holders (under universal protections) to the set of forfeited-right persons through a judicial determination of guilt; simultaneously transfers the murdered victim from the set of the living to the set of the vindicated-dead, whose dignity is restored through proportional state killing.
% ABSENT_VOICES: Death-penalty abolitionists and international human rights bodies that reject the forfeiture premise itself; those who would argue the foundational problem (unpunished murder) is soluble by means other than execution (life imprisonment without parole, restitution, victim family choice); those who bear costs without a seat (family of the condemned, prison workers, witnesses to execution, society bearing the psychological weight of state killing).
% DISAPPEARANCE_RATIONALE: Under retributive desert reading, if the constraint vanished, the world would NOT rearrange in the way deterrence or victim-preference readings would claim. Instead: the proportionality norm itself disappears; the state loses a claimed mechanism of vindicating murdered victims; murder would cease to carry the consequence the framework says it merits. Abolitionists would argue the world rearranges by becoming more humane and that other deterrents (long incarceration) remain. The verdict is contested because the readings dispute whether murder-response is a duty of the state (retributive view: yes, vindicating proportionality; abolitionist view: no, state killing is prohibited; deterrence view: only if empirically necessary).
% FOUNDING_PROBLEM: When one person murders another, the victim's death leaves an unpunished wrong and an unmade proportional response; the retributive framework holds that the state has a duty to vindicat the victim through a punishment proportional to the crime, and that taking a life requires death as the only equivalent sanction.
% FOUNDING_PROBLEM_CORROBORATION: Retributive legal scholars and some state justice systems attest the founding problem is live and that proportional desert is the proper response. Death-penalty abolitionists and international human rights bodies attest the founding problem is either non-existent (state execution is not a legitimate response to murder; it IS a second murder) or that other punishments (life imprisonment) adequately address it. Empirical criminology attests the claim that execution uniquely vindicates the victim is not corroborated by victim family outcomes data — families report mixed or negative psychological effects from execution. No consensus corroborator outside the retributive framework itself attests the founding problem requires this specific remedy.
narrative_ontology:disappearance_verdict(state_killing_authority__retributive_desert, contested).
narrative_ontology:founding_problem_status(state_killing_authority__retributive_desert, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__retributive_desert, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_killing_authority__retributive_desert, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_authority__retributive_desert, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate (0.58 at interval end) because the constraint does solve a genuine coordination problem (public rule linking crime to consequence) but overlays it with extraction that serves the state's claim to a particular legitimacy ground (proportional desert) rather than outcome (rehabilitation, deterrence, victim healing). The measurement series show slight rise then plateau: extractiveness peaks around t=30 (after abolition movements gain institutional traction and the state doubles down on the retributive ground to defend executions), then stabilizes as the constraint settles into a steady state defended by institutional inertia and judicial review processes. Theater ratio is moderate-to-rising (0.28 to 0.42): the constraint invokes proportionality as its justification, but increasing share of enforcement activity is devoted to defending the constraint against challenges (appeals, clemency hearings, ritual appeals to victim vindication) rather than administering proportionate punishment cleanly. Suppression is high (0.72 at interval end) because the constraint's persistence depends on actively suppressing alternatives: abolition arguments are excluded from the legitimacy structure (they are not 'just an alternative policy choice' but treated as external to retributive authority); appeals and clemency options exist but are largely performative (reversals are rare, clemency is politically costly). Accessibility collapse is 0.68: once the retributive framework is understood, alternatives (life imprisonment, restorative justice, victim-family-choice regimes) do collapse somewhat from public discourse, but abolition has grown strong enough that they remain technically available at the legislative level in many jurisdictions. Resistance is high (0.74): death-penalty abolitionists, international human rights bodies, and victim families frequently dissent; the constraint meets active legal and moral challenge. The suppression and resistance values move together, suggesting the constraint's persistence depends on institutional enforcement against ongoing contestation, characteristic of tangled_rope.
 *
 * PERSPECTIVAL GAP:
 *   The state retributive authority seat and the condemned murderer seat should compute radically different classifications. From the state's seat, the constraint is coordination: a public rule that citizens can know (if you murder, you forfeit your life), administered consistently through judicial review. From the condemned seat, the constraint is pure extraction: the state has decided to end their life based on a principle (proportionality) the condemned person does not accept and cannot refuse; the 'coordination' is one-directional—it coordinates the state's action, not the condemned's choice. The murderer victim family sits between: they gain the symbolic vindicative benefit the retributive reading promises, but many report the state's execution does not heal their loss and leaves them with a second killing to process. From the abolitionist seat, the entire structure is snare or worse: the state has claimed a killing authority grounded in a principle (proportional desert) that the abolitionist reading rejects, and enforces it against a condemned person who cannot exit and a family who cannot veto. The engine computes these divergences from the power/exit/beneficiary/victim structure; the authored metrics do not assume convergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The murdered victim sits at d≈0.0 (full beneficiary, analytically) — the constraint exists to vindicate them, though they cannot experience the benefit and their family's actual benefit is contested. The state retributive authority sits near d≈0.15-0.25 (beneficiary with some payer costs): the state collects legitimacy and authority assertion from executing the constraint, but also bears costs (judicial review delays, clemency pressures, international condemnation). The condemned murderer sits at d≈1.0 (full target): they have no exit, no alternatives, no claimed benefits; the constraint exists to extract their life. The condemned murderer family sits at d≈0.85 (high target): they bear profound costs (grief, loss, stigma) with only indirect symbolic benefit if they accept the retributive frame. The murder victim family sits at d≈0.50-0.65 (moderate-to-high target): they are invoked as beneficiaries but often experience execution as a second trauma; the constraint's claimed benefit (vindication) does not deliver empirical healing. The retributive jurisprudence community sits at d≈0.2-0.3 (beneficiary with constraints): they gain legitimacy for their theory but are constrained by the empirical failure of the constraint to deliver victim healing and by abolitionist challenge. These divergences are authored through the stakeholder roles and power/exit/scope specifications; the engine derives d and chi per-seat from the structural data.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unpunished murder; the victim's dignity unmade proportional) was live when the constraint was established. The retributive reading treats the founding problem as STILL LIVE: victims require vindication through proportional desert, and death is the only response that satisfies that duty. However, the disappearance verdict is CONTESTED: abolitionists argue the founding problem is either non-existent (state execution is not legitimate vindication, it is a second murder) or solved by life imprisonment. The measurement data show the theater ratio rising from 0.28 to 0.42 over 80 time units: as the constraint faces abolition challenges, an increasing share of enforcement activity is devoted to defending the proportionality claim itself (appeals, clemency hearings, policy advocacy) rather than administering proportionate punishment. This is classic mandatrophy pattern: the founding problem's framing is being contested, the constraint is defended through symbolic and theatrical means (appeals to victim vindication that do not empirically deliver healing), and the state doubles down on the legitimacy ground (proportional desert) rather than demonstrating outcome. The omega on victim_vindication_mechanism documents the core mandatrophy question: if the constraint fails to deliver what it claims (victim healing / vindication), does it become a pure extraction mechanism dressed in proportionality language? The retributive reading would answer: the constraint delivers NORMATIVE vindication independent of empirical healing; abolition would answer: that claim is a cover story for state killing. Mandatrophy_resolved is NOT declared because the contest is live—the reading is holding the line on proportionality as a normative principle, not surrendering the claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    forfeiture_premise_ambiguity,
    'Does committing murder logically entail forfeiture of all rights, or only forfeiture of specific protections (e.g., bodily integrity in confinement)? Does a murderer retain human dignity in principle, or is dignity extinguished by the act?',
    'Genealogical and philosophical analysis of forfeiture doctrine across legal traditions; examination of whether forfeiture is partial (context-specific) or total (comprehensive). Sibling readings disagree on whether the doctrine holds at all.',
    'If forfeiture is total, the retributive reading is internally consistent: the murderer has no rights-claim against execution. If forfeiture is partial or denied, the retributive reading must rest on a different ground (e.g., desert as a positive duty to punish, separate from rights-loss), which brings it closer to the abolition reading''s rejection of the premise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(forfeiture_premise_ambiguity, conceptual, 'Whether murderers logically forfeit all rights or only specific protections.').

omega_variable(
    proportionality_measure_ambiguity,
    'Is death the uniquely proportional response to murder, or is life imprisonment proportional? Can proportionality be satisfied by means other than death?',
    'Comparative retributive theory: examine whether retributive scholars agree death is the ONLY proportional response, or whether proportionality admits a range of responses. Historical and cross-cultural survey of desert-based punishment systems.',
    'If proportionality admits life imprisonment as adequate, the retributive reading becomes compatible with death-abolition in jurisdictions that choose that path. If only death satisfies retributive desert, the reading forecloses abolitionist alternatives within its own framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_measure_ambiguity, conceptual, 'Whether death is uniquely proportional or one proportional option among several.').

omega_variable(
    state_authority_grounding_contest,
    'Is the state''s authority to execute grounded in retributive desert (the reading''s claim), or is it grounded in utilitarian calculation (deterrence reading), or is it not grounded at all (abolition reading)?',
    'The kernel contest itself: different readings attach the state''s killing authority to different legitimacy sources. This omega documents that the constraint story has chosen ONE reading; sibling readings emit different cs_structure entries and different authority_grounding values.',
    'This reading treats authority as grounded in proportional desert independent of outcome. If the deterrence evidence turns unfavorable (execution does not deter), this reading''s authority persists; if deterrence were the ground, it would erode. If abolition wins public or legal consensus, this reading''s authority is rejected, not weakened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_authority_grounding_contest, conceptual, 'Which grounds the state''s killing authority: retributive desert, deterrent efficacy, or none.').

omega_variable(
    victim_vindication_mechanism,
    'Does executing the murderer factually vindicate or heal the murdered victim''s injury, or is vindication purely symbolic? Does victim family preference constitute vindication, or is the state''s proportional response vindication regardless of family preference?',
    'Empirical psychology and phenomenology: interview victim families pre- and post-execution; measure whether execution resolves grief, closure, or sense of justice. Philosophical analysis of what ''vindication'' means in a retributive frame when the victim is dead and cannot experience the state''s response.',
    'If vindication is empirically measured (closure, healing), execution''s failure to deliver it would undermine the retributive reading''s claim that it serves victim vindication. If vindication is understood as purely normative (the state''s public affirmation of proportional desert), empirical outcomes do not erode the reading. This omega documents whether vindication is a benefit the constraint delivers or a narrative the retributive frame invokes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_vindication_mechanism, empirical, 'Whether execution empirically vindicates murdered victims or expresses only symbolic vindication.').

omega_variable(
    reading_kernel_relationship,
    'Is this story one reading of a contested kernel (state_killing_authority), or is it a free-standing constraint? Do the other readings (deterrence, abolition) share the same kernel or describe entirely different constraints?',
    'The kernel context declares the relationship: all three readings (retributive_desert, deterrence_instrument, categorical_abolition) are readings of a single contested kernel grounding state killing authority. The sibling readings'' constraint_ids and cs_structure.reading_relations entries document the family membership.',
    'This constraint is one voice in a kernel contest. Its classification as tangled_rope is relative to the retributive reading; sibling readings may classify the SAME institutional arrangement differently (deterrence might classify as rope or scaffold; abolition might classify as snare). The engine computes per-reading classifications; the corpus captures the dispute.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_relationship, conceptual, 'The contested kernel structure: one state arrangement, multiple readings, multiple classifications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__retributive_desert, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_authority__retributive_desert, theater_ratio, 0, 0.28).
narrative_ontology:measurement(stat_tr_t10, state_killing_authority__retributive_desert, theater_ratio, 10, 0.31).
narrative_ontology:measurement(stat_tr_t20, state_killing_authority__retributive_desert, theater_ratio, 20, 0.35).
narrative_ontology:measurement(stat_tr_t30, state_killing_authority__retributive_desert, theater_ratio, 30, 0.38).
narrative_ontology:measurement(stat_tr_t40, state_killing_authority__retributive_desert, theater_ratio, 40, 0.4).
narrative_ontology:measurement(stat_tr_t50, state_killing_authority__retributive_desert, theater_ratio, 50, 0.42).
narrative_ontology:measurement(stat_tr_t60, state_killing_authority__retributive_desert, theater_ratio, 60, 0.41).
narrative_ontology:measurement(stat_tr_t70, state_killing_authority__retributive_desert, theater_ratio, 70, 0.4).
narrative_ontology:measurement(stat_tr_t80, state_killing_authority__retributive_desert, theater_ratio, 80, 0.41).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_authority__retributive_desert, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(stat_be_t10, state_killing_authority__retributive_desert, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(stat_be_t20, state_killing_authority__retributive_desert, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(stat_be_t30, state_killing_authority__retributive_desert, base_extractiveness, 30, 0.6).
narrative_ontology:measurement(stat_be_t40, state_killing_authority__retributive_desert, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(stat_be_t50, state_killing_authority__retributive_desert, base_extractiveness, 50, 0.57).
narrative_ontology:measurement(stat_be_t60, state_killing_authority__retributive_desert, base_extractiveness, 60, 0.58).
narrative_ontology:measurement(stat_be_t70, state_killing_authority__retributive_desert, base_extractiveness, 70, 0.59).
narrative_ontology:measurement(stat_be_t80, state_killing_authority__retributive_desert, base_extractiveness, 80, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_authority__retributive_desert, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(stat_su_t10, state_killing_authority__retributive_desert, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(stat_su_t20, state_killing_authority__retributive_desert, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(stat_su_t30, state_killing_authority__retributive_desert, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(stat_su_t40, state_killing_authority__retributive_desert, suppression_requirement, 40, 0.71).
narrative_ontology:measurement(stat_su_t50, state_killing_authority__retributive_desert, suppression_requirement, 50, 0.73).
narrative_ontology:measurement(stat_su_t60, state_killing_authority__retributive_desert, suppression_requirement, 60, 0.72).
narrative_ontology:measurement(stat_su_t70, state_killing_authority__retributive_desert, suppression_requirement, 70, 0.72).
narrative_ontology:measurement(stat_su_t80, state_killing_authority__retributive_desert, suppression_requirement, 80, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__retributive_desert, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(state_killing_authority__retributive_desert, 0.12).
narrative_ontology:affects_constraint(state_killing_authority__retributive_desert, state_killing_authority__deterrence_instrument).
narrative_ontology:affects_constraint(state_killing_authority__retributive_desert, state_killing_authority__categorical_abolition).

% DUAL FORMULATION NOTE:
% Three readings of one kernel: state_killing_authority. The retributive reading (this story) grounds state execution authority in proportional desert (lex talionis). The deterrence reading grounds it in deterrent efficacy. The abolition reading rejects state execution as inherently impermissible. All three describe the same institutional practice; ε (extractiveness of that practice under the reading's own lights) diverges: retributive reads the arrangement as moderate extraction with genuine coordination; deterrence reads it as justified only if deterrence succeeds; abolition reads it as high-extractive snare. The three stories form a constraint family linked by network.affects_constraints. Each story carries its own beneficiary/victim structure, its own cs_structure with reading_relations and axioms unique to that reading, and its own computed-per-seat classification. The contest is whether the state arrangement instantiates proportional justice (retributive), utilitarian prevention (deterrence), or state killing masked as justice (abolition).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_killing_authority__retributive_desert, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
