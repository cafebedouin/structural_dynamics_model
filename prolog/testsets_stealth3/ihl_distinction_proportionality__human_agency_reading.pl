% ============================================================================
% CONSTRAINT STORY: ihl_distinction_proportionality__human_agency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ihl_distinction_proportionality__human_agency_reading, []).

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
 *   constraint_id: ihl_distinction_proportionality__human_agency_reading
 *   human_readable: Human Final Targeting Decision Requirement (Human-Agency Reading of IHL Distinction/Proportionality)
 *   domain: legal/military/technological
 *
 * SUMMARY:
 *   This file instantiates ONE reading of a contested kernel. The kernel is
 *   IHL's distinction and proportionality obligations as applied to lethal
 *   force; this story authors the human_agency_reading: lethal force is
 *   lawful only where an identifiable human being makes the final targeting
 *   decision, grounded in Additional Protocol I's distinction/proportionality
 *   duties and the Martens Clause's principles of humanity. The reading's
 *   constraint suppresses the entire fully-autonomous design space by
 *   construction — any system lacking a human final decision is unlawful
 *   regardless of demonstrated discrimination performance — while authorizing
 *   human-supervised autonomy. The epsilon referent is the standing
 *   arrangement under contest: the human-decision requirement as it actually
 *   operates, assessed by this reading's own lights. The reading endorses the
 *   requirement morally but authors its operating costs honestly rather than
 *   presenting the costless picture its advocacy materials give: categorical
 *   closure of a design space without performance evidence, tempo and scale
 *   costs on commands, sunk-cost destruction for developers, and compounding
 *   interpretive centrality flowing to the custodial institutions. Moral
 *   endorsement and cost accounting are kept separate — the claim and the
 *   metrics are independent authored facts. KEY AGENTS (by structural
 *   relationship): - icrc_interpretive_authorities: Primary beneficiary
 *   (institutional/arbitrage) — collects interpretive centrality and agenda
 *   control - ihl_targeting_legal_profession: Secondary beneficiary
 *   (organized/identity_locked) — professional indispensability multiplied by
 *   every preserved human decision - autonomous_weapons_developers: Primary
 *   target (powerful/constrained) — flagship product lines ruled unlawful
 *   irrespective of performance - military_operational_commands: Primary
 *   target (institutional/constrained) — tempo, scale, and link-vulnerability
 *   costs - frontline_operators: Mixed seat (moderate/constrained) — decision
 *   burden and prosecution exposure, offset by retained refusal authority -
 *   civilian_populations_in_armed_conflict: Intended protected class
 *   (powerless/trapped) — promised protection, partially delivered -
 *   humanitarian_disarmament_coalition: Agenda ally (organized/mobile) —
 *   quotable demand anchoring campaigns and funding - resisting_major_powers:
 *   Payer with arbitrage exit (institutional/arbitrage) — blocks binding
 *   instruments, develops freely - future_battlefield_civilians: Excluded
 *   voice — governed by the winning standard without a seat in the process -
 *   independent_technology_assessors: Analytical observer — supplies the
 *   performance data the contest consumes
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__human_agency_reading, 0.73).
domain_priors:suppression_score(ihl_distinction_proportionality__human_agency_reading, 0.67).
domain_priors:theater_ratio(ihl_distinction_proportionality__human_agency_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, extractiveness, 0.73).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 0.67).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__human_agency_reading, tangled_rope).
narrative_ontology:human_readable(ihl_distinction_proportionality__human_agency_reading, "Human Final Targeting Decision Requirement (Human-Agency Reading of IHL Distinction/Proportionality)").
narrative_ontology:topic_domain(ihl_distinction_proportionality__human_agency_reading, "legal/military/technological").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__human_agency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__human_agency_reading, '0c2d1eec-7ca0-4182-9cb6-7a899e08f4f1').
narrative_ontology:cs_kernel_codification('0c2d1eec-7ca0-4182-9cb6-7a899e08f4f1', fixed_text).
narrative_ontology:cs_authority_grounding('0c2d1eec-7ca0-4182-9cb6-7a899e08f4f1', lineage).
narrative_ontology:cs_interpretation_layer_present('0c2d1eec-7ca0-4182-9cb6-7a899e08f4f1').
narrative_ontology:cs_reading_relation('0c2d1eec-7ca0-4182-9cb6-7a899e08f4f1', ihl_distinction_proportionality__outcomes_based_reading, forecloses).
narrative_ontology:cs_reading_relation('0c2d1eec-7ca0-4182-9cb6-7a899e08f4f1', ihl_distinction_proportionality__categorical_prohibition_reading, influences).
narrative_ontology:cs_axiom('0c2d1eec-7ca0-4182-9cb6-7a899e08f4f1', foundational, human_moral_judgment_irreducible_at_force_application).
narrative_ontology:cs_axiom_status(human_moral_judgment_irreducible_at_force_application, holdable).
narrative_ontology:cs_axiom_grounding('0c2d1eec-7ca0-4182-9cb6-7a899e08f4f1', human_moral_judgment_irreducible_at_force_application, deontological).
narrative_ontology:cs_axiom('0c2d1eec-7ca0-4182-9cb6-7a899e08f4f1', secondary, meaningful_human_control_required_for_lawful_engagement).
narrative_ontology:cs_axiom_status(meaningful_human_control_required_for_lawful_engagement, holdable).
narrative_ontology:cs_axiom_grounding('0c2d1eec-7ca0-4182-9cb6-7a899e08f4f1', meaningful_human_control_required_for_lawful_engagement, conventional).
narrative_ontology:cs_reference_frame('0c2d1eec-7ca0-4182-9cb6-7a899e08f4f1', human_judgment_constitutes_lawful_force).
narrative_ontology:cs_drift_state('0c2d1eec-7ca0-4182-9cb6-7a899e08f4f1', contemporary_autonomous_systems_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0c2d1eec-7ca0-4182-9cb6-7a899e08f4f1', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__human_agency_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, icrc_interpretive_authorities).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, ihl_targeting_legal_profession).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, civilian_populations_in_armed_conflict).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, humanitarian_disarmament_coalition).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, autonomous_weapons_developers).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, military_operational_commands).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, frontline_operators).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, frontline_operators).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, resisting_major_powers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Publishes interpretive guidance on what distinction and proportionality require as weapon systems automate; convenes states, issues joint appeals with the UN Secretary-General, and supplies the doctrinal language that treaties and national policies quote. Every state or court adopting the human-decision standard cites this body's interpretations, compounding its custodial standing, access, and agenda leverage. Exit is meaningless: the organization is constituted by this custodial role.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, icrc_interpretive_authorities, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ihl_distinction_proportionality__human_agency_reading, icrc_interpretive_authorities, agenda_setter).

% Military lawyers, academy instructors, and tribunal practitioners whose services become mandatory at every preserved human decision point: vetting target lists, certifying that a human will make the engagement decision, training operators, advising commanders. Each preserved decision multiplies career-relevant legal work. Their professional self-understanding is bound to the doctrine that law enters war through human judgment; abandoning the standard would dissolve the role they have built careers inhabiting.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, ihl_targeting_legal_profession, beneficiary,
    organized, biographical, identity_locked, global).

% Defense contractors building systems that select and engage targets without a human confirming each engagement. Under the human-decision standard their flagship product lines are unlawful however well they demonstrate target discrimination, so sunk research must be re-architected around human confirmation nodes or abandoned. They can lobby, rebrand autonomy as decision support, or sell where the standard does not bind, but the core market segment they invested in is closed wherever it does.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, autonomous_weapons_developers, payer,
    powerful, biographical, constrained, global).

% Commands planning against adversaries fielding massed, machine-speed systems. Requiring a human to make each final engagement decision caps reaction time, forces communication links that can be jammed or targeted, and places personnel inside the decision chain where they can be killed or captured. Their planning alternatives — shedding the human node or fighting at machine speed — are precisely what the standard forbids, and doctrine and liability follow them across jurisdictions.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, military_operational_commands, payer,
    institutional, generational, constrained, continental).

% Soldiers and remote operators who must review machine nominations and confirm engagements under time pressure, carrying legal and moral responsibility for each one. The duty exposes them to automation-bias accusations, prosecution risk when reviews are rushed, and the weight of kill decisions. It also keeps them inside the moral and legal community: they retain authority to refuse an unlawful engagement rather than execute a machine's output, and they are not left holding blame for errors no human chose.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, frontline_operators, payer,
    moderate, immediate, constrained, regional).
narrative_ontology:stakeholder_secondary_role(ihl_distinction_proportionality__human_agency_reading, frontline_operators, beneficiary).

% People living where these systems would operate. The standard promises them that a human being accountable to law will weigh their presence before force is released, rather than a classifier's confidence score. How much protection actually arrives depends on whether the standard binds the belligerents above them, whether the human in the loop is attentive rather than ceremonial, and whether human judgment outperforms machine judgment in their particular battlespace — none of which they control or can verify.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, civilian_populations_in_armed_conflict, beneficiary,
    powerless, civilizational, trapped, global).

% NGO networks campaigning against autonomous weapons. The human-decision standard gives their campaigns a concrete, quotable demand, anchors their diplomatic strategy in the CCW and UNGA processes, and sustains membership and funding around an objective that seems winnable short of full prohibition. Their organizational health is tied to the standard remaining both urgent and unachieved; universal achievement tomorrow would convert them into compliance monitors.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, humanitarian_disarmament_coalition, beneficiary,
    organized, generational, mobile, global).

% Major military powers investing heavily in autonomy who block consensus on binding instruments and decline to internalize the human-final-decision rule. They bear reputational cost and cede normative ground, but no treaty binds them, their rivals' restraint under the standard is a competitive gift, and they can shop for permissive partners and markets. Their resistance is why the standard's reach remains uneven.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, resisting_major_powers, payer,
    institutional, generational, arbitrage, global).

% Populations of wars not yet fought, in whose battlespaces the winning standard will govern life-and-death sorting. They have no seat in the rooms where the standard is being settled; whichever reading prevails will have been chosen without them, though they will live inside its consequences longest.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, future_battlefield_civilians, excluded,
    powerless, civilizational, trapped, global).

% Researchers and testing bodies measuring how well automated systems actually discriminate combatants from civilians and how faithfully human oversight operates under combat time pressure. They publish the performance data the interpretive debate consumes but hold no vote on which reading becomes law.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, independent_technology_assessors, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ihl_distinction_proportionality__human_agency_reading, icrc_interpretive_authorities).
narrative_ontology:fixing_cost_class(ihl_distinction_proportionality__human_agency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real collective-action problem: absent a shared rule fixing who may decide a killing, every state's automation choices impose unpriced risks on other states' soldiers and civilians, and no one can attribute responsibility for machine-caused deaths. The standard makes lethal decision-authority traceable to identifiable, trainable, prosecutable human agents across all parties.
% TRANSFER_FUNCTION: Moves final engagement authority from automated processing pipelines to designated human decision-makers; moves the costs of that reservation — slower engagement cycles, vulnerable communication links, re-engineered product lines — onto commands and manufacturers; moves interpretive authority, doctrinal ownership, and professional indispensability toward the ICRC and the IHL legal profession.
% ABSENT_VOICES: Future battlefield civilians have no seat anywhere in the process. The technical communities that build and test the systems participate only as witnesses, not as voting parties; the states most invested in autonomy engage mainly to block rather than to shape. Consensus in the CCW therefore reflects the preferences of states willing to sit through the process, not of those whose conduct the standard would govern most.
% DISAPPEARANCE_RATIONALE: If the human-final-decision requirement vanished overnight, procurement pipelines would redirect toward fully autonomous engagement, operational architectures would reorganize around machine-speed targeting, the accountability framework for machine-caused deaths would lose its anchor, and the interpretive institutions whose centrality depends on the standard would lose their central object — the governance of lethal force would rearrange around whichever performance standard replaced it.
% FOUNDING_PROBLEM: Ensuring that killing in war remains governed by human moral judgment and accountable human agency — a problem first posed by industrialized warfare and answered by the Martens Clause, reposed when precision-strike automation began compressing the human role in targeting, and now posed acutely by systems able to complete the sensor-to-engagement loop without one.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the ICRC-centered beneficiary set: major militaries' own directives (for example, U.S. DoD Directive 3000.09's requirement for appropriate levels of human judgment over force-application decisions), UN General Assembly resolutions adopted by wide margins, and open letters from AI and robotics researchers attesting the accountability-gap concern. No comparable external body attests that the problem is solved.
narrative_ontology:disappearance_verdict(ihl_distinction_proportionality__human_agency_reading, world_rearranges).
narrative_ontology:founding_problem_status(ihl_distinction_proportionality__human_agency_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__human_agency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ihl_distinction_proportionality__human_agency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ihl_distinction_proportionality__human_agency_reading, 0.73, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ihl_distinction_proportionality__human_agency_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ihl_distinction_proportionality__human_agency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ihl_distinction_proportionality__human_agency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.73) because the requirement binds by construction rather than by evidence: a fully autonomous system is unlawful whatever its measured discrimination record, so the burden on developers and commands is decoupled from any showing they could make. Suppression (0.67) is a raw structural property, unscaled by power or scope: the enforcement machinery — CCW mandates, the eleven Guiding Principles, UNGA resolutions, national doctrine, export controls, command-responsibility exposure — has matured steadily since 2013 but stops short of a binding treaty, and major-power arbitrage keeps exits partly open. Theater ratio (0.38) reflects a genuine doctrinal and training function increasingly diluted by compressed-loop ratification, where the preserved 'human decision' is a click-through on machine nominations under automation bias. Accessibility collapse is low (0.30): understanding the standard does not close the outcomes-based alternative, which remains live and funded. Resistance (0.62) is sustained consensus-blocking by major military powers plus industry lobbying. The measurement series run on one shared time grid — every tracked metric is authored at every examined year — with an enforcement-ratchet trajectory (rising suppression_requirement) and no oscillation modeled; the 2026 endpoints carry projected basis flags.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the ICRC and legal-profession seats the arrangement is the constitution of lawful warfare they steward — its demands are the price of law itself. From the developer and command seats it is a categorical closure imposed without performance evidence, borne by those with the least exit. From the operator seat it is simultaneously burden (kill-decision responsibility under time pressure, prosecution exposure) and protection (retained authority to refuse, insulation from blame for errors no human chose). Inter-institutionally, the ICRC's arbitrage-grade exit (its standing survives any single state's defection) contrasts with commands' constrained exit (doctrine and liability follow them across jurisdictions). Same-level divergence appears between developers (powerful, constrained — market segment closed) and resisting major powers (institutional, arbitrage — free to develop outside the standard), showing that nominal power parity masks exit asymmetry. The identity-lock on the legal profession is professional: self-concept constituted through custodianship of the human-judgment doctrine; if overwhelming performance data broke that frame, the seat's resistance to the outcomes-based sibling would collapse.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries map to low directionality: the ICRC collects compounding interpretive centrality (every citation of 'meaningful human control' deepens its custodial standing), and the legal profession collects multiplied indispensability. Civilian populations are the intended protected class, but derivation would read their beneficiary declaration as full subsidy; the override to d=0.18 corrects for partial realized delivery — the standard binds unevenly, no treaty exists, and whether the human in the loop outperforms the machine is exactly what the protection-effectiveness omega leaves open. Developers and commands sit near the full-target end: they bear the transfer with constrained exit. Resisting major powers are declared payers but their arbitrage exit dampens effective burden well below trapped payers — their restraint under the standard would be a competitive gift to rivals, so they simply decline to internalize it. Operators mix payer and beneficiary positions: burden and retained moral agency in one seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim prevents mislabeling in both directions. Reading the requirement as pure rope (the advocacy framing: a costless moral necessity) erases the asymmetric extraction — design-space closure without performance evidence, and authority rents flowing to interpretive custodians. Reading it as pure snare (the industry framing: jurisdictional rent-seeking dressed as ethics) erases the genuine coordination function — traceable accountability for machine-age killing solves a real collective-action problem that no accepted performance-certification mechanism currently replaces. On genealogy: the founding problem (killing escaping human moral agency, first industrialized, now automatable) predates this reading's crystallization around 2013 and remains live — automation is accelerating, not receding — so no mandatrophy resolution is declared and no sunset clause exists; the arrangement presents itself as a permanent constitutional feature of IHL rather than transitional support. The mismatch consumer should note status=live paired with verdict=world_rearranges: arrangements genuinely depend on it, and the dependency is growing, not atrophying.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the kernel ihl_distinction_proportionality (the human_agency_reading). What structural differences would prevail under the sibling readings, outcomes_based_reading and categorical_prohibition_reading?',
    'Observe which reading consolidates in treaty text, state practice, and national military doctrine over the coming decade; the consolidated reading''s constraint supersedes this one''s beneficiary/victim structure.',
    'Under outcomes_based_reading, epsilon falls sharply (suppression becomes performance-contingent and interpretive rents shrink); under categorical_prohibition_reading, epsilon rises further (all autonomy suppressed and the victim set expands to include human-supervised systems).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame routing: this story instantiates the human-agency reading; sibling readings would change the constraint''s epsilon and victim set.').

omega_variable(
    locus_of_lawfulness_disagreement,
    'Where exactly do the readings locate lawfulness of lethal force: is it constituted by who decides (human agency) or by how well the decision performs (outcome quality)?',
    'Conceptual analysis of what Additional Protocol I''s feasible-precautions and constant-care language can bear: whether those clauses specify a decision-maker or only a decision quality.',
    'If the texts specify decision quality only, the human-agency reading loses its textual anchor and collapses toward the outcomes-based sibling; if they presuppose a responsible human agent, this reading holds and the foreclosure of the outcomes-based sibling is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(locus_of_lawfulness_disagreement, conceptual, 'The specific structural element on which the kernel''s readings diverge.').

omega_variable(
    ratification_genuineness,
    'Does nominal human ratification inside compressed sensor-to-engagement loops constitute the irreducible moral judgment the standard requires, or is it performative compliance?',
    'Operator studies and engagement telemetry: time available per decision, override rates, automation-bias incidence, and whether reviewing operators ever reject machine nominations under combat conditions.',
    'If ratification is largely ceremonial, the theater ratio is understated and the coordination function degrades toward staged maintenance; if operators exercise real judgment under pressure, the function is genuine and the current theater estimate stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ratification_genuineness, empirical, 'Whether preserved human decision points deliver real judgment or rubber-stamp theater.').

omega_variable(
    relative_civilian_protection_performance,
    'Does a rested, accountable human decision-maker actually discriminate combatants from civilians and proportion force better than a well-engineered autonomous system under equivalent conditions?',
    'Standardized scenario-battery evaluations plus post-conflict casualty auditing comparing human-in-the-loop and autonomous engagements.',
    'If machines outperform fatigued, biased, or vengeful humans, the civilian-protection benefit weakens, the reading''s coordination justification narrows to accountability alone, and the effective burden on developers and commands rises without the protective payoff.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(relative_civilian_protection_performance, empirical, 'Whether the intended protected class actually collects protection from the human-decision standard.').

omega_variable(
    binding_instrument_trajectory,
    'Will the human-decision standard harden into binding treaty law, or dissipate into voluntary guidelines and national doctrine?',
    'Track CCW and UNGA outcomes, national implementations, and export-control harmonization through the late 2020s.',
    'Treaty hardening raises suppression and locks the current beneficiary structure for a generation; dissipation leaves the standard as soft law with eroding enforcement capacity and eventual mandatrophy risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(binding_instrument_trajectory, empirical, 'Whether the enforcement machinery consolidates or decays.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__human_agency_reading, 2013, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl_human_agency_tr_t2013, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 2013, 0.2).
narrative_ontology:measurement(ihl_human_agency_tr_t2015, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 2015, 0.24).
narrative_ontology:measurement(ihl_human_agency_tr_t2017, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 2017, 0.28).
narrative_ontology:measurement(ihl_human_agency_tr_t2019, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 2019, 0.31).
narrative_ontology:measurement(ihl_human_agency_tr_t2021, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 2021, 0.34).
narrative_ontology:measurement(ihl_human_agency_tr_t2023, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 2023, 0.36).
narrative_ontology:measurement(ihl_human_agency_tr_t2026, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 2026, 0.38).

% Extraction over time
narrative_ontology:measurement(ihl_human_agency_be_t2013, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 2013, 0.45).
narrative_ontology:measurement(ihl_human_agency_be_t2015, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 2015, 0.52).
narrative_ontology:measurement(ihl_human_agency_be_t2017, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 2017, 0.58).
narrative_ontology:measurement(ihl_human_agency_be_t2019, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 2019, 0.63).
narrative_ontology:measurement(ihl_human_agency_be_t2021, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 2021, 0.67).
narrative_ontology:measurement(ihl_human_agency_be_t2023, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 2023, 0.7).
narrative_ontology:measurement(ihl_human_agency_be_t2026, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 2026, 0.73).

% Suppression requirement over time
narrative_ontology:measurement(ihl_human_agency_su_t2013, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 2013, 0.3).
narrative_ontology:measurement(ihl_human_agency_su_t2015, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 2015, 0.38).
narrative_ontology:measurement(ihl_human_agency_su_t2017, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 2017, 0.46).
narrative_ontology:measurement(ihl_human_agency_su_t2019, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 2019, 0.55).
narrative_ontology:measurement(ihl_human_agency_su_t2021, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 2021, 0.6).
narrative_ontology:measurement(ihl_human_agency_su_t2023, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 2023, 0.64).
narrative_ontology:measurement(ihl_human_agency_su_t2026, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 2026, 0.67).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__human_agency_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__human_agency_reading, ihl_distinction_proportionality__outcomes_based_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__human_agency_reading, ihl_distinction_proportionality__categorical_prohibition_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'IHL obligations on autonomous weapons' decomposes into three structurally distinct constraints — readings of one kernel — per the epsilon-invariance principle. This member instantiates the human-agency reading. The members' epsilon values differ because each reading constitutes a different constraint with a different beneficiary/victim structure, not because one constraint is measured different ways: the outcomes-based sibling renders suppression performance-contingent (low epsilon), the categorical sibling extends suppression to human-supervised systems (higher epsilon, expanded victim set). Edges record family kinship and mutual citation of the same Additional Protocol I / Martens Clause textual base; no strict upstream/downstream causal ordering is asserted among the readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ihl_distinction_proportionality__human_agency_reading, powerless, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
