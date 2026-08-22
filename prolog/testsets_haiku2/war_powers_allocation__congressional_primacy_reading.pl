% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__congressional_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_powers_allocation__congressional_primacy_reading, []).

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
 *   constraint_id: war_powers_allocation__congressional_primacy_reading
 *   human_readable: War Powers: Congressional Primacy Reading
 *   domain: constitutional_law/separation_of_powers
 *
 * SUMMARY:
 *   This constraint instantiates the congressional-primacy reading of the
 *   contested war-powers kernel: the claim that military force beyond
 *   immediate self-defense requires explicit congressional authorization as a
 *   constitutional imperative, not a courtesy or political preference. From
 *   this reading's standpoint, executive deployment without authorization
 *   constitutes extraction of the war-making power that the Constitution
 *   allocates to Congress, leveraging doctrines of inherent authority and
 *   emergency necessity to shift constitutional authority away from the
 *   legislative branch and toward executive unilateralism. The constraint
 *   shows tangled coordination (Congress is theoretically beneficiary of the
 *   check on executive power; Congress also suffers as victim when the
 *   constraint is violated and its authority is eroded) with active
 *   enforcement friction. High suppression reflects the institutional
 *   machinery deployed to maintain executive assertions of inherent authority
 *   against this reading's claims.
 *
 * KEY AGENTS:
 *   - legislative_branch: Nominal holder of war-making power, but structurally bypassed and victim when unilateral action proceeds; attempting to enforce the constraint faces resistance framed as constitutional overreach
 *   - executive_branch: Deployer of force, agent of the constraint's violation; benefits from flexibility and accumulated de facto authority; frames unilateral action as constitutionally protected necessity
 *   - courts: Abstain from resolving the underlying constitutional question, allowing the balance to shift without judicial settlement
 *   - military_personnel: Lowest-power seat; execute orders regardless of authorization source; bear direct physical costs
 *   - affected_foreign_populations: Completely excluded from U.S. constitutional process; bear costs of force deployed without the domestic deliberative check this reading claims is constitutionally required
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__congressional_primacy_reading, 0.68).
domain_priors:suppression_score(war_powers_allocation__congressional_primacy_reading, 0.72).
domain_priors:theater_ratio(war_powers_allocation__congressional_primacy_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__congressional_primacy_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__congressional_primacy_reading, "War Powers: Congressional Primacy Reading").
narrative_ontology:topic_domain(war_powers_allocation__congressional_primacy_reading, "constitutional_law/separation_of_powers").

domain_priors:requires_active_enforcement(war_powers_allocation__congressional_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__congressional_primacy_reading, 'bde54b57-0ce5-48ff-b260-44a3f8d17e57').
narrative_ontology:cs_kernel_codification('bde54b57-0ce5-48ff-b260-44a3f8d17e57', formalized).
narrative_ontology:cs_authority_grounding('bde54b57-0ce5-48ff-b260-44a3f8d17e57', extraction).
narrative_ontology:cs_interpretation_layer_present('bde54b57-0ce5-48ff-b260-44a3f8d17e57').
narrative_ontology:cs_reading_relation('bde54b57-0ce5-48ff-b260-44a3f8d17e57', war_powers_allocation__inherent_executive_reading, forecloses).
narrative_ontology:cs_reading_relation('bde54b57-0ce5-48ff-b260-44a3f8d17e57', war_powers_allocation__functional_accommodation_reading, coexists_with).
narrative_ontology:cs_axiom('bde54b57-0ce5-48ff-b260-44a3f8d17e57', foundational, exclusive_congressional_war_authority).
narrative_ontology:cs_axiom_status(exclusive_congressional_war_authority, holdable).
narrative_ontology:cs_axiom_grounding('bde54b57-0ce5-48ff-b260-44a3f8d17e57', exclusive_congressional_war_authority, deontological).
narrative_ontology:cs_axiom('bde54b57-0ce5-48ff-b260-44a3f8d17e57', foundational, authorization_required_before_force).
narrative_ontology:cs_axiom_status(authorization_required_before_force, holdable).
narrative_ontology:cs_axiom_grounding('bde54b57-0ce5-48ff-b260-44a3f8d17e57', authorization_required_before_force, conventional).
narrative_ontology:cs_reference_frame('bde54b57-0ce5-48ff-b260-44a3f8d17e57', framers_constitutional_allocation).
narrative_ontology:cs_drift_state('bde54b57-0ce5-48ff-b260-44a3f8d17e57', contemporary_unilateral_practice, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('bde54b57-0ce5-48ff-b260-44a3f8d17e57', '').
narrative_ontology:cs_kernel_id(war_powers_allocation__congressional_primacy_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__congressional_primacy_reading, legislative_branch).
narrative_ontology:constraint_victim(war_powers_allocation__congressional_primacy_reading, legislative_branch).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(war_powers_allocation__congressional_primacy_reading, executive_branch).
narrative_ontology:constraint_beneficiary(war_powers_allocation__congressional_primacy_reading, civilian_population).
narrative_ontology:constraint_victim(war_powers_allocation__congressional_primacy_reading, military_personnel).
narrative_ontology:constraint_victim(war_powers_allocation__congressional_primacy_reading, affected_foreign_populations).
narrative_ontology:constraint_victim(war_powers_allocation__congressional_primacy_reading, civilian_population).
narrative_ontology:constraint_vindicates(war_powers_allocation__congressional_primacy_reading, constitutional_separation_of_powers).
narrative_ontology:constraint_vindicates(war_powers_allocation__congressional_primacy_reading, legislative_war_power_supremacy).
narrative_ontology:constraint_vindicates(war_powers_allocation__congressional_primacy_reading, check_on_executive_unilateralism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds formal constitutional authority to declare war and authorize military force. In practice, experiences repeated circumvention when executives deploy force under doctrines of inherent authority, emergency necessity, or treaty commitment. Bears the political cost of opposing popular military actions and the institutional cost of assertion (constitutional challenges, institutional conflict). Nominally holds the power but functionally shares it; attempting to exercise exclusive authority triggers executive resistance framed as constitutionally protected inherent power.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, legislative_branch, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(war_powers_allocation__congressional_primacy_reading, legislative_branch, payer).

% Deploys military force under doctrines of commander-in-chief authority, national emergency response, and treaty defense obligations without prior congressional authorization. Justifies action as necessary for national security and executive responsibility for foreign affairs. Gains operational flexibility, avoids the political friction of seeking authorization, and accumulates de facto control over war-making power that shifts the constitutional balance even when congressional authorization ultimately follows. Can frame resistance to unilateral action as constitutional overreach by legislative branch.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, executive_branch, beneficiary,
    institutional, biographical, arbitrage, national).

% Adjudicate disputes over war-powers allocation but historically decline to resolve the underlying constitutional question, citing political questions doctrine or ripeness. Occasionally enjoin executive actions but rarely strike down military deployments on authorization grounds alone. Their abstention allows the balance to shift toward executive action without producing a binding constitutional settlement.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, courts, observer,
    institutional, generational, analytical, national).

% Execute military orders regardless of the constitutional source of authorization; they bear the direct cost (injury, death, psychological harm) of military operations. Their legal liability for actions follows executive orders even when those orders circumvent congressional authorization. Have no vote on deployment and no practical exit from the chain of command.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, military_personnel, payer,
    powerless, biographical, trapped, global).

% Experience military force deployed without the deliberative process that congressional authorization theoretically requires. This reading asserts that bypassing congressional war-making authority also bypasses domestic political constraints that would otherwise limit military scope and duration. Bear costs of conflict initiated and conducted without the constitutional check.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, affected_foreign_populations, payer,
    powerless, immediate, trapped, global).

% Theoretically benefits from congressional authorization as a mechanism that forces deliberation on war's costs and necessity before deployment; gains legitimacy and accountability through legislative process. Also bears indirect costs: war funding, potential blowback, militarized foreign policy. Popular support for military action can pressure Congress to authorize retroactively, limiting the deliberative brake the requirement was designed to provide.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, civilian_population, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(war_powers_allocation__congressional_primacy_reading, civilian_population, payer).

% Historical Congresses that passed broad authorizations (AUMF 2001, Gulf of Tonkin resolution) or acquiesced to executive deployments cannot retroactively reclaim the war-making authority they surrendered or permitted to be stripped through precedent accumulation. Their silence or acquiescence becomes binding on successors, even when successor Congresses would oppose the original deployment or its expansions.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, prior_congresses, excluded,
    institutional, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_powers_allocation__congressional_primacy_reading, executive_branch).
narrative_ontology:fixing_cost_class(war_powers_allocation__congressional_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates war-making authority to the legislative branch to ensure military force decisions pass through a deliberative body with broader constituencies than the executive, slowing and constraining unilateral deployment and forcing cost-benefit accounting before commitment to major force projection.
% TRANSFER_FUNCTION: Transfers de facto war-making power from the legislative branch (nominally holding exclusive authority) to the executive branch (accumulating unilateral deployment capacity through doctrines of inherent authority and emergency response), as executives repeatedly act without prior authorization and Congress either acquiesces or authorizes retroactively, entrenching the shift.
% ABSENT_VOICES: Foreign populations affected by unilateral military action have no voice in U.S. constitutional deliberation; domestic political constituencies opposed to military action are bypassed when authorization is not sought; military personnel executing orders are not consulted on the constitutional legitimacy of their deployment. Prior Congresses whose silence or acquiescence established precedent cannot correct their decision's downstream effects.
% DISAPPEARANCE_RATIONALE: If this constraint (the requirement for congressional authorization of military force beyond immediate defense) disappeared entirely and executive unilateral deployment became constitutionally unchallenged, the balance of war-making authority would formally consolidate around the executive. The legislative branch would lose even nominal war-making power; foreign policy would become exclusively executive-controlled; and the deliberative requirement that theoretically precedes major military commitments would vanish. The world does not naturally return to this state; it requires sustained executive assertion and congressional acquiescence that the constraint's existence creates friction against.
% FOUNDING_PROBLEM: The Framers sought to prevent any single person (the president) from controlling the decision to go to war, believing that concentration of war-making authority in one office invited the wars of monarchy. Military force requires deliberation across multiple constituencies (representatives of different regions, interests, and electoral pressures) to constrain executive ambition and to force candor about costs before commitment.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars and legislative war-powers advocates attest the founding problem remains live: modern presidents continue to deploy force without authorization, relying on doctrines of inherent authority that the Framers did not endorse and that shift the war-making balance away from the legislative branch. Executive-branch attorneys and national security scholars attest the founding problem is substantially outdated: modern security threats move too fast for deliberative congressional process, and executive flexibility is necessary. Federal judges (in the rare cases they touch the question) decline to resolve the dispute, but historical pattern — Tonkin Gulf deployment, Iraq War, Syria airstrikes — shows executives repeatedly acting unilaterally and Congresses acquiescing or authorizing retroactively. Legislative bodies in other democracies (UK, Canada, Australia) maintain stricter authorization requirements, suggesting the mechanism is not antiquated but rather eroded by U.S. executive power accumulation.
narrative_ontology:disappearance_verdict(war_powers_allocation__congressional_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(war_powers_allocation__congressional_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__congressional_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(war_powers_allocation__congressional_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(war_powers_allocation__congressional_primacy_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_powers_allocation__congressional_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_powers_allocation__congressional_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_powers_allocation__congressional_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extraction metric (0.68 at interval end) reflects the accumulated shift of war-making authority from Congress to executive: each unilateral deployment without authorization transfers effective control and sets precedent that future presidents cite. The series shows monotonic increase from 0.35 to 0.68 across the 80-year interval, tracking the accumulation of executive power through doctrines the executive asserts and Congress has not successfully challenged. Suppression (0.72) is high because maintaining the executive's expanded authority requires active suppression of congressional assertion: legal doctrines of inherent authority are deployed to block legislative challenges, broad war authorizations are framed as sufficient to cover new theaters, and emergency declarations are used to bypass the authorization requirement. Theater ratio (0.41) is moderate: some enforcement activity is genuine security response (the substantive basis executives cite), but a growing proportion is theatrical maintenance of authority—justifying unilateral action through doctrines that depend on interpretations the reading rejects. Accessibility collapse (0.64) is high because once the legislative seat understands the extraction mechanism (that the executive is building de facto control through precedent), the alternative of reasserting congressional authority becomes politically costly and institutionally difficult; escape requires constitutional confrontation Congress is structurally reluctant to mount. Resistance (0.58) is moderate-high because legislative bodies do resist (War Powers Resolution 1973, authorization denials on specific operations), but resistance is intermittent and often overcome by claims of national emergency or retroactive authorization.
 *
 * PERSPECTIVAL GAP:
 *   The executive-branch seat should compute as beneficiary/coordinate (the constraint's interpretation enforces its power claims; unilateral action advances its interests) while the legislative-branch seat computes as partly victim (authority eroded, enforcement attempts blocked). From the executive seat, unilateral deployment is a legitimate exercise of commander-in-chief power and constitutional responsibility for national security; from the legislative seat, the same behavior is extraction of constitutionally allocated war-making authority. The engine derives d from the beneficiary/victim declaration and exit options: executive has low d (beneficiary, high exit options including arbitrage of emergency doctrines), Congress has high d (nominal beneficiary of the structural check, but victim when bypassed, with constrained options for enforcement). This reading's core claim is that the constraint is structurally extractive—it maintains power asymmetry through doctrines that suppress legislative assertion—hence tangled_rope rather than rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Legislative branch declared as both beneficiary and victim reflects the tangled structure: Congress is the nominal beneficiary of a constitutional rule that allocates war-making to itself, but it is also the victim when that rule is violated and its authority is extracted. The congressional seat benefits from the EXISTENCE of the authorization requirement (it is the seat authorized) but suffers from the ENFORCEMENT gap (executives bypass it with impunity). Executive branch is clearly beneficiary: it accumulates unilateral deployment capacity and avoids the friction of seeking authorization. Military personnel and affected foreign populations are victims: they bear the costs of force deployed without the deliberative constraint. Courts' observer role reflects their abstention from the core dispute. The civilian population is dual-positioned: benefits from theoretical deliberation on war costs (the coordination function), but also bears indirect costs and can be mobilized to support military action, which undermines the deliberative brake. This reading's frame implies that suppressing legislative assertion (0.72 suppression metric) is necessary to maintain the extraction—hence why suppression is high rather than negligible.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids the false-natural-law trap by declaring both beneficiary and victim (Congress) and acknowledging the coordination function (deliberation on war authority) alongside the extraction that violates it. The constraint is tangled_rope, not mountain or rope: it has a real coordination function (forcing deliberation before military commitment) AND asymmetric extraction (executive gains unilateral flexibility at legislative expense). The mandatrophy claim (founding problem status=contested, disappearance verdict=world_rearranges) is grounded in the structural fact that unilateral executive deployment would reshape the constitutional balance if unsupervised—Congress has not extinguished the authorization requirement, but it has tolerated repeated violations, which erodes the constraint's actual force. The theater ratio (0.41) indicates that a growing share of the constraint's enforcement machinery is performative: executives justify unilateral action through emergency rhetoric and doctrines that presuppose their own authority; Congress holds hearings and passes war-powers resolutions but permits the president to continue acting; courts decline to intervene. The performance masks a shift in substantive power.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretation_vs_text,
    'Does the constitutional text clearly allocate war-making authority exclusively to Congress, or is the text ambiguous enough that inherent executive authority could be a legitimate reading of the same words?',
    'Comparative constitutional law (how other democracies read similar provisions) and historical analysis of the Framers'' intent, though the latter is itself contested across readings.',
    'If the text is ambiguous, the congressional-primacy reading competes with inherent-executive on equal textual footing and the balance depends on institutional practice and political power rather than constitutional clarity. If the text clearly allocates war-making to Congress, the inherent-executive reading is a usurpation, not a defensible interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretation_vs_text, conceptual, 'Whether the constitutional text resolves or leaves open the war-powers question.').

omega_variable(
    emergency_necessity_constraint,
    'Does the constitutional allocation of war-making authority include an exception for imminent threats that require immediate presidential response, or does the authorization requirement apply even to emergency deployments?',
    'Legislative or judicial clarity on the scope of presidential emergency authority and where authorization kicks in temporally. Natural experiment from jurisdictions with strict authorization requirements that maintain exceptions for imminent defense.',
    'If imminent-threat exceptions are constitutionally valid, the boundary of executive authority is shifted outward and the congressional-primacy reading must accommodate those exceptions. If no exception exists, any unilateral deployment violates the constraint. The functional_accommodation reading essentially claims the exception is large and context-dependent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergency_necessity_constraint, conceptual, 'The scope of emergency exceptions to the authorization requirement.').

omega_variable(
    institutional_capacity_question,
    'Can modern Congress, given the speed of contemporary military threats and the classified intelligence required for authorization decisions, actually deliberate in real-time on military deployments, or does the deliberative requirement become costless theater when threats are urgent?',
    'Comparative analysis of expedited authorization procedures in allied democracies; empirical study of congressional deliberation speed when national security is at stake; assessment of whether Congress can access classification needed for informed voting.',
    'If Congress genuinely cannot deliberate in time for urgent threats, the authorization requirement becomes an unfunded mandate that no seat can actually execute—the constraint becomes unenforceable in practice and the functional_accommodation reading gains credibility as a realist alternative. If Congress can maintain both speed and informed deliberation, the requirement remains structurally viable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_capacity_question, empirical, 'Whether modern Congress can operationalize real-time authorization deliberation.').

omega_variable(
    prior_authorization_cumulative_effect,
    'Do broad standing authorizations (AUMF 2001 authorizing force against al-Qaeda and associated forces globally) effectively constitute a blanket delegation that erodes the need for case-by-case authorization, or does each new military operation still require fresh authorization even if covered by a prior statute?',
    'Legislative and judicial clarity on the legal scope of standing authorizations; explicit congressional votes on whether new theaters and operations require new authorization or are covered by existing law.',
    'If standing authorizations can be broadly read to cover new contexts, the practical requirement for authorization is hollowed out even when Congress holds the nominal authority—the extraction metric would be even higher because Congress has surrendered control once, and the executive can reuse that surrender indefinitely. If Congress maintains strict authority-per-operation, the constraint''s enforcement becomes more stringent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prior_authorization_cumulative_effect, empirical, 'Whether prior broad authorizations eliminate the need for fresh authorization of new military operations.').

omega_variable(
    reading_contest_empirical_signature,
    'How would the three readings (congressional_primacy, functional_accommodation, inherent_executive) differ in their empirical predictions about executive behavior and constraints? What observable patterns would distinguish them?',
    'Longitudinal analysis of presidential deployment patterns, congressional response patterns, and court rulings over decades. Controlled comparison of war-powers behavior under different administrations with different ideological commitments to executive authority.',
    'If the readings produce identical observable predictions, they are not actually distinct constraints—they are the same structure described in different language. If they diverge observably (e.g., congressional_primacy predicts more authorization attempts; inherent_executive predicts unilateral deployment regardless of congressional position; functional_accommodation predicts rapid authorization for imminent threats but resistance to prolonged campaigns), the readings are structurally distinguishable and the measurement of which applies to actual behavior resolves some of the contest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_empirical_signature, conceptual, 'The observable empirical signatures that would distinguish the three competing readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__congressional_primacy_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war_powers_cong_primacy_tr_t0, war_powers_allocation__congressional_primacy_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(war_powers_cong_primacy_tr_t0, observed).
narrative_ontology:measurement(war_powers_cong_primacy_tr_t10, war_powers_allocation__congressional_primacy_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement_basis(war_powers_cong_primacy_tr_t10, observed).
narrative_ontology:measurement(war_powers_cong_primacy_tr_t20, war_powers_allocation__congressional_primacy_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement_basis(war_powers_cong_primacy_tr_t20, observed).
narrative_ontology:measurement(war_powers_cong_primacy_tr_t40, war_powers_allocation__congressional_primacy_reading, theater_ratio, 40, 0.37).
narrative_ontology:measurement_basis(war_powers_cong_primacy_tr_t40, observed).
narrative_ontology:measurement(war_powers_cong_primacy_tr_t60, war_powers_allocation__congressional_primacy_reading, theater_ratio, 60, 0.39).
narrative_ontology:measurement_basis(war_powers_cong_primacy_tr_t60, observed).
narrative_ontology:measurement(war_powers_cong_primacy_tr_t80, war_powers_allocation__congressional_primacy_reading, theater_ratio, 80, 0.41).
narrative_ontology:measurement_basis(war_powers_cong_primacy_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(war_powers_cong_primacy_be_t0, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(war_powers_cong_primacy_be_t0, observed).
narrative_ontology:measurement(war_powers_cong_primacy_be_t10, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement_basis(war_powers_cong_primacy_be_t10, observed).
narrative_ontology:measurement(war_powers_cong_primacy_be_t20, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement_basis(war_powers_cong_primacy_be_t20, observed).
narrative_ontology:measurement(war_powers_cong_primacy_be_t40, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement_basis(war_powers_cong_primacy_be_t40, observed).
narrative_ontology:measurement(war_powers_cong_primacy_be_t60, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 60, 0.65).
narrative_ontology:measurement_basis(war_powers_cong_primacy_be_t60, observed).
narrative_ontology:measurement(war_powers_cong_primacy_be_t80, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 80, 0.68).
narrative_ontology:measurement_basis(war_powers_cong_primacy_be_t80, observed).

% Suppression requirement over time
narrative_ontology:measurement(war_powers_cong_primacy_su_t0, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(war_powers_cong_primacy_su_t0, observed).
narrative_ontology:measurement(war_powers_cong_primacy_su_t10, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 10, 0.54).
narrative_ontology:measurement_basis(war_powers_cong_primacy_su_t10, observed).
narrative_ontology:measurement(war_powers_cong_primacy_su_t20, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement_basis(war_powers_cong_primacy_su_t20, observed).
narrative_ontology:measurement(war_powers_cong_primacy_su_t40, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 40, 0.66).
narrative_ontology:measurement_basis(war_powers_cong_primacy_su_t40, observed).
narrative_ontology:measurement(war_powers_cong_primacy_su_t60, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 60, 0.7).
narrative_ontology:measurement_basis(war_powers_cong_primacy_su_t60, observed).
narrative_ontology:measurement(war_powers_cong_primacy_su_t80, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 80, 0.72).
narrative_ontology:measurement_basis(war_powers_cong_primacy_su_t80, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__congressional_primacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(war_powers_allocation__congressional_primacy_reading, 0.12).
narrative_ontology:affects_constraint(war_powers_allocation__congressional_primacy_reading, war_powers_allocation__inherent_executive_reading).
narrative_ontology:affects_constraint(war_powers_allocation__congressional_primacy_reading, war_powers_allocation__functional_accommodation_reading).
narrative_ontology:affects_constraint(war_powers_allocation__congressional_primacy_reading, war_powers_allocation__presidential_emergency_authority).
narrative_ontology:affects_constraint(war_powers_allocation__congressional_primacy_reading, war_powers_allocation__standing_authorization_doctrine).

% DUAL FORMULATION NOTE:
% war_powers_allocation is a contested kernel with three structural readings: congressional_primacy (this story), inherent_executive, and functional_accommodation. Each reading instantiates a different constraint because they have different ε values (what counts as extraction is different under each reading), different beneficiary/victim structures, and different foundational axioms about constitutional authority. The readings are linked via network.affects_constraints because changes in one reading's credibility or institutional practice create pressure on the others—e.g., courts enforcing congressional authorization would weaken the inherent_executive reading; widespread executive unilateral deployments without challenge would strengthen that reading. The decomposition is governed by the ε-invariance principle: measuring the constraint (empirical extractiveness of unilateral deployment) differently under the three readings would show materially different ε values because each reading defines what counts as extraction differently. Three separate stories model this more accurately than one story with a 'measurement basis' parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(war_powers_allocation__congressional_primacy_reading, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
