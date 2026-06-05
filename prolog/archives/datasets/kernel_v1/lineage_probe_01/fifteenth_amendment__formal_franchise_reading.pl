% ============================================================================
% CONSTRAINT STORY: fifteenth_amendment__formal_franchise_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fifteenth_amendment__formal_franchise_reading, []).

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
 *   constraint_id: fifteenth_amendment__formal_franchise_reading
 *   human_readable: Fifteenth Amendment Formal Franchise Reading: Facially Neutral Disfranchisement
 *   domain: legal/constitutional/voting_rights
 *
 * SUMMARY:
 *   The formal franchise reading of the Fifteenth Amendment represents a
 *   specific constitutional interpretation: the Amendment forbids only
 *   explicit racial conditions on voting ('race, color, or previous condition
 *   of servitude shall not be a qualification'); devices that are facially
 *   race-neutral—literacy tests, poll taxes, grandfather clauses, property
 *   requirements—stand outside the Amendment's terms even when operationally
 *   deployed to achieve racial disfranchisement. This reading generates a
 *   constraint because it determines which agents can regulate voting access
 *   and which disfranchisement mechanisms are constitutionally permissible.
 *   The constraint's extractiveness has increased over time (from 0.45 in
 *   1870 to 0.68 by 1960) as proxy devices proliferated and their racial
 *   operation became documented and undeniable. The theater_ratio (0.58)
 *   reflects that the formal reading's interpretive justification—'read the
 *   text literally'—is performative rather than functionally verifiable; the
 *   reading's survival depends on sustained attention to the text's explicit
 *   language and deliberate inattention to operational effects. The
 *   suppression requirement (0.72) reflects the coercive infrastructure
 *   needed to sustain the reading as operational targeting intensified:
 *   greater enforcement effort to prevent courts or Congress from recognizing
 *   the proxy mechanisms as discriminatory. This reading stands in structural
 *   opposition to the effective franchise reading, which interprets the
 *   Fifteenth Amendment to reach any device that operates to deny the vote by
 *   race, enabling Congress to impose results-based regulation via the Voting
 *   Rights Act.
 *
 * KEY AGENTS:
 *   - African American Voters and Racial Minorities: Primary victims (powerless/trapped) — subjected to disfranchisement by proxy devices with no legal recourse under the formal reading
 *   - Southern State Governments: Primary beneficiaries (institutional/arbitrage) — can deploy literacy tests, poll taxes, and grandfather clauses without violating the Fifteenth Amendment's explicit bar on racial conditions
 *   - Federal Courts: Secondary actors (moderate/constrained) — constrained by the formal reading to adjudicate voting disputes narrowly, generating extractive outcomes they did not choose
 *   - Congress (Reconstruction Era): Institutional authority (institutional/arbitrage) — chose the formal reading by drafting only an explicit racial bar rather than a functional disfranchisement ban
 *   - Civil Rights Coalition: Organized agents (organized/mobile) — recognized the formal reading as a temporary institutional arrangement vulnerable to legislative reframing via Section 2 enforcement authority
 *   - Analytical Observer: Civilization-scale perspective (analytical/analytical) — risks treating the formal reading as a deduction from constitutional text rather than a contingent interpretive choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifteenth_amendment__formal_franchise_reading, 0.68).
domain_priors:suppression_score(fifteenth_amendment__formal_franchise_reading, 0.72).
domain_priors:theater_ratio(fifteenth_amendment__formal_franchise_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifteenth_amendment__formal_franchise_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(fifteenth_amendment__formal_franchise_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(fifteenth_amendment__formal_franchise_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifteenth_amendment__formal_franchise_reading, snare).
narrative_ontology:human_readable(fifteenth_amendment__formal_franchise_reading, "Fifteenth Amendment Formal Franchise Reading: Facially Neutral Disfranchisement").
narrative_ontology:topic_domain(fifteenth_amendment__formal_franchise_reading, "legal/constitutional/voting_rights").

domain_priors:requires_active_enforcement(fifteenth_amendment__formal_franchise_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifteenth_amendment__formal_franchise_reading, '4d2a81aa-9e72-4449-8418-ec6476a36a99').
narrative_ontology:cs_kernel_codification('4d2a81aa-9e72-4449-8418-ec6476a36a99', fixed_text).
narrative_ontology:cs_authority_grounding('4d2a81aa-9e72-4449-8418-ec6476a36a99', lineage).
narrative_ontology:cs_interpretation_layer_present('4d2a81aa-9e72-4449-8418-ec6476a36a99').
narrative_ontology:cs_reading_relation('4d2a81aa-9e72-4449-8418-ec6476a36a99', fifteenth_amendment__effective_franchise_reading, coexists_with).
narrative_ontology:cs_axiom('4d2a81aa-9e72-4449-8418-ec6476a36a99', foundational, explicit_racial_bar_only).
narrative_ontology:cs_axiom_status(explicit_racial_bar_only, holdable).
narrative_ontology:cs_axiom_grounding('4d2a81aa-9e72-4449-8418-ec6476a36a99', explicit_racial_bar_only, conventional).
narrative_ontology:cs_axiom('4d2a81aa-9e72-4449-8418-ec6476a36a99', foundational, congress_section_two_enforcement_narrow).
narrative_ontology:cs_axiom_status(congress_section_two_enforcement_narrow, holdable).
narrative_ontology:cs_axiom_grounding('4d2a81aa-9e72-4449-8418-ec6476a36a99', congress_section_two_enforcement_narrow, conventional).
narrative_ontology:cs_reference_frame('4d2a81aa-9e72-4449-8418-ec6476a36a99', narrow_textual_federalism).
narrative_ontology:cs_drift_state('4d2a81aa-9e72-4449-8418-ec6476a36a99', contemporary_voting_rights_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('4d2a81aa-9e72-4449-8418-ec6476a36a99', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(fifteenth_amendment__formal_franchise_reading, fifteenth_amendment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifteenth_amendment__formal_franchise_reading, southern_states).
narrative_ontology:constraint_beneficiary(fifteenth_amendment__formal_franchise_reading, state_governments_deploying_neutral_devices).
narrative_ontology:constraint_victim(fifteenth_amendment__formal_franchise_reading, african_american_voters).
narrative_ontology:constraint_victim(fifteenth_amendment__formal_franchise_reading, disenfranchised_racial_minorities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: AFRICAN AMERICAN VOTERS (SNARE) — Trapped by literacy tests, poll taxes, and grandfather clauses that are facially race-neutral but operationally targeted. The formal reading confines the Fifteenth Amendment's reach to explicit racial bars, leaving these proxy devices untouched. Voters have no exit option: they cannot vote elsewhere, cannot change jurisdictions effectively, and have no legal recourse because the suppressive mechanism is formal-language compliant. Maximum experienced extraction — the entire structure is designed to achieve disfranchisement by indirection while appearing constitutional.
constraint_indexing:constraint_classification(fifteenth_amendment__formal_franchise_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SOUTHERN STATE GOVERNMENTS (ROPE) — Under the formal reading, states benefit from a coordination solution: they can preserve racial disfranchisement through facially neutral devices without violating the Fifteenth Amendment's explicit bar on racial conditions. This perspective sees the constraint as coordination — the federal constitution and state practice align through a narrow reading that permits states to achieve their electoral objectives. States experience the constraint as enabling rather than restricting; they have arbitrage options (deploy literacy tests, poll taxes, grandfather clauses; or enforce explicit bars) and face no suppression.
constraint_indexing:constraint_classification(fifteenth_amendment__formal_franchise_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: FEDERAL COURTS (TANGLED ROPE) — Courts face a genuine coordination problem (how do we adjudicate voting rights under the Constitution?) alongside asymmetric extraction (their interpretive choice constrains voter access and enables state disfranchisement). Under the formal reading, courts interpret the Fifteenth Amendment narrowly — protecting only against explicit racial bars — while the practical consequence is to license proxy disfranchisement. Courts experience suppression: they are constrained by the text's apparent scope ('race shall not be a qualification'), yet their interpretive choice generates enormous asymmetric extraction of voting rights. The coordination function is real (providing a rule for adjudication); the extraction is also real (the rule permits disfranchisement by proxy).
constraint_indexing:constraint_classification(fifteenth_amendment__formal_franchise_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: FORMAL-LANGUAGE DOCTRINE (PITON) — The interpretive rule 'read the text literally; if race is not explicitly mentioned, the rule does not apply' is largely theater. By the 1960s, the doctrine's performative function had become apparent: it preserved a veneer of race-neutral legality while everyone knew the devices were targeting race operationally. The doctrine persists through institutional inertia — legal formalism, precedent, and the legitimacy of narrow textualism — even as its functional verification capacity collapsed. The constraint's theater_ratio reflects that the doctrine's credibility depends on not noticing its operational purpose.
constraint_indexing:constraint_classification(fifteenth_amendment__formal_franchise_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CIVIL RIGHTS COALITION (SCAFFOLD) — Organized agents (NAACP, voting rights advocates) recognized the formal-reading constraint as a temporary institutional arrangement vulnerable to legislative action. The Voting Rights Act (1965) imposed a sunset clause: federal oversight of voting practices in jurisdictions with clear patterns of racial disfranchisement. The coalition saw the formal reading's extractive mechanism as tied to a specific reading of constitutional text that Congress could reframe. The constraint operates under a sunset: the moment Congress invokes Section 2 of the Fifteenth Amendment (giving Congress enforcement power) and applies results-based tests, the formal reading's exclusion of proxy devices collapses. Low effective extraction from this perspective because exit path is visible and achievable.
constraint_indexing:constraint_classification(fifteenth_amendment__formal_franchise_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From the civilizational analytical context, the formal reading appears as an immutable principle of constitutional interpretation: 'The Amendment forbids only what it explicitly forbids; silent on the mechanism means the mechanism is unregulated.' This reading treats textual silence as a natural interpretive law. However, the structural data reveals this as a false summit: the formal reading is a contingent interpretive choice (chosen by particular justices, in particular historical moments, producing particular distributional consequences), not a deduction from the text's logical structure. The analytical observer risks naturalizing what is actually a reading selection.
constraint_indexing:constraint_classification(fifteenth_amendment__formal_franchise_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fifteenth_amendment__formal_franchise_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fifteenth_amendment__formal_franchise_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fifteenth_amendment__formal_franchise_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fifteenth_amendment__formal_franchise_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(fifteenth_amendment__formal_franchise_reading, TR),
    TR >= 0.70.

:- end_tests(fifteenth_amendment__formal_franchise_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): Moderate-high. The formal reading benefits states by permitting proxy disfranchisement and harms voters by confining constitutional protection to explicit racial language. The extractiveness increased from 1870 (0.45) to 1960 (0.68) because the gap between text (explicit racial bars only) and operation (widespread racial disfranchisement via neutral devices) widened as proxy mechanisms proliferated. The measurement reflects growing knowledge of operational targeting—the constraint's extractive mechanism became less deniable over time. Suppression (0.72): High. Maintaining the formal reading required significant enforcement effort: restricting access to legislative records of intent, minimizing visibility of operational effects, preventing courts from applying results-based tests. As evidence of proxy targeting accumulated, the suppressive infrastructure had to strengthen (documentation of voter disfranchisement was actively suppressed until the 1960s). Theater ratio (0.58): Moderate-high. The formal reading's justification ('read the text literally; if race is not explicit, regulation does not apply') is performative rather than functionally verifiable. The reading persists through adherence to a textual rule, not through demonstrated epistemic accuracy. Theater increased from 1870 (0.35) to 1960 (0.58) as everyone involved (legislators, judges, observers) could see the operational disfranchisement, yet the formal reading continued to shield it. The theater-ratio rise reflects growing disconnect between what the rule says and what everyone knows it does.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximal. Voters see a snare; states see a rope; courts see tangled rope; the doctrine becomes piton; the coalition sees scaffold; the analytical observer risks naturalizing a contingent choice as immutable law. The gap reflects that the formal reading generates structurally distinct experiences depending on agent position: beneficiaries experience enabling coordination; victims experience extraction; institutional actors experience constraint; observers risk misclassification. The mandatrophy is resolved by recognizing that all six types are real perspectival readings of the same structure—the formal reading is not 'really' any one type but produces different types depending on the observer's structural relationship to disfranchisement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by beneficiary/victim status and exit options. Voters (victim + trapped) derive d → 0.95, experiencing maximum extraction. States (beneficiary + arbitrage) derive d → 0.05, experiencing enabling coordination. Courts (neither + constrained) derive d → 0.65, experiencing moderate tension. The formal-language doctrine (institutional + arbitrage) derives low d—the doctrine itself benefits from the narrow reading—but the doctrine's theater ratio (0.58) reveals its performative character: it persists through adherence to a rule whose operational justification has collapsed. The civil rights coalition (organized + mobile) derives d → 0.40: they have agency and can mobilize Congress to reframe Section 2 authority, reducing experienced extraction. The analytical observer (analytical + analytical) derives d → 0.72: they see the full structure but their native instruments cannot detect that the formal reading is a constructed choice rather than a logical law.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint's mandatrophy is resolved by recognizing that all six types are real perspectival readings. The classification is not 'the constraint is really a snare' but 'the constraint produces snare experiences for victims, rope experiences for beneficiaries, tangled rope for courts, piton for the doctrine, scaffold for the organized coalition, and mountain misclassification for the analytical observer.' The constraint exists because the formal reading allocates power: it permits states to regulate voting access via proxy devices, confines federal authority to explicit racial bars, and places victims in a position with no exit. The perspectival variety reflects that the formal reading generates structurally distinct relationships for each observer—no single type captures the constraint's full structure. The mandatrophy resolves by showing that the apparent inconsistency (snare for victims, rope for states, mountain at analytical scale) is not a classification error but an observation that the constraint's extractiveness, suppression, and theater depend on the agent's position within the disfranchisement structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_silence_interpretation,
    'Does textual silence on a regulatory mechanism mean Congress intended no regulation, or does it leave regulatory space for Congress to occupy?',
    'Historical legislative record from the Reconstruction Congress (39th-40th sessions) showing explicit debates over whether Section 1 or Section 2 of the Fifteenth Amendment was intended to reach proxy devices; analysis of how other constitutional provisions (e.g., Fourth Amendment silence on digital surveillance) have been interpreted to evolve with operational contexts.',
    'If silence means no Congress power: formal reading is locked into original 1870 understanding, and proxy devices remain untouched absent constitutional amendment. If silence permits Congress to regulate by results: Congress can reframe the amendment''s reach via Section 2 enforcement authority, and the formal reading becomes provisional, not immutable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_silence_interpretation, conceptual, 'Whether textual silence on mechanisms precludes congressional regulation by results').

omega_variable(
    operational_vs_facial_targeting,
    'At what level of knowledge does a state''s deployment of a facially neutral device constitute racial discrimination for constitutional purposes — intent only, or intent plus operation?',
    'Comparative constitutional law analysis; examination of how other democracies (Canada, South Africa, Germany) distinguish between formal and operative discrimination; historical reconstruction of state legislative debates contemporaneous with literacy test and poll tax adoption showing explicit intent to target race.',
    'If intent alone: states can shield themselves by claiming neutrality even when operational targeting is certain (formal reading holds). If intent plus operation, or operation alone: literacy tests and poll taxes are discriminatory regardless of facial language, and the Fifteenth Amendment reaches them (effective reading holds).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(operational_vs_facial_targeting, empirical, 'What triggers constitutional violation: facial language, intent, or operational effect').

omega_variable(
    congressional_enforcement_authority_scope,
    'Does the Fifteenth Amendment''s Section 2 (''Congress shall have power to enforce this article by appropriate legislation'') permit results-based regulation of voting practices, or only prohibition of explicitly racial conditions?',
    'Textual analysis of Section 2 language versus Section 5 of the Fourteenth Amendment (explicitly authoritative over state action), which courts have interpreted expansively; legislative history of Section 2 Fifteenth Amendment debates; comparative interpretation across other constitutional enforcement clauses.',
    'If Section 2 permits results-based enforcement: Congress can override the formal reading through legislation (Voting Rights Act is constitutionally grounded). If Section 2 is narrow: Congress''s authority is limited to remedying explicit racial bars, and facially neutral devices remain unreachable absent constitutional amendment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(congressional_enforcement_authority_scope, conceptual, 'Scope of congressional enforcement power under Fifteenth Amendment Section 2').

omega_variable(
    knowledge_of_proxy_operation,
    'From which historical moment should we measure the formal reading''s extractiveness — from 1870 (when the amendment''s scope was genuinely unclear), or from 1896 (when Plessy v. Ferguson sanctified separation), or from the 1950s (when proxy operation became universally documented)?',
    'Historical reconstruction of legal consciousness: when did jurists and legislators have access to evidence that literacy tests were disfranchising Black voters operationally? Comparison with modern interpretations of constitutional provisions applied retroactively.',
    'If 1870: the formal reading was a defensible interpretation given contemporary knowledge, and the constraint''s extractiveness should reflect that epistemic position. If 1950s: the formal reading became complicit in documented racial targeting, and extractiveness should reflect knowledge of operation. This shifts mandatrophy analysis: is the constraint a good-faith interpretive dispute or a chosen blindness?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_of_proxy_operation, empirical, 'Historical knowledge of proxy disfranchisement effects at time of formal reading''s establishment').

omega_variable(
    effective_reading_logical_containment,
    'Does the formal reading logically foreclose the effective reading, or can both readings coexist as competing interpretations held by different constitutional authorities?',
    'Modal logic analysis: if a jurisdiction adopts the formal reading, can it simultaneously honor the effective reading via Congress''s Section 2 power? Or does the formal reading''s constraint on amendment scope necessarily exclude Congress''s enforcement authority?',
    'If foreclosure: only one reading can be constitutionally correct, and the other must be wrong (gate is binary). If coexistence: both readings remain live positions, and the constitutional contest is resolved by institutional power (which branch prevails), not by logical necessity. This determines whether the kernel reading_relations should be ''forecloses'' or ''coexists_with''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effective_reading_logical_containment, conceptual, 'Whether formal and effective readings are logically exclusive or coexistent interpretations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifteenth_amendment__formal_franchise_reading, 0, 95).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fif_formal_theater_1870, fifteenth_amendment__formal_franchise_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(fif_formal_theater_1900, fifteenth_amendment__formal_franchise_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement(fif_formal_theater_1960, fifteenth_amendment__formal_franchise_reading, theater_ratio, 90, 0.58).

% Extraction over time
narrative_ontology:measurement(fif_formal_extr_1870, fifteenth_amendment__formal_franchise_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(fif_formal_extr_1900, fifteenth_amendment__formal_franchise_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(fif_formal_extr_1960, fifteenth_amendment__formal_franchise_reading, base_extractiveness, 90, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fif_formal_supp_1870, fifteenth_amendment__formal_franchise_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(fif_formal_supp_1900, fifteenth_amendment__formal_franchise_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(fif_formal_supp_1960, fifteenth_amendment__formal_franchise_reading, suppression_requirement, 90, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifteenth_amendment__formal_franchise_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fifteenth_amendment__formal_franchise_reading, fifteenth_amendment__effective_franchise_reading).
narrative_ontology:affects_constraint(fifteenth_amendment__formal_franchise_reading, voting_rights_act_1965__preclearance_mandate).
narrative_ontology:affects_constraint(fifteenth_amendment__formal_franchise_reading, literacy_test_as_proxy_disfranchisement).

% DUAL FORMULATION NOTE:
% The formal and effective franchise readings of the Fifteenth Amendment are siblings in a constraint kernel. They have structurally distinct ε values because they reach different mechanisms: the formal reading (this story) confines disfranchisement reach to explicit racial bars (ε=0.68, snare), while the effective reading reaches proxy devices by operational targeting (ε would be lower for proxy mechanisms individually measured, but the effective reading as a constitutional interpretation has higher authority-enforcement coupling). The two readings are not mutually exclusive in logical scope—both cite the Amendment's language—but in practical authority: they represent different institutional choices about what the Amendment permits Congress to regulate. The Voting Rights Act instantiates the effective reading by imposing results-based preclearance, which the formal reading treats as exceeding congressional Section 2 authority. Both readings are live constitutional positions (the formal reading persists in current jurisprudence for provisions outside the VRA's scope); they coexist as competing frameworks held by different authorities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fifteenth_amendment__formal_franchise_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
