% ============================================================================
% CONSTRAINT STORY: combatant_status_definition__national_liberation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_combatant_status_definition__national_liberation_reading, []).

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
 *   constraint_id: combatant_status_definition__national_liberation_reading
 *   human_readable: National Liberation Combatant Status Extension (AP I Article 1(4))
 *   domain: legal/humanitarian/international
 *
 * SUMMARY:
 *   Additional Protocol I (1977) declares that armed conflicts in which
 *   peoples fight colonial domination, alien occupation, or racist regimes in
 *   exercise of their right of self-determination are international
 *   conflicts, and relaxes the combatant criteria for such fighters: a
 *   movement that is organized under responsible command and whose members
 *   carry arms openly during engagements and deployments earns its captured
 *   members prisoner-of-war status. This story authors THAT arrangement - the
 *   national-liberation extension - as a single epsilon-invariant constraint.
 *   It is one reading of the combatant_status_definition kernel; the
 *   state-centric reading (categorical exclusion of non-state actors) and the
 *   functional-protection reading (status-independent baseline protections)
 *   are separate constraint files with their own epsilon values,
 *   beneficiary/victim structures, and classifications, linked through
 *   network.affects_constraints. Per the kernel-reading rule, the contest
 *   between readings is NOT described inside this constraint; it is routed to
 *   the kernel_reading_position omega. KEY AGENTS (by structural
 *   relationship): - colonial_occupation_powers: primary target
 *   (institutional/arbitrage) - bears the obligation to grant immunity and
 *   POW treatment, exits via non-ratification and denial of qualification; -
 *   national_liberation_movements: primary beneficiary
 *   (organized/constrained) - collect recognition and protection conditional
 *   on discipline; - captured_liberation_fighters: direct protected seat
 *   (powerless/trapped) - hold or lack POW status by decision of their
 *   captor; - occupied_civilian_populations: mixed population seat
 *   (powerless/trapped) - gain distinguishability, absorb blurring costs; -
 *   ap_i_high_contracting_parties: agenda-setter (institutional/mobile) -
 *   wrote and police the text; - non_ratifying_military_powers: excluded
 *   objector (institutional/arbitrage); - icrc: custodian-observer
 *   (institutional/analytical).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__national_liberation_reading, 0.62).
domain_priors:suppression_score(combatant_status_definition__national_liberation_reading, 0.52).
domain_priors:theater_ratio(combatant_status_definition__national_liberation_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, accessibility_collapse, 0.46).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__national_liberation_reading, tangled_rope).
narrative_ontology:human_readable(combatant_status_definition__national_liberation_reading, "National Liberation Combatant Status Extension (AP I Article 1(4))").
narrative_ontology:topic_domain(combatant_status_definition__national_liberation_reading, "legal/humanitarian/international").

domain_priors:requires_active_enforcement(combatant_status_definition__national_liberation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__national_liberation_reading, '39bc89de-842d-4c7c-bda5-f8d81782f114').
narrative_ontology:cs_kernel_codification('39bc89de-842d-4c7c-bda5-f8d81782f114', fixed_text).
narrative_ontology:cs_authority_grounding('39bc89de-842d-4c7c-bda5-f8d81782f114', lineage).
narrative_ontology:cs_interpretation_layer_present('39bc89de-842d-4c7c-bda5-f8d81782f114').
narrative_ontology:cs_reading_relation('39bc89de-842d-4c7c-bda5-f8d81782f114', combatant_status_definition__state_centric_reading, forecloses).
narrative_ontology:cs_reading_relation('39bc89de-842d-4c7c-bda5-f8d81782f114', combatant_status_definition__functional_protection_reading, coexists_with).
narrative_ontology:cs_axiom('39bc89de-842d-4c7c-bda5-f8d81782f114', foundational, wars_of_self_determination_are_international_conflicts).
narrative_ontology:cs_axiom_status(wars_of_self_determination_are_international_conflicts, holdable).
narrative_ontology:cs_axiom_grounding('39bc89de-842d-4c7c-bda5-f8d81782f114', wars_of_self_determination_are_international_conflicts, conventional).
narrative_ontology:cs_axiom('39bc89de-842d-4c7c-bda5-f8d81782f114', foundational, organized_liberation_fighters_deserve_combatant_immunity).
narrative_ontology:cs_axiom_status(organized_liberation_fighters_deserve_combatant_immunity, holdable).
narrative_ontology:cs_axiom_grounding('39bc89de-842d-4c7c-bda5-f8d81782f114', organized_liberation_fighters_deserve_combatant_immunity, deontological).
narrative_ontology:cs_axiom('39bc89de-842d-4c7c-bda5-f8d81782f114', secondary, criteria_gating_preserves_distinction).
narrative_ontology:cs_axiom_status(criteria_gating_preserves_distinction, holdable).
narrative_ontology:cs_axiom_grounding('39bc89de-842d-4c7c-bda5-f8d81782f114', criteria_gating_preserves_distinction, instrumental).
narrative_ontology:cs_reference_frame('39bc89de-842d-4c7c-bda5-f8d81782f114', self_determination_inclusive_combatancy).
narrative_ontology:cs_drift_state('39bc89de-842d-4c7c-bda5-f8d81782f114', contemporary_asymmetric_conflict_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('39bc89de-842d-4c7c-bda5-f8d81782f114', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__national_liberation_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__national_liberation_reading, national_liberation_movements).
narrative_ontology:constraint_beneficiary(combatant_status_definition__national_liberation_reading, captured_liberation_fighters).
narrative_ontology:constraint_beneficiary(combatant_status_definition__national_liberation_reading, occupied_civilian_populations).
narrative_ontology:constraint_victim(combatant_status_definition__national_liberation_reading, colonial_occupation_powers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(combatant_status_definition__national_liberation_reading, ap_i_high_contracting_parties).
narrative_ontology:constraint_victim(combatant_status_definition__national_liberation_reading, occupied_civilian_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Armed wings of peoples fighting colonial domination, alien occupation, or racist regimes. The arrangement offers them a path to lawful-combatant recognition if they maintain organizational structure, responsible command, and open carriage of arms during operations. Declining the terms means fighting as categorically criminalized insurgents whose captured members can be prosecuted or executed; accepting them means discipline costs, command accountability, and exposure of their fighters at the moment of attack, in exchange for protected status for anyone captured and a claim to legitimacy as a party to an international conflict.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, national_liberation_movements, beneficiary,
    organized, generational, constrained, regional).

% Members of qualifying movements who fall into the enemy's hands. If their movement is recognized as meeting the criteria, they hold prisoner-of-war status: shielded from criminal prosecution for lawful acts of war, entitled to detention safeguards, and repatriated at hostilities' end. If recognition is denied, they face trial as criminals or saboteurs with penalties up to death. They control nothing about the determination; their protection depends entirely on a status decision made by the power detaining them.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, captured_liberation_fighters, beneficiary,
    powerless, immediate, trapped, local).

% States administering colonies, occupations, or racially structured regimes in territories where armed resistance operates. The arrangement obliges them to treat fighters of qualifying movements as lawful combatants: extending immunity for acts of war, owing prisoner-of-war treatment on capture, and surrendering the option of prosecuting resistance as ordinary crime. Their exits are short of formal withdrawal: declining to ratify the protocol, denying that a particular conflict falls within the trigger classes, or judging the opposing movement non-compliant - routes most major military powers have in fact taken.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, colonial_occupation_powers, payer,
    institutional, generational, arbitrage, global).

% The states that adopted and ratified Protocol I and thereby wrote the extension into treaty law, predominantly a post-colonial bloc that carried the provision through the 1974-1977 diplomatic conference. They control invocation and interpretation through diplomatic conferences, decide which conflicts get formally characterized as qualifying, and have collectively declined to amend or withdraw the provision despite the friction it generates with allied military powers. Most collect reputational and precedential value from the self-determination framing.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, ap_i_high_contracting_parties, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(combatant_status_definition__national_liberation_reading, ap_i_high_contracting_parties, beneficiary).

% Civilians living in territories under colonial or military administration where liberation armed groups operate. They gain when the arrangement pulls insurgent violence into commanded, distinguishable channels - fighters who must carry arms openly and answer to a command structure are easier to tell apart from civilians - and when captured fighters hold protected status rather than disappearing into punitive detention. They lose when fighters blend into civilian space and the detaining power responds with collective punishments, curfews, and sweeps that fall on everyone.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, occupied_civilian_populations, beneficiary,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(combatant_status_definition__national_liberation_reading, occupied_civilian_populations, payer).

% Major military powers that declined to ratify Protocol I, several having objected during negotiation that the liberation extension would confer combatant legitimacy on irregular fighters, blur the combatant-civilian line, and hand propaganda victories to armed opposition movements. They sit outside the ratified arrangement and outside the conference rooms where its interpretation is negotiated, yet their detention practices and doctrinal writings weigh heavily on whether the extension consolidates as custom.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, non_ratifying_military_powers, excluded,
    institutional, generational, arbitrage, global).

% Custodian and promoter of the Geneva body of law. Visits detainees held by all sides in qualifying and non-qualifying conflicts alike, published the authoritative commentary on the protocol's provisions at adoption, and documents - without adjudicating - where invoking states apply the extension and where they quietly decline to. Its institutional interest lies in the coherence and authority of the legal category system as a whole.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, icrc, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(combatant_status_definition__national_liberation_reading, captured_liberation_fighters).
narrative_ontology:fixing_cost_class(combatant_status_definition__national_liberation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Regularizes asymmetric conflict between peoples and the powers administering their territories by publishing verifiable criteria - organization, responsible command, open carriage of arms - under which insurgent fighters are treated as lawful combatants. It solves a real classification problem: how to extend soldier-like status and protection to anti-colonial and anti-occupation fighters without either criminalizing them wholesale or dissolving the combatant-civilian distinction that protects everyone else.
% TRANSFER_FUNCTION: Moves legal recognition and protective status - combatant immunity, prisoner-of-war treatment, exemption from prosecution for lawful acts of war - from the detaining or occupying power to captured members of qualifying liberation movements; correspondingly moves prosecutorial discretion over captured fighters away from the territorial sovereign.
% ABSENT_VOICES: Non-ratifying military powers - most prominently the United States and Israel, which objected during negotiation that the extension legitimized irregular violence - are outside the ratified conversation entirely; their objections survive only in negotiating-record reservations and doctrinal writing. Settler and garrison communities in contested territories, whose security concerns motivated much of the occupying-side objection, likewise had no seat in the provision's design. They are kept out by the treaty-consensus structure itself, not by any procedural exclusion they could appeal.
% DISAPPEARANCE_RATIONALE: If the extension vanished overnight, captured liberation fighters would revert to unlawful-participant classification subject to criminal trial or execution; movements would lose the incentive structure that trades discipline for protected status, weakening the pull toward commanded, distinguishable conduct; occupying powers would regain unrestricted prosecutorial discretion over resistance; and the self-determination-conflict framework built on Article 1(4) - UN characterizations, ICRC operational assumptions, post-colonial legal doctrine - would reorganize around ad hoc detention practice.
% FOUNDING_PROBLEM: Wars of national liberation against colonial domination were treated as internal rebellions, letting colonial and occupying powers execute or imprison captured fighters as murderers and bandits rather than soldiers. The arrangement was built to secure soldier-like dignity and protection for fighters in self-determination struggles, and to give those fighters a concrete reason - protected status on capture - to fight within the laws of war.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set by the ICRC's official commentary on Protocol I (which records the drafting intent to bring liberation wars inside the international-conflict framework), by successive UN General Assembly resolutions affirming the international character of such conflicts, and by the academic IHL literature. Former colonial and occupying-power governments attest the opposite on status: that classic colonial domination has ended and the trigger classes are being stretched to cover ordinary insurgencies - so the problem's liveness is affirmed by humanitarian and multilateral sources and disputed by the powers that would bear the obligation.
narrative_ontology:disappearance_verdict(combatant_status_definition__national_liberation_reading, world_rearranges).
narrative_ontology:founding_problem_status(combatant_status_definition__national_liberation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(combatant_status_definition__national_liberation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(combatant_status_definition__national_liberation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(combatant_status_definition__national_liberation_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(combatant_status_definition__national_liberation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(combatant_status_definition__national_liberation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(combatant_status_definition__national_liberation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. The claimed type is tangled_rope because the structure satisfies all three canonical conditions: a genuine coordination function (publishing verifiable criteria that solve the classification problem for asymmetric war and give fighters a concrete incentive toward commanded, distinguishable conduct), asymmetric payment through the same structure (the occupying power owes immunity and POW treatment while receiving no equivalent concession - the expected structural delta names high burden for occupying powers and only moderate, conditional burden for liberation movements), and dependence on active enforcement (qualification determinations, ICRC access, compliance pressure; no self-executing mechanism). Extractiveness 0.62 reflects that asymmetry: the arrangement's principal cost lands on the detaining power, moderated by its arbitrage-grade exits. Suppression 0.52 is moderate: entry was consensual (ratification), but once inside, the obligation binds and the arrangement actively forecloses the criminalization strategy occupying powers prefer; suppression is authored as a raw structural property and is deliberately NOT scaled by scope or directionality - the engine owns that arithmetic. Accessibility collapse 0.46: alternatives persist (stay outside the treaty, deny the trigger, judge the movement non-compliant), so understanding the arrangement does not close the option set. Resistance 0.61: sustained - major powers refused ratification over precisely this provision, and occupying powers resist invocation in practice. Theater 0.34: the underlying Geneva-III machinery is real and used, but the extension itself is invoked far more often as normative assertion than as applied status determination - an invocation gap that widens slowly over the interval. The measurement series run on ONE shared grid (t = 0, 8, 16, 24, 32, 40, 48; adoption-era 1977 to the present) with every tracked metric authored at every point, so no scalar substitution injects end-state values into earlier times. The suppression_requirement series is authored deliberately: the story traces enforcement-capacity maturation (ratification growth through decolonization, consolidation of ICRC detention-access practice, the Article 90 fact-finding machinery), a rising trajectory from 0.30 to 0.52. Base extractiveness rises monotonically 0.35 to 0.62 as the treaty consolidated and the normative burden concentrated on occupying powers; theater drifts up gently as the invocation gap grows. No cyclical dynamics are authored - the trajectory is maturation, not oscillation.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and the divergence is the finding. From the colonial_occupation_powers seat, the arrangement is an imposed obligation: it strips the criminalization tool, rewards their opponents with legitimacy, and is administered by a post-colonial majority bloc - with arbitrage exit damping but not removing the burden (non-ratification carries reputational cost, and customary-consolidation pressure threatens the exit itself). From the national_liberation_movements and captured_liberation_fighters seats, the same structure is conditional protection: real, valued, but gated by criteria their adversary adjudicates. From the ap_i_high_contracting_parties seat, it is a settled normative achievement. The engine computes per-seat classifications from the structural data (power, exit, directionality derived from the beneficiary/victim declarations); the authored claim does not adjudicate between these experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for national_liberation_movements, captured_liberation_fighters, and (weakly, via the secondary payer position) occupied_civilian_populations; the victim declaration drives colonial_occupation_powers toward the full-target end, with its arbitrage exit options pulling it back from maximum effective burden relative to a trapped target. The agenda_setter seat sits near-symmetric: the contracting parties wrote the rule and collect precedential value while bearing no direct operational cost. Excluded and observer seats contribute no directionality. No directionality_overrides are needed: the derivation from beneficiary/victim data plus exit options reproduces the intended relationships, including the expected structural delta (moderate burden for qualifying movements, high burden for occupying powers).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem's status is contested, and that contest is the live analytical question: classic colonial administration has largely ended, which would strand the original trigger, but alien-occupation cases persist and keep the arrangement attached to live disputes - so the (contested x world_rearranges) cell correctly avoids the dead-mandate zombie flag while flagging the trigger-class stretching documented in the trigger_category_contestation omega. Classification prevents mislabeling in both directions: reading the arrangement as pure extraction (a snare) would erase its real coordination achievement - it solved a genuine classification problem and buys distinguishable, commanded conduct with protected status; reading it as pure coordination (a rope) would erase the asymmetric payment structure through which occupying powers fund the protection of their opponents. Tangled rope holds both facts in one structure. The piton failure mode is guarded against by the receipt surface: gains demonstrably accrue to a named seat (captured fighters), so this is not diffuse-cost inertia; and fixing_cost is prohibitive, so neglect-by-cheap-exit does not apply either.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint instantiates the national_liberation_reading of the combatant_status_definition kernel; the state_centric_reading and functional_protection_reading are separate constraints with their own epsilon values and victim sets. Which reading governs a given legal order changes who counts as a protected fighter at all.',
    'Not resolvable within this file; tracked across the linked sibling stories. Each reading is generated as a clean epsilon-invariant constraint and the corpus compares their computed classifications.',
    'Adopting the state-centric sibling would remove the liberation-movement and captured-fighter beneficiary seats entirely (non-state actors categorically excluded); adopting the functional-protection sibling would detach baseline detention protections from status altogether, shrinking what this reading''s conditional grant adds. The disagreement is located at whether combatant status is a state-monopolized category or conditionally extensible to organized non-state groups.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: one reading of a three-reading kernel; sibling readings are separate files.').

omega_variable(
    trigger_category_contestation,
    'Which concrete conflicts satisfy the trigger classes ''colonial domination'', ''alien occupation'', and ''racist regimes''? The classes are politically charged and no automatic activation test exists.',
    'Case-by-case authoritative determination through UN organ pronouncements, state-party practice, and eventual judicial treatment of specific conflicts.',
    'Activation is the gate for everything else: where no conflict is recognized as qualifying, the arrangement imposes nothing and protects no one; where activated, the full obligation-to-grant-immunity burden falls on the detaining power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trigger_category_contestation, empirical, 'Whether a given conflict falls inside the Article 1(4) trigger classes.').

omega_variable(
    compliance_verification_asymmetry,
    'Whether a detained fighter''s movement actually met the organization, responsible-command, and open-carriage criteria is typically assessed by the detaining power itself - the very party bearing the obligation adjudicates whether the obligation attaches.',
    'Independent fact-finding: International Humanitarian Fact-Finding Commission inquiries, ICRC detention assessments, third-state or tribunal review of status determinations.',
    'Self-assessed non-qualification collapses the protected fighter''s status in practice while leaving the arrangement nominally intact - the beneficiary seat''s protection becomes contingent on the payer seat''s honesty, which would raise effective burden asymmetry beyond the authored epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_verification_asymmetry, empirical, 'Status determinations are made by the obligated party, creating a verification asymmetry.').

omega_variable(
    customary_consolidation_question,
    'Does the Article 1(4)/44(3) extension bind states that never ratified Protocol I, as emerging customary law, or does it remain treaty-bounded?',
    'Systematic tracking of state practice and opinio juris among persistent non-ratifiers, including their battlefield detention policies and any tribunal acceptance of the extension as custom.',
    'If custom, the arbitrage exit of non-ratifying military powers closes and the arrangement''s coercive reach becomes global; if not, the constraint remains confined to ratifying parties and the excluded seats stay outside permanently.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(customary_consolidation_question, empirical, 'Customary-law status of the liberation-fighter extension beyond the treaty parties.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__national_liberation_reading, 0, 48).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comb_tr_t0, combatant_status_definition__national_liberation_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(comb_tr_t8, combatant_status_definition__national_liberation_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(comb_tr_t16, combatant_status_definition__national_liberation_reading, theater_ratio, 16, 0.27).
narrative_ontology:measurement(comb_tr_t24, combatant_status_definition__national_liberation_reading, theater_ratio, 24, 0.29).
narrative_ontology:measurement(comb_tr_t32, combatant_status_definition__national_liberation_reading, theater_ratio, 32, 0.31).
narrative_ontology:measurement(comb_tr_t40, combatant_status_definition__national_liberation_reading, theater_ratio, 40, 0.33).
narrative_ontology:measurement(comb_tr_t48, combatant_status_definition__national_liberation_reading, theater_ratio, 48, 0.34).

% Extraction over time
narrative_ontology:measurement(comb_be_t0, combatant_status_definition__national_liberation_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(comb_be_t8, combatant_status_definition__national_liberation_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(comb_be_t16, combatant_status_definition__national_liberation_reading, base_extractiveness, 16, 0.48).
narrative_ontology:measurement(comb_be_t24, combatant_status_definition__national_liberation_reading, base_extractiveness, 24, 0.53).
narrative_ontology:measurement(comb_be_t32, combatant_status_definition__national_liberation_reading, base_extractiveness, 32, 0.57).
narrative_ontology:measurement(comb_be_t40, combatant_status_definition__national_liberation_reading, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(comb_be_t48, combatant_status_definition__national_liberation_reading, base_extractiveness, 48, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(comb_su_t0, combatant_status_definition__national_liberation_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(comb_su_t8, combatant_status_definition__national_liberation_reading, suppression_requirement, 8, 0.37).
narrative_ontology:measurement(comb_su_t16, combatant_status_definition__national_liberation_reading, suppression_requirement, 16, 0.43).
narrative_ontology:measurement(comb_su_t24, combatant_status_definition__national_liberation_reading, suppression_requirement, 24, 0.47).
narrative_ontology:measurement(comb_su_t32, combatant_status_definition__national_liberation_reading, suppression_requirement, 32, 0.49).
narrative_ontology:measurement(comb_su_t40, combatant_status_definition__national_liberation_reading, suppression_requirement, 40, 0.51).
narrative_ontology:measurement(comb_su_t48, combatant_status_definition__national_liberation_reading, suppression_requirement, 48, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(combatant_status_definition__national_liberation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(combatant_status_definition__national_liberation_reading, combatant_status_definition__state_centric_reading).
narrative_ontology:affects_constraint(combatant_status_definition__national_liberation_reading, combatant_status_definition__functional_protection_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'combatant status in IHL'. The label conflates three structurally distinct claims with different epsilon values and different victim sets: (1) this national-liberation reading - conditional extension of combatant status to organized non-state groups in self-determination conflicts, with occupying powers bearing the obligation side; (2) the state-centric reading - categorical exclusion of non-state actors, with liberation movements as the excluded class; (3) the functional-protection reading - status-independent minimum protections for all detainees, with no status gate at all. This file instantiates reading (2-of-the-three ordering notwithstanding) the national-liberation reading only; the siblings are separate stories linked here. Upstream/downstream: the state-centric reading is the inherited Geneva-III baseline this reading modifies; the functional-protection reading runs on a parallel layer (Common Article 3 / Article 75 baselines) that composes with rather than competes against the status question.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
