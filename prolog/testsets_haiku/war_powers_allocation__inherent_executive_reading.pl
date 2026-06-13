% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__inherent_executive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_powers_allocation__inherent_executive_reading, []).

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
 *   constraint_id: war_powers_allocation__inherent_executive_reading
 *   human_readable: Inherent Executive War Powers Authority (Commander-in-Chief Reading)
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   Under the inherent-executive reading of Article II war powers, the
 *   president as commander-in-chief claims constitutional authority to deploy
 *   military force in defense of national interests without prior
 *   congressional authorization. Congress retains theoretical power of the
 *   purse and can later refuse appropriations or condition them, but the
 *   prospective decision to use force rests with the executive. This reading
 *   is one of three structurally distinct interpretations of the contested
 *   war-powers kernel: it competes with congressional-primacy reading
 *   (authorization required as constitutional necessity) and
 *   functional-accommodation reading (unilateral authority only for imminent
 *   threats). The constraint instantiates THIS reading and measures its
 *   structural consequences: Congress enters the victim set; presidential
 *   flexibility becomes the beneficiary; suppression is moderate because
 *   constraint persistence depends on political question doctrine and
 *   institutional deference rather than active coercion. The claim/metric gap
 *   is intentional: the constraint is CLAIMED as tangled_rope (coordination +
 *   enforcement) while measuring what appears to be moderately extractive
 *   institutional power; the divergence from pure rope is exactly the point —
 *   Congress accepts diluted authority as the cost of avoiding the political
 *   friction that would come from reasserting prospective veto.
 *
 * KEY AGENTS:
 *   - president_as_commander_in_chief: Claims inherent constitutional authority; sets the agenda for force deployment; exit would require constitutional amendment or forced judicial intervention
 *   - congress_as_institutional_actor: Theoretically retains appropriations power; operationally constrained by political cost of appearing weak on military commitments; secondary role as beneficiary (benefits from stable military framework even as authority is transferred)
 *   - military_command_structure: Receives clear command authority; trapped in chain of command; benefits from operational speed without authorization delays
 *   - service_members: Bear the direct cost (physical, psychological, mortality risk) of deployments authorized unilaterally; constrained exit (contractual duty, desertion consequences)
 *   - affected_populations_in_theater: Powerless victims; experience military force without consent, without formal war declaration, without legitimacy from democratic authorization
 *   - supreme_court_as_observer: Holds theoretical authority to constrain war powers but has abstained via political question doctrine; observes without enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__inherent_executive_reading, 0.68).
domain_priors:suppression_score(war_powers_allocation__inherent_executive_reading, 0.42).
domain_priors:theater_ratio(war_powers_allocation__inherent_executive_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__inherent_executive_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__inherent_executive_reading, "Inherent Executive War Powers Authority (Commander-in-Chief Reading)").
narrative_ontology:topic_domain(war_powers_allocation__inherent_executive_reading, "constitutional/political").

domain_priors:requires_active_enforcement(war_powers_allocation__inherent_executive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__inherent_executive_reading, '15d22942-4a1b-4f9b-a771-b0da42921951').
narrative_ontology:cs_kernel_codification('15d22942-4a1b-4f9b-a771-b0da42921951', fixed_text).
narrative_ontology:cs_authority_grounding('15d22942-4a1b-4f9b-a771-b0da42921951', lineage).
narrative_ontology:cs_interpretation_layer_present('15d22942-4a1b-4f9b-a771-b0da42921951').
narrative_ontology:cs_reading_relation('15d22942-4a1b-4f9b-a771-b0da42921951', war_powers_allocation__congressional_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('15d22942-4a1b-4f9b-a771-b0da42921951', war_powers_allocation__functional_accommodation_reading, coexists_with).
narrative_ontology:cs_axiom('15d22942-4a1b-4f9b-a771-b0da42921951', foundational, commander_in_chief_inherent_authority).
narrative_ontology:cs_axiom_status(commander_in_chief_inherent_authority, holdable).
narrative_ontology:cs_axiom_grounding('15d22942-4a1b-4f9b-a771-b0da42921951', commander_in_chief_inherent_authority, deontological).
narrative_ontology:cs_axiom('15d22942-4a1b-4f9b-a771-b0da42921951', foundational, executive_military_initiative_primacy).
narrative_ontology:cs_axiom_status(executive_military_initiative_primacy, holdable).
narrative_ontology:cs_axiom_grounding('15d22942-4a1b-4f9b-a771-b0da42921951', executive_military_initiative_primacy, instrumental).
narrative_ontology:cs_reference_frame('15d22942-4a1b-4f9b-a771-b0da42921951', commander_in_chief_supremacy_framework).
narrative_ontology:cs_drift_state('15d22942-4a1b-4f9b-a771-b0da42921951', post_cold_war_institutional_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('15d22942-4a1b-4f9b-a771-b0da42921951', '').
narrative_ontology:cs_kernel_id(war_powers_allocation__inherent_executive_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__inherent_executive_reading, executive_branch).
narrative_ontology:constraint_beneficiary(war_powers_allocation__inherent_executive_reading, presidential_operational_flexibility).
narrative_ontology:constraint_victim(war_powers_allocation__inherent_executive_reading, congress_as_institutional_actor).
narrative_ontology:constraint_victim(war_powers_allocation__inherent_executive_reading, war_policy_deliberation_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(war_powers_allocation__inherent_executive_reading, congress_as_institutional_actor).
narrative_ontology:constraint_beneficiary(war_powers_allocation__inherent_executive_reading, military_command_structure).
narrative_ontology:constraint_victim(war_powers_allocation__inherent_executive_reading, service_members).
narrative_ontology:constraint_victim(war_powers_allocation__inherent_executive_reading, affected_populations_in_theater).
narrative_ontology:constraint_vindicates(war_powers_allocation__inherent_executive_reading, executive_prerogative_doctrine).
narrative_ontology:constraint_vindicates(war_powers_allocation__inherent_executive_reading, commander_in_chief_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the agenda for military deployments, commands the armed forces directly, can initiate unilateral action to defend national interests. Under this reading, possesses inherent constitutional authority derived from commander-in-chief clause. Exit from authority would require constitutional amendment, Supreme Court repudiation, or electoral defeat — all politically costly. Operates with broad operational discretion and frames the national interest narrative.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, president_as_commander_in_chief, agenda_setter,
    institutional, biographical, constrained, global).

% Retains appropriations power and theoretical authority to refuse funding, but operates under this reading with prospective war-authorization authority transferred to the executive. Can withhold funds or condition appropriations post-hoc, but doing so carries political cost of appearing weak on national security. Theoretically benefits from stable military framework and cannot exit without redefining its institutional role. Constrained not by legal barrier but by political asymmetry: the initiative belongs to presidency; Congress reacts.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, congress_as_institutional_actor, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(war_powers_allocation__inherent_executive_reading, congress_as_institutional_actor, beneficiary).

% Receives clear operational command authority from the president without requirement for legislative authorization between deployments. Chain of command operates without ambiguity about authorization source. Trapped: cannot refuse presidential orders without institutional dissolution; professional advancement depends on executing presidential directives faithfully. Benefits from operational speed and clarity.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, military_command_structure, beneficiary,
    institutional, biographical, trapped, global).

% Bear the direct physical and psychological costs of deployments authorized unilaterally by the executive. Contractually obligated to serve; exit requires discharge (most exit mechanisms) or desertion (criminal penalty). Cannot contest presidential deployment orders through normal constitutional channels. Risk of death, injury, psychological trauma is imposed without their consent and without access to prospective legislative decision-making.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, service_members, payer,
    moderate, biographical, constrained, global).

% Experience military force deployed without their consent and without formal declaration of war or international treaty authorization legitimating the action. Civilians in conflict zones bear casualties, displacement, property destruction, long-term trauma. Have no seat at the U.S. constitutional table and no recourse to U.S. legal remedies. Cannot exit the constraint except by fleeing or enduring.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, affected_populations_in_theater, payer,
    powerless, immediate, trapped, universal).

% Groups opposing particular wars or seeking congressional prospective authorization have no formal veto mechanism; their opposition enters only after the operation is underway and political momentum favors continuation. Must build post-hoc congressional coalitions to withhold funding or mandate exit rather than participate in prospective authorization decisions. Constrained: cannot easily change the constraint's structure through normal advocacy.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, domestic_war_deliberation_constituencies, excluded,
    organized, biographical, constrained, national).

% Holds theoretical constitutional authority to adjudicate war-powers disputes and could impose prospective constraints on executive unilateral action. Consistently declines to intervene in live military operations, citing political question doctrine. Observes the constraint's operation, documents competing constitutional readings, but does not enforce limits on this reading's operation.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, supreme_court, observer,
    institutional, generational, analytical, national).

% UN Charter, jus ad bellum norms, and international humanitarian law nominally constrain military action and require authorization through international channels or demonstration of legitimate self-defense. This reading treats international legal requirements as advisory rather than binding on U.S. executive constitutional authority. International bodies are excluded from the U.S. domestic constitutional question.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, international_law_regimes, excluded,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Centralizes military deployment authority in the president to enable rapid response to emerging security threats without legislative deliberation delays. Solves the coordination problem of slow institutional response in time-sensitive crises where strategic advantage requires speed of decision-making and unified command.
% TRANSFER_FUNCTION: Transfers prospective war-authorization authority from Congress (constitutionally designated via Article I war-clause) to the president (via commander-in-chief authority). Congress retains post-hoc appropriations veto; president gains prospective deployment initiative and legitimacy framing. Flow: presidential unilateral action → military deployment → request for appropriations → congressional post-hoc ratification or defunding.
% ABSENT_VOICES: Congressional delegations arguing for restored legislative prospective authorization (excluded from prospective decision-making); international legal communities arguing UN authorization is binding on U.S. action (excluded via political question doctrine); populations in theaters of operation experiencing the force (excluded from U.S. constitutional process); domestic constituencies opposing the wars (excluded from prospective authorization, must organize post-hoc resistance).
% DISAPPEARANCE_RATIONALE: If this reading's constitutional authority disappeared and congressional prospective authorization became binding requirement, the entire architecture of U.S. military operations since the Cold War would restructure. Standing deployments would require specific authorization; unilateral executive action would become illegal; political dynamics would shift from presidential initiative + legislative post-hoc ratification to shared prospective authority. The constraint's disappearance would require constitutional amendment or Supreme Court repudiation of political question doctrine, but if either occurred, military operations would reorganize around a different authorization structure.
% FOUNDING_PROBLEM: Early Cold War need for executive flexibility in response to rapidly emerging communist threats in Korea, Berlin, and Southeast Asia, and nuclear-age requirement for speed of decision-making where deliberative legislative process created unacceptable delays in strategic response to imminent threats.
% FOUNDING_PROBLEM_CORROBORATION: Presidents and executive-branch constitutional lawyers attest the founding problem (threat speed, strategic flexibility) is still live in the context of terrorism and peer-state competition. Congress, constitutional scholars outside the executive branch, and legal analysts from international law communities attest the founding problem is substantially solved by modern communication technology and surveillance capability (decision speed is no longer a binding constraint), and that the reading persists as institutional power asymmetry rather than emergency response. Legislative testimony and scholarly consensus from outside the benefiting parties (Office of Legal Counsel, etc.) support the institutional-power-persistence reading over the emergency-response justification.
narrative_ontology:disappearance_verdict(war_powers_allocation__inherent_executive_reading, world_rearranges).
narrative_ontology:founding_problem_status(war_powers_allocation__inherent_executive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__inherent_executive_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(war_powers_allocation__inherent_executive_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_powers_allocation__inherent_executive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_powers_allocation__inherent_executive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_powers_allocation__inherent_executive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness reaches 0.68 (high) by interval end because presidential unilateral authority transfers a constitutionally designated power from the legislative branch to the executive without surrendering the power formally — Congress retains theoretical appropriations veto but exercises it under political asymmetry (appearing weak on military commitments is costly). The measurement series shows extractiveness rising from 0.48 to 0.68 over 75 years, indicating institutional power accumulation rather than emergency response (the founding problem was Cold War threat speed; modern communication removes that justification, yet the constraint persists and strengthens). Theater_ratio at 0.38 reflects moderate performative activity: constitutional framing and war-powers rhetoric continue despite the substantive shift in authority. Suppression is low (0.42) because constraint persistence depends on legal precedent (political question doctrine, structural deference) rather than active force; Congress could theoretically contest but internalizes the authority loss. Accessibility_collapse at 0.52 reflects that alternatives (forcing congressional prospective authorization) remain available to Congress but are costly — the collapse is partial, asymmetric. Resistance at 0.71 reflects sustained institutional contest: courts have never fully endorsed inherent authority (they abstain rather than decide), Congress continues to debate war powers, and legal scholars outside the executive dissent — the reading is not accepted as natural law but as strategic institutional claim. The shared time grid ensures every metric is authored at every examined point; the trajectory shows a constraint that began as emergency response and has matured into stable institutional power transfer.
 *
 * PERSPECTIVAL GAP:
 *   From the presidential seat, this is genuine coordination: rapid response to threats, operational clarity, unambiguous command authority — a functional necessity. From Congress's institutional seat, the same structure is enforced authority loss: formal power retained but political costs of using it are prohibitive, legitimacy of war decisions transferred to unilateral executive action, prospective deliberation supplanted by post-hoc appropriations. Service members experience it as mandatory risk imposed without their consent to deploy. Affected populations experience it as unilateral force without formal democratic authorization. The engine computes these divergences from the structural data: same constraint, different directionality per seat, different effective extraction per seat. The presidency benefits (d near 0.0 = beneficiary end); Congress as institutional actor is constrained-payer (d high = target end); service members are trapped payers (d = 1.0 = full target); affected populations experience extracted legitimacy (d = 1.0). The single constraint produces multiple types when classified per seat because the structural relationships differ radically.
 *
 * DIRECTIONALITY LOGIC:
 *   President (institutional power, unconstrained exit via electoral cycle): d ≈ 0.1 (full beneficiary of authority transfer; exit would require political defeat, not structural barrier). Congress (institutional power, constrained exit via legitimacy logic): d ≈ 0.75 (high target; formal power retained but political costs make exit prohibitive; transfers authority but retains responsibility for defunding, creating political asymmetry). Service members (moderate power, trapped exit): d = 0.95 (near-full target; contractual obligation to serve; cannot refuse deployments; bear direct cost). Affected populations (powerless, trapped exit): d = 1.0 (full extraction; no consent mechanism, no veto, no formal recourse; bear casualties without legitimacy). No directionality overrides needed; derivation from beneficiary/victim + exit maps cleanly to these seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint shows mandatrophy dynamics: founded to solve an emergency-response coordination problem (Cold War threat speed), the founding problem has substantially degraded (modern communication eliminates decision-speed justification), yet the constraint persists at high extractiveness (0.68) and continues to accumulate institutional power (theater_ratio rising). This is classic zombie constraint: the original mandate (rapid response to existential threats) no longer drives enforcement; institutional power maintenance drives persistence. Congress theoretically could revoke the constraint by passing legislation requiring prospective authorization, but political costs make revocation unlikely (appearing weak on national security). The measurement series' rising extractiveness despite stable theater_ratio suggests mandate drift: the operational justification weakens but the power transfer strengthens. This supports the tangled_rope classification: genuine coordination problem (response speed) is now decoupled from the constraint's operation, leaving pure authority extraction. A pure rope reading (coordination without extraction) would predict flat or declining extractiveness as the founding problem faded; instead, extractiveness rises, indicating the constraint's social function has shifted from coordination to power distribution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is commander-in-chief power best read as granting inherent unilateral authority to deploy force, or as requiring congressional authorization except in cases of imminent defense?',
    'This omega documents the reading itself: the constraint instantiates ONE reading of a contested kernel (war_powers_allocation). Sibling readings declare congressional_primacy_reading and functional_accommodation_reading. The constraint''s structural viability depends entirely on which reading the interpreter adopts; the engine''s classification task is to measure the structural consequences of THIS reading, not to adjudicate between readings. Foreclosure between readings is the domain of constitutional amendment, Supreme Court binding precedent, or institutional settlement after armed conflict.',
    'If the congressional_primacy reading were accepted as binding, the entire constraint vanishes; if the functional_accommodation reading were adopted, the constraint would split into multiple context-dependent constraints with different ε values. This reading''s persistence in practice depends on executive institutional control of war operations and on courts'' continued invocation of political question doctrine.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'The constraint is one reading of a kernel contest; its identity depends on the reading frame.').

omega_variable(
    emergency_vs_institutional_power,
    'Does the inherent authority reading persist because it solves an enduring emergency-response coordination problem, or because it constitutionalizes executive institutional advantage?',
    'Counterfactual institutional experiment: restore full congressional prospective authorization requirement and measure (a) whether U.S. security outcomes measurably degrade due to decision speed, or (b) whether the constraint persists as political asymmetry even under statutory requirements for authorization. Modern communication and surveillance technology have substantially reduced the decision-speed justification since the reading''s founding; empirical test would require legislative adoption of prospective authorization requirements and observation of operational friction.',
    'If the emergency justification is real and enduring, the constraint solves a genuine coordination problem (tangled_rope classification holds). If the justification has evaporated and the reading persists as pure power asymmetry, the constraint approaches snare. The measured extraction (0.68) and theater_ratio (0.38) suggest emergencies are no longer the primary driver; institutional power maintenance is.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergency_vs_institutional_power, empirical, 'Whether the reading''s constitutional justification tracks actual war-response needs or institutional power asymmetry.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is suppression of congressional prospective authority structural (legal precedent, political question doctrine, judicial non-intervention) or internalized (Congress has accepted diminished authority and no longer contests it)?',
    'Post-constraint-removal trajectory: if Congress reasserted prospective authorization authority and the constraint collapsed, the structural suppression would be revealed. If Congress reasserted authority but political pressure still muted its use (members voting to fund wars they opposed), the suppression would be partially internalized. Current low suppression (0.42) reflects that the constraint''s persistence depends lightly on active enforcement (courts don''t intervene, Congress accepts the framing) rather than coercive pressure — suggesting structural suppression (legal precedent) dominates.',
    'If suppression is primarily structural, removal requires constitutional or legal change. If internalized, Congress could simply reassert authority without formal amendment. The low measured suppression may understate the constraint''s grip if internalization is high — Congress may no longer have the institutional will to contest, independent of legal barriers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Whether suppression of congressional authority is structural precedent or internalized institutional deference.').

omega_variable(
    appropriations_as_ratification,
    'Does congressional appropriation of war funds constitute genuine post-hoc ratification that preserves legislative constraint, or does it constitute de facto surrender of prospective authority?',
    'Behavioral experiment: Congress withholds appropriations for a sustained operation despite presidential insistence that it is ongoing. If the operation halts or degrades significantly, appropriations remain a live constraint. If the operation persists (presidents claim power to obligate funds, redeploy from other accounts), appropriations are the formal veto Congress no longer exercises because the political cost is too high.',
    'If appropriations function as real constraint, the tangled_rope reading (coordination + extraction + enforcement) holds — Congress retains veto power even if it rarely uses it. If appropriations are formal theater while the constraint persists operationally, the reading approaches snare (extraction with suppressed alternative authority).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(appropriations_as_ratification, empirical, 'Whether appropriations-based control is a live constraint or a theoretical option Congress has internalized as unavailable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__inherent_executive_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t0, war_powers_allocation__inherent_executive_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(war__tr_t0, observed).
narrative_ontology:measurement(war__tr_t12, war_powers_allocation__inherent_executive_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement_basis(war__tr_t12, observed).
narrative_ontology:measurement(war__tr_t25, war_powers_allocation__inherent_executive_reading, theater_ratio, 25, 0.32).
narrative_ontology:measurement_basis(war__tr_t25, observed).
narrative_ontology:measurement(war__tr_t38, war_powers_allocation__inherent_executive_reading, theater_ratio, 38, 0.35).
narrative_ontology:measurement_basis(war__tr_t38, observed).
narrative_ontology:measurement(war__tr_t50, war_powers_allocation__inherent_executive_reading, theater_ratio, 50, 0.37).
narrative_ontology:measurement_basis(war__tr_t50, observed).
narrative_ontology:measurement(war__tr_t62, war_powers_allocation__inherent_executive_reading, theater_ratio, 62, 0.38).
narrative_ontology:measurement_basis(war__tr_t62, observed).
narrative_ontology:measurement(war__tr_t75, war_powers_allocation__inherent_executive_reading, theater_ratio, 75, 0.38).
narrative_ontology:measurement_basis(war__tr_t75, observed).

% Extraction over time
narrative_ontology:measurement(war__be_t0, war_powers_allocation__inherent_executive_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(war__be_t0, observed).
narrative_ontology:measurement(war__be_t12, war_powers_allocation__inherent_executive_reading, base_extractiveness, 12, 0.54).
narrative_ontology:measurement_basis(war__be_t12, observed).
narrative_ontology:measurement(war__be_t25, war_powers_allocation__inherent_executive_reading, base_extractiveness, 25, 0.61).
narrative_ontology:measurement_basis(war__be_t25, observed).
narrative_ontology:measurement(war__be_t38, war_powers_allocation__inherent_executive_reading, base_extractiveness, 38, 0.65).
narrative_ontology:measurement_basis(war__be_t38, observed).
narrative_ontology:measurement(war__be_t50, war_powers_allocation__inherent_executive_reading, base_extractiveness, 50, 0.67).
narrative_ontology:measurement_basis(war__be_t50, observed).
narrative_ontology:measurement(war__be_t62, war_powers_allocation__inherent_executive_reading, base_extractiveness, 62, 0.68).
narrative_ontology:measurement_basis(war__be_t62, observed).
narrative_ontology:measurement(war__be_t75, war_powers_allocation__inherent_executive_reading, base_extractiveness, 75, 0.68).
narrative_ontology:measurement_basis(war__be_t75, observed).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t0, war_powers_allocation__inherent_executive_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(war__su_t0, observed).
narrative_ontology:measurement(war__su_t12, war_powers_allocation__inherent_executive_reading, suppression_requirement, 12, 0.38).
narrative_ontology:measurement_basis(war__su_t12, observed).
narrative_ontology:measurement(war__su_t25, war_powers_allocation__inherent_executive_reading, suppression_requirement, 25, 0.4).
narrative_ontology:measurement_basis(war__su_t25, observed).
narrative_ontology:measurement(war__su_t38, war_powers_allocation__inherent_executive_reading, suppression_requirement, 38, 0.41).
narrative_ontology:measurement_basis(war__su_t38, observed).
narrative_ontology:measurement(war__su_t50, war_powers_allocation__inherent_executive_reading, suppression_requirement, 50, 0.42).
narrative_ontology:measurement_basis(war__su_t50, observed).
narrative_ontology:measurement(war__su_t62, war_powers_allocation__inherent_executive_reading, suppression_requirement, 62, 0.42).
narrative_ontology:measurement_basis(war__su_t62, observed).
narrative_ontology:measurement(war__su_t75, war_powers_allocation__inherent_executive_reading, suppression_requirement, 75, 0.42).
narrative_ontology:measurement_basis(war__su_t75, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__inherent_executive_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(war_powers_allocation__inherent_executive_reading, 0.12).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, war_powers_allocation__congressional_primacy_reading).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, war_powers_allocation__functional_accommodation_reading).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, appropriations_as_war_authorization_mechanism).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, separation_of_powers_institutional_capture).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the war_powers_allocation kernel. Sibling readings (congressional_primacy_reading, functional_accommodation_reading) represent competing constitutional framings of the same textual provision (Article II, Section 2). Each reading instantiates different beneficiary/victim structures and different ε values. Network edges reflect family kinship: all three readings affect downstream constraints about appropriations and institutional capture. The network preserves the constraint family structure: upstream siblings (more established readings) influence downstream interpretations (more contested institutional applications).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(war_powers_allocation__inherent_executive_reading, institutional, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
