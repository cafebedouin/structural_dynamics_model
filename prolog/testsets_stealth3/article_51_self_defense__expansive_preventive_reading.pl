% ============================================================================
% CONSTRAINT STORY: article_51_self_defense__expansive_preventive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_51_self_defense__expansive_preventive_reading, []).

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
 *   constraint_id: article_51_self_defense__expansive_preventive_reading
 *   human_readable: Expansive Preventive Reading of Article 51 Self-Defense
 *   domain: international_law/security_studies
 *
 * SUMMARY:
 *   This story authors ONE reading of the contested Article 51 self-defense
 *   kernel: the expansive preventive reading, under which self-defense
 *   extends to preemptive or preventive force against non-state actors and
 *   emerging threats whenever the acting state judges necessity demonstrated.
 *   The standing arrangement under contest — the operative practice of
 *   self-judged preventive force — is the epsilon referent; the reading's
 *   endorsed alternatives are not scored here. Structurally the reading pairs
 *   a genuine coordination service with asymmetric extraction: it supplies a
 *   lawful-action vocabulary for the veto-paralysis problem, while the
 *   widened action-space accrues to militarily capable states and their
 *   suppliers and the costs land on target-region populations, host states,
 *   and the Security Council's gatekeeping authority. Because necessity is
 *   self-judged, the threshold constrains mainly through reputation and
 *   rhetoric. CONSTRAINT FAMILY NOTE: the colloquial label 'Article 51
 *   self-defense' decomposes into three structurally distinct constraints
 *   distinguished by trigger condition and judging authority. This file
 *   carries the highest epsilon of the three: the narrow armed-attack reading
 *   (separate story) leaves epsilon near coordination-cost levels because
 *   responses to actual or imminent attributable attacks impose mutual
 *   restraint; the unable-unwilling reading (separate story) sits between,
 *   conditioning force on demonstrable host-state failure; this reading
 *   removes the external check, so extraction runs highest. The claim/metric
 *   split is deliberate: claimed_type records tangled_rope as the structure I
 *   believe true; the metrics record descriptive operation independently.
 *
 * KEY AGENTS:
 *   - militarily_capable_states: agenda-setting beneficiary seat (powerful/arbitrage) — publishes the threat assessments, conducts the operations, defends the reading in legal fora, collects the widened action-space
 *   - defense_industrial_sector: secondary beneficiary (organized/arbitrage) — supplies platforms and munitions that preventive posture sustains
 *   - target_region_populations: primary target (powerless/trapped) — bears strikes, displacement, and escalation with no seat where necessity is judged
 *   - un_security_council_authority: institutional target (institutional/constrained) — gatekeeping authority eroded by each unauthorized invocation it cannot punish
 *   - host_states_of_nonstate_actors: exposed intermediary (moderate/constrained) — territories become operating areas on another capital's self-certification
 *   - international_law_community: analytical observer (analytical/analytical) — argues the interpretive questions with no enforcement lever
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__expansive_preventive_reading, 0.68).
domain_priors:suppression_score(article_51_self_defense__expansive_preventive_reading, 0.57).
domain_priors:theater_ratio(article_51_self_defense__expansive_preventive_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, suppression_requirement, 0.57).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__expansive_preventive_reading, tangled_rope).
narrative_ontology:human_readable(article_51_self_defense__expansive_preventive_reading, "Expansive Preventive Reading of Article 51 Self-Defense").
narrative_ontology:topic_domain(article_51_self_defense__expansive_preventive_reading, "international_law/security_studies").

domain_priors:requires_active_enforcement(article_51_self_defense__expansive_preventive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__expansive_preventive_reading, '9793d3e5-8c33-48f6-80ef-1734c4288e2c').
narrative_ontology:cs_kernel_codification('9793d3e5-8c33-48f6-80ef-1734c4288e2c', fixed_text).
narrative_ontology:cs_authority_grounding('9793d3e5-8c33-48f6-80ef-1734c4288e2c', extraction).
narrative_ontology:cs_interpretation_layer_present('9793d3e5-8c33-48f6-80ef-1734c4288e2c').
narrative_ontology:cs_reading_relation('9793d3e5-8c33-48f6-80ef-1734c4288e2c', article_51_self_defense__article_51_narrow_armed_attack_reading, coexists_with).
narrative_ontology:cs_reading_relation('9793d3e5-8c33-48f6-80ef-1734c4288e2c', article_51_self_defense__article_51_unable_unwilling_doctrine_reading, influences).
narrative_ontology:cs_axiom('9793d3e5-8c33-48f6-80ef-1734c4288e2c', foundational, pre_attack_necessity_justifies_force).
narrative_ontology:cs_axiom_status(pre_attack_necessity_justifies_force, holdable).
narrative_ontology:cs_axiom_grounding('9793d3e5-8c33-48f6-80ef-1734c4288e2c', pre_attack_necessity_justifies_force, instrumental).
narrative_ontology:cs_axiom('9793d3e5-8c33-48f6-80ef-1734c4288e2c', foundational, armed_attack_trigger_extends_to_emerging_threats).
narrative_ontology:cs_axiom_status(armed_attack_trigger_extends_to_emerging_threats, holdable).
narrative_ontology:cs_axiom_grounding('9793d3e5-8c33-48f6-80ef-1734c4288e2c', armed_attack_trigger_extends_to_emerging_threats, conventional).
narrative_ontology:cs_reference_frame('9793d3e5-8c33-48f6-80ef-1734c4288e2c', imminence_as_revisable_floor).
narrative_ontology:cs_drift_state('9793d3e5-8c33-48f6-80ef-1734c4288e2c', contemporary, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9793d3e5-8c33-48f6-80ef-1734c4288e2c', '').
narrative_ontology:cs_kernel_id(article_51_self_defense__expansive_preventive_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__expansive_preventive_reading, militarily_capable_states).
narrative_ontology:constraint_beneficiary(article_51_self_defense__expansive_preventive_reading, defense_industrial_sector).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, target_region_populations).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, un_security_council_authority).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, host_states_of_nonstate_actors).
narrative_ontology:constraint_vindicates(article_51_self_defense__expansive_preventive_reading, preemptive_necessity_doctrine).
narrative_ontology:constraint_vindicates(article_51_self_defense__expansive_preventive_reading, expansive_charter_interpretation_tradition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain forces able to project power across borders and decide for themselves when an emerging threat justifies striking first. They publish the threat assessments that ground necessity claims, conduct the operations, and defend the reading's legality in UN debates and legal memoranda. They collect the widened action-space the reading opens; they also absorb blowback, retaliation risk, and the precedent that rivals may cite. Leaving the practice would mean accepting Council gatekeeping they can currently bypass, and shifting between this reading and narrower framings as circumstances suit is always available to them.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, militarily_capable_states, agenda_setter,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article_51_self_defense__expansive_preventive_reading, militarily_capable_states, beneficiary).

% Supplies the platforms, munitions, and surveillance systems that preventive operations consume. Threat framing that widens the range of foreseeable engagements sustains procurement demand beyond what reactive defense postures would generate. Revenue follows contracts; portfolio diversification across clients and missions gives them wide latitude regardless of how any single legal debate resolves.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, defense_industrial_sector, beneficiary,
    organized, immediate, arbitrage, global).

% Live where preventive strikes, follow-on raids, and the destabilization they trigger actually land. They bear casualties, displacement, destroyed infrastructure, and the recruitment dynamics that prolonged campaigns feed. They have no seat in the councils where necessity is judged and no practical ability to relocate out of operating areas; their exposure continues across generations.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, target_region_populations, payer,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(article_51_self_defense__expansive_preventive_reading, target_region_populations, excluded).

% Holds the Charter's gatekeeping role over cross-border force. Each invocation of self-judged necessity that proceeds without Council authorization erodes the body's claim to be the arbiter, and its responses — debates, resolutions, presidential statements — carry no enforcement against the states doing the striking. The Charter text it administers cannot be revised without the consent of the same powers the reading empowers.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, un_security_council_authority, payer,
    institutional, generational, constrained, global).

% Govern territories where armed non-state groups operate. When another state judges the threat emergent and necessity demonstrated, these governments face operations on their soil whether or not they consent; protesting sovereignty violations carries little weight against the acting state's threat assessment. Their options — suppressing the groups themselves, consenting to cooperation, or absorbing the strikes — are all costly, and several are beyond their capacity.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, host_states_of_nonstate_actors, payer,
    moderate, biographical, constrained, regional).

% Scholars, jurists, and legal advisers who analyze invocation practice, publish assessments of necessity claims, and argue the interpretive questions in journals, courtrooms, and UN fora. They hold no enforcement lever; their influence runs through argument, advisory opinions, and the slow accumulation of doctrine.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, international_law_community, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_51_self_defense__expansive_preventive_reading, militarily_capable_states).
narrative_ontology:fixing_cost_class(article_51_self_defense__expansive_preventive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies a shared legal vocabulary and threshold — necessity, demonstrated before an attack matures — under which states coordinate expectations about when first strikes against non-state actors and emerging capabilities will be treated as lawful, allowing action when Council authorization is blocked or unavailable.
% TRANSFER_FUNCTION: Moves decision-authority over cross-border force from the Security Council to individual militarily capable states; moves the risks of that force — casualties, displacement, escalation — onto target-region populations and host states; moves procurement demand to defense suppliers.
% ABSENT_VOICES: Target-region populations are absent from every forum where necessity is judged; host states are heard but structurally outweighed; narrow-reading jurists participate in debate but command no enforcement. Unanimity in capable-state legal memoranda partly reflects who is in the room.
% DISAPPEARANCE_RATIONALE: If the reading vanished overnight, capable states would face a choice between Council authorization they often cannot obtain and openly unlawful force; alliance planning, threat-assessment publication, and strike tempo would reorganize around the imminence standard; target regions would see fewer first strikes while substitute arrangements settled.
% FOUNDING_PROBLEM: The Charter's collective-security design assumed discrete, attributable armed attacks by states; it offered no lawful path against threats that accumulate gradually — terrorist networks embedded in unwilling hosts, nuclear programs approaching capability — especially when a permanent member's veto blocks Council action.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: the 2004 UN High-level Panel on Threats, Challenges and Change and successive Secretary-General reports attest that non-state and proliferant threats are real and that Council action can be blocked; host-state governments and target-region authorities attest the threat environment while disputing the remedy; the academic international-law literature on the post-9/11 use-of-force debates documents the problem independently of any benefiting government.
narrative_ontology:disappearance_verdict(article_51_self_defense__expansive_preventive_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_51_self_defense__expansive_preventive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__expansive_preventive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_51_self_defense__expansive_preventive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_51_self_defense__expansive_preventive_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_51_self_defense__expansive_preventive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_51_self_defense__expansive_preventive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_51_self_defense__expansive_preventive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.68: decision-authority over cross-border force migrates from a collective body to single capitals, and the arrangement's costs concentrate on people with no seat in the deciding forums; offsetting this, the necessity vocabulary still imposes reputational and evidentiary costs on invoking states, and the coordination service is real. Suppression 0.57: enforcement is discursive and institutional — coalition discipline, doctrinal assertion, sidelining of narrow-reading positions in operational practice — rather than physical coercion of participating states, but for target populations the suppression is total and for the Council it is structural. Theater 0.48: a large share of necessity-demonstration activity is post-hoc justification (the 2003 WMD case is the canonical instance), though genuine intelligence assessment persists alongside it. Accessibility collapse 0.35: the sibling readings and the Council pathway remain live, advocated, and occasionally decisive — alternatives have not collapsed. Resistance 0.62: sustained opposition from most of the Global South, much of the legal academy, and recurring General Assembly and judicial pushback. All three tracked series run on one shared grid (t = 0, 7, 15, 22, 30, 37, 45, mapping roughly 1981-2026); the t22 point marks the 2003 peak, where theater and enforcement intensity spike together and extraction jumps; suppression declines after the peak as the enforcing coalition's discursive monopoly frays under multipolarity.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setting capable-state seat experiences the reading as prudent adaptation: an antiquated trigger updated for networks and proliferators, exercised under legal advice and domestic oversight. The target-population seat experiences the same text as a standing permission for someone else's government to strike first on self-certified evidence. The Council seat experiences it as gatekeeping dissolution. These are computed divergences from the structural data — power, exit, and role differences — not authored conclusions; the engine derives each seat's type.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations: militarily_capable_states and defense_industrial_sector derive low directionality (subsidized seats). Victim declarations: target_region_populations, un_security_council_authority, and host_states_of_nonstate_actors derive high directionality, amplified for the trapped target-population seat. One override: the derivation would place militarily_capable_states near the pure-beneficiary pole (declared beneficiary, arbitrage-grade exit), but their net position carries real costs — retaliatory blowback, reciprocal-precedent exposure when rivals invoke the same reading, and entanglement in open-ended campaigns — so d is overridden to 0.18, still firmly on the beneficiary side but short of subsidy. The Security Council seat is left to derivation despite containing the same capable states as individual members: the gatekeeping function itself is what pays here, and it pays fully.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — gradual threats the Charter machinery cannot reach, compounded by veto paralysis — remains live, so this is not a mandate outliving its function and no mandatrophy resolution is declared. What has shifted is the function mix: the measurement series shows theater and extraction climbing after the 2003 peak while the enforcement requirement decays, i.e., the arrangement increasingly performs justification rather than exercising judgment. The tangled_rope classification prevents mislabeling in both directions: calling this a rope would erase the identifiable payers and the self-judging defect; calling it a snare would erase the genuine coordination service a blocked Council otherwise leaves unsupplied. The R5 mismatch check reads live-status against world_rearranges and finds no zombie signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint instantiates the expansive_preventive_reading of the article_51_self_defense kernel; how would classification and the beneficiary/victim structure shift under the sibling readings (narrow_armed_attack_reading, unable_unwilling_doctrine_reading)?',
    'Author and classify the two sibling stories against the same referent (the standing Article 51 arrangement) and compare per-seat classifications; the divergence locates the disagreement structurally.',
    'Under the narrow reading the victim set shrinks to states struck without an attribution-worthy attack and epsilon falls toward coordination-cost levels; under unable-unwilling the host-state seat becomes the pivotal payer and epsilon sits between this reading and the narrow one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer-frame uncertainty: this is one of three readings of the Article 51 kernel, and the classification is reading-indexed.').

omega_variable(
    necessity_demonstration_genuineness,
    'Does ''necessity demonstrated'' operate as an externally checkable threshold, or is necessity in practice self-judged by the acting state with the demonstration supplied after the fact?',
    'Compare pre-strike intelligence assessments and legal advice with post-hoc findings (Iraq WMD inquiries, targeted-strike casualty reviews) across a sample of invocations.',
    'If demonstration is systematically post hoc, the effective check approaches zero and the arrangement trends toward pure extraction; if some invocations show genuine ex ante checking, the coordination share is larger than the metrics assume.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_demonstration_genuineness, empirical, 'Whether the necessity threshold constrains or merely decorates self-judged force decisions.').

omega_variable(
    precedent_reciprocity_damping,
    'Do militarily capable states discount their own use of the reading because rivals and adversaries can invoke the same precedent against them and their partners?',
    'Code invocation patterns across crises: do capable states restrain invocations when adversaries would gain precedential benefit, or invoke symmetrically regardless?',
    'Strong reciprocity damping would lower effective extraction for the capable-state seat and partially self-limit the arrangement; weak damping confirms one-directional extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precedent_reciprocity_damping, empirical, 'Whether reciprocal-precedent exposure dampens the beneficiary seat''s net gain.').

omega_variable(
    victim_set_boundary_definition,
    'Where does the victim set end: populations in direct strike zones only, or also populations bearing displacement, destabilization, and radicalization spillover across the wider region?',
    'Displacement and mortality data tracing second-order harms from preventive-force campaigns against comparable non-intervention baselines.',
    'A wider victim set raises measured epsilon and strengthens the extraction side of the classification; a narrower set keeps epsilon nearer the tangled midpoint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_set_boundary_definition, conceptual, 'Boundary of the paying population under the standing arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__expansive_preventive_reading, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_51_self_defense__expansive_preventive_reading, theater_ratio, 0, 0.24).
narrative_ontology:measurement(arti_tr_t7, article_51_self_defense__expansive_preventive_reading, theater_ratio, 7, 0.27).
narrative_ontology:measurement(arti_tr_t15, article_51_self_defense__expansive_preventive_reading, theater_ratio, 15, 0.31).
narrative_ontology:measurement(arti_tr_t22, article_51_self_defense__expansive_preventive_reading, theater_ratio, 22, 0.52).
narrative_ontology:measurement(arti_tr_t30, article_51_self_defense__expansive_preventive_reading, theater_ratio, 30, 0.51).
narrative_ontology:measurement(arti_tr_t37, article_51_self_defense__expansive_preventive_reading, theater_ratio, 37, 0.49).
narrative_ontology:measurement(arti_tr_t45, article_51_self_defense__expansive_preventive_reading, theater_ratio, 45, 0.48).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(arti_be_t7, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 7, 0.36).
narrative_ontology:measurement(arti_be_t15, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 15, 0.41).
narrative_ontology:measurement(arti_be_t22, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 22, 0.6).
narrative_ontology:measurement(arti_be_t30, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 30, 0.64).
narrative_ontology:measurement(arti_be_t37, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 37, 0.67).
narrative_ontology:measurement(arti_be_t45, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 45, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(arti_su_t7, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 7, 0.33).
narrative_ontology:measurement(arti_su_t15, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 15, 0.39).
narrative_ontology:measurement(arti_su_t22, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 22, 0.72).
narrative_ontology:measurement(arti_su_t30, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 30, 0.66).
narrative_ontology:measurement(arti_su_t37, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 37, 0.61).
narrative_ontology:measurement(arti_su_t45, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 45, 0.57).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_51_self_defense__expansive_preventive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, article_51_narrow_armed_attack_reading).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, article_51_unable_unwilling_doctrine_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Article 51 self-defense' conflates three structurally distinct constraints distinguished by trigger condition and judging authority; per the epsilon-invariance principle they are separate stories linked here. The narrow armed-attack reading is the Charter's baseline with the lowest epsilon; this expansive preventive reading carries the highest epsilon because necessity is self-judged; the unable-unwilling hybrid sits between. Downstream pressure runs from this reading to the hybrid: its acceptance expands the legitimacy space in which conditional-trigger arguments are made.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_51_self_defense__expansive_preventive_reading, powerful, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
