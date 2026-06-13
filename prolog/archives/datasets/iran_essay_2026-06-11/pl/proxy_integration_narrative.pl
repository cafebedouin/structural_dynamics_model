% ============================================================================
% CONSTRAINT STORY: proxy_integration_narrative
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_proxy_integration_narrative, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: proxy_integration_narrative
 *   human_readable: Proxy Integration Narrative Constraint
 *   domain: geopolitical/information_warfare
 *
 * SUMMARY:
 *   The proxy integration narrative is a geopolitical framing constraint that
 *   treats Hezbollah as a fully integrated component of Iranian military
 *   infrastructure rather than as an autonomous Lebanese political and
 *   military organization with its own decision-making capacity, local
 *   legitimacy, and strategic interests. The narrative is propagated by
 *   Iranian state media (to legitimize support and direction), Israeli
 *   strategic communications (to justify strikes on Lebanese territory as
 *   counter-Iranian operations), and Western policy discourse (to simplify
 *   regional complexity into manageable categories). The constraint extracts
 *   strategic autonomy and political legitimacy from Hezbollah and Lebanese
 *   sovereignty claims, transferring interpretive authority to external
 *   powers. Resistance is high because the narrative contradicts observable
 *   organizational behavior and documented historical development, requiring
 *   continuous enforcement through media repetition, policy framework
 *   embedding, and delegitimization of complexity-preserving accounts.
 *
 * KEY AGENTS:
 *   - iranian_regional_control: Primary agenda-setter (institutional/mobile) — propagates integration narrative through state media and diplomatic channels, benefits by legitimizing support as internal coordination
 *   - israeli_strategic_framing: Beneficiary (institutional/mobile) — uses narrative to justify strikes on Lebanese territory as attacks on Iranian infrastructure
 *   - western_policy_simplification: Beneficiary (institutional/constrained) — uses narrative to reduce regional complexity to single-principal problem for sanctions and diplomacy
 *   - hezbollah_autonomy_narrative: Primary victim (organized/identity_locked) — loses strategic autonomy and local legitimacy in international discourse
 *   - lebanese_sovereignty_claims: Victim (moderate/trapped) — sovereignty assertions undermined by framing Lebanese territory as Iranian strategic depth
 *   - regional_complexity_understanding: Victim (powerless/trapped) — nuanced analysis crowded out by dominant simplification
 *   - hezbollah_internal_factions: Excluded (moderate/identity_locked) — internal debate and reform invisible under monolithic framing
 *   - academic_regional_studies: Observer (organized/analytical) — documents gap between narrative and observable behavior
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(proxy_integration_narrative, 0.78).
domain_priors:suppression_score(proxy_integration_narrative, 0.81).
domain_priors:theater_ratio(proxy_integration_narrative, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(proxy_integration_narrative, extractiveness, 0.78).
narrative_ontology:constraint_metric(proxy_integration_narrative, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(proxy_integration_narrative, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(proxy_integration_narrative, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(proxy_integration_narrative, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(proxy_integration_narrative, snare).
narrative_ontology:human_readable(proxy_integration_narrative, "Proxy Integration Narrative Constraint").
narrative_ontology:topic_domain(proxy_integration_narrative, "geopolitical/information_warfare").

domain_priors:requires_active_enforcement(proxy_integration_narrative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(proxy_integration_narrative, iranian_regional_control).
narrative_ontology:constraint_beneficiary(proxy_integration_narrative, israeli_strategic_framing).
narrative_ontology:constraint_beneficiary(proxy_integration_narrative, western_policy_simplification).
narrative_ontology:constraint_victim(proxy_integration_narrative, hezbollah_autonomy_narrative).
narrative_ontology:constraint_victim(proxy_integration_narrative, lebanese_sovereignty_claims).
narrative_ontology:constraint_victim(proxy_integration_narrative, regional_complexity_understanding).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and propagates the integration narrative through state media, diplomatic channels, and proxy coordination mechanisms. Benefits from the narrative by legitimizing material support and strategic direction as internal coordination rather than external interference. The framing converts what could be read as imperial overreach into organizational management of allied forces.
narrative_ontology:constraint_stakeholder(proxy_integration_narrative, iranian_regional_control, agenda_setter,
    institutional, generational, mobile, regional).

% Benefits from the integration narrative by using it to justify strikes on Lebanese territory as attacks on Iranian infrastructure rather than violations of Lebanese sovereignty. The narrative simplifies threat assessment and legitimizes preemptive action by collapsing Hezbollah's independent decision-making into Iranian command structure.
narrative_ontology:constraint_stakeholder(proxy_integration_narrative, israeli_strategic_framing, beneficiary,
    institutional, generational, mobile, regional).

% Uses the integration narrative to reduce regional complexity to manageable policy categories: sanctions targeting Iran automatically cover Hezbollah, diplomatic pressure on Tehran is treated as sufficient to constrain Lebanese actors, and alliance structures are read as hierarchical rather than networked. The narrative allows treating multi-actor regional dynamics as a single-principal problem.
narrative_ontology:constraint_stakeholder(proxy_integration_narrative, western_policy_simplification, beneficiary,
    institutional, biographical, constrained, global).

% Bears the cost of having its strategic autonomy, local legitimacy, and independent decision-making capacity systematically erased from international discourse. The integration narrative denies the organization's roots in Lebanese Shia political mobilization and treats every action as Iranian directive rather than local response. Exit is identity-locked because rejecting Iranian support would collapse operational capacity, but accepting it validates the integration framing.
narrative_ontology:constraint_stakeholder(proxy_integration_narrative, hezbollah_autonomy_narrative, payer,
    organized, generational, identity_locked, national).

% Lebanese state institutions and civil society actors who assert national sovereignty find their claims systematically undermined by the integration narrative, which treats Lebanese territory as Iranian strategic depth. When Israel strikes Lebanon, the integration narrative frames it as legitimate counter-Iranian action rather than violation of Lebanese sovereignty, making Lebanese appeals to international law structurally unpersuasive.
narrative_ontology:constraint_stakeholder(proxy_integration_narrative, lebanese_sovereignty_claims, payer,
    moderate, generational, trapped, national).

% Analysts, journalists, and publics attempting to understand regional dynamics as multi-actor networks with local grievances, historical trajectories, and independent agency find the integration narrative crowds out nuanced analysis. The narrative's dominance in policy and media discourse makes complexity-preserving accounts appear as apologia or naivety, suppressing alternative framings.
narrative_ontology:constraint_stakeholder(proxy_integration_narrative, regional_complexity_understanding, payer,
    powerless, biographical, trapped, global).

% Internal factions within Hezbollah that prioritize Lebanese political integration, social service provision, or independent strategic calculation are structurally excluded from the discourse. The integration narrative erases intra-organizational debate and treats the organization as monolithic Iranian instrument, making internal reform or reorientation invisible to external actors.
narrative_ontology:constraint_stakeholder(proxy_integration_narrative, hezbollah_internal_factions, excluded,
    moderate, biographical, identity_locked, local).

% Scholars of Middle Eastern politics, Shia political movements, and regional security document the gap between the integration narrative and observable organizational behavior, decision-making patterns, and historical development. They produce evidence of Hezbollah's independent strategic calculation and local political embeddedness, but find this evidence systematically discounted in policy discourse.
narrative_ontology:constraint_stakeholder(proxy_integration_narrative, academic_regional_studies, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(proxy_integration_narrative, iranian_regional_control).
narrative_ontology:fixing_cost_class(proxy_integration_narrative, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The narrative coordinates threat assessment and policy response across multiple state actors by providing a simplified principal-agent model of regional dynamics, reducing information costs for alliance coordination and sanctions enforcement.
% TRANSFER_FUNCTION: Transfers strategic autonomy and political legitimacy from Hezbollah and Lebanese sovereignty claims to Iranian regional control and Israeli/Western policy frameworks. Moves interpretive authority over regional events from local actors to external powers.
% ABSENT_VOICES: Hezbollah internal factions prioritizing Lebanese integration, Lebanese civil society actors asserting sovereignty independent of both Hezbollah and external powers, and regional analysts documenting organizational autonomy are structurally excluded from policy discourse. They would contest the integration framing but are pre-emptively delegitimized by it.
% DISAPPEARANCE_RATIONALE: If the integration narrative vanished, Israeli strikes on Lebanese territory would require different legal justification, Western sanctions would need to distinguish Iranian state action from allied organization behavior, Hezbollah's local political legitimacy would become a factor in regional analysis, and Lebanese sovereignty claims would gain traction in international forums. Policy frameworks built on the integration assumption would require substantial revision.
% FOUNDING_PROBLEM: After the 1982 Israeli invasion of Lebanon and the formation of Hezbollah with Iranian support, regional and international actors needed frameworks to understand the relationship between the new organization and its Iranian patron, particularly for threat assessment and policy response.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem of understanding a new organization's relationship to its patron was live in the 1980s. Academic regional studies and declassified intelligence assessments from the 1990s-2000s document Hezbollah's evolution into an autonomous political actor with independent decision-making capacity, local legitimacy, and strategic calculation that often diverges from Iranian preferences. The integration narrative persists despite this documented autonomy because it serves the beneficiaries' strategic interests, not because the founding uncertainty remains unresolved.
narrative_ontology:disappearance_verdict(proxy_integration_narrative, world_rearranges).
narrative_ontology:founding_problem_status(proxy_integration_narrative, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(proxy_integration_narrative, '046e0a40c34cddf4fff29b8c15f632dbdef31b7a',
    'c6d6880c39ec6bdfedde2a1d41cc00211f451559', '2026-06-11',
    'strategic_communications_geopolitical_narrative', 'agent/example_platform_commission.json',
    'claude-sonnet-4-20250514', 'temperature=1.0').
narrative_ontology:story_seed(proxy_integration_narrative, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(proxy_integration_narrative_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(proxy_integration_narrative, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(proxy_integration_narrative_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the narrative systematically transfers strategic autonomy, political legitimacy, and interpretive authority from local actors to external powers, with the transfer serving beneficiary interests rather than descriptive accuracy. Suppression is high (0.81) because maintaining the narrative requires active suppression of complexity-preserving accounts, delegitimization of local actor testimony, and continuous media/policy reinforcement against contradictory evidence. Theater ratio is moderate (0.42) because while some genuine coordination analysis occurs, a substantial share of discourse activity is performative repetition of the integration frame rather than evidence-based assessment. Accessibility collapse is moderate-low (0.48) because alternative framings (autonomous actor, networked alliance, local political movement) remain conceptually available and are actively articulated by excluded voices, though systematically marginalized. Resistance is high (0.72) because the narrative contradicts documented organizational behavior, historical development, and observable decision-making patterns, requiring continuous enforcement against persistent counter-evidence.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats should compute as coordination or weak extraction — from their positions the narrative is a useful simplification that enables policy coordination and threat assessment. The victim seats should compute as snare — from their positions the same narrative operates as enforced erasure of autonomy and legitimacy, maintained through active suppression of contradictory evidence. The observer seat documents this divergence through comparison of narrative claims with organizational behavior data.
 *
 * DIRECTIONALITY LOGIC:
 *   Iranian regional control is the primary agenda-setter and beneficiary (d near 0.2) — sets the narrative and benefits from legitimization of support and direction. Israeli strategic framing and Western policy simplification are beneficiaries (d near 0.25-0.3) — collect strategic advantages from the simplification without setting it. Hezbollah autonomy narrative is the primary victim (d near 0.85) — bears the full extraction of strategic autonomy and legitimacy, with identity-locked exit because rejecting Iranian support would collapse operational capacity. Lebanese sovereignty claims are victims (d near 0.8) with trapped exit — cannot escape the framing's effects on international legal standing. Regional complexity understanding is a diffuse victim (d near 0.75) — bears the cost of systematically degraded analysis quality.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by distinguishing genuine coordination (the founding problem of understanding a new organization's patron relationship in the 1980s) from current extraction (the narrative's persistence after organizational autonomy is documented serves beneficiary strategic interests, not descriptive accuracy). The founding problem is dead — academic research and intelligence assessments document Hezbollah's evolution into an autonomous actor. The narrative persists because it benefits multiple institutional actors, not because the uncertainty remains. This is classic mandatrophy: a framework built to solve an information problem continues after the problem is resolved because it serves extraction functions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_measurement_ambiguity,
    'What observable organizational behaviors would definitively establish Hezbollah''s strategic autonomy versus full integration, and do existing data meet that standard?',
    'Systematic comparison of Hezbollah strategic decisions with Iranian stated preferences across multiple domains (Lebanese domestic politics, Syrian intervention timing, ceasefire decisions, political coalition formation). Documented divergences would establish autonomy; perfect alignment would support integration.',
    'If systematic divergences are documented (as academic literature claims), the integration narrative is empirically false and its persistence is pure extraction. If perfect alignment is documented, the narrative is descriptively accurate and extraction is lower than measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_measurement_ambiguity, empirical, 'Whether observable behavior supports integration or autonomy framing.').

omega_variable(
    beneficiary_coordination_vs_extraction,
    'Do the multiple beneficiaries (Iran, Israel, Western policy) coordinate to maintain the narrative, or does it persist through independent convergent interests?',
    'Analysis of information operations, policy coordination channels, and narrative synchronization patterns. Evidence of coordination would establish active conspiracy; independent convergence would indicate structural incentive alignment.',
    'Coordination would increase suppression score and establish the narrative as actively maintained conspiracy. Independent convergence would suggest the narrative is an emergent property of aligned incentives, reducing agency but not extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_coordination_vs_extraction, empirical, 'Whether narrative maintenance is coordinated or emergent.').

omega_variable(
    identity_lock_mechanism,
    'Is Hezbollah''s identity-locked exit due to operational dependency on Iranian support, ideological commitment to resistance axis framing, or both?',
    'Counterfactual analysis of alternative support sources and ideological reframing possibilities. If operational dependency alone binds, alternative patrons could break the lock. If ideological commitment binds, the lock persists regardless of material support.',
    'Pure operational dependency would make the lock contingent and potentially breakable through alternative support. Ideological fusion would make the lock structural and permanent, increasing effective extraction by eliminating even theoretical exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, conceptual, 'Whether identity lock is material or ideological.').

omega_variable(
    narrative_vs_reality_gap_stability,
    'Can the integration narrative persist indefinitely despite contradictory evidence, or does accumulating divergence eventually force narrative revision?',
    'Historical analysis of similar geopolitical narratives that persisted despite counter-evidence. Identification of conditions under which narrative-reality gaps become unsustainable (policy failure, credibility collapse, beneficiary interest shift).',
    'If gaps can persist indefinitely, suppression is sustainable and extraction continues. If gaps eventually force revision, the constraint has a natural lifecycle limit and current extraction is time-bounded.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(narrative_vs_reality_gap_stability, empirical, 'Whether narrative-reality gaps are self-limiting or stable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(proxy_integration_narrative, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prox_tr_t0, proxy_integration_narrative, theater_ratio, 0, 0.25).
narrative_ontology:measurement(prox_tr_t7, proxy_integration_narrative, theater_ratio, 7, 0.29).
narrative_ontology:measurement(prox_tr_t14, proxy_integration_narrative, theater_ratio, 14, 0.33).
narrative_ontology:measurement(prox_tr_t21, proxy_integration_narrative, theater_ratio, 21, 0.37).
narrative_ontology:measurement(prox_tr_t28, proxy_integration_narrative, theater_ratio, 28, 0.4).
narrative_ontology:measurement(prox_tr_t35, proxy_integration_narrative, theater_ratio, 35, 0.42).

% Extraction over time
narrative_ontology:measurement(prox_be_t0, proxy_integration_narrative, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(prox_be_t7, proxy_integration_narrative, base_extractiveness, 7, 0.63).
narrative_ontology:measurement(prox_be_t14, proxy_integration_narrative, base_extractiveness, 14, 0.68).
narrative_ontology:measurement(prox_be_t21, proxy_integration_narrative, base_extractiveness, 21, 0.73).
narrative_ontology:measurement(prox_be_t28, proxy_integration_narrative, base_extractiveness, 28, 0.76).
narrative_ontology:measurement(prox_be_t35, proxy_integration_narrative, base_extractiveness, 35, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(prox_su_t0, proxy_integration_narrative, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(prox_su_t7, proxy_integration_narrative, suppression_requirement, 7, 0.67).
narrative_ontology:measurement(prox_su_t14, proxy_integration_narrative, suppression_requirement, 14, 0.72).
narrative_ontology:measurement(prox_su_t21, proxy_integration_narrative, suppression_requirement, 21, 0.76).
narrative_ontology:measurement(prox_su_t28, proxy_integration_narrative, suppression_requirement, 28, 0.79).
narrative_ontology:measurement(prox_su_t35, proxy_integration_narrative, suppression_requirement, 35, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(proxy_integration_narrative, information_standard).
narrative_ontology:affects_constraint(proxy_integration_narrative, iranian_regional_hegemony_narrative).
narrative_ontology:affects_constraint(proxy_integration_narrative, lebanese_sovereignty_erosion).
narrative_ontology:affects_constraint(proxy_integration_narrative, regional_threat_assessment_frameworks).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(proxy_integration_narrative, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
