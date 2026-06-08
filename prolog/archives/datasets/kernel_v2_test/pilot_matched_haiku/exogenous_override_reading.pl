% ============================================================================
% CONSTRAINT STORY: exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exogenous_override_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: exogenous_override_reading
 *   human_readable: Exogenous Override: State-Imposed Norms via Monopoly on Violence
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   The exogenous override reading models state-imposed norms that derive
 *   legitimacy from the state's monopoly on violence rather than from
 *   cultural acceptance or endogenous evolution. This reading instantiates
 *   one structural position within the contested imposition_mechanism_kernel:
 *   the claim that norms can be imposed exogenously, maintained through
 *   coercion, and internalized over generations without ever achieving
 *   genuine cultural legitimacy. The constraint exhibits high extractiveness
 *   (0.78) and high suppression (0.85), reflecting that compliance is
 *   conditional on state monitoring and the threat of violence, and that
 *   alternative norm frameworks are actively suppressed. The theater ratio
 *   (0.38) is moderate because the coercive apparatus is still functional and
 *   visible — the state has not yet fully transitioned to relying on
 *   internalized legitimacy theater. Over the 25-year interval,
 *   extractiveness rises (0.65 → 0.78) as the state consolidates control and
 *   deepens norm embedding; suppression requirement rises (0.80 → 0.85) as
 *   resistance persists and requires increasing enforcement; theater ratio
 *   rises (0.25 → 0.38) as the state begins to construct legitimacy
 *   narratives around the imposed norms. This trajectory reflects the typical
 *   pattern of exogenous override: initial naked coercion, followed by
 *   institutional embedding and legitimacy theater, followed by generational
 *   internalization that makes the coercive apparatus less visible but no
 *   less extractive.
 *
 * KEY AGENTS:
 *   - Subject Populations: Primary victims (powerless/trapped) — bear the full cost of norm imposition; compliance is conditional on state monitoring; no exit option
 *   - Indigenous Cultural Systems: Primary victims (powerless/trapped) — suppressed alternative norm frameworks; cultural autonomy extracted
 *   - State Apparatus: Primary beneficiary (institutional/arbitrage) — benefits from monopoly on norm-setting; coercive apparatus legitimated by state's claimed authority
 *   - Coercive Elite: Primary beneficiary (institutional/arbitrage) — military, secret police, security apparatus benefit from monopoly on violence; norm enforcement is their function
 *   - Intermediate Bureaucracy: Secondary actor (institutional/constrained) — state functionaries experience mixed coordination (career advancement, institutional stability) and extraction (career lock-in, moral hazard)
 *   - Legitimacy Theater: Institutional performance (institutional/arbitrage) — the state's narrative of cultural acceptance becomes increasingly performative as the constraint persists
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as immutable features of state formation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exogenous_override_reading, 0.78).
domain_priors:suppression_score(exogenous_override_reading, 0.85).
domain_priors:theater_ratio(exogenous_override_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exogenous_override_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(exogenous_override_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(exogenous_override_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exogenous_override_reading, snare).
narrative_ontology:human_readable(exogenous_override_reading, "Exogenous Override: State-Imposed Norms via Monopoly on Violence").
narrative_ontology:topic_domain(exogenous_override_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exogenous_override_reading, 'a72c1be9-f6f4-4318-9b77-aa271417bd78').
narrative_ontology:cs_kernel_codification('a72c1be9-f6f4-4318-9b77-aa271417bd78', distributed).
narrative_ontology:cs_authority_grounding('a72c1be9-f6f4-4318-9b77-aa271417bd78', extraction).
narrative_ontology:cs_interpretation_layer_present('a72c1be9-f6f4-4318-9b77-aa271417bd78').
narrative_ontology:cs_reading_relation('a72c1be9-f6f4-4318-9b77-aa271417bd78', exogenous_override_reading__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('a72c1be9-f6f4-4318-9b77-aa271417bd78', exogenous_override_reading__hybrid_legitimation_reading, influences).
narrative_ontology:cs_axiom('a72c1be9-f6f4-4318-9b77-aa271417bd78', foundational, legitimacy_derives_from_coercion).
narrative_ontology:cs_axiom_status(legitimacy_derives_from_coercion, holdable).
narrative_ontology:cs_axiom_grounding('a72c1be9-f6f4-4318-9b77-aa271417bd78', legitimacy_derives_from_coercion, empirically_contingent).
narrative_ontology:cs_axiom('a72c1be9-f6f4-4318-9b77-aa271417bd78', secondary, cultural_acceptance_is_theater).
narrative_ontology:cs_axiom_status(cultural_acceptance_is_theater, holdable).
narrative_ontology:cs_axiom_grounding('a72c1be9-f6f4-4318-9b77-aa271417bd78', cultural_acceptance_is_theater, empirically_contingent).
narrative_ontology:cs_reference_frame('a72c1be9-f6f4-4318-9b77-aa271417bd78', state_monopoly_on_norm_setting).
narrative_ontology:cs_drift_state('a72c1be9-f6f4-4318-9b77-aa271417bd78', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a72c1be9-f6f4-4318-9b77-aa271417bd78', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(exogenous_override_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exogenous_override_reading, state_apparatus).
narrative_ontology:constraint_beneficiary(exogenous_override_reading, coercive_elite).
narrative_ontology:constraint_victim(exogenous_override_reading, subject_populations).
narrative_ontology:constraint_victim(exogenous_override_reading, indigenous_cultural_systems).
narrative_ontology:constraint_victim(exogenous_override_reading, alternative_norm_frameworks).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(exogenous_override_reading, intermediate_bureaucracy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Subject populations are required to adopt and comply with state-imposed norms. Compliance is monitored and enforced through legal penalties, social stigma, and coercive force. They bear the cost of abandoning pre-existing cultural practices and adopting state-mandated frameworks. They have no exit option — territorial jurisdiction and coercive apparatus prevent departure. Their cultural autonomy is extracted and transferred to the state apparatus.
narrative_ontology:constraint_stakeholder(exogenous_override_reading, subject_populations, payer,
    powerless, biographical, trapped, national).

% Alternative norm frameworks (indigenous practices, minority traditions, pre-state cultural systems) are actively suppressed through legal prohibition, institutional exclusion, and coercive force. These frameworks cannot exit the state's jurisdiction. Their legitimacy is denied, their practitioners are marginalized, and their transmission to new generations is blocked. The suppression is structural — the state's monopoly on norm-setting requires the elimination of competing frameworks.
narrative_ontology:constraint_stakeholder(exogenous_override_reading, indigenous_cultural_systems, payer,
    powerless, generational, trapped, national).

% The state apparatus (legislature, executive, judiciary) sets and enforces norms. It benefits from the monopoly on norm-setting: it can extract cultural autonomy from subject populations, consolidate political control, and legitimize its authority through the claim that norms are culturally accepted. It has arbitrage options — it can defect to rival states or alternative governance structures. Its primary function is to maintain state power through norm monopoly.
narrative_ontology:constraint_stakeholder(exogenous_override_reading, state_apparatus, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(exogenous_override_reading, state_apparatus, beneficiary).

% Military, secret police, and security apparatus benefit from the monopoly on violence that legitimates their role. Norm enforcement is their function, and they extract career advancement, resource access, and institutional power from the constraint. They have arbitrage options — they can defect to rival states or private security. They experience the constraint as pure coordination: it solves their problem of how to maintain state power without constant renegotiation of legitimacy.
narrative_ontology:constraint_stakeholder(exogenous_override_reading, coercive_elite, beneficiary,
    institutional, immediate, arbitrage, national).

% State functionaries and local administrators implement norm enforcement. They benefit from career advancement, resource access, and institutional stability (coordination function). But they also bear costs: career lock-in (leaving the state apparatus is difficult), moral hazard (enforcing norms they may not endorse), and vulnerability to state purges. They have constrained exit — they can leave but face severe career and social penalties. They experience mixed coordination and extraction.
narrative_ontology:constraint_stakeholder(exogenous_override_reading, intermediate_bureaucracy, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(exogenous_override_reading, intermediate_bureaucracy, payer).

% The state's claim that norms are culturally accepted is a non-agent entity (a doctrine, a narrative, a legitimacy claim) that the state maintains through theater and institutional performance. It is not an agent that collects rents, but it is a structural feature of the constraint. The narrative persists through institutional inertia even as its functional role atrophies — the state no longer needs cultural acceptance to maintain compliance, but it continues to construct legitimacy narratives around the imposed norms.
narrative_ontology:constraint_stakeholder(exogenous_override_reading, legitimacy_narrative, observer,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_non_agent(exogenous_override_reading, legitimacy_narrative).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The state apparatus coordinates norm enforcement across a territory and population. Without a unified norm framework, the state cannot maintain monopoly on violence or consolidate political control. The coordination problem is: how to establish a single set of binding norms that all subject populations must follow, and how to enforce compliance across diverse populations with different pre-existing cultural practices?
% TRANSFER_FUNCTION: The constraint transfers cultural autonomy from subject populations to the state apparatus. Subject populations give up the right to define their own norms and must adopt state-mandated frameworks. The state apparatus gains the power to set norms, extract compliance, and legitimize its authority through the claim that norms are culturally accepted. The coercive elite gain career advancement and institutional power from norm enforcement.
% ABSENT_VOICES: Suppressed alternative norm frameworks (indigenous practices, minority traditions, pre-state cultural systems) would object if they were in the conversation. They are excluded from norm-setting because the state's monopoly requires their elimination. Diaspora communities and underground resistance movements also object but are marginalized and prevented from participating in norm-setting. The absent voices are those whose cultural autonomy is extracted — they are not at the table where norms are decided.
% DISAPPEARANCE_RATIONALE: If the exogenous override constraint disappeared overnight, the world would rearrange substantially. Subject populations would revert to pre-existing cultural practices or develop new norms through endogenous processes. The state apparatus would lose its monopoly on norm-setting and would need to renegotiate legitimacy with subject populations. The coercive elite would lose the institutional power derived from norm enforcement. Alternative norm frameworks would re-emerge from suppression. The constraint is not a natural fact — it is a contingent institutional arrangement that depends on the state's continued coercive capacity and willingness to enforce norm monopoly.
% FOUNDING_PROBLEM: The founding problem is the state's need to consolidate political control over a diverse population with different pre-existing cultural practices. Early states faced the problem of how to establish a unified legal and normative framework that would enable centralized governance, tax collection, military conscription, and monopoly on violence. Exogenous override (imposing norms through coercion) was one solution to this problem — it allowed states to establish unified norms without waiting for cultural consensus or endogenous evolution.
% FOUNDING_PROBLEM_CORROBORATION: Historical sociologists (Weber, Tilly, Foucault) document that early states used coercive force to impose norms and consolidate control. However, the status of the founding problem is contested: some scholars argue that the problem of consolidating control over diverse populations is still live (modern states still face this problem); others argue that the problem is dead (modern states have achieved sufficient institutional embedding that norm monopoly is no longer necessary for control); still others argue that the problem is contested (some states maintain norm monopoly through coercion, others through cultural acceptance, and the distinction is unclear). The corroboration comes from historical case studies of state formation (European nation-states, colonial states, post-colonial states) where exogenous override was used to consolidate control. However, the status of the founding problem depends on whether the state's current norm monopoly is maintained through coercion (problem still live) or through internalized legitimacy (problem solved).
narrative_ontology:disappearance_verdict(exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(exogenous_override_reading, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBJECT POPULATION (SNARE) — Trapped by territorial jurisdiction and coercive apparatus. Compliance is conditional on state monitoring and threat of violence. No exit option; alternatives are suppressed. Experiences the constraint as pure extraction: the imposed norms extract cultural autonomy and replace it with state-mandated frameworks. Maximum experienced extraction.
constraint_indexing:constraint_classification(exogenous_override_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: GENERATIONAL INTERNALIZATION (SNARE) — Over time, imposed norms become internalized through childhood socialization, institutional embedding, and epistemic closure. The second generation experiences the constraint as identity-locked: exit would require abandoning the only cultural framework they know. The binding mechanism shifts from external coercion to cognitive capture, but the extraction persists. The snare deepens as internalization makes the coercive apparatus less visible.
constraint_indexing:constraint_classification(exogenous_override_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: INTERMEDIATE BUREAUCRACY (TANGLED ROPE) — State functionaries and local administrators experience mixed coordination and extraction. They benefit from career advancement, resource access, and institutional stability (coordination function: the state apparatus requires coordinated norm enforcement). But they also bear costs: career lock-in, moral hazard of enforcing norms they may not endorse, and vulnerability to state purges. Constrained exit — they can leave but face severe career and social penalties.
constraint_indexing:constraint_classification(exogenous_override_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: COERCIVE ELITE (ROPE) — Military, secret police, and security apparatus experience the constraint as pure coordination: norm enforcement is their function, and they benefit from the monopoly on violence that legitimates their role. They have arbitrage options (can defect to rival states or private security). Net beneficiaries — extraction runs toward them. The constraint solves their coordination problem: how to maintain state power without constant renegotiation of legitimacy.
constraint_indexing:constraint_classification(exogenous_override_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGITIMACY THEATER (PITON) — At civilizational scale, the state's claim to legitimacy through 'cultural acceptance' becomes increasingly performative as the constraint persists. Rituals of consent (elections, referenda, public ceremonies affirming the imposed norms) persist long after their functional role has atrophied. The state maintains the theater of legitimacy because the alternative — explicit acknowledgment that norms rest on coercion alone — would destabilize the system. Theater ratio is moderate (0.38) because the coercive apparatus is still functional, but the legitimacy narrative is degraded.
constraint_indexing:constraint_classification(exogenous_override_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN — FALSE SUMMIT CANDIDATE) — From a universal/civilizational perspective, the constraint risks being naturalized as an immutable feature of state formation: 'All states impose norms; coercion is inherent to governance; legitimacy is always contested.' This perspective sees the exogenous override as a natural law of political organization. However, the structural data contradicts this: the constraint has identifiable beneficiaries (state apparatus, coercive elite), high suppression, and high extractiveness. The engine's false summit detector will reclassify this as a snare or tangled rope, revealing that what appears as natural law is actually a contingent institutional arrangement that benefits specific actors.
constraint_indexing:constraint_classification(exogenous_override_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exogenous_override_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(exogenous_override_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(exogenous_override_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(exogenous_override_reading, TR),
    TR >= 0.70.

:- end_tests(exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): High. The state extracts cultural autonomy from subject populations and replaces it with state-mandated norms. The extraction is not total (0.78 rather than 0.95) because some norms may align with pre-existing cultural practices, and some subject populations may find benefits in the imposed framework (e.g., unified legal system, reduced inter-group violence). But the core extraction is substantial: the right to define one's own cultural norms is transferred to the state apparatus. Suppression (0.85): High. Alternative norm frameworks are actively suppressed through legal prohibition, social stigma, institutional exclusion, and coercive force. Suppression is not total (0.85 rather than 0.95) because some alternative frameworks persist in hidden or marginalized spaces (underground religious practices, diaspora communities, intellectual resistance). Theater ratio (0.38): Moderate. The coercive apparatus is still functional and visible — the state has not yet fully transitioned to relying on internalized legitimacy. The theater ratio rises over time as the state constructs legitimacy narratives (national identity, civilizing mission, rule of law) around the imposed norms. The moderate theater ratio reflects that the exogenous override reading emphasizes coercion over legitimacy theater, in contrast to the hybrid_legitimation_reading which would show higher theater ratio.
 *
 * PERSPECTIVAL GAP:
 *   The exogenous override reading produces a wide perspectival gap between beneficiaries and victims. The subject population (powerless/trapped) experiences pure snare: extraction with no coordination benefit. The generational internalization (powerless/identity_locked) experiences snare with cognitive capture: the binding mechanism becomes internal, making the coercive apparatus less visible. The intermediate bureaucracy (institutional/constrained) experiences tangled rope: they benefit from institutional stability and career advancement, but also bear costs of career lock-in and moral hazard. The coercive elite (institutional/arbitrage) experience rope: norm enforcement is their function, and they benefit from the monopoly on violence. The legitimacy theater (institutional/arbitrage) experiences piton: the state's claim to cultural acceptance becomes increasingly performative. The analytical observer (analytical/analytical) risks seeing mountain: naturalizing the exogenous override as an immutable feature of state formation. This perspectival gap reveals that the same structural phenomenon — state-imposed norms — appears as extraction to the powerless, coordination to the beneficiaries, and natural law to the analytical observer.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is computed from the agent's structural position relative to the constraint. Subject populations are full targets (d ≈ 1.0): they bear extraction costs with no exit option and no benefit. The coercive elite are full beneficiaries (d ≈ 0.0): they benefit from the monopoly on violence with arbitrage exit options. The intermediate bureaucracy are partial targets (d ≈ 0.6): they benefit from institutional stability but bear costs of career lock-in and moral hazard. The analytical observer is neutral (d ≈ 0.5): they see the constraint from outside the extraction flow. The engine derives d from beneficiary/victim declarations and exit options; the directionality values feed into the sigmoid f(d) to produce experienced extractiveness chi. High d (targets) produces high chi; low d (beneficiaries) produces low or negative chi. The perspectival gap emerges because different agents have different d values and thus experience different chi values from the same base extractiveness.
 *
 * MANDATROPHY ANALYSIS:
 *   The exogenous override reading resolves the mandatrophy by clarifying that the constraint's mandate (to impose and maintain state-backed norms) has NOT outlived its function — the state continues to benefit from norm monopoly, and the coercive apparatus continues to function. However, the reading reveals a secondary mandatrophy: the state's legitimacy claim (that norms are culturally accepted) has outlived its function. The state no longer needs cultural acceptance to maintain norm compliance — coercion and internalization suffice. The theater ratio rising from 0.25 to 0.38 reflects the state's attempt to construct legitimacy narratives around norms that are fundamentally coerced. This is the classic piton pattern: the legitimacy theater persists through institutional inertia, not because it is functionally necessary. The exogenous override reading distinguishes this from the hybrid_legitimation_reading, which would claim that the state's legitimacy is genuinely rooted in cultural acceptance. The exogenous override reading asserts that legitimacy is theater, and the constraint is fundamentally extractive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_vs_coercion_boundary,
    'At what point does a norm transition from ''culturally accepted with state enforcement'' to ''purely coerced with legitimacy theater''? Is there a structural boundary or a continuous spectrum?',
    'Historical analysis of norm adoption timelines; correlation between enforcement intensity and reported cultural acceptance; measurement of compliance rates under monitoring vs. without monitoring',
    'If boundary exists: some exogenous overrides are actually hybrid legitimation (sibling reading). If spectrum: all exogenous overrides are pure snares with varying degrees of internalization theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_vs_coercion_boundary, conceptual, 'Boundary between legitimacy and coercion in norm imposition').

omega_variable(
    internalization_mechanism_ambiguity,
    'Does generational internalization of imposed norms represent genuine cultural acceptance or sophisticated cognitive capture? Can internalized norms be distinguished from authentic cultural evolution?',
    'Cross-generational attitude surveys; analysis of norm persistence after coercive apparatus weakens; comparison with norms that evolved endogenously vs. those imposed exogenously',
    'If internalization = acceptance: the constraint transitions from snare to rope over time. If internalization = capture: the constraint remains snare but becomes invisible (identity_locked exit). Classification stability depends on this distinction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalization_mechanism_ambiguity, conceptual, 'Whether internalization represents acceptance or cognitive capture').

omega_variable(
    alternative_norm_framework_viability,
    'Are suppressed alternative norm frameworks genuinely viable alternatives to the imposed norms, or are they incompatible with the state''s structural requirements? Is suppression of alternatives extraction or necessary coordination?',
    'Historical counterfactual analysis: cases where alternative frameworks were permitted; measurement of state stability under norm pluralism vs. norm monopoly; identification of core vs. peripheral norms in the imposed framework',
    'If alternatives are viable: suppression is pure extraction (snare). If alternatives are incompatible with state function: suppression is coordination cost (tangled rope or rope). This determines whether the constraint is fundamentally extractive or mixed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_norm_framework_viability, empirical, 'Viability of suppressed alternative norm frameworks').

omega_variable(
    reading_kernel_ambiguity,
    'This constraint is one reading of the imposition_mechanism_kernel. The sibling readings (endogenous_climb_reading, hybrid_legitimation_reading) represent alternative framings of how norms become state-backed. Is the exogenous override reading a genuine structural alternative, or is it a rhetorical position that collapses into one of the siblings under scrutiny?',
    'Comparative historical analysis of state formation cases: identification of cases that fit the exogenous override pattern (norms imposed against cultural resistance) vs. cases that fit endogenous climb (norms that evolved culturally then were formalized) vs. hybrid (mixed mechanisms). Measurement of enforcement intensity and legitimacy claims across reading types.',
    'If exogenous override is structurally distinct: the three readings coexist as live positions. If exogenous override collapses into hybrid: the reading_relations should shift from coexists_with to influences. If exogenous override is merely rhetorical: the kernel itself may be under-specified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, conceptual, 'Whether exogenous override is a distinct reading or rhetorical position').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exogenous_override_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exog_theater_t0, exogenous_override_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(exog_theater_t10, exogenous_override_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(exog_theater_t25, exogenous_override_reading, theater_ratio, 25, 0.38).

% Extraction over time
narrative_ontology:measurement(exog_extract_t0, exogenous_override_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(exog_extract_t10, exogenous_override_reading, base_extractiveness, 10, 0.72).
narrative_ontology:measurement(exog_extract_t25, exogenous_override_reading, base_extractiveness, 25, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(exog_suppress_t0, exogenous_override_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(exog_suppress_t10, exogenous_override_reading, suppression_requirement, 10, 0.83).
narrative_ontology:measurement(exog_suppress_t25, exogenous_override_reading, suppression_requirement, 25, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exogenous_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(exogenous_override_reading, endogenous_climb_reading).
narrative_ontology:affects_constraint(exogenous_override_reading, hybrid_legitimation_reading).

% DUAL FORMULATION NOTE:
% The exogenous override reading is part of the imposition_mechanism_kernel constraint family. The three readings (exogenous_override_reading, endogenous_climb_reading, hybrid_legitimation_reading) represent structurally distinct claims about how norms become state-backed. Each reading has its own epsilon value, beneficiary/victim structure, and classification. The readings are linked via network.affects_constraints to enable cross-reading analysis and to support the engine's kernel decomposition logic.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
