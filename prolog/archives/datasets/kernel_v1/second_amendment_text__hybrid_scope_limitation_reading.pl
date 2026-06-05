% ============================================================================
% CONSTRAINT STORY: second_amendment_text__hybrid_scope_limitation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_hybrid_scope, []).

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
 *   constraint_id: second_amendment_text__hybrid_scope_limitation_reading
 *   human_readable: Second Amendment: Hybrid Scope-Limitation Reading (Individual Right with Regulatory Tolerance)
 *   domain: constitutional_law/political_philosophy/gun_policy
 *
 * SUMMARY:
 *   The Second Amendment hybrid-scope-limitation reading represents a middle
 *   constitutional position that acknowledges an individual right to firearm
 *   possession while permitting substantial regulation for public safety.
 *   This reading interprets the text as protecting personal firearm ownership
 *   within a framework that allows licensing requirements, background checks,
 *   prohibitions on particular weapons classes, and restrictions on carry in
 *   sensitive places. The reading instantiates one solution to the
 *   interpretive puzzle posed by the Second Amendment kernel: how to honor
 *   both the individualist claim ('right...shall not be infringed') and the
 *   regulatory claim (the state's police power to protect public safety).
 *   Unlike the individualist reading (which minimizes regulatory scope) or
 *   the collectivist reading (which treats the right as militia-contingent),
 *   the hybrid reading attempts to stabilize a licensed plurality — many
 *   people may own firearms, but not all firearms, not in all places, not
 *   without background verification. The constraint emerges from the
 *   institutional tension between constitutional text and democratic
 *   regulation. The theater ratio increases over the interval (0.35 → 0.48)
 *   as both individualist and regulatory advocates appeal to 'the text' while
 *   advancing incompatible positions — originalist hermeneutics becomes
 *   performative. The base extractiveness rises (0.22 → 0.38) as regulatory
 *   expansion accumulates (dangerous-person definitions broaden,
 *   sensitive-place categories multiply, licensing requirements tighten),
 *   incrementally reducing the practical scope of the nominally protected
 *   right without formally abolishing it.
 *
 * KEY AGENTS:
 *   - Licensed Firearm Owners: Primary beneficiary (institutional/arbitrage) — gain legal recognition and predictable regulatory environment
 *   - Unregulated Access Advocates: Primary victim (powerless/trapped) — excluded from unlimited carrying rights; bear cost of licensing and place-based restrictions
 *   - State Regulatory Authority: Secondary beneficiary (institutional/arbitrage) — gains institutional legitimacy to regulate while appearing to preserve constitutional right
 *   - Federal Judiciary: Institutional constraint maker (institutional/constrained) — benefits from hybrid frame that avoids extreme outcomes but constrained by stare decisis and text
 *   - Licensing & Safety Advocacy Coalition: Organized beneficiary (organized/mobile) — advocates for evidence-based safety regulations under the hybrid frame
 *   - Constitutional Originalist Tradition: Piton actor (institutional/arbitrage) — hermeneutic persists through institutional practice despite degraded predictive power
 *   - Policy Stability Across Jurisdictions: Primary victim (powerless/trapped) — abstract collective good that cannot organize; bears cost of regulatory fragmentation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__hybrid_scope_limitation_reading, 0.38).
domain_priors:suppression_score(second_amendment_text__hybrid_scope_limitation_reading, 0.52).
domain_priors:theater_ratio(second_amendment_text__hybrid_scope_limitation_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__hybrid_scope_limitation_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(second_amendment_text__hybrid_scope_limitation_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(second_amendment_text__hybrid_scope_limitation_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__hybrid_scope_limitation_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_text__hybrid_scope_limitation_reading, "Second Amendment: Hybrid Scope-Limitation Reading (Individual Right with Regulatory Tolerance)").
narrative_ontology:topic_domain(second_amendment_text__hybrid_scope_limitation_reading, "constitutional_law/political_philosophy/gun_policy").

domain_priors:requires_active_enforcement(second_amendment_text__hybrid_scope_limitation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__hybrid_scope_limitation_reading, '1545cb58-b230-466c-93c0-27eb0ed815cc').
narrative_ontology:cs_kernel_codification('1545cb58-b230-466c-93c0-27eb0ed815cc', formalized).
narrative_ontology:cs_authority_grounding('1545cb58-b230-466c-93c0-27eb0ed815cc', lineage).
narrative_ontology:cs_interpretation_layer_present('1545cb58-b230-466c-93c0-27eb0ed815cc').
narrative_ontology:cs_reading_relation('1545cb58-b230-466c-93c0-27eb0ed815cc', second_amendment_text__individualist_reading, coexists_with).
narrative_ontology:cs_reading_relation('1545cb58-b230-466c-93c0-27eb0ed815cc', second_amendment_text__collectivist_reading, coexists_with).
narrative_ontology:cs_axiom('1545cb58-b230-466c-93c0-27eb0ed815cc', foundational, individual_right_preservable_under_regulation).
narrative_ontology:cs_axiom_status(individual_right_preservable_under_regulation, holdable).
narrative_ontology:cs_axiom_grounding('1545cb58-b230-466c-93c0-27eb0ed815cc', individual_right_preservable_under_regulation, deontological).
narrative_ontology:cs_axiom('1545cb58-b230-466c-93c0-27eb0ed815cc', foundational, regulation_scope_permissible_for_public_safety).
narrative_ontology:cs_axiom_status(regulation_scope_permissible_for_public_safety, holdable).
narrative_ontology:cs_axiom_grounding('1545cb58-b230-466c-93c0-27eb0ed815cc', regulation_scope_permissible_for_public_safety, empirically_contingent).
narrative_ontology:cs_reference_frame('1545cb58-b230-466c-93c0-27eb0ed815cc', dual_rights_framework).
narrative_ontology:cs_drift_state('1545cb58-b230-466c-93c0-27eb0ed815cc', contemporary_regulatory_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1545cb58-b230-466c-93c0-27eb0ed815cc', '').
narrative_ontology:cs_kernel_id(second_amendment_text__hybrid_scope_limitation_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__hybrid_scope_limitation_reading, licensed_firearm_owners).
narrative_ontology:constraint_beneficiary(second_amendment_text__hybrid_scope_limitation_reading, state_regulatory_authority).
narrative_ontology:constraint_victim(second_amendment_text__hybrid_scope_limitation_reading, policy_stability_across_jurisdictions).
narrative_ontology:constraint_victim(second_amendment_text__hybrid_scope_limitation_reading, unregulated_access_advocates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNREGULATED ACCESS ADVOCATES (SNARE) — Structurally trapped by constitutional text that this reading interprets as permitting substantial regulation. No exit from the regulatory framework; bears full cost of licensing, background checks, and place-based restrictions. Maximum extraction from their structural position — the constraint forecloses their preferred interpretation within the licensed plural framework.
constraint_indexing:constraint_classification(second_amendment_text__hybrid_scope_limitation_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LICENSED FIREARM OWNERS (TANGLED ROPE) — Constrained by licensing requirements, background checks, and place-based restrictions (schools, federal buildings). But also benefit from the licensing system as a mechanism that stabilizes their rights within a predictable regulatory framework. They gain legal recognition, interstate reciprocity pathways, and protection against arbitrary prohibition. Mixed coordination (licensing provides stable legality) and extraction (regulatory overhead and access restrictions).
constraint_indexing:constraint_classification(second_amendment_text__hybrid_scope_limitation_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE REGULATORY AUTHORITY (ROPE) — Benefits from the hybrid reading as a coordination mechanism that permits evidence-based regulation (dangerous persons, sensitive places, unusual weapons) while preserving individual right. The reading provides institutional cover for regulation: 'We are enforcing a constitutional individual right, not denying it.' Arbitrage through regulatory discretion — can modify scope of regulation while remaining within the framework's bounds.
constraint_indexing:constraint_classification(second_amendment_text__hybrid_scope_limitation_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LICENSING & SAFETY ADVOCACY COALITION (ROPE) — Organized advocates see the hybrid reading as enabling evidence-based safety regulations (universal background checks, extreme risk protection orders, secure storage requirements) while nominally preserving constitutional right. The frame provides institutional legitimacy for safety policy. They have exit options: if the reading proves too permissive, they can push for interpretive narrowing or amendment. Rope classification reflects genuine coordination function (balancing right and safety) with minimal experienced extraction.
constraint_indexing:constraint_classification(second_amendment_text__hybrid_scope_limitation_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: FEDERAL JUDICIARY AS CONSTRAINT MAKER (TANGLED ROPE) — Constrained by the constitutional text and stare decisis. The judiciary benefits from the hybrid reading's institutionalization (it provides a middle ground that avoids extreme outcomes, maintains judicial legitimacy, and sidesteps unresolvable political contests). But it also bears the cost of continual reinterpretation — each case challenges the frame's boundaries. Mixed: genuine coordination function (resolving intractable political dispute) with embedded extraction (institutional power to define the frame).
constraint_indexing:constraint_classification(second_amendment_text__hybrid_scope_limitation_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CONSTITUTIONAL ORIGINALIST TRADITION (PITON) — The originalist hermeneutic applies to Second Amendment interpretation but has largely degraded into performative originalism: both originalists and non-originalists claim the text supports their reading. The hermeneutic persists through institutional inertia (judicial opinions cite original understanding, academic work continues) but provides minimal actual constraint on interpretation — outcome appears predetermined by political position. Theater ratio is high because originalist apparatus is maintained despite low predictive power.
constraint_indexing:constraint_classification(second_amendment_text__hybrid_scope_limitation_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / LINGUISTIC CONSTRAINT VIEW (MOUNTAIN) — From a civilizational and linguistic standpoint, the Second Amendment text creates an irreducible indexical: 'the right of the people to keep and bear Arms, shall not be infringed.' Any reading must navigate the tension between 'the people' (collective framing), 'keep and bear Arms' (individual capacity), and 'shall not be infringed' (prohibition on interference). No amount of regulation can fully satisfy both the individualist demand (unlimited access) and the regulatory demand (substantial control). This is a logical/linguistic constraint on the text's meaning-space, not a natural law but a structural feature of language itself. However, false summit risk: treating linguistic ambiguity as immutability when the ambiguity is productive — different polities can resolve it differently.
constraint_indexing:constraint_classification(second_amendment_text__hybrid_scope_limitation_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__hybrid_scope_limitation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(second_amendment_text__hybrid_scope_limitation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(second_amendment_text__hybrid_scope_limitation_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(second_amendment_text__hybrid_scope_limitation_reading, TR),
    TR >= 0.70.

:- end_tests(second_amendment_text__hybrid_scope_limitation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The hybrid reading permits substantial regulation but preserves nominally individual right. Unlike individualist reading (ε ≈ 0.08, near-absolute protection) or collectivist reading (ε ≈ 0.72, near-total regulation), the hybrid permits dangerous-person exceptions, sensitive-place restrictions, and licensing requirements while stopping short of categorical prohibition. The 0.38 value reflects the trade-off: access is constrained but not foreclosed; regulation is permitted but not unlimited. Suppression (0.52): Moderate-high. Licensing requirements, background checks, waiting periods, and place-based restrictions create significant friction on access. But suppression is not total — those who meet regulatory standards can still carry in most contexts. The suppression level reflects both the regulatory burden and the political contestation: unregulated-access advocates experience high suppression; licensed carriers experience moderate suppression. Theater ratio (0.48): Moderate, rising over interval. The hybrid reading permits evidence-based regulation, so performative activity is lower than in a purely individualist frame. But as regulatory categories proliferate (dangerous persons, sensitive places, unusual weapons), courts and advocates engage in increasingly theatrical interpretation — citing 'original meaning' to support incompatible regulatory stances. The rise from 0.35 to 0.48 reflects accumulating category creep and interpretive elaboration.
 *
 * PERSPECTIVAL GAP:
 *   The unregulated-access advocate sees Snare (structurally trapped, no exit from the regulatory framework, maximum extraction). The licensed firearm owner sees Tangled Rope (constrained by regulation but also stabilized by the licensing system; mixed extraction and coordination). The state regulatory authority sees Rope (the hybrid frame provides a coordination solution to the intractable political conflict between unlimited access and prohibition). The licensing & safety advocacy coalition sees Rope (evidence-based regulations are enabled by the frame's permission for danger-person and place-based exceptions). The federal judiciary sees Tangled Rope (institutional benefit from a middle ground that maintains legitimacy, constrained by stare decisis and the text). The originalist tradition sees Piton (the hermeneutic persists through institutional practice despite degraded constraint on outcomes — both sides claim originalism). The analytical observer sees Mountain (risks naturalizing the text's indexicality as immutability) — but false summit risk is high because linguistic ambiguity is productive, not immutable.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (licensed firearm owners, state regulatory authority) occupy structural positions where the hybrid frame produces benefit: it provides legal recognition, predictable regulatory environment, and institutional legitimacy. They have exit options (arbitrage) — if the frame becomes too restrictive, they can advocate for interpretive narrowing. Victims (unregulated-access advocates, policy stability across jurisdictions) are trapped by the frame — it forecloses their preferred interpretation without providing alternative paths. The directionality computation (from beneficiary/victim declarations and exit options) yields high d values for victims (↑ extraction) and low d values for beneficiaries (↓ extraction). This drives the perspectival gap: victims see Snare, beneficiaries see Rope, and the institutional analyst sees mixed Tangled Rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid-scope-limitation reading avoids both mandatrophies through an institutional framing: it acknowledges coordination (licensing provides stable legal recognition and regulatory predictability) without collapsing into pure coordination (regulation is substantial and constrains access). The reading resolves by accepting that both the individualist claim (right exists) and the regulatory claim (limitation is permissible) are legitimate, and institutionalizing a balance between them. The balance is not a natural law or a stable equilibrium — it requires ongoing judicial and legislative negotiation. The rising theater_ratio (0.35 → 0.48) and extractiveness (0.22 → 0.38) indicate drift: as regulatory categories proliferate, the frame's stability deteriorates. The measurement trajectory suggests that the reading's institutional mediation is eroding — the frame may eventually collapse toward either pure regulation (collectivist) or pure right (individualist) unless new institutional structures are developed to stabilize it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dangerous_person_definitional_scope,
    'What definition of ''dangerous person'' operationalizes the dangerous-person exception without swallowing the right entirely?',
    'Longitudinal empirical analysis of dangerous-person standards across jurisdictions; correlation between standard stringency and crime reduction; comparative study of how different democracies operationalize this exception.',
    'If the definition is too narrow (e.g., prior felony only): most regulations permitted under hybrid reading become unenforceable. If too broad (e.g., mental health status alone): reading collapses into prohibition, contradicting individualist premise. The reading''s stability depends on finding a defensible empirical threshold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dangerous_person_definitional_scope, empirical, 'Operationalization of dangerous-person exception').

omega_variable(
    sensitive_place_proliferation_risk,
    'Does the ''sensitive place'' exception create a creeping erosion where regulation by categorical exclusion eventually becomes de facto prohibition?',
    'Geographic analysis: proportion of public space (by square footage or frequency of habitation) designated as sensitive places over time; mapping of cumulative restrictions; study of jurisdictions where sensitive-place expansion has effectively eliminated practical carry rights.',
    'If sensitive-place categories remain stable and narrow (schools, federal buildings): hybrid reading holds. If categories proliferate (parks, public transit, commercial districts, ''sensitive events''): the reading becomes a Snare for ordinary carriers — nominally a right but practically unavailable. This drift would indicate the reading''s regularization has failed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sensitive_place_proliferation_risk, empirical, 'Sensitive-place scope creep risk').

omega_variable(
    regulatory_parity_across_jurisdictions,
    'Can a coherent hybrid-scope reading be maintained when different states adopt radically different regulatory regimes (e.g., Vermont permitless carry vs. New York strict licensing)?',
    'Comparative analysis of interstate comity and reciprocity frameworks; examination of whether federal standards could harmonize state variation without collapsing into either extreme; study of how other fundamental rights (speech, voting, religion) maintain parity across federal systems with state variation.',
    'If parity is impossible: the reading cannot be stabilized at national scale — it degrades into a state-level pluralism rather than a constitutional standard. If parity is achieved: the reading requires ongoing institutional coordination to prevent drift toward either individualist or regulatory extreme.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_parity_across_jurisdictions, empirical, 'Interstate regulatory parity feasibility').

omega_variable(
    kernel_reading_contest_between_individualist_and_hybrid,
    'Which reading — individualist (unlimited right) or hybrid-scope-limitation (regulated right) — is warranted by the text''s original public meaning?',
    'Historicist analysis: examination of Founding-era militia law, carrying practices, and contemporary regulations; linguistic analysis of 18th-century usage of ''keep and bear Arms'' and ''infringed''; comparative study of how Founders understood similar conditional rights (speech with sedition laws, religion with state establishments).',
    'If individualist reading is warranted: this hybrid reading is a legitimate progressive reinterpretation (coexists_with the individualist reading); if hybrid reading is warranted: the individualist reading is either a misreading or a values-driven override of the text. The empirical historical record may underdetermine the interpretive choice — in which case this omega documents a conceptual/preference conflict, not an empirical one.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_between_individualist_and_hybrid, conceptual, 'Original public meaning of the Second Amendment text').

omega_variable(
    enforcement_overhead_measurement,
    'How much enforcement infrastructure and state capacity is required to maintain the hybrid-scope distinction (licensed right vs. prohibited access)?',
    'Comparative study of enforcement costs across jurisdictions with different licensing regimes; analysis of enforcement capacity in low-infrastructure states vs. high-capacity states; examination of how enforcement overhead correlates with state capacity and political stability.',
    'If enforcement overhead is low and scalable: the reading is stable across diverse state capacities. If enforcement overhead is high and capacity-dependent: the reading will degrade differently across high-capacity and low-capacity jurisdictions, potentially undermining national standardization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_overhead_measurement, empirical, 'Enforcement infrastructure requirements').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__hybrid_scope_limitation_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_text__hybrid_scope_limitation_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(seco_tr_t15, second_amendment_text__hybrid_scope_limitation_reading, theater_ratio, 15, 0.42).
narrative_ontology:measurement(seco_tr_t30, second_amendment_text__hybrid_scope_limitation_reading, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_text__hybrid_scope_limitation_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(seco_be_t15, second_amendment_text__hybrid_scope_limitation_reading, base_extractiveness, 15, 0.32).
narrative_ontology:measurement(seco_be_t30, second_amendment_text__hybrid_scope_limitation_reading, base_extractiveness, 30, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_text__hybrid_scope_limitation_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(seco_su_t15, second_amendment_text__hybrid_scope_limitation_reading, suppression_requirement, 15, 0.45).
narrative_ontology:measurement(seco_su_t30, second_amendment_text__hybrid_scope_limitation_reading, suppression_requirement, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__hybrid_scope_limitation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_text__hybrid_scope_limitation_reading, second_amendment_text__individualist_reading).
narrative_ontology:affects_constraint(second_amendment_text__hybrid_scope_limitation_reading, second_amendment_text__collectivist_reading).
narrative_ontology:affects_constraint(second_amendment_text__hybrid_scope_limitation_reading, dangerous_person_exception_operationalization).
narrative_ontology:affects_constraint(second_amendment_text__hybrid_scope_limitation_reading, sensitive_place_definition_scope).
narrative_ontology:affects_constraint(second_amendment_text__hybrid_scope_limitation_reading, interstate_licensing_reciprocity).

% DUAL FORMULATION NOTE:
% The Second Amendment kernel decomposes into three constraint stories with distinct epsilons: individualist_reading (ε ≈ 0.08, near-absolute right protection), hybrid_scope_limitation_reading (ε ≈ 0.38, regulated right), collectivist_reading (ε ≈ 0.72, militia-contingent near-prohibition). Each story represents a different resolution of the kernel's textual ambiguity. The three readings coexist as live positions held by different parties in constitutional discourse. They are linked via network.affects_constraints to show their interdependence: each reading's institutional implementation constrains the others' feasibility at national scale.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_text__hybrid_scope_limitation_reading, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
