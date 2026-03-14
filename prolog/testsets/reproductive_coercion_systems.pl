% ============================================================================
% CONSTRAINT STORY: reproductive_coercion_systems
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reproductive_coercion_systems, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: reproductive_coercion_systems
 *   human_readable: Reproductive Coercion Systems
 *   domain: interpersonal/medical/bodily_autonomy
 *
 * SUMMARY:
 *   Reproductive coercion systems comprise intentional behavior designed to
 *   control another person's reproductive choices through manipulation,
 *   threat, or coercion. These include forced pregnancy, forced
 *   sterilization, contraception sabotage, withholding reproductive
 *   information or access, and control over fertility decisions. The
 *   constraint operates across interpersonal, institutional, and patriarchal
 *   structural levels, with extraction flows both from the coerced individual
 *   to the coercer and from reproductive autonomy as a commons to patriarchal
 *   institutional systems. The measurement interval (0-6) represents typical
 *   relationship progression in coercive partnerships: early extraction
 *   moderate (~0.52), escalating as control deepens and alternatives erode
 *   (~0.68). Theater ratio increases as institutional screening protocols
 *   (piton perspective) proliferate without enforcement, raising performative
 *   detection while suppression remains high.
 *
 * KEY AGENTS:
 *   - Reproductive autonomy bearers (primarily women): Primary victims (powerless/trapped, identity_locked) — bear full cost of coercion with minimal exit options
 *   - Coercive partners: Controllers (moderate/identity_locked) — benefit from reproductive control; identity fused with controlling role; see control as necessary for relationship
 *   - Patriarchal institutional systems: Institutional beneficiaries (institutional/arbitrage) — benefit from reproductive coercion through property transmission, paternity certainty, women's economic containment
 *   - Medical system: Institutional gatekeeper (institutional/constrained) — maintains performative protocols without enforcement capacity; degraded piton function
 *   - Reproductive autonomy commons: Collective victim (powerless/trapped at collective level, constrained/moderate at individual level) — abstract right that cannot organize or escape
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reproductive_coercion_systems, 0.68).
domain_priors:suppression_score(reproductive_coercion_systems, 0.78).
domain_priors:theater_ratio(reproductive_coercion_systems, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reproductive_coercion_systems, extractiveness, 0.68).
narrative_ontology:constraint_metric(reproductive_coercion_systems, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(reproductive_coercion_systems, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reproductive_coercion_systems, snare).
narrative_ontology:human_readable(reproductive_coercion_systems, "Reproductive Coercion Systems").
narrative_ontology:topic_domain(reproductive_coercion_systems, "interpersonal/medical/bodily_autonomy").

domain_priors:requires_active_enforcement(reproductive_coercion_systems).

% --- Structural relationships ---
narrative_ontology:constraint_victim(reproductive_coercion_systems, reproductive_autonomy_bearers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE COERCED PARTNER (SNARE) — Trapped by economic dependency, threat of abandonment/custody loss, isolation from support networks, or physical threat. Bears full cost of reproductive control without alternatives. Suppression operates through multiple mechanisms: economic control, isolation, threat, legal vulnerability (immigration status, custody threats). No meaningful exit without risking child safety, financial collapse, or deportation. Maximum experienced extraction.
constraint_indexing:constraint_classification(reproductive_coercion_systems, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: REPRODUCTIVE AUTONOMY COMMONS (SNARE) — Collective bodily autonomy right that cannot exit or organize as an agent. Bears full cost of systematic coercion through intergenerational trauma patterns, health complications, psychological damage, and erosion of reproductive self-determination as a recognized norm. Constrained at individual level but powerless at collective level — no mechanism to restore autonomy after breach.
constraint_indexing:constraint_classification(reproductive_coercion_systems, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE PARTNER WITH COERCIVE CONTROL (TANGLED ROPE) — Benefits from reproductive control (relationship stability, paternity certainty, financial retention of partner labor). Also coordinates genuine relational functions (childcare, household economics, sexual partnership). Identity fused with controller role — relationship identity constituted through control dynamics. Can physically walk away but identity frame makes exit unthinkable; sees control as necessary to preserve family unit. Extraction embedded in coordination.
constraint_indexing:constraint_classification(reproductive_coercion_systems, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 4: THE MEDICAL SYSTEM (PITON) — Maintains performative protocols (screening questions, consent rituals) while institutional barriers prevent intervention: provider time constraints, cultural deference to partner/husband decision-making authority, documentation burden, liability fears, mandatory reporting thresholds. Theater ratio high — the system performs coercion detection through bureaucratic forms but lacks enforcement capacity or willingness. Institutional inertia preserves the ritual despite degraded function.
constraint_indexing:constraint_classification(reproductive_coercion_systems, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PATRIARCHAL INSTITUTIONAL ORDER (ROPE) — Experiences reproductive coercion as pure coordination: organizing sexual access, controlling paternity certainty, allocating reproductive labor. Benefits from the constraint through preservation of asset inheritance, paternity rights, reduced female economic independence. No extraction cost from this perspective — constraint is purely coordinative for patriarchal property transmission. Arbitrage exit available: institutional systems can abandon reproductive control when economically convenient (as in post-demographic-transition societies).
constraint_indexing:constraint_classification(reproductive_coercion_systems, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / BIOLOGICAL NATURALIZATION (MOUNTAIN) — This perspective risks classifying reproductive coercion as a natural law of sexual dimorphism and reproductive biology (women bear pregnancy costs, men guard paternity certainty). However, this is a false summit: the constraint's existence depends on institutional enforcement (legal coverture, medical gatekeeping, economic dependency, threat of violence), not on biology. Cross-cultural and historical variation demonstrates contingency. The engine's false summit detector will identify this as naturalization of a contingent institutional arrangement.
constraint_indexing:constraint_classification(reproductive_coercion_systems, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reproductive_coercion_systems_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(reproductive_coercion_systems, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(reproductive_coercion_systems, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(reproductive_coercion_systems, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(reproductive_coercion_systems, TR),
    TR >= 0.70.

:- end_tests(reproductive_coercion_systems_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Reproductive coercion extracts reproductive labor, bodily autonomy, fertility decision-making capacity, and (often) financial independence through control of childbearing and pregnancy outcomes. The coerced partner loses primary control over their most consequential bodily process. Unlike consensual pregnancy, the coerced context adds psychological damage (undermined autonomy, violated consent, reproductive trauma), health risks from coerced or prevented pregnancy/contraception, and identity damage. The value 0.68 reflects that extraction is severe but not absolute — some coerced partners maintain partial agency in other domains and some escape. Suppression (0.78): High. Multiple suppression mechanisms operate simultaneously: (1) Economic dependency created by pregnancy/childcare burden; (2) Isolation from support networks (partner control, shame, cultural isolation); (3) Physical threat or history of violence; (4) Custody threats (partner threatens to take children); (5) Immigration status dependency (partner controls visa/citizenship); (6) Internalized blame/shame (suppression moves from external to internal). Theater ratio (0.62): Moderate-high. Medical screening protocols ("Are you safe at home?") are increasingly routine, creating appearance of institutional response. However, the theater is substantial: providers document concerns but lack capacity/liability framework for intervention; social services have resource constraints; legal protections are incomplete (cohabitation loopholes, proof burdens). The theater has increased as institutional awareness has grown, without corresponding enforcement capacity.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence. The coerced partner experiences a snare (pure extraction with maximum suppression). The reproductive autonomy commons experiences a snare (abstract victim that cannot exit). The coercive partner experiences a tangled rope (mixing coordination of household/sexual/reproductive functions with extraction through control). The patriarchal institutional order experiences a rope (pure coordination of paternity certainty and property transmission with no extraction cost to the institution). The medical system experiences a piton (performative detection protocols with degraded intervention function). The biological naturalization perspective risks a mountain (reproductive dimorphism as natural law) but the engine detects this as false summit — the constraint's existence depends on institutional enforcement, not biology.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position relative to the coercion flow. The coerced partner is full target (d ≈ 0.95): victim of direct extraction, trapped by economic and legal barriers, no arbitrage option. The reproductive autonomy commons is structural victim (powerless/trapped at collective level): abstract right with no exit mechanism. The coercive partner is mixed: identity-locked moderately benefiting agent (d ≈ 0.25). Their structural relationship is that of beneficiary (extracts reproductive control and household labor) but with identity fusion to the control role — they experience the constraint as necessary for relationship preservation, not as pure extraction. The patriarchal institutional system is institutional beneficiary (d ≈ 0.05): experiences reproductive coercion as coordinative rather than extractive; paternity certainty and asset transmission flow naturally from the system. The medical system is institutionally constrained (d ≈ 0.60): caught between awareness of harm and institutional incapacity to intervene effectively.
 *
 * MANDATROPHY ANALYSIS:
 *   REPRODUCTIVE COERCION AS SNARE EXEMPLAR: The mandatrophy resolves by showing why reproductive coercion cannot be classified as rope (pure coordination) despite patriarchal systems framing it as natural sexual/reproductive organization. The coordination function (if present) is entirely constituted by the extraction mechanism — household members coordinate childcare, but only under coercion; sexual access is negotiated, but only through threat; paternity is assured, but only through reproductive control. The beneficiary (patriarchal system) can achieve coordination through voluntary agreement (as shown in post-demographic-transition societies where reproductive autonomy is granted); the existence of the coercive mechanism reveals it is not necessary for coordination but rather provides asymmetric advantage. The constraint is a snare: extraction wrapped in coordination language. The medical system's piton classification clarifies that institutional response (screening, documentation) is largely performative — the theater increases (more protocols, more screening, more awareness) while suppression remains high (barriers to intervention persist). True scaffold trajectory would require actual sunset clause (legal liability for coercion, enforcement funding, custody protections) with declining theater as real enforcement replaced ritual. Current state is degraded piton: system maintains the appearance of addressing coercion while functional intervention capacity remains low.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_structural_vs_internalized,
    'What proportion of the measured suppression (0.78) is structural (external barriers: economic dependency, custody threats, immigration status) versus internalized (cognitive patterns: shame, self-blame, identity fusion with abuser''s goals)?',
    'Longitudinal post-exit suppression trajectory: if suppression persists after the coercive partner is removed, reclassify proportion as internalized. If suppression drops sharply after exit, the original suppression was primarily structural.',
    'If 70%+ internalized: the constraint''s effective suppression is higher than the structural measure — the target carries suppression with them after exit, requiring therapeutic intervention. If 70%+ structural: exit itself resolves most suppression, though structural rebuilding remains necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural vs internalized suppression mechanism').

omega_variable(
    identity_lock_dissolution_rate,
    'In coercive relationships, what fraction of coerced partners experience identity reframe that enables exit, versus those whose identity remains locked despite awareness that exit is possible?',
    'Follow-up interviews with exit-capable but remaining partners; measurement of identity reframe timing relative to exit decision; comparison of frame-shift vs practical-barrier-removal interventions',
    'If dissolution rare (<20%): identity lock is primary binding mechanism, requiring identity-work interventions before practical exit is possible. If common (>50%): practical barrier removal alone enables exit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_dissolution_rate, empirical, 'Rate of identity frame dissolution enabling exit').

omega_variable(
    medical_gatekeeping_intentionality,
    'Does the medical system''s failure to intervene in reproductive coercion derive from institutional barriers (workflow, liability, training gaps) versus deliberate alignment with coercive partners'' interests?',
    'Provider interviews; institutional policy analysis; comparison of intervention rates across healthcare systems with different liability frameworks and training requirements',
    'If primarily barriers: system can be reformed through protocol change and training. If intentional alignment: piton classification is accurate (system has become the enforcement mechanism). If mixed: tangled rope classification is more accurate than piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medical_gatekeeping_intentionality, empirical, 'Whether medical non-intervention is barrier-based or intentional').

omega_variable(
    coercion_type_extractiveness_variation,
    'Does the extractiveness metric (0.68) accurately capture all forms of reproductive coercion, or does variation by coercion mechanism (forced pregnancy, forced sterilization, contraception sabotage, withholding reproductive information) produce meaningfully different ε values?',
    'Separate constraint stories for each coercion type if ε varies >0.20 between mechanisms. Current value (0.68) is weighted average; decomposition would reveal if some mechanisms approach snare (ε>0.75) while others approach tangled rope (ε~0.55).',
    'If high variation: decompose into constraint family with separate stories per mechanism. If low variation: unified story is appropriate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coercion_type_extractiveness_variation, empirical, 'Whether extractiveness varies by coercion mechanism type').

omega_variable(
    institutional_reform_sustainability,
    'Can institutional interventions (mandatory provider training, screening protocols, legal liability for non-reporting) meaningfully reduce reproductive coercion, or do the underlying power asymmetries (economic, social, patriarchal) re-create coercion through new mechanisms?',
    'Longitudinal comparison of coercion rates pre- and post-intervention in jurisdictions with reformed medical systems; measurement of mechanism-switching versus rate reduction',
    'If mechanisms switch (forced pregnancy → contraception sabotage when forced pregnancy is detected): reform addresses symptoms, not structure. Classification remains snare; intervention theater increases. If rates drop durably: partial scaffold trajectory possible; system moving toward genuine coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_reform_sustainability, empirical, 'Whether institutional reform reduces coercion or merely redirects mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reproductive_coercion_systems, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(repro_coerce_tr_t0, reproductive_coercion_systems, theater_ratio, 0, 0.48).
narrative_ontology:measurement(repro_coerce_tr_t3, reproductive_coercion_systems, theater_ratio, 3, 0.55).
narrative_ontology:measurement(repro_coerce_tr_t6, reproductive_coercion_systems, theater_ratio, 6, 0.62).

% Extraction over time
narrative_ontology:measurement(repro_coerce_be_t0, reproductive_coercion_systems, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(repro_coerce_be_t3, reproductive_coercion_systems, base_extractiveness, 3, 0.6).
narrative_ontology:measurement(repro_coerce_be_t6, reproductive_coercion_systems, base_extractiveness, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reproductive_coercion_systems, attachment_coordination).
narrative_ontology:affects_constraint(reproductive_coercion_systems, intimate_partner_violence).
narrative_ontology:affects_constraint(reproductive_coercion_systems, reproductive_healthcare_access).
narrative_ontology:affects_constraint(reproductive_coercion_systems, custody_threat_dynamics).

% DUAL FORMULATION NOTE:
% Reproductive coercion is structurally upstream of intimate partner violence (coercion often includes threat/violence) and custody threat dynamics (partner threatens child removal), and structurally downstream of patriarchal property transmission systems (which create economic dependency enabling coercion). Each linked constraint has its own ε value and perspectival profile. This story focuses on the coercion mechanism itself (ε=0.68); upstream stories address institutional patriarchy (higher ε); downstream stories address specific threat mechanisms (varying ε).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reproductive_coercion_systems, moderate, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
