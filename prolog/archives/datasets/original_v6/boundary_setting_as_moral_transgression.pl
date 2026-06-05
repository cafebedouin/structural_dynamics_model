% ============================================================================
% CONSTRAINT STORY: boundary_setting_as_moral_transgression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_boundary_setting_as_moral_transgression, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: boundary_setting_as_moral_transgression
 *   human_readable: Boundary Setting Framed as Moral Transgression
 *   domain: interpersonal/psychological/relational
 *
 * SUMMARY:
 *   The constraint 'boundary setting as moral transgression' is a relational
 *   extraction mechanism that uses moral and identity-based framing to
 *   suppress agents' capacity to refuse unwanted contact, labor, or resource
 *   extraction. The constraint operates interpersonally (dyadic or
 *   small-group relationships) but scales to institutional contexts (family
 *   systems, religious communities, organizational hierarchies) where moral
 *   narratives ('unconditional love,' 'loyalty,' 'sacrifice') are deployed to
 *   prevent boundary-setters from exercising their autonomy. The constraint
 *   is characterized by: (1) morphing the problem of boundary-setting from a
 *   negotiation task into a moral judgment about the boundary-setter's
 *   character; (2) creating an asymmetry where violation of boundaries by one
 *   party is reframed as relational care while boundary-setting by the other
 *   party is reframed as selfishness; (3) locking boundary-setters into
 *   identity frames where 'good self' and 'boundary setter' are perceived as
 *   incompatible; (4) suppressing material alternatives (housing, childcare,
 *   economic independence) that would make exit viable. The constraint
 *   exhibits high theater ratio (0.65) because enforcement increasingly
 *   relies on moral rhetoric and social shaming rather than material coercion
 *   — the moral framing has become somewhat decoupled from genuine relational
 *   function. Extractiveness has risen over the measurement interval
 *   (0.45→0.68) as boundary violations have accumulated and the moral framing
 *   has ossified into institutional narrative, making individual
 *   boundary-setting increasingly costly and rare.
 *
 * KEY AGENTS:
 *   - Boundary Setter: Primary victim (powerless/identity_locked or moderate/constrained) — experiences suppression both as internalized moral judgment and as material barriers to exit
 *   - Boundary Violator: Primary beneficiary (institutional/arbitrage) — extracts time, emotional labor, resources, with minimal cost to themselves; perceives the constraint as coordination
 *   - Second-Order Authority (parent/therapist/advocate): Secondary victim (moderate/constrained) — attempts to set meta-boundaries; faces suppression and institutional retaliation
 *   - Cultural/Institutional Authority: Institutional actor (institutional/arbitrage) — maintains the moral framing through rhetoric; increasingly performative as enforcement mechanism declines
 *   - Analytical Observer: Civilizational position (analytical/analytical) — sees genuine coordination function alongside genuine extraction; identifies false summit in 'boundaries are inherently immoral' narratives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(boundary_setting_as_moral_transgression, 0.68).
domain_priors:suppression_score(boundary_setting_as_moral_transgression, 0.72).
domain_priors:theater_ratio(boundary_setting_as_moral_transgression, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(boundary_setting_as_moral_transgression, extractiveness, 0.68).
narrative_ontology:constraint_metric(boundary_setting_as_moral_transgression, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(boundary_setting_as_moral_transgression, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(boundary_setting_as_moral_transgression, snare).
narrative_ontology:human_readable(boundary_setting_as_moral_transgression, "Boundary Setting Framed as Moral Transgression").
narrative_ontology:topic_domain(boundary_setting_as_moral_transgression, "interpersonal/psychological/relational").

domain_priors:requires_active_enforcement(boundary_setting_as_moral_transgression).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(boundary_setting_as_moral_transgression, boundary_violators).
narrative_ontology:constraint_victim(boundary_setting_as_moral_transgression, boundary_setters).
narrative_ontology:constraint_victim(boundary_setting_as_moral_transgression, relational_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BOUNDARY SETTER (SNARE) — The agent who attempts to set a limit experiences overwhelming moral framing: 'good people don't say no to family,' 'real love is unconditional,' 'setting boundaries means you're selfish/ungrateful/cold.' The agent's identity has been constituted through relational availability and accommodation. Exit would require becoming a different person (abandoning the 'good child/partner/friend' identity). Structurally mobile (could physically leave) but identity-locked (cannot exercise mobility from within their identity frame). Suppression is maximum: the agent internalizes the moral framing and enforces their own violation.
constraint_indexing:constraint_classification(boundary_setting_as_moral_transgression, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: BOUNDARY SETTER (TANGLED ROPE) — From the constrained exit view, the boundary setter faces material costs to setting limits: economic dependency, housing insecurity, childcare arrangements, or social isolation. The relationship also provides coordination: emotional support, financial stability, social belonging. But these genuine coordination benefits are asymmetric — the cost of maintaining them (accepting boundary violations) is extracted from the boundary setter. High suppression reflects material barriers that persist even if identity frame shifted.
constraint_indexing:constraint_classification(boundary_setting_as_moral_transgression, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: BOUNDARY VIOLATOR (ROPE) — From the beneficiary's view, the constraint functions as pure coordination: 'maintaining access to this person's time, emotional labor, and resources.' The moral framing ('boundaries are selfish') is genuinely perceived as an efficient coordination solution — it maintains relational stability and resource flow without requiring explicit negotiation. The violator sees themselves as protecting the relationship, not extracting. Low perceived extraction; high perceived coordination benefit.
constraint_indexing:constraint_classification(boundary_setting_as_moral_transgression, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: SECOND-ORDER BOUNDARY SETTER (SNARE) — A parent/partner/elder attempting to set boundaries around the constraint itself (e.g., 'my child should be able to say no') faces the same moral suppression. The constraint metastasizes: not only must the boundary setter accept violation, but they cannot name it as violation without being reframed as the problem ('you're poisoning the family by encouraging rebelliousness'). The second-order boundary setter is constrained by the need to protect their child/dependent while not triggering institutional retaliation (family exclusion, custody challenges, etc.).
constraint_indexing:constraint_classification(boundary_setting_as_moral_transgression, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 5: CULTURAL/RELIGIOUS AUTHORITY (PITON) — Institutional actors (clergy, family patriarchs, cultural tradition carriers) who maintain this framing often see themselves as preserving relational harmony and moral order. The enforcement mechanism (moral condemnation of boundary setters) has become largely performative — repeated and theatrical rather than functionally enforcing relational coherence. Theater ratio rises as the authority's actual enforcement capacity declines but the moral rhetoric persists through institutional inertia. The authority is itself captured by the constraint — it maintains the framing not because it works but because alternatives (acknowledging boundary violations as valid) would require institutional transformation.
constraint_indexing:constraint_classification(boundary_setting_as_moral_transgression, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational analytical view, the constraint exhibits genuine coordination function (maintaining relational bonds, creating predictability, sustaining caregiving) alongside asymmetric extraction (boundary violators extract time/resources/emotional labor; boundary setters extract moral capital and relational stability at the cost of autonomy). Active enforcement is real: moral shaming, social exclusion, institutional pressure all maintain the constraint. The tangled rope classification reflects both genuine coordination and genuine extraction, neither reducible to the other.
constraint_indexing:constraint_classification(boundary_setting_as_moral_transgression, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(boundary_setting_as_moral_transgression_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(boundary_setting_as_moral_transgression, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(boundary_setting_as_moral_transgression, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(boundary_setting_as_moral_transgression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(boundary_setting_as_moral_transgression, TR),
    TR >= 0.70.

:- end_tests(boundary_setting_as_moral_transgression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The boundary violator extracts ongoing access to the boundary setter's time, emotional labor, financial resources, and physical/emotional presence without negotiation or reciprocal obligation. The extraction is severe and sustained. The measured value of 0.68 reflects that some genuine coordination (emotional bonding, financial mutual aid, caregiving relationships) exists — this is not pure coercion like slavery, hence not 0.85+. But the asymmetry is severe: boundary violations are normalized; boundary-setters' costs are invisible; alternatives are suppressed. Suppression (0.72): Very high. The moral framing ('boundaries are selfish/cold/ungrateful/betrayal') creates internalized enforcement — the boundary setter enforces their own violation through guilt and shame. Material barriers compound: economic dependency, housing insecurity, childcare access, social isolation, institutional support for the violator. The suppression is both internalized (identity lock) and structural (material barriers). Theater ratio (0.65): Moderate-high. As the constraint has become institutionalized, enforcement increasingly relies on moral rhetoric, social condemnation, and guilt-induction rather than material coercion. The performative aspect has risen because the actual relational function (genuine emotional bonding) has degraded — moral rhetoric now carries enforcement that once was distributed across genuine relational benefit. The theater is not at maximum (0.85+) because material barriers remain real and functional; the rhetoric works because it maps onto actual constraint.
 *
 * PERSPECTIVAL GAP:
 *   The maximum gap appears between the violator (Rope) and the boundary setter (Snare). From the violator's position, the constraint solves the problem 'how do I maintain access to this person's care without explicit renegotiation?' From the boundary setter's position, the constraint IS the problem — it forces them to provide care they don't consent to while experiencing moral shame for their non-consent. The gap is not a measurement disagreement; it is a fundamental asymmetry in what the constraint does for each agent. The analytical observer's Tangled Rope classification resolves the gap by acknowledging that the constraint performs both functions (genuine coordination of relational stability, genuine extraction of asymmetric labor) and that the asymmetry is the actual problem — better coordination would require explicit boundary negotiation, not moral suppression of the need to negotiate.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for boundary violators (beneficiary status + institutional power + arbitrage exit) derives d ≈ 0.10-0.20, producing f(d) ≈ -0.05 to 0.02 — the violator experiences the constraint as supporting them (negative or near-zero effective extraction). Directionality for boundary setters (victim status + powerless/moderate power + identity_locked/constrained exit) derives d ≈ 0.85-0.95, producing f(d) ≈ 1.15-1.42 — the boundary setter experiences maximum effective extraction. The identity_locked exit option produces higher d (≈0.89, f(d)≈1.28) than trapped (≈0.95, f(d)≈1.42) because the binding is cognitive rather than purely material — the agent has structural mobility but identity-based immobility. Second-order boundary setters (moderate power + constrained exit) derive d ≈ 0.65-0.75, experiencing high but not maximum extraction (the constraint limits their agency in protecting their dependents but doesn't fully trap them). Institutional authorities (beneficiary status + institutional power + arbitrage exit + piton) derive d ≈ 0.05-0.15, but the piton classification reflects that their experienced extraction has become largely theatrical — they are trapped by institutional inertia in maintaining the framing, even though they no longer benefit materially from it.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is at risk of mandatrophy misclassification in two directions: (1) Rope overcounting — the genuine coordination function ('relationships need some predictability and mutual obligation') can be mistaken for the moral framing ('boundaries are immoral') and used to justify the extraction. (2) Snare undercounting — the extractiveness (0.68) is high but not maximum (0.85+), and the presence of genuine emotional bonding can obscure that this is extraction, not reciprocal care. The tangled_rope classification requires three gates: (a) beneficiaries declared ✓ (boundary_violators), (b) victims declared ✓ (boundary_setters), (c) active enforcement ✓ (moral rhetoric, social shaming, institutional pressure). All three are met. The mandatrophy is resolved by recognizing that the constraint's legitimacy claim ('we need relational harmony and mutual obligation') is genuine but incomplete — it does not address why harmony must be purchased through asymmetric suppression of one party's autonomy rather than through explicit negotiation. A constraint that maintained relational coordination through transparent boundary negotiation would be Rope; this constraint maintains it through suppression, making it Tangled Rope at minimum and Snare from the powerless perspective.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    internalized_vs_structural_suppression,
    'What proportion of the measured suppression (0.72) is internalized moral framing vs. structural material barriers?',
    'Post-exit suppression trajectory: track whether suppression persists after the boundary setter leaves the relationship. If suppression declines rapidly post-exit, most was internalized. If suppression persists (agent experiences guilt, self-blame, identity fragmentation), internalized component was significant.',
    'If mostly internalized (>70%): the constraint''s effective suppression is higher than the structural measure suggests — the agent carries the enforcement mechanism with them even after escaping material barriers. Reclassify as Snare with higher behavioral grip. If mostly structural (<30%): exit removes suppression; focus interventions on material accessibility (housing, employment, childcare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Internalized vs structural suppression mechanism').

omega_variable(
    moral_framing_authenticity,
    'To what extent do beneficiary actors genuinely believe the moral framing (''boundaries are selfish''), vs. instrumentally deploy it knowing it is false?',
    'Comparative analysis across different relationships: does the same actor frame boundaries as transgression uniformly (suggesting authentic belief) or selectively (suggesting instrumental deployment)? Do they accept boundaries from institutional equals?',
    'If authentic belief (constraint-violators truly think boundaries are immoral): classification remains Snare from victim view, but the violator''s moral agency is genuine — constraint is maintained by distributed false consciousness rather than deliberate extraction. If instrumental (violators know better but strategically frame boundaries as transgression): the extraction is more deliberate; consider upgraded Snare type for high confidence. Classification impact: shifts confidence in malice attribution but not the structural type.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(moral_framing_authenticity, conceptual, 'Whether moral framing is genuine belief or instrumental deployment').

omega_variable(
    identity_locked_threshold,
    'How deep is the identity fusion? Could the boundary setter perceive themselves as ''good'' while setting the boundary if their identity frame shifted (identity_locked classification) or are they trapped by material barriers regardless of framing (trapped classification)?',
    'Thought experiment validation: ask boundary setters ''If you believed that boundaries were moral and necessary, would you set them? What would stop you?'' Material-barrier answers → trapped. Identity-barrier answers → identity_locked. Combined answers → both, weight by prevalence.',
    'If mostly identity_locked: interventions should target identity reframing (therapy, community exposure to boundary-setting models). If mostly trapped: interventions should target material barriers (economic independence, housing access, childcare). If both: interventions must address both simultaneously or the non-addressed barrier will sustain the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_threshold, empirical, 'Depth and mechanism of identity lock vs material trapping').

omega_variable(
    generational_transmission_mechanism,
    'Is the constraint transmitted primarily through explicit moral teaching (''boundaries are selfish'') or through embodied relational modeling (children observe boundary violations and internalize them as normal)?',
    'Qualitative analysis of family narratives: do parents explicitly teach the moral framing or do children infer the norms from observed patterns? Comparison across families with explicit vs implicit transmission.',
    'If explicit teaching: intervention can target the moral framing directly (reframing education). If embodied modeling: intervention requires breaking the relational pattern (family therapy, community exposure). If both: both interventions are necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generational_transmission_mechanism, empirical, 'Generational transmission: explicit teaching vs embodied modeling').

omega_variable(
    coordination_function_authenticity,
    'Does the constraint genuinely coordinate relational stability, or does the ''coordination'' function serve primarily as a framing mechanism to justify extraction?',
    'Historical comparison: relationships that abandoned this constraint (e.g., after boundary-setting therapy) — do they maintain or improve relational stability, emotional closeness, and durability? If relationships improve after abandoning the constraint, the coordination function was fabricated or minimal.',
    'If genuine coordination (relationships degrade without boundary violations): classify as Tangled Rope; coordinate better by explicitly negotiating boundaries rather than enforcing them through moral framing. If fabricated coordination (relationships improve with boundaries): classify as Snare; the coordination narrative is a cover story for pure extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_function_authenticity, empirical, 'Whether constraint genuinely coordinates relational stability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(boundary_setting_as_moral_transgression, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bsmt_tr_t0, boundary_setting_as_moral_transgression, theater_ratio, 0, 0.5).
narrative_ontology:measurement(bsmt_tr_t10, boundary_setting_as_moral_transgression, theater_ratio, 10, 0.6).
narrative_ontology:measurement(bsmt_tr_t20, boundary_setting_as_moral_transgression, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(bsmt_be_t0, boundary_setting_as_moral_transgression, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(bsmt_be_t10, boundary_setting_as_moral_transgression, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(bsmt_be_t20, boundary_setting_as_moral_transgression, base_extractiveness, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(boundary_setting_as_moral_transgression, attachment_coordination).
narrative_ontology:boltzmann_floor_override(boundary_setting_as_moral_transgression, 0.12).
narrative_ontology:affects_constraint(boundary_setting_as_moral_transgression, emotional_coercion_in_families).
narrative_ontology:affects_constraint(boundary_setting_as_moral_transgression, identity_fusion_in_intimate_relationships).
narrative_ontology:affects_constraint(boundary_setting_as_moral_transgression, moral_gaslighting_dynamics).

% DUAL FORMULATION NOTE:
% This constraint is upstream in a family of interpersonal extraction mechanisms. Boundary_setting_as_moral_transgression creates the conditions for identity fusion (downstream) and enables emotional coercion tactics (downstream). Each story has its own extractiveness value reflecting the different observable used to measure it: moral framing (boundary_setting_as_moral_transgression, ε=0.68), identity fusion depth (identity_fusion_in_intimate_relationships, ε=0.55), coercion frequency (emotional_coercion_in_families, ε=0.72). The network links show structural dependencies: moral framing enables and sustains identity fusion; identity fusion enables emotional coercion escalation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(boundary_setting_as_moral_transgression, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
