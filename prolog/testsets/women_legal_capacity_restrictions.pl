% ============================================================================
% CONSTRAINT STORY: women_legal_capacity_restrictions
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_women_legal_capacity_restrictions, []).

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
 *   constraint_id: women_legal_capacity_restrictions
 *   human_readable: Women's Legal Capacity Restrictions
 *   domain: law/social/gender
 *
 * SUMMARY:
 *   Women's legal capacity restrictions systematically extract authority,
 *   autonomy, and economic agency from women while concentrating power in
 *   male guardians and state patriarchal apparatus. These restrictions appear
 *   across multiple jurisdictions with varying legal formalization: some
 *   embedded in civil codes (guardianship requirements for contracts,
 *   property, travel), others in family law (marriage decision authority,
 *   divorce initiation, custody). The constraint exhibits the full range of
 *   DR classifications across different observer positions, making it
 *   diagnostically useful for understanding how power arrangements are
 *   naturalized as unchangeable. From women subject to restrictions, the
 *   constraint is a snare with maximum extraction and total suppression. From
 *   male beneficiaries, it functions as rope or piton—coordination mechanism
 *   or degraded ritual. From organized movements and international bodies, it
 *   appears as a tangled rope with sunset clause. The theater ratio (0.45)
 *   indicates moderate performativity: restrictions are justified through
 *   religious/cultural essentialism framing, but enforcement requires ongoing
 *   active effort and state violence, not pure consensus. The extractiveness
 *   trajectory (0.55→0.68) reflects intensification during periods of
 *   modernization when women's educational access and market participation
 *   increase, requiring more aggressive enforcement to maintain control.
 *   Suppression (0.78) is high and multi-layered: legal system itself
 *   enforces, families enforce honor norms, communities enforce through
 *   reputation and exclusion, and internalized identity frameworks enforce
 *   compliance.
 *
 * KEY AGENTS:
 *   - Women subject to restrictions: Powerless/trapped (biographical) and identity-locked (generational) — primary victims bearing maximum extraction
 *   - Male guardians and state patriarchal apparatus: Institutional/arbitrage — primary beneficiaries extracting authority and economic control
 *   - Women with economic independence: Moderate/constrained — experience mixed extraction with workaround capacity
 *   - Religious and traditional authority institutions: Institutional/arbitrage at civilizational timescale — maintain restrictions through degraded ritual (piton)
 *   - Women's rights organizations: Organized/constrained — face state suppression but see legal reform path with sunset
 *   - International human rights framework: Powerful/mobile — external pressure for norm alignment and reform
 *   - Analytical observer: Risk of false natural law framing — naturalizing contingent power arrangement as unchangeable cultural/religious property
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(women_legal_capacity_restrictions, 0.68).
domain_priors:suppression_score(women_legal_capacity_restrictions, 0.78).
domain_priors:theater_ratio(women_legal_capacity_restrictions, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(women_legal_capacity_restrictions, extractiveness, 0.68).
narrative_ontology:constraint_metric(women_legal_capacity_restrictions, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(women_legal_capacity_restrictions, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(women_legal_capacity_restrictions, snare).
narrative_ontology:human_readable(women_legal_capacity_restrictions, "Women's Legal Capacity Restrictions").
narrative_ontology:topic_domain(women_legal_capacity_restrictions, "law/social/gender").

domain_priors:requires_active_enforcement(women_legal_capacity_restrictions).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(women_legal_capacity_restrictions, male_relatives).
narrative_ontology:constraint_beneficiary(women_legal_capacity_restrictions, state_patriarchal_apparatus).
narrative_ontology:constraint_victim(women_legal_capacity_restrictions, women).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Women under these restrictions face structural entrapment: cannot sign contracts, own property independently, travel without permission, work without male guardian approval, testify with equal weight in court. Exit barriers are total—legal system itself enforces restriction. Maximum experienced extraction with no alternatives.
constraint_indexing:constraint_classification(women_legal_capacity_restrictions, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% Male guardians and state apparatus experience this as coordination mechanism: allocates legal authority, stabilizes property transmission, centralizes decision-making. Net beneficiaries—extraction flows toward them. Low experienced effective extraction because they designed and maintain the system.
constraint_indexing:constraint_classification(women_legal_capacity_restrictions, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% Women with education, business assets, or professional standing face constrained exit: can navigate restrictions through workarounds (male proxies, parallel informal systems), but at ongoing cost and risk. Mixed experience—some genuine coordination benefits (family stability norms they may internalize) alongside asymmetric extraction (control and subordination).
constraint_indexing:constraint_classification(women_legal_capacity_restrictions, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% Institutions that cite religious or traditional legitimacy for restrictions experience them as degraded rituals: enforcement requires active effort despite weakening social consensus; younger generations question or ignore restrictions; the performative justification (religious duty, cultural preservation) increasingly decouples from actual compliance. Theater ratio high—the restriction persists through institutional inertia and selective enforcement, not voluntary internalization.
constraint_indexing:constraint_classification(women_legal_capacity_restrictions, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% At generational timescale, some women internalize restriction frameworks as identity: maternal role, family honor, proper womanhood. Structurally mobile (could legally exit if laws changed) but identity-locked (internalized framing makes exit unthinkable from within). Classification shift from mountain (trapped at biographical) to rope (identity_locked at generational) reveals cognitive capture mechanism. The binding is internal—the woman carries the restriction even if external barriers removed.
constraint_indexing:constraint_classification(women_legal_capacity_restrictions, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(regional))).

% Organized movements see mixed constraints: genuine coordination problems (collective action for reform requires coordination), but asymmetric extraction (state suppresses organizing, criminalizes dissent, arrests activists). Movement has agency and sees exit path (legal reform, international pressure, cultural norm shift) but faces state-level enforcement against organizing itself.
constraint_indexing:constraint_classification(women_legal_capacity_restrictions, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% Global human rights bodies and reforming states see restrictions as temporary institutional failures with sunset: CEDAW (Convention on Elimination of All Forms of Discrimination Against Women), international pressure, and legal harmonization are building alternative pathways. Mobile exit (powerful states can withdraw from traditionalist alignment without cost). Sunset clause real: growing number of jurisdictions liberalizing or eliminating restrictions.
constraint_indexing:constraint_classification(women_legal_capacity_restrictions, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% The analytical perspective risks naturalizing restrictions as immutable features of 'traditional culture' or 'religious law'—seeing them as unchangeable properties of societies rather than contingent power arrangements. Framing restrictions as natural law justifies non-intervention and obscures active enforcement. Engine false summit detector: these restrictions are maintained through ongoing institutional effort and state violence, not inherent properties of culture or religion.
constraint_indexing:constraint_classification(women_legal_capacity_restrictions, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(women_legal_capacity_restrictions_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(women_legal_capacity_restrictions, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(women_legal_capacity_restrictions, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(women_legal_capacity_restrictions, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(women_legal_capacity_restrictions, TR),
    TR >= 0.70.

:- end_tests(women_legal_capacity_restrictions_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Male guardians extract authority over women's contracts, property, travel, work, and legal standing. Women cannot independently access capital, establish businesses, sign agreements, travel, or seek legal remedies without guardian permission. The extraction is not merely economic but political and epistemic—women's capacity to make claims about themselves is legally disabled. The value (0.68 rather than higher) reflects that some jurisdictions enforce restrictions selectively and that women with economic resources can often navigate around them via informal mechanisms. Suppression (0.78): Very high. Structural barriers are total—the legal system itself enforces restrictions. Family and community enforcement through honor norms, reputation damage, and social exclusion create multiple suppression layers. Internalized identity frameworks suppress from within. Exit barriers are multilayered: economic dependence (cannot work independently), social dependence (family honor tied to compliance), legal barriers (cannot independently contract for housing, transportation, services), and cognitive barriers (identity fusion with family role). Theater ratio (0.45): Moderate. Restrictions are justified through religious and cultural essentialism—framed as unchangeable properties of 'true Islam,' 'authentic tradition,' 'family stability.' But the performative element is visible: enforcement requires active effort, younger generations increasingly ignore or challenge restrictions, parallel informal systems develop (women working through male proxies), and selective enforcement reveals that the restriction depends on ongoing institutional maintenance rather than pure consensus. The theater has increased over the interval (0.25→0.45) as the gap between formal restrictions and actual practice has widened—the performative justification grows louder as actual compliance weakens.
 *
 * PERSPECTIVAL GAP:
 *   Women subject to restrictions (powerless/trapped) classify as snare: maximum extraction with no alternatives. This matches women's structural reality—the legal system itself is the suppression mechanism. Male guardians (institutional/arbitrage) classify as rope: they experience the restrictions as solving coordination problems (property allocation, decision authority, inheritance clarity) with minimal coercive overhead. This matches their experience—the system works for them and requires no costly enforcement from their perspective. Women with economic independence (moderate/constrained) classify as tangled rope: they benefit from family stability norms they may have internalized, but also bear extraction through subordination and control. This mixed experience reflects their structural position—enough autonomy to see alternatives but not enough power to freely choose them. Religious institutions (institutional/arbitrage at civilizational scale) classify as piton: they maintain the restrictions through degraded ritual, enforcement requires effort, social consensus is fragmenting, but institutions persist through inertia. This reflects the visible gap between formal justification and actual enforcement. The analytical observer risks classifying as mountain—naturalizing the constraint as an immutable property of culture or religion. The false summit detector reveals this as reification: restrictions are maintained through ongoing state and community enforcement, not natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each agent is derived from their structural position: beneficiary status, power level, and exit options. Male guardians as institutional/arbitrage beneficiaries have d≈0.05 (full beneficiaries with ability to arbitrage out)—they experience negative effective extraction, the system subsidizes them. Women as powerless/trapped victims have d≈0.95 (full targets with no exit)—they experience maximum f(d)≈1.42 amplification of base extractiveness. Women with economic independence as moderate/constrained have d≈0.70 (partial victims with partial exit capacity)—moderate experienced extraction. Women's organizations as organized/constrained agents have d≈0.55 (victims with coalition capacity)—organized power moderates experienced extraction. The perspectival gap reflects the divergence in d values and resulting chi computations: the system extracts heavily from trapped women and extracts moderately from constrained women, while providing negative extraction (subsidy) to male beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by recognizing that all six classifications are legitimate perspectival readings. Women subject to restrictions genuinely experience a snare (no exit, maximum extraction). Male beneficiaries genuinely experience coordination benefits (the system solves legitimate problems from their position—property allocation, decision concentration, family authority). Institutions maintaining restrictions genuinely experience institutional inertia (piton)—the restrictions persist through degraded enforcement rituals. Reform movements genuinely see a sunset path (scaffold)—international pressure, legal harmonization, and norm shift are building alternative arrangements. The analytical observer's risk is false natural law (mountain)—treating contingent power arrangement as immutable. The mandatrophy resolution: all six types are correct from their indexed positions. The engine's task is not to find 'the true type' but to map the perspectival landscape and identify where natural law framing conceals power arrangement, where coordination framing conceals extraction, and where sunset framing is aspirational rather than structural.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_vs_structural_entrapment,
    'What proportion of women''s compliance derives from internalized identity frameworks vs structural barriers?',
    'Post-reform longitudinal tracking: when legal restrictions are removed, do compliance patterns persist at rates higher than structural barriers would predict? If yes, internalization is significant.',
    'High internalization: constraint persists after formal restriction lifted (requires cognitive/cultural intervention). Low internalization: restriction elimination directly reduces extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_structural_entrapment, empirical, 'Internalized identity-lock vs structural barrier mechanisms').

omega_variable(
    suppression_mechanism_internalization,
    'Is suppression enforced through external state violence, through family/community enforcement of honor norms, or through internalized constraints that women police in themselves?',
    'Separation of enforcement mechanisms: track instances requiring active external enforcement vs passive compliance; measure divergence between public behavior and private expressed preferences; analyze transgression and sanction patterns.',
    'If primarily external: suppression structural, removable by law change. If primarily internalized: suppression persists after formal restriction lifted; requires identity-frame intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is external enforcement or internalized').

omega_variable(
    coordination_function_legitimacy,
    'Do legal capacity restrictions serve genuine coordination functions (property stability, inheritance clarity, family decision authority) or are these framing justifications for pure extraction?',
    'Comparative institutional analysis: identify actual coordination problems restrictions solve (if any) vs alternative institutional arrangements that solve same problems without asymmetric extraction (e.g., unified property systems, joint ownership, explicit consent requirements).',
    'If genuine coordination: some perspectives legitimately see Tangled Rope. If purely extractive: all perspectives see Snare (orchestrated as Rope/Mountain by beneficiaries).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_function_legitimacy, empirical, 'Whether restrictions serve coordination functions or pure extraction').

omega_variable(
    reform_ceiling_mechanisms,
    'What structural factors prevent or slow liberalization of restrictions even when political will exists?',
    'Institutional historical analysis: track reform attempts, identify blocking coalitions, measure lag between international norm adoption and domestic implementation.',
    'If blocking mechanism is institutional inertia (Piton dynamics): reform accelerates as norms shift. If blocking mechanism is structural beneficiary coalitions: reform faces organized resistance proportional to extraction rents.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reform_ceiling_mechanisms, empirical, 'Institutional mechanisms preventing or slowing restriction liberalization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(women_legal_capacity_restrictions, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wome_tr_t0, women_legal_capacity_restrictions, theater_ratio, 0, 0.25).
narrative_ontology:measurement(wome_tr_t10, women_legal_capacity_restrictions, theater_ratio, 10, 0.35).
narrative_ontology:measurement(wome_tr_t20, women_legal_capacity_restrictions, theater_ratio, 20, 0.45).

% Extraction over time
narrative_ontology:measurement(wome_be_t0, women_legal_capacity_restrictions, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(wome_be_t10, women_legal_capacity_restrictions, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(wome_be_t20, women_legal_capacity_restrictions, base_extractiveness, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(women_legal_capacity_restrictions, identity_coordination).
narrative_ontology:affects_constraint(women_legal_capacity_restrictions, womens_property_ownership).
narrative_ontology:affects_constraint(women_legal_capacity_restrictions, consent_requirement_marriage).
narrative_ontology:affects_constraint(women_legal_capacity_restrictions, guardianship_system).

% DUAL FORMULATION NOTE:
% Legal capacity restrictions decompose into multiple structurally distinct constraints: property ownership capacity (legal/economic ε), marriage/divorce decision authority (relational/identity ε), guardianship requirement scope (administrative ε). Each has different evidence status and reform timelines. This story captures the integrated system; specific component stories track individual restriction types with their own ε values and reform dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(women_legal_capacity_restrictions, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
