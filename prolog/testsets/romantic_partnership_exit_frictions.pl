% ============================================================================
% CONSTRAINT STORY: romantic_partnership_exit_frictions
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_romantic_partnership_exit_frictions, []).

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
 *   constraint_id: romantic_partnership_exit_frictions
 *   human_readable: Romantic Partnership Exit Frictions
 *   domain: interpersonal/relational
 *
 * SUMMARY:
 *   Romantic partnership exit frictions describe the structural and
 *   institutional mechanisms that make leaving intimate relationships costly,
 *   dangerous, or perceived as impossible. The constraint operates through
 *   multiple interlocking mechanisms: economic dependency (shared housing,
 *   one partner's career sacrifice, unequal asset accumulation), childcare
 *   entanglement (custody uncertainty, loss of daily contact), legal
 *   complexity (divorce fees, asset division battles, spousal support
 *   negotiations), social stigma (shame narratives, community judgment,
 *   identity loss), and internalized identity fusion (self-concept
 *   constituted through 'married person' role, fused relational identity).
 *   The constraint exhibits all six DR types from different agent positions,
 *   revealing how institutional frameworks, legal structures, social norms,
 *   and internalized psychology interact to make exit increasingly costly
 *   over the partnership's lifetime. The extractiveness rises from 0.35
 *   (early partnership, low sunk costs) to 0.62 (mature partnership, maximum
 *   entanglement), showing how the constraint strengthens as dependencies
 *   accumulate. Theater ratio is modest (0.48) because while relationship
 *   maintenance includes performative elements (public displays of
 *   commitment, sexual availability scripts, emotional regulation theater),
 *   the constraint's primary function is genuinely structural — controlling
 *   exit through real material and legal barriers rather than purely symbolic
 *   enforcement.
 *
 * KEY AGENTS:
 *   - Subordinate Partner: Primary victim (powerless/trapped) — bears maximum extraction cost; typically has lower income, higher childcare burden, higher exit cost relative to resources
 *   - Dominant Partner: Primary beneficiary (institutional/arbitrage) — benefits from labor asymmetry, sexual access, financial control, decision-making power; can exit at lower relative cost
 *   - Ambivalent Partner: Secondary actor (moderate/constrained) — experiences both genuine coordination benefits and real extraction; not fully trapped but facing high exit costs
 *   - Legal/Social Institutions: Secondary beneficiary (institutional/constrained) — courts, religious authorities, family governance structures; benefit from fee extraction, power to adjudicate, cultural authority; also genuinely coordinate some relational functions
 *   - Exit Alternative Coalition: Reform-oriented agents (organized/mobile) — family law advocates, no-fault divorce proponents, cohabitation alternative promoters; see exit frictions as solvable
 *   - Marriage Ritual System: Performative institutional actor (institutional/arbitrage) — ceremonies, vows, public recognition; maintains inertial structure through theater rather than functional necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(romantic_partnership_exit_frictions, 0.58).
domain_priors:suppression_score(romantic_partnership_exit_frictions, 0.62).
domain_priors:theater_ratio(romantic_partnership_exit_frictions, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(romantic_partnership_exit_frictions, extractiveness, 0.58).
narrative_ontology:constraint_metric(romantic_partnership_exit_frictions, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(romantic_partnership_exit_frictions, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(romantic_partnership_exit_frictions, tangled_rope).
narrative_ontology:human_readable(romantic_partnership_exit_frictions, "Romantic Partnership Exit Frictions").
narrative_ontology:topic_domain(romantic_partnership_exit_frictions, "interpersonal/relational").

domain_priors:requires_active_enforcement(romantic_partnership_exit_frictions).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(romantic_partnership_exit_frictions, dominant_partner).
narrative_ontology:constraint_beneficiary(romantic_partnership_exit_frictions, institutional_gatekeepers).
narrative_ontology:constraint_victim(romantic_partnership_exit_frictions, subordinate_partner).
narrative_ontology:constraint_victim(romantic_partnership_exit_frictions, relational_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBORDINATE PARTNER (SNARE) — Structurally trapped by economic dependency, childcare obligations, social stigma, legal costs, and internalized identity as 'married person' or 'committed partner.' Cannot exit without material catastrophe: housing loss, custody battles, financial ruin, social isolation. The partnership constraint operates as pure extraction with minimal coordination function from this position. Suppression is maximal — exit capacity is near zero.
constraint_indexing:constraint_classification(romantic_partnership_exit_frictions, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: DOMINANT PARTNER (ROPE) — Experiences the partnership as coordination with low extraction cost. Controls household resources, decision-making, information flow. Exit options are mobile (less economic dependency, social capital more portable). Benefits from the constraint structure while perceiving it as natural or mutually desired. Experiences low effective extraction because can exit at moderate cost.
constraint_indexing:constraint_classification(romantic_partnership_exit_frictions, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: AMBIVALENT PARTNER (TANGLED ROPE) — Some partners experience genuine coordination benefits (emotional intimacy, shared child-rearing, economic efficiency, mutual caregiving) alongside real extraction (labor asymmetry, sexual coercion, emotional regulation burden, autonomy constraints). Not fully trapped but facing high exit costs: custody loss, housing insecurity, social penalty. Moderate power because some capacity for negotiation, but constrained by economic and social dependencies.
constraint_indexing:constraint_classification(romantic_partnership_exit_frictions, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 4: LEGAL/SOCIAL INSTITUTION (TANGLED ROPE) — Institutional framework (marriage law, family courts, social norms, religious structures) coordinates genuine relational functions (property succession, child custody clarity, tax efficiency, social stability) while actively enforcing exit frictions through high divorce costs, custody battles, alimony complexity, and cultural shame. Requires active institutional enforcement — legal machinery, social judgment, religious condemnation. Institutional actors benefit from the constraint structure (fee extraction from divorce proceedings, power to adjudicate custody, cultural authority over 'proper' relationships).
constraint_indexing:constraint_classification(romantic_partnership_exit_frictions, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: MARRIAGE RITUAL SYSTEM (PITON) — From a civilizational view, marriage rituals (ceremonies, vows, social recognition) are largely performative theater that maintains inertial institutional structure rather than serving coordination function. The vows restate abstract commitment; the ceremony is public performance; the social recognition persists regardless of actual relational function. Theater ratio is high — most marriage 'work' (maintaining appearances, performing commitment, sexual availability, emotional management) is performative rather than functionally necessary. The system persists through institutional inertia (expectations, life-planning around marriage, cultural identity fusion with marital status) rather than because it solves coordination problems better than alternatives.
constraint_indexing:constraint_classification(romantic_partnership_exit_frictions, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURALIZATION VIEW (MOUNTAIN) — From analytical distance, relationship exit frictions appear as inherent to human pair-bonding: attachment costs are real, shared child-rearing requires commitment mechanisms, economic interdependence creates friction, social embeddedness makes dissolution costly. This perspective risks treating contingent institutional arrangements (legal marriage monopoly, custody allocation mechanisms, alimony formulas, social shame for divorced persons) as natural laws of pair-bonding. However, comparative analysis (varying legal regimes, different cultural norms around partnership dissolution, historical changes in exit friction) reveals that most frictions are institutional rather than inevitable — a false summit.
constraint_indexing:constraint_classification(romantic_partnership_exit_frictions, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: EXIT ALTERNATIVE COALITION (SCAFFOLD) — Organized advocates (family law reformers, no-fault divorce advocates, cohabitation-as-alternative promoters, feminist legal scholarship) see exit frictions as a solvable coordination problem with a sunset clause. No-fault divorce, shared custody norms, accessible legal aid, childcare decoupling from marriage, economic independence pathways are building alternative structures. From this perspective, the friction is temporary — a mismatch between institutional constraints and actual relational diversity. Organized agents have agency and a pathway out. Theater is moderate because alternative structures are partly already functioning (cohabitation, informal partnerships, community co-parenting) — the transition is underway.
constraint_indexing:constraint_classification(romantic_partnership_exit_frictions, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(romantic_partnership_exit_frictions_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(romantic_partnership_exit_frictions, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(romantic_partnership_exit_frictions, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(romantic_partnership_exit_frictions, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(romantic_partnership_exit_frictions, TR),
    TR >= 0.70.

:- end_tests(romantic_partnership_exit_frictions_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts labor, sexual access, emotional regulation, financial control, and autonomy from the subordinate partner while delivering unequal benefits. Early in partnerships, extractiveness is lower (0.35) because dependencies are minimal and exit costs are moderate. Over time, children, shared assets, intertwined finances, and identity fusion accumulate, raising extractiveness to 0.62 by year 8. This is not maximum extraction (which would be 0.80+) because many partnerships also provide genuine coordination benefits (shared child-rearing, mutual caregiving, economic efficiency), and some partners maintain partial autonomy. Suppression (0.62): Moderate-high, reflecting multiple overlapping barriers: economic dependency (housing, income inequality, childcare costs), legal complexity (divorce filing fees, property division fights, custody battles costing $10,000-50,000+), social penalties (custody presumptions against mothers, judgment for 'failed' marriages, isolation from married social circles), and internalized identity fusion. Suppression is not total (0.90+) because some exits are possible, some legal aid exists, some social acceptance of dissolution is emerging. Theater ratio (0.48): Moderate. Relationship maintenance includes genuine coordination work (child-rearing, household management, emotional support) and performative theater (public displays of commitment, sexual availability scripts, emotional management performances). The theater is less than in marriage rituals (0.72) because daily partnership has functional necessity — you cannot raise children or share housing purely through ceremony. But the theater is significant because much relationship work is about maintaining external appearances (staying for the children, performing happiness for social circles, managing reputation) rather than actual relational function.
 *
 * PERSPECTIVAL GAP:
 *   The gap between subordinate partner (Snare) and dominant partner (Rope) is the engine's primary diagnostic signal: the same constraint structure is experienced as extractive from below and coordinative from above. This gap reveals that the constraint's classification depends entirely on the agent's structural position. No single type is 'correct' — the presheaf over all positions is. The mountain perspective is a false summit: treating partnership exit friction as natural law naturalizes institutional choices (fault-based divorce, custody allocation rules, legal fee structures) as inevitable constraints. The piton perspective correctly identifies that much marriage ritual (vows, ceremonies, public recognition) is performative — the functions these perform (commitment signaling, social recognition, legal recognition) could be performed by alternatives (cohabitation with commitment ceremonies, informal social recognition, accessible legal partnership registration) with lower theater ratio and lower enforcement costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is computed from structural position. Subordinate partner: high d (~0.90) derives from trapped exit options + victim status in labor/autonomy extraction. Dominant partner: low d (~0.15) derives from arbitrage exit + beneficiary status in labor/sexual/financial extraction. Ambivalent partner: moderate d (~0.55) reflects constrained exit but also some beneficiary status (coordination benefits). Institutional actors: moderate d (~0.45-0.60) reflects that they benefit from enforcement (fee extraction, cultural authority) but also genuinely coordinate some relational functions (custody clarity, property succession, child protection). Reform coalition: low d (~0.25) reflects mobile exit options (they can advocate exit friction reduction) and moderate beneficiary status from implementation (law reformers benefit professionally). The engine applies the sigmoid f(d) to these values: high d (trapped victim) produces high f(d) ~1.42 → maximum experienced extraction; low d (arbitrage beneficiary) produces negative f(d) ~-0.12 → extraction subsidizes this agent. This directionality structure explains why the constraint feels like pure extraction from below but coordination from above — the d values are structurally different.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that classification divergence is not a problem to solve but a structural fact to report. The same constraint system (romantic partnership + exit frictions) is simultaneously Snare (from victim perspective), Rope (from beneficiary perspective), Tangled Rope (from ambivalent perspective and institutional perspective), Piton (from ritual system perspective), Scaffold (from reform coalition perspective), and false Mountain (from naturalizing observer perspective). The mandatrophy is not 'which type is correct?' but 'what is the presheaf structure across all positions?' The ambivalence of many partners (experiencing both coordination benefits and extraction) is not a classification error but a structural reality of tangled_rope type — genuine coordination functions embedded in asymmetric extraction. The reform coalition's scaffold perspective is not aspirational but structural: the alternative pathways (cohabitation, co-parenting, communal living, friendship-based caregiving) already exist and function; the scaffold sunset is real. The piton classification accurately captures that much marriage ritual (vows, ceremonies, public performance) maintains institutional inertia rather than solving coordination problems better than alternatives. The false mountain classification reveals the core mandatrophy resolution: a naturalizing observer treats exit friction as inherent to pair-bonding, but structural analysis shows most friction (0.50+ of 0.62 suppression) derives from institutional design choices, not biological necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_vs_structural_trap,
    'Is the measured suppression (0.62) primarily structural barriers (economic, legal, childcare) or internalized identity fusion (self-concept as ''married person,'' fused relational identity)?',
    'Post-exit trajectory analysis: if suppression declines after barrier removal (economic independence achieved, custody stable), suppression was structural. If suppression persists (agent cannot imagine alternative self, experiences identity dissolution despite material safety), suppression is internalized identity lock. Longitudinal follow-up of exited partners; comparison of suppression during relationship vs after barrier removal.',
    'If structural: reclassify subordinate partner perspective exit_options from ''trapped'' to ''constrained'' (barriers are high but surmountable). If internalized: identity_locked exit is appropriate; suppression is effectively higher than measured because it persists after barriers fall.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_structural_trap, empirical, 'Whether suppression is structural or identity-based').

omega_variable(
    coordination_benefit_authenticity,
    'The ambivalent partner perspective claims genuine coordination benefits (emotional intimacy, shared child-rearing, mutual caregiving) alongside extraction. Are these coordination functions or performative claims masking extraction?',
    'Comparative case analysis: do partners in ''high-extraction'' relationships (high labor asymmetry, emotional domination, sexual coercion) still report coordination benefits? If yes, investigate whether benefits are actual or defensive (agent minimizes costs because exit is too high to acknowledge). Comparison with non-partnered individuals'' access to same benefits via alternative arrangements (co-parenting, communal living, friendship-based caregiving networks).',
    'If coordination is genuine: tangled_rope classification is correct for many partnerships. If coordination is defensive rationalization: many partnerships currently classified as tangled_rope belong in snare category, with higher ε. Classification shifts affect victim group identification and beneficiary/victim directionality derivation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_benefit_authenticity, empirical, 'Whether reported coordination benefits are genuine or defensive rationalization').

omega_variable(
    institutional_enforcement_necessity,
    'How much of the measured exit friction (0.62 suppression) depends on active institutional enforcement (courts, legal fees, social sanctions) versus passive structural dependencies (no alternative housing, childcare infrastructure, income)?',
    'Comparative jurisdiction analysis: compare suppression levels in regimes with high active enforcement (fault-based divorce, custody presumptions favoring male partners, alimony mandatory) vs low active enforcement (no-fault divorce, no-cost mediation, joint custody default). Temporal analysis: how much suppression declines when legal enforcement mechanisms are reformed without changing material dependencies.',
    'If enforcement-dependent: reducing institutional active enforcement would materially lower exit friction. If dependency-dependent: legal reform alone insufficient — must also address economic independence, childcare infrastructure, social support. Affects which institutional actors are properly classified as beneficiaries (courts/lawyers vs employers/social structure).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_enforcement_necessity, empirical, 'Whether exit friction depends on active institutional enforcement or passive structural dependency').

omega_variable(
    dominant_partner_awareness,
    'Does the dominant partner perceive the partnership as coordination (rope perspective) or as beneficial extraction (beneficiary in tangled_rope)? Are these the same thing or different structural positions?',
    'Qualitative analysis of dominant partners'' framing: do they describe partnership as ''solving coordination problem'' or as ''getting what they want''? Behavioral indicator: how much enforcement/vigilance do dominant partners invest in maintaining the constraint? If high vigilance, dominant partner may experience extraction benefits (rope misses the enforcement cost). Comparative analysis of partnership stability predictions based on dominant partner perspective.',
    'If rope is accurate: dominant partner genuinely sees mutual coordination, and beneficiary/victim asymmetry is less total. If extraction-aware: dominant partner should be reclassified with higher d value, reflecting that they are enforcer, not coordinator. Affects institutional actor classification — are courts/religious authorities truly coordinating or actively extracting?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dominant_partner_awareness, empirical, 'Whether dominant partners perceive partnership as coordination or beneficial extraction').

omega_variable(
    natural_attachment_vs_institutional_lock,
    'The mountain perspective claims exit frictions are inherent to human pair-bonding. How much of the measured friction derives from biological/psychological attachment vs institutional structures designed to prevent exit?',
    'Comparative analysis: measure attachment strength (separation distress, pair-bond formation speed) in humans vs institutional exit friction (divorce costs, custody battles). Do humans in low-friction exit regimes (no-fault divorce, co-parenting alternatives available) show different attachment patterns or slower pair dissolution? Cross-cultural comparison: do societies with minimal institutional exit frictions show different partnership formation/dissolution dynamics?',
    'If attachment-driven: mountain classification has legitimate basis — inherent biological/psychological foundation to friction. If institution-driven: mountain is false summit; most friction (0.50+ of 0.62) is contingent institutional architecture. Affects whether partnership exit friction is classified as immutable or reformable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_attachment_vs_institutional_lock, empirical, 'How much partnership exit friction is biological attachment vs institutional structure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(romantic_partnership_exit_frictions, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rpef_tr_t0, romantic_partnership_exit_frictions, theater_ratio, 0, 0.35).
narrative_ontology:measurement(rpef_tr_t3, romantic_partnership_exit_frictions, theater_ratio, 3, 0.42).
narrative_ontology:measurement(rpef_tr_t6, romantic_partnership_exit_frictions, theater_ratio, 6, 0.48).
narrative_ontology:measurement(rpef_tr_t10, romantic_partnership_exit_frictions, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(rpef_be_t0, romantic_partnership_exit_frictions, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(rpef_be_t2, romantic_partnership_exit_frictions, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(rpef_be_t5, romantic_partnership_exit_frictions, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(rpef_be_t8, romantic_partnership_exit_frictions, base_extractiveness, 8, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(romantic_partnership_exit_frictions, attachment_coordination).
narrative_ontology:affects_constraint(romantic_partnership_exit_frictions, domestic_labor_asymmetry).
narrative_ontology:affects_constraint(romantic_partnership_exit_frictions, sexual_coercion_in_partnership).
narrative_ontology:affects_constraint(romantic_partnership_exit_frictions, custody_determination_mechanism).
narrative_ontology:affects_constraint(romantic_partnership_exit_frictions, financial_interdependence_lock).

% DUAL FORMULATION NOTE:
% Romantic partnership exit frictions decompose into multiple structurally distinct constraints sharing a common institutional framework (marriage law, social norms, economic structures) but having different ε values and different primary mechanisms. Financial interdependence has ε~0.45 (coordination function + moderate extraction); sexual coercion has ε~0.72 (minimal coordination, maximum extraction); domestic labor asymmetry has ε~0.55 (mixed); custody determination has ε~0.68 (enforcement-dependent). This story captures the aggregate constraint from all these mechanisms operating together. Upstream is the institutional framework itself (marriage law, legal fee structures, custody presumptions) which could be decomposed as a separate story. Downstream are specific relationship pathologies (abuse, control, isolation) which operate on top of and within the partnership exit friction constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(romantic_partnership_exit_frictions, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
