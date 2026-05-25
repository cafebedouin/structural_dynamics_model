% ============================================================================
% CONSTRAINT STORY: eu_regulatory_capture_by_individual_states
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_regulatory_capture_by_individual_states, []).

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
 *   constraint_id: eu_regulatory_capture_by_individual_states
 *   human_readable: EU Regulatory Capture by Individual Member States
 *   domain: political_economy/regulatory_governance
 *
 * SUMMARY:
 *   The European Union's regulatory framework creates a structural
 *   opportunity for individual member states to capture supranational rules
 *   in favor of their domestic industries while maintaining the appearance of
 *   compliance with common standards. This constraint combines genuine
 *   coordination benefits (single market harmonization enables trade and
 *   integration) with systematic extraction (member states with sufficient
 *   political leverage — population, economic power, Council voting weight —
 *   can informally weaken or selectively enforce regulations to protect
 *   national champions). The constraint has intensified from 2010-2025 as
 *   larger member states (Germany, France, Italy) have increasingly used
 *   regulatory discretion, delayed implementation, and selective enforcement
 *   to shield domestic firms from competition while smaller states lack
 *   equivalent leverage. The EU Commission's formal oversight machinery
 *   (infringement procedures, regulatory review) has become increasingly
 *   performative — procedures are maintained but enforcement against powerful
 *   member states has atrophied. The theater ratio (0.62) reflects that
 *   compliance monitoring generates extensive documentation and procedural
 *   activity while functional verification of actual market access and
 *   competition remains weak. The constraint's core extraction mechanism:
 *   capturing state governments receive regulatory favors from the Commission
 *   or can delay/obstruct unfavorable regulations through Council procedures,
 *   protected domestic industries gain market shelter, while non-capturing
 *   states and EU regulatory integrity bear the costs of weakened
 *   competition, policy divergence, and loss of single-market benefits.
 *
 * KEY AGENTS:
 *   - Capturing Member State Government: Primary beneficiary (institutional/arbitrage) — shields domestic industry from competition, maintains EU market access, can threaten veto or subsidiarity claims to extract favorable terms
 *   - Protected Domestic Industry: Secondary beneficiary (powerful/mobile) — gains regulatory barriers excluding competitors; faces innovation costs and reputational risk from exposure of capture
 *   - Non-Capturing Member States: Primary victim (organized/constrained) — benefits from EU coordination but cannot leverage equivalent capture, faces higher competition from shielded foreign firms, subsidizes capturing states' industrial policy
 *   - EU Regulatory Integrity: Abstract victim (powerless/trapped) — the supranational framework has no mechanism to resist individual state capture, enforcement mechanisms are weakened
 *   - EU Citizens: Distributed victim (powerless/trapped) — as consumers and competitors face reduced choice, higher prices, and slower innovation from weakened competition
 *   - EU Commission: Institutional actor (institutional/arbitrage) — maintains formal oversight authority but lacks political will to enforce against powerful member states; benefits from maintaining the appearance of regulatory control
 *   - Analytical Observer: Civilizational view (analytical/analytical) — reveals the hybrid coordination-extraction structure that institutional actors naturalize as either 'necessary national policy' or 'inherent complexity of multi-level governance'
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_regulatory_capture_by_individual_states, 0.58).
domain_priors:suppression_score(eu_regulatory_capture_by_individual_states, 0.65).
domain_priors:theater_ratio(eu_regulatory_capture_by_individual_states, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_regulatory_capture_by_individual_states, extractiveness, 0.58).
narrative_ontology:constraint_metric(eu_regulatory_capture_by_individual_states, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(eu_regulatory_capture_by_individual_states, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_regulatory_capture_by_individual_states, tangled_rope).
narrative_ontology:human_readable(eu_regulatory_capture_by_individual_states, "EU Regulatory Capture by Individual Member States").
narrative_ontology:topic_domain(eu_regulatory_capture_by_individual_states, "political_economy/regulatory_governance").

domain_priors:requires_active_enforcement(eu_regulatory_capture_by_individual_states).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_regulatory_capture_by_individual_states, capturing_member_state).
narrative_ontology:constraint_beneficiary(eu_regulatory_capture_by_individual_states, protected_domestic_industry).
narrative_ontology:constraint_victim(eu_regulatory_capture_by_individual_states, eu_regulatory_integrity).
narrative_ontology:constraint_victim(eu_regulatory_capture_by_individual_states, non_capturing_member_states).
narrative_ontology:constraint_victim(eu_regulatory_capture_by_individual_states, eu_citizens).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EU REGULATORY INTEGRITY (SNARE) — The supranational regulatory system and member states without sufficient capture leverage face extraction with no exit. They bear the cost of weakened enforcement, contradictory standards, and loss of competitive neutrality. The common regulatory framework has no mechanism to escape individual member state vetoes or informal capture. Maximum experienced extraction.
constraint_indexing:constraint_classification(eu_regulatory_capture_by_individual_states, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NON-CAPTURING MEMBER STATES (TANGLED ROPE) — These states benefit from the EU's regulatory coordination and market access but face significant costs from being unable to capture regulations in their favor. They have some agency through Council voting and coalition-building but face high exit costs (exit from EU) relative to the coordination benefits. Asymmetric extraction: capturing states benefit, non-capturing states subsidize.
constraint_indexing:constraint_classification(eu_regulatory_capture_by_individual_states, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: CAPTURING MEMBER STATE (ROPE) — Benefits from regulatory arbitrage: can shield domestic champions from competition while appearing to comply with EU rules through informal channels or procedural loopholes. Experiences the constraint as coordination — the EU framework enables them to solve the problem of 'how to protect national interests within international rules.' Net beneficiary with high exit options (can threaten veto, subsidiarity claims, opt-outs).
constraint_indexing:constraint_classification(eu_regulatory_capture_by_individual_states, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: PROTECTED DOMESTIC INDUSTRY (TANGLED ROPE) — Benefits from informal regulatory barriers that exclude competitors, but faces real costs: isolation from EU market integration, slower innovation from reduced competition, reputational risk if capture is exposed. Some agency through lobby groups and trade associations; some mobility (can invest elsewhere in EU, or seek to expand capture to other member states). Mixed extraction and coordination.
constraint_indexing:constraint_classification(eu_regulatory_capture_by_individual_states, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: EU COMMISSION (PITON) — The formal EU regulatory apparatus has increasingly become performative theater. The Commission maintains compliance procedures, infringement actions, and regulatory review mechanisms, but enforcement capacity and political will to challenge member states have atrophied. The Commission sees its own process as degraded — maintaining formal oversight while informal capture proceeds unchecked. High theater ratio: extensive documentation and procedure with limited functional verification.
constraint_indexing:constraint_classification(eu_regulatory_capture_by_individual_states, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a global and generational perspective, the EU's regulatory capture by individual states reveals a hybrid mechanism: genuine coordination function (harmonized standards enable trade and integration) exists alongside systematic extraction (member states capture that coordination to benefit domestic firms). The constraint is not pure extraction (it performs real coordination) nor pure coordination (it systematically advantages some at others' cost). The engine should classify this as Tangled Rope, not as natural or inherent fragmentation.
constraint_indexing:constraint_classification(eu_regulatory_capture_by_individual_states, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_regulatory_capture_by_individual_states_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(eu_regulatory_capture_by_individual_states, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(eu_regulatory_capture_by_individual_states, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(eu_regulatory_capture_by_individual_states, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(eu_regulatory_capture_by_individual_states, TR),
    TR >= 0.70.

:- end_tests(eu_regulatory_capture_by_individual_states_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Member states can extract measurable regulatory favors through multiple channels: delayed implementation of directives, selective enforcement of competition rules, exemptions justified as 'national particularity,' and informal influence over Commission enforcement priorities. However, extraction is not total — the single market framework does enforce some common rules, smaller member states can sometimes block or amend proposals, and public pressure occasionally forces reversal. The extractiveness has increased over the interval (0.35 in 2010 to 0.58 in 2025) as political will for supranational enforcement has declined and member states' confidence in using regulatory discretion has grown. Suppression (0.65): High. Non-capturing states face significant barriers to exit — the EU provides too much coordination benefit relative to the cost of staying, but exit (withdrawal under Article 50) carries enormous political and economic costs. Smaller states cannot easily form blocking coalitions because larger states can offer bilateral extraction benefits (exemptions from specific rules, exemptions from specific rules, favorable treatment in merger review) as side payments to defect. The suppression mechanism is not physical constraint but institutional structure: the rules that would enable resistance (Council voting procedures, Commission enforcement) are controlled by those doing the extracting. Theater ratio (0.62): Moderate-high. The EU maintains extensive regulatory machinery (impact assessments, stakeholder consultations, infringement proceedings, regulatory reviews) that generates visible compliance activity. However, functional verification of actual market access and competition remains weak — Commission investigations of member state violations are lengthy, politically sensitive, and often resolved through negotiations that leave underlying capture mechanisms intact. The theater has increased as the Commission maintains procedure (to preserve its authority and legitimacy) while enforcement capacity has atrophied.
 *
 * PERSPECTIVAL GAP:
 *   The capturing state's institutional perspective (Rope) fundamentally diverges from non-capturing states' perspective (Tangled Rope/Snare) because they sit opposite sides of the extraction flow. Both experience the same formal regulatory structure, but directionality (d) inverts: beneficiaries see coordination, victims see extraction. The Commission's piton perspective reveals why institutional reform is difficult — the formal machinery (infringement procedures, regulatory review) is maintained not because it works but because the Commission preserves its own authority through the appearance of enforcement. The analytical observer's generational and global perspective shows that EU regulatory capture is not inherent to supranational governance (not a Mountain) nor a simple coordination problem (not pure Rope), but a hybrid that naturalizes asymmetry as either 'necessary national policy' or 'unavoidable complexity.'
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies sharply across institutional perspectives. The capturing member state has low d (0.15-0.25): it is a beneficiary with arbitrage exit options (can threaten withdrawal or veto). The non-capturing member state has high d (0.70-0.80): it is a victim with constrained exit (can leave the EU but at catastrophic cost). The EU Commission has d around 0.40 (mixed): it benefits from maintaining regulatory authority but loses enforcement leverage against powerful member states. The protected domestic industry has moderate d (0.55-0.65): benefits from capture but faces asymmetric exposure if the constraint weakens. These directionality values derive from the structural relationship: beneficiaries with exit options experience low effective extraction chi; victims with constrained exit experience high chi; mixed beneficiary-victim institutions experience moderate chi. The engine's chi formula (χ = ε × f(d) × σ(S)) captures these differentials automatically from the declared beneficiary/victim structure and exit options. Scope modifier σ(continental) = 1.1 amplifies chi slightly — EU-wide capture affects more actors and hides behind complexity.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that this is genuinely a Tangled Rope (not a misclassified Snare or Rope). The constraint has a real coordination function: harmonized standards do reduce transaction costs and enable trade. But it also has a genuine asymmetric extraction component: member states with capture leverage extract regulatory favors at the cost of those without. The classification requires both components — the coordination function is not theater (it is real), and the extraction is not an unfortunate side effect (it is structural). The challenge for policy resolution is that any attempt to strengthen coordination (more supranational enforcement) collides with member state sovereignty, while any attempt to weaken extraction (redistribute Council power) weakens the coordination benefits that legitimate the whole system. The mandatrophy shows why EU regulatory reform cycles between tightening supranational control (increasing coordination, revealing extraction) and loosening it (reducing enforcement friction, accelerating capture). The resolution mechanism is not to eliminate capture (it is inherent to the hybrid structure) but to make it visible and impose costs on capturing states (reputational, electoral, reciprocal). The theater ratio (0.62) offers a policy lever: increasing functional verification (independent audits of market access, real-time data on regulatory divergence) would reduce theater and make capture visible, potentially imposing the reputational costs that formal enforcement cannot.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capture_mechanism_formality,
    'Is regulatory capture operating through formal Council procedures and legal mechanisms, or through informal channels and procedural obstruction?',
    'Documentary analysis of Council voting patterns, infringement procedures, and impact assessments; comparison of formal positions vs actual enforcement outcomes',
    'If formal: capture is more visible and potentially reversible through voting rule reform. If informal: capture is more durable and requires cultural/institutional change to address. Classification shifts from high-suppression Snare toward more distributed Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capture_mechanism_formality, empirical, 'Formality of capture mechanism (procedural vs informal)').

omega_variable(
    regulatory_coordination_necessity,
    'How much of the EU regulatory framework''s value is genuine coordination benefit vs regulatory arbitrage opportunity?',
    'Economic analysis comparing transaction costs under unified standards vs costs of capture loopholes; measured policy divergence despite single market rules',
    'If coordination >> arbitrage opportunity: constraint is closer to Rope (coordination with extraction overhead). If arbitrage >> coordination value: constraint approaches pure extraction (Snare). Determines whether the beneficiary genuinely needs the shared framework or merely exploits it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_coordination_necessity, empirical, 'Proportion of regulatory value that is genuine coordination vs capture opportunity').

omega_variable(
    member_state_coalition_dynamics,
    'Can non-capturing member states effectively form counter-coalitions to resist capture by larger states, or is individual state capture a stable equilibrium?',
    'Historical analysis of Council coalition patterns; presence/absence of persistent blocking coalitions; vulnerability of counter-coalitions to defection by smaller states receiving bilateral capture benefits',
    'If coalitions are stable: capture is constrainable and organized states have genuine agency. If equilibrium is individual capture: suppression is higher and organized perspectives should shift toward constrained/trapped. Affects whether non-capturing state classifications are Tangled Rope vs Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(member_state_coalition_dynamics, empirical, 'Stability of anti-capture coalitions among member states').

omega_variable(
    commission_enforcement_capacity_trend,
    'Has EU Commission enforcement capacity against member state regulatory violations increased, remained stable, or declined over the 2010-2025 period?',
    'Temporal analysis of infringement proceedings launched, sustained, resolved; staffing levels in DG COMP and relevant regulatory directorates; average time to resolution',
    'If declining: supports Piton classification (degraded institutional function). If stable/increasing: Piton classification is aspirational and Commission retains functional capacity. Affects theater_ratio trend and measurement trajectory.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(commission_enforcement_capacity_trend, empirical, 'Trend in EU Commission enforcement capacity over time').

omega_variable(
    identity_locked_member_state_dynamics,
    'Are smaller member states that have not captured regulations locked into compliance by identity fusion with ''EU member'' status, even when exit would benefit them economically?',
    'Comparative political economy of exit rhetoric vs exit feasibility for small states; analysis of nationalist movements vs EU-integrationist identity in non-capturing states',
    'If identity_locked is dominant: exit options should be re-classified from constrained to identity_locked, which produces different mandatrophy analysis and classification patterns. If economic constraints dominate: trapped or constrained classification is appropriate. Reveals psychological vs material binding mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_member_state_dynamics, conceptual, 'Whether non-capturing states are bound by institutional identity rather than material constraints').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_regulatory_capture_by_individual_states, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eurc_tr_t0, eu_regulatory_capture_by_individual_states, theater_ratio, 0, 0.45).
narrative_ontology:measurement(eurc_tr_t8, eu_regulatory_capture_by_individual_states, theater_ratio, 8, 0.54).
narrative_ontology:measurement(eurc_tr_t16, eu_regulatory_capture_by_individual_states, theater_ratio, 16, 0.62).

% Extraction over time
narrative_ontology:measurement(eurc_be_t0, eu_regulatory_capture_by_individual_states, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(eurc_be_t8, eu_regulatory_capture_by_individual_states, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(eurc_be_t16, eu_regulatory_capture_by_individual_states, base_extractiveness, 16, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_regulatory_capture_by_individual_states, enforcement_mechanism).
narrative_ontology:affects_constraint(eu_regulatory_capture_by_individual_states, european_single_market_competition).
narrative_ontology:affects_constraint(eu_regulatory_capture_by_individual_states, member_state_industrial_policy_coordination).
narrative_ontology:affects_constraint(eu_regulatory_capture_by_individual_states, eu_commission_enforcement_authority).

% DUAL FORMULATION NOTE:
% EU regulatory capture is structurally downstream of the single market coordination framework (affects_constraints) but represents a distinct constraint with its own extractiveness and suppression metrics. The capture mechanism depends on the coordination framework existing, but the coordination framework does not require capture to occur — the capture is a contingent institutional failure, not a necessary feature.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eu_regulatory_capture_by_individual_states, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
