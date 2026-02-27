% ============================================================================
% CONSTRAINT STORY: eu_irgc_terrorist_designation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [POTENTIAL]
% ============================================================================

:- module(constraint_eu_irgc_terrorist_designation, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: eu_irgc_terrorist_designation
 *   human_readable: EU Terrorist Designation of Iran's IRGC
 *   domain: geopolitical
 *
 * SUMMARY:
 *   The potential designation of Iran's IRGC as a terrorist organization by
 *   the EU is a complex geopolitical constraint. It aims to curb the IRGC's
 *   destabilizing activities but carries significant risks, including
 *   escalating tensions and hindering diplomatic efforts. The EU's
 *   decision-making process, the IRGC's influence within Iran, and the
 *   broader regional dynamics all contribute to the complexity of this
 *   constraint.
 *
 * KEY AGENTS:
 *   - EU Member States: Primary beneficiary (institutional/arbitrage) – aims to enhance security and deter IRGC activities.
 *   - Iranian Economy: Primary victim (powerless/trapped) – faces increased sanctions and economic isolation.
 *   - EU-Iran Diplomacy: Constrained actor (moderate/constrained) – faces limitations on diplomatic options.
 *   - IRGC Rivals: Powerful actors (powerful/mobile) - benefit from diminished IRGC influence
 *   - Analytical Observer: Assesses long-term, global effects (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_irgc_terrorist_designation, 0.6).
domain_priors:suppression_score(eu_irgc_terrorist_designation, 0.7).
domain_priors:theater_ratio(eu_irgc_terrorist_designation, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_irgc_terrorist_designation, extractiveness, 0.6).
narrative_ontology:constraint_metric(eu_irgc_terrorist_designation, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(eu_irgc_terrorist_designation, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_irgc_terrorist_designation, tangled_rope).
narrative_ontology:human_readable(eu_irgc_terrorist_designation, "EU Terrorist Designation of Iran's IRGC").
narrative_ontology:topic_domain(eu_irgc_terrorist_designation, "geopolitical").

domain_priors:requires_active_enforcement(eu_irgc_terrorist_designation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_irgc_terrorist_designation, eu_member_states).
narrative_ontology:constraint_beneficiary(eu_irgc_terrorist_designation, irgc_rivals).
narrative_ontology:constraint_victim(eu_irgc_terrorist_designation, iranian_economy).
narrative_ontology:constraint_victim(eu_irgc_terrorist_designation, irgc_legitimate_activities).
narrative_ontology:constraint_victim(eu_irgc_terrorist_designation, eu_iran_diplomacy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Iranian economy is trapped, bearing the brunt of increased sanctions and reputational damage. Limited exit options due to dependence on the IRGC and international isolation.
constraint_indexing:constraint_classification(eu_irgc_terrorist_designation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% EU-Iran diplomacy is constrained. On one hand the designation limits diplomatic options and dialogue, but on the other it offers a tool to address IRGC's actions in the region. Some agency and some cost.
constraint_indexing:constraint_classification(eu_irgc_terrorist_designation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% EU member states benefit from the designation as it provides a coordinated approach to counter IRGC's activities and potentially deters future actions. They have the ability to adjust their policies based on evolving circumstances.
constraint_indexing:constraint_classification(eu_irgc_terrorist_designation, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% Rivals of the IRGC benefit as it weakens the IRGC's influence and ability to operate globally. However, the benefit to rivals may come with costs of increased instability or conflict.
constraint_indexing:constraint_classification(eu_irgc_terrorist_designation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% Analytical perspective looking at the long-term effects, seeing it as a tangled rope because while it does provide security for some countries, it restricts others and can lead to an increase in global unrest.
constraint_indexing:constraint_classification(eu_irgc_terrorist_designation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_irgc_terrorist_designation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(eu_irgc_terrorist_designation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(eu_irgc_terrorist_designation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(eu_irgc_terrorist_designation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eu_irgc_terrorist_designation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): Moderate-high. The designation extracts from the Iranian economy and limits the IRGC's legitimate activities. Suppression (0.70): High. The designation actively suppresses the IRGC's ability to operate in Europe and engage in financial transactions. Theater Ratio (0.30): Low. There is some performative aspect to the designation, but it is a substantive action with real consequences.
 *
 * PERSPECTIVAL GAP:
 *   The Iranian economy perceives the designation as a Snare, limiting its options and exacerbating economic hardship. EU member states see it as a Rope, facilitating a coordinated approach to counter the IRGC. EU-Iran diplomacy views it as a Tangled Rope, hindering diplomatic efforts while providing a tool to address IRGC's actions. IRGC rivals see it as a Tangled Rope - helping to diminish a threat, but perhaps at the cost of instability.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality values reflect the structural positions of the agents. EU member states, as beneficiaries with arbitrage options, experience low or negative effective extraction. The Iranian economy, as a trapped victim, bears maximum extraction. EU-Iran diplomacy experiences a moderate extraction due to constrained options. The analytical observer balances both sides.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as Tangled Rope prevents mislabeling the constraint as purely a Snare or a Rope. It acknowledges the coordination benefits for the EU while recognizing the extraction costs for Iran. The heterogeneity of perspectives demonstrates the nuanced and multifaceted nature of this geopolitical constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    irgc_activities_attribution,
    'To what extent can specific destabilizing activities be definitively attributed to the IRGC as opposed to other actors within Iran?',
    'Intelligence gathering, forensic analysis of attacks, defector testimony.',
    'Stronger attribution strengthens justification for designation. Weak attribution undermines legitimacy and intensifies diplomatic costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(irgc_activities_attribution, empirical, 'The degree to which destabilizing activities can be attributed to the IRGC').

omega_variable(
    eu_internal_consensus,
    'Can the EU member states maintain a united front regarding the designation, given differing economic and political interests with Iran?',
    'Diplomatic negotiations, economic impact assessments, security threat briefings.',
    'Strong consensus enhances the effectiveness of the designation. Internal divisions weaken its impact and invite Iranian countermeasures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eu_internal_consensus, conceptual, 'The level of internal consensus within the EU regarding the designation').

omega_variable(
    iranian_regime_response,
    'How will the Iranian regime respond to the designation, and will its response escalate regional tensions or lead to diplomatic concessions?',
    'Monitoring Iranian rhetoric, military deployments, diplomatic overtures, and support for proxy groups.',
    'Escalatory response increases the risk of conflict. Concessions may open avenues for de-escalation and negotiation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(iranian_regime_response, preference, 'How the Iranian regime will respond to the designation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_irgc_terrorist_designation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_i_tr_t0, eu_irgc_terrorist_designation, theater_ratio, 0, 0.2).
narrative_ontology:measurement(eu_i_tr_t5, eu_irgc_terrorist_designation, theater_ratio, 5, 0.3).
narrative_ontology:measurement(eu_i_tr_t10, eu_irgc_terrorist_designation, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(eu_i_be_t0, eu_irgc_terrorist_designation, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(eu_i_be_t5, eu_irgc_terrorist_designation, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(eu_i_be_t10, eu_irgc_terrorist_designation, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_irgc_terrorist_designation, enforcement_mechanism).
narrative_ontology:affects_constraint(eu_irgc_terrorist_designation, iran_nuclear_deal).
narrative_ontology:affects_constraint(eu_irgc_terrorist_designation, eu_iran_trade_relations).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
