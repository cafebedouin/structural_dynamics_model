% ============================================================================
% CONSTRAINT STORY: ministerial_responsibility__resignation_norm_decay_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ministerial_responsibility__resignation_norm_decay_reading, []).

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
 *   constraint_id: ministerial_responsibility__resignation_norm_decay_reading
 *   human_readable: Ministerial Responsibility: Resignation Norm Decay
 *   domain: constitutional_law/parliamentary_accountability
 *
 * SUMMARY:
 *   The ministerial responsibility convention — the expectation that a
 *   minister falls from office when their department fails — has eroded from
 *   a binding norm (exemplified by the 1954 Crichel Down case, where Sir
 *   Thomas Dugdale resigned as Agriculture Minister over departmental error)
 *   to a rhetorical flourish cited mainly in its violation. Ministers now
 *   apologize, reshuffle, or remain in post after operational failures that
 *   would once have been terminal. This constraint instantiates the decay
 *   reading of the ministerial responsibility kernel: the convention itself
 *   is understood as weakened, its suppressive force diminishing, its
 *   extractive price for failure renegotiated downward in favor of embattled
 *   incumbents and party leadership. The structural mechanism is not the
 *   absence of parliamentary theater — formal accountability processes
 *   persist — but the attenuation of the sanction those processes once
 *   enforced. As suppression weakens, extractiveness moderates: failed
 *   ministers can now negotiate survival, shifting the balance from
 *   extraction (party bears the cost of ministerial failure) to coordination
 *   (party and minister share the management of failure within an attenuated
 *   accountability frame).
 *
 * KEY AGENTS:
 *   - Embattled Ministers and Party Leadership: Primary beneficiaries (institutional/arbitrage) — benefit directly from the convention's decay as resignations become optional rather than mandatory, enabling party stability over accountability.
 *   - The Convention's Deterrent Force: Primary victim (powerless/trapped) — the residual expectation that failure carries a cost, now unenforced and voiceless. Abstract collective good with no organizing constituency.
 *   - Public Accountability: Secondary victim (moderate/constrained) — the public's ability to hold government to account through the sanction of ministerial removal is constrained by the convention's decay. Limited exit: voters can remove the party but not individual failed ministers within a parliament.
 *   - Opposition and Parliamentary Critics: Moderate agent (moderate/constrained) — benefit from the convention's existence as a tool for accountability but face constraints in enforcing it due to party discipline and floor access asymmetries. Their leverage decays as the convention weakens.
 *   - Select Committee System: Organized alternative (organized/constrained) — emerging substitute accountability mechanism with sunset logic relative to the traditional convention.
 *   - Parliamentary Theater: Institutional performance (institutional/arbitrage) — maintains formal accountability rituals (questioning, apologies, confidence motions) while their actual enforcement power decays. Characterized as piton: the form persists through institutional inertia while function atrophies.
 *   - Analytical Observer: Civilizational risk (analytical/analytical) — risks naturalizing the decay as inevitable structural feature of modern democracy rather than contingent institutional choice benefiting identifiable actors.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ministerial_responsibility__resignation_norm_decay_reading, 0.58).
domain_priors:suppression_score(ministerial_responsibility__resignation_norm_decay_reading, 0.48).
domain_priors:theater_ratio(ministerial_responsibility__resignation_norm_decay_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ministerial_responsibility__resignation_norm_decay_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(ministerial_responsibility__resignation_norm_decay_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(ministerial_responsibility__resignation_norm_decay_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ministerial_responsibility__resignation_norm_decay_reading, tangled_rope).
narrative_ontology:human_readable(ministerial_responsibility__resignation_norm_decay_reading, "Ministerial Responsibility: Resignation Norm Decay").
narrative_ontology:topic_domain(ministerial_responsibility__resignation_norm_decay_reading, "constitutional_law/parliamentary_accountability").

domain_priors:requires_active_enforcement(ministerial_responsibility__resignation_norm_decay_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ministerial_responsibility__resignation_norm_decay_reading, '2109d673-6a37-4697-8d8f-2415b9a64cd2').
narrative_ontology:cs_kernel_codification('2109d673-6a37-4697-8d8f-2415b9a64cd2', fixed_text).
narrative_ontology:cs_authority_grounding('2109d673-6a37-4697-8d8f-2415b9a64cd2', lineage).
narrative_ontology:cs_interpretation_layer_present('2109d673-6a37-4697-8d8f-2415b9a64cd2').
narrative_ontology:cs_reading_relation('2109d673-6a37-4697-8d8f-2415b9a64cd2', ministerial_responsibility__agency_accountability_gap_reading, coexists_with).
narrative_ontology:cs_reading_relation('2109d673-6a37-4697-8d8f-2415b9a64cd2', ministerial_responsibility__select_committee_accountability_reading, coexists_with).
narrative_ontology:cs_axiom('2109d673-6a37-4697-8d8f-2415b9a64cd2', foundational, ministerial_resignation_is_enforceable_norm).
narrative_ontology:cs_axiom_status(ministerial_resignation_is_enforceable_norm, overridden).
narrative_ontology:cs_axiom_grounding('2109d673-6a37-4697-8d8f-2415b9a64cd2', ministerial_resignation_is_enforceable_norm, conventional).
narrative_ontology:cs_axiom('2109d673-6a37-4697-8d8f-2415b9a64cd2', foundational, departmental_failure_triggers_cabinet_removal).
narrative_ontology:cs_axiom_status(departmental_failure_triggers_cabinet_removal, overridden).
narrative_ontology:cs_axiom_grounding('2109d673-6a37-4697-8d8f-2415b9a64cd2', departmental_failure_triggers_cabinet_removal, deontological).
narrative_ontology:cs_reference_frame('2109d673-6a37-4697-8d8f-2415b9a64cd2', resignation_as_mandatory_sanction).
narrative_ontology:cs_drift_state('2109d673-6a37-4697-8d8f-2415b9a64cd2', contemporary_post_2000, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2109d673-6a37-4697-8d8f-2415b9a64cd2', '').
narrative_ontology:cs_kernel_id(ministerial_responsibility__resignation_norm_decay_reading, ministerial_responsibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ministerial_responsibility__resignation_norm_decay_reading, embattled_ministers).
narrative_ontology:constraint_beneficiary(ministerial_responsibility__resignation_norm_decay_reading, party_leadership).
narrative_ontology:constraint_victim(ministerial_responsibility__resignation_norm_decay_reading, convention_deterrent_force).
narrative_ontology:constraint_victim(ministerial_responsibility__resignation_norm_decay_reading, public_accountability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The residual expectation that failure carries a price has no enforcement mechanism and no organized constituency. It exists only as an internalized norm among MPs and the public — a diffuse, voiceless victim. Cannot exit or appeal. Bears the full cost of norm decay as the sanction disappears.
constraint_indexing:constraint_classification(ministerial_responsibility__resignation_norm_decay_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Opposition parties benefit from the convention's existence as a tool for demanding resignations and holding majorities accountable, but face constraints in exercising it — limited parliamentary time, party loyalty norms, asymmetric floor access. The convention's decay directly reduces their leverage. Mixed: they want the norm enforced (coordination benefit) but lack power to enforce it (extraction experience).
constraint_indexing:constraint_classification(ministerial_responsibility__resignation_norm_decay_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Primary beneficiaries. Party leadership benefits from the convention's decay — failed ministers can be retained, moved, or rehabilitated without the party absorbing a resignation cost. Ministers in crisis benefit directly: the threat of forced resignation weakens. Experiences the constraint as coordination: maintaining party cohesion and cabinet stability. Net beneficiary — the extraction mechanism runs toward this agent.
constraint_indexing:constraint_classification(ministerial_responsibility__resignation_norm_decay_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Formal parliamentary procedures (confidence votes, censure motions) exist and are invoked, but carry diminishing actual force. The ritual persists — ministers are questioned, apologies given, Opposition makes speeches — but the sanction it once carried is degraded. Theater ratio high because the form is maintained while the function (enforced resignation) has atrophied.
constraint_indexing:constraint_classification(ministerial_responsibility__resignation_norm_decay_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% The select committee system (Public Accounts Committee, specific departmental committees) represents an emerging alternative accountability pathway with sunset logic: if sustained parliamentary interrogation shifts from the floor to committee chairs, the traditional resignation convention's role diminishes further. Organized but constrained by limited enforcement power short of calling for resignation.
constraint_indexing:constraint_classification(ministerial_responsibility__resignation_norm_decay_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Structurally positioned between ministers and parliamentary accountability. Benefit from minister retention (less turnover = institutional stability) but also constrained by the minister's reduced accountability (civil servants' own delegation shield weakens if ministers cannot be held to account). Mobile — can move to private sector or other departments — but experience tension between coordination and extraction.
constraint_indexing:constraint_classification(ministerial_responsibility__resignation_norm_decay_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% From a civilizational perspective, the decay might appear inherent to democratic governance: conventions erode as political conditions shift, majoritarian discipline weakens, and exit costs become internalized rather than externalized. The resignation convention's decline reads as a structural inevitability of modern parliamentary politics. However, the structural data reveals beneficiaries (party leadership, ministers in crisis) and specific institutional mechanisms (asymmetric party discipline, reduced Opposition leverage) — this naturalizes a contingent choice.
constraint_indexing:constraint_classification(ministerial_responsibility__resignation_norm_decay_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ministerial_responsibility__resignation_norm_decay_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ministerial_responsibility__resignation_norm_decay_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ministerial_responsibility__resignation_norm_decay_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ministerial_responsibility__resignation_norm_decay_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ministerial_responsibility__resignation_norm_decay_reading, TR),
    TR >= 0.70.

:- end_tests(ministerial_responsibility__resignation_norm_decay_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate, declining. In 1954 (Crichel Down), the convention extracted a severe cost — immediate resignation was the price of departmental failure. By 2024, the price has been renegotiated sharply downward. Ministers apologize and remain; reshuffles replace resignations; 'lessons learned' substitutes for departure. The measurement trajectory (0.72 → 0.58) models this renegotiation of failure's cost. The residual extractiveness reflects that some reputational and political cost remains, but it is no longer sufficient to force resignation. Suppression (0.48): Moderate, declining. The convention's suppressive force — the internalized expectation that failure will lead to resignation — has weakened. MPs, ministers, and the public increasingly expect that resignations will not occur even after acknowledged failures. The measurement trajectory (0.68 → 0.48) models the erosion of the suppressive mechanism. The convention is no longer binding; agents behave as if it has ceased to be mandatory. Theater ratio (0.65): High, increasing. Parliamentary accountability rituals persist and are invoked (questions, apologies, confidence motions, committee inquiries), but their actual sanction has hollowed out. The rituals are maintained through institutional inertia and constitutional tradition, but the functional enforcement mechanism (resignation) is degraded. The trajectory (0.35 → 0.65) models the growing performative character of parliamentary accountability as the convention's teeth have been removed.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a major perspectival gap between beneficiaries and victims. Party leadership and embattled ministers perceive a coordination benefit: the convention's decay enables party stability and ministerial retention, turning accountability from a career-ending threat into a manageable reputational cost. They experience the constraint as rope (coordination with asymmetric benefit distribution). Opposition and parliamentary critics perceive tangled rope: the convention enables some accountability mechanism (parliamentary questioning, committee scrutiny) but loses enforceability as the convention decays. They are partially organized and have some leverage (committee positions, floor access) but face constraints (party discipline, whip asymmetries). The convention's deterrent force perceives snare: it is voiceless, unorganized, and bears the full cost of the norm's decay as the sanction disappears entirely. The analytical observer risks seeing mountain (the decay as inevitable feature of modern democracy) but the structural data reveals a tangled rope with identified beneficiaries — the decay is contingent, not natural.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from structural position. Party leadership and ministers, as beneficiaries with arbitrage options (they can move through reshuffles, rebuild reputations in new portfolios, or exit government entirely), experience low directional pressure — the convention's decay runs extraction away from them. Opposition critics, as organized but constrained agents with moderate power facing an institutional actor, experience higher d — they cannot fully exit parliamentary politics but face barriers in enforcing accountability. The convention's deterrent force, as a powerless and trapped actor with no exit, experiences maximal d — full-target status. The civil service, positioned between ministers and parliamentary accountability, experiences tension: it benefits from stability (low d via beneficiary mechanism) but risks accountability loss if ministers are no longer held accountable for delegation (higher d as victim of accountability gap). The measurement trajectory models the downward renegotiation of d for ministers and party leadership as the convention's enforcement weakens.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves mandatrophy by showing that the resignation norm decay is genuinely a renegotiation rather than a loss of coordination function. The tangled rope classification (rather than snare) reflects that some coordination persists: the party retains mechanisms for managing ministerial failure (apologies, reshuffles, committee scrutiny). What has changed is not the absence of accountability but the calibration of its cost and the distribution of leverage. The beneficiary (party leadership) experiences the constraint as increasingly favorable (extraction declining, coordination benefit steady). The victim (convention's deterrent force) experiences it as increasingly severe (sanction declining, deterrence capacity hollowing out). The analytical observer risks misclassifying this as a natural decline in parliamentary power, missing the contingent institutional renegotiation visible from the victim perspective. The mandatrophy is resolved by anchoring on the structural data (beneficiaries identified, suppression declining, extractiveness declining) rather than on the ceremonial continuity (parliamentary ritual persisting despite functional attenuation).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    norm_decay_vs_norm_renegotiation,
    'Is the resignation convention decaying (loss of internalized expectation) or being renegotiated (redefined to cover policy failures but not operational/delegated failures)?',
    'Textual analysis of ministerial apologies and retention decisions post-2000: are ministers explicitly claiming a narrower definition of responsibility, or is the convention simply ceasing to be invoked? Interview data from MPs and civil service about expectation thresholds.',
    'If decay: convention is losing force across all failure types (high theater ratio, low suppression). If renegotiation: convention is being redrawn — agency accountability reading (downstream constraint) becomes the new boundary, and suppression shifts to new failure types (policy-only failures still carry resignation threat).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(norm_decay_vs_norm_renegotiation, empirical, 'Whether the resignation convention is decaying or being redefined').

omega_variable(
    internalized_vs_externalized_sanction,
    'Is the weakening suppression due to ministers no longer internalizing the convention as a binding expectation, or due to external enforcement mechanisms (party whip, press, Opposition) atrophying?',
    'Comparative analysis of ministerial rhetoric: do ministers post-Crichel Down claim the convention is no longer operative (external loss), or do they apologize while remaining, suggesting they''ve redefined what the convention demands (internal reframing)? Historical contrast with pre-1954 ministerial statements about responsibility.',
    'If internalized loss: suppression is genuinely weakening (ministers believe they can survive failure). If externalized loss: suppression is steady but ineffective (ministers fear resignation but party/press no longer enforce it). Different mechanisms suggest different omega readings and different terminal states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_externalized_sanction, conceptual, 'Whether suppression loss is due to internalization shift or external enforcement decay').

omega_variable(
    select_committee_substitution_completeness,
    'Do select committees actually provide comparable accountability to the resignation convention, or do they substitute performative scrutiny for actual sanction?',
    'Measurement of select committee outcomes: proportion of inquiries leading to policy change, ministerial removal, or funding decisions vs. inquiries producing reports archived without implementation. Comparison with resignation convention''s historical sanction rate.',
    'If substitution is complete: scaffold perspective is accurate, and the resignation convention''s decay represents a deliberate institutional reallocation rather than a loss. If incomplete: the convention''s decay leaves a genuine accountability gap (agency accountability reading becomes active). Classification implications differ significantly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(select_committee_substitution_completeness, empirical, 'Whether select committees provide functional accountability equivalent to resignation convention').

omega_variable(
    party_discipline_vs_convention_decay,
    'Is the resignation convention decaying, or is declining party discipline making it harder to enforce an otherwise stable convention?',
    'Time-series analysis of party whip effectiveness, backbench rebellion rates, and cross-party coalition patterns pre- and post-1997. Separate the signal of convention decay from the noise of declining whip capacity. Interview MPs about whether they believe a minister *should* resign for departmental failure, vs. whether they believe the party *will demand* it.',
    'If convention decay: suppression is genuinely weakening (agents believe the rule has changed). If party discipline decline: suppression is stable but unenforced (agents believe the rule holds but fear non-compliance). Extractiveness interpretation differs — the first suggests beneficiary renegotiation, the second suggests coordination failure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(party_discipline_vs_convention_decay, empirical, 'Decay of the resignation convention vs. decline in party discipline capacity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ministerial_responsibility__resignation_norm_decay_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(minres_tr_t0, ministerial_responsibility__resignation_norm_decay_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(minres_tr_t15, ministerial_responsibility__resignation_norm_decay_reading, theater_ratio, 15, 0.48).
narrative_ontology:measurement(minres_tr_t30, ministerial_responsibility__resignation_norm_decay_reading, theater_ratio, 30, 0.65).

% Extraction over time
narrative_ontology:measurement(minres_be_t0, ministerial_responsibility__resignation_norm_decay_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(minres_be_t15, ministerial_responsibility__resignation_norm_decay_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(minres_be_t30, ministerial_responsibility__resignation_norm_decay_reading, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(minres_su_t0, ministerial_responsibility__resignation_norm_decay_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(minres_su_t15, ministerial_responsibility__resignation_norm_decay_reading, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(minres_su_t30, ministerial_responsibility__resignation_norm_decay_reading, suppression_requirement, 30, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ministerial_responsibility__resignation_norm_decay_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ministerial_responsibility__resignation_norm_decay_reading, ministerial_responsibility__agency_accountability_gap_reading).
narrative_ontology:affects_constraint(ministerial_responsibility__resignation_norm_decay_reading, ministerial_responsibility__select_committee_accountability_reading).

% DUAL FORMULATION NOTE:
% The ministerial responsibility kernel decomposes into three structurally distinct readings of how parliamentary accountability has evolved since Crichel Down. This story traces the decay of the resignation convention itself (norm erosion as renegotiation). The agency accountability gap reading traces the fragmentation of responsibility through delegation. The select committee reading traces the institutional relocation of accountability mechanisms. All three coexist as live readings of the kernel; none forecloses the others. They are linked because the constitutional question 'when does a minister fall?' is answered differently by each reading, and the answers create mutual structural pressure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ministerial_responsibility__resignation_norm_decay_reading, powerful, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
