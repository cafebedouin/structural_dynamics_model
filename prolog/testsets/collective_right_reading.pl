% ============================================================================
% CONSTRAINT STORY: collective_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_collective_right_reading, []).

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
 *   constraint_id: collective_right_reading
 *   human_readable: Second Amendment as Collective Right (Militia Authority Reading)
 *   domain: constitutional_law/rights_jurisprudence
 *
 * SUMMARY:
 *   The collective-right reading of the Second Amendment interprets 'the
 *   right to keep and bear arms' as a power granted to states to maintain
 *   militias, not as a right of individual citizens to own firearms
 *   independent of militia service. This reading became dominant
 *   constitutional doctrine after United States v. Miller (1939) and remained
 *   the judicial consensus until District of Columbia v. Heller (2008), which
 *   reversed it. This JSON instantiates the collective-right reading as ONE
 *   of three structurally distinct constraints that share a contested kernel
 *   (the Second Amendment's scope and meaning). The other readings
 *   (individual-right reading, civic-right reading) are separate constraint
 *   stories with different ε values, different beneficiary/victim
 *   declarations, and different measurement trajectories. This story models
 *   the collective-right reading's structure, beneficiaries, victims, and
 *   suppression mechanisms as they functioned from Miller through Heller.
 *
 * KEY AGENTS:
 *   - State Governments: Primary beneficiary (institutional/arbitrage) — collective-right reading grants comprehensive regulatory authority over civilian gun ownership without constitutional impediment
 *   - Militia Apparatus (National Guard): Specialized beneficiary (institutional/arbitrage) — reading grounds militia authority directly in Amendment text
 *   - Individual Gun Owners: Primary victim (powerless/trapped) — under this reading, Second Amendment provides zero protection for private ownership; regulatory authority is unlimited
 *   - Gun Rights Advocacy Movement: Secondary victim (moderate/constrained) — organization responds to doctrinal vulnerability; benefits from clarity of boundary but bears extraction of claimed constitutional right
 *   - Gun Control Coalition: Organized beneficiary (organized/constrained) — advances public safety regulation; benefits from doctrinal clarity but must actively suppress counter-interpretation
 *   - Historical Scholarly Consensus: Institutional mechanism (institutional/arbitrage) — Miller-based scholarship maintained collective-right dominance through interpretive work; theater ratio increased as work became visibly strained
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(collective_right_reading, 0.32).
domain_priors:suppression_score(collective_right_reading, 0.48).
domain_priors:theater_ratio(collective_right_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(collective_right_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(collective_right_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(collective_right_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(collective_right_reading, tangled_rope).
narrative_ontology:human_readable(collective_right_reading, "Second Amendment as Collective Right (Militia Authority Reading)").
narrative_ontology:topic_domain(collective_right_reading, "constitutional_law/rights_jurisprudence").

domain_priors:requires_active_enforcement(collective_right_reading).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(collective_right_reading, state_governments).
narrative_ontology:constraint_beneficiary(collective_right_reading, militia_apparatus).
narrative_ontology:constraint_victim(collective_right_reading, individual_gun_ownership_claims).
narrative_ontology:constraint_victim(collective_right_reading, private_self_defense_rights).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL GUN OWNER (SNARE) — Under the collective-right reading, the Second Amendment offers no protection for private ownership. The individual is trapped: the constraint denies a claimed constitutional right and offers no exit mechanism. All extraction runs toward state regulatory authority; no coordination benefit exists for this agent. Maximum experienced coercion.
constraint_indexing:constraint_classification(collective_right_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: GUN RIGHTS ADVOCACY MOVEMENT (TANGLED ROPE) — Constrained by doctrinal vulnerability of the collective-right reading but benefits from the clarity it provides (a sharp legal boundary enables organized response and counter-interpretation). Significant extraction (loss of claimed right) but also real coordination function (galvanizes movement identity and coalition). Neither pure extraction nor pure coordination — hybrid.
constraint_indexing:constraint_classification(collective_right_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE GOVERNMENT (ROPE) — Primary beneficiary. The collective-right reading grants state authority to comprehensively regulate civilian gun ownership without constitutional impediment. States experience the constraint as coordination: managing public safety through regulatory control of weapons. Net beneficiary — experiences the constraint as legitimate governance mechanism, not extraction.
constraint_indexing:constraint_classification(collective_right_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MILITIA APPARATUS (ROPE) — Specialized beneficiary. The reading grounds militia authority directly in the Amendment's text. National Guard and state militia systems experience the constraint as their constitutional foundation. Pure coordination from this position — the constraint legitimizes their role.
constraint_indexing:constraint_classification(collective_right_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: GUN CONTROL ADVOCACY COALITION (TANGLED ROPE) — Organized agents with genuine coordination function (advancing public safety regulation) but also extraction mechanism (suppressing counter-interpretation that would constrain their policy space). The coalition benefits from the doctrinal clarity the collective-right reading provides, but must actively enforce suppression of alternative readings to maintain it. This is active mandate work, not automatic.
constraint_indexing:constraint_classification(collective_right_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: HISTORICAL SCHOLARLY CONSENSUS (PITON) — The collective-right reading rested primarily on United States v. Miller (1939), which cited a narrow militia-focused reading. That consensus was dominant for ~70 years (1939-2008) but has substantially degraded through Heller and McDonald. The scholarship that upheld Miller's framing now appears largely performative — the interpretive work required to avoid the individual-right reading became increasingly visible as strained. Theater ratio high because the doctrinal edifice required increasingly elaborate interpretive moves to sustain Miller against mounting textual and historical evidence.
constraint_indexing:constraint_classification(collective_right_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational/textual perspective, the Second Amendment's grammar (subject-verb structure, prefatory clause, operative clause) might appear to constrain interpretation to one coherent reading. This perspective risks naturalizing a particular interpretive method as an immutable constraint on meaning. However, the structural data reveals this as a false summit: the 'textual constraint' naturalizes a contested interpretive choice (how to weight prefatory vs operative clauses, how to construe 'the people,' what militia-reference contributes to scope).
constraint_indexing:constraint_classification(collective_right_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(collective_right_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(collective_right_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(collective_right_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(collective_right_reading, TR),
    TR >= 0.70.

:- end_tests(collective_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32): Moderate. The collective-right reading extracts individual ownership claims and privileges state regulatory authority. However, the extraction is not severe (ε > 0.46) because: (1) the state's regulatory interest is genuine — public safety through firearms control is a legitimate governance function, not pure rent-seeking; (2) the reading, while contested, has serious historical and textual support grounded in the militia clause; (3) the suppression mechanisms are legal/doctrinal rather than material (no one is physically prevented from arguing the individual-right reading, just legally excluded from courts that follow Miller). Suppression (0.48): Moderate-high. The constraint suppresses alternative readings through judicial precedent and doctrinal authority, but suppression is not absolute — the individual-right reading persists in scholarship and eventually prevails in Heller. Theater ratio (0.65): High. The Miller-based scholarship increasingly required interpretive moves that strained credibility. By the 1990s, the collective-right reading required heavy lifting to avoid the grammatical structure of the operative clause ('the right of the people to keep and bear arms'); scholarly defenders of Miller had to argue that 'the people' did not mean what it appears to mean, that the operative clause was not operative, or that textual meaning was subordinated to policy preference. The theater increased over time as the interpretive apparatus became more visibly artificial.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary perspectives (state government, militia, gun control coalition) see coordination and legitimate governance authority. The victim perspectives (gun owners, gun rights movement) see extraction and suppression of claimed rights. The historical perspective sees increasing performative work required to defend the reading against textual and historical evidence. The analytical perspective risks naturalizing the reading as textually mandated when it is actually one of several defensible interpretations. The perspectival gap is maximal: what state actors experience as legitimate regulatory authority, individual actors experience as constitutional denial. The gap is not due to information asymmetry but to structural position — the constraint systematically privileges state-level organization over individual-level claim-making.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary set (states, militia apparatus) receives regulatory authority and unified sovereign power over firearms; their exit option is arbitrage (they can maintain regulatory authority regardless of how the Amendment is read, as long as explicit statutory authority substitutes for constitutional authority). This yields low d for both beneficiaries — they experience the constraint as coordination rather than extraction. The victim set (individual gun owners, gun rights advocates) loses a claimed constitutional protection; their exit option is trapped or constrained (they cannot exit the regulatory jurisdiction or exit the constitutional interpretation through individual choice). This yields high d for both victims — they experience maximum or near-maximum extraction. The gun control coalition sits between: beneficiary status (advances its policy goals) with constrained exit (must actively suppress counter-interpretation; if counter-interpretation prevails, coalition loses doctrinal support). This produces moderate d, moderate experienced extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids mandatrophy through honest acknowledgment of its interpretive costs. The collective-right reading CAN claim genuine coordination function (state authority to maintain militias is a real public safety function) AND acknowledge extraction (individual ownership claims are suppressed). The error would be claiming ONLY coordination (hiding the extraction, pretending individual gun owners are not victims) or claiming ONLY extraction (denying that state regulatory authority has any legitimacy). The Tangled Rope classification holds both in tension: real coordination function + real asymmetric extraction + real suppression mechanism. The piton perspective (historical scholarship) captures the degradation: Miller-based scholarship had to do increasingly elaborate interpretive work to sustain the collective-right reading against mounting textual and historical evidence, making the constraint look performative even to those defending it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prefatory_operative_clause_weight,
    'Does the prefatory militia clause limit the scope of ''the right to keep and bear arms'' (operative reading), or merely provide context/motivation without limiting scope (independent-clause reading)?',
    'Comparative constitutional law analysis: examine parallel structures in Framers'' era documents; analyze modern constitutional drafting conventions for prefatory/operative relationships; examine Supreme Court precedent on prefatory clause interpretation across different amendments',
    'If prefatory limits scope: collective-right reading strengthens (mountain-adjacent). If prefatory provides context only: individual-right reading strengthens (expands victim set, lowers state beneficiary advantage). This is the hinge on which the entire interpretive contest swings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(prefatory_operative_clause_weight, conceptual, 'Semantic weight of prefatory militia clause vs operative clause').

omega_variable(
    historical_militia_scope,
    'In 1791, did ''militia'' refer narrowly to formal state-organized militias, or to the broader armed citizenry understood as the ''militia of the whole'' (historical sources suggest both meanings coexisted)?',
    'Historical textual analysis of Framers'' writings, colonial militia practices, state constitutions, and post-ratification commentary; reconstruction of 1791 semantic field for ''militia''',
    'If narrow state-organized meaning: collective-right reading is historically grounded. If broad-citizenry meaning: individual-right reading is historically grounded. Both readings can cite plausible historical evidence — the semantic instability IS the omega.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_militia_scope, empirical, 'Historical semantic scope of ''militia'' in 1791').

omega_variable(
    this_reading_vs_siblings,
    'This JSON instantiates the collective-right reading of the Second Amendment kernel. The sibling readings (individual-right and civic-right) would assign different beneficiary/victim sets, different ε values, and different suppression mechanisms. Which reading is this constraint?',
    'This is the COLLECTIVE-RIGHT reading: states/militia as primary beneficiary, individual ownership claims as victims, ε=0.32 (moderate extraction, balanced by genuine state regulatory coordination). The sibling readings have different structural properties and inhabit different JSON files. This omega documents the kernel''s contested nature and routes the reading choice to irreducible ambiguity rather than pre-adjudicating it.',
    'This constraint represents one defensible instantiation of the Second Amendment''s scope and force. The alternative readings (individual-right, civic-right) would classify differently, with different victims, different beneficiaries, different ε. The corpus will contain all three constraint stories linked via network.affects_constraints. An analyst reviewing the full family sees the interpretive contest as structural rather than as a matter of legal opinion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(this_reading_vs_siblings, conceptual, 'This constraint as one reading of a contested kernel (collective vs individual vs civic)').

omega_variable(
    beneficiary_authenticity,
    'Does the collective-right reading genuinely ground state regulatory authority in the Amendment, or does it instrumentalize the Amendment to justify extraction the state would pursue anyway (regulatory capture of a kernel)?',
    'Counterfactual analysis: if the individual-right reading became dominant, would state regulatory authority collapse or merely require explicit statutory rather than constitutional basis? Comparative federalism: do other constitutional democracies achieve comparable public safety regulation through different textual anchors?',
    'If the collective-right reading is authentic grounding: the state is a genuine beneficiary whose interests are legitimately represented. If the reading is post-hoc justification: the constraint is snare-like even from the institutional beneficiary perspective (using doctrinal cover for extraction). This affects whether the state perspective should classify as rope vs tangled_rope at the generational horizon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_authenticity, preference, 'Whether collective-right reading authentically grounds state authority or instrumentalizes the Amendment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(collective_right_reading, 1939, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(collective_reading_theater_1939, collective_right_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(collective_reading_theater_1974, collective_right_reading, theater_ratio, 35, 0.62).
narrative_ontology:measurement(collective_reading_theater_2008, collective_right_reading, theater_ratio, 69, 0.65).

% Extraction over time
narrative_ontology:measurement(collective_reading_extract_1939, collective_right_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(collective_reading_extract_1974, collective_right_reading, base_extractiveness, 35, 0.3).
narrative_ontology:measurement(collective_reading_extract_2008, collective_right_reading, base_extractiveness, 69, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(collective_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(collective_right_reading, individual_right_reading).
narrative_ontology:affects_constraint(collective_right_reading, civic_right_reading).

% DUAL FORMULATION NOTE:
% The Second Amendment's scope is contested across three structurally distinct constraint readings: (1) collective-right (this file): ε=0.32, state/militia as beneficiary, individuals as victims, moderate theater; (2) individual-right reading (separate file): ε likely 0.10-0.20, individuals as beneficiary, state regulatory capacity as victim, lower theater; (3) civic-right reading (separate file): ε likely 0.25-0.35, dual beneficiary structure, hybrid coordination-extraction, medium theater. All three readings share the same kernel (Amendment text) but disagree on its scope. The ε-invariance principle applies: if changing the reading changes ε, you have different constraints. The network links them as a constraint family undergoing interpretive contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(collective_right_reading, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
