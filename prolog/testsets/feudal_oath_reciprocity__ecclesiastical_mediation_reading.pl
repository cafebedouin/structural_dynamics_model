% ============================================================================
% CONSTRAINT STORY: feudal_oath_reciprocity__ecclesiastical_mediation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feudal_oath_reciprocity__ecclesiastical_mediation_reading, []).

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
 *   constraint_id: feudal_oath_reciprocity__ecclesiastical_mediation_reading
 *   human_readable: Feudal Oath Reciprocity Under Ecclesiastical Mediation
 *   domain: medieval_political_economy/institutional_analysis/religious_authority
 *
 * SUMMARY:
 *   Medieval feudal oaths—sworn by vassal to lord—create binding personal
 *   obligations that are mediated and enforced by the church through
 *   sacramental language and threat of spiritual sanction. This
 *   ecclesiastical mediation reading emphasizes how Christian theology
 *   (charity, divine justice, sacramental efficacy) constrains the lord's
 *   ability to extract arbitrarily from the vassal. The oath is not merely a
 *   contract between two parties but a sacred commitment witnessed by God and
 *   the church. The church gains institutional authority as interpreter of
 *   what the oath requires and enforcer of its limits through confession,
 *   absolution, and excommunication. The constraint operates at the
 *   intersection of feudal reciprocity and ecclesiastical authority: lords
 *   and vassals are bound by mutual obligation sanctified by the church, but
 *   the church's mediation role itself limits how much secular extraction the
 *   constraint permits. Over time (measured interval 1–3), the theater ratio
 *   increases as oath-swearing becomes more elaborate and ritualistic, while
 *   actual suppression strength slightly decreases as secular alternatives
 *   emerge and ecclesiastical monopoly weakens. The ecclesiastical mediation
 *   reading produces a moderate ε (0.42) Tangled Rope: genuine coordination
 *   function (stable vassalage through mutual obligation) coupled with
 *   asymmetric extraction (church gains authority, secular lords constrained
 *   by theological limits). The key agents are the vassal (trapped by
 *   sacramental obligation), the lord class (organized but constrained by
 *   reciprocal oaths), the church hierarchy (institutional beneficiary from
 *   mediation monopoly), and the analytical observer at risk of naturalizing
 *   a contingent institutional arrangement as an immutable law.
 *
 * KEY AGENTS:
 *   - Oath-Bound Vassal: Primary victim (powerless/trapped) — bound by sacramental obligation with no secular exit option; bears suppression through spiritual fear of damnation and social excommunication
 *   - Lord Class (Collective Institution): Secondary victim and beneficiary (organized/constrained) — benefits from oath-binding of subordinates (coordination function) but constrained by reciprocal obligations and ecclesiastical limits on extraction
 *   - Church Hierarchy: Primary beneficiary (institutional/arbitrage) — gains interpretive monopoly over oath obligations; all oath disputes flow to clerical courts; all oath-breaking requires priestly mediation
 *   - Ecclesiastical Councils and Penitential System: Enforcement mechanism (institutional) — translates oath obligations into sacramental duties; enforces through confession, absolution conditions, and excommunication threat
 *   - Reformation-Era Centralizing States: Exit pathway (powerful/mobile) — create secular oath-binding mechanisms and written contracts that displace ecclesiastical mediation
 *   - Analytical Observer: Risk position (analytical/analytical) — tempted to naturalize ecclesiastical mediation as inherent to feudal stability rather than as contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.42).
domain_priors:suppression_score(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.48).
domain_priors:theater_ratio(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, tangled_rope).
narrative_ontology:human_readable(feudal_oath_reciprocity__ecclesiastical_mediation_reading, "Feudal Oath Reciprocity Under Ecclesiastical Mediation").
narrative_ontology:topic_domain(feudal_oath_reciprocity__ecclesiastical_mediation_reading, "medieval_political_economy/institutional_analysis/religious_authority").

domain_priors:requires_active_enforcement(feudal_oath_reciprocity__ecclesiastical_mediation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__ecclesiastical_mediation_reading, '975022a6-637a-4a58-b748-bbc05b2aff14').
narrative_ontology:cs_kernel_codification('975022a6-637a-4a58-b748-bbc05b2aff14', fixed_text).
narrative_ontology:cs_authority_grounding('975022a6-637a-4a58-b748-bbc05b2aff14', lineage).
narrative_ontology:cs_interpretation_layer_present('975022a6-637a-4a58-b748-bbc05b2aff14').
narrative_ontology:cs_reading_relation('975022a6-637a-4a58-b748-bbc05b2aff14', feudal_oath_reciprocity__lord_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('975022a6-637a-4a58-b748-bbc05b2aff14', feudal_oath_reciprocity__vassal_coordination_reading, coexists_with).
narrative_ontology:cs_axiom('975022a6-637a-4a58-b748-bbc05b2aff14', foundational, ecclesiastical_mediation_stabilizes_feudal_reciprocity).
narrative_ontology:cs_axiom_status(ecclesiastical_mediation_stabilizes_feudal_reciprocity, holdable).
narrative_ontology:cs_axiom_grounding('975022a6-637a-4a58-b748-bbc05b2aff14', ecclesiastical_mediation_stabilizes_feudal_reciprocity, theological).
narrative_ontology:cs_axiom('975022a6-637a-4a58-b748-bbc05b2aff14', foundational, sacramental_oath_limits_secular_extraction).
narrative_ontology:cs_axiom_status(sacramental_oath_limits_secular_extraction, holdable).
narrative_ontology:cs_axiom_grounding('975022a6-637a-4a58-b748-bbc05b2aff14', sacramental_oath_limits_secular_extraction, theological).
narrative_ontology:cs_reference_frame('975022a6-637a-4a58-b748-bbc05b2aff14', sacramental_oath_reciprocity_framework).
narrative_ontology:cs_drift_state('975022a6-637a-4a58-b748-bbc05b2aff14', late_medieval_period, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('975022a6-637a-4a58-b748-bbc05b2aff14', '').
narrative_ontology:cs_kernel_id(feudal_oath_reciprocity__ecclesiastical_mediation_reading, feudal_oath_reciprocity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__ecclesiastical_mediation_reading, church_hierarchy).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__ecclesiastical_mediation_reading, oath_enforcement_mechanism).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, secular_extraction_capacity).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, lord_unilateral_authority).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VASSAL (SNARE) — Bound by both feudal obligation and sacramental oath sworn before clergy. The ecclesiastical reading transforms the oath from a secular contract into a spiritual lock: breaking faith violates not just feudal law but divine law, damnation, and excommunication. Exit is structurally impossible — the vassal cannot dissolve the oath without losing salvation and social standing. Suppression is maximal through spiritual coercion. From this position, the constraint appears as pure extraction dressed in theological legitimacy.
constraint_indexing:constraint_classification(feudal_oath_reciprocity__ecclesiastical_mediation_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: LORDS COLLECTIVE (TANGLED ROPE) — As an institutional class, lords benefit from the ecclesiastical mediation: sacramental oaths bind their own subordinates and constrain rival lords' extraction (reciprocal obligations limit naked usurpation). But lords are also constrained by the same ecclesiastical framework — they cannot unilaterally revoke oaths or extract beyond what canon law permits. They coordinate vassal loyalty (genuine coordination good) while being limited in their ability to extract arbitrarily. This is the genuine tangled rope: the constraint both enables feudal stability (beneficiary function) and prevents unlimited domination (victim function).
constraint_indexing:constraint_classification(feudal_oath_reciprocity__ecclesiastical_mediation_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CHURCH HIERARCHY (ROPE) — Gains interpretive authority over the definition and enforcement of oaths. The ecclesiastical reading embeds church authority into every feudal contract. The church benefits from this mediation role: all oaths require priestly witness or blessing, all oath-breaking potentially requires priestly absolution, and theological disputes about oath obligations flow to clerical courts. From this perspective, the constraint is pure coordination — it solves the feudal stability problem AND simultaneously extends church institutional reach. The church experiences this as beneficial coordination with no extraction cost.
constraint_indexing:constraint_classification(feudal_oath_reciprocity__ecclesiastical_mediation_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LOCAL ENFORCEMENT ACTORS (PITON) — The ecclesiastical oath validation becomes increasingly ritualistic over time. By the late medieval period, oath-swearing is theatrically elaborate but functionally degraded: the same sacramental oaths are sworn, re-sworn, and violated with mounting frequency. The theater (ceremonial, ecclesiastical witness, theological language) persists through institutional inertia while the binding mechanism itself weakens. From this perspective, the constraint is a vestigial performance maintained because alternatives haven't fully replaced it — the piton classification.
constraint_indexing:constraint_classification(feudal_oath_reciprocity__ecclesiastical_mediation_reading, piton,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: REFORMATION-ERA REFORMERS AND CENTRALIZING MONARCHS (SCAFFOLD) — See the ecclesiastical oath mediation as a temporary coordination device being displaced by state-administered oaths and written contracts. As secular authority consolidates (12th-14th centuries), the need for ecclesiastical mediation of oath-binding declines — sovereign states can enforce contracts directly. The ecclesiastical constraint becomes a sunset mechanism: as state capacity grows and written law replaces sacramental obligation, the church loses interpretive monopoly. From this perspective, the constraint is temporary, with a real sunset horizon as secular institutions mature.
constraint_indexing:constraint_classification(feudal_oath_reciprocity__ecclesiastical_mediation_reading, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE SUMMIT (MOUNTAIN) — Risks viewing the ecclesiastical constraint as a natural law of medieval political organization: that loyalty oaths require spiritual mediation, that faith-binding is the only stable mechanism for vassalage, that church authority is inherent to feudal stability. The engine detects this as a false summit: the constraint is analytically modeled with identifiable beneficiaries (church, oath enforcement mechanism) and empirical sources (council documents, penitential canons, oath formulas). The 'natural law' framing naturalizes what is actually a contingent institutional choice — the ecclesiastical READING of feudal reciprocity, not its inevitable structure.
constraint_indexing:constraint_classification(feudal_oath_reciprocity__ecclesiastical_mediation_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feudal_oath_reciprocity__ecclesiastical_mediation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(feudal_oath_reciprocity__ecclesiastical_mediation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(feudal_oath_reciprocity__ecclesiastical_mediation_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(feudal_oath_reciprocity__ecclesiastical_mediation_reading, TR),
    TR >= 0.70.

:- end_tests(feudal_oath_reciprocity__ecclesiastical_mediation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42): Moderate. The ecclesiastical reading produces moderate extraction because the constraint genuinely solves a coordination problem (stable vassalage through mutual oath-binding) while simultaneously benefiting the church as an extractive institution. Unlike the lord_extraction_reading (which emphasizes naked domination) or the vassal_coordination_reading (which emphasizes mutual restraint), this reading holds both in tension: the oath binds both parties reciprocally through theological constraint, but the church extracts authority and institutional benefit from mediation. The ε value reflects this hybrid character. Suppression (0.48): Moderate-to-high. The suppression is structural (vassals cannot exit without excommunication and social death) but slightly declining over the interval as secular alternatives emerge. The primary suppression mechanism is spiritual (damnation fear, sacramental efficacy) rather than purely material, which creates an ambiguity captured in the omega variables. Theater ratio (0.55): Moderate and rising. Early feudal oaths are more functional (binding mechanism actually constrains extraction). Over time, oath-swearing becomes increasingly ritualistic—the elaborate ecclesiastical ceremony persists while the binding mechanism weakens as state capacity grows. By the late medieval period (time point 3), oath theater is highest (0.55) reflecting vestigial ceremonialism maintained by institutional inertia. Claimed type (Tangled Rope): The constraint exhibits genuine coordination (mutual oath obligation ensures vassals show up, lords honor duties) AND asymmetric extraction (church monopolizes interpretation, lords extract from vassals despite reciprocal limits, ecclesiastical authority prevents exit). The beneficiary/victim structure is asymmetric: church and oath-enforcement mechanism benefit; secular extraction capacity of individual lords and vassal autonomy are constrained. Active enforcement is true: the constraint requires continuous ecclesiastical witnessing, confession-based monitoring, and excommunication threat to maintain binding force.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximal perspectival divergence across the six perspectives. The oath-bound vassal sees Snare (spiritual lock, no exit, maximum suppression). The lord class sees Tangled Rope (genuine coordination benefit coupled with reciprocal constraint). The church hierarchy sees Rope (pure coordination between church and feudal system, beneficial mediation role). The local enforcement officer sees Piton (elaborate theater masking degraded binding force). Reformers see Scaffold (temporary coordination mechanism being displaced by state oaths and written contracts). The analytical observer risks seeing Mountain (naturalizing ecclesiastical mediation as inherent to feudal stability). These are not measurement errors or differences of opinion — they reflect genuine structural differences in how each agent experiences the constraint. The beneficiary experiences it as expansion of authority; the victim experiences it as tightening of cage. The trapeze is real; the experience of the trapeze differs radically by position.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is derived from the agent's structural position relative to the constraint. The oath-bound vassal (powerless/trapped) has maximum d (0.95+) — the constraint targets them fully. The church (institutional/arbitrage) has minimum d (~0.05) — they are full beneficiary. The lord collective (organized/constrained) has moderate d (~0.50) — they both benefit (coordination) and bear costs (reciprocal limits). The reformer perspective (powerful/mobile) has d around 0.40 — they have exit options and see the constraint as temporary. The local enforcement officer (moderate/constrained) has d around 0.60 — they are caught between the institutional structure and its degrading function. These d values drive the chi formula: agents with high d (vassal) experience maximum suppression; agents with low d (church) experience negative extraction (subsidy); agents with moderate d experience mixed extraction. The perspectival gap IS the range of d values across positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The ecclesiastical mediation reading resolves mandatrophy by showing that the constraint is genuinely Tangled Rope at the systemic level: it solves feudal coordination (genuine benefit) through ecclesiastical mediation (extraction mechanism). The constraint is not pure extraction masquerading as coordination, nor pure coordination hiding exploitation — it is authentically both. The church genuinely solves a stability problem that lords needed solved (coordination function). The church genuinely extracts authority and institutional benefit from providing this solution (extraction function). The mandatrophy is resolved by acknowledging both: the ecclesiastical authority structure is simultaneously the mechanism of coordination AND the mechanism of extraction. This is the defining feature of Tangled Rope. The vassal-perspective Snare is that agent's genuine experience (maximal extraction from their position), but it is not the constraint's 'true type' — the constraint is Tangled Rope systemically, Snare perspectivally from the powerless position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_lock_vs_social_convention,
    'Is the vassal''s experience of the sacramental oath as spiritually unbreakable a genuine theological fact (damnation risk, sacramental efficacy) or a socially maintained convention that vassals internalize?',
    'Comparative analysis of oath-breaking incidence and outcomes across regions with different ecclesiastical authority strength. Confessional records and penitential practices. Whether oath violations correlate with documented damnation anxiety or with material incentive structures.',
    'If genuine theological mechanism: suppression rating (0.48) understates the binding force — the constraint operates at higher effective suppression through spiritual fear. If social convention: the suppression is internalized rather than structural, and reclassifiable as identity_locked exit option rather than trapped.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_lock_vs_social_convention, empirical, 'Whether sacramental lock is theological fact or internalized social convention').

omega_variable(
    church_extractive_motivation_ambiguity,
    'Does the church gain interpretive authority over feudal oaths because the ecclesiastical framing solves a genuine coordination problem (best available stability mechanism), or because the church extracts benefit from mediation monopoly?',
    'Historical analysis of church teaching on oath obligations before and after church gained mediation role. Comparison with non-ecclesiastical oath-binding mechanisms (Germanic folk-oath traditions, Islamic waqf contracts, Greek polis oaths). Whether church actively promoted ecclesiastical mediation or inherited it as pre-existing practice.',
    'If coordination-motivated: the ecclesiastical reading is justified (Rope from church perspective). If extraction-motivated: the beneficiary declaration is correct and the constraint is more accurately Snare from vassal perspective with church as primary extractor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(church_extractive_motivation_ambiguity, conceptual, 'Whether ecclesiastical mediation solves coordination or enables extraction').

omega_variable(
    alternative_oath_binding_mechanisms,
    'Were non-ecclesiastical oath-binding mechanisms available to medieval lords and deliberately rejected in favor of ecclesiastical mediation, or was sacramental binding the only stable mechanism available?',
    'Historical evidence of secular oath-binding alternatives (lay courts, guild arbitration, written contracts with material penalties). Whether these alternatives pre-dated or post-dated ecclesiastical mediation becoming dominant.',
    'If alternatives existed and were rejected: ecclesiastical mediation is a contingent choice, not a necessity, and the constraint is artifact of institutional path-dependency. If alternatives post-dated ecclesiastical dominance: the ecclesiastical mechanism was the best available at the time (weakens the extraction framing).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_oath_binding_mechanisms, empirical, 'Availability and rejection history of non-ecclesiastical oath mechanisms').

omega_variable(
    kernel_reading_rivalry_ambiguity,
    'To what degree do the three kernel readings (ecclesiastical_mediation, lord_extraction, vassal_coordination) represent genuinely different historical periods vs. different ideological framings of the same institutional period?',
    'Chronological mapping: ecclesiastical mediation dominates 11th-13th centuries, lord extraction narrative gains prominence in late medieval sources, vassal coordination framing emerges with Magna Carta and charter traditions. If readings correlate with distinct periods, they are empirical alternatives (different constraint at different times). If all three are present in the same period, they are framings of the same constraint.',
    'If temporal stages: the three readings should be three separate constraint stories (constraint families with time-stamped interval ranges). If simultaneous framings: they are genuinely coexisting readings of one constraint. This affects how the family is modeled: three stories with different intervals vs. three perspectives on one story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_rivalry_ambiguity, empirical, 'Whether kernel readings represent historical stages or simultaneous framings').

omega_variable(
    ecclesiastical_authority_binding_mechanism,
    'What makes ecclesiastical authority binding on oath-breaking? Is it divine power (sacramental efficacy), institutional sanction (excommunication consequences), or internalized identity (the vassal conceives themselves as spiritually obligated)?',
    'Close reading of oath formulas, penitential canons, and council documents on oath enforcement. Analysis of excommunication outcomes and vassal responses. Comparison with post-Reformation contexts where ecclesiastical authority weaker but oath-binding persisted (suggesting the mechanism is not purely ecclesial).',
    'If divine power: the constraint operates at the level of spiritual reality and cannot be dismantled by secular institutional change alone. If institutional sanction: secular power that grows can displace ecclesiastical authority. If identity internalization: the suppression mechanism travels with the vassal and persists even after institutional framework changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecclesiastical_authority_binding_mechanism, conceptual, 'Mechanism by which ecclesiastical authority binds oaths').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 1, 3).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feudal_eccl_tr_t1, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 1, 0.42).
narrative_ontology:measurement(feudal_eccl_tr_t2, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 2, 0.48).
narrative_ontology:measurement(feudal_eccl_tr_t3, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 3, 0.55).

% Extraction over time
narrative_ontology:measurement(feudal_eccl_be_t1, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 1, 0.38).
narrative_ontology:measurement(feudal_eccl_be_t2, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 2, 0.4).
narrative_ontology:measurement(feudal_eccl_be_t3, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 3, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(feudal_eccl_su_t1, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 1, 0.52).
narrative_ontology:measurement(feudal_eccl_su_t2, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 2, 0.5).
narrative_ontology:measurement(feudal_eccl_su_t3, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 3, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_oath_reciprocity__ecclesiastical_mediation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.12).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__ecclesiastical_mediation_reading, feudal_oath_reciprocity__lord_extraction_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__ecclesiastical_mediation_reading, feudal_oath_reciprocity__vassal_coordination_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__ecclesiastical_mediation_reading, church_institutional_authority_expansion).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__ecclesiastical_mediation_reading, secular_oath_binding_mechanisms).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__ecclesiastical_mediation_reading, penitential_canon_enforcement_system).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the feudal_oath_reciprocity kernel. The sibling readings (lord_extraction_reading and vassal_coordination_reading) are structurally distinct constraints with different ε values, different beneficiary/victim structures, and different perspectives. All three are present in medieval sources simultaneously, representing different interpretive frames rather than historical stages (though there is some temporal layering). The ecclesiastical_mediation_reading emphasizes the church's institutional role; the lord_extraction_reading emphasizes secular domination; the vassal_coordination_reading emphasizes mutual obligation. These should be authored as three separate constraint stories linked via network.affects_constraints, not as three perspectives on one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(feudal_oath_reciprocity__ecclesiastical_mediation_reading, organized, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
