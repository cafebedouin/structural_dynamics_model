% ============================================================================
% CONSTRAINT STORY: tribunician_sacrosanctity__demagogic_lever_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tribunician_sacrosanctity__demagogic_lever_reading, []).

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
 *   constraint_id: tribunician_sacrosanctity__demagogic_lever_reading
 *   human_readable: Tribunician Sacrosanctity as Demagogic Lever
 *   domain: legal/doctrinal
 *
 * SUMMARY:
 *   The Tribunician sacrosanctity reading as demagogic lever captures the
 *   historical moment when the Republic's protection mechanism—the inviolable
 *   office designed to shield the plebs from magistrate violence—was inverted
 *   into an instrument of obstruction. Clodius Pulcher and, later, Mark
 *   Antony wielded the sacrosanct office to veto senatorial decisions, block
 *   prosecutions, obstruct legislation, and impose factional will on public
 *   business. The constraint manifests as a structural asymmetry: a single
 *   actor, immune from all coercive enforcement, can hold the entire
 *   governance apparatus hostage to their private political agenda. The
 *   mechanism is extractive because the protection doctrine (designed as a
 *   symmetric shield) becomes an asymmetric weapon when claimed by actors
 *   with sufficient factional power to withstand retaliation. The
 *   extractiveness increases over the interval (0.35 → 0.58) as demagogues
 *   learn to weaponize sacrosanctity more systematically; suppression
 *   increases as the Senate recognizes it cannot enforce decisions against
 *   the protected tribune without constitutional collapse; theater ratio
 *   increases as the demagogue's claim to represent popular will becomes
 *   increasingly performative (the protection doctrine is invoked, but
 *   genuine popular consultation is absent).
 *
 * KEY AGENTS:
 *   - Demagogue Faction (Clodius, Antony): Primary beneficiary (institutional/arbitrage) — wields sacrosanctity to block decisions, protect allies, obstruct enemies, with full immunity from enforcement
 *   - Public Governance and Senatorial Authority: Primary victim (powerful/constrained) — structural ability to legislate and execute is blocked by a single protected actor; cannot govern without violating the doctrinal framework
 *   - Citizen Body and Public Business: Secondary victim (powerless/trapped) — services are not provisioned, disputes cannot be adjudicated, legislation cannot pass; the protection intended for them becomes the means of their suppression
 *   - Constitutional Reform Movement: Organized responders (organized/constrained) — perceive a temporary problem with a structural solution; building alternative doctrinal frameworks or sunset conditions for veto claims
 *   - Analytical Observer (Civilizational): Views the constraint as a structural inevitability—any system combining absolute immunity with veto power will attract ambitious actors
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tribunician_sacrosanctity__demagogic_lever_reading, 0.58).
domain_priors:suppression_score(tribunician_sacrosanctity__demagogic_lever_reading, 0.72).
domain_priors:theater_ratio(tribunician_sacrosanctity__demagogic_lever_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tribunician_sacrosanctity__demagogic_lever_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(tribunician_sacrosanctity__demagogic_lever_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(tribunician_sacrosanctity__demagogic_lever_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tribunician_sacrosanctity__demagogic_lever_reading, snare).
narrative_ontology:human_readable(tribunician_sacrosanctity__demagogic_lever_reading, "Tribunician Sacrosanctity as Demagogic Lever").
narrative_ontology:topic_domain(tribunician_sacrosanctity__demagogic_lever_reading, "legal/doctrinal").

domain_priors:requires_active_enforcement(tribunician_sacrosanctity__demagogic_lever_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tribunician_sacrosanctity__demagogic_lever_reading, 'b55a72ed-94c6-4949-b734-350fe4b23031').
narrative_ontology:cs_kernel_codification('b55a72ed-94c6-4949-b734-350fe4b23031', formalized).
narrative_ontology:cs_authority_grounding('b55a72ed-94c6-4949-b734-350fe4b23031', lineage).
narrative_ontology:cs_interpretation_layer_present('b55a72ed-94c6-4949-b734-350fe4b23031').
narrative_ontology:cs_reading_relation('b55a72ed-94c6-4949-b734-350fe4b23031', tribunician_sacrosanctity__popular_shield_reading, coexists_with).
narrative_ontology:cs_reading_relation('b55a72ed-94c6-4949-b734-350fe4b23031', tribunician_sacrosanctity__imperial_absorption_reading, influences).
narrative_ontology:cs_axiom('b55a72ed-94c6-4949-b734-350fe4b23031', foundational, sacrosanctity_enables_factional_extraction).
narrative_ontology:cs_axiom_status(sacrosanctity_enables_factional_extraction, holdable).
narrative_ontology:cs_axiom_grounding('b55a72ed-94c6-4949-b734-350fe4b23031', sacrosanctity_enables_factional_extraction, empirically_contingent).
narrative_ontology:cs_axiom('b55a72ed-94c6-4949-b734-350fe4b23031', secondary, veto_power_without_criteria_invites_obstruction).
narrative_ontology:cs_axiom_status(veto_power_without_criteria_invites_obstruction, holdable).
narrative_ontology:cs_axiom_grounding('b55a72ed-94c6-4949-b734-350fe4b23031', veto_power_without_criteria_invites_obstruction, empirically_contingent).
narrative_ontology:cs_reference_frame('b55a72ed-94c6-4949-b734-350fe4b23031', magisterial_authority_framework).
narrative_ontology:cs_drift_state('b55a72ed-94c6-4949-b734-350fe4b23031', clodius_antony_dominance, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b55a72ed-94c6-4949-b734-350fe4b23031', '').
narrative_ontology:cs_kernel_id(tribunician_sacrosanctity__demagogic_lever_reading, tribunician_sacrosanctity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tribunician_sacrosanctity__demagogic_lever_reading, demagogue_faction).
narrative_ontology:constraint_victim(tribunician_sacrosanctity__demagogic_lever_reading, public_governance).
narrative_ontology:constraint_victim(tribunician_sacrosanctity__demagogic_lever_reading, senatorial_authority).
narrative_ontology:constraint_victim(tribunician_sacrosanctity__demagogic_lever_reading, citizen_body).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The citizen body and legitimate governance process are structurally trapped. A single protected actor (Clodius, Antony) can veto any public business indefinitely. The people's own instrument of protection—the sacrosanct tribune—has been inverted into a weapon against the commonwealth. No exit from this constraint without dissolving the office itself or the protection doctrine.
constraint_indexing:constraint_classification(tribunician_sacrosanctity__demagogic_lever_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Senatorial authority cannot govern or enforce law against a sacrosanct tribune allied with a demagogue. The Senate faces escalating constraint: legitimate legislative and executive action is blocked by a functionary who claims immunity. The Senate could technically override by stripping the office, but doing so requires formal process—itself subject to veto. High extraction cost; exit only through violence or constitutional collapse.
constraint_indexing:constraint_classification(tribunician_sacrosanctity__demagogic_lever_reading, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% The demagogue and allied faction experience this as pure coordination: the sacrosanct office coordinates their political power without need for explicit coercion. They can obstruct without accountability. The protection doctrine, designed to shield the weak, is refitted to shield their interests. Net beneficiary with maximum flexibility—arbitrage option (can exit by resigning office, or simply cease obstruction).
constraint_indexing:constraint_classification(tribunician_sacrosanctity__demagogic_lever_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% Organized actors (reformers, alternative institutional factions) perceive a temporary structural problem: the protection doctrine is being weaponized. They see a clear mechanism for transition—formal restriction of tribunician veto, legislative oversight of sacrosanctity claims, or sunset of the protection when used for obstruction. The scaffold sits at the generational horizon because constitutional reform takes decades. This reading assumes a genuine reform pathway exists and is being actively constructed.
constraint_indexing:constraint_classification(tribunician_sacrosanctity__demagogic_lever_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Moderate actors (provincial magistrates, popular assemblies, client networks) experience mixed coordination and extraction. The demagogue's faction coordinating with sacrosanctity offers some protection against arbitrary senatorial power AND blocks public business that might benefit their interests. High constraint cost (services cannot be provisioned, disputes cannot be adjudicated) but some genuine coordination benefit from the protection mechanism itself. Trapped in a system that partially works for them and partially against them.
constraint_indexing:constraint_classification(tribunician_sacrosanctity__demagogic_lever_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% From civilizational distance, this reads as a structural inevitability: any system that grants absolute immunity to block governance will eventually be weaponized by actors with ambition and popular support. The mechanism is immutable—the logic flows directly from 'untouchable' plus 'veto power' plus 'ambitious faction.' This perspective risks naturalizing what is actually a contingent doctrinal choice. The engine will flag this as a false summit.
constraint_indexing:constraint_classification(tribunician_sacrosanctity__demagogic_lever_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tribunician_sacrosanctity__demagogic_lever_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(tribunician_sacrosanctity__demagogic_lever_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(tribunician_sacrosanctity__demagogic_lever_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(tribunician_sacrosanctity__demagogic_lever_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tribunician_sacrosanctity__demagogic_lever_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The demagogue extracts significant value: immunity from prosecution, ability to block decisions, factional protection, leverage over other actors. This is not maximal extraction (0.66+) because the demagogue still depends on popular support and cannot impose affirmative governance—they can only block. The extraction is primarily negative (preventing what others would do) rather than positive (imposing what they want). Suppression (0.72): High. The constraint operates by suppressing the Senate's ability to govern and the People's ability to obtain services. However, suppression is not absolute (0.95+) because the demagogue cannot force affirmative action—they can only paralyze. Theater ratio (0.55): Moderate. The demagogue's claim to represent popular will and the doctrinal framework of protection contain genuine theatrical elements (invocation of the people's name while actually pursuing factional interests), but the mechanism is not purely performative—real obstruction occurs, real decisions are blocked, real immunity is enforced.
 *
 * PERSPECTIVAL GAP:
 *   The demagogue's faction classifies the constraint as Rope—they experience it as a coordination mechanism for their political power, requiring no coercion beyond the invocation of sacrosanctity. The Senate classifies it as Snare—they are structurally trapped; they cannot govern without either dissolving the office (requiring constitutional change) or accepting permanent paralysis. The citizen body also sees Snare—their interests are excluded entirely from the demagogue's calculus. The reform movement sees Scaffold—a temporary problem with a clear constitutional solution (veto restriction, explicit criteria for legitimate obstruction, sunset conditions). The analytical observer risks seeing Mountain—treating the extraction as inevitable to the logical structure of veto power—but the structural data reveals this as a false summit: the extraction depends on factional dynamics and doctrinal interpretation, not on immutable law.
 *
 * DIRECTIONALITY LOGIC:
 *   The demagogue's faction derives d ≈ 0.15 (beneficiary + arbitrage exit = low d → negative effective extraction toward them). The Senate derives d ≈ 0.55 (powerful, but victim of the veto + constrained exit = moderate d → moderate extraction away from them). The citizen body derives d ≈ 0.85 (powerless + trapped = high d → maximum extraction away from them). These directionality values, combined with the sigmoid f(d) and scope σ(local=0.8), produce the effective extractiveness experienced by each observer. The perspectival gap emerges from these differential d values: what the beneficiary experiences as coordination (low chi), the victim experiences as pure extraction (high chi).
 *
 * MANDATROPHY ANALYSIS:
 *   The demagogic lever reading resolves mandatrophy by identifying the distributional asymmetry. The protection doctrine is legitimate in the popular_shield_reading because it genuinely protects the weak against magistrate violence. The same doctrine becomes extractive in the demagogic_lever_reading when claimed by actors (Clodius, Antony) with sufficient factional power to use it as a weapon. The doctrine is not inherently extraction or protection—it is a structural tool that becomes one or the other depending on who wields it and how the power gradient aligns. The false summit (analytical Mountain view) naturalizes this as inevitable; the structural analysis reveals it as contingent on factional capability and doctrinal interpretation. The constraint exists in the intersection of the protection doctrine (fixed) and the demagogue's factional power (variable).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sacrosanctity_doctrine_discretion,
    'Is the demagogic weaponization of sacrosanctity a corruption of the doctrine''s true purpose, or a latent extraction mechanism revealed under pressure?',
    'Historical analysis of the doctrine''s stated purpose (shielding the tribune from magistrate retaliation) versus its actual structural use in the periods of Clodius and the Second Triumvirate. Comparison with periods where tribunes wielded sacrosanctity defensively vs offensively.',
    'If corruption: the doctrine is sound, the problem is misuse—constitutional reform focuses on usage restriction. If latent extraction: the doctrine itself contains an exploitable asymmetry—fundamental reform restructures protection to exclude veto. The reading''s framing assumes latent extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sacrosanctity_doctrine_discretion, conceptual, 'Whether tribunician sacrosanctity enables weaponization by structural design or incidental abuse').

omega_variable(
    factional_substitution_inevitable,
    'Does eliminating the demagogic veto (Clodius, Antony) resolve the constraint, or will the same extraction mechanism simply migrate to the next powerful faction able to invoke sacrosanctity?',
    'Historical tracking of which actors invoke sacrosanctity and for what purposes across the late Republic and Principate. Pattern analysis: does restriction of veto eliminate the constraint or simply shift the locus of power?',
    'If easily substitutable: the constraint''s core extractiveness ε is structural to the office, not to individual demagogues—reform must eliminate the office or the protection doctrine. If actor-dependent: ε can be reduced by excluding certain coalition patterns—reform can be tactical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(factional_substitution_inevitable, empirical, 'Whether demagogic veto extraction is intrinsic to the office or contingent on factional dynamics').

omega_variable(
    reader_frame_dependence,
    'Does this reading (demagogic lever) privilege the Senate/institutional perspective as the normative reference frame, thereby framing veto-wielding tribunes as corruption rather than legitimate use of office?',
    'Examine this reading''s axiom set (below) for implicit preference for senatorial continuity as the baseline. Compare to the popular_shield_reading''s axiom set (in the sibling constraint file), which privileges tribune sacrosanctity as baseline. Identify where the two readings'' reference frames diverge and whether that divergence is symmetrical or asymmetrical.',
    'If asymmetrical (one reading''s reference frame is treated as natural/default): the engine''s normative analysis must flag the frame choice as contentious, not neutral. If symmetrical: both readings are equally defensible within their respective frameworks, and the constraint contest is genuinely underdetermined.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reader_frame_dependence, conceptual, 'Frame dependence of this reading''s normative baseline').

omega_variable(
    extraction_versus_veto_legitimacy,
    'Can a tribune legitimately wield veto to obstruct a senatorial decision, and if so, under what conditions does obstruction become extraction rather than protection?',
    'Doctrinal analysis: does the source material (Livy, Tacitus, Cassius Dio, Sallust) establish criteria for legitimate veto use? Identification of cases where commentators distinguish protective veto from obstructive veto. Modern constitutional precedent (U.S. presidential veto doctrine, parliamentary confidence votes) for comparison of obstruction vs protection frames.',
    'If legitimate criteria exist: ε is lower (constrained veto use is Tangled Rope, not Snare). If no criteria established: veto is inherently subject to faction-dependent interpretation—this reading''s framing (extraction) and the popular_shield_reading''s framing (protection) are both defensible, and the constraint is genuinely underdetermined.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_versus_veto_legitimacy, conceptual, 'Doctrinal criteria distinguishing legitimate protective veto from extractive obstruction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tribunician_sacrosanctity__demagogic_lever_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trib_dem_tr_t0, tribunician_sacrosanctity__demagogic_lever_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(trib_dem_tr_t3, tribunician_sacrosanctity__demagogic_lever_reading, theater_ratio, 3, 0.48).
narrative_ontology:measurement(trib_dem_tr_t6, tribunician_sacrosanctity__demagogic_lever_reading, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(trib_dem_be_t0, tribunician_sacrosanctity__demagogic_lever_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(trib_dem_be_t3, tribunician_sacrosanctity__demagogic_lever_reading, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(trib_dem_be_t6, tribunician_sacrosanctity__demagogic_lever_reading, base_extractiveness, 6, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(trib_dem_su_t0, tribunician_sacrosanctity__demagogic_lever_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(trib_dem_su_t3, tribunician_sacrosanctity__demagogic_lever_reading, suppression_requirement, 3, 0.65).
narrative_ontology:measurement(trib_dem_su_t6, tribunician_sacrosanctity__demagogic_lever_reading, suppression_requirement, 6, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tribunician_sacrosanctity__demagogic_lever_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(tribunician_sacrosanctity__demagogic_lever_reading, tribunician_sacrosanctity__popular_shield_reading).
narrative_ontology:affects_constraint(tribunician_sacrosanctity__demagogic_lever_reading, tribunician_sacrosanctity__imperial_absorption_reading).

% DUAL FORMULATION NOTE:
% Tribunician sacrosanctity decomposes into three structurally distinct constraint stories, each modeling one reading of the same kernel. Shared base: the doctrine of inviolability and veto authority. Distinct readings: (1) demagogic_lever—extraction mechanism weaponizing the protection; (2) popular_shield—legitimate protection mechanism with doctrinal integrity; (3) imperial_absorption—doctrinal obsolescence under consolidation. The three stories are linked by network.affects_constraints, not by single-story ambiguity. Each has its own ε, its own victim set, and its own beneficiary structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tribunician_sacrosanctity__demagogic_lever_reading, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
