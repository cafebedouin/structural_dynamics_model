% ============================================================================
% CONSTRAINT STORY: tenth_amendment__anticommandeering_doctrine
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tenth_amendment__anticommandeering_doctrine, []).

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
 *   constraint_id: tenth_amendment__anticommandeering_doctrine
 *   human_readable: Tenth Amendment Anticommandeering Doctrine (New York/Printz Line)
 *   domain: constitutional_law/federalism
 *
 * SUMMARY:
 *   The anticommandeering doctrine — the modern constitutional principle that
 *   Congress may not conscript state legislatures or state executive officers
 *   to administer federal programs — represents a specific reading of the
 *   Tenth Amendment's reserved-powers clause. New York v. United States
 *   (1992) and Printz v. United States (1997) drew the doctrinal line: direct
 *   commandeering of state machinery is unconstitutional, but conditional
 *   spending (even if coercive) and cooperative federalism frameworks remain
 *   permissible. This constraint story instantiates the
 *   anticommandeering_doctrine reading of the contested tenth_amendment
 *   kernel. The constraint exhibits tangled_rope classification from the
 *   institutional perspective (states benefit from protected autonomy while
 *   experiencing coordination gains from federal partnerships) and
 *   false-summit dynamics (the 'natural law' framing of federalism masks a
 *   contingent judicial choice that advantages state actors). Over 32 years
 *   (1992–2024), the extractiveness to federal programs has risen (from 0.22
 *   to 0.38) as Congress has learned to navigate around the doctrine's
 *   constraints, while theater has risen (from 0.35 to 0.55) as courts
 *   selectively enforce the doctrine and Congress predictably works around
 *   it. Suppression has paradoxically declined slightly (from 0.72 to 0.62)
 *   because states' organized political response (NGA litigation, multistate
 *   coordination) has improved, elevating exit options from 'trapped' toward
 *   'constrained.'
 *
 * KEY AGENTS:
 *   - Federal Program Administrators (powerless/trapped): Bear the structural cost of commandeering prohibition; must hire federal employees or purchase state cooperation through expensive conditional spending
 *   - State Legislatures and Executive Officers (institutional/arbitrage): Primary beneficiaries; retain autonomy to decline federal programs and cannot be conscripted involuntarily
 *   - States Participating in Federal Programs (moderate/constrained): Experience both coordination gains (interstate highway system, Clean Air Act) and extraction (federal mandates, compliance costs, constrained exit)
 *   - National Governors Association and Multistate Coalitions (organized/constrained): Organized states can negotiate with Congress and coordinate litigation; suppression is reduced by collective action
 *   - Federal Courts (institutional/arbitrage): Enforce the doctrine selectively; gatekeep commandeering statutes; apply different scrutiny to conditional spending and cooperative federalism
 *   - Congress (institutional/arbitrage): Learns and adapts; converts commandeering into conditional spending or private-party conscription (permissible under Printz); routinely works around doctrine
 *   - Analytical Observer (analytical/analytical): Risks naturalizing a contingent judicial choice as inevitable federalism structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenth_amendment__anticommandeering_doctrine, 0.38).
domain_priors:suppression_score(tenth_amendment__anticommandeering_doctrine, 0.62).
domain_priors:theater_ratio(tenth_amendment__anticommandeering_doctrine, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenth_amendment__anticommandeering_doctrine, extractiveness, 0.38).
narrative_ontology:constraint_metric(tenth_amendment__anticommandeering_doctrine, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(tenth_amendment__anticommandeering_doctrine, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenth_amendment__anticommandeering_doctrine, tangled_rope).
narrative_ontology:human_readable(tenth_amendment__anticommandeering_doctrine, "Tenth Amendment Anticommandeering Doctrine (New York/Printz Line)").
narrative_ontology:topic_domain(tenth_amendment__anticommandeering_doctrine, "constitutional_law/federalism").

domain_priors:requires_active_enforcement(tenth_amendment__anticommandeering_doctrine).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenth_amendment__anticommandeering_doctrine, '60584398-0965-4b0b-9ad5-d8c6a0a2e7ed').
narrative_ontology:cs_kernel_codification('60584398-0965-4b0b-9ad5-d8c6a0a2e7ed', formalized).
narrative_ontology:cs_authority_grounding('60584398-0965-4b0b-9ad5-d8c6a0a2e7ed', lineage).
narrative_ontology:cs_interpretation_layer_present('60584398-0965-4b0b-9ad5-d8c6a0a2e7ed').
narrative_ontology:cs_reading_relation('60584398-0965-4b0b-9ad5-d8c6a0a2e7ed', tenth_amendment__political_safeguards_reading, coexists_with).
narrative_ontology:cs_reading_relation('60584398-0965-4b0b-9ad5-d8c6a0a2e7ed', tenth_amendment__truism_reading, coexists_with).
narrative_ontology:cs_axiom('60584398-0965-4b0b-9ad5-d8c6a0a2e7ed', foundational, states_retain_structural_sovereignty).
narrative_ontology:cs_axiom_status(states_retain_structural_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('60584398-0965-4b0b-9ad5-d8c6a0a2e7ed', states_retain_structural_sovereignty, conventional).
narrative_ontology:cs_axiom('60584398-0965-4b0b-9ad5-d8c6a0a2e7ed', foundational, involuntary_conscription_violates_sovereignty).
narrative_ontology:cs_axiom_status(involuntary_conscription_violates_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('60584398-0965-4b0b-9ad5-d8c6a0a2e7ed', involuntary_conscription_violates_sovereignty, deontological).
narrative_ontology:cs_reference_frame('60584398-0965-4b0b-9ad5-d8c6a0a2e7ed', pre_1992_uncertain_tenth_amendment_enforcement).
narrative_ontology:cs_drift_state('60584398-0965-4b0b-9ad5-d8c6a0a2e7ed', contemporary_post_printz, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('60584398-0965-4b0b-9ad5-d8c6a0a2e7ed', '').
narrative_ontology:cs_kernel_id(tenth_amendment__anticommandeering_doctrine, tenth_amendment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenth_amendment__anticommandeering_doctrine, state_legislatures).
narrative_ontology:constraint_beneficiary(tenth_amendment__anticommandeering_doctrine, state_executive_officers).
narrative_ontology:constraint_victim(tenth_amendment__anticommandeering_doctrine, federal_regulatory_programs).
narrative_ontology:constraint_victim(tenth_amendment__anticommandeering_doctrine, congressional_implementation_flexibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FEDERAL PROGRAM ADMINISTRATORS (SNARE) — Cannot exit the constraint without congressional action to rewrite federal statute. Trapped by the anticommandeering doctrine: they must hire federal employees, purchase compliance from states through conditional spending, or accept program failure. Suppression is structural (constitutional barrier). No alternative pathways to state enforcement. Experienced as pure extraction: the constraint blocks the most efficient implementation mechanism (commandeering) and forces more costly federal hiring or incentive structures.
constraint_indexing:constraint_classification(tenth_amendment__anticommandeering_doctrine, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: STATE LEGISLATURES AND EXECUTIVE OFFICERS (ROPE) — Beneficiaries of the anticommandeering doctrine. Arbitrage exit option: states can choose to cooperate with federal programs (conditional spending), decline them, or negotiate terms. The constraint protects administrative autonomy and preserves the option to opt out of costly federal mandates. Experienced as coordination: the doctrine preserves the boundary between state and federal implementation while enabling mutually beneficial cooperation. The benefit is structural autonomy; the cost is zero for non-participating states.
constraint_indexing:constraint_classification(tenth_amendment__anticommandeering_doctrine, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: STATES PARTICIPATING IN FEDERAL PROGRAMS (TANGLED ROPE) — States that voluntarily accept conditional federal funding experience both coordination and extraction. The coordination function: federal-state partnerships enable outcomes neither could achieve alone (interstate highway system, environmental protection, healthcare). The extraction: states must implement federal standards, incur compliance costs, and cannot unilaterally exit without forfeiting federal funding. The anticommandeering doctrine protects against involuntary commandeering, but participating states experience constrained exit (high cost of withdrawal). Genuine coordination function exists alongside asymmetric cost allocation.
constraint_indexing:constraint_classification(tenth_amendment__anticommandeering_doctrine, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: STATES COLLECTIVELY / NATIONAL GOVERNORS ASSOCIATION (TANGLED ROPE) — When states organize (NGA, ALEC, multistate litigation), they experience the anticommandeering doctrine as a coordination mechanism with mixed benefits and costs. Organized states can negotiate with Congress, refuse programs, or coordinate litigation (as in Printz itself). But suppression remains high: federal conditional spending still constrains choice. Organized exit option (constrained rather than trapped) because collective action creates political power. Both coordination (states can push back collectively) and extraction (federal programs still impose costs).
constraint_indexing:constraint_classification(tenth_amendment__anticommandeering_doctrine, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: THE DOCTRINE AS INSTITUTIONAL THEATER (PITON) — Over 40 years since New York v. United States (1992) and Printz v. United States (1997), the anticommandeering doctrine has become substantially performative. Congress routinely enacts 'commandeering' statutes under guises that survive anticommandeering scrutiny: conditional spending (permissible even if coercive), cooperative federalism frameworks (permissible even if effectively mandatory), private-party conscription (permissible — only government actors are protected). The doctrine's teeth have eroded. Courts enforce it selectively; Congress works around it predictably. Theater ratio is moderate (0.55) because the doctrine still blocks some direct commandeering, but vast federal-state coordination proceeds with the anticommandeering barrier largely performative rather than functional.
constraint_indexing:constraint_classification(tenth_amendment__anticommandeering_doctrine, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a structural analysis, the anticommandeering doctrine appears as a necessary logical consequence of federalism: if states retain sovereignty, Congress cannot conscript state machinery without consent. The constraint looks like an immutable feature of the constitutional structure — following necessarily from the premise that states are not mere administrative subdivisions. However, the structural data reveals a false summit: the doctrine is a judicial choice (appearing suddenly in 1992 after 200 years of uncertain status), reflects beneficiary interests (state autonomy), and exhibits enforcement theater. The 'natural law of federalism' framing naturalizes what is actually a contested institutional commitment.
constraint_indexing:constraint_classification(tenth_amendment__anticommandeering_doctrine, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tenth_amendment__anticommandeering_doctrine_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(tenth_amendment__anticommandeering_doctrine, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(tenth_amendment__anticommandeering_doctrine, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(tenth_amendment__anticommandeering_doctrine, TR),
    TR >= 0.70.

:- end_tests(tenth_amendment__anticommandeering_doctrine_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. Federal programs incur significant additional costs from the commandeering prohibition — federal hiring (Social Security Administration), private-sector conscription (Brady Act background checks, Clean Air Act monitoring), and incentive-based state cooperation all cost more than direct commandeering would. But extractiveness is not severe (not ≥0.46) because conditional spending and cooperativism do enable genuine federal-state coordination. Congress has effective workarounds; the doctrine constrains but does not prevent implementation. The rising trajectory (0.22→0.38) reflects increasing Congressional sophistication in working around the doctrine rather than increasing severity of the constraint itself. Suppression (0.62): High. The constitutional barrier is absolute for direct commandeering of state governments; no consent mechanism exists; states cannot override the doctrine except through constitutional amendment. However, suppression is not maximal (not ≥0.72) because states retain exit options through refusing conditional federal funding, and organized states can lobby Congress to modify federal programs. The declining trajectory (0.72→0.62) reflects states' growing organized capacity to negotiate terms and courts' selective enforcement. Theater ratio (0.55): Moderate. The doctrine produces real legal constraints (direct commandeering statutes fail in court), but these constraints have proven navigable. Congress has developed reliable workarounds; courts enforce inconsistently; the doctrine's practical effect is to reshape federal-state bargaining rather than to block programs outright. Rising theater (0.35→0.55) reflects growing asymmetry between doctrinal language ('no commandeering') and functional reality ('cooperativism and conditional spending achieve commandeering outcomes with different institutional forms'). The doctrine persists because it provides political cover for both Congress (respecting federalism in form) and states (preserving autonomy in theory), even as functional commandeering proceeds.
 *
 * PERSPECTIVAL GAP:
 *   The anticommandeering doctrine demonstrates maximum perspectival divergence. Federal program administrators see snare (blocked from efficient implementation mechanism, no alternative exits). State legislatures see rope (protected autonomy, beneficial coordination option). Participating states see tangled rope (genuine coordination gains alongside constrained exit). Organized states see modified tangled rope (enhanced exit options through collective power). Courts see piton (selective enforcement, institutional theater persisting through inertia). The analytical observer sees mountain (natural federalism consequence) but the structural data reveals a false summit (contingent judicial choice advantaging state actors). The gap reveals that the constraint is not an immutable natural law but a contested institutional commitment whose legitimacy depends on which reading of the Tenth Amendment one accepts.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from the agent's structural position relative to the anticommandeering constraint. Federal program administrators (primary victim) have high d (constrained/trapped exit + victim status + powerless position) → maximum effective extraction. State legislatures (primary beneficiary) have low d (arbitrage exit + beneficiary status + institutional position) → minimal/negative effective extraction. Participating states (secondary victims despite beneficiary status) have mid-range d (they both benefit from autonomy protection and bear costs from conditional spending constraints + constrained exit). Organized states have lower d than unorganized ones (constrained→mobile shift through coalition power). The piton perspective derives low d (institutional/arbitrage) but classification from high theater_ratio, not from chi. The mountain perspective has canonical d (analytical context) but the false summit detection triggers because the constraint exhibits beneficiary interests despite natural law framing.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commandeering_vs_conditional_spending_boundary,
    'Is the anticommandeering/conditional-spending boundary a logically stable distinction or a performative separation that Congress can navigate at will?',
    'Longitudinal analysis of federal statutory innovation: tracking statutes that appear to commandeer but survive anticommandeering challenge vs those that fail; measuring Congress''s deliberate use of conditional spending as workaround',
    'If boundary is stable: anticommandeering doctrine has real teeth (snare and tangled_rope perspectives confirmed). If Congress routinely converts commandeering to conditional spending without friction: doctrine is mostly theater (piton perspective confirmed, theater ratio rises toward 0.75).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(commandeering_vs_conditional_spending_boundary, empirical, 'Whether anticommandeering/conditional-spending boundary is stable or navigable by Congress').

omega_variable(
    state_exit_option_costliness,
    'Can states realistically exit federal programs when commandeering is avoided but conditional spending is used, or does the cost structure make refusal prohibitive?',
    'Case studies of states refusing federal funding; cost-benefit analysis of federal programs; measuring whether states'' actual exit rates differ between commandeering-vulnerable statutes and conditional-spending statutes',
    'If states can exit conditional spending: exit option is ''mobile'' (lower d for state perspective). If conditional spending creates de facto traps: exit option is ''constrained'' (higher d), elevating extraction classification. This drives the tangled_rope vs snare perspectival distinction for participating states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_exit_option_costliness, empirical, 'Costliness of state exit from conditional federal programs').

omega_variable(
    anticommandeering_doctrine_naturity_vs_contingency,
    'Is the anticommandeering doctrine a natural structural consequence of federalism (mountain), or a judicial choice that could have gone differently and reflects beneficiary interests in state autonomy (tangled_rope or false summit)?',
    'Doctrinal genealogy: tracing Tenth Amendment interpretation from McCulloch (1819) through Darby (1941) to New York (1992). Counterfactual analysis: what would federalism look like without anticommandeering (conditional spending + cooperativism could coordinate more tightly)? Examining whether courts apply anticommandeering consistently or selectively based on policy outcomes.',
    'If doctrine is natural law: mountain classification stands (no false summit). If doctrine is contingent choice reflecting state-autonomy beneficiaries: false summit detected, reclassify as tangled_rope, and the entire constraint''s legitimacy shifts from ''inevitable structural consequence'' to ''institutional choice that advantages states.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anticommandeering_doctrine_naturity_vs_contingency, conceptual, 'Whether anticommandeering is natural consequence of federalism or contingent judicial choice').

omega_variable(
    interstate_negotiating_power_heterogeneity,
    'Do all states experience the anticommandeering doctrine as protective of autonomy, or do states with weak legislative capacity benefit more from federal commandeering (access to federal administrative machinery) than from protection against it?',
    'Comparative institutional analysis: correlating state capacity (GDP, administrative infrastructure, tax base) with actual federal-program participation and legislative preferences toward commandeering vs conditional spending. Measuring whether low-capacity states lobby for different federalism boundaries than high-capacity states.',
    'If heterogeneous: some state perspectives should classify differently (low-capacity states might see anticommandeering as snare rather than rope). This breaks the unified ''state beneficiary'' assumption and reveals distributional conflict within the beneficiary group.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interstate_negotiating_power_heterogeneity, empirical, 'Heterogeneous state capacity and differing preferences on commandeering doctrine').

omega_variable(
    kernel_reading_contest_in_doctrine,
    'Which sibling reading (political_safeguards_reading, truism_reading) is increasingly dominant in judicial and scholarly treatment, and does that dominance reflect erosion of the anticommandeering_doctrine reading itself?',
    'Citation analysis: measuring prevalence of anticommandeering doctrine in federal court opinions, law review articles, and policy documents over time. Tracking Supreme Court language shifts: toward political safeguards (federalism as self-enforcing through Senate), toward truism (Tenth is merely declaratory), or toward anticommandeering (states have enforceable immunity). Longitudinal measurement of whether the doctrine''s functional scope has narrowed relative to judicial language about it.',
    'If political_safeguards dominates: the anticommandeering doctrine''s authority grounding shifts from lineage (binding precedent) to diffuse_epistemic (scholarly opinion that federalism is self-correcting). If truism dominates: the doctrine becomes performative (piton classification, theater rises). This is the committer-frame omega — it records the kernel-level contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_in_doctrine, conceptual, 'Ascendance of sibling readings in doctrine relative to anticommandeering reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenth_amendment__anticommandeering_doctrine, 1992, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenth_anticommandeering_tr_t0, tenth_amendment__anticommandeering_doctrine, theater_ratio, 0, 0.35).
narrative_ontology:measurement(tenth_anticommandeering_tr_t10, tenth_amendment__anticommandeering_doctrine, theater_ratio, 10, 0.45).
narrative_ontology:measurement(tenth_anticommandeering_tr_t20, tenth_amendment__anticommandeering_doctrine, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(tenth_anticommandeering_be_t0, tenth_amendment__anticommandeering_doctrine, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(tenth_anticommandeering_be_t10, tenth_amendment__anticommandeering_doctrine, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(tenth_anticommandeering_be_t20, tenth_amendment__anticommandeering_doctrine, base_extractiveness, 20, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(tenth_anticommandeering_su_t0, tenth_amendment__anticommandeering_doctrine, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(tenth_anticommandeering_su_t10, tenth_amendment__anticommandeering_doctrine, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(tenth_anticommandeering_su_t20, tenth_amendment__anticommandeering_doctrine, suppression_requirement, 20, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenth_amendment__anticommandeering_doctrine, enforcement_mechanism).
narrative_ontology:affects_constraint(tenth_amendment__anticommandeering_doctrine, conditional_spending_doctrine).
narrative_ontology:affects_constraint(tenth_amendment__anticommandeering_doctrine, cooperative_federalism_frameworks).
narrative_ontology:affects_constraint(tenth_amendment__anticommandeering_doctrine, state_capacity_heterogeneity_in_federal_programs).

% DUAL FORMULATION NOTE:
% The anticommandeering doctrine is one element of the broader federalism constraint ecosystem. It influences downstream constraints (conditional spending, cooperative federalism) by providing a doctrinal boundary that Congress works around. The doctrine itself is shaped by upstream state-capacity constraints and interstate power asymmetries. Network decomposition: each constraint gets its own story with differentiated ε values because they measure different aspects of federal-state coordination (commandeering prohibition, spending coercion, capacity distribution).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
