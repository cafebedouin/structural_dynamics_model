% ============================================================================
% CONSTRAINT STORY: constitutional_hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_hybrid_reading, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: constitutional_hybrid_reading
 *   human_readable: Constitutional Hybrid Legitimacy: Inherited Status Constrained by Constitutional Limits Grounded in Popular Sovereignty
 *   domain: political_theory/constitutional_law/comparative_government
 *
 * SUMMARY:
 *   The constitutional hybrid reading claims that political authority is
 *   legitimate when inherited status is constrained by constitutional limits
 *   grounded in popular sovereignty. This reading represents one of three
 *   structurally distinct positions on the ground of political legitimacy in
 *   monarchical systems. The monarchical reading argues that authority
 *   derives wholly from heredity (tradition, dynasty, natural order); the
 *   republican reading argues that authority derives wholly from the people
 *   (popular will, contract, elections); the constitutional hybrid reading —
 *   this constraint — argues that authority is legitimate when BOTH sources
 *   operate: succession is hereditary (stability, continuity) BUT power is
 *   constitutionally limited (accountability, popular input). The hybrid
 *   reading attempts to synthesize the beneficiaries of both alternatives
 *   (hereditary house gains stable succession; populace gains constitutional
 *   protection) while avoiding their victims (absolute monarchy's threat to
 *   rights; pure republicanism's threat to stable succession). The constraint
 *   exhibits Tangled Rope structure: genuine coordination functions
 *   (succession rules prevent civil war, constitutional rights protect
 *   property and contract) coexist with asymmetric extraction (the hereditary
 *   house retains practical authority over most key decisions; constitutional
 *   limits are more theatrical than functional). Theater ratio increases from
 *   0.38 to 0.55 over the interval, reflecting the gradual divergence between
 *   constitutional claims and actual practice as the hereditary authority
 *   finds ways to work around written constraints while maintaining
 *   ceremonial compliance with constitutional form.
 *
 * KEY AGENTS:
 *   - Hereditary House (Dynasty): Primary beneficiary (institutional/arbitrage) — gains stable succession rule and concentrated authority that is legitimated through constitutional theater
 *   - Populace/Citizens: Primary victim (powerless/trapped and identity_locked/generational) — bears extraction through constrained rights and inherited taxation while being locked into a legitimacy narrative that makes the constraint appear inevitable
 *   - Constitutional Officials (Judges, Ministers): Secondary beneficiary and victim (moderate/constrained) — benefit from rule-based authority and property protection; suffer from inability to challenge succession and dependence on monarchical pleasure
 *   - Democratic Accountability Mechanism: Claimed beneficiary (institutional/analytical) — represented as a constraint on the hereditary house; in practice, often subordinate or decorative
 *   - Absolute Monarchical Power: Named victim (analyzed as what the hybrid reading claims to constrain) — the reading positions itself as limiting pure autocracy
 *   - Pure Popular Sovereignty: Named victim (analyzed as what the hybrid reading claims to temper) — the reading positions itself as limiting pure democracy's threats to stability
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the hybrid as an immutable solution to an eternal political problem
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_hybrid_reading, 0.38).
domain_priors:suppression_score(constitutional_hybrid_reading, 0.42).
domain_priors:theater_ratio(constitutional_hybrid_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_hybrid_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(constitutional_hybrid_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(constitutional_hybrid_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_hybrid_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_hybrid_reading, "Constitutional Hybrid Legitimacy: Inherited Status Constrained by Constitutional Limits Grounded in Popular Sovereignty").
narrative_ontology:topic_domain(constitutional_hybrid_reading, "political_theory/constitutional_law/comparative_government").

domain_priors:requires_active_enforcement(constitutional_hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_hybrid_reading, '469fb9a8-ce12-4378-a663-fc6909696aec').
narrative_ontology:cs_created_at('469fb9a8-ce12-4378-a663-fc6909696aec', '').
narrative_ontology:cs_kernel_codification('469fb9a8-ce12-4378-a663-fc6909696aec', formalized).
narrative_ontology:cs_authority_grounding('469fb9a8-ce12-4378-a663-fc6909696aec', extraction).
narrative_ontology:cs_interpretation_layer_present('469fb9a8-ce12-4378-a663-fc6909696aec').
narrative_ontology:cs_kernel_id(constitutional_hybrid_reading, sovereign_legitimacy).
narrative_ontology:cs_reading_relation('469fb9a8-ce12-4378-a663-fc6909696aec', monarchical_reading, forecloses).
narrative_ontology:cs_reading_relation('469fb9a8-ce12-4378-a663-fc6909696aec', republican_reading, forecloses).
narrative_ontology:cs_axiom('469fb9a8-ce12-4378-a663-fc6909696aec', foundational, hereditary_succession_necessity).
narrative_ontology:cs_axiom_status(hereditary_succession_necessity, holdable).
narrative_ontology:cs_axiom('469fb9a8-ce12-4378-a663-fc6909696aec', foundational, constitutional_constraint_legitimacy).
narrative_ontology:cs_axiom_status(constitutional_constraint_legitimacy, holdable).
narrative_ontology:cs_reference_frame('469fb9a8-ce12-4378-a663-fc6909696aec', balanced_hereditary_constitutionalism).
narrative_ontology:cs_drift_state('469fb9a8-ce12-4378-a663-fc6909696aec', contemporary_implementation_divergence, gap(practice_drift, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_hybrid_reading, hereditary_succession_institution).
narrative_ontology:constraint_beneficiary(constitutional_hybrid_reading, democratic_accountability_mechanism).
narrative_ontology:constraint_victim(constitutional_hybrid_reading, absolute_monarchical_power).
narrative_ontology:constraint_victim(constitutional_hybrid_reading, pure_popular_sovereignty).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBJECT UNDER ABSOLUTE DISCRETION (SNARE) — Citizens have no institutional exit from the hereditary authority structure and no effective accountability mechanism independent of the monarch's discretion. Theater surrounds the constitution (claims of limits it does not enforce); suppression is high (legal prohibition on challenging succession, inherited tax claims). The constitution constrains rhetoric but not power. Maximum extraction from the trapped subject's view.
constraint_indexing:constraint_classification(constitutional_hybrid_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: POPULACE LOCKED INTO LEGITIMACY NARRATIVE (SNARE) — Citizens are structurally mobile (could theoretically organize, emigrate, or revolt) but identity-locked into a framing where hereditary authority is 'legitimate' precisely because it is constitutionally limited. The populace internalizes the hybrid reading's core claim — that inherited status is acceptable *if* constrained — making the constraint appear consensual and unchangeable. This identity lock sustains extraction across generations by making alternative framings (pure popular sovereignty, pure hereditary autonomy) unthinkable within the inherited legitimacy narrative.
constraint_indexing:constraint_classification(constitutional_hybrid_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: CONSTITUTIONAL OFFICIAL (TANGLED ROPE) — Judges and ministers experience genuine coordination benefits (property rights, contractual enforcement, rule-based precedent) while also bearing extraction costs (career dependence on the monarch's pleasure, inability to challenge succession, constrained reform authority). The constitution both enables and constrains their agency — they coordinate resource allocation and dispute resolution while remaining subordinate to inherited authority. Mixed experience: real benefits, real costs, some agency.
constraint_indexing:constraint_classification(constitutional_hybrid_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: HEREDITARY HOUSE (ROPE) — The reigning family experiences the constitutional constraint as coordination: the constitution legitimates their succession and concentrates power in their hands while *appearing* to limit it. They benefit from the theatrical performance of constitutional limits (which grants popular acceptance) while maintaining practical authority. They perceive coordination rather than extraction — the constitution solves their succession problem and grants stable rule. Arbitrage exit: they can always abandon the constraint and reassert absolute power (though at cost of legitimacy).
constraint_indexing:constraint_classification(constitutional_hybrid_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: IMPERIAL PROJECT (DYNASTIC CONTINUITY) — At the scale of dynastic survival across multiple realms or regions, the constitutional constraint enables coordination: it provides a stable succession rule (preventing civil war over the throne) and distributes accountability claims in ways that pacify different regional factions. The dynasty sees the constitution as a coordination mechanism for managing multi-regional legitimacy — it accepts limits in one realm in exchange for hereditary continuity across realms. Net benefit to the dynasty: coordination.
constraint_indexing:constraint_classification(constitutional_hybrid_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: CONSTITUTIONAL TEXT (PITON) — The written constitution claims to limit inherited authority through procedures and rights protections, but these provisions are unenforceable (no supreme court with jurisdiction over succession, no impeachment power, monarchical power to suspend the constitution). The constitutional text persists through institutional inertia and theatrical compliance — the crown acknowledges constitutional limits in ceremonial contexts while maintaining de facto discretion. Theater ratio 0.55 reflects the performative character of constitutional constraint without functional bite.
constraint_indexing:constraint_classification(constitutional_hybrid_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational and universal standpoint, the hybrid reading appears as a natural resolution of an unsolvable tension: all authority requires both stability (hereditary succession) and accountability (popular consent). The fusion of these requirements in a constitutional hybrid is presented as an immutable law of political order — there is no way to have legitimate authority without both. However, this naturalizes what is actually a contingent institutional arrangement that serves hereditary power by clothing it in democratic language. The engine's false summit detector will identify this mountain as false: beneficiaries (hereditary succession, democratic-seeming limits) are clearly present, revealing that the 'natural law' framing obscures a distributional conflict.
constraint_indexing:constraint_classification(constitutional_hybrid_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_hybrid_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(constitutional_hybrid_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(constitutional_hybrid_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(constitutional_hybrid_reading, TR),
    TR >= 0.70.

:- end_tests(constitutional_hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The hybrid reading extracts value for the hereditary house (stable rule, legitimate authority) but less than pure monarchy would, because constitutional limits (even if largely theatrical) do impose some real constraints on revenue extraction, personnel decisions, and emergency powers. The moderateness reflects that the coordination benefits (property rights, contractual stability, succession clarity) are genuine and not entirely extractive overhead. Suppression (0.42): Moderate. Barriers to exit include legal prohibition on challenging succession, cultural conditioning into the legitimacy narrative, and practical difficulty of organizing against an incumbent authority. But suppression is not total — emigration is possible, dissent occurs (though punished), and alternative political imaginaries circulate in neighboring jurisdictions. Theater ratio (0.55): Moderate-high and rising. Constitutional provisions claim to limit the crown (bills of rights, legislative oversight, judicial review) but lack independent enforcement mechanisms. The crown acknowledges these limits in ceremonial contexts (swearing oaths, consulting Parliament) while retaining practical discretion. Over time, the gap between constitutional theater and actual practice grows as the crown learns to work around written constraints while maintaining formal compliance. The measurement shows a linear drift from 0.38 to 0.55 across the 40-year interval, indicating Goodhart decay — the constitutional form persists while its substantive function atrophies.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a striking perpectival gulf between the beneficiary institutions and the trapped/identity-locked subjects. The hereditary house sees coordination (stable succession, concentrated rule); the populace sees snares (extraction, no exit). Constitutional officials see tangled coordination (mixed benefits and constraints). The constitutional text itself (piton perspective) is degrading: the theater increases while enforcement decays. The analytical observer risks naturalizing this asymmetry as an immutable law of political order — that legitimacy requires both heredity and democracy, that the hybrid is the only sustainable solution. But the structural data reveals this as a false summit: the 'necessity' of the hybrid naturalizes what is actually a distribution that benefits the incumbent. The identity-lock perspective (generational snare) is the diagnostic key: the populace is not trapped by material barriers alone but by having internalized the hybrid reading's legitimacy frame, making alternatives unthinkable.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality reflects the agent's position in the extraction flow. The powerless subject (trapped) experiences maximum d ≈ 0.95, yielding high f(d) ≈ 1.42, therefore high experienced χ. The populace (identity-locked, generational) experiences moderate d ≈ 0.85 because while they are victims, their identity has fused with the constraint, creating partial cognitive suspension of the exit option — they perceive the constraint as legitimate and therefore somewhat accepted, reducing their subjective f(d) despite structural d being high. The constitutional official (constrained, moderate power) experiences moderate d ≈ 0.55, intermediate in the extraction flow. The hereditary house (beneficiary, arbitrage) experiences low d ≈ 0.10, yielding negative f(d) ≈ -0.01, therefore negative or near-zero χ — they experience the constraint as pure benefit. The analytical observer at civilizational scope experiences d ≈ 0.73 (canonical analytical), but the false summit classification indicates the observer is partially captured by the hybrid reading's naturalizing frame.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating how a reading of a contested kernel distributes benefits and costs asymmetrically while claiming synthesis. The hybrid reading claims to resolve the tension between hereditary stability and democratic accountability by combining both. But the structural data shows the combination is not symmetric: the hereditary house captures most of the benefits (stable succession, concentrated authority), while the populace bears most of the costs (constrained rights, no effective exit, identity-lock into accepting the arrangement). Tangled Rope classification is correct: genuine coordination functions (succession rules, property rights) coexist with asymmetric extraction (authority concentrated in the crown, constitutional limits largely theatrical). The mandatrophy is resolved by refusing the false claim that the hybrid 'balances' the two sources of legitimacy equally. Instead, the reading clearly benefits one party (the dynasty) more than the other, and this must be stated explicitly in any legitimacy claim that follows from this reading. A truly balanced hybrid would produce perspectives that showed similar distributions of benefit and extraction across all parties. This one does not.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_constraint_enforcement,
    'Are the constitutional limits on inherited authority actually enforceable, or is ''enforcement'' entirely dependent on the monarch''s voluntary compliance?',
    'Historical analysis: frequency of successful constitutional challenges to monarchical action; existence of independent adjudicatory bodies; presence of enforceable sanctions for constitutional violation; comparison with pure hereditary vs. pure democratic accountability mechanisms',
    'If enforceable: constraint is genuine Tangled Rope (real coordination + real constraints). If enforced only through voluntary compliance: constraint is Snare (extraction masked by legitimacy theater). If monarchy can unilaterally reinterpret constitution: Mountain status collapses and constraint reclassifies as pure hereditary extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_constraint_enforcement, empirical, 'Whether constitutional limits are independent from monarch''s discretion').

omega_variable(
    popular_sovereignty_content,
    'What does ''popular sovereignty'' mean within this reading: direct decision-making power, veto rights over major actions, electoral accountability for the crown, or merely symbolic consent?',
    'Textual analysis of constitutional provisions and legislative records; comparison of actual decision-making authority between crown and representative institutions; examination of succession procedures (can populace reject a successor?)',
    'If popular sovereignty means effective veto or decision power: reading is genuine hybrid with real popular authority. If it means only symbolic consent: victim group ''pure_popular_sovereignty'' is misnamed, and extraction is higher than measured. Reading reclassifies toward Snare from populace perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(popular_sovereignty_content, conceptual, 'Substantive meaning of popular sovereignty in this constitutional framework').

omega_variable(
    stability_legitimacy_tradeoff,
    'Is the constitutional hybrid''s benefit structure (stability through heredity, legitimacy through constitutional limits) actually a stable equilibrium, or does it inevitably degrade toward either pure monarchy or pure republicanism?',
    'Comparative historical analysis of hybrid constitutions across nations and time periods; measurement of institutional drift (do constitutional limits strengthen or weaken over time?); identification of structural feedback loops that preserve or destabilize the hybrid.',
    'If stable: Tangled Rope and Rope classifications are accurate for long-term perspectives. If degrades toward pure monarchy: victim ''pure_popular_sovereignty'' is prophetic; reading is unstable stage in transition to Snare. If degrades toward republicanism: beneficiary ''hereditary_succession_institution'' is prophetic; reading obscures transition to democratic constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stability_legitimacy_tradeoff, empirical, 'Whether constitutional hybrid is a stable institutional equilibrium').

omega_variable(
    identity_lock_mechanism_depth,
    'What sustains the populace''s identity-lock into the hybrid reading: cultural tradition, lack of alternative political imagination, successful ideological education, or structural dependence?',
    'Ethnographic and discourse analysis: examination of dissent suppression mechanisms; historical moments when the populace questioned the hybrid (revolutions, reforms); measurement of identity-lock persistence across generations in diaspora communities (do emigrants maintain the loyalty frame?)',
    'If cultural tradition alone: identity-lock is fragile; external demonstration of alternatives (neighboring republics, revolutionary discourse) can break it rapidly. If structural dependence: identity-lock is durable but rests on suppression; removing economic/legal barriers would enable exit. If successful ideological education: breaking the lock requires counter-ideological work; material barriers are secondary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_depth, empirical, 'Mechanism sustaining populace''s identity-lock into hybrid legitimacy frame').

omega_variable(
    kernel_contention_nature,
    'Is the contested kernel (sovereignty''s ground) fundamentally about WHERE legitimacy originates (heredity vs. people) or about HOW MUCH constraint is placed on authority once legitimacy is granted?',
    'Textual analysis of constitutional preambles and legitimacy claims; comparison with monarchical and republican readings on this specific axis; examination of whether disagreement is about *sources* of legitimacy or about *limits* on power once legitimacy is established',
    'If about origins: the three readings (monarchical, hybrid, republican) genuinely foreclose each other — they stake incompatible claims about the foundation. If about limits: readings coexist because they answer different questions (different factions might hold that legitimacy derives from the people AND that once in office, the crown should have broad discretion). Affects reading_relations classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contention_nature, conceptual, 'Whether kernel dispute concerns source or extent of legitimate authority').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_hybrid_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(conhyb_tr_t0, constitutional_hybrid_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(conhyb_tr_t20, constitutional_hybrid_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement(conhyb_tr_t40, constitutional_hybrid_reading, theater_ratio, 40, 0.55).

% Extraction over time
narrative_ontology:measurement(conhyb_be_t0, constitutional_hybrid_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(conhyb_be_t20, constitutional_hybrid_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(conhyb_be_t40, constitutional_hybrid_reading, base_extractiveness, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_hybrid_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_hybrid_reading, monarchical_reading).
narrative_ontology:affects_constraint(constitutional_hybrid_reading, republican_reading).
narrative_ontology:affects_constraint(constitutional_hybrid_reading, electoral_succession_constraint).
narrative_ontology:affects_constraint(constitutional_hybrid_reading, constitutional_review_power).

% DUAL FORMULATION NOTE:
% This constraint is a reading of the sovereign_legitimacy kernel alongside monarchical_reading and republican_reading. Each reading has distinct ε values and beneficiary/victim structures. The hybrid reading (ε=0.38) represents an intermediate position between pure monarchy (higher ε for absolute extraction) and pure republicanism (lower ε for distributed authority). The three readings form a family linked by network.affects_constraints, but each is a distinct constraint with its own measurement trajectory and classification profile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_hybrid_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
