% ============================================================================
% CONSTRAINT STORY: article_27_veto_power__oligopoly_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_27_veto_power__oligopoly_reading, []).

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
 *   constraint_id: article_27_veto_power__oligopoly_reading
 *   human_readable: Article 27 P5 Veto Power as Oligopoly Entrenchment (Oligarchy Reading)
 *   domain: international_relations/institutional_design
 *
 * SUMMARY:
 *   The Article 27 veto provision of the UN Charter represents one of the
 *   most consequential institutional entrenchments in modern history. Read
 *   through the oligarchy lens (this constraint), the veto is not merely a
 *   coordination mechanism but a structural device that locks permanent P5
 *   authority into place indefinitely, suppresses institutional evolution
 *   that would redistribute power, and extracts ongoing authority rents from
 *   a non-P5 majority that cannot exit or reform the system. This reading
 *   locates extraction not in any single veto action but in the long-term
 *   structural architecture that prevents charter amendment despite 80 years
 *   of geopolitical change. The constraint exhibits high extractiveness
 *   (0.68) and high suppression (0.72) because non-P5 states face multiple
 *   binding mechanisms: (1) UN universality makes exit impossible (no
 *   alternative global forum exists), (2) charter amendment requires 7/15
 *   Security Council votes (P5 can block any reform that threatens their
 *   interests), (3) veto power itself suppresses emergence of competing
 *   institutions, (4) the legitimacy structure of international law treats UN
 *   decisions as binding, making alternatives illegitimate. Theater_ratio
 *   (0.55) reflects that while the UN General Assembly and Security Council
 *   maintain performative functions, real coordination among great powers
 *   happens through bilateral diplomacy and back-channel negotiations outside
 *   the formal veto structure.
 *
 * KEY AGENTS:
 *   - Permanent Five States (P5: US, China, Russia, UK, France): Primary beneficiaries (institutional/arbitrage) — capture exclusive authority, blocking power, and indefinite geopolitical primacy. Can exit UN with minimal cost. Extractiveness flows toward this agent.
 *   - Non-P5 Majority (188 nations): Primary victims (powerless/trapped) — cannot exit UN (no alternative), cannot reform charter (veto blocks), cannot access decision-making equivalent to power (forced subordination). Maximum suppression.
 *   - Aspirant Great Powers (India, Brazil, Japan, Germany, South Africa): Secondary victims (moderate/constrained) — seek permanent seat but face P5 veto block; indefinitely subordinate despite equivalent geopolitical weight. Medium suppression and constrained exit.
 *   - Non-Aligned Movement & Regional Blocs (organized actors with 80+ members): Organized secondary victims (organized/constrained) — have coalition voice but face absolute veto suppression on core interests. Medium extractiveness through blocked reforms they collectively support.
 *   - UN Secretariat & Administrative Apparatus: Institutional decay (institutional/arbitrage) — maintains performative functions (monitoring, reporting, mediation) but lacks real enforcement power. Piton classification: formerly functional coordination, now ritualized.
 *   - Analytical observer (civilizational): Risks naturalizing contingent institutional arrangement (1945 compromise) as immutable law of geopolitics.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__oligopoly_reading, 0.68).
domain_priors:suppression_score(article_27_veto_power__oligopoly_reading, 0.72).
domain_priors:theater_ratio(article_27_veto_power__oligopoly_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__oligopoly_reading, snare).
narrative_ontology:human_readable(article_27_veto_power__oligopoly_reading, "Article 27 P5 Veto Power as Oligopoly Entrenchment (Oligarchy Reading)").
narrative_ontology:topic_domain(article_27_veto_power__oligopoly_reading, "international_relations/institutional_design").

domain_priors:requires_active_enforcement(article_27_veto_power__oligopoly_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__oligopoly_reading, 'c325104e-a365-4bbb-a1a2-0ca2ba720ccd').
narrative_ontology:cs_kernel_codification('c325104e-a365-4bbb-a1a2-0ca2ba720ccd', formalized).
narrative_ontology:cs_authority_grounding('c325104e-a365-4bbb-a1a2-0ca2ba720ccd', extraction).
narrative_ontology:cs_reading_relation('c325104e-a365-4bbb-a1a2-0ca2ba720ccd', article_27_veto_power__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('c325104e-a365-4bbb-a1a2-0ca2ba720ccd', article_27_veto_power__sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('c325104e-a365-4bbb-a1a2-0ca2ba720ccd', foundational, veto_locks_geopolitical_inequality_indefinitely).
narrative_ontology:cs_axiom_status(veto_locks_geopolitical_inequality_indefinitely, holdable).
narrative_ontology:cs_axiom_grounding('c325104e-a365-4bbb-a1a2-0ca2ba720ccd', veto_locks_geopolitical_inequality_indefinitely, empirically_contingent).
narrative_ontology:cs_axiom('c325104e-a365-4bbb-a1a2-0ca2ba720ccd', foundational, suppressed_alternative_platforms_reveal_extractiveness).
narrative_ontology:cs_axiom_status(suppressed_alternative_platforms_reveal_extractiveness, holdable).
narrative_ontology:cs_axiom_grounding('c325104e-a365-4bbb-a1a2-0ca2ba720ccd', suppressed_alternative_platforms_reveal_extractiveness, empirically_contingent).
narrative_ontology:cs_reference_frame('c325104e-a365-4bbb-a1a2-0ca2ba720ccd', p5_permanent_security_authority).
narrative_ontology:cs_drift_state('c325104e-a365-4bbb-a1a2-0ca2ba720ccd', contemporary_multipolar_shift, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c325104e-a365-4bbb-a1a2-0ca2ba720ccd', '').
narrative_ontology:cs_kernel_id(article_27_veto_power__oligopoly_reading, article_27_veto_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_27_veto_power__oligopoly_reading, permanent_five_states).
narrative_ontology:constraint_victim(article_27_veto_power__oligopoly_reading, non_p5_majority).
narrative_ontology:constraint_victim(article_27_veto_power__oligopoly_reading, institutional_reform_capacity).
narrative_ontology:constraint_victim(article_27_veto_power__oligopoly_reading, geopolitical_adaptability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-P5 MAJORITY (SNARE) — 188 nations cannot exit the UN system (it is the only global coordination platform for existential threats) yet cannot reform it (Article 27 blocks all charter amendments that threaten P5 interests). Experiences maximum extraction: forced participation, zero voice in fundamental rules, suppression of exit alternatives (no functional competitor to UN exists). Maximum experienced χ.
constraint_indexing:constraint_classification(article_27_veto_power__oligopoly_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGIONAL POWERS / ASPIRANT P6 CANDIDATES (SNARE) — India, Brazil, Japan, Germany, South Africa face suppressed pathways to institutional elevation (veto holders block expansion of permanent seats). Extraction occurs through indefinite subordinate status despite equivalent geopolitical weight. High suppression due to veto block; constrained exit options (could abstain from UN but lose legitimacy; must participate to maintain relevance). χ approaches beneficiary range but remains snare territory due to asymmetric information and veto threat.
constraint_indexing:constraint_classification(article_27_veto_power__oligopoly_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PERMANENT FIVE VETO STATES (ROPE) — Legitimate coordination function: P5 veto enables great-power consensus on existential matters (nuclear wars, genocide intervention), reducing unilateral action risk. But this perspective also experiences the constraint as a pure coordination mechanism: maintaining the consensus framework benefits all parties. From this view, the veto is proportionate to power, necessary to prevent defection. The beneficiary perspective experiences the constraint as purely coordinative, with their own authority as the legitimate basis for coordination. They can exit (leave UN) with minimal cost to themselves. χ is low or negative from this perspective.
constraint_indexing:constraint_classification(article_27_veto_power__oligopoly_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: NON-ALIGNED MOVEMENT & REGIONAL BLOCS (TANGLED ROPE) — These organized actors (80+ nations) have coordination benefits from bloc voting and collective pressure (genuine coordination function), but also face asymmetric extraction through veto-blocking of resolutions they collectively support. They have some agency (coalition pressure can shift outcomes), but face hard suppression ceiling (veto is absolute). χ reflects mixed experience: some coordination benefit, significant extraction.
constraint_indexing:constraint_classification(article_27_veto_power__oligopoly_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: UN SECRETARIAT & ADMINISTRATIVE STRUCTURE (PITON) — The institutional apparatus persists through ritual and inertia. The Secretariat's actual enforcement power has eroded (cannot enforce security council resolutions without member compliance; cannot mediate when P5 diverge). The veto structure delegates all power to P5, making the UN bureaucracy performative: it appears to coordinate but actual coordination happens in P5 back-channels and bilateral diplomacy. Theater_ratio high because the UN General Assembly and Secretariat maintain performative functions despite real decision power flowing through veto. Piton classification: formerly functional coordination (rope in post-1945 brief moment), now degraded to ritual maintenance.
constraint_indexing:constraint_classification(article_27_veto_power__oligopoly_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURALIZATION VIEW (MOUNTAIN) — From a civilizational perspective, the veto could be seen as natural law: great powers have always required veto rights to prevent coercive subordination; geopolitical realism mandates that armed superpowers cannot be bound by majority vote; any global system without P5 consent will fragment into competing blocs. This reading naturalizes the veto as an inherent feature of how international coordination must work. However, the structural data contradicts this mountain classification: the veto is contingent (chosen in 1945, could be reformed), extractive (benefits P5 indefinitely), and suppresses alternatives (prevents emergence of competing global forums). The engine flags this as a false summit.
constraint_indexing:constraint_classification(article_27_veto_power__oligopoly_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_27_veto_power__oligopoly_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(article_27_veto_power__oligopoly_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(article_27_veto_power__oligopoly_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_27_veto_power__oligopoly_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(article_27_veto_power__oligopoly_reading, TR),
    TR >= 0.70.

:- end_tests(article_27_veto_power__oligopoly_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts from non-P5 states through forced subordination without voice, indefinite power inequality despite shifted geopolitical weight, and blocked pathways to institutional reform. The extraction is not from a single veto action but from the accumulated lock-in: 80 years of prevented reform despite multiple reform attempts (Uniting for Peace, expansion of permanent seats, weighted voting proposals). The extractiveness value increases over time (0.45→0.68) because the geopolitical relevance of the 1945 power distribution decayed while the veto structure became more rigid. Suppression (0.72): Very high. Multiple binding mechanisms prevent exit: (1) UN universality is necessary for legitimacy and coordination on existential threats, (2) article 27 supermajority blocks all charter amendments that P5 oppose, (3) non-state actors lack standing to create alternative institutions, (4) bilateral and regional alternatives cannot achieve universal coordination. Theater_ratio (0.55): Moderate-high. The UN Security Council and General Assembly maintain substantial performative function: countries participate in debates, draft resolutions, cast votes, give speeches — but the real outcomes (blocking, allowing, or shaping action) are determined by P5 veto and great-power consensus outside the formal forum. The rise from 0.35 to 0.55 reflects increasing delegation of real coordination to back-channel forums (P5 dinners, bilateral diplomacy) while the UN maintains ritual functions.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between the P5 beneficiary perspective (Rope — sees veto as necessary coordination mechanism) and the non-P5 majority perspective (Snare — sees veto as extractive entrenchment). The P5 genuinely benefits from the coordination framework and can exit with minimal cost; they experience the constraint as proportionate authority allocation. The non-P5 genuinely experiences suppression and cannot exit; they experience the constraint as coercive subordination. The organized coalition perspective (Tangled Rope) bridges: they have coordination benefits from bloc voting but face extraction through veto blocking. The piton perspective (Secretariat ritual) shows that the UN's performative functions have decayed while the veto structure remains rigid. The mountain perspective (naturalizing veto as geopolitical law) risks obscuring the contingency of the 1945 choice and the possibility of reform. All perspectives are truthful readings of the same structural phenomenon — the disagreement is about whether the veto is a functional coordination mechanism (P5 reading), a degraded ritual (Secretariat reading), a temporary entrenchment with sunset (non-existent perspective — no actor sees this), or a pure extraction mechanism (non-P5 reading).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's experienced extractiveness (χ) is computed from base_extractiveness (0.68) scaled by f(d) and scope modifier. The P5 beneficiary with arbitrage exit experiences low or negative χ because d ≈ 0.05 (full beneficiary status). The non-P5 powerless agent with trapped exit experiences maximum χ because d ≈ 0.95 (full target status). Regional power aspirants with constrained exit experience moderate-to-high χ. The piton perspective (analytical observing degraded ritual) experiences moderate χ but classifies as piton because theater_ratio exceeds 0.70. The mountain perspective (naturalizing the veto as inherent to geopolitics) faces false-summit detection because structural data (increasing extractiveness over time, asymmetric power distribution, suppressed alternatives) contradicts the naturalization frame.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by recognizing that the same structural phenomenon genuinely appears as coordination (from P5 perspective), extraction (from non-P5 perspective), and degraded ritual (from Secretariat perspective). The mandatrophy is not 'which type is correct?' but 'whose experience are we measuring?' The oligarchy reading (this constraint) adopts the non-P5 and reformist perspective, making snare the core classification. The coordination reading would adopt the P5 and great-power-security perspective, making rope the core. The sovereignty reading would adopt the state-autonomy perspective, emphasizing protection from majoritarian coercion. No single type encompasses the full structural picture — but the oligarchy reading is justified by (1) the measurement data showing extraction accumulation over 80 years, (2) the lock-in mechanisms preventing exit and reform, and (3) the asymmetry of voice (P5 can veto any change to their power; non-P5 cannot veto P5 authority). The false-summit detection flags the mountain perspective, preventing naturalization of a contingent institutional arrangement as inherent law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    post_1945_geopolitical_shift_relevance,
    'Do the P5 nations of 2025 (US, China, Russia, UK, France) represent the same geopolitical weight distribution as 1945, or have structural changes in military capability, economic power, and global influence made the veto allocation increasingly disproportionate?',
    'Quantitative power index comparison: military expenditure, nuclear arsenal size, GDP, UN General Assembly voting alignment, regional sphere-of-influence scope (1945 vs 2025)',
    'If power distribution has substantially shifted: veto allocation is demonstrated extractive rent-seeking, not coordination equilibrium. Snare classification is robust. If power distribution is stable: veto allocation may retain coordination function. Snare classification weakens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(post_1945_geopolitical_shift_relevance, empirical, 'Whether geopolitical weight distribution has shifted since 1945, making veto allocation disproportionate').

omega_variable(
    alternative_coordination_platform_feasibility,
    'Could a reformed UN (with expanded permanent seats or qualified-majority voting) or a competing global governance platform (e.g., Conference of Sovereign Equals) provide coordination benefits equivalent to or superior to the current P5 veto system?',
    'Comparative institutional analysis: what governance structure would satisfy the legitimate interests (preventing unilateral coercion of great powers) while enabling institutional evolution? Simulation or historical analogy (League of Nations, Concert of Europe, modern regional structures like EU/AU)',
    'If alternative exists: the veto is not a natural law but a chosen institutional arrangement that suppresses superior alternatives — snare classification is maximally robust, omega is resolved toward extractiveness. If no alternative provides equivalent security: veto retains coordination function, snare classification weakens toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_coordination_platform_feasibility, conceptual, 'Whether functionally superior alternative coordination structures are feasible').

omega_variable(
    charter_amendment_barrier_intentionality,
    'Was the Article 27 supermajority voting rule designed intentionally by P5 to entrench their power indefinitely, or was it a pragmatic compromise for immediate post-war consensus that became unintentionally rigid?',
    'Historical analysis of 1945 San Francisco Conference negotiations, primary source intent documentation, institutional evolution of veto usage over 80 years (correlation between geopolitical drift and veto blocking patterns)',
    'If intentional entrenchment: veto is deliberately extractive, snare classification is maximally clear, and the oligarchy reading is the core reading. If pragmatic compromise that became rigid: the constraint may be better understood as Piton (degraded scaffold with unintended rigidity) rather than Snare. Foundational axiom status changes from ''holdable'' to potential ''overridden''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(charter_amendment_barrier_intentionality, empirical, 'Whether veto entrenchment was intentional design or unintended rigidity').

omega_variable(
    veto_usage_asymmetry_and_blocking_pattern,
    'Do the P5 veto states use veto power equally, or do some states use it asymmetrically to block resolutions that threaten their specific interests (rather than great-power interests generally), revealing personal/regional extraction rather than system-level coordination?',
    'Quantitative analysis of veto usage by state, target of vetoed resolutions (security vs non-security, regional interest vs global concern), frequency over time, and stated justifications vs actual geopolitical interest served',
    'If asymmetric use pattern: veto is instrument of individual power extraction, not collective coordination. Snare classification is robust. If symmetric use: veto functions as deterrent against coercive majority, retaining coordination function. Snare classification weakens toward tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(veto_usage_asymmetry_and_blocking_pattern, empirical, 'Whether veto usage is symmetric (coordination) or asymmetric (personal extraction)').

omega_variable(
    successor_institution_feasibility_and_path_dependence,
    'What institutional lock-in mechanisms prevent the emergence of a successor institution that would supersede UN authority without P5 consent? Is the barrier structural (no actor has incentive to defect to new forum because coordination value requires universality) or extractive (P5 actively suppresses alternatives)?',
    'Analysis of barriers to new global forums: treaty-signature requirements, network effects binding states to UN participation, P5 sanctions against competitors, legitimacy dependency on UN imprimatur. Comparison to historical institutional replacements (League→UN, Concert of Europe→Congress system)',
    'If barriers are structural-coordination: non-P5 states truly cannot exit because UN universality is necessary. Suppression is coordination-cost, not extractive. Snare classification weakens. If barriers include active P5 suppression: the veto extends beyond veto-power itself to suppression-of-alternatives. Snare classification is maximally robust, and suppression value may be understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(successor_institution_feasibility_and_path_dependence, empirical, 'Mechanisms preventing emergence of successor global coordination institutions').

omega_variable(
    committer_frame_reading_ambiguity,
    'Which reading of the Article 27 kernel is the core reading — oligarchy, coordination, or sovereignty? The oligarchy reading (this constraint) treats veto as extractive entrenchment; the coordination reading treats it as necessary great-power consensus mechanism; the sovereignty reading treats it as respect for state autonomy. These framings are held by different parties (P5 vs non-P5, institutions vs states), and no single framework can hold all three simultaneously.',
    'Committer-axis analysis: which framing is embedded in the UN Charter''s legitimacy structure? The Charter names ''maintenance of international peace and security'' as the veto''s purpose (coordination reading), but does not address geopolitical rigidity (oligarchy reading) or state autonomy (sovereignty reading). The committer must choose which framing is primary to the kernel''s authority. This is not empirically resolvable — it is a choice about which reading the Charter''s legitimacy commits to.',
    'If coordination reading is primary: the veto is a functional instrument of collective security, and reform attempts are viewed as destabilizing. If oligarchy reading is primary: the veto is an extractive entrenchment mechanism, and reform is viewed as necessary institutional evolution. If sovereignty reading is primary: the veto is a protection of state autonomy against majoritarian coercion. The three readings coexist; none forecloses the others. Mandatrophy is resolved by recognizing that all three are live positions held by different parties, and institutional evolution requires negotiating among them rather than proving one ''correct''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_reading_ambiguity, conceptual, 'Committer-axis ambiguity: which reading (oligarchy, coordination, sovereignty) is the kernel''s core').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__oligopoly_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(p5veto_theater_1945, article_27_veto_power__oligopoly_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(p5veto_theater_1985, article_27_veto_power__oligopoly_reading, theater_ratio, 40, 0.48).
narrative_ontology:measurement(p5veto_theater_2025, article_27_veto_power__oligopoly_reading, theater_ratio, 80, 0.55).

% Extraction over time
narrative_ontology:measurement(p5veto_extract_1945, article_27_veto_power__oligopoly_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(p5veto_extract_1965, article_27_veto_power__oligopoly_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(p5veto_extract_1985, article_27_veto_power__oligopoly_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(p5veto_extract_2025, article_27_veto_power__oligopoly_reading, base_extractiveness, 80, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(p5veto_suppress_1945, article_27_veto_power__oligopoly_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(p5veto_suppress_1965, article_27_veto_power__oligopoly_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(p5veto_suppress_1985, article_27_veto_power__oligopoly_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(p5veto_suppress_2025, article_27_veto_power__oligopoly_reading, suppression_requirement, 80, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_27_veto_power__oligopoly_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_27_veto_power__oligopoly_reading, article_27_veto_power__coordination_reading).
narrative_ontology:affects_constraint(article_27_veto_power__oligopoly_reading, article_27_veto_power__sovereignty_reading).
narrative_ontology:affects_constraint(article_27_veto_power__oligopoly_reading, un_reform_deadlock).
narrative_ontology:affects_constraint(article_27_veto_power__oligopoly_reading, great_power_concert_system).

% DUAL FORMULATION NOTE:
% The Article 27 veto kernel decomposes into three structurally distinct constraints (oligarchy, coordination, sovereignty readings) based on which normative reading of the charter authority is primary. Each reading produces different ε values, different perspectives, and different classifications. The oligarchy reading (this constraint, ε=0.68) treats veto as extractive entrenchment; the coordination reading (sibling constraint, ε≈0.35) treats it as necessary consensus mechanism; the sovereignty reading (sibling constraint, ε≈0.40) treats it as state autonomy protection. All three readings flow from the same institutional text but are fundamentally different constraints with different beneficiary/victim structures. The oligarchy reading is downstream of (and affected by) the coordination reading's institutional legitimacy claims — the coordination reading's authority rents depend on naturalizing the veto as necessary, which the oligarchy reading contests.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_27_veto_power__oligopoly_reading, organized, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
