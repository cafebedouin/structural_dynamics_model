% ============================================================================
% CONSTRAINT STORY: fork_mechanical_substitution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fork_mechanical_substitution, []).

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
 *   constraint_id: fork_mechanical_substitution
 *   human_readable: Fork Mechanical Substitution and Bite Configuration Drift
 *   domain: cultural_anthropology/technology_adoption/behavioral_economics
 *
 * SUMMARY:
 *   Fork adoption in medieval and early modern Europe represents a case of
 *   technology-mediated behavioral drift where a material object (the fork)
 *   gradually displaced manual eating practices, eventually making
 *   hand-eating socially impossible and embodied hand-eating skill rare
 *   through generational erosion. The constraint is puzzling because it
 *   exhibits suppression (fork etiquette becomes mandatory, hand-eating
 *   becomes socially unavailable) and extraction (dental phenotype shifts,
 *   manual dexterity atrophies, cultural competence gatekeeping restricts
 *   access to elite social spaces) without a clear identifiable beneficiary
 *   enforcing the system. No monopoly controls fork production; no single
 *   agent captures rents from the technology. Instead, the constraint appears
 *   to emerge from distributed status competition among elite actors seeking
 *   cultural distinction. This makes it an ideal test case for whether
 *   path-naturalization (the appearance of inevitability and naturalness)
 *   requires an explicit extractor, or whether coordination equilibria can
 *   lock into extractive configurations without anyone intentionally
 *   extracting. The constraint's extractiveness has accumulated over
 *   generations — initially (12th century) the fork was optional status
 *   signaling; by the 18th century fork-free eating had become socially
 *   impossible in European urban contexts; by the 21st century, the manual
 *   eating capacity itself has degraded (overbite development, loss of
 *   embodied skill transmission). Theater ratio has also risen: early fork
 *   adoption was openly performative (explicit status signal); modern fork
 *   use is naturalized as necessary and inevitable rather than as social
 *   choice.
 *
 * KEY AGENTS:
 *   - Elite Dining Culture: Primary beneficiary (institutional/arbitrage) — uses fork as status signal and coordination mechanism; enforces etiquette through social exclusion
 *   - Fork Manufacturing and Trade Networks: Secondary beneficiary (organized/arbitrage) — profits from increased tool demand; treats fork as solution to distribution problem, not extraction mechanism
 *   - Individual Eaters: Primary victim/constrained agent (moderate/constrained) — faces forced adoption costs but also gains coordination benefits (etiquette reduces social friction)
 *   - The Dental Phenotype: Structural victim (powerless/trapped) — embodied capacity shifts through generational habituation; skill atrophy makes manual eating increasingly difficult
 *   - Manual Eating Capacity: Distributed victim (powerless/trapped) — generational loss of hand-eating skill transmission; embodied knowledge erodes as fork becomes obligatory
 *   - Fork Resistance Communities: Organized but outcompeted agents (organized/constrained) — clergy, manual laborers maintained fork-free practices; resistance failed through attrition not active suppression
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent status-signaling choice into biomechanical necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fork_mechanical_substitution, 0.38).
domain_priors:suppression_score(fork_mechanical_substitution, 0.42).
domain_priors:theater_ratio(fork_mechanical_substitution, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fork_mechanical_substitution, extractiveness, 0.38).
narrative_ontology:constraint_metric(fork_mechanical_substitution, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(fork_mechanical_substitution, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fork_mechanical_substitution, tangled_rope).
narrative_ontology:human_readable(fork_mechanical_substitution, "Fork Mechanical Substitution and Bite Configuration Drift").
narrative_ontology:topic_domain(fork_mechanical_substitution, "cultural_anthropology/technology_adoption/behavioral_economics").

domain_priors:requires_active_enforcement(fork_mechanical_substitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fork_mechanical_substitution, fork_manufacturing_interests).
narrative_ontology:constraint_beneficiary(fork_mechanical_substitution, elite_dining_culture).
narrative_ontology:constraint_victim(fork_mechanical_substitution, manual_eating_capacity).
narrative_ontology:constraint_victim(fork_mechanical_substitution, dental_phenotype_variability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DENTAL PHENOTYPE (SNARE) — The human mouth, constrained by generational habituation to fork-mediated eating, loses mechanical capacity for direct bite manipulation. Once edge-to-edge bite becomes rare through fork use, manual dexterity for precise food manipulation atrophies. The phenotype is trapped: reverting to hand-eating requires relearning embodied skills that are no longer transmitted. Maximum extraction — the structural capacity itself has shifted.
constraint_indexing:constraint_classification(fork_mechanical_substitution, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INDIVIDUAL EATER (TANGLED ROPE) — Experiences both coordination benefit (fork standardizes eating etiquette, reduces social friction) and extraction (forced reliance on tool, loss of embodied eating skill). Can in principle revert to hand-eating but faces significant social cost. Constrained rather than trapped — exit is possible at a price (social stigma, loss of cultural competence).
constraint_indexing:constraint_classification(fork_mechanical_substitution, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ELITE DINING CULTURE (ROPE) — Primary beneficiary. Fork adoption signals and enforces status distinction. Benefits from coordination through standardization of etiquette. Experiences the constraint as pure coordination mechanism — the fork enables the cultural distinction they seek. Net positive — extraction runs toward this agent through the status asymmetry the fork creates and maintains.
constraint_indexing:constraint_classification(fork_mechanical_substitution, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: FORK MANUFACTURING (ROPE) — Organized economic actor that benefits from standardization and tool proliferation. Sees the constraint as pure coordination: the fork is a material object whose production and distribution coordinate economic activity. Benefits from increased tool demand. Experiences the constraint as the solution to a logistics problem, not as an extraction mechanism.
constraint_indexing:constraint_classification(fork_mechanical_substitution, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: FORK RESISTANCE COMMUNITIES (SCAFFOLD) — Organized agents (clergy, manual laborers, traditional practitioners) maintained fork-free eating practices for centuries as conscious cultural resistance. Saw the constraint as temporary — believed hand-eating practices would persist through deliberate practice transmission. The sunset never occurred; resistance was outcompeted by generational erosion of skill transmission, not by active enforcement. Theater ratio here reflects the performative nature of resistance: maintaining fork-free eating became a explicit marker of status and tradition rather than a functional practice.
constraint_indexing:constraint_classification(fork_mechanical_substitution, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: BIOMECHANICAL NECESSITY (PITON) — From a biomechanical perspective, the claim that forks are mechanically necessary for efficient eating of certain foods (particularly Mediterranean pasta, continental meats) is now treated as self-evident. The fork appears as a natural solution to an objective technical problem. However, the constraint is degraded: the original functional argument (pasta requires a tool) was partially contingent on pasta's adoption itself — Mediterranean cuisine evolved to accommodate the fork, not vice versa. The piton classification reflects that the necessity claim persists through institutional inertia (culinary schools, etiquette manuals) rather than through actual functional testing.
constraint_indexing:constraint_classification(fork_mechanical_substitution, piton,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, human dentition and eating behavior are plastic in response to tool use — the fork constraint appears as an immutable feature of embodied cognition: tools reshape the body that uses them. This is true in principle but obscures the contingency of the fork's adoption itself. The engine will classify this as a false summit: the constraint naturalizes what was a specific institutional choice (elite culture signaling) into a necessity claim.
constraint_indexing:constraint_classification(fork_mechanical_substitution, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fork_mechanical_substitution_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fork_mechanical_substitution, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fork_mechanical_substitution, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(fork_mechanical_substitution, TR),
    TR >= 0.70.

:- end_tests(fork_mechanical_substitution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint exhibits real costs — dental phenotype shifts, manual skill erosion, social exclusion from elite contexts for those without fork competence. But the extraction is not maximal because (1) the fork also solves genuine coordination problems (standardized etiquette reduces dining friction), (2) no single agent monopolizes benefits, (3) early adoption was clearly optional, and (4) the transition was gradual over centuries rather than coercive. The value reflects that extraction accumulated through path-dependence rather than through active enforcement. Suppression (0.42): Moderate. Significant barriers to fork-free eating exist — social stigma, loss of cultural competence, generational loss of skill transmission, elite gatekeeping of dining contexts. But suppression is not total — hand-eating remains technically possible (though costly), and resistance movements persisted for centuries. Barriers are real but not insurmountable. Theater ratio (0.55): Moderate-high. Modern fork use is substantially performative — dining etiquette is the operative extraction mechanism, and much of the fork's necessity is socially constructed rather than biomechanically essential. Early fork adoption was explicitly performative (status signaling); modern naturalization obscures this. The theater has risen over time as the fork's optional status has been forgotten and its necessity has become naturalized.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows how a coordination mechanism (standardizing eating etiquette, enabling efficient consumption of certain foods) can become an extraction mechanism (cultural capital gatekeeping, embodied skill loss, dental phenotype shift) through path-dependence without anyone intentionally extracting. The elite dining culture sees the fork as pure coordination — a solution to the problem of dining etiquette consistency. The fork manufacturing sector sees it as logistics coordination. But the individual eater and the dental phenotype experience extraction: forced reliance on tool, skill atrophy, social exclusion. The resistance communities saw it as optional; the analytical observer risks seeing it as inevitable. The puzzle is: who is the extractor? There is no answer, which reveals that the framework must distinguish between three classes of constraints: (1) extracted constraints with identifiable beneficiary agents, (2) coordination equilibria that lock into place without conscious enforcement, and (3) hybrid cases where coordination and extraction are intertwined and distributed across many small actors making rational choices that aggregate into an extractive outcome.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each perspective depends on the agent's structural relationship to the fork mechanism. Elite dining culture is a clear beneficiary with arbitrage options (they can choose when/where to use forks; other options exist for them) — low d value. Fork manufacturing has arbitrage (can produce alternative tools) — low d. Individual eaters are constrained victims (can in principle exit by learning hand-eating, but social cost is high) — moderate d. Dental phenotype and manual eating capacity are trapped (generational erosion makes reversal difficult) — high d. Resistance communities are organized victims who tried to exit but were outcompeted (high d but with organized power modifier). The analytical observer sees the structure from outside (analytical d ≈ 0.72). The directionality derivation chain maps these exit options and beneficiary/victim status to f(d) values that feed the chi formula. No overrides needed — the structural data sufficiently determines the perspectival gap.
 *
 * MANDATROPHY ANALYSIS:
 *   The fork constraint resolves mandatrophy by revealing that the tension between coordination (rope aspects) and extraction (snare aspects) is real and distributed. The constraint is genuinely a tangled rope — it solves coordination problems (etiquette standardization) while enabling extraction (cultural capital gatekeeping, skill atrophy). The puzzle that motivates mandatrophy analysis is: how can extraction occur without an extractor? The answer is that distributed coordination choices (each elite actor rationally preferring fork-based etiquette for local status signaling) aggregate into a system-level lock-in that extracts from those without cultural competence or embodied skill. No single agent decided to extract; the extraction emerged. This is distinct from snare (where explicit extraction is intended) and from rope (where coordination is the only function). The false summit risk is that analytical observers will naturalize the fork as inevitable (mountain classification), thereby obscuring the contingency of elite status-signaling and the coordination-lock mechanism that trapped later generations. The mandatrophy is resolved by accepting that the constraint is really a tangled rope with no single identifiable beneficiary — the beneficiary is a distributed coalition (elite culture broadly), not a specific monopolist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    beneficiary_identification_paradox,
    'Who explicitly benefited from fork adoption enough to enforce its spread, given that no clear extractive agent (no monopoly, no rent-seeker with enforceable control) can be identified?',
    'Historical analysis of fork promotion mechanisms: which groups actively propagandized fork adoption? Where does the enforcement signal originate? Is the spread driven by elite status competition (distributed coordination problem solving) or by a coordinated campaign?',
    'If no identifiable beneficiary exists: the constraint is a purely emergent coordination equilibrium (shifts toward Rope). If elite status-signaling is the driver: beneficiaries are identifiable and extraction is real (Tangled Rope confirmed). If manufacturing interests actively promoted adoption: extraction was economically motivated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identification_paradox, empirical, 'Identification of beneficiary agents in fork adoption').

omega_variable(
    bite_configuration_causation,
    'Is the observed shift toward overbite in European populations causally driven by fork use (generational habituation to fork-mediated eating reducing bite pressure intensity), or is the correlation spurious (driven by dietary changes, genetic mixing, or other confounds)?',
    'Controlled comparison of bite configuration in populations with differential fork adoption timelines; dental anthropological analysis of archaeological remains before/after fork adoption windows; mechanistic analysis of bite pressure distribution under fork-mediated vs hand-mediated eating',
    'If causal: the constraint is real and extractiveness ≥0.38 is conservative (the extraction includes permanent phenotypic change). If spurious: extractiveness should be lower (~0.20) and the constraint reclassifies toward Rope (coordination without real cost).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(bite_configuration_causation, empirical, 'Causal linkage between fork use and overbite development').

omega_variable(
    path_naturalization_without_agent,
    'Can a technological constraint achieve path-dependence and naturalization (appearing as inevitable) without an identifiable agent enforcing or profiting from the lock-in?',
    'Comparative historical analysis of technology adoption with vs. without clear beneficiary agents; game-theoretic modeling of coordination equilibrium stability under distributed vs. centralized enforcement',
    'If yes: the framework must distinguish between extracted constraints and coordination-lock constraints that appear extractive (high suppression, reduced alternatives) but have no extractor. Classification implications: shifts toward Rope or Scaffold depending on perceived reversibility. If no: beneficiary identification failures suggest analytical error rather than structural ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(path_naturalization_without_agent, conceptual, 'Whether path naturalization requires identifiable beneficiary agent').

omega_variable(
    embodied_skill_erosion_reversibility,
    'Are the attentional and motor skills required for fork-free eating truly lost through generations of disuse, or merely suppressed by social convention? Would intensive retraining recover the capacity?',
    'Ethnographic observation of individuals relearning fork-free eating in fork-dominant cultures (contemporary hand-eating in high-fork environments); developmental psychology of motor skill acquisition timing windows; comparison to language attrition patterns',
    'If truly lost: suppression value ≥0.50 and classification as Snare justified. If merely suppressed: suppression should be lower (~0.30) and exit_options should be mobile not trapped (retraining is possible but costly). Shapes the irreversibility claim central to the constraint''s extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(embodied_skill_erosion_reversibility, empirical, 'Reversibility of motor skill loss in fork-mediated eating').

omega_variable(
    fork_adoption_speed_anomaly,
    'Why did fork adoption spread rapidly in elite contexts (< 2 centuries for Italian urban elites) but took 400+ years to reach non-elite European populations? Was spread rate constrained by material scarcity, by active resistance, or by elite gatekeeping of status symbol?',
    'Historical analysis of fork production capacity, pricing trajectories, and wealth concentration; comparative adoption curves (forks vs. other luxury goods); documentation of explicit resistance narratives',
    'If constrained by scarcity: extraction was incidental (Rope). If by active gatekeeping: extraction was deliberate (Snare or Tangled Rope). If by rational resistance: beneficiary awareness existed (victims knew they were being extracted from).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fork_adoption_speed_anomaly, empirical, 'Explanation for temporal asymmetry in fork adoption across social classes').

omega_variable(
    cultural_competence_as_extraction,
    'Is the requirement to learn fork etiquette a form of cultural capital gatekeeping (extraction mechanism) or a genuine coordination mechanism enabling social participation?',
    'Social network analysis of exclusion mechanisms tied to dining incompetence; economic mobility analysis (does fork etiquette competence affect access to economic opportunity?); ethnographic documentation of social consequence severity',
    'If extraction: beneficiary is the elite group controlling access (shifts classification toward Snare). If coordination: beneficiary is the community gaining lower communication friction (Rope). The distinction determines whether the constraint is maintaining asymmetry (extraction) or solving a coordination problem (cooperation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_competence_as_extraction, empirical, 'Whether fork etiquette operates as cultural capital gatekeeping').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fork_mechanical_substitution, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fork_tr_t0, fork_mechanical_substitution, theater_ratio, 0, 0.35).
narrative_ontology:measurement(fork_tr_t3, fork_mechanical_substitution, theater_ratio, 3, 0.42).
narrative_ontology:measurement(fork_tr_t6, fork_mechanical_substitution, theater_ratio, 6, 0.5).
narrative_ontology:measurement(fork_tr_t9, fork_mechanical_substitution, theater_ratio, 9, 0.55).

% Extraction over time
narrative_ontology:measurement(fork_be_t0, fork_mechanical_substitution, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(fork_be_t3, fork_mechanical_substitution, base_extractiveness, 3, 0.22).
narrative_ontology:measurement(fork_be_t6, fork_mechanical_substitution, base_extractiveness, 6, 0.32).
narrative_ontology:measurement(fork_be_t9, fork_mechanical_substitution, base_extractiveness, 9, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(fork_su_t0, fork_mechanical_substitution, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(fork_su_t3, fork_mechanical_substitution, suppression_requirement, 3, 0.3).
narrative_ontology:measurement(fork_su_t6, fork_mechanical_substitution, suppression_requirement, 6, 0.38).
narrative_ontology:measurement(fork_su_t9, fork_mechanical_substitution, suppression_requirement, 9, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fork_mechanical_substitution, attachment_coordination).
narrative_ontology:affects_constraint(fork_mechanical_substitution, table_manners_status_gatekeeping).
narrative_ontology:affects_constraint(fork_mechanical_substitution, medieval_hand_food_competence).

% DUAL FORMULATION NOTE:
% Fork adoption as a material technology (fork_mechanical_substitution) must be distinguished from fork adoption as a coordination mechanism (table etiquette standardization). The mechanical constraint (how the fork displaces manual eating) has ε≈0.38; the social constraint (how fork etiquette becomes mandatory for status participation) has higher ε and different beneficiary structure. Both stories share the same interval and measurement period but have different classification profiles. The mechanical constraint is upstream; the social constraint is downstream and depends on generational habituation to create suppression.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
