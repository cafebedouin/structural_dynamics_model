% ============================================================================
% CONSTRAINT STORY: salic_law_succession_kernel
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_salic_law_succession_kernel, []).

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
 *   constraint_id: salic_law_succession_kernel
 *   human_readable: Salic Law as Anchored Succession Kernel
 *   domain: french_history/succession_principle
 *
 * SUMMARY:
 *   The Salic Law's male-line succession principle served as an anchored
 *   kernel within the French monarchy's legitimacy framework from the 14th
 *   century onward. Rather than emerging as a contingent institutional
 *   choice, the principle was progressively reframed as a fundamental
 *   constitutional law — a natural rule of legitimate succession that could
 *   not be revised without dissolving the entire basis of royal authority.
 *   This constraint exhibits remarkable structural stability across six
 *   distinct classification types depending on the observer's position: it
 *   appears as immutable natural law to the monarchy (mountain), as pure
 *   extraction to excluded female heirs (snare), as coordination mechanism to
 *   beneficiary elites (rope), as degraded theater to the church (piton), as
 *   mixed coordination-extraction to regional competitors (tangled rope), and
 *   as a temporary institutional choice to post-crisis legal reformers
 *   (scaffold). The constraint demonstrates how anchored kernel elements
 *   within outer containers can persist and even strengthen during periods
 *   when the overall container's bandwidth is degraded — the principle's
 *   rigidity may have prevented adaptive responses to the Hundred Years' War
 *   and succession crises, yet its anchoring prevented renegotiation even
 *   when flexibility might have stabilized the dynasty. The theater ratio's
 *   rise from 0.45 to 0.68 over the 350-year interval reflects either genuine
 *   degradation of the principle's functional necessity or increasing
 *   ceremonial emphasis to compensate for weakening acceptance — the omega
 *   variable cannot resolve this ambiguity from available evidence alone.
 *
 * KEY AGENTS:
 *   - Female Heirs and Matrilineal Claimants: Primary victims (powerless/trapped) — Isabella of Valois, Joan of Arc, daughters of Louis X and successors. Structurally excluded by the principle with no exit mechanism or recourse.
 *   - Male-Line Heirs and Agnatic Elites: Primary beneficiaries (institutional/arbitrage) — benefit from clarity in succession; experience the principle as coordination, not extraction. Can arbitrage between loyalty and alternative arrangements.
 *   - Regional Noble Houses with Female-Line Claims: Secondary victims (moderate/constrained) — Burgundy, Brittany, Plantagenet claimants. Benefit when aligning with male-line winners; suffer extraction when their female-line claims are nullified.
 *   - Royal Legitimacy Framework: Institutional anchor (institutional/analytical) — the principle is embedded in the core legitimacy structure; revision would dissolve royal authority. Cannot exit without reconstructing the entire framework.
 *   - Church and Religious Authority: Institutional legitimation (institutional/analytical) — provides ceremonial sanction and theological framing; role becomes increasingly performative as underlying property principle gains clarity.
 *   - Competing Monarchies (England, Castile, Aragon): External actors (powerful/mobile) — powerful enough to contest the principle through military challenge; mobile enough to abandon the contest or accept alternative arrangements. Experience tangled coordination-extraction.
 *   - Legal and Political Theorists (17th-18th centuries): Organized reformers (organized/constrained) — begin analyzing the principle as contingent rather than natural law; constrained by the legitimacy framework but able to envision alternatives.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_law_succession_kernel, 0.68).
domain_priors:suppression_score(salic_law_succession_kernel, 0.75).
domain_priors:theater_ratio(salic_law_succession_kernel, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_law_succession_kernel, extractiveness, 0.68).
narrative_ontology:constraint_metric(salic_law_succession_kernel, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(salic_law_succession_kernel, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_law_succession_kernel, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(salic_law_succession_kernel, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_law_succession_kernel, tangled_rope).
narrative_ontology:human_readable(salic_law_succession_kernel, "Salic Law as Anchored Succession Kernel").
narrative_ontology:topic_domain(salic_law_succession_kernel, "french_history/succession_principle").

domain_priors:requires_active_enforcement(salic_law_succession_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_law_succession_kernel, male_line_heirs).
narrative_ontology:constraint_beneficiary(salic_law_succession_kernel, royal_legitimacy_framework).
narrative_ontology:constraint_beneficiary(salic_law_succession_kernel, feudal_elite_with_agnatic_interests).
narrative_ontology:constraint_victim(salic_law_succession_kernel, female_heirs_and_their_lines).
narrative_ontology:constraint_victim(salic_law_succession_kernel, matrilineal_succession_claimants).
narrative_ontology:constraint_victim(salic_law_succession_kernel, alternative_succession_configurations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FEMALE HEIRS (SNARE) — Structurally excluded from legitimate succession regardless of actual power or proximity to throne. Isabella of Valois, Joan of Arc, the daughters of Louis X — all trapped by the principle. No mechanism for exit or renegotiation. Maximum suppression: the exclusion is framed as natural law, making even conceptual challenge to the principle appear illegitimate. The extraction is of political agency and dynastic voice.
constraint_indexing:constraint_classification(salic_law_succession_kernel, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REGIONAL NOBLES WITH FEMALE CLAIMS (TANGLED ROPE) — Moderate power but constrained exit. The Salic principle coordinated succession transitions by removing ambiguity in a multi-claimant feudal system. These nobles benefit from the clarity when they align with male-line winners (Burgundy, Brittany securing marriages to male heirs), but suffer extraction when their female-line claims are nullified (like the Plantagenet claim through Isabella). They can organize or contest, but the legitimacy framework's anchoring makes their challenges costly.
constraint_indexing:constraint_classification(salic_law_succession_kernel, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ROYAL SUCCESSION ELITES (ROPE) — Institutional beneficiaries with arbitrage exit. For the male-line heir or the feudal elite backing agnatic succession, the Salic principle is pure coordination: it solves the multi-claimant problem by excluding half the potential claimants on principle. These agents experience the constraint as beneficial coordination, not extraction. They can arbitrage between loyalty to the principle and acceptance of alternative arrangements if bandwidth permits, but the principle's anchoring prevents such arbitrage during crises.
constraint_indexing:constraint_classification(salic_law_succession_kernel, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ROYAL LEGITIMACY FRAMEWORK (MOUNTAIN) — From the monarchy's embedded perspective, Salic succession is treated as a fundamental law of nature, not a contingent rule. It appears immutable because revising it would dissolve the entire legitimacy framework — the king's authority rests on inherited right through the male line, so questioning the male-line principle is tantamount to questioning royal authority itself. This creates a mountain-like accessibility collapse: even when revision might have solved succession crises (as with Joan of Arc's claims or the English Plantagenet challenges), the framework cannot be revised without collapsing itself. Theater ratio is moderate (0.55) because the principle requires continuous citation and ceremonial reinforcement to maintain its naturalized status.
constraint_indexing:constraint_classification(salic_law_succession_kernel, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

% PERSPECTIVE 5: CHURCH LEGITIMATION APPARATUS (PITON) — The church's role in sanctifying Salic succession is increasingly performative by the late medieval period. Religious authority is invoked to naturalize what is fundamentally a feudal property principle — male-line inheritance follows property law, not theological necessity. The church's ceremonial role persists through institutional inertia even as the theological foundation weakens. The high theater ratio (0.55 average, rising to 0.68 by the 15th century) reflects this degradation: coronation ceremonies invoke divine sanction, but the underlying mechanism is customary and political, not divinely ordained.
constraint_indexing:constraint_classification(salic_law_succession_kernel, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

% PERSPECTIVE 6: COMPETING MONARCHIES (TANGLED ROPE) — From the perspective of England (Plantagenet claims through Isabella), Castile, Aragon, and other succession competitors, the Salic principle functions as both coordination and extraction. It coordinates succession within France (benefits English kings who accept the principle and inherit through male line), but extracts legitimacy from those who challenge it through female lines. These actors are powerful with mobile exit options — they can abandon the contest (Plantagenet renunciation after Hundred Years' War), accept alternative arrangements, or maintain claims. The principle's enforcement against them is an extractive mechanism, but acceptance of the principle (when beneficial) coordinates otherwise chaotic multi-kingdom succession dynamics.
constraint_indexing:constraint_classification(salic_law_succession_kernel, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 7: LEGAL REFORMERS (SCAFFOLD) — By the 17th-18th centuries, legal and political theorists (Bodin, Jurieu, later Enlightenment thinkers) begin analyzing Salic succession as a contingent rule rather than natural law. These organized actors have constrained exit but can envision alternative arrangements. However, the principle persists not because it functions well but because revising it would require reconstructing royal legitimacy from scratch. The scaffold perspective sees the constraint as temporary — alternative succession frameworks (electoral monarchy, constitutional succession) exist and could replace Salic law if the legitimacy framework could be rebuilt. But the anchoring prevents such reconstruction until the entire monarchical system degrades (as happens in 1789).
constraint_indexing:constraint_classification(salic_law_succession_kernel, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational/universal analytical position, one might argue that Salic succession reflects immutable structures of patrilineal property transmission in feudal systems — it's natural law for how feudal inheritance works. However, the structural data (beneficiaries, victims, enforcement costs) contradicts this. The principle required continuous active enforcement, religious legitimation, and exclusion of empirically viable alternatives. This is a false summit: the 'natural law' framing naturalizes a contingent institutional choice that benefited a specific class (agnatic heirs and their supporting elites). The analytical engine will flag this as FSM candidate because beneficiaries are explicitly declared.
constraint_indexing:constraint_classification(salic_law_succession_kernel, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(salic_law_succession_kernel_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(salic_law_succession_kernel, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(salic_law_succession_kernel, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(salic_law_succession_kernel, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(salic_law_succession_kernel, TR),
    TR >= 0.70.

:- end_tests(salic_law_succession_kernel_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and rising. The principle extracts political agency and dynastic voice from female heirs and their supporters. The extraction increases over time (0.45 → 0.68) because the principle becomes more rigidly enforced and more explicitly defended during succession crises — exactly when flexibility might have been adaptive. The Hundred Years' War and the complex succession disputes (especially around Joan of Arc) show that the principle's enforcement becomes more costly and more clearly extractive as alternatives present themselves. Suppression (0.75): Very high. The principle operates through layered suppression mechanisms: legal exclusion by customary right, religious legitimation framing the exclusion as divinely ordained, social stigmatization of female succession claims (as seen in attacks on Joan), institutional reinforcement through ceremony and citation, and intellectual suppression of alternative frameworks. The alternatives are not merely difficult to implement; they are framed as illegitimate even to contemplate. Theater ratio (0.55, rising to 0.68): Moderate and increasing. The principle requires continuous ceremonial reinforcement (coronations, legitimacy validations, religious sanction) to maintain its naturalized status. The rising theater ratio suggests that as the principle's contingency became more apparent (through succession crises, comparative observations of other monarchies' flexible approaches), more theatrical emphasis was required to maintain its perceived naturalness. By the 15th century (time_point 150+), the theater becomes dominant: the underlying coordination function (solving multi-claimant succession problems) could have been served by alternative rules, but the specific Salic rule persists through institutional inertia and ceremonial maintenance rather than through functional necessity.
 *
 * PERSPECTIVAL GAP:
 *   The perspectives diverge sharply on whether the constraint is fundamental or contingent. The royal legitimacy framework (mountain) insists the principle is immutable — revising it would collapse royal authority itself. Female heirs (snare) experience it as arbitrary external force with no mechanism for negotiation. Beneficiary elites (rope) experience it as beneficial coordination. Regional competitors (tangled rope) experience both coordination and extraction depending on whether their male-line connection aligns with the principle. Legal reformers (scaffold) begin to see it as a temporary institutional choice that could be replaced. The analytical observer (mountain, false summit candidate) risks naturalizing the contingent choice as immutable law. These gaps reveal that the principle's apparent immutability is not structural necessity but rather a self-reinforcing institutional choice: the legitimacy framework depends on the male-line rule because the framework was constructed around it, and questioning the rule threatens to unravel the entire legitimacy structure. This is fundamentally different from a genuine natural law, which would be immutable regardless of the framework built upon it.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's structural position relative to extraction flow. Female heirs and matrilineal claimants occupy d ≈ 0.95 (full targets): they are victims of the principle with no arbitrage exit and no counter-extraction mechanism. Beneficiary elites occupy d ≈ 0.10 (near-full beneficiaries): they benefit from the principle's clarity and have arbitrage options. Regional nobles with female claims occupy d ≈ 0.60 (mixed): they benefit from clarity when aligned with male-line inheritors but suffer extraction when their female-line claims are excluded. Competing monarchies occupy d ≈ 0.65 (net targets in enforcement but powerful actors): they experience the principle as an extractive barrier to their succession claims but have power and mobility to resist or abandon the contest. The royal legitimacy framework and church occupy d ≈ 0.25 (embedded beneficiaries): they benefit from the principle's stability and coordination function. The directionality overrides are not necessary here — the automatic derivation from beneficiary/victim declarations produces accurate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The Salic succession kernel resolves mandatrophy through temporal analysis and comparative institutional perspective. The principle is neither pure coordination nor pure extraction but rather ANCHORED EXTRACTION masquerading as coordination. Here is the structure: (1) GENUINE COORDINATION FUNCTION: The male-line principle does solve the multi-claimant succession problem by excluding half the potential claimants on principle. This is real coordination value. (2) EMBEDDED EXTRACTION: The principle also extracts political voice and legitimacy from female heirs and their supporters, with no compensation or exit mechanism. (3) ANCHORING MECHANISM: The principle cannot be revised because the legitimacy framework is built around it. Revision would require reconstructing royal authority from scratch, which is bandwidth-prohibitive during normal periods and possibly impossible without external shock (as occurred in 1789). (4) DEGRADATION SIGNAL: The rising theater ratio and extractiveness metrics indicate that the principle's functional necessity is declining while its enforcement costs rise. By the 17th-18th centuries, alternative coordination rules (elective monarchy, constitutional succession) had been demonstrated in other European contexts. The persistence of Salic succession reflects institutional inertia and anchoring rather than functional necessity. (5) MANDATROPHY RESOLVED: The constraint is a TANGLED ROPE because it genuinely coordinates succession transitions (coordination function present, beneficiaries explicitly benefit, elites experience it as solving a real problem) while simultaneously extracting legitimacy from excluded agents (victims explicitly exist with no exit, extraction is enforced through suppression). The classification holds from multiple perspectives without contradiction. The false summit attempt (mountain perspective) fails because beneficiaries are declared and the principle is not emergent-naturally but rather enforced through active institutional maintenance (requires_active_enforcement: true). The piton perspective (degraded theater) is secondary — it applies to the church's role in legitimation, not to the principle itself. The snare perspective (female heirs) is real but not primary — it's one pole of the tangled rope's asymmetry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_contingent_rule,
    'Is Salic succession a natural law of feudal property transmission, or a contingent institutional rule that benefits identifiable agents?',
    'Comparative analysis: examine succession systems in other feudal territories (HRE, Castile, Naples) to identify whether male-line exclusivity is universal or regional. If female-line succession worked in other contexts without destabilizing legitimacy, the principle is contingent. If universal, it reflects structural properties of feudalism.',
    'If contingent: false summit classification confirmed; the principle naturalizes what is fundamentally a political choice. If structural: mountain classification holds; the principle reflects feudal property law constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_contingent_rule, empirical, 'Whether Salic succession is natural law or contingent institutional rule').

omega_variable(
    legitimacy_reconstruction_possibility,
    'Could the French monarchy have survived and maintained legitimacy if Salic succession were revised to permit female heirs?',
    'Historical counterfactual analysis informed by comparative case studies (Portugal''s succession through female line in 15th century; later Polish elective monarchy). Examine whether legitimacy depends on the specific rule or on the framework''s perceived antiquity and religious sanction.',
    'If yes: the anchoring is a constraint choice, not a necessity; the principle persists because revising it is costly, not because revision is impossible. If no: the legitimacy framework is genuinely dependent on the specific Salic rule, making the constraint less voluntary. Mandatrophy analysis changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_reconstruction_possibility, conceptual, 'Whether female-line succession could maintain royal legitimacy').

omega_variable(
    suppression_internalization_degree,
    'To what degree did female heirs and their supporters internalize the Salic principle as legitimate rather than experiencing it purely as external suppression?',
    'Textual analysis of claims by matrilineal claimants (Isabella''s partisans, Joan of Arc''s faction). Look for evidence of: (a) explicit challenge to the principle itself vs. (b) acceptance of the principle while contesting its application; (c) internalized shame about female claim vs. (d) angry rejection of exclusion.',
    'If high internalization: suppression metric understates the constraint''s binding force. If low: victims clearly perceived the constraint as illegitimate, potentially signaling future instability. Affects interpretation of theater_ratio and long-term sustainability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_degree, empirical, 'Degree of internalization of Salic principle by excluded claimants').

omega_variable(
    outer_container_bandwidth_degradation,
    'To what degree did the Salic kernel''s rigid anchoring prevent bandwidth-adaptive responses during succession crises (particularly the Hundred Years'' War)?',
    'Institutional analysis of missed opportunities: moments when revising or relaxing the succession principle might have prevented wars, alliances, or legitimacy crises. Compare outcomes where the principle was enforced rigidly vs. moments of flexible application.',
    'If high: the kernel''s rigidity degraded outer-container resilience; the constraint actively harmed system survival. If low: the principle''s anchoring had no countervailing cost; it stabilized succession transitions overall. Affects mandatrophy analysis of the constraint''s net contribution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(outer_container_bandwidth_degradation, empirical, 'Impact of succession kernel rigidity on outer-container resilience during crises').

omega_variable(
    theater_ratio_growth_mechanism,
    'Does the rising theater ratio (0.45 → 0.68) reflect genuine degradation of the principle''s functional necessity, or increased ceremonial emphasis to compensate for weakening acceptance?',
    'Analysis of coronation, succession validation, and legitimation ceremonies over time. Compare: (a) frequency of explicit Salic citations in official documents; (b) complexity of legitimation rituals; (c) evidence of contestation requiring stronger justification.',
    'If degradation: the constraint is losing structural necessity; it persists through inertia. If compensation: the constraint remains functionally necessary but requires increasing theater to maintain acceptance. Affects piton vs. tangled_rope classification long-term.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_growth_mechanism, empirical, 'Whether rising theater reflects degradation or compensatory emphasis').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_law_succession_kernel, 0, 350).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(salic_theater_t0, salic_law_succession_kernel, theater_ratio, 0, 0.45).
narrative_ontology:measurement(salic_theater_t150, salic_law_succession_kernel, theater_ratio, 150, 0.58).
narrative_ontology:measurement(salic_theater_t350, salic_law_succession_kernel, theater_ratio, 350, 0.68).
narrative_ontology:measurement(salic_theater_t100, salic_law_succession_kernel, theater_ratio, 100, 0.52).
narrative_ontology:measurement(salic_theater_t250, salic_law_succession_kernel, theater_ratio, 250, 0.64).

% Extraction over time
narrative_ontology:measurement(salic_extract_t0, salic_law_succession_kernel, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(salic_extract_t150, salic_law_succession_kernel, base_extractiveness, 150, 0.62).
narrative_ontology:measurement(salic_extract_t350, salic_law_succession_kernel, base_extractiveness, 350, 0.68).
narrative_ontology:measurement(salic_extract_t100, salic_law_succession_kernel, base_extractiveness, 100, 0.54).
narrative_ontology:measurement(salic_extract_t250, salic_law_succession_kernel, base_extractiveness, 250, 0.66).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(salic_law_succession_kernel, resource_allocation).
narrative_ontology:affects_constraint(salic_law_succession_kernel, hundred_years_war_legitimacy_crisis).
narrative_ontology:affects_constraint(salic_law_succession_kernel, female_religious_authority_exclusion).
narrative_ontology:affects_constraint(salic_law_succession_kernel, inherited_property_law_agnatic_default).

% DUAL FORMULATION NOTE:
% The Salic succession principle operates at multiple structural levels: (1) as a property law rule (male-line inheritance of feudal holdings — see inherited_property_law_agnatic_default); (2) as a legitimacy principle for kingship specifically; (3) as a coordination mechanism for succession transitions; (4) as a suppression mechanism excluding female agency. These are distinct constraints sharing a common label. The present story focuses on (2) and (3). The property law story (1) has lower extractiveness (≈0.35, Rope or Tangled Rope) because property transmission follows clearer functional rules. The female religious authority story (4) has higher extractiveness (≈0.72, Snare) because religious authority exclusion lacks the 'coordination' justification available for succession rules. Linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
