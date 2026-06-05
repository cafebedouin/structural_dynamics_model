% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_authority__popular_constitutionalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_authority__popular_constitutionalism_reading, []).

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
 *   constraint_id: basic_law_interpretive_authority__popular_constitutionalism_reading
 *   human_readable: Constitutional Meaning via Perpetual Democratic Contestation (Popular Constitutionalism Reading)
 *   domain: constitutional_law/political_theory/institutional_design
 *
 * SUMMARY:
 *   The popular constitutionalism reading of basic law interpretive authority
 *   locates constitutional meaning in ongoing democratic contestation rather
 *   than terminal institutional adjudication by courts or legislatures. Under
 *   this reading, the Constitution is not a fixed text whose meaning is
 *   discovered (or correctly applied) by judicial expertise, but a living
 *   commitment that continuously re-emerges through the political struggle of
 *   constituencies, movements, and subordinate institutional sites.
 *   Constitutional meaning is not settled by Supreme Court pronouncements —
 *   those pronouncements are instead one voice in a perpetual conversation
 *   that includes state legislatures, municipalities, executive agencies,
 *   social movements, and the broader public. This reading privileges the
 *   dispersal of interpretive authority over institutional gatekeeping,
 *   treating the constant contestation and renegotiation of constitutional
 *   meaning as a feature of democratic life, not a bug to be eliminated by
 *   assigning final interpretive power to a single institution. The
 *   constraint exhibits the structural signature of a tangled rope: it
 *   coordinates genuine democratic input and decentralizes interpretive power
 *   (coordination benefit), while simultaneously extracting institutional
 *   stability and legal predictability, concentrating those costs on the
 *   powerless who cannot navigate perpetual constitutional uncertainty
 *   (asymmetric extraction cost). The theater ratio (0.65) reflects that
 *   significant performance work is required to maintain the appearance of
 *   constitutional rule of law while the reading denies any institutional
 *   site the authority to finally settle what the Constitution means.
 *
 * KEY AGENTS:
 *   - Decentralized Democratic Constituencies: Primary beneficiary (organized/constrained) — empowered to reshape constitutional meaning through mobilization and pressure; participate in meaning-making rather than receiving top-down interpretation
 *   - Social Movements and Advocacy Groups: Secondary beneficiary (organized/constrained) — can leverage the reading's framework to push constitutional interpretation in new directions; benefit from the denial of judicial monopoly on meaning
 *   - Subordinate Institutional Sites: Tertiary beneficiary (institutional/mobile) — courts, legislatures, agencies at state/local levels gain interpretive legitimacy and authority; can contribute to constitutional meaning without being overridden by the Supreme Court
 *   - The Supreme Court: Primary victim (institutional/constrained) — experiences erosion of its interpretive legitimacy; must constantly defend its authority against popular contestation; performance burden increases over time
 *   - Ordinary Citizens: Secondary victim (powerless/trapped) — face institutional gridlock, legal uncertainty, and delayed vindication of constitutional rights; cannot opt out of perpetual constitutional struggle; bear differential costs depending on access to legal/political resources
 *   - Legal Predictability and Institutional Clarity: Tertiary victim (powerless/trapped) — abstract collective goods that are sacrificed; constitutional meaning becomes fluid and contestable; destabilizes lower-court decision-making and regulation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.58).
domain_priors:suppression_score(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.52).
domain_priors:theater_ratio(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_authority__popular_constitutionalism_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_authority__popular_constitutionalism_reading, "Constitutional Meaning via Perpetual Democratic Contestation (Popular Constitutionalism Reading)").
narrative_ontology:topic_domain(basic_law_interpretive_authority__popular_constitutionalism_reading, "constitutional_law/political_theory/institutional_design").

domain_priors:requires_active_enforcement(basic_law_interpretive_authority__popular_constitutionalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_authority__popular_constitutionalism_reading, 'de504c2b-171b-4cc1-ab56-81da5b96ec4f').
narrative_ontology:cs_kernel_codification('de504c2b-171b-4cc1-ab56-81da5b96ec4f', formalized).
narrative_ontology:cs_authority_grounding('de504c2b-171b-4cc1-ab56-81da5b96ec4f', distributed).
narrative_ontology:cs_reading_relation('de504c2b-171b-4cc1-ab56-81da5b96ec4f', basic_law_interpretive_authority__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('de504c2b-171b-4cc1-ab56-81da5b96ec4f', basic_law_interpretive_authority__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('de504c2b-171b-4cc1-ab56-81da5b96ec4f', foundational, perpetual_contestation_normative).
narrative_ontology:cs_axiom_status(perpetual_contestation_normative, holdable).
narrative_ontology:cs_axiom_grounding('de504c2b-171b-4cc1-ab56-81da5b96ec4f', perpetual_contestation_normative, deontological).
narrative_ontology:cs_axiom('de504c2b-171b-4cc1-ab56-81da5b96ec4f', foundational, distributed_authority_legitimacy).
narrative_ontology:cs_axiom_status(distributed_authority_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('de504c2b-171b-4cc1-ab56-81da5b96ec4f', distributed_authority_legitimacy, deontological).
narrative_ontology:cs_reference_frame('de504c2b-171b-4cc1-ab56-81da5b96ec4f', constitutional_authority_as_democratic_trusteeship).
narrative_ontology:cs_drift_state('de504c2b-171b-4cc1-ab56-81da5b96ec4f', contemporary_polarization_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('de504c2b-171b-4cc1-ab56-81da5b96ec4f', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_authority__popular_constitutionalism_reading, basic_law_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__popular_constitutionalism_reading, decentralized_democratic_constituencies).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__popular_constitutionalism_reading, social_movements).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__popular_constitutionalism_reading, subordinate_institutional_sites).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, judicial_legitimacy_claim).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, institutional_decisional_clarity).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, legal_predictability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ORDINARY CITIZEN (SNARE) — Trapped in endless constitutional disputes with no terminal resolution. Cannot exit the interpretive struggle; bears full cost of institutional gridlock and strategic mobilization. Constitutional meaning perpetually unstable beneath their feet; no stable law to rely on.
constraint_indexing:constraint_classification(basic_law_interpretive_authority__popular_constitutionalism_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SOCIAL MOVEMENTS & CONSTITUENCIES (TANGLED ROPE) — Both benefits from and constrained by the perpetual contestation framework. Mobilized groups can reshape constitutional meaning through collective action and institutional pressure (coordination benefit), but must continuously fight to maintain interpretive gains against countermobilization. Extraction runs both ways: the framework empowers some movements while sapping resources from all.
constraint_indexing:constraint_classification(basic_law_interpretive_authority__popular_constitutionalism_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SUBORDINATE INSTITUTIONAL SITES (ROPE) — Municipal courts, state legislatures, regulatory agencies, and local venues experience perpetual contestation as a coordination mechanism: they are empowered to contribute to constitutional meaning-making rather than receiving top-down directives. Exit option is mobile because institutions can shift interpretive positions strategically. Coordination function is genuine — the framework distributes authority and legitimacy across sites.
constraint_indexing:constraint_classification(basic_law_interpretive_authority__popular_constitutionalism_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: SUPREME COURT (SNARE) — Constrained by the delegitimizing effect of popular constitutionalism. If the Court's interpretations can be overridden or reinterpreted by democratic constituencies, its institutional authority erodes. The Court experiences the constraint as extractive: it must constantly defend its interpretive authority against challenges from below, burning institutional capital in the process. Theater requirement: the Court must perform its judicial supremacy even as the reading denies it.
constraint_indexing:constraint_classification(basic_law_interpretive_authority__popular_constitutionalism_reading, snare,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: THE JUDICIARY (TANGLED ROPE) — From a longer time horizon, the judiciary benefits from the popular constitutionalism framework by retaining interpretive legitimacy through responsiveness to democratic sentiment, while also experiencing constraint. The Court has arbitrage options — it can lead or follow democratic movements strategically — but it cannot opt out of the legitimacy struggle. The constraint is hybrid: genuine coordination function (Court and constituencies jointly produce constitutional meaning) plus asymmetric extraction (Court's interpretive authority is always provisional).
constraint_indexing:constraint_classification(basic_law_interpretive_authority__popular_constitutionalism_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SCAFFOLD) — From the analytical perspective, popular constitutionalism is a transitional institutional design. It resolves the crisis of judicial supremacy (which concentrates too much interpretive authority) by distributing meaning-making across constituencies. But this distribution creates gridlock and instability. The sunset condition is implicit: as democratic institutions mature and produce stable interpretive coalitions, the perpetual contestation model may yield to a more stable pluralist framework where multiple institutional sites have recognized, non-contestable domains of constitutional authority.
constraint_indexing:constraint_classification(basic_law_interpretive_authority__popular_constitutionalism_reading, scaffold,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(national))).

% PERSPECTIVE 7: VESTIGIAL JUDICIAL SUPREMACY (PITON) — Traditional doctrine that the Supreme Court has final say on constitutional meaning persists through institutional inertia even as the popular constitutionalism reading actively undermines it. Courts continue performing the ritual of issuing binding interpretations, but the legitimacy behind that performance has eroded. Theater ratio is high because the form (judicial pronouncement) persists while the substance (binding finality) is contested. This is a degraded natural law claim — courts used to have terminal authority, but that authority is now performative.
constraint_indexing:constraint_classification(basic_law_interpretive_authority__popular_constitutionalism_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_authority__popular_constitutionalism_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(basic_law_interpretive_authority__popular_constitutionalism_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(basic_law_interpretive_authority__popular_constitutionalism_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_authority__popular_constitutionalism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(basic_law_interpretive_authority__popular_constitutionalism_reading, TR),
    TR >= 0.70.

:- end_tests(basic_law_interpretive_authority__popular_constitutionalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The reading produces genuine coordination benefits (decentralized input, democratic accountability, no institutional monopoly on meaning) but these are accompanied by asymmetric extraction of institutional clarity and legal stability. The benefits accrue to organized constituencies with mobilization capacity; the costs accrue to those without such capacity. The extractiveness value captures that while this is not pure extraction (Snare, which would be ~0.75+), it is not pure coordination (Rope, which would be ~0.30 or lower) — the constraint requires active enforcement (judicial performance of its role despite delegitimization) and produces suppression. Suppression (0.52): Moderate. Not as high as a pure snare because the reading does not depend on violently suppressing alternatives — it is a live interpretive position held by scholars, activists, and some judges. But suppression is real: the reading requires suppressing the alternative claim that one institutional site should have terminal authority, and it requires citizens and courts to tolerate the suppressiveness of perpetual contestation itself. The measurement trajectory (0.38 → 0.52) shows that as the reading gains institutional traction, suppression requirements increase — the judiciary must work harder to justify why its pronouncements are binding even as those pronouncements are being perpetually contested. Theater ratio (0.65): High and rising. Constitutional rule of law requires the appearance of law — binding pronouncements, predictable doctrine, institutional authority. But the reading denies any institutional site the legitimacy to provide that appearance credibly. The result is performative judging: courts must issue decisions as if they are binding while acknowledging they can be reinterpreted by democratic constituencies. This theater has increased over the interval as the reading has become more salient in constitutional discourse and the gap between judicial performance (issuing decisions) and the reading's implications (those decisions are contestable) has widened.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival range here is exceptionally wide. The powerless citizen experiences pure extraction (Snare) — they are trapped in an unstable constitutional landscape with no exit. Organized movements experience mixed coordination and extraction (Tangled Rope) — they gain power to reshape meaning but also bear the costs of perpetual struggle. Subordinate institutions experience coordination (Rope) — they are genuinely empowered to contribute to constitutional interpretation. The Supreme Court experiences extraction (Snare) — its authority is eroded and it must perform its legitimacy against a reading that denies it. The judiciary with a longer time horizon experiences mixed coordination and extraction (Tangled Rope) — it retains interpretive leadership capacity but only through responsiveness to democratic pressure. The analytical observer with a full institutional view experiences this as a transitional design (Scaffold) — useful for dismantling judicial supremacy but requiring sunset into a more stable institutional arrangement. The perspectival gap reveals that the reading's benefits are concentrated on organized institutional sites and movements, while its costs are concentrated on the powerless and on the institutional clarity that all depend on.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation for this constraint runs through beneficiary/victim declarations and exit option differentiation. Beneficiaries (decentralized constituencies, subordinate sites) have mobile or arbitrage exit options — they can invest or withdraw from constitutional contestation strategically, and they benefit from the reading's framework. High-power, high-exit agents derive lower d values (toward 0.0), reducing their experienced extractiveness chi. Victims (ordinary citizens, judicial legitimacy, predictability) are trapped or constrained — they cannot exit the constitutional framework, and they bear costs. Lower-power, lower-exit agents derive higher d values (toward 1.0), amplifying their experienced extractiveness chi. The Supreme Court as an institutional actor with constrained exit (it cannot opt out of being the Court) and facing delegitimization derives a moderately-high d. This differentiation produces the perspectival gap: same constraint, different χ values, different experienced classifications by different agents.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy for this reading is resolved by mapping the six classifications to their structural positions: powerless citizens experience Snare (trapped in uncertainty, no exit, costs borne by them); organized movements and subordinate institutions experience Tangled Rope or Rope (empowered by the framework, mobile exit options, genuine coordination benefit plus extraction cost or pure coordination); the judiciary experiences Snare or Tangled Rope depending on time horizon (constrained by delegitimization, performance burden, but also given a participatory role); the analytical observer experiences Scaffold (transitional design). The mandatrophy is not a puzzle about which type is correct — it is a diagnostic that the reading produces radically different structural positions for different agents. A constraint that appears as Rope to powerful, organized agents while appearing as Snare to the powerless is exhibiting the precise extraction pattern this reading was designed to analyze: the democratization of interpretive authority benefits those with organizational capacity to engage in constitutional contestation (movements, subordinate institutions, pluralist coalitions) while imposing costs on those without such capacity (ordinary citizens, marginalized groups, institutions dependent on legal clarity).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    democratic_sentiment_source_ambiguity,
    'What counts as ''democratic sentiment'' — spontaneous popular will, organized social movement, electoral mandate, legislative action, all of the above?',
    'Empirical mapping of constitutional change cases: which types of popular mobilization historically shifted judicial interpretation? Correlation analysis between constituency pressure and doctrinal drift.',
    'If only spontaneous mass movements count: constraint is highly illiberal (mob rule risk). If legislative and electoral actions count: constraint is a normal legitimacy check (less extractive). If all count: classification shifts toward more snare territory (ambiguity itself becomes extractive).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_sentiment_source_ambiguity, conceptual, 'Definition of democratic sentiment in popular constitutionalism').

omega_variable(
    terminal_authority_paradox,
    'Does the reading require that NO institutional site has terminal authority, or merely that no SINGLE site has exclusive terminal authority?',
    'Formal logical analysis of the reading''s premises. If no site can have terminal authority: popular constitutionalism is a pure contestation model (perpetual gridlock). If multiple sites can have authority within domains: the reading is compatible with stable institutional pluralism.',
    'Pure contestation model: much higher suppression, snare-tilted classification. Pluralism model: lower suppression, rope-tilted classification. This is the core structural ambiguity driving the entire reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(terminal_authority_paradox, conceptual, 'Whether terminal authority is forbidden entirely or only forbidden as monopoly').

omega_variable(
    countermobilization_capacity,
    'Can well-organized anti-democratic movements (fascism, theocracy, authoritarian coalitions) use the popular constitutionalism framework to reshape constitutional meaning in anti-democratic directions?',
    'Historical case analysis: Weimar Constitution under Nazi mobilization; post-colonial constitutions under authoritarian takeover. Does the framework''s resistance to institutional gatekeeping make it more or less vulnerable to authoritarian capture?',
    'If vulnerable: the constraint is extractive in a new dimension — it empowers malign constituencies. Extractiveness rises toward pure snare. If resilient: the reading''s claim that distributed contestation is self-correcting is validated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(countermobilization_capacity, empirical, 'Whether popular constitutionalism framework is vulnerable to anti-democratic capture').

omega_variable(
    reading_versus_judicial_supremacy_foreclosure,
    'Does popular constitutionalism logically foreclose judicial supremacy, or merely relocate judicial authority to one voice among many?',
    'Formal analysis of whether a judge can hold both ''I have the final say'' and ''meaning is decided by ongoing contestation.'' If both can be held simultaneously (judge participates in contestation but claims special weight), the readings coexist. If not, foreclosure is real.',
    'Foreclosure: sibling relationship is ''forecloses''. Coexistence: sibling relationship is ''coexists_with''. This determines whether the readings can be held in the same constitutional framework by different parties.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_versus_judicial_supremacy_foreclosure, conceptual, 'Whether popular constitutionalism forecloses judicial supremacy or coexists with it').

omega_variable(
    institutional_cost_distribution,
    'Are the costs of perpetual contestation — gridlock, instability, litigation burden, delayed rights vindication — genuinely distributed across constituencies, or concentrated on the powerless?',
    'Empirical mapping of how constitutional delays and gridlock affect different groups: do wealthy/organized groups weather constitutional uncertainty better? Do marginalized groups bear disproportionate costs of delayed rights protection?',
    'If costs are concentrated: suppression measure should be higher (~0.70), classification tilts toward pure snare for powerless agents. If genuinely distributed: suppression and extractiveness are properly calibrated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_cost_distribution, empirical, 'Whether gridlock costs are distributed or concentrated on the powerless').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_authority__popular_constitutionalism_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(popcon_theater_t0, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(popcon_theater_t20, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 20, 0.55).
narrative_ontology:measurement(popcon_theater_t40, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 40, 0.65).

% Extraction over time
narrative_ontology:measurement(popcon_extract_t0, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(popcon_extract_t20, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 20, 0.51).
narrative_ontology:measurement(popcon_extract_t40, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(popcon_suppress_t0, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(popcon_suppress_t20, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(popcon_suppress_t40, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_authority__popular_constitutionalism_reading, identity_coordination).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__popular_constitutionalism_reading, basic_law_interpretive_authority__judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__popular_constitutionalism_reading, basic_law_interpretive_authority__parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% The basic_law_interpretive_authority kernel decomposes into three distinct constraint stories, each instantiating a different reading and producing different ε values, beneficiary/victim structures, and classification profiles. Popular constitutionalism (this file) models distributed authority with ε ≈ 0.58 (Tangled Rope). Judicial supremacy reading models concentrated authority in courts with different extractiveness and suppression metrics. Parliamentary sovereignty reading models concentration in elected institutions. The three readings are structurally incompatible in the sense that no single constitutional framework can simultaneously grant terminal authority to the Court, the legislature, and the people — but they can coexist as different parties' commitments, making them 'coexists_with' rather than 'forecloses' in most political contexts (except during constitutional crises when one reading must prevail).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
