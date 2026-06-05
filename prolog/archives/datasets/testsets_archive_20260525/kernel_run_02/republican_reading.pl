% ============================================================================
% CONSTRAINT STORY: republican_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_republican_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: republican_reading
 *   human_readable: Republican Legitimacy: Authority from Popular Sovereignty
 *   domain: political_philosophy/constitutional_theory
 *
 * SUMMARY:
 *   The republican reading instantiates a specific claim about political
 *   legitimacy: that authority is valid only when it flows upward from the
 *   people through delegated consent mechanisms, grounded in the principle of
 *   popular sovereignty and social contract theory. This reading is ONE of at
 *   least three structurally distinct ways to ground political authority —
 *   competing with monarchical readings (authority flows from dynastic
 *   succession, divine right, or hereditary tradition) and
 *   constitutional-hybrid readings (authority derives from mixed sources:
 *   electoral consent, constitutional text, institutional stability, or
 *   tradition). The republican reading is a commitment-system constraint
 *   because it grounds legitimacy claims in a kernel (the principle of
 *   popular sovereignty) that different parties read differently: as a
 *   deontological right to self-governance, as an instrumental tool for
 *   peaceful succession, as an empirical claim about actual popular control,
 *   or as a performative ritual whose legitimacy depends on maintaining
 *   electoral theater. The extractiveness trajectory (0.32 → 0.52 over 100
 *   years) reflects the historical drift: as electoral systems mature,
 *   participation expands, but the gap between legitimacy claims and actual
 *   power concentration grows. Theater ratio rises (0.42 → 0.61) as systems
 *   maintain democratic rituals while concentrating power through financial,
 *   informational, and structural constraints on majoritarian outcomes.
 *
 * KEY AGENTS:
 *   - Voting Citizenry: Formal beneficiary (institutional/arbitrage) — holds sovereign authority in principle, can remove authority through elections
 *   - Excluded Populations: Primary victim (powerless/trapped) — subject to state authority without representation or exit; includes age-based (children), status-based (non-citizens), and deliberate exclusions (felons, property requirements)
 *   - Permanent Minority: Secondary victim (moderate/constrained) — enfranchised but systematically outvoted; bears extraction from majoritarian preference-aggregation
 *   - Electoral Institution: Coordinating beneficiary (institutional/arbitrage) — consolidates authority transfer through legitimacy mechanism
 *   - State Executing Authority: Institutional victim (institutional/constrained) — must enforce legitimacy narratives and suppress dissent against majoritarian outcomes
 *   - Constitutional Minority Protection Movement: Organized advocate (organized/constrained) — institutionalizes minority exit threat through bills of rights and constitutional limits
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees the republican reading as contingent on historical conditions (literacy, communication, electoral capacity) that fail in many contexts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(republican_reading, 0.48).
domain_priors:suppression_score(republican_reading, 0.52).
domain_priors:theater_ratio(republican_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(republican_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(republican_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(republican_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(republican_reading, tangled_rope).
narrative_ontology:human_readable(republican_reading, "Republican Legitimacy: Authority from Popular Sovereignty").
narrative_ontology:topic_domain(republican_reading, "political_philosophy/constitutional_theory").

domain_priors:requires_active_enforcement(republican_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(republican_reading, formalized).
narrative_ontology:cs_authority_grounding(republican_reading, lineage).
narrative_ontology:cs_interpretation_layer_present(republican_reading).
narrative_ontology:cs_kernel_id(republican_reading, sovereign_legitimacy).
narrative_ontology:cs_reading_relation(republican_reading, monarchical_reading, forecloses).
narrative_ontology:cs_reading_relation(republican_reading, constitutional_hybrid_reading, influences).
narrative_ontology:cs_axiom(republican_reading, foundational, popular_sovereignty_principle).
narrative_ontology:cs_axiom_status(popular_sovereignty_principle, holdable).
narrative_ontology:cs_axiom_grounding(republican_reading, popular_sovereignty_principle, deontological).
narrative_ontology:cs_axiom(republican_reading, foundational, delegated_consent_requirement).
narrative_ontology:cs_axiom_status(delegated_consent_requirement, holdable).
narrative_ontology:cs_axiom_grounding(republican_reading, delegated_consent_requirement, deontological).
narrative_ontology:cs_reference_frame(republican_reading, enlightenment_popular_sovereignty).
narrative_ontology:cs_drift_state(republican_reading, contemporary_information_age, gap(practice_drift, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(republican_reading, voting_citizenry).
narrative_ontology:constraint_beneficiary(republican_reading, franchise_holders).
narrative_ontology:constraint_victim(republican_reading, excluded_populations).
narrative_ontology:constraint_victim(republican_reading, representation_gaps).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISENFRANCHISED SUBJECT (TANGLED ROPE) — Structurally excluded from consent mechanisms (by age, citizenship status, or deliberate legal exclusion). Trapped: cannot exit the territorial jurisdiction without severe cost. Experiences extraction (coerced obedience without representation) and minimal coordination benefit. Yet the republican framework's legitimacy narrative claims to speak FOR this agent, creating performative inclusion without actual power.
constraint_indexing:constraint_classification(republican_reading, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ENFRANCHISED VOTER (ROPE) — Has formal consent mechanism (electoral participation) but constrained by winner-take-all logic, information barriers, and majoritarian suppression of minority interests. Experiences genuine coordination: elections solve the collective action problem of peaceful succession and aggregate preference. Extraction is moderate because legitimacy requires ongoing electoral validation — the voter's withdrawal of consent is theoretically fatal to the system.
constraint_indexing:constraint_classification(republican_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PERMANENT MINORITY (SNARE) — Structurally disadvantaged by majoritarian decision rules. Constrained: could theoretically vote but systematically outvoted. Extraction rises to snare levels when the minority cannot credibly threaten exit and majoritarian preferences consistently override minority interests. The republican legitimacy frame offers only the slim hope of converting to a majority — insufficient exit option.
constraint_indexing:constraint_classification(republican_reading, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUTIONAL MINORITY PROTECTION MOVEMENT (TANGLED ROPE) — Organized attempt to enforce constitutional limits on majoritarian extraction (bills of rights, super-majority requirements, federal checks). Coordinating function: institutionalizes the minority's exit threat (constitutional amendment, judicial remedy, federation exit). Extraction through the need for continuous litigation and constitutional struggle. Benefits from legitimacy framework that acknowledges minority protection as an inherent republican concern.
constraint_indexing:constraint_classification(republican_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ELECTORAL INSTITUTION (ROPE) — Benefits from the legitimacy delegation: periodic elections consolidate authority transfer peacefully and with popular validation. Experiences the constraint as pure coordination: solving succession and aggregating preferences. Extraction is minimal because the institution's authority depends entirely on maintaining appearance of responsive legitimacy.
constraint_indexing:constraint_classification(republican_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: EXECUTING STATE AUTHORITY (TANGLED ROPE) — Coordinates implementation of popular will through delegation. But faces genuine extraction pressure: the state apparatus must enforce legitimacy narratives that may not reflect actual popular preferences, must suppress dissent against majoritarian outcomes, and must maintain performative responsiveness (town halls, consultations) that consume resources without genuine policy impact. Extraction rises from the gap between legitimacy claims and actual authority derivation.
constraint_indexing:constraint_classification(republican_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — Observes that the republican reading depends on historical conditions (literacy, mass communication, electoral mechanisms) that are contingent, not universal. When those conditions fail, the legitimacy narrative provides cover for majoritarian tyranny or elite control masquerading as popular sovereignty. The constraint becomes pure extraction — the legitimacy frame extracts compliance from those who would resist if they perceived the system as non-responsive. Yet the analytical view sees the fiction is not inherent to republicanism but to IMPLEMENTATION of republicanism.
constraint_indexing:constraint_classification(republican_reading, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(republican_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(republican_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(republican_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(republican_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(republican_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The republican reading requires ongoing legitimacy validation through elections. This creates asymmetry: those excluded from voting (children, non-citizens, populations suppressed from effective participation) face full extraction without consent. Those enfranchised face moderate extraction through majoritarian suppression of minority interests and through the performance of participation (elections, consultations) that consume resources without policy impact. The trajectory shows rising extractiveness: early republics had lower participation rates but less sophisticated suppression mechanisms; modern republics have wider suffrage but more refined methods of constraining outcomes (campaign finance, information control, structural barriers to third parties). Suppression (0.52): Moderate-high. Structural barriers include: disenfranchisement of non-propertied classes (historical), denial of voting rights to non-citizens and non-residents, literacy requirements, voter registration burdens, felony disenfranchisement, geographic gerrymandering, and informational asymmetries. These are sufficiently high that exit from the jurisdiction is effectively forced (trapped classification for the excluded) or constrained (high cost). But suppression is not total — voting rights are expandable, and the legitimacy narrative itself provides a hook for expanding the franchise. Theater ratio (0.58): Moderate-high. Electoral rituals (debates, campaigns, polling) consume significant resources and perform legitimacy validation. The ratio rises over time as media coverage intensifies, yet actual policy responsiveness to electoral preferences declines (as measured by preference-outcome congruence studies). This reflects growing gap between the performance of democracy and its functional operation — the constraint's increasing theater signature.
 *
 * PERSPECTIVAL GAP:
 *   The republican reading produces strong perspectival divergence across power and exit dimensions. Enfranchised voters in majority factions see rope (coordination solution to succession). Permanent minorities see snare (systematic exclusion from preference-aggregation). Disenfranchised populations see tangled rope (the system claims to speak for them while extracting obedience). The executing state sees tangled rope (coordination function meets extraction pressure). The analytical observer sees snare (the legitimacy narrative masks actual authority structure). This gap reveals a fundamental structural tension: the reading's beneficiaries are those with effective voice in the legitimacy mechanism (voters in competitive districts); its victims are those without (excluded populations and permanent minorities). The reading's extractiveness partly derives from this definitional asymmetry — who counts as 'the people' determines whether a person is beneficiary or victim.
 *
 * DIRECTIONALITY LOGIC:
 *   The republican reading establishes beneficiaries (voting citizenry with effective franchise) and victims (excluded and permanently minoritized populations). For beneficiaries with arbitrage exit options (mobility, voice in electoral process), the derivation produces low d, yielding low chi. For victims with trapped exit (territorial immobility, no franchise), the derivation produces high d, yielding high chi. For moderate agents (enfranchised but constrained by majoritarian logic), derivation produces mid-range d. The state executing authority is institutionally constrained (cannot exit the legitimacy frame without losing authority), producing higher d than arbitrage despite beneficiary status from coordination function. The permanent minority perspective derives from moderate power + constrained exit + victim status, producing high d sufficient for snare classification despite nominal enfranchisement. No overrides are needed; the structural data flow correctly through the derivation chain.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves mandatrophy through the perspectival structure: the constraint is simultaneously rope (for beneficiaries), tangled rope (for moderate participants and for the state authority), snare (for permanent minorities), and snare (for the analytical observer seeing through legitimacy claims). The reading is NOT an error in classification — it is a structurally accurate representation of how the republican legitimacy frame operates across different positions. Those with voice experience coordination; those without experience extraction. The constraint's legitimacy depends on maintaining the claim that 'the people' exercise authority, which requires continuous performance (elections, consultations, rituals). As extractiveness rises (t0→t100), the reading faces increasing pressure: if the gap between participation claims and power concentration becomes too visible, the theatrical validation fails and the snare character emerges into view even for beneficiaries. The constraint is manageable only if suppression remains high enough that excluded and minoritized populations cannot coordinate refusal of consent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_authenticity_threshold,
    'What level of informed participation constitutes genuine consent vs. performative participation?',
    'Comparative analysis of electoral systems: does voter knowledge correlate with electoral outcomes? Do policy changes reflect majority preferences? Do supermajority requirements improve representation? Do participatory mechanisms (sortition, deliberative polling) produce different legitimacy outcomes than voting alone?',
    'If threshold is very high: modern republics fail to meet genuine consent standard, revealing extraction mechanism. If threshold is low: the system''s legitimacy claims are defensible. Classification shifts from snare/tangled_rope toward rope depending on observed participation quality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_authenticity_threshold, empirical, 'Threshold for distinguishing informed consent from performative participation').

omega_variable(
    majoritarian_tyranny_boundary,
    'At what point does majoritarian preference-aggregation become tyranny of the majority against irreducible minorities?',
    'Empirical review of constitutional court decisions protecting minorities; historical cases where majorities voted to strip minority rights; effectiveness of supermajority requirements and courts in blocking majoritarian extraction',
    'If constitutional limits effectively protect minorities: extraction is moderate (tangled rope). If supermajorities and courts are regularly overridden: extraction rises (snare). The reading''s legitimacy depends on its structural capacity to prevent majority tyranny.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(majoritarian_tyranny_boundary, empirical, 'Point at which majoritarian aggregation becomes minority extraction').

omega_variable(
    alternative_readings_foreclosure,
    'Does the republican reading''s axiom of popular sovereignty logically rule out monarchical or constitutional-hybrid readings, or do they coexist as live policy alternatives?',
    'Logical/conceptual: Does affirming that authority flows from popular consent require denying that authority flows from divine right or hereditary tradition? Can a constitutional monarchy hold both claims (popular validation of hereditary authority)? Can a hybrid system (elected upper chamber, hereditary lower chamber) instantiate both readings simultaneously?',
    'If forecloses: republican reading makes monarchical reading incoherent within a single framework. If coexists: readings represent different institutional expressions that can overlap. If influences: the republican framework creates structural pressure toward elective succession even in monarchical systems.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_readings_foreclosure, conceptual, 'Whether popular sovereignty axiom rules out alternative legitimacy sources').

omega_variable(
    extraction_mechanism_source,
    'Is the measured extractiveness (0.48) structural to the republican reading, or does it reflect historical implementation failures?',
    'Comparative constitutional design: Do systems with stronger participatory mechanisms (recall elections, sortition, proportional representation, direct democracy components) show lower extractiveness? Do systems with weaker participation mechanisms show higher extractiveness? Is the gap attributable to design or to elite capture of nominally democratic institutions?',
    'If structural: extractiveness reflects inherent tension between delegation and control — even ideal republican systems extract from those excluded from franchise and from minorities. If implementation: extractiveness could be reduced by institutional redesign. Affects whether mandatrophy resolves or whether constraint is endemic to the reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_mechanism_source, empirical, 'Whether extractiveness is structural to republicanism or contingent on implementation').

omega_variable(
    reading_kernel_ambiguity,
    'What is the actual kernel this reading depends on: the principle of popular sovereignty, the institutional form of elections, the narrative claim that authority flows from the people, or the historical experience of successful republics?',
    'Clarify whether this reading''s legitimacy derives from deontological claim (people have right to self-governance), instrumental claim (elections work well for succession), empirical claim (people actually exercise control), or conventional claim (the form of elections is what matters, not outcomes)',
    'If deontological kernel: reading forecloses non-consensual systems. If instrumental: reading coexists with any system that achieves succession. If empirical: reading''s status depends on whether elections actually aggregate preferences. If conventional: reading depends on maintaining electoral theater regardless of outcomes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, conceptual, 'Ambiguity in what kernel this reading depends on').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(republican_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rep_theater_t0, republican_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(rep_theater_t50, republican_reading, theater_ratio, 50, 0.58).
narrative_ontology:measurement(rep_theater_t100, republican_reading, theater_ratio, 100, 0.61).

% Extraction over time
narrative_ontology:measurement(rep_extract_t0, republican_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(rep_extract_t50, republican_reading, base_extractiveness, 50, 0.48).
narrative_ontology:measurement(rep_extract_t100, republican_reading, base_extractiveness, 100, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(republican_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(republican_reading, monarchical_reading).
narrative_ontology:affects_constraint(republican_reading, constitutional_hybrid_reading).
narrative_ontology:affects_constraint(republican_reading, majoritarian_tyranny).
narrative_ontology:affects_constraint(republican_reading, franchise_expansion_dynamics).

% DUAL FORMULATION NOTE:
% The republican reading is part of a constraint family decomposed by kernel interpretation. Each reading (republican, monarchical, hybrid) depends on the same contested kernel (sovereign legitimacy) but instantiates different structural claims about authority sources. The extractiveness values differ (republican: 0.48; monarchical: projected ~0.35; hybrid: projected ~0.42) because each reading's legitimacy mechanism differs. Write each as a separate constraint story with its own perspectives, beneficiaries/victims, and ε values. Link via network.affects_constraints to show the family structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
