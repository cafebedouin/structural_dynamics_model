% ============================================================================
% CONSTRAINT STORY: spanish_political_opposition_suppression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_spanish_political_opposition_suppression, []).

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
 *   constraint_id: spanish_political_opposition_suppression
 *   human_readable: Spanish Political Opposition Suppression (Post-2017 Catalonia)
 *   domain: political/governance
 *
 * SUMMARY:
 *   Following the 2017 Catalan independence referendum and unilateral
 *   declaration, Spain's central government implemented a multi-faceted
 *   suppression apparatus targeting pro-independence movements and regional
 *   autonomy advocates. This constraint operates through criminal prosecution
 *   (sedition and rebellion charges), surveillance infrastructure, media
 *   control, and institutional pressure on the judiciary. The suppression is
 *   not presented as raw authoritarianism but justified through state
 *   security and constitutional unity narratives. The constraint exhibits
 *   snare characteristics: high extraction (imprisonment, financial ruin,
 *   political exclusion), high suppression (legal barriers, prosecution
 *   threat, surveillance), minimal coordination benefit (no genuine dialogue
 *   or problem-solving), and reliance on criminalization to prevent
 *   alternatives. The judicial system performs due process theater while
 *   applying laws designed to criminalize political opposition. The
 *   extractiveness has risen from 0.42 to 0.68 over six years as legal
 *   penalties accumulated and prosecution infrastructure expanded. Theater
 *   ratio (0.58) reflects that formal legal procedures mask substantive
 *   political use of law as suppression tool.
 *
 * KEY AGENTS:
 *   - Catalan Independence Leadership: Primary targets (powerless/trapped) — imprisoned, banned from office, facing financial ruin; cannot exit without renouncing political identity and goals
 *   - Catalan Civil Society: Broader victim pool (moderate-powerless/identity_locked) — subject to surveillance, self-censorship, prosecution threat; identity-locked through fusion of Catalan regional identity with democratic participation; structurally mobile but identity prevents exit
 *   - Madrid Central Government: Primary beneficiary (institutional/arbitrage) — maintains territorial control, eliminates electoral competition, concentrates political power; can exit via negotiation or constitutional reform
 *   - Spanish Left-Opposition Parties: Secondary victim (moderate/constrained) — face surveillance and prosecution risk for allies but retain some parliamentary voice; barriers are high but not insurmountable
 *   - Spanish Judiciary: Institutional actor (institutional/arbitrage) — applies politically-motivated prosecution under guise of neutral law; maintains theater of independence while executing executive political priorities
 *   - International Democratic Institutions: Analytical observers (analytical/analytical) — view constraint as violation of democratic norms; no power to exit or modify constraint from within Spain
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(spanish_political_opposition_suppression, 0.68).
domain_priors:suppression_score(spanish_political_opposition_suppression, 0.72).
domain_priors:theater_ratio(spanish_political_opposition_suppression, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(spanish_political_opposition_suppression, extractiveness, 0.68).
narrative_ontology:constraint_metric(spanish_political_opposition_suppression, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(spanish_political_opposition_suppression, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(spanish_political_opposition_suppression, snare).
narrative_ontology:human_readable(spanish_political_opposition_suppression, "Spanish Political Opposition Suppression (Post-2017 Catalonia)").
narrative_ontology:topic_domain(spanish_political_opposition_suppression, "political/governance").

domain_priors:requires_active_enforcement(spanish_political_opposition_suppression).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(spanish_political_opposition_suppression, madrid_central_government).
narrative_ontology:constraint_beneficiary(spanish_political_opposition_suppression, ruling_political_coalition).
narrative_ontology:constraint_victim(spanish_political_opposition_suppression, catalan_independence_movements).
narrative_ontology:constraint_victim(spanish_political_opposition_suppression, regional_political_autonomy).
narrative_ontology:constraint_victim(spanish_political_opposition_suppression, democratic_opposition_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CATALAN INDEPENDENCE ACTIVISTS (SNARE) — Face criminal prosecution, sedition charges, imprisonment, financial ruin. Exit mechanism (renounce independence goal) requires abandoning political identity. No structural escape without accepting political defeat. Maximum extraction with minimal coordination benefit — constraint exists to suppress, not coordinate.
constraint_indexing:constraint_classification(spanish_political_opposition_suppression, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CATALAN CIVIL SOCIETY (SNARE) — Identity-locked perspective. Structurally mobile (some could relocate, change political affiliation) but identity constituted through Catalan regional identity and democratic participation. Exit would require abandoning self-conception as Catalan and participant in regional democracy. Severe suppression of speech, assembly, and autonomous governance. Binding is cognitive (identity fusion with regional autonomy) + structural (legal/enforcement barriers).
constraint_indexing:constraint_classification(spanish_political_opposition_suppression, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 3: SPANISH LEFT-OPPOSITION PARTIES (TANGLED ROPE) — Constrained but not trapped. Face barriers to organizing (surveillance, media control, prosecution risk for allies) but retain some parliamentary voice and organizational capacity. Constraint includes genuine coordination of state security (legitimate function) alongside asymmetric suppression of electoral opposition. Mixed extraction: some benefit from state monopoly on political narrative, but also bear suppression costs.
constraint_indexing:constraint_classification(spanish_political_opposition_suppression, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MADRID CENTRAL GOVERNMENT (ROPE) — Net beneficiary. Constraint functions as coordination mechanism for state authority (legitimate security function) and extraction mechanism for political opponents (illegitimate suppression). Experiences constraint as governance tool with coordination benefit (maintaining territorial integrity through enforcement). Can exit via constitutional reform or negotiation — arbitrage options available.
constraint_indexing:constraint_classification(spanish_political_opposition_suppression, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: SPANISH JUDICIARY (PITON) — Maintains theater of legal due process and judicial independence while applying sedition and rebellion laws designed to criminalize political opposition. High theater ratio: procedural regularity masks substantive politicization. Judicial system persists through institutional inertia despite degraded function (independence compromised by political pressure). Primary function (neutral adjudication) has atrophied; constraint sustained by performance of legality rather than actual independence.
constraint_indexing:constraint_classification(spanish_political_opposition_suppression, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From democratic principles (universal scope), suppression of legitimate political opposition violates foundational norms. Constraint is predatory extraction disguised through state authority. High suppression, minimal coordination benefit, systematic extraction of political rights. Classification remains Snare at analytical level — no perspective shift naturalizes this into coordination or necessity.
constraint_indexing:constraint_classification(spanish_political_opposition_suppression, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(spanish_political_opposition_suppression_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(spanish_political_opposition_suppression, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(spanish_political_opposition_suppression, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(spanish_political_opposition_suppression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(spanish_political_opposition_suppression, TR),
    TR >= 0.70.

:- end_tests(spanish_political_opposition_suppression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and increasing. The constraint extracts political rights, liberty (imprisonment), economic resources (legal fees, asset freezes), and political voice from pro-independence populations. The extraction has accumulated over six years as prosecution infrastructure expanded and political penalties increased. The 0.68 value reflects demonstrated extraction (hundreds imprisoned, thousands prosecuted, electoral suppression through disqualifications) not potential extraction. Suppression (0.72): Severe. Legal mechanisms (sedition laws applied to political speech), surveillance infrastructure (Pegasus deployment reported), prosecution threat (selective enforcement against opposition), and institutional capture (judiciary applying politically-motivated rulings) create multiple layers of suppression. Targets cannot organize freely, communicate openly, or participate in political processes without prosecution risk. Theater ratio (0.58): Moderate-high. The constraint is administered through formal legal procedures (trials, appeals, constitutional court review) that perform judicial legitimacy while substantively serving political suppression. The theater is significant but not absolute — Spain retains some international legal norms and EU oversight that constrain pure arbitrariness. As extraction deepened, theater increased (more elaborate legal justifications required).
 *
 * PERSPECTIVAL GAP:
 *   The snare classification diverges across almost all perspectives except analytical. The beneficiary (Madrid government) would classify this as rope (coordination for state security). The captured judiciary would classify as rope (neutral adjudication). The broader analytical perspective must classify as snare (suppression of political opposition). The gap reveals the suppression's defining feature: beneficiaries invoke coordination framing ('we are managing territorial integrity') while victims experience extraction without coordination dialogue ('we are being criminalized for political speech'). The identity_locked perspective (civil society) is particularly diagnostic: these agents are structurally mobile (could relocate, change political affiliation, cease activism) but perceive the constraint as immutable because their identity is constituted through Catalan regional participation. If their identity frame shifted, the constraint would become mobile. The frame IS the trap.
 *
 * DIRECTIONALITY LOGIC:
 *   Madrid central government (institutional/arbitrage) derives low directionality through beneficiary status + arbitrage exit options (can negotiate, reform constitution, offer amnesty). Effective extraction flowing TO this agent, not FROM them — derived d ≈ 0.15, f(d) ≈ -0.01. Catalan independence targets (powerless/trapped) derive high directionality through victim status + trapped exit (imprisoned, disqualified, criminalized; cannot exit without renouncing political identity and goal). Maximum extraction flowing FROM this agent — derived d ≈ 0.95, f(d) ≈ 1.42. Catalan civil society (moderate-powerless/identity_locked) derive high-moderate directionality through victim status + identity_locked exit (structurally mobile but identity-constituted through regional autonomy). Extraction flowing FROM this agent with internal binding — derived d ≈ 0.89, f(d) ≈ 1.28. Spanish judiciary (institutional/arbitrage claiming independence) appears in perspective as beneficiary + arbitrage, but directionality override may be needed: judicial actors are partly captured (constrained by executive pressure), not true arbitrage. Could override d upward from 0.15 to 0.35 to reflect capture.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves through perspectival dominance, not through ambiguity. The snare classification is correct from analytical and victim perspectives (high suppression, high extraction, minimal coordination benefit). The rope classification from the beneficiary is a false summit — it naturalizes political suppression as legitimate state function. The theater (formal legal procedures) creates the false summit: procedures look like coordination (trials, appeals, constitutional review) but are substantially suppression (prosecution designed to eliminate political alternatives). The piton classification of judiciary reflects this theater accurately: formal legal process with degraded underlying independence. The mandatrophy is resolved by recognizing that structural extraction (imprisonment, disqualification, prosecution) contradicts the coordination frame — this is not a problem-solving mechanism where both sides negotiate toward shared benefit. It is a constraint where one side prevents the other from participating. Therefore snare is correct. Mandate (state security and territorial integrity) does not override classification: extraction is the PRIMARY function, not a side effect of legitimate security coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sedition_law_legitimacy,
    'Are sedition charges for independence advocacy legitimate state security enforcement or illegitimate criminalization of political opposition?',
    'Comparative analysis: judicial outcomes in other EU democracies for similar independence movements; international human rights assessment against ECHR standards; proportionality test (imprisonment length vs alleged harm)',
    'If legitimate: constraint reclassifies toward tangled_rope (genuine security coordination with extraction). If illegitimate: constraint is pure snare (political suppression disguised as law). Current classification assumes illegitimacy based on ECHR findings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sedition_law_legitimacy, empirical, 'Legitimacy of sedition laws applied to independence activism').

omega_variable(
    democratic_reform_pathway,
    'Is constitutional reform pathway to Catalan autonomy genuinely available or is it politically closed by suppression logic?',
    'Historical analysis of reform timelines; negotiation capacity measurement; political feasibility assessment by independent analysts; comparison to other EU federal restructurings',
    'If reform pathway open: constraint has sunset potential (scaffold reclassification possible). If pathway closed by suppression itself: constraint is self-perpetuating snare with no exit mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_reform_pathway, conceptual, 'Availability of democratic constitutional reform pathway').

omega_variable(
    judiciary_independence_capture,
    'Has Spain''s judiciary been captured by executive political pressure or does it retain functional independence despite controversial rulings?',
    'Analysis of judicial reversals in independence cases; comparison of sentencing patterns between independence and non-independence defendants; assessment of prosecution selectivity; international rule-of-law indices; judicial autonomy survey data',
    'If captured: piton classification is accurate (theater disguises politicized enforcement). If independent: judiciary is genuinely adjudicating complex constitutional questions and classifications require adjustment toward tangled_rope. Current data suggests significant capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judiciary_independence_capture, empirical, 'Degree of judicial independence from executive political pressure').

omega_variable(
    civil_society_suppression_scope,
    'Is suppression limited to political leadership (elite targeting) or extended to broader civil society participation (mass suppression)?',
    'Data on arrests/prosecutions by social class; measurement of protest participation changes; survey data on self-censorship prevalence; analysis of prosecution patterns across organizational levels',
    'If elite-targeting only: extraction is concentrated (snare affecting powerless political class, rope for broader society). If mass suppression: constraint affects entire regional population (snare for all). Current evidence suggests both elite prosecution AND mass suppression via surveillance/prosecution threat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(civil_society_suppression_scope, empirical, 'Scope of suppression (elite targeting vs mass suppression)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(spanish_political_opposition_suppression, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(span_tr_t0, spanish_political_opposition_suppression, theater_ratio, 0, 0.48).
narrative_ontology:measurement(span_tr_t3, spanish_political_opposition_suppression, theater_ratio, 3, 0.54).
narrative_ontology:measurement(span_tr_t6, spanish_political_opposition_suppression, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(span_be_t0, spanish_political_opposition_suppression, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(span_be_t3, spanish_political_opposition_suppression, base_extractiveness, 3, 0.58).
narrative_ontology:measurement(span_be_t6, spanish_political_opposition_suppression, base_extractiveness, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(spanish_political_opposition_suppression, enforcement_mechanism).
narrative_ontology:affects_constraint(spanish_political_opposition_suppression, catalan_regional_autonomy_degradation).
narrative_ontology:affects_constraint(spanish_political_opposition_suppression, spanish_judicial_independence_capture).
narrative_ontology:affects_constraint(spanish_political_opposition_suppression, eu_rule_of_law_deterioration).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(spanish_political_opposition_suppression, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
