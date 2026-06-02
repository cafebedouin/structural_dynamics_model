% ============================================================================
% CONSTRAINT STORY: public_health_mandate_authority__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_health_mandate_authority__bodily_autonomy_primary, []).

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
 *   constraint_id: public_health_mandate_authority__bodily_autonomy_primary
 *   human_readable: Public Health Mandate as Bodily Autonomy Violation (Reading: Autonomy Primary)
 *   domain: public_health_law/constitutional_rights/bioethics
 *
 * SUMMARY:
 *   Public health mandates that require medical intervention (vaccination,
 *   quarantine, treatment protocols) present a constitutional contest over
 *   the source and limits of state authority. This constraint story
 *   instantiates ONE READING of that contest: the reading that treats bodily
 *   autonomy as foundational and inviolable, such that no collective benefit
 *   — however large — can justify non-consensual medical invasion. Under this
 *   reading, an unvaccinated person bears a state-imposed bodily cost
 *   (vaccination) to provide a benefit (collective immunity) they did not
 *   consent to receive in exchange. The constraint exists precisely to
 *   suppress the agent's refusal and extract their bodily compliance. This is
 *   structurally a Snare: high extractiveness (0.68), high suppression
 *   (0.75), and minimal genuine coordination benefit. The rising measurements
 *   reflect how the constraint's enforcement intensified over time: initial
 *   appeals to voluntary vaccination gave way to employment mandates, school
 *   exclusions, healthcare access restrictions, and travel prohibitions. Each
 *   escalation increased the suppression requirement and the extractiveness
 *   (agents faced with total deprivation across life domains have no
 *   meaningful exit). The theater ratio reflects that as enforcement
 *   escalated, the justifying rhetoric became more performative — early
 *   discourse around 'stopping transmission' gave way to 'protecting
 *   hospitals' and finally to 'public health infrastructure' as transmission
 *   prevention claims eroded empirically. The constraint now persists largely
 *   through institutional inertia and legal precedent (Jacobson v.
 *   Massachusetts, 1905) rather than current epidemiological necessity.
 *
 * KEY AGENTS:
 *   - Unvaccinated individuals: Primary victims (powerless/trapped) — face coercive medical intervention with no exit; bear full extraction cost
 *   - Broader bodily autonomy constituency: Secondary victims (moderate/constrained) — even compliers are affected by precedent and cognitive capture mechanisms
 *   - Public health authority: Institutional beneficiary (institutional/arbitrage) — implements mandate, benefits from compliance data and operational simplicity
 *   - Medical/pharmaceutical establishment: Institutional beneficiary (institutional/arbitrage) — maintains authority over medical decisions, protected from liability
 *   - Organized opposition movements: Organized agents (organized/mobile) — build alternative institutional frameworks; perceive constraint as both oppressive and mobilizing
 *   - Analytical observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as immutable medical necessities
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__bodily_autonomy_primary, 0.68).
domain_priors:suppression_score(public_health_mandate_authority__bodily_autonomy_primary, 0.75).
domain_priors:theater_ratio(public_health_mandate_authority__bodily_autonomy_primary, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__bodily_autonomy_primary, snare).
narrative_ontology:human_readable(public_health_mandate_authority__bodily_autonomy_primary, "Public Health Mandate as Bodily Autonomy Violation (Reading: Autonomy Primary)").
narrative_ontology:topic_domain(public_health_mandate_authority__bodily_autonomy_primary, "public_health_law/constitutional_rights/bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__bodily_autonomy_primary, '5d1e5ebb-253e-4407-919c-109a5b198ac6').
narrative_ontology:cs_kernel_codification('5d1e5ebb-253e-4407-919c-109a5b198ac6', formalized).
narrative_ontology:cs_authority_grounding('5d1e5ebb-253e-4407-919c-109a5b198ac6', lineage).
narrative_ontology:cs_interpretation_layer_present('5d1e5ebb-253e-4407-919c-109a5b198ac6').
narrative_ontology:cs_reading_relation('5d1e5ebb-253e-4407-919c-109a5b198ac6', public_health_mandate_authority__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('5d1e5ebb-253e-4407-919c-109a5b198ac6', public_health_mandate_authority__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('5d1e5ebb-253e-4407-919c-109a5b198ac6', foundational, bodily_autonomy_foundational_and_inviolable).
narrative_ontology:cs_axiom_status(bodily_autonomy_foundational_and_inviolable, holdable).
narrative_ontology:cs_axiom_grounding('5d1e5ebb-253e-4407-919c-109a5b198ac6', bodily_autonomy_foundational_and_inviolable, deontological).
narrative_ontology:cs_axiom('5d1e5ebb-253e-4407-919c-109a5b198ac6', secondary, collective_health_does_not_justify_coercion).
narrative_ontology:cs_axiom_status(collective_health_does_not_justify_coercion, holdable).
narrative_ontology:cs_axiom_grounding('5d1e5ebb-253e-4407-919c-109a5b198ac6', collective_health_does_not_justify_coercion, deontological).
narrative_ontology:cs_reference_frame('5d1e5ebb-253e-4407-919c-109a5b198ac6', individual_bodily_sovereignty_framework).
narrative_ontology:cs_drift_state('5d1e5ebb-253e-4407-919c-109a5b198ac6', contemporary_mandate_enforcement_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('5d1e5ebb-253e-4407-919c-109a5b198ac6', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__bodily_autonomy_primary, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_victim(public_health_mandate_authority__bodily_autonomy_primary, unvaccinated_individuals).
narrative_ontology:constraint_victim(public_health_mandate_authority__bodily_autonomy_primary, bodily_autonomy_right_holders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNVACCINATED INDIVIDUAL (SNARE) — Faces coercive medical intervention (vaccination mandate) backed by employment exclusion, school attendance restrictions, healthcare access barriers, travel prohibitions. No meaningful exit: refusal triggers cascading deprivation across life domains. The constraint exists precisely to suppress this agent's bodily autonomy choice. Maximum experienced extraction with minimal coordination benefit — pure coercion.
constraint_indexing:constraint_classification(public_health_mandate_authority__bodily_autonomy_primary, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: BODILY AUTONOMY RIGHT-HOLDERS BROADLY (SNARE) — The mandate establishes a precedent for state coercion over bodily medical decisions. Even those who comply perceive extraction: the constraint normalizes state authority to compel medical procedures. Exit is costly (migration to non-mandating jurisdictions, social ostracism, economic penalties). The constraint functions to suppress the class of agents who assert bodily autonomy claims, not to coordinate a public good.
constraint_indexing:constraint_classification(public_health_mandate_authority__bodily_autonomy_primary, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PUBLIC HEALTH AUTHORITY (ROPE) — From the implementing institution's perspective, the mandate is coordination: aligning individual vaccination decisions with a collective immunization target. The authority benefits from compliance (easier to track, simpler epidemiological modeling, reduced liability exposure). Experiences the constraint as a coordination mechanism, not extraction — the constraint solves their collective action problem (free-riding on herd immunity without vaccination).
constraint_indexing:constraint_classification(public_health_mandate_authority__bodily_autonomy_primary, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LEGAL/MEDICAL ESTABLISHMENT (PITON) — The institutional apparatus that enforces and legitimates the mandate (medical licensing boards, employment law, public health regulations) experiences the constraint as increasingly ritualized. The theater involves citing 'settled science' and 'public health necessity' while actual enforcement relies on coercion, not persuasion or genuine democratic deliberation. The constraint persists through institutional inertia and legal precedent (Jacobson v. Massachusetts 1905), not current functional necessity. Theater ratio high because legitimating rhetoric must suppress the extraction reality.
constraint_indexing:constraint_classification(public_health_mandate_authority__bodily_autonomy_primary, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ORGANIZED OPPOSITION (TANGLED ROPE) — Organized groups (medical freedom advocates, libertarian networks, some faith communities) perceive both extraction (coercion structure) and coordination benefits (mobilizing collective action against state medical authority, building alternative institutional frameworks). Their exit is mobile (organizing in non-mandating jurisdictions, building parallel institutions). They experience the constraint as both oppressive and structurally necessary to their own coalition-formation — the constraint is what makes them organized.
constraint_indexing:constraint_classification(public_health_mandate_authority__bodily_autonomy_primary, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURALIZATION VIEW (MOUNTAIN) — From a civilizational view, some state authority over health emergencies appears as a natural law: collective disease transmission is a physical fact; managing pandemics requires coordinated action; therefore, state authority to override individual choice appears inevitable and unchangeable. This perspective risks false summitism — it naturalizes what is actually a contingent institutional arrangement (state monopoly on medical authority, coercive enforcement mechanisms) as immutable.
constraint_indexing:constraint_classification(public_health_mandate_authority__bodily_autonomy_primary, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_health_mandate_authority__bodily_autonomy_primary_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(public_health_mandate_authority__bodily_autonomy_primary, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(public_health_mandate_authority__bodily_autonomy_primary, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(public_health_mandate_authority__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(public_health_mandate_authority__bodily_autonomy_primary, TR),
    TR >= 0.70.

:- end_tests(public_health_mandate_authority__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Under the autonomy-primary reading, the mandate requires agents to undergo a bodily medical procedure they did not consent to, in order to generate a benefit (collective immunity) they did not choose and may not receive. This is pure extraction: the cost (bodily invasion, medical risk, autonomy violation) flows from unvaccinated agents to collective benefit. The measure reflects that extraction increased over time as enforcement mechanisms escalated from persuasion to coercion (employment mandates, travel restrictions, school exclusions). Suppression (0.75): High. Agents face cascading deprivations if they refuse vaccination: loss of employment (primary livelihood), exclusion from education (children), restrictions on healthcare access (medical services for unrelated conditions), travel prohibition (movement freedom). These are not small costs — they suppress exit options entirely. The suppression measures rising trajectory reflects increasing enforcement infrastructure. Theater ratio (0.55): Moderate-high. The justifying rhetoric for mandates has shifted multiple times (stopping transmission → reducing severity → protecting hospital capacity → public health infrastructure) as empirical foundations eroded. Current enforcement relies on legal precedent (Jacobson) and institutional authority, not on transparency about actual epidemiological benefits. However, theater is not as high as institutional piton mechanisms — there is still explicit enforcement apparatus visible. The constraint does not hide itself as much as a pure piton would.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits extreme perspectival divergence. The unvaccinated victim sees pure coercion (Snare). The public health authority sees legitimate coordination (Rope). The institutional beneficiary sees procedural legitimacy based on precedent (Piton). The organized opposition sees the constraint as simultaneously oppressive and mobilizing (Tangled Rope). The analytical observer risks seeing natural law (Mountain) when examining civilizational-scale disease control as an apparent inevitability. The core disagreement is whether bodily autonomy is foundational (autonomy-primary reading, this story) or whether public health emergency creates legitimate override authority (public-health-primary reading, sibling). These readings do not resolve through empirical evidence — they reflect different foundational commitments that coexist in constitutional discourse.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality in this reading is extremely high (d ≈ 0.92–0.95) for unvaccinated agents: they are structurally positioned as pure targets (victims with no exit, trapped by cascading deprivations). The public health authority has d ≈ 0.10–0.15 (beneficiary with arbitrage exit — they can choose non-enforcement or enforcement strategies). The engine derives these d values from the victim/beneficiary declarations and exit options. For unvaccinated agents: victim status + trapped exit + powerless power atom yields the sigmoid f(d) at its maximum, producing high χ (effective extraction). For the authority: beneficiary status + arbitrage exit + institutional power yields negative or minimal f(d), producing near-zero or negative χ (minimal extraction experienced). The perspectival gap emerges because the authority's low extraction experience (they are beneficiary with easy exit) contrasts with the victim's high extraction (trapped with no meaningful choice). Each agent is measuring the same constraint from radically different structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved via omega variables rather than structural reclassification. The core mandatrophy is: 'Does this constraint serve genuine public health necessity (coordination) or does it serve institutional interests in medical authority and liability protection (extraction)?' Under the autonomy-primary reading, the answer is: it serves extraction regardless of empirical public health outcomes. The constraint is classified as Snare because the mechanism is coercive (suppression: 0.75) and the coordination function is minimal or non-existent under this reading. There is no version of this reading that produces Rope — the reading's foundational axiom (bodily autonomy is inviolable) rules out coordination-based justification for medical coercion. The mandatrophy is resolved by specifying that this reading does not claim any coordination function — it claims pure coercion. The public-health-primary reading (sibling) would place the constraint in Tangled Rope or Rope territory by emphasizing genuine coordination benefits; the autonomy-primary reading rules out that framing. The readings coexist (coexists_with relation) without resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_definition_scope,
    'Does bodily autonomy include the right to refuse medical intervention that benefits others? Or does it extend only to interventions affecting solely the individual?',
    'Foundational philosophical analysis; comparison of legal traditions (absolute autonomy vs. qualified autonomy frameworks); empirical study of which conception of autonomy is actually operative in mandate enforcement jurisdictions.',
    'If autonomy is absolute: mandate is pure coercion regardless of epidemiological benefit — Snare from all perspectives. If autonomy is qualified by collective harm: mandate may be legitimate constraint on autonomy exercise — shifts to Tangled Rope or Rope from multiple perspectives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(autonomy_definition_scope, conceptual, 'Whether bodily autonomy right includes refusal when refusal harms others').

omega_variable(
    counterfactual_health_necessity,
    'Did the mandate serve genuine public health necessity, or could equivalent epidemiological outcomes have been achieved through voluntary-uptake incentives, targeted protection of vulnerable populations, and transparency-based persuasion?',
    'Epidemiological modeling comparing actual mandate + compliance outcomes against counterfactual voluntary-incentive scenarios; historical analysis of similar health challenges resolved without coercive mandates (e.g., smoking cessation, seatbelt adoption in early phases).',
    'If necessity is confirmed: mandate is justified extraction (moves toward Tangled Rope from beneficiary perspective). If voluntary alternatives existed: mandate is unjustified coercion — strengthens Snare classification and reduces claimed public health coordination benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_health_necessity, empirical, 'Whether coercive mandate was necessary or voluntary alternatives could achieve equivalent outcomes').

omega_variable(
    institutional_capture_in_authority,
    'Does the mandate reflect genuine public health science, or does it reflect institutional interests of medical/pharmaceutical establishments in preserving authority, liability protection, and market control?',
    'Institutional analysis of financial incentives, liability structures, and career advancement pathways within medical/pharmaceutical institutions; comparison of public health advice in countries with different institutional structures; longitudinal analysis of which experts changed position post-mandate.',
    'If captured: the ''public health'' beneficiary in Perspective 3 is actually a disguised victim (institutional extraction hidden behind public health framing) — reclassifies large portions of the constraint as Snare. If not captured: the coordination function in Perspective 3 is genuine — supports Rope/Tangled Rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_in_authority, empirical, 'Whether public health authority is autonomous or captured by institutional interests').

omega_variable(
    identity_lock_mechanism_compliance,
    'Does mandate compliance represent autonomous decision-making, or has the constraint produced identity-locked compliance (agent has internalized state medical authority as part of their identity as ''responsible citizen'')?',
    'Qualitative interviews with compliant populations examining post-mandate autonomy perception; analysis of whether autonomy framing persists after mandate removal; comparison of compliance levels in high-autonomy vs. low-autonomy cultural contexts.',
    'If identity-locked: even compliant populations are victims of cognitive capture — the constraint exhibits suppression through internalization, not just external coercion. Raises effective suppression above base measure. If autonomous: compliance represents genuine agreement, reducing measured suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_compliance, empirical, 'Whether compliance reflects autonomous choice or identity-locked cognitive capture').

omega_variable(
    kernel_reading_contest,
    'Which reading of the public_health_mandate_authority kernel is correct: bodily_autonomy_primary (this reading), public_health_primary, or proportionality_reading?',
    'This is not an empirical question. It is a contest between foundational normative commitments that cannot be resolved by evidence alone. The readings coexist in legal/ethical discourse without logical resolution — different parties hold different readings within their own coherent frameworks.',
    'If autonomy_primary is held: mandate is Snare from most perspectives, false summit from analytics. If public_health_primary is held: mandate is Rope/Tangled Rope from most perspectives. If proportionality is held: mandate is conditionally justified (Scaffold) during genuine emergency, impermissible after.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Kernel reading contest: which normative framework grounds mandate legitimacy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__bodily_autonomy_primary, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(phma_theater_t0, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 0, 0.42).
narrative_ontology:measurement(phma_theater_t6, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 6, 0.48).
narrative_ontology:measurement(phma_theater_t12, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 12, 0.55).

% Extraction over time
narrative_ontology:measurement(phma_extract_t0, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(phma_extract_t6, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 6, 0.62).
narrative_ontology:measurement(phma_extract_t12, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 12, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(phma_suppress_t0, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(phma_suppress_t6, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 6, 0.68).
narrative_ontology:measurement(phma_suppress_t12, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 12, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_mandate_authority__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(public_health_mandate_authority__bodily_autonomy_primary, public_health_mandate_authority__public_health_primary).
narrative_ontology:affects_constraint(public_health_mandate_authority__bodily_autonomy_primary, public_health_mandate_authority__proportionality_reading).
narrative_ontology:affects_constraint(public_health_mandate_authority__bodily_autonomy_primary, vaccine_adverse_event_causation_ambiguity).
narrative_ontology:affects_constraint(public_health_mandate_authority__bodily_autonomy_primary, pandemic_emergency_declaration_authority).

% DUAL FORMULATION NOTE:
% The public_health_mandate_authority kernel admits multiple constraint readings with different ε values and different beneficiary/victim structures. Each reading (autonomy_primary, public_health_primary, proportionality_reading) is instantiated as a separate constraint story linked by network.affects_constraints. The ε-invariance principle requires separation because the observable used to evaluate the constraint (which normative framework grounds authority?) changes the entire structural analysis, not merely the classification. Each story declares its own ε, its own beneficiaries/victims, and its own perspectives under its own reading's foundational commitments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
