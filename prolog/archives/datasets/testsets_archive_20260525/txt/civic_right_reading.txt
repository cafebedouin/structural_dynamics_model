% ============================================================================
% CONSTRAINT STORY: civic_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_civic_right_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: civic_right_reading
 *   human_readable: Second Amendment as Civic Right Conditioned on Militia Participation
 *   domain: constitutional_law/political_theory/rights_jurisprudence
 *
 * SUMMARY:
 *   The civic-right reading of the Second Amendment proposes that the
 *   protected individual right to bear arms is constitutionally conditioned
 *   on the bearer's eligibility and availability for militia participation.
 *   This reading attempts to reconcile the Amendment's militia clause with
 *   individual right doctrine by treating militia participation as a gating
 *   condition rather than the exhaustive scope of the right. Under this
 *   interpretation, individuals meeting militia eligibility criteria
 *   (citizenship, age, no felony record, mental fitness) possess a right to
 *   bear arms as part of civic duty; individuals outside these categories
 *   possess no such right. This constraint exhibits moderate extractiveness
 *   (0.48) because the gating mechanism creates genuine coordination (shared
 *   defense responsibility, tied to collective duty) but also produces
 *   asymmetric extraction: some populations are excluded entirely from the
 *   right, while others bear implicit obligations to participate. The
 *   extractiveness has accumulated over time (from 0.32 at the Founding to
 *   0.48 in present day) as exclusion categories have expanded (felon
 *   disfranchisement, immigration status, age-based restrictions, capacity
 *   screening). The theater ratio (0.58) reflects that the civic
 *   participation framework is partly performative: citizens do not actually
 *   constitute modern defense (state maintains monopoly on organized force),
 *   yet the Amendment's language is interpreted to tie individual rights to a
 *   militia function that no longer exists in its original form.
 *
 * KEY AGENTS:
 *   - Civic Militia-Eligible Citizens: Primary beneficiaries (institutional/arbitrage) — possess the right to bear arms conditioned on militia eligibility; experience constraint as coordination mechanism yoked to duty
 *   - Non-Militia-Eligible Populations (Felons, Undocumented, Others): Primary victims (powerless/trapped) — excluded from the right entirely; bear suppression of the gating mechanism with no counterbalancing benefit
 *   - State Regulatory Authority: Institutional actor (institutional/arbitrage) — defines militia eligibility criteria, maintains gating mechanism, captures legitimacy of rights-protection and civic duty framing
 *   - Organized Militia-Rights Coalition: Secondary actor (organized/constrained) — advocates for militia-eligible citizens; negotiates terms of gating mechanism; constrained by state authority to define eligibility
 *   - Constitutional Interpreters: Analytical observer (analytical/analytical) — assess whether civic-right reading represents genuine structural coordination or degraded institutional theater
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(civic_right_reading, 0.48).
domain_priors:suppression_score(civic_right_reading, 0.52).
domain_priors:theater_ratio(civic_right_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(civic_right_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(civic_right_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(civic_right_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(civic_right_reading, tangled_rope).
narrative_ontology:human_readable(civic_right_reading, "Second Amendment as Civic Right Conditioned on Militia Participation").
narrative_ontology:topic_domain(civic_right_reading, "constitutional_law/political_theory/rights_jurisprudence").

domain_priors:requires_active_enforcement(civic_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(civic_right_reading, fixed_text).
narrative_ontology:cs_authority_grounding(civic_right_reading, lineage).
narrative_ontology:cs_interpretation_layer_present(civic_right_reading).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(civic_right_reading, civic_militia_eligible_citizens).
narrative_ontology:constraint_beneficiary(civic_right_reading, state_regulatory_authority).
narrative_ontology:constraint_victim(civic_right_reading, non_militia_eligible_populations).
narrative_ontology:constraint_victim(civic_right_reading, excluded_categories).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED POPULATION (SNARE) — Individuals categorized as ineligible for militia participation (felons, undocumented immigrants, those deemed unfit for service) experience the constraint as pure extraction with no coordination benefit. They bear the suppression of the gating mechanism but receive no counterbalancing right to bear arms. No exit option exists — their excluded status is structurally locked. Maximum experienced extraction.
constraint_indexing:constraint_classification(civic_right_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MARGINALLY ELIGIBLE CITIZEN (TANGLED ROPE) — Citizens meeting formal militia eligibility criteria (age, citizenship, no felony record) experience genuine coordination benefits (shared security, collective defense norms) but also face meaningful extraction: registration requirements, training mandates, readiness obligations, or implicit expectations of service in genuine civic emergencies. They have exit options (refusal to participate) but at significant cost. Mixed coordination and extraction.
constraint_indexing:constraint_classification(civic_right_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE REGULATORY AUTHORITY (ROPE) — The state experiences the constraint as pure coordination: it defines militia eligibility, maintains the gating mechanism, and captures the legitimacy benefit of rights-protection. From the state's institutional view, the gating requirement is a coordination feature (ensuring armed capability is yoked to civic duty), not an extraction mechanism. The state has arbitrage options (setting different eligibility criteria, modulating training requirements). Net beneficiary through legitimacy.
constraint_indexing:constraint_classification(civic_right_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MILITIA-PARTICIPATORY COALITION (TANGLED ROPE) — Organized gun-rights advocates and militia-aligned groups see the constraint as both coordination (collective defense, shared civic duty) and extraction (potential for state overreach in defining eligibility, service obligations, training requirements). They have constrained exit (could abandon civic participation entirely and lose the right) but also have organized power to negotiate the terms of the constraint. Moderate extraction, significant coordination function.
constraint_indexing:constraint_classification(civic_right_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL INTERPRETER (PITON) — From a civilizational/analytical perspective examining how constitutional texts degrade through reinterpretation, the civic-right reading appears as a degraded Enlightenment-era coordination mechanism. The original Framers' vision of citizen-militia as alternative to standing armies has become largely performative: modern citizens do not actually constitute the defense against tyranny through militia participation; the constraint persists through constitutional interpretation inertia rather than functional militia capacity. The theater (constitutional ceremony) has replaced the coordination (actual armed readiness). Theater ratio reflects this drift.
constraint_indexing:constraint_classification(civic_right_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: NATURAL LAW PERSPECTIVE (MOUNTAIN) — Some analytical readings see the civic-militia condition as reflecting an immutable natural law: armed populations require civic participation and accountability to prevent tyranny. This perspective treats the right-duty coupling as inherent to justice itself, not as a contingent institutional arrangement. From this view, the gating mechanism is as natural and unchangeable as the principle that freedom requires virtue. However, the structural data (identifiable beneficiaries, enforcement requirements, theater_ratio drift) reveals this as a false summit: the natural-law framing naturalizes what is actually a contested institutional reading.
constraint_indexing:constraint_classification(civic_right_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(civic_right_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(civic_right_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(civic_right_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(civic_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(civic_right_reading, TR),
    TR >= 0.70.

:- end_tests(civic_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate. The constraint creates genuine coordination benefits for eligible citizens (civic duty tied to armed capacity provides shared-defense logic and legitimacy claim) but also produces significant extraction through the gating mechanism. The excluded populations cannot exit their ineligibility status — they are trapped. The eligible populations face implicit or explicit service expectations. The extractiveness is neither low (pure coordination) nor high (pure extraction), but genuinely hybrid. The 16-point rise over the interval (0.32 to 0.48) reflects expansion of exclusion categories (felon disfranchisement, immigration status restrictions) that were less systematically enforced in the Founding era. Suppression (0.52): Moderate-high. Structural barriers to participation include citizenship requirements, background checks, potential service obligations, age gates, and implicit expectations of readiness. However, suppression is not absolute — eligible individuals can choose not to participate (though with potential social cost or legal ambiguity), and some jurisdictions have weaker enforcement. The theater ratio (0.58) reflects that the militia-participation framework has become partially performative: citizens do not actually constitute the state's defense force (standing military provides that), yet the right is justified by reference to militia function. The theater has increased over time as the gap between the Amendment's militia language and modern military reality has widened.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival disagreement across the six types. The excluded population sees pure snare (extraction with no coordination benefit). The marginally eligible citizen sees tangled rope (mixed coordination and extraction, with exit costs). The state sees rope (pure coordination — defining eligibility and yoked rights/duties). The organized coalition sees tangled rope (coordination with potential state overreach). The constitutional interpreter sees piton (degraded militia function, performative interpretation). The natural-law analyst risks seeing mountain (immutable right-duty coupling) but structural data reveals false summit (contingent institutional reading with identifiable beneficiaries). The perspectival gap is maximal because the constraint's core function (militia participation) has become structurally incoherent in modern context — no actual militia exists to participate in; the coordination mechanism has lost its referent.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values differ sharply by agent position. Excluded populations (trapped + no exit + victim status) derive high d (0.92-0.95), producing maximum experienced extraction through f(d). Marginally eligible citizens (constrained exit + mixed benefit/cost + modest victim/beneficiary status) derive moderate d (0.55-0.65), producing moderate χ. State regulatory authority (beneficiary + arbitrage exit) derives low d (0.12-0.18), producing negative or minimal experienced extraction. Organized coalition (organized agent + constrained exit + mixed status) derives moderate d (0.50-0.60), producing balanced χ. The perspectival gap emerges directly from this directionality spread: agents with high d experience snare; agents with low d experience rope; agents with moderate d experience tangled rope.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through reading-specificity. The civic-right reading produces tangled_rope because it embeds both a coordination mechanism (militia participation tied to armed capacity) and asymmetric extraction (gating mechanism excludes populations with no coordination benefit). An individual-right reading (no militia condition, universal eligibility except felons) would produce rope or scaffold (lower extraction, higher coordination). A collective-right reading (militia-dependent only, no universal right) would produce snare (high extraction, state control). The mandatrophy is not 'which reading is correct?' but 'which reading do you instantiate?' The civic-right reading is defensible and corresponds to an identifiable constitutional tradition (early-republic militia theory), but it produces moderate extraction through gating asymmetry. The natural-law perspective's mountain classification is a false summit: the civic-right reading naturalizes a contingent institutional choice (tying rights to militia participation) as immutable law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    militia_participation_definition,
    'Does ''militia participation'' require active service, implicit eligibility, membership in organized units, or merely availability in emergencies?',
    'Historical analysis of militia deployment expectations; statutory definitions across states; comparison of gating rigor in jurisdictions with different militia interpretations',
    'Narrow definition (active service): suppression increases, extracted categories expand, ε rises toward snare. Broad definition (implicit eligibility): suppression decreases, tangled_rope classification strengthens, ε falls toward rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(militia_participation_definition, conceptual, 'Definition of what militia participation entails determines suppression severity').

omega_variable(
    excluded_category_empirical_scope,
    'How many individuals in the United States are functionally excluded from militia participation under this reading? (Felons: ~5.2M; documented immigrants: ~10.5M; undocumented: ~10M; others by age/capacity: ~20M+)',
    'Census-based exclusion rate calculation; comparison of excluded population to included population; trend analysis of exclusion category expansion over time',
    'If excluded population > 25%: snare classification strengthens as primary affected group grows. If < 5%: snare is minor perspective, tangled_rope dominates. Current estimates ~40-45% excluded.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(excluded_category_empirical_scope, empirical, 'Population-level incidence of militia ineligibility').

omega_variable(
    service_obligation_enforceability,
    'Does the civic-right reading actually impose enforceable service obligations, or are obligations merely implicit/aspirational?',
    'Statutory analysis: which jurisdictions enforce militia readiness? Prosecution data for militia dereliction. Historical cases of enforcement attempts. Comparison to conscription regimes that actually enforce.',
    'If enforceable: tangled_rope classification correct, suppression justified. If aspirational only: constraint is piton (performative), ε should fall to ~0.25, theater_ratio should rise to 0.80+.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(service_obligation_enforceability, empirical, 'Whether civic militia participation obligations are actually enforced').

omega_variable(
    kernel_reading_contest,
    'This constraint instantiates ONE reading of the Second Amendment kernel. What are the sibling readings and how do they change classification?',
    'Separate constraint stories for: (a) individual_right_reading (no militia condition, universal eligibility except felons), (b) collective_right_reading (militia-dependent only, no individual right). Compare ε values, beneficiary/victim sets, and perspectival gaps across readings.',
    'Individual reading: ε ~0.15, rope or scaffold (low extraction, high coordination). Collective reading: ε ~0.65, snare or tangled_rope (high extraction, state control). This reading (civic-right): ε ~0.48, tangled_rope (moderate extraction, gated coordination). The delta reveals that classification is reading-dependent, not observable-independent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'This constraint is one of three readings of the Second Amendment kernel; sibling readings produce different ε values and classifications').

omega_variable(
    regulatory_authority_capture,
    'Can the state use militia eligibility criteria to disguise disarmament of disfavored populations (felon status as proxy for racial targeting)?',
    'Historical analysis of felon-disfranchisement pattern correlation with racial incarceration disparities. Comparison of felony conviction rates by demographics. Analysis of whether militia-eligibility gating differs in effect from explicit categorical prohibition.',
    'If capture is documented: state perspective shifts from rope to tangled_rope/snare (extraction rather than coordination). Institutional beneficiary status becomes contested. FSM trigger possible if natural-law reading claims immutability despite capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_authority_capture, empirical, 'Potential regulatory capture of militia-eligibility criteria for covert disarmament').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(civic_right_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(civi_tr_t0, civic_right_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(civi_tr_t50, civic_right_reading, theater_ratio, 50, 0.52).
narrative_ontology:measurement(civi_tr_t100, civic_right_reading, theater_ratio, 100, 0.58).

% Extraction over time
narrative_ontology:measurement(civi_be_t0, civic_right_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(civi_be_t50, civic_right_reading, base_extractiveness, 50, 0.42).
narrative_ontology:measurement(civi_be_t100, civic_right_reading, base_extractiveness, 100, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(civic_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(civic_right_reading, 0.12).
narrative_ontology:affects_constraint(civic_right_reading, individual_right_reading).
narrative_ontology:affects_constraint(civic_right_reading, collective_right_reading).
narrative_ontology:affects_constraint(civic_right_reading, militia_functionality_threshold).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Second Amendment kernel (constraint_id: second_amendment_scope). The individual_right_reading and collective_right_reading are separate constraints with different ε values, beneficiary/victim sets, and perspectival profiles. All three affect the militia_functionality_threshold constraint, which determines whether the civic-militia coordination mechanism has any real referent in modern military organization. The three readings are linked but not entangled — each should be compiled and analyzed independently, with network edges indicating dependency and perspectival rivalry.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(civic_right_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
