% ============================================================================
% CONSTRAINT STORY: constitutional_text__popular_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text__popular_sovereignty_reading, []).

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
 *   constraint_id: constitutional_text__popular_sovereignty_reading
 *   human_readable: Constitutional Text as Popular Sovereignty (Democratic Amendment Authority)
 *   domain: constitutional_theory/political_philosophy/comparative_law
 *
 * SUMMARY:
 *   This constraint story instantiates the popular sovereignty reading of the
 *   constitutional text kernel. The reading claims that legitimate
 *   constitutional authority derives from the constituent power of the demos
 *   — that neither courts nor legislatures hold supreme interpretive
 *   authority, but rather that 'the people' retain the ultimate power to
 *   establish, revise, and reinterpret constitutional meaning through formal
 *   amendment, constitutional convention, or revolutionary reconstitution.
 *   This reading is structurally distinct from the judicial supremacy reading
 *   (courts are the authoritative interpreters) and the legislative
 *   sovereignty reading (elected representatives are the supreme authority).
 *   The popular sovereignty reading creates a tangled rope constraint because
 *   it coordinates democratic mobilization while simultaneously creating
 *   extraction pressures: it makes institutional legitimacy contingent on
 *   popular consent while imposing high barriers (super-majority
 *   requirements, federalism, entrenched interests) that suppress actual
 *   popular amendment capacity. The constraint is not a pure mountain of
 *   political theory (an immutable feature of democracy) nor a pure rope
 *   (simple coordination); rather, it exhibits the mixed character of an
 *   institutionalized commitment that serves both coordination and extraction
 *   functions depending on which agent perspective is adopted.
 *
 * KEY AGENTS:
 *   - Democratic mobilization / social movements: Primary beneficiary (organized/constrained) — the principle legitimates popular constitutional initiative and provides a framework for collective self-governance
 *   - Institutional stability apparatus: Primary victim (powerless/trapped) — courts and legislatures experience popular sovereignty as a perpetual threat to institutional continuity and expertise-based authority
 *   - Expert constitutional interpretation: Secondary victim (moderate/constrained) — legal scholars and judges bear the extraction of continual subordination to potential popular override
 *   - Amendment mechanism: Coordination agent (institutional/mobile) — the formal procedures for constitutional amendment instantiate the principle but also constrain it through super-majority thresholds
 *   - Dominant political coalition: Moderate agent (powerful/arbitrage) — experiences the principle as both legitimating their rule and suppressing their dominance; can often reshape meaning through interpretation without formal amendment
 *   - Analytical observer: Epistemically external (analytical/analytical) — risks naturalizing the principle as an immutable feature rather than a contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__popular_sovereignty_reading, 0.38).
domain_priors:suppression_score(constitutional_text__popular_sovereignty_reading, 0.48).
domain_priors:theater_ratio(constitutional_text__popular_sovereignty_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__popular_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text__popular_sovereignty_reading, "Constitutional Text as Popular Sovereignty (Democratic Amendment Authority)").
narrative_ontology:topic_domain(constitutional_text__popular_sovereignty_reading, "constitutional_theory/political_philosophy/comparative_law").

domain_priors:requires_active_enforcement(constitutional_text__popular_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__popular_sovereignty_reading, 'a839742b-86b4-458d-b80b-04e24b426b89').
narrative_ontology:cs_kernel_codification('a839742b-86b4-458d-b80b-04e24b426b89', fixed_text).
narrative_ontology:cs_authority_grounding('a839742b-86b4-458d-b80b-04e24b426b89', lineage).
narrative_ontology:cs_interpretation_layer_present('a839742b-86b4-458d-b80b-04e24b426b89').
narrative_ontology:cs_reading_relation('a839742b-86b4-458d-b80b-04e24b426b89', constitutional_text__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('a839742b-86b4-458d-b80b-04e24b426b89', constitutional_text__legislative_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('a839742b-86b4-458d-b80b-04e24b426b89', foundational, ultimate_authority_resides_in_demos).
narrative_ontology:cs_axiom_status(ultimate_authority_resides_in_demos, holdable).
narrative_ontology:cs_axiom_grounding('a839742b-86b4-458d-b80b-04e24b426b89', ultimate_authority_resides_in_demos, deontological).
narrative_ontology:cs_axiom('a839742b-86b4-458d-b80b-04e24b426b89', foundational, institutions_derive_legitimacy_from_popular_consent).
narrative_ontology:cs_axiom_status(institutions_derive_legitimacy_from_popular_consent, holdable).
narrative_ontology:cs_axiom_grounding('a839742b-86b4-458d-b80b-04e24b426b89', institutions_derive_legitimacy_from_popular_consent, deontological).
narrative_ontology:cs_reference_frame('a839742b-86b4-458d-b80b-04e24b426b89', constituent_power_retained_by_generations).
narrative_ontology:cs_drift_state('a839742b-86b4-458d-b80b-04e24b426b89', contemporary_democratic_practice, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a839742b-86b4-458d-b80b-04e24b426b89', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(constitutional_text__popular_sovereignty_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, democratic_mobilization).
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, constitutional_amendment_capacity).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, institutional_stability).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, expert_governance).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INSTITUTIONAL STABILITY (SNARE) — Courts and legislatures experience the popular sovereignty principle as a perpetual threat to institutional continuity. They bear the suppressive cost of uncertainty: every constitutional ruling lives under the shadow of potential popular override through amendment or convention. No exit from the constraint — institutions cannot opt out of being subject to popular revision. Maximum suppression experienced by those whose authority derives from institutional longevity rather than immediate popular consent.
constraint_indexing:constraint_classification(constitutional_text__popular_sovereignty_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INSTITUTIONALIST LEGAL THEORY (TANGLED ROPE) — Legal scholars committed to rule of law and expert constitutional interpretation experience the constraint as coordination (they benefit from clarity about how amendment works) and extraction (their interpretive authority is continually subordinated to potential popular override). Constrained by the need to maintain interpretive legitimacy while acknowledging popular sovereignty; cannot exit entirely without abandoning foundational constitutional commitments.
constraint_indexing:constraint_classification(constitutional_text__popular_sovereignty_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DEMOCRATIC COALITIONS (ROPE) — Organized popular movements see the popular sovereignty principle as pure coordination: it enables collective constitutional self-governance, provides a legitimacy framework for popular mobilization, and creates the structural capacity for transformation. The constraint coordinates democratic agency. Constrained by high barriers to actual amendment (super-majority thresholds, federalism, entrenched interests) but experiences the principle itself as enabling rather than extractive.
constraint_indexing:constraint_classification(constitutional_text__popular_sovereignty_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: AMENDMENT MECHANISM (ROPE) — The formal procedures for amendment (Article V in US context, comparable mechanisms in other democracies) experience the constraint as a coordination function: the amendment process IS the institutional implementation of popular sovereignty. Pure coordination with low extraction — the mechanism serves no party's interest except the people's collective interest in self-government. Mobile because the mechanism can be invoked or abandoned depending on political will.
constraint_indexing:constraint_classification(constitutional_text__popular_sovereignty_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: DOMINANT COALITION (TANGLED ROPE) — A politically dominant coalition experiences the popular sovereignty principle as both coordination (legitimacy framework for their rule) and extraction (they bear suppressive burden of needing to maintain popular support and constitutional legitimacy). Arbitrage exit because dominant coalitions can often reshape constitutional meaning through judicial and legislative interpretation without formal amendment. Lower extraction than subordinate groups because they have interpretive power.
constraint_indexing:constraint_classification(constitutional_text__popular_sovereignty_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN — FALSE SUMMIT CANDIDATE) — From a civilizational view, this perspective risks naturalizing the popular sovereignty principle as an immutable feature of democratic legitimacy: 'democracies by definition derive authority from the people.' This classification appears natural-law-like (ε ≤ 0.25, suppression low) but the structural data reveals beneficiaries and extraction mechanisms that contradict mountain classification. The false summit detector will identify this as naturalization.
constraint_indexing:constraint_classification(constitutional_text__popular_sovereignty_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__popular_sovereignty_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(constitutional_text__popular_sovereignty_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(constitutional_text__popular_sovereignty_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(constitutional_text__popular_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38, rising to 0.45): Moderate and rising. The popular sovereignty principle carries real extraction cost for institutional actors: they must maintain legitimacy through democratic responsiveness, bear uncertainty about potential popular override, and face suppression of their expert authority. However, the extraction is not severe (not a snare) because institutions retain significant interpretive power through judicial and legislative action, and formal amendment is genuinely difficult (providing some stability). The rising trajectory over the 200-year interval reflects increasing democratization and popular mobilization capacity, which raises the effective extraction pressure on institutions. Suppression (0.48, rising to 0.58): Moderate-high and rising. Institutional actors face substantial barriers to ignoring popular sovereignty: constitutional legitimacy requires at least rhetorical deference to the principle, amendment threats create uncertainty, and sustained popular mobilization can eventually change constitutional meaning (through formal amendment or informal norm cascade). The rising trajectory reflects the increasing capacity of popular movements to organize and the erosion of elite gatekeeping on constitutional interpretation. Theater ratio (0.55, rising to 0.63): Moderate and rising. The principle itself is functional coordination (the amendment process works as designed), but the measured theater reflects the gap between the principle's rhetoric (ultimate popular authority) and its practice (extremely difficult formal amendment, reliance on informal interpretation by elites). Theater rises over time as gap widens between principle and practice — the constraint becomes increasingly performative as formal amendment barriers prove insurmountable and actual popular sovereignty operates through informal channels.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a deep perspectival gap rooted in structural position. Institutional stability actors see a snare: they are trapped in a constraint that subordinates their authority without exit, and they experience maximal suppression. Democratic coalitions see rope: pure coordination enabling their collective agency. Institutionalist scholars see tangled rope: they benefit from clarity about amendment procedures (coordination) but experience extraction of their interpretive authority. Dominant coalitions see tangled rope but with lower experienced extraction due to arbitrage capacity: they can reshape constitutional meaning through interpretation without triggering formal amendment. The analytical observer at the civilizational scope risks seeing a mountain (democracy by definition rests on popular sovereignty), but the measurement trajectory and structural data reveal this as a false summit: the principle is not an immutable law but a contingent institutional commitment with clear beneficiaries (popular mobilization) and victims (institutional stability, expert governance).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from agent structural relationships to this specific constraint. Institutional stability actors are victims with trapped exit, producing high d (approximately 0.92), yielding high f(d) and maximum experienced extraction. Democratic coalitions are beneficiaries with constrained exit (high barriers to actual amendment), producing moderate-low d (approximately 0.35), yielding moderate f(d) and positive experienced extraction. Amendment mechanism actors are pure beneficiaries with mobile exit (formality can be invoked or bypassed), producing very low d (approximately 0.08). Dominant coalitions are beneficiaries with arbitrage exit (can reshape meaning through interpretation), producing low d (approximately 0.15). The perspectival gap emerges from these differentiated d values: the institutional victim experiences χ ≈ 0.38 × 1.35 × 1.0 ≈ 0.51 (high effective extraction); the democratic beneficiary experiences χ ≈ 0.38 × 0.50 × 1.0 ≈ 0.19 (low effective extraction, benefits outweigh costs).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that this constraint is simultaneously a coordination mechanism (for democratic self-governance) and an extraction mechanism (for institutional subordination). The tangled rope classification captures this duality: the principle genuinely coordinates democratic mobilization (beneficiaries exist, coordination function is real) while simultaneously extracting from institutional actors who prefer stability and expertise-based authority (victims exist, extraction is asymmetric, enforcement is active). The measurement trajectory shows the growing tension: as theater ratio rises, the constraint increasingly splits into formal procedure (coordination) and informal practice (extraction) — the gap between the principle's rhetoric and its practice is the wedge revealing mandatrophy rather than concealing it. The analytical mountain perspective is false summit: naturalizing the principle as immutable locks in institutional subordination as fate rather than choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    amendment_barrier_effectiveness,
    'Do super-majority amendment thresholds represent coordination cost (managing consensus) or extraction mechanism (entrenching minority rule)?',
    'Historical analysis: comparison of amendment rates across democracies with varying thresholds; assessment of whether threshold prevents ''volatile'' change (coordination benefit) or enables ''locked-in'' inequality (extraction harm)',
    'If coordination: extractiveness drops to 0.25, constraint becomes Rope at moderate/generational. If extraction: extractiveness rises to 0.55, constraint becomes Snare at powerless view and institutional stability view.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(amendment_barrier_effectiveness, empirical, 'Whether super-majority amendment requirements enable stable democracy or entrench minority rule').

omega_variable(
    popular_mobilization_capacity,
    'Can popular movements realistically achieve constitutional amendment through formal mechanisms, or is the popular sovereignty principle performative (theater) when formal barriers are too high?',
    'Analysis of amendment success rates by mobilization type (broad grassroots vs elite-driven); assessment of whether non-formal modes (convention, revolution, norm cascade) are the actual mechanisms of popular sovereignty',
    'If formal amendment is accessible: popular sovereignty is functional (Rope). If formal barriers are insurmountable: popular sovereignty is theatrical (theater_ratio rises, constraint becomes Piton). If popular mobilization must bypass formal mechanisms: constraint bifurcates into two stories (formal procedure vs actual democratic expression).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(popular_mobilization_capacity, empirical, 'Whether formal amendment procedures provide real access to popular constitutional authority').

omega_variable(
    judicial_interpretation_sovereignty_tension,
    'When courts interpret the constitution using living constitutionalism, are they instantiating popular sovereignty (reflecting evolved popular will) or usurping it (imposing elite interpretation)?',
    'Process tracing of constitutional interpretation: identification of whether court decisions align with or diverge from documented popular preference; assessment of whether living constitutionalism is hermeneutic (revealing popular meaning) or substantive (substituting judicial judgment)',
    'If hermeneutic alignment: constraint becomes Rope (courts are implementing popular sovereignty through interpretation). If substitution: constraint becomes Snare from popular view (courts extract interpretive authority from the people). If mixed: different perspectives see different ε values — indicates constraint family decomposition needed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_interpretation_sovereignty_tension, empirical, 'Whether living constitutionalism implements or usurps popular sovereignty').

omega_variable(
    founding_moment_authority,
    'Does the constraint define popular sovereignty as authority exercised at the founding moment (ratification of constitution) or as ongoing authority retained by each generation?',
    'Textual analysis of constitutional founding documents; historical reconstruction of what ratifiers and founders intended by ''We the People''; examination of whether later generations are viewed as bound by founding will or as retaining sovereign authority',
    'If founding-moment-only: popular sovereignty is fixed at ratification, making it a historical precedent rather than an ongoing constraint (ε drops to 0.10, becomes Mountain). If generational: each generation retains authority, making the constraint an active structural tension (ε stays at 0.38).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_moment_authority, conceptual, 'Whether popular sovereignty is founding-moment authority or ongoing generational authority').

omega_variable(
    reading_foreclosure_boundaries,
    'Does this reading (popular sovereignty as meta-authority) logically foreclose the judicial supremacy reading or merely coexist with it as a different perspective on the same constitutional commitment?',
    'Structural analysis: examination of whether courts can simultaneously hold that they are supreme interpreters AND that the people retain ultimate authority. If holding both requires compartmentalization (courts are supreme within their sphere, but people are ultimate in the broader constitutional order), then readings coexist. If holding both produces logical contradiction, then foreclosure applies.',
    'Foreclosure: the readings are genuine alternatives, and adoption of one rules out the other within a single constitutional framework. Coexistence: both readings are live positions in contemporary constitutional theory, held by different schools and at different moments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_boundaries, conceptual, 'Whether popular sovereignty reading forecloses judicial supremacy or coexists with it').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__popular_sovereignty_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(consti_pop_tr_t0, constitutional_text__popular_sovereignty_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(consti_pop_tr_t100, constitutional_text__popular_sovereignty_reading, theater_ratio, 100, 0.55).
narrative_ontology:measurement(consti_pop_tr_t200, constitutional_text__popular_sovereignty_reading, theater_ratio, 200, 0.63).

% Extraction over time
narrative_ontology:measurement(consti_pop_be_t0, constitutional_text__popular_sovereignty_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(consti_pop_be_t100, constitutional_text__popular_sovereignty_reading, base_extractiveness, 100, 0.38).
narrative_ontology:measurement(consti_pop_be_t200, constitutional_text__popular_sovereignty_reading, base_extractiveness, 200, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(consti_pop_su_t0, constitutional_text__popular_sovereignty_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(consti_pop_su_t100, constitutional_text__popular_sovereignty_reading, suppression_requirement, 100, 0.48).
narrative_ontology:measurement(consti_pop_su_t200, constitutional_text__popular_sovereignty_reading, suppression_requirement, 200, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__popular_sovereignty_reading, identity_coordination).
narrative_ontology:affects_constraint(constitutional_text__popular_sovereignty_reading, constitutional_text__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_text__popular_sovereignty_reading, constitutional_text__legislative_sovereignty_reading).
narrative_ontology:affects_constraint(constitutional_text__popular_sovereignty_reading, amendment_mechanism_capture).
narrative_ontology:affects_constraint(constitutional_text__popular_sovereignty_reading, constitutional_convention_deadlock).

% DUAL FORMULATION NOTE:
% The popular sovereignty reading is one member of the constitutional text constraint family. The kernel (written constitution) is shared across three readings: popular sovereignty, judicial supremacy, and legislative sovereignty. Each reading produces a distinct constraint with different ε values and different beneficiary/victim structures. This story (popular sovereignty) affects the sibling stories (judicial supremacy and legislative sovereignty) as competes-with and influences relationships: adoption of this reading in a jurisdiction influences whether the judicial supremacy reading can maintain institutional authority. Network links enable cross-reading contamination analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_text__popular_sovereignty_reading, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
