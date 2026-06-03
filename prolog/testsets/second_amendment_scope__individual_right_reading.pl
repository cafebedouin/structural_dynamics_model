% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_scope__individual_right_reading, []).

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
 *   constraint_id: second_amendment_scope__individual_right_reading
 *   human_readable: Second Amendment Individual Right Reading: Firearms Ownership Unconnected to Militia Service
 *   domain: constitutional_law/political_theory/rights_jurisprudence
 *
 * SUMMARY:
 *   The individual-right reading of the Second Amendment interprets the
 *   constitutional text as protecting an individual's right to own firearms
 *   for self-defense purposes unconnected to militia service or civic
 *   obligation. This reading, institutionalized in landmark Supreme Court
 *   decisions (District of Columbia v. Heller, 2008; McDonald v. Chicago,
 *   2010), represents one of three competing constitutional interpretations
 *   of the same clause: the collective-right reading (Second Amendment
 *   protects state authority to maintain militias, not individual ownership);
 *   the civic-right reading (individual right exists but conditioned on
 *   militia participation or civic readiness). This constraint story
 *   instantiates the individual-right reading as a single, structurally
 *   distinct commitment with its own extractiveness profile,
 *   beneficiary/victim structure, and perspectival gap. The reading generates
 *   high extractiveness (0.58) because it extends the protected category to
 *   all individuals (not conditional on militia service) and forecloses many
 *   state regulatory pathways through strict scrutiny application. The
 *   measurement trajectory shows rising extractiveness over the interval
 *   (0.35→0.58) reflecting accumulating precedent narrowing regulatory space
 *   and rising theater ratio (0.22→0.38) indicating increasing reliance on
 *   formalized interpretive authority (originalism) to legitimize
 *   predetermined policy outcomes.
 *
 * KEY AGENTS:
 *   - Gun owners and Second Amendment advocates: Primary beneficiaries (powerful/mobile or organized/constrained) — captured by the constraint's expansion of protected category and constitutional foreclosure of regulation
 *   - State regulatory authorities: Primary victims (powerful/constrained) — regulatory capacity restricted by strict scrutiny; cannot implement public health measures the reading forecloses
 *   - Communities in high-firearm-density regions: Secondary victims (powerless/trapped) — bear injury/mortality costs from high gun density; cannot exit; state regulatory authority to protect them is constrained
 *   - Public health and safety coalition: Organized victims (organized/constrained) — constrained by regulatory foreclosure; respond with evidence-gathering and alternative pathways (scaffold logic)
 *   - Conservative legal coalition: Institutional beneficiary (institutional/arbitrage) — frames and litigates the reading; benefits from institutional entrenchment and ideological alignment
 *   - Originalist judicial doctrine: Institutional authority structure (institutional/arbitrage) — provides legitimacy through formalized historical-discovery method; functions theatrically to authorize predetermined outcome
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__individual_right_reading, 0.58).
domain_priors:suppression_score(second_amendment_scope__individual_right_reading, 0.48).
domain_priors:theater_ratio(second_amendment_scope__individual_right_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_scope__individual_right_reading, "Second Amendment Individual Right Reading: Firearms Ownership Unconnected to Militia Service").
narrative_ontology:topic_domain(second_amendment_scope__individual_right_reading, "constitutional_law/political_theory/rights_jurisprudence").

domain_priors:requires_active_enforcement(second_amendment_scope__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__individual_right_reading, 'd4ffe957-24df-48bd-be1c-f2a09a172643').
narrative_ontology:cs_kernel_codification('d4ffe957-24df-48bd-be1c-f2a09a172643', fixed_text).
narrative_ontology:cs_authority_grounding('d4ffe957-24df-48bd-be1c-f2a09a172643', lineage).
narrative_ontology:cs_interpretation_layer_present('d4ffe957-24df-48bd-be1c-f2a09a172643').
narrative_ontology:cs_reading_relation('d4ffe957-24df-48bd-be1c-f2a09a172643', second_amendment_scope__civic_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('d4ffe957-24df-48bd-be1c-f2a09a172643', second_amendment_scope__collective_right_reading, forecloses).
narrative_ontology:cs_axiom('d4ffe957-24df-48bd-be1c-f2a09a172643', foundational, prefatory_clause_non_binding).
narrative_ontology:cs_axiom_status(prefatory_clause_non_binding, holdable).
narrative_ontology:cs_axiom_grounding('d4ffe957-24df-48bd-be1c-f2a09a172643', prefatory_clause_non_binding, conventional).
narrative_ontology:cs_axiom('d4ffe957-24df-48bd-be1c-f2a09a172643', foundational, individual_right_antecedent_to_state).
narrative_ontology:cs_axiom_status(individual_right_antecedent_to_state, holdable).
narrative_ontology:cs_axiom_grounding('d4ffe957-24df-48bd-be1c-f2a09a172643', individual_right_antecedent_to_state, deontological).
narrative_ontology:cs_reference_frame('d4ffe957-24df-48bd-be1c-f2a09a172643', individual_liberty_armed_self_defense).
narrative_ontology:cs_drift_state('d4ffe957-24df-48bd-be1c-f2a09a172643', contemporary_post_heller_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d4ffe957-24df-48bd-be1c-f2a09a172643', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(second_amendment_scope__individual_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, gun_owners).
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, second_amendment_advocates).
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, conservative_legal_coalition).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, regulatory_authority_of_states).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, public_safety_regulatory_capacity).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, communities_subject_to_high_firearm_density).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GUN OWNER (ROPE) — Powerful individuals with exit options (can relocate to gun-friendly jurisdictions, affiliate with advocacy networks) experience this reading as pure coordination: the constraint coordinates a shared commitment to individual armed self-defense rights. The reading solves the collective action problem of maintaining constitutional protection against state encroachment. Extraction is minimal or negative (beneficiary experiences subsidy, not cost). Rope classification reflects genuine coordination function without asymmetric coercion.
constraint_indexing:constraint_classification(second_amendment_scope__individual_right_reading, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: COMMUNITIES IN HIGH-FIREARM-DENSITY REGIONS (SNARE) — Powerless agents in jurisdictions where firearms are prevalent experience maximal extraction: they are trapped within geographic and legal contexts where state regulatory authority to restrict gun density is severely constrained by this reading's strict scrutiny. They bear full cost (injury risk, regulatory incapacity) with no exit option (trapped by economic dependency, family ties, or lack of relocation capacity). No coordination benefit accrues to this agent. Maximum extraction, high suppression.
constraint_indexing:constraint_classification(second_amendment_scope__individual_right_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE REGULATORY AUTHORITY (TANGLED ROPE) — State actors (legislatures, public health agencies, law enforcement) experience a mixed constraint: they benefit from the coordination of stable property rights (individuals own firearms predictably, legally, integrated into commercial markets) but are severely constrained in their ability to regulate gun density, feature restrictions, and distribution. They have some agency (certain regulations survive strict scrutiny) but face high litigation burden and narrowed authority. The constraint provides coordination value (stable property rights regime) alongside asymmetric extraction (regulatory capacity asymmetrically reduced).
constraint_indexing:constraint_classification(second_amendment_scope__individual_right_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PUBLIC HEALTH AND SAFETY COALITION (SCAFFOLD) — Organized agents (public health authorities, anti-gun violence organizations, trauma centers) see this reading as a temporary coordination failure with a potential sunset: if empirical evidence mounting on gun injury outcomes accumulates sufficiently, if state-level experimentation in harm reduction produces measurable results, if litigation strategy successfully distinguishes narrow individual protection from sweeping regulatory foreclosure, the landscape could shift. This coalition has organized agency (data collection, alternative regulatory pathways like accident-prevention infrastructure) and perceives an exit path through evidence accumulation and legal evolution. Sunset logic applies if constitutional interpretation can be decoupled from permanent regulatory incapacity.
constraint_indexing:constraint_classification(second_amendment_scope__individual_right_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ORIGINALIST JUDICIAL DOCTRINE (PITON) — The originalist method that grounds this reading's legitimacy is largely inert/theatrical at the operative level: originalist jurisprudence claims to discover objective historical meaning ('original public meaning'), but the historical record is contested, the Framers' intent fragmentary and contradictory, and the mapping from 18th-century militia context to modern firearms is non-straightforward. The doctrine persists through institutional inertia (originalism has become institutionalized as a canonical interpretive school) and theatrical maintenance (formal legal reasoning performed with high rigor that creates legitimacy through procedural authority) rather than through demonstrated capacity to resolve constitutional ambiguity. The high theater_ratio reflects that originalist reasoning provides legitimacy cover for a predetermined policy outcome rather than generating that outcome from neutral method.
constraint_indexing:constraint_classification(second_amendment_scope__individual_right_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a natural-law or civilizational analytical perspective, individual self-defense is treated as a pre-political right requiring no justification beyond itself — grounded in the human condition's irreducible exposure to threat, forming a foundation of political legitimacy itself. This perspective sees the individual right reading as natural and inevitable, not as a contingent constitutional choice. However, this perspective is vulnerable to FSM detection: the 'naturalness' of individual self-defense may be a rationalization of institutional interests (modern firearms industries, Second Amendment advocacy coalitions) rather than a true natural law.
constraint_indexing:constraint_classification(second_amendment_scope__individual_right_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: SECOND AMENDMENT ADVOCACY INSTITUTIONS (TANGLED ROPE) — Institutional actors (NRA, conservative legal organizations, gun rights groups) experience mixed benefit and constraint: they benefit from the reading's expansion of individual rights (it validates their core mission, grows member bases, funds litigation campaigns) while being constrained by the need to defend the reading against political and empirical challenges. They provide coordination of gun owners' political voice but also extract organizational resources from members and shape discourse toward their preferred framings. They have real exit constraints (if the reading were overturned, organizational missions dissolve) even while wielding significant power.
constraint_indexing:constraint_classification(second_amendment_scope__individual_right_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_scope__individual_right_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(second_amendment_scope__individual_right_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(second_amendment_scope__individual_right_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_scope__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(second_amendment_scope__individual_right_reading, TR),
    TR >= 0.70.

:- end_tests(second_amendment_scope__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The individual-right reading creates broad beneficiary class (all individuals) while narrowing state regulatory capacity. This is asymmetric extraction: gun owners extract the benefit of protected access and foreclosed regulation; state authorities and communities extract the cost of regulatory incapacity. The value reflects that the reading does provide genuine coordination benefit (stable property rights, predictable legal regime) alongside extraction, distinguishing it from a pure snare. The rising trajectory (0.35→0.58) reflects accumulating judicial precedent narrowing regulatory pathways post-Heller, indicating the constraint's extractive force has strengthened as the reading's institutional embedding deepened. Suppression (0.48): Moderate. Multiple suppression mechanisms operate: (1) regulatory foreclosure by strict scrutiny (courts block many regulatory pathways); (2) discourse suppression (opposing views are labeled 'unconstitutional' rather than legitimate policy debate); (3) political suppression (gun-rights advocacy coalitions are well-funded and organized, while gun-control coalitions face resource asymmetries). But suppression is not total — state experimentation continues (background checks, licensing in some jurisdictions), public health research persists, and political opposition remains organized. Theater ratio (0.38): Moderate. Originalist jurisprudence provides legitimacy through formal historical-discovery method, but the method is contestable (historical record is ambiguous, originalism applies selectively across domains). The theater ratio is not as high as a pure piton because the individual-right reading does generate real coordination benefits and legal clarity, not merely performed legitimacy. Rising trajectory (0.22→0.38) reflects increasing formalization and theoretical elaboration of originalist doctrine, making the interpretive authority more theatrical as institutional commitment deepens.
 *
 * PERSPECTIVAL GAP:
 *   The individual-right reading generates the full spectrum of DR classification from different structural positions. Gun owners (powerful/mobile) experience this as pure coordination (Rope) — they enjoy protection and legal clarity without constraint. States (powerful/constrained) experience mixed coordination and extraction (Tangled Rope) — stable property rights provide coordination value, but regulatory foreclosure extracts capacity. Powerless communities in high-gun-density regions experience pure extraction (Snare) — they bear harm and cannot exit or regulate. Public health coalitions (organized) see a temporary problem with potential sunset (Scaffold) — evidence accumulation and legal innovation may eventually shift the landscape. The originalist doctrine (institutional) sees its own reasoning as degraded performance (Piton) — formal originalist method persists through institutional inertia rather than demonstrated resolution capacity. The natural-law perspective (analytical/civilizational) risks naturalizing what is contingent policy (Mountain/FSM candidate) — the claim that individual armed self-defense is an immutable natural law may rationalize constructed institutional interests. The perspectival gap reveals that all six DR types are simultaneously operative on this constraint — the classification is not undetermined but rather multiply determined across different observation sites.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from structural position: beneficiary status, victim status, and exit options. Gun owners (beneficiaries with mobile/arbitrage exit) have low d (~0.15), experiencing negative or minimal effective extraction (f(d)≈-0.01 to 0.02). States (mixed position: benefit from coordination, victimized by foreclosure; constrained exit) have moderate d (~0.55), experiencing moderate extraction (f(d)≈0.75). Powerless communities (victims with trapped exit) have high d (~0.95), experiencing maximum extraction (f(d)≈1.42). Organized coalitions (victims with constrained exit) have moderate d (~0.60), experiencing moderate extraction (f(d)≈0.85). The originalist doctrine (beneficiary through legitimacy authority; arbitrage exit through institutional maintenance) has low d (~0.20), but the piton classification derives from high theater rather than low chi. The natural-law perspective (analytical observer) has canonical d (~0.73), but FSM detection identifies the mountain classification as a false summit when beneficiaries are declared and institutional interests are identifiable.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through explicit recognition that all three readings (individual, civic, collective) are coherent constitutional interpretations occupying different positions on the power/exit/time/scope grid. Mandatrophy is dissolved by showing that the classification depends on which reading is adopted: individual reading generates tangled_rope (coordination + extraction mixed), civic reading would generate more balanced tangled_rope or even rope (civic obligation provides justification for regulation), collective reading would generate snare from individual gun owners' perspective (state control without individual protection). The 'correct' classification is not determinate from the text alone — it depends on which foundational commitment (individual liberty, civic participation, state authority) is prioritized. The empirical question (what does historical record show?) is distinct from the normative question (which reading should we adopt?). The framework allows both to be precise by treating this as one reading of a contested kernel, with omega variables documenting irreducible uncertainties about historical interpretation and axioms documenting foundational normative commitments.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_record_ambiguity,
    'Does the historical record of the Second Amendment''s framing establish that the Framers intended an individual right unconnected to militia service, or do primary sources reflect militia-conditional understanding?',
    'Systematic analysis of Founding-era sources (Federalist Papers, state ratification debates, Framing-era legal commentaries, correspondence of key framers). Determination of whether contemporary legal consensus on militia conditionality (pre-1970s) reflected true original understanding or subsequent misinterpretation.',
    'If historical record shows militia condition: individual right reading reclassifies toward civic_right_reading, dramatically raising base extraction on unregulated gun owners. If record shows unconnected individual right: individual right reading is strengthened as descriptively accurate, not merely politically motivated. If record is genuinely ambiguous: foundational axiom overridden_by_selectivity omegas multiply.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_record_ambiguity, empirical, 'Whether historical record supports unconditioned individual right or militia-conditioned right').

omega_variable(
    originalist_method_stability,
    'Is originalism a neutral historical-discovery method, or is it a selective interpretive frame that yields predetermined outcomes aligned with modern ideological preferences?',
    'Meta-analysis of originalist interpretation across constitutional domains: does originalist method produce consistent answers to constitutional questions, or do outcomes systematically align with conservative policy preferences? Comparison of originalist predictions in politically neutral vs. politically charged domains.',
    'If originalism is neutral method: originalist legitimacy is warranted, piton classification may be unfair. If originalism systematically selective: piton classification is confirmed (theatrical authority masking predetermined outcomes), and FSM detection fires on natural-law perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(originalist_method_stability, empirical, 'Whether originalist interpretation is neutral historical discovery or selective ideological frame').

omega_variable(
    empirical_outcome_of_broad_individual_right,
    'What is the causal relationship between broad individual-right interpretation and downstream public safety outcomes (gun homicide, suicide, accidental death, injury rates)?',
    'Comparative analysis of jurisdictions with broad vs. restricted individual-right interpretations, controlling for confounds (socioeconomic factors, gun ownership baseline, law enforcement capacity). Temporal analysis of safety outcomes pre/post landmark individual-right rulings (DC v. Heller, McDonald v. Chicago).',
    'If broad right causally increases harm: extractiveness classification is validated, snare perspective is strengthened. If no causal relationship or protective effect: individual right reading provides genuine coordination benefit with neutral/positive safety profile. If evidence is inconclusive: measurement uncertainty omega justifies multiple perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_outcome_of_broad_individual_right, empirical, 'Causal relationship between broad individual-right interpretation and public safety outcomes').

omega_variable(
    regulatory_capacity_foreclosure_scope,
    'Does the individual-right reading foreclose ALL state regulatory authority to restrict firearms (an absolute), or only some regulations (categorical strict scrutiny screen)?',
    'Detailed analysis of post-Heller case law: which categories of regulations survive strict scrutiny (licensing, background checks, mental health screening, felon prohibition)? Which categories are foreclosed (assault weapon bans, magazine capacity limits, zoning restrictions)? Is the foreclosure binary or scalar?',
    'If foreclosure is absolute: state regulatory capacity is maximally constrained, extraction is high. If foreclosure is categorical but partial: some regulatory paths remain open, extraction is moderate, states retain meaningful agency. Precision here directly affects tangled_rope vs snare classification for state perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_capacity_foreclosure_scope, empirical, 'Scope of regulatory authority foreclosed by individual-right reading').

omega_variable(
    militia_clause_interpretive_choice,
    'Is the ''well regulated Militia'' clause a condition on the right, a rationale that does not limit scope, or a discrete historical reference context that modern reading should decouple from?',
    'Linguistic and constitutional-theory analysis of prefatory vs operative clause structure. Comparative study of how other constitutional provisions handle prefatory/operative pairs (e.g., Fourth Amendment ''unreasonable'' searches). Determination of whether militia clause is logically binding on interpretation or merely explanatory.',
    'If militia clause is a binding condition: civic_right_reading is structurally required. If clause is non-binding rationale: individual_right_reading stands unconstrained. If clause is ambiguous in linguistic structure: foundational axiom (''prefatory_clause_non_binding'') is itself contestable, and reading_relations to civic_right_reading shift from coexists_with toward forecloses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_clause_interpretive_choice, conceptual, 'Whether militia clause constrains the scope of the right or is merely explanatory context').

omega_variable(
    readings_simultaneous_enforcement_paradox,
    'Can all three readings (collective, civic, individual) coexist as live legal interpretations within a single constitutional order, or does enforcement of one reading''s core commitments necessarily undermine others?',
    'Structural analysis of contradictory holdings: if individual right permits widespread gun ownership without militia participation, and collective-right reading reserves gun regulation to state legislatures, can both be constitutionally true simultaneously? Do these readings foreclose, coexist, or influence each other?',
    'If readings coexist: multiple valid constitutional interpretations exist simultaneously (high uncertainty, legitimacy through pluralism). If readings foreclose: some readings cannot be law while others are, and framework choice determines which reading becomes operative. If readings influence: one reading''s adoption changes operating conditions for others but does not rule them out logically.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(readings_simultaneous_enforcement_paradox, conceptual, 'Whether all three readings of Second Amendment scope can coexist as valid constitutional law').

omega_variable(
    false_summit_natural_law_claim,
    'Is the individual right to firearms ownership grounded in a genuine natural law (pre-political, immutable, universally applicable) or is the ''natural'' framing a constructed narrative serving identifiable modern beneficiaries?',
    'Genealogical analysis: when did the framing of ''natural right to self-defense via firearms'' emerge in political discourse? Does it predate modern firearms technology and mass casualty capacity? Is the framing consistent across cultures and historical periods, or is it specific to modern Anglo-American constitutional tradition? Do identifiable institutional beneficiaries (firearms industry, advocacy coalitions) have incentive to naturalize what is contingent institutional choice?',
    'If natural law is genuine: mountain classification is correct, FSM does not fire. If framing is constructed/beneficiary-serving: FSM reclassifies to tangled_rope or snare, revealing hidden extraction. If naturalization is partial (genuine self-defense need + constructed modern expansion): omega documents the boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_claim, conceptual, 'Whether individual firearm right is grounded in natural law or constructed institutional narrative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__individual_right_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sa_ind_theater_t0, second_amendment_scope__individual_right_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(sa_ind_theater_t20, second_amendment_scope__individual_right_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement(sa_ind_theater_t40, second_amendment_scope__individual_right_reading, theater_ratio, 40, 0.38).

% Extraction over time
narrative_ontology:measurement(sa_ind_extract_t0, second_amendment_scope__individual_right_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sa_ind_extract_t20, second_amendment_scope__individual_right_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(sa_ind_extract_t40, second_amendment_scope__individual_right_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(sa_ind_suppress_t0, second_amendment_scope__individual_right_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(sa_ind_suppress_t20, second_amendment_scope__individual_right_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(sa_ind_suppress_t40, second_amendment_scope__individual_right_reading, suppression_requirement, 40, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_scope__individual_right_reading, identity_coordination).
narrative_ontology:affects_constraint(second_amendment_scope__individual_right_reading, second_amendment_scope__civic_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__individual_right_reading, second_amendment_scope__collective_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__individual_right_reading, second_amendment_scope__historical_record_ambiguity).
narrative_ontology:affects_constraint(second_amendment_scope__individual_right_reading, firearms_market_extraction_dynamics).
narrative_ontology:affects_constraint(second_amendment_scope__individual_right_reading, state_public_health_regulatory_authority).

% DUAL FORMULATION NOTE:
% The Second Amendment scope is a contested kernel with three distinct readings (individual, civic, collective). Each reading instantiates a structurally distinct constraint with different extractiveness, beneficiary/victim structures, and perspectival classifications. This file documents the individual_right_reading. The sibling readings appear in separate constraint stories. All three are linked via network.affects_constraints to enable analysis of how adopting one reading constrains or enables the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_scope__individual_right_reading, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
