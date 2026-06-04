% ============================================================================
% CONSTRAINT STORY: directive_principles_part_iv__non_justiciable_conscience_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_directive_principles_part_iv__non_justiciable_conscience_reading, []).

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
 *   constraint_id: directive_principles_part_iv__non_justiciable_conscience_reading
 *   human_readable: Directive Principles Part IV: Non-Justiciable Conscience Reading
 *   domain: constitutional_law/indian_constitutionalism
 *
 * SUMMARY:
 *   The Indian Constitution's Part IV (Articles 36–51) declares directive
 *   principles of state policy: the right to livelihood, the right to health,
 *   the right to education, the right to adequate wages, the abolition of
 *   child labor. These are constitutional promises. But they are deliberately
 *   non-justiciable — a citizen cannot sue the state for breach. Article 37
 *   states: 'The provisions contained in this Part shall not be enforceable
 *   by any court, but the principles therein laid down are nevertheless
 *   fundamental in the governance of the country, and it shall be the duty of
 *   the State to apply these principles in making laws.' This reading
 *   instantiates ONE interpretation of Part IV: the principles are conscience
 *   without compulsion. They instruct the state (via Article 37's duty
 *   language) and arm the voter (by providing electoral leverage — vote for
 *   governments that respect the conscience), but they do not arm the
 *   litigant (do not create justiciable rights). This reading stands in
 *   contest with two siblings: the harmonization ascendancy reading (courts
 *   have progressively read Part IV into Part III, discovering justiciable
 *   rights in the non-justiciable promises) and the welfare blueprint reading
 *   (Part IV is the planned society's outline, specifying the state's
 *   commitment to concrete welfare architecture). This constraint story
 *   instantiates the non-justiciable conscience reading as a clean
 *   ε-invariant claim, with its own beneficiary/victim structure, its own
 *   perspectival gap, and its own CS structure documenting how it forecloses
 *   or coexists with siblings.
 *
 * KEY AGENTS:
 *   - Welfare Claimant (Powerless/Trapped): Primary victim — cites Part IV in court, finds no remedy; structurally prevented from accessing the promise
 *   - Progressive Judicial Bench (Moderate/Constrained): Secondary victim and secondary beneficiary — constrained by non-justiciability but expanding interpretive reach; extracts institutional power while coordinating welfare expansion
 *   - Legislative-Executive State (Institutional/Arbitrage): Primary beneficiary — receives the legitimacy of welfare promises without binding obligation; experiences the constraint as pure coordination of state prerogatives
 *   - Constitutional Text & Drafting Intent (Institutional/Arbitrage): Analytical actor — the explicit design choice encoded in Articles 36–51 and Article 37's language; degrades over time (Piton perspective)
 *   - Electoral Accountability Coalition (Organized/Mobile): Secondary beneficiary — interprets Part IV as a mechanism for voter-mediated accountability; seeks to activate electoral enforcement through civil society mobilization
 *   - Analytical Observer (Analytical/Analytical): Civilizational view — risks naturalizing the non-justiciable boundary as a universal constitutional necessity rather than a contingent design choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(directive_principles_part_iv__non_justiciable_conscience_reading, 0.52).
domain_priors:suppression_score(directive_principles_part_iv__non_justiciable_conscience_reading, 0.68).
domain_priors:theater_ratio(directive_principles_part_iv__non_justiciable_conscience_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(directive_principles_part_iv__non_justiciable_conscience_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(directive_principles_part_iv__non_justiciable_conscience_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(directive_principles_part_iv__non_justiciable_conscience_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(directive_principles_part_iv__non_justiciable_conscience_reading, tangled_rope).
narrative_ontology:human_readable(directive_principles_part_iv__non_justiciable_conscience_reading, "Directive Principles Part IV: Non-Justiciable Conscience Reading").
narrative_ontology:topic_domain(directive_principles_part_iv__non_justiciable_conscience_reading, "constitutional_law/indian_constitutionalism").

domain_priors:requires_active_enforcement(directive_principles_part_iv__non_justiciable_conscience_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(directive_principles_part_iv__non_justiciable_conscience_reading, '4901b5ff-2274-4ccf-bda6-97e476fca07e').
narrative_ontology:cs_kernel_codification('4901b5ff-2274-4ccf-bda6-97e476fca07e', formalized).
narrative_ontology:cs_authority_grounding('4901b5ff-2274-4ccf-bda6-97e476fca07e', extraction).
narrative_ontology:cs_interpretation_layer_present('4901b5ff-2274-4ccf-bda6-97e476fca07e').
narrative_ontology:cs_reading_relation('4901b5ff-2274-4ccf-bda6-97e476fca07e', directive_principles_part_iv__harmonization_ascendancy_reading, coexists_with).
narrative_ontology:cs_reading_relation('4901b5ff-2274-4ccf-bda6-97e476fca07e', directive_principles_part_iv__welfare_blueprint_reading, coexists_with).
narrative_ontology:cs_axiom('4901b5ff-2274-4ccf-bda6-97e476fca07e', foundational, non_justiciable_conscience_is_binding).
narrative_ontology:cs_axiom_status(non_justiciable_conscience_is_binding, holdable).
narrative_ontology:cs_axiom_grounding('4901b5ff-2274-4ccf-bda6-97e476fca07e', non_justiciable_conscience_is_binding, deontological).
narrative_ontology:cs_axiom('4901b5ff-2274-4ccf-bda6-97e476fca07e', foundational, judicial_enforcement_exceeds_proper_role).
narrative_ontology:cs_axiom_status(judicial_enforcement_exceeds_proper_role, holdable).
narrative_ontology:cs_axiom_grounding('4901b5ff-2274-4ccf-bda6-97e476fca07e', judicial_enforcement_exceeds_proper_role, deontological).
narrative_ontology:cs_reference_frame('4901b5ff-2274-4ccf-bda6-97e476fca07e', article_37_conscience_constraint).
narrative_ontology:cs_drift_state('4901b5ff-2274-4ccf-bda6-97e476fca07e', contemporary_harmonization_pressure, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('4901b5ff-2274-4ccf-bda6-97e476fca07e', '').
narrative_ontology:cs_kernel_id(directive_principles_part_iv__non_justiciable_conscience_reading, directive_principles_part_iv).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(directive_principles_part_iv__non_justiciable_conscience_reading, legislative_executive_discretion).
narrative_ontology:constraint_beneficiary(directive_principles_part_iv__non_justiciable_conscience_reading, state_policy_priority_setting).
narrative_ontology:constraint_victim(directive_principles_part_iv__non_justiciable_conscience_reading, court_accessible_welfare_claimants).
narrative_ontology:constraint_victim(directive_principles_part_iv__non_justiciable_conscience_reading, justiciable_rights_framework).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WELFARE CLAIMANT (SNARE) — A poor citizen citing Part IV (right to livelihood, right to education, right to health) in court finds the bench cannot act — the principles are deliberately non-justiciable by design. Maximum suppression: the legal remedy appears available but is structurally unavailable. The claimant has no exit (cannot afford alternative provision, cannot access legislative remedy). High extraction: state extracts compliance-without-obligation, appearing to promise welfare while retaining complete discretion on delivery.
constraint_indexing:constraint_classification(directive_principles_part_iv__non_justiciable_conscience_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PROGRESSIVE JUDICIAL BRANCH (TANGLED ROPE) — Courts reading Part IV into Part III via 'right to life includes livelihood' (Olga Tellis, Unni Krishnan, Directive Principles-in-Part-III methods) benefit from interpretive authority expansion while facing resistance from strict constitutional textualists. Constrained by text and institutional legitimacy; they coordinate a welfare function (judicial reach for the poor) while extracting interpretive power (reshaping constitutional doctrine without amendment). Mixed coordination-extraction: genuine welfare expansion alongside institutional aggrandizement.
constraint_indexing:constraint_classification(directive_principles_part_iv__non_justiciable_conscience_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LEGISLATIVE-EXECUTIVE STATE (ROPE) — The state apparatus benefits from Part IV's non-justiciability: it receives the political legitimacy of welfare promises without the binding obligation of court-enforceable rights. The design coordinates with the state's needs (discretion, budget flexibility, policy priority-setting) while providing political-rhetorical cover. Net beneficiary — experiences the constraint as pure coordination of state prerogatives. Low extraction cost: the state simply exercises its constitutionally-delegated role.
constraint_indexing:constraint_classification(directive_principles_part_iv__non_justiciable_conscience_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUTIONAL TEXT & DRAFTING INTENT (PITON) — The explicit non-justiciability of Part IV is a formally codified design choice (Articles 36-51 versus 12-35), but the function it was meant to serve (state guidance, electoral accountability, conscience-constraint) has degraded through non-enforcement. The constitutional provision persists through inertia (it is still there in the text) but the mechanism it relied on (legislative responsiveness to voter conscience rather than court mandate) has atrophied. High theater: the text remains, the interpretive ritual of debating whether Part IV 'can' apply continues, but the primary coordination function (voter-mediated accountability) is diminished as electoral politics treats welfare as policy discretion rather than constitutional conscience.
constraint_indexing:constraint_classification(directive_principles_part_iv__non_justiciable_conscience_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / CONSTITUTIONAL SEPARATIONISM (MOUNTAIN) — From a civilizational/universal perspective, the separation of justiciable rights from non-justiciable principles may appear as an immutable structure of constitutional architecture: some claims are inherently unsuitable for judicial enforcement (political economy questions, resource allocation, policy tradeoffs), and this boundary is a natural limit of the rule of law. However, this perspective risks naturalizing what this reading calls 'conscience without compulsion' — a specific design choice about the distribution of institutional power — as a universal logical necessity. The engine's false summit detector will flag this as committer capture: the mountain classification naturalizes the non-justiciable boundary, whereas the boundary itself is the contested kernel.
constraint_indexing:constraint_classification(directive_principles_part_iv__non_justiciable_conscience_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: ELECTORAL ACCOUNTABILITY COALITION (SCAFFOLD) — Organized civil-society actors (right-to-food campaigns, right-to-health movements, voter mobilization initiatives) interpret Part IV as a temporary design awaiting electoral pressure: if voters consistently demand welfare accountability through electoral choice, the state will gradually convert non-justiciable principles into enforceable rights through constitutional amendment or legislative action. This perspective sees the current constraint as a sunset — a deliberate delay mechanism that delegates adjudication to the electorate. Theater is moderate (below rope threshold) because the coalition maintains real alternative channels: voter mobilization, legislative pressure, constitutional convention processes. The coalition has exit: they can work outside the court system. This makes the constraint a scaffold, not a snare or tangled rope.
constraint_indexing:constraint_classification(directive_principles_part_iv__non_justiciable_conscience_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(directive_principles_part_iv__non_justiciable_conscience_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(directive_principles_part_iv__non_justiciable_conscience_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(directive_principles_part_iv__non_justiciable_conscience_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(directive_principles_part_iv__non_justiciable_conscience_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(directive_principles_part_iv__non_justiciable_conscience_reading, TR),
    TR >= 0.70.

:- end_tests(directive_principles_part_iv__non_justiciable_conscience_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The non-justiciable design extracts compliance-without-obligation from the state's perspective (state gains legitimacy while retaining discretion) and extracts remedy-without-recourse from the claimant's perspective (apparent right, unavailable relief). The extractiveness is not extreme (0.72+) because the electoral accountability mechanism, if operative, constrains state discretion through political pressure rather than legal mandate. The reading's core claim is that Part IV is deliberately designed to operate through conscience and electoral accountability, not judicial enforcement — this is the intended function, not a failure. Extractiveness measures how much the constraint extracts from those it governs; the measurement shows increasing extractiveness (0.35 → 0.52) over the interval as judicial capacity for welfare expansion (via harmonization) creates pressure on the non-justiciable boundary, making the separation of justiciable/non-justiciable more visible and more contentious. Suppression (0.68): Moderate-high and stable. The suppression is structural and intentional — deliberate removal of judicial remedy. But suppression is not total (does not reach 0.85): voters retain electoral accountability mechanisms, civil society can mobilize, legislatures can amend, the state can voluntarily implement. The stability of suppression reflects that the design choice remains intact despite harmonization pressure. Theater ratio (0.55): Moderate. The ritual of debating whether Part IV 'can' apply, the language of 'fundamental in the governance of the country,' and the continued citation of non-justiciability despite judicial circumvention via Part III harmonization all constitute theater. But the theater is not dominant (below 0.70 piton threshold) because the political effects are real — Part IV does constrain electoral politics and legislative discourse, even if it does not constrain courts. The stable theater trajectory reflects that the performative aspect persists despite doctrinal evolution.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a critical gap between the welfare claimant's experience (snare: no remedy available) and the state's experience (rope: pure coordination with no extraction cost). The progressive bench occupies a middle position (tangled rope: coordinating welfare expansion while extracting interpretive power). The constitutional text perspective shows piton degradation: the design persists but the function it relied on (electoral accountability) has weakened as political contestation over Part IV has shifted toward the courts rather than the electorate. The scaffold perspective (electoral coalition) sees the constraint as temporary with a sunset — if voters enforce welfare accountability, the non-justiciable boundary will erode through amendment or harmonization. The mountain perspective (analytical observer) risks naturalizing the boundary as immutable constitutional architecture. The gap reveals the kernel contest: is non-justiciability a principled constitutional limit (mountain-like), a contingent institutional choice (tangled rope or snare), or a temporary design awaiting fuller realization (scaffold)? Different readings activate different perspectival gaps — the harmonization reading would flip the primary beneficiary from 'legislative-executive' to 'progressive bench,' and the welfare blueprint reading would flip the victim set from 'court-accessible claimants' to 'those excluded from state welfare architecture.' This reading's specific gap (state gains conscience-obligation without judicial obligation) is the signature of the non-justiciable conscience interpretation.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality for each perspective is derived from beneficiary/victim status and exit options. The welfare claimant (powerless/trapped) has d ≈ 0.95 (full victim), producing high f(d) and high experienced chi. The state (institutional/arbitrage) has d ≈ 0.05 (full beneficiary), producing negative f(d) and negative experienced chi (the constraint benefits the state with no cost). The progressive bench (moderate/constrained) has d ≈ 0.55 (mixed beneficiary-victim), producing moderate f(d) ≈ 0.75 and moderate chi. The electoral coalition (organized/mobile) has d ≈ 0.45 (some exit, constrained victim status), producing moderate chi. Each agent's structural position relative to the extraction flow determines their experienced intensity and classification. The reading's claim is that the non-justiciable design is intended to route welfare accountability through electoral/legislative channels rather than judicial channels — this produces the specific directionality pattern where the state benefits, the claimant bears cost, and intermediate actors (bench, organized coalitions) experience mixed effects.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves mandatrophy by declaring its own interpretation of Part IV as the authoritative reading — conscience without compulsion, not justiciability. The tangled rope classification (not snare) reflects that the constraint genuinely coordinates something: the delegation of welfare policy-setting to the state with electoral accountability. This coordination function distinguishes this reading from a pure snare reading (which would claim the promises are entirely illusory). The piton perspective shows that the design's function has degraded — the electoral accountability mechanism that was meant to enforce the 'conscience' constraint has weakened as political contestation has moved into courts. The harmonization pressure (measured in increasing extractiveness over time) creates tension with the non-justiciable design, but this reading's mandatrophy resolution is that the tension itself is part of the kernel contest. The reading does not claim that harmonization is wrong or that non-justiciability is immutable — it claims that the non-justiciable conscience mechanism is a defensible design choice with its own logic, even under pressure from the harmonization and welfare blueprint readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conscience_enforcement_boundary,
    'Is the non-justiciability of Part IV a principled limit on judicial review (matters of resource allocation, political economy, and electoral accountability are inherently non-judicial) or a doctrinal choice that could be revised?',
    'Comparative constitutional analysis: jurisdictions that treat analogous social rights as justiciable (South Africa post-1996, Brazil post-1988) and their enforcement outcomes; measurement of whether judicial enforcement of welfare claims produces resource allocation distortions or coherent redistribution',
    'If principled limit: the non-justiciable boundary is structurally immovable (mountain-like), and the reading''s core thesis (conscience without compulsion) reflects constitutional architecture. If doctrinal choice: the boundary is contingent, and competing readings (harmonization, welfare blueprint) have equal structural legitimacy. This omega determines whether sibling readings foreclose or coexist with this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conscience_enforcement_boundary, conceptual, 'Whether non-justiciability is a principled constitutional limit or a contingent doctrinal choice').

omega_variable(
    electoral_accountability_efficacy,
    'Do voters actually enforce welfare accountability through electoral choice, making Part IV''s conscience function operative, or is electoral accountability too diffuse and delayed to constrain state welfare discretion?',
    'Longitudinal analysis of electoral outcomes correlated with state welfare performance on Part IV targets (living wage, education access, health, agrarian welfare); measurement of voter information about state compliance with Part IV obligations; analysis of whether electoral defeats correlate with welfare failures',
    'If efficacious: the non-justiciable design works as intended — Part IV instructs the state and arms the voter, creating a soft accountability mechanism. If inefficacious: the design fails, and Part IV becomes performative (piton-like), requiring judicial enforcement to have effect. This determines the reading''s plausibility and the magnitude of extraction against powerless agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(electoral_accountability_efficacy, empirical, 'Whether electoral accountability actually enforces state compliance with Part IV welfare targets').

omega_variable(
    harmonization_pressure_direction,
    'Is the historical trajectory toward greater judicial enforcement of Part IV through harmonization (reading Part IV into Part III) a structural inevitability or a reversible doctrinal direction?',
    'Case law analysis: measure proportion of Part IV claims accepted into Part III (''right to life includes livelihood'') versus rejected; analysis of whether harmonization trajectory continues, plateaus, or reverses under different judicial compositions; comparative study of whether other jurisdictions show similar drift from non-justiciability toward enforceability',
    'If inevitable: this reading''s core premise (non-justiciability by design) is under permanent pressure from the harmonization reading, and they foreclose rather than coexist. If reversible: the readings coexist as live judicial doctrines with shifting dominance. This determines the CS reading_relations classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harmonization_pressure_direction, empirical, 'Whether harmonization trajectory is structural or reversible').

omega_variable(
    welfare_blueprint_realization,
    'Has the state''s conception of Part IV — as blueprints for a planned welfare architecture (panchayats, living wages, health, education systems) — ever been realized, or does the welfare blueprint remain aspirational?',
    'Empirical assessment: measurement of actual state delivery against the welfare blueprint''s specifications in Part IV; analysis of whether the gap between blueprint and delivery is structural (resource constraints, institutional capacity) or volitional (state discretion to deprioritize)',
    'If realized: the welfare blueprint reading''s core claim has substance, and Part IV''s non-justiciability appears less extractive (state has actually delivered on the blueprints). If aspirational: the gap reveals extractiveness — welfare promises without enforcement mechanism. This omega highlights the committer disagreement between this reading and the welfare blueprint reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_blueprint_realization, empirical, 'Whether the state has realized the welfare blueprint specified in Part IV').

omega_variable(
    committer_frame_reading_contest,
    'Which of the three readings (non-justiciable conscience, harmonization ascendancy, welfare blueprint) best explains the historical trajectory and current doctrinal state of Part IV?',
    'This is a conceptual omega about the kernel contest itself. Resolution requires integrating answers to conscience_enforcement_boundary, electoral_accountability_efficacy, harmonization_pressure_direction, and welfare_blueprint_realization with historical analysis of the constitutional convention, legislative history, and case law trajectory. No single observable resolves this — it is a hermeneutical synthesis.',
    'The engine uses this omega to compute whether sibling readings foreclose, coexist, or influence this reading. This omega acknowledges that the true structural relationship between readings is under-determined by any single observable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_frame_reading_contest, conceptual, 'Kernel-level hermeneutical contest: which reading best explains Part IV').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(directive_principles_part_iv__non_justiciable_conscience_reading, 1950, 1980).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dp_nj_theater_1950, directive_principles_part_iv__non_justiciable_conscience_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(dp_nj_theater_1965, directive_principles_part_iv__non_justiciable_conscience_reading, theater_ratio, 15, 0.52).
narrative_ontology:measurement(dp_nj_theater_1980, directive_principles_part_iv__non_justiciable_conscience_reading, theater_ratio, 30, 0.55).

% Extraction over time
narrative_ontology:measurement(dp_nj_extract_1950, directive_principles_part_iv__non_justiciable_conscience_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(dp_nj_extract_1965, directive_principles_part_iv__non_justiciable_conscience_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(dp_nj_extract_1980, directive_principles_part_iv__non_justiciable_conscience_reading, base_extractiveness, 30, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(dp_nj_suppress_1950, directive_principles_part_iv__non_justiciable_conscience_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(dp_nj_suppress_1965, directive_principles_part_iv__non_justiciable_conscience_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(dp_nj_suppress_1980, directive_principles_part_iv__non_justiciable_conscience_reading, suppression_requirement, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(directive_principles_part_iv__non_justiciable_conscience_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(directive_principles_part_iv__non_justiciable_conscience_reading, directive_principles_part_iv__harmonization_ascendancy_reading).
narrative_ontology:affects_constraint(directive_principles_part_iv__non_justiciable_conscience_reading, directive_principles_part_iv__welfare_blueprint_reading).

% DUAL FORMULATION NOTE:
% Part IV generates a constraint family: three structurally distinct readings with different ε values, different beneficiary/victim structures, and different forecloses/coexists relationships. This story (non_justiciable_conscience_reading) has ε ≈ 0.52 and claims electoral-accountability routing. The harmonization_ascendancy_reading (ε ≈ 0.65) claims judicial routing with higher extraction cost for the state. The welfare_blueprint_reading (ε ≈ 0.48) claims state-commitment routing with welfare realization as the measure. Each reading is a defensible interpretation of the same constitutional text; none is reducible to the others by changing observables or metrics. The family is linked via network.affects_constraints because the comparative analysis of readings is necessary for understanding how the kernel contest structures Indian constitutional law.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
