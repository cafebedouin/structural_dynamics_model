% ============================================================================
% CONSTRAINT STORY: free_exercise_clause__smith_neutral_law_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_free_exercise_clause__smith_neutral_law_reading, []).

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
 *   constraint_id: free_exercise_clause__smith_neutral_law_reading
 *   human_readable: Free Exercise Clause: Smith Neutral Law Reading (Religion Yields to Neutral Regulations)
 *   domain: constitutional_law/religious_freedom
 *
 * SUMMARY:
 *   This constraint instantiates the Smith neutral law reading of the Free
 *   Exercise Clause, established in Employment Division v. Smith (1990). The
 *   reading holds that the Free Exercise Clause does not require exemption
 *   from neutral laws of general applicability, even when such laws
 *   substantially burden religious practice. This is ONE reading of a
 *   contested constitutional kernel — the meaning and scope of religious
 *   liberty protections. The sibling reading (Sherbert/compelling interest
 *   standard) held that substantial burdens on religious practice require the
 *   state to demonstrate a compelling interest pursued by the least
 *   restrictive means. The kernel contest is whether religious exemptions are
 *   constitutionally required (Sherbert) or constitutionally optional absent
 *   targeting (Smith). This story models the Smith reading's structural
 *   position: it benefits uniform regulatory administration, imposes
 *   incidental burdens on minority religious practitioners, and naturalizes
 *   these burdens as a consequence of constitutional order rather than a
 *   distributional choice. The reading's extractiveness is moderate because
 *   it permits real burdens while appearing to offer constitutional
 *   protection (the theater). The suppression metric reflects that affected
 *   groups can pursue legislative accommodation but face substantial
 *   political barriers to relief. The constraint exhibits tangled rope
 *   characteristics because it coordinates legitimate state administration
 *   (uniform application) while asymmetrically extracting from religious
 *   minorities who lack political power for exemption.
 *
 * KEY AGENTS:
 *   - Minority Religious Practitioners: Primary victims (powerless/trapped) — subject to neutral laws that incidentally but severely burden sincere religious practice; no exit without abandoning practice
 *   - Minority Religious Communities: Secondary agents (moderate/constrained) — can organize and lobby for legislative exemptions but face political barriers; benefit from non-targeting but lose access to exemption mechanism
 *   - Majority Religious Interests: Beneficiaries with power (powerful/arbitrage) — benefit from predictable uniform laws and can exit via legislative accommodation when necessary; rarely burdened by neutral regulations
 *   - Uniform Regulatory Regimes: Institutional beneficiary (institutional/arbitrage) — benefit from administrative simplicity and equal application without religious carve-outs
 *   - Legislatures: Secondary actors (institutional/arbitrage) — retain power to grant exemptions but the doctrine creates incentive structure favoring uniform application
 *   - Courts Applying the Doctrine: Institutional implementers (institutional/arbitrage) — apply rational basis review to 'neutral' laws; doctrine provides appearance of objectivity while permitting selective enforcement
 *   - Analytical Observer: The civilizational perspective (analytical/analytical) — risks naturalizing the doctrinal choice as a constitutional necessity rather than a contingent institutional preference
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(free_exercise_clause__smith_neutral_law_reading, 0.35).
domain_priors:suppression_score(free_exercise_clause__smith_neutral_law_reading, 0.38).
domain_priors:theater_ratio(free_exercise_clause__smith_neutral_law_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(free_exercise_clause__smith_neutral_law_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(free_exercise_clause__smith_neutral_law_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(free_exercise_clause__smith_neutral_law_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(free_exercise_clause__smith_neutral_law_reading, tangled_rope).
narrative_ontology:human_readable(free_exercise_clause__smith_neutral_law_reading, "Free Exercise Clause: Smith Neutral Law Reading (Religion Yields to Neutral Regulations)").
narrative_ontology:topic_domain(free_exercise_clause__smith_neutral_law_reading, "constitutional_law/religious_freedom").

domain_priors:requires_active_enforcement(free_exercise_clause__smith_neutral_law_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(free_exercise_clause__smith_neutral_law_reading, '233d074b-41e1-47c2-8fc0-71e5300a0e5d').
narrative_ontology:cs_kernel_codification('233d074b-41e1-47c2-8fc0-71e5300a0e5d', fixed_text).
narrative_ontology:cs_authority_grounding('233d074b-41e1-47c2-8fc0-71e5300a0e5d', lineage).
narrative_ontology:cs_interpretation_layer_present('233d074b-41e1-47c2-8fc0-71e5300a0e5d').
narrative_ontology:cs_reading_relation('233d074b-41e1-47c2-8fc0-71e5300a0e5d', free_exercise_clause__sherbert_compelling_interest_reading, forecloses).
narrative_ontology:cs_axiom('233d074b-41e1-47c2-8fc0-71e5300a0e5d', foundational, neutrality_suffices_for_constitutionality).
narrative_ontology:cs_axiom_status(neutrality_suffices_for_constitutionality, holdable).
narrative_ontology:cs_axiom_grounding('233d074b-41e1-47c2-8fc0-71e5300a0e5d', neutrality_suffices_for_constitutionality, deontological).
narrative_ontology:cs_axiom('233d074b-41e1-47c2-8fc0-71e5300a0e5d', foundational, rational_basis_for_neutral_laws).
narrative_ontology:cs_axiom_status(rational_basis_for_neutral_laws, holdable).
narrative_ontology:cs_axiom_grounding('233d074b-41e1-47c2-8fc0-71e5300a0e5d', rational_basis_for_neutral_laws, instrumental).
narrative_ontology:cs_reference_frame('233d074b-41e1-47c2-8fc0-71e5300a0e5d', neutral_law_constitutional_order).
narrative_ontology:cs_drift_state('233d074b-41e1-47c2-8fc0-71e5300a0e5d', contemporary_post_2020s, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('233d074b-41e1-47c2-8fc0-71e5300a0e5d', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(free_exercise_clause__smith_neutral_law_reading, free_exercise_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(free_exercise_clause__smith_neutral_law_reading, uniform_regulatory_regimes).
narrative_ontology:constraint_beneficiary(free_exercise_clause__smith_neutral_law_reading, legislative_simplicity).
narrative_ontology:constraint_victim(free_exercise_clause__smith_neutral_law_reading, minority_religious_practitioners).
narrative_ontology:constraint_victim(free_exercise_clause__smith_neutral_law_reading, religious_liberty_claimants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MINORITY RELIGIOUS PRACTITIONER (SNARE) — Subject to neutral laws that incidentally burden sincere religious practice (e.g., drug prohibition affecting sacramental use, zoning laws affecting places of worship, military service requirements) with no available exemption. No structural exit without abandoning religious practice or relocating. High experienced extraction from incidental regulation.
constraint_indexing:constraint_classification(free_exercise_clause__smith_neutral_law_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MINORITY RELIGIOUS COMMUNITY (TANGLED ROPE) — Experiences both coordination benefit (neutral laws protect religious liberty from intentional targeting) and extraction cost (incidental burdens permitted without compelling interest test). Community can organize for legislative relief but faces high political barriers. Moderate power with constrained exit options.
constraint_indexing:constraint_classification(free_exercise_clause__smith_neutral_law_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: UNIFIED REGULATORY SCHEME (ROPE) — Experiences the reading as pure coordination: neutral laws can be uniformly applied without religious accommodation carve-outs, reducing administrative burden and ensuring equal application. Primary beneficiary of the reading. Experiences constraint as legitimate coordination mechanism with negligible extraction cost.
constraint_indexing:constraint_classification(free_exercise_clause__smith_neutral_law_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MAJORITY RELIGIOUS INTERESTS (TANGLED ROPE) — Benefits from the reading insofar as it prevents targeted discrimination against majority religions (neutral laws apply equally), but also benefits from not requiring exemptions that would be costly or administratively complex. Powerful actors with arbitrage options — can exit via legislative accommodation if politically advantageous, but the reading's logic incentivizes uniform application without carve-outs.
constraint_indexing:constraint_classification(free_exercise_clause__smith_neutral_law_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: THE RATIONAL BASIS FORMALISM (PITON) — The doctrinal machinery (rational basis review, general applicability test) is largely performative. 'Neutral' and 'generally applicable' are highly malleable interpretive categories; courts can and do find regulations non-neutral when politically motivated. The formalism persists through institutional inertia (familiar doctrine, judicial efficiency) despite critics showing that neutrality is often a retroactive cover story for policies with hidden religious targeting. Theater ratio reflects gap between the doctrine's appearance of neutrality and the political contingency of its application.
constraint_indexing:constraint_classification(free_exercise_clause__smith_neutral_law_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the reading can be framed as reflecting an immutable principle: the state cannot function if it must accommodate every religious practice; neutrality (non-targeting) is a structural floor of governmental legitimacy. However, this naturalizes what the kernel context reveals as a contingent doctrinal choice. The engine's false summit detector will expose beneficiary presence (uniform regulatory schemes) as a signal that the 'natural law' framing masks an institutional preference.
constraint_indexing:constraint_classification(free_exercise_clause__smith_neutral_law_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(free_exercise_clause__smith_neutral_law_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(free_exercise_clause__smith_neutral_law_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(free_exercise_clause__smith_neutral_law_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(free_exercise_clause__smith_neutral_law_reading, TR),
    TR >= 0.70.

:- end_tests(free_exercise_clause__smith_neutral_law_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The constraint permits real burdens on religious practice but does not require intentional targeting to trigger heightened review. The measurement trajectory (0.25 → 0.35) reflects gradual expansion of what counts as 'incidental burden' — early period post-Smith treated broader class of regulations as neutral; over time, critics documented patterns suggesting selective application. Extractiveness remains moderate rather than high because legislative accommodation remains theoretically available (though practically difficult), and the doctrine does protect against intentional discrimination. Suppression (0.38): Moderate. Multiple barriers exist to relief: (1) the doctrine itself removes rational basis review; (2) political power imbalance makes legislative exemption difficult for minority groups; (3) the 'neutrality' framing obscures the distributional effect, reducing visibility of the burden. But suppression is not total — successful legislative exemptions exist (Native American Church, Amish) and awareness of the issue has grown. Theater ratio (0.55): Moderate-high. The doctrine's appearance of neutrality masks political contingency. The rational basis test and 'general applicability' analysis create performative rigor that obscures how the same regulation is applied differently to majority vs. minority religions depending on court composition and judicial attitudes toward religious liberty. The theater increases over time (0.40 → 0.60) as critics expose the gap between the doctrine's neutral facade and its selective application.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits sharp perspectival divergence. The institutional regulatory perspective (legislatures, courts) sees coordination (Rope): neutral laws can be uniformly applied without administrative burden of religious exemptions. The minority practitioner sees extraction (Snare): the doctrine strips away their only doctrinal tool for relief from burdens that substantially obstruct their religious practice. The minority community sees mixed extraction and coordination (Tangled Rope): they benefit from non-targeting but lose exemption access. The majority religious perspective sees conditional coordination (Tangled Rope): protected from targeting but incentivized toward non-exemption compliance. The doctrine itself appears as degraded theater (Piton): the neutrality and rationality machinery performs rigor without delivering consistency. The civilizational analytical observer risks naturalizing the doctrinal choice (Mountain) — seeing religious exemption as constitutionally optional as a structural necessity rather than a contingent institutional preference. The false summit detector will expose the beneficiary presence (uniform regulatory schemes explicitly listed as beneficiary) as evidence that the mountain is constructed.
 *
 * DIRECTIONALITY LOGIC:
 *   The Smith reading positions institutional actors (legislatures, courts, regulatory agencies) as beneficiaries of simplified administration without religious carve-outs (low directionality d toward extraction). Minority practitioners are positioned as victims bearing incidental burdens without constitutional recourse (high d toward extraction). The doctrinal structure itself carries low suppression for majority groups (they rarely encounter neutral laws that burden their practices) but high suppression for minorities (political barriers to legislative relief). Majority religious institutions occupy a privileged structural position — they benefit from the neutrality framing (protected from targeting) and have political power for exemptions (arbitrage exit option). Minority practitioners lack both protections. This asymmetry drives the tangled rope classification: the constraint simultaneously coordinates legitimate state administration and extracts from those without political power to avoid the extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that Smith and Sherbert are genuinely competing readings of a single kernel (the Free Exercise Clause), not different constraints. The mandatrophy question is not 'which type is correct?' but 'which reading governs the legal interpretation?' The constraint here models Smith as a tangled rope: it coordinates uniform administration while extracting from religious minorities. Sherbert (the sibling) would model as rope or tangled rope from the minority practitioner perspective (high protection) and as snare from the regulatory perspective (high constraint on administration). The readings are not compatible within a single legal framework — courts must choose which test to apply. This choice is a doctrinal fork point, not a measurement ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    neutrality_detectability,
    'Is ''general applicability'' and ''neutrality'' objectively discernible or is it a post-hoc doctrinal cover for regulatory intent?',
    'Comparative analysis: historical pattern of regulations classified as neutral/generally applicable vs. historical evidence of legislative intent; examination of regulations with disproportionate impact on minority religions classified as neutral; case law consistency in applying neutrality test across majority vs. minority religious burdens.',
    'If neutrality is discernible: Smith reading is stable, extraction is incidental consequence of neutral administration. If neutrality is post-hoc: reading enables disguised targeting, extraction becomes a feature, not a bug — reclassify toward Snare from institutional perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neutrality_detectability, empirical, 'Whether neutrality is objectively detectible or applied selectively').

omega_variable(
    incidental_burden_threshold,
    'At what magnitude does an ''incidental'' burden on religious practice become functionally prohibitive, shifting the reading from permissible to extractive?',
    'Empirical study of burdens permitted under Smith: unemployment benefits denial (Employment Div v. Smith itself), military service restrictions, drug laws affecting sacraments, zoning affecting houses of worship; correlation between burden magnitude and susceptibility to legislative relief; historical expansion of ''incidental'' category.',
    'If threshold is low (burden considered incidental despite near-total elimination of practice): extraction is severe, reading should reclassify Snare. If threshold is high: reading permits only truly marginal burdens, extractiveness remains moderate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(incidental_burden_threshold, empirical, 'Threshold at which incidental burden becomes functionally prohibitive').

omega_variable(
    foreclosure_relationship_to_sherbert,
    'Does the Smith reading''s core premise (neutral laws need not satisfy compelling interest test) logically foreclose the Sherbert reading''s core premise (substantial burdens require compelling interest + least restrictive means)?',
    'Formal analysis: can both readings coexist in a single legal framework? The Smith reading explicitly rejected the Sherbert test; the two readings cannot both hold in constitutional law simultaneously. However, they can coexist as competing judicial doctrines held by different courts or historical periods. Determine whether the relationship is logical (foreclosure) or merely empirical (coexistence with conflict).',
    'If forecloses: the kernel exhibits genuine logical contradiction between readings, not merely political contestation. If coexists: both readings remain live doctrinal options depending on court composition or legislative override. Affects how to model kernel evolution and the possibility of a unified framework holding both.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(foreclosure_relationship_to_sherbert, conceptual, 'Whether Smith logically forecloses Sherbert or merely contradicts it empirically').

omega_variable(
    legislative_accommodation_availability,
    'Is legislative accommodation of religious practices (via exemption statutes) a real exit option for minority practitioners, or is political power imbalance a structural barrier?',
    'Historical study of religious exemption statutes: success rates of legislative relief efforts by minority vs. majority religions; political economy analysis of exemption-seeking; case studies of successful (Native American Church, Amish) vs. unsuccessful (Jehovah''s Witnesses, Satanists) accommodation efforts.',
    'If legislative accommodation is available: victims have constrained but real exit option, extractiveness is lower. If legislative accommodation is systematically denied to minorities: victims are effectively trapped, extractiveness is higher, snare classification is more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legislative_accommodation_availability, empirical, 'Whether legislative accommodation provides real exit for religious minorities').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(free_exercise_clause__smith_neutral_law_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fe_smith_tr_t0, free_exercise_clause__smith_neutral_law_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(fe_smith_tr_t15, free_exercise_clause__smith_neutral_law_reading, theater_ratio, 15, 0.55).
narrative_ontology:measurement(fe_smith_tr_t30, free_exercise_clause__smith_neutral_law_reading, theater_ratio, 30, 0.6).

% Extraction over time
narrative_ontology:measurement(fe_smith_be_t0, free_exercise_clause__smith_neutral_law_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(fe_smith_be_t15, free_exercise_clause__smith_neutral_law_reading, base_extractiveness, 15, 0.32).
narrative_ontology:measurement(fe_smith_be_t30, free_exercise_clause__smith_neutral_law_reading, base_extractiveness, 30, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(fe_smith_su_t0, free_exercise_clause__smith_neutral_law_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(fe_smith_su_t15, free_exercise_clause__smith_neutral_law_reading, suppression_requirement, 15, 0.36).
narrative_ontology:measurement(fe_smith_su_t30, free_exercise_clause__smith_neutral_law_reading, suppression_requirement, 30, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(free_exercise_clause__smith_neutral_law_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(free_exercise_clause__smith_neutral_law_reading, free_exercise_clause__sherbert_compelling_interest_reading).
narrative_ontology:affects_constraint(free_exercise_clause__smith_neutral_law_reading, religious_freedom_restoration_act__statutory_override).

% DUAL FORMULATION NOTE:
% This constraint and the Sherbert reading are sibling interpretations of the same Free Exercise Clause kernel. They are linked by kernel relationship, not by ε-invariance decomposition. Each reading has its own ε (this story: 0.35; Sherbert would be lower, reflecting higher protection). They compete at the doctrinal level — courts must choose which interpretation to apply — rather than decomposing into separate structural claims. The network edge to RFRA reflects that the statutory reading partially overrides Smith by legislative fiat, restoring closer to Sherbert protections.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
