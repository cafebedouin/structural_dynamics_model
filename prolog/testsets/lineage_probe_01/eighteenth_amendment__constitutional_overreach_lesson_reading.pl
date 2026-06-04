% ============================================================================
% CONSTRAINT STORY: eighteenth_amendment__constitutional_overreach_lesson_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eighteenth_amendment__constitutional_overreach_lesson_reading, []).

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
 *   constraint_id: eighteenth_amendment__constitutional_overreach_lesson_reading
 *   human_readable: The Eighteenth Amendment as Constitutional Overreach Lesson
 *   domain: legal/doctrinal
 *
 * SUMMARY:
 *   The Eighteenth Amendment stands in constitutional jurisprudence as a
 *   canonical cautionary tale: social regulation does not belong in
 *   constitutional text. This reading instantiates one structural constraint
 *   embedded in that tale — the constraint that the Prohibition precedent
 *   suppresses every subsequent proposal to constitutionalize policy. The
 *   lesson paid once (the cost of discovering that comprehensive alcohol
 *   prohibition cannot be sustained as constitutional mandate) is cited
 *   forever (the precedent blocks morals amendments, policy
 *   constitutionalization, and similar structural moves). This reading models
 *   the suppression and extraction mechanisms through which the lesson
 *   operates as a constraint on later amendment proposals. It is one of three
 *   structurally distinct readings of the Eighteenth Amendment kernel: this
 *   reading (constitutional overreach doctrine), the enforcement collapse
 *   reading (Volstead Act's reach exceeded enforcement capacity), and the
 *   organized crime reading (Prohibition created criminal markets). Each
 *   reading has its own epsilon, beneficiary structure, and doctrinal
 *   function. This story instantiates the overreach reading specifically.
 *
 * KEY AGENTS:
 *   - Amendment Minimalism Doctrine: Primary beneficiary (institutional/arbitrage) — the doctrine that constitutions should address structure, not policy, derives its canonical justification from the Prohibition precedent
 *   - Morals Amendment Coalitions: Primary victim (powerless/identity_locked) — advocates for policy constitutionalization face the precedent as a legitimacy ceiling; their professional identity is fused with constitutional-amendment framing, making the lesson an existential constraint
 *   - Policy Constitutionalization Advocates: Secondary victim (moderate/constrained) — broader movements (drug policy, abortion, marriage equality) experience the Prohibition lesson as delegitimation; they face higher burden-of-proof and intensified opposition
 *   - Constitutional Reform Coalitions: Organized victims (organized/constrained) — movements for campaign finance, voting rights, climate action seek constitutional entrenchment but face Prohibition-invoked opposition
 *   - The Constitutional Canon: Institutional memory carrier (institutional/arbitrage) — law school textbooks, court opinions, and academic discourse perpetuate the lesson through citation and canonical authority
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing the doctrine as timeless constitutional truth rather than contingent institutional artifact
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eighteenth_amendment__constitutional_overreach_lesson_reading, 0.38).
domain_priors:suppression_score(eighteenth_amendment__constitutional_overreach_lesson_reading, 0.62).
domain_priors:theater_ratio(eighteenth_amendment__constitutional_overreach_lesson_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eighteenth_amendment__constitutional_overreach_lesson_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(eighteenth_amendment__constitutional_overreach_lesson_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(eighteenth_amendment__constitutional_overreach_lesson_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eighteenth_amendment__constitutional_overreach_lesson_reading, tangled_rope).
narrative_ontology:human_readable(eighteenth_amendment__constitutional_overreach_lesson_reading, "The Eighteenth Amendment as Constitutional Overreach Lesson").
narrative_ontology:topic_domain(eighteenth_amendment__constitutional_overreach_lesson_reading, "legal/doctrinal").

domain_priors:requires_active_enforcement(eighteenth_amendment__constitutional_overreach_lesson_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eighteenth_amendment__constitutional_overreach_lesson_reading, '5feb93c4-1391-4eed-8f12-5d206dbd9f4a').
narrative_ontology:cs_kernel_codification('5feb93c4-1391-4eed-8f12-5d206dbd9f4a', formalized).
narrative_ontology:cs_authority_grounding('5feb93c4-1391-4eed-8f12-5d206dbd9f4a', lineage).
narrative_ontology:cs_interpretation_layer_present('5feb93c4-1391-4eed-8f12-5d206dbd9f4a').
narrative_ontology:cs_reading_relation('5feb93c4-1391-4eed-8f12-5d206dbd9f4a', eighteenth_amendment__enforcement_collapse_reading, coexists_with).
narrative_ontology:cs_reading_relation('5feb93c4-1391-4eed-8f12-5d206dbd9f4a', eighteenth_amendment__organized_crime_externality_reading, influences).
narrative_ontology:cs_axiom('5feb93c4-1391-4eed-8f12-5d206dbd9f4a', foundational, policy_constitutionalization_inherently_self_defeating).
narrative_ontology:cs_axiom_status(policy_constitutionalization_inherently_self_defeating, holdable).
narrative_ontology:cs_axiom_grounding('5feb93c4-1391-4eed-8f12-5d206dbd9f4a', policy_constitutionalization_inherently_self_defeating, empirically_contingent).
narrative_ontology:cs_axiom('5feb93c4-1391-4eed-8f12-5d206dbd9f4a', foundational, constitutional_text_should_address_structure_not_policy).
narrative_ontology:cs_axiom_status(constitutional_text_should_address_structure_not_policy, holdable).
narrative_ontology:cs_axiom_grounding('5feb93c4-1391-4eed-8f12-5d206dbd9f4a', constitutional_text_should_address_structure_not_policy, deontological).
narrative_ontology:cs_reference_frame('5feb93c4-1391-4eed-8f12-5d206dbd9f4a', structural_constitutionalism_framework).
narrative_ontology:cs_drift_state('5feb93c4-1391-4eed-8f12-5d206dbd9f4a', contemporary_morals_amendment_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5feb93c4-1391-4eed-8f12-5d206dbd9f4a', '').
narrative_ontology:cs_kernel_id(eighteenth_amendment__constitutional_overreach_lesson_reading, eighteenth_amendment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eighteenth_amendment__constitutional_overreach_lesson_reading, amendment_minimalism_doctrine).
narrative_ontology:constraint_beneficiary(eighteenth_amendment__constitutional_overreach_lesson_reading, originalist_constitutional_restraint_movement).
narrative_ontology:constraint_victim(eighteenth_amendment__constitutional_overreach_lesson_reading, morals_amendment_coalitions).
narrative_ontology:constraint_victim(eighteenth_amendment__constitutional_overreach_lesson_reading, policy_constitutionalization_advocates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MORALS AMENDMENT COALITION (SNARE) — Advocates for constitutionalizing policy (drug criminalization, abortion, gender identity, marriage definition) face the Prohibition precedent as an inexorable constraint. Their professional identity is constituted through policy-to-constitution narratives; the lesson makes that identity unthinkable in mainstream discourse. Structurally mobile (could shift to statutory approaches) but identity-locked into constitutional framing. Maximum experienced suppression: the precedent forecloses legitimacy, not legality.
constraint_indexing:constraint_classification(eighteenth_amendment__constitutional_overreach_lesson_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: POLICY CONSTITUTIONALIZATION MOVEMENT (TANGLED ROPE) — Advocates for policy-embedded constitutional amendments experience genuine coordination benefits (constitutional codification does provide stability and majoritarian entrenchment) alongside asymmetric extraction (the Prohibition lesson suppresses legitimacy, raising burden-of-proof, intensifying opposition scrutiny). The constraint both enables and constrains: constitutional entrenchment is real coordination; Prohibition-based delegitimation is real extraction.
constraint_indexing:constraint_classification(eighteenth_amendment__constitutional_overreach_lesson_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: AMENDMENT MINIMALISM DOCTRINE (ROPE) — The judicial and academic doctrine that constitutional amendments should address structural governance, not policy, benefits from the Prohibition precedent. The lesson provides the canonical justification for restraint. This is pure coordination: the doctrine solves a real problem (constitutional text should not become a policy codebook) without requiring suppression. The Prohibition lesson is a coordination resource, freely available to all actors who accept the problem it solves.
constraint_indexing:constraint_classification(eighteenth_amendment__constitutional_overreach_lesson_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE CONSTITUTIONAL CANON (PITON) — The Prohibition example persists in constitutional pedagogy and jurisprudence through institutional inertia and theatrical authority rather than continuous verification of the lesson's validity. Law school casebooks cite it ritualistically; courts invoke it as settled precedent; the canonical status is maintained even as historical scholarship questions the causal narrative (was Prohibition's failure about overreach into policy, or about enforcement collapse and organized crime profit structures?). The theater ratio is high because the lesson's epistemic foundation (the causal diagnosis) is contested but its institutional function (delegitimizing policy amendments) remains unchallenged.
constraint_indexing:constraint_classification(eighteenth_amendment__constitutional_overreach_lesson_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL REFORM COALITIONS (TANGLED ROPE) — Broader reform movements (campaign finance regulation, voting rights, climate action) experience the Prohibition lesson as a mixed constraint. They experience coordination benefits from constitutional entrenchment (supermajority protection, trans-generational stability, judicial enforcement leverage) alongside extraction via delegitimation (opponents cite Prohibition to argue the proposals are policy overreach, not structural fixes). The coalitions are organized and have partial exit (can pursue statutory routes) but face suppression via the precedent.
constraint_indexing:constraint_classification(eighteenth_amendment__constitutional_overreach_lesson_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal analytical perspective, the Prohibition lesson appears as a natural law of constitutional design: policy constitutionalization is inherently self-defeating because policy domains require empirical adaptation and amendment lock-in prevents adaptation. This perspective sees the lesson as a timeless truth about the nature of constitutional text, not a historically contingent artifact of one failed experiment. However, the structural data (identified beneficiaries, clear suppression mechanism, distinct victims) reveals this as a false summit: the 'natural law' framing naturalizes what is actually a contingent doctrine sustained through institutional inertia and canonical repetition.
constraint_indexing:constraint_classification(eighteenth_amendment__constitutional_overreach_lesson_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eighteenth_amendment__constitutional_overreach_lesson_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(eighteenth_amendment__constitutional_overreach_lesson_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(eighteenth_amendment__constitutional_overreach_lesson_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(eighteenth_amendment__constitutional_overreach_lesson_reading, TR),
    TR >= 0.70.

:- end_tests(eighteenth_amendment__constitutional_overreach_lesson_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The Prohibition precedent extracts legitimacy from policy-amendment movements — it raises the burden-of-proof, attracts opposition scrutiny, and creates an aura of historical futility around policy constitutionalization. But the extraction is not total suppression: movements can still pursue amendments (and some succeed, e.g., civil rights amendments addressing policy); they must simply overcome the delegitimating precedent. The moderate value reflects that the constraint operates through doctrinal suppression rather than absolute legal prohibition. Suppression (0.62): Moderate-high. The Prohibition lesson suppresses alternative framings of constitutional policy-embedding through canonical repetition, institutional inertia in legal education, and rhetorical dominance in amendment debates. The suppression is substantial because invoking Prohibition has become a reflexive move in constitutional argument; it requires effortful counter-narrative to even question the precedent. But suppression is not absolute — alternative readings of Prohibition do exist in scholarship and can be articulated. Theater ratio (0.68): High. The Prohibition lesson is perpetuated largely through canonical repetition and institutional authority rather than continuous empirical verification of the causal diagnosis. Law school casebooks cite it; courts invoke it; the doctrine becomes settled through repetition, not through fresh examination of whether policy constitutionalization truly fails for the reasons the precedent suggests. Historical scholarship contests the overreach diagnosis, attributing Prohibition's failure to enforcement collapse and organized crime, but this scholarly critique does not reach the canonical level — the textbook narrative persists.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence. Amendment minimalists and constitutional conservatives see the Prohibition precedent as settled coordinate doctrine — the rope perspective — solving a real problem (preventing constitutionalization of transient policy fads). Morals amendment coalitions see the precedent as an inexorable suppression ceiling that forecloses their entire identity frame — the snare perspective with identity_locked exit. Constitutional scholars and casebook authors maintain the canonical narrative through repetition, treating it as pedagogically settled — the piton perspective, high theater, degraded verification. Reform coalitions seeking constitutional entrenchment for progressive goals (campaign finance, voting rights, climate) occupy the tangled rope perspective: they need constitutional amplitude to entrench their goals, but the Prohibition precedent delegitimizes any policy amendment as overreach. The analytical observer risks the false summit: seeing the doctrine as a natural law of constitutional design rather than as a contingent institutional arrangement that could be altered if the historical diagnosis (overreach failure) were revised.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiaries of this constraint (amendment minimalism doctrine, originalist constitutional restraint) experience low d-values: they benefit from the Prohibition precedent without bearing costs. The doctrine's legitimacy is sustained through the precedent; the precedent is cited to support their position; they experience the constraint as coordination (rope perspective). The victims (morals amendment movements, policy constitutionalization advocates) experience high d-values: they bear the suppression cost (delegitimation, heightened opposition, reduced plausibility) without receiving counterbalancing benefits from the precedent. Their d-values differ based on exit options: the powerless/identity_locked morals coalitions (d ≈ 0.89) experience maximum extraction; the organized reform coalitions (d ≈ 0.65) with exit routes (statutory alternatives, advocacy on other grounds) experience lower effective extraction. The institutional canonical carrier (the constitutional canon itself) experiences arbitrage exit (d ≈ 0.05): the canon benefits from maintaining the Prohibition narrative without bearing the suppression cost — the cost falls on those proposing amendments.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that the Prohibition lesson operates as both coordination mechanism and extraction vehicle simultaneously. For amendment minimalists, it is coordination: doctrinal coherence, stable principle preventing constitutional policy overreach. For morals amendment coalitions, it is extraction: suppression of legitimacy, foreclosure of a framing they depend on. Neither reading is wrong — both are structurally accurate from their respective positions. The tangled rope classification (rather than resolving to either pure rope or pure snare) captures this simultaneous coordination and extraction. The mandatrophy would only arise if the engine classified the constraint identically from all perspectives, masking the perspectival structure; instead, the multi-perspectival analysis reveals why both coordination and extraction are real.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prohibition_causal_diagnosis_contested,
    'Was Prohibition''s failure fundamentally due to constitutional overreach into policy (structural design flaw), or due to enforcement collapse and organized crime externalities (implementation failure and criminal market creation)?',
    'Comparative analysis of constitutional policy amendments with sustainable enforcement (e.g., federal income tax constitutional authorization, interstate commerce regulation) vs. those with enforcement collapse (Prohibition); examination of counterfactual: would Prohibition have succeeded under statute with lower enforcement burden?',
    'If structural design flaw: the overreach lesson is valid; policy constitutionalization is inherently self-defeating (mountain classification holds). If implementation failure: the lesson is misattributed; the constraint is doctrinal suppression masquerading as natural law (false summit confirmed, snare/tangled_rope from powerless perspectives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(prohibition_causal_diagnosis_contested, empirical, 'Whether Prohibition''s failure was due to constitutional overreach or implementation collapse').

omega_variable(
    doctrine_perpetuation_mechanism,
    'Is the Prohibition lesson perpetuated through continuous verification of its validity, or through canonical repetition and institutional inertia decoupled from empirical checking?',
    'Citation analysis of Prohibition precedent in constitutional law: frequency of citations; presence of empirical validation or reference to historical evidence; comparison to frequency of invocation in contemporary debates vs. in scholarly reexamination',
    'If continuous verification: the constraint is legitimate rope/scaffold (real coordination function, periodic validation). If inertial perpetuation: the constraint is piton (performative canonical authority, theater ≥ 0.70).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_perpetuation_mechanism, empirical, 'Whether the Prohibition lesson is perpetuated through verification or inertia').

omega_variable(
    suppression_mechanism_structural_vs_rhetorical,
    'Does the Prohibition precedent suppress policy constitutionalization through structural legal barriers (rejected amendments, no path through ratification), or through rhetorical delegitimation and burden-shifting (opponents cite it but could legally proceed)?',
    'Analysis of proposed constitutional amendments citing Prohibition: examination of ratification prospects, legislative sponsorship, and opposition strategy; comparison of Prohibition-invoked objections vs. other doctrinal objections in amendment debates',
    'If structural: the suppression is fundamental to the constitutional architecture (mountain-adjacent, high accessibility collapse). If rhetorical: the suppression is discursive dominance (snare/tangled rope, suppression through legitimacy denial rather than legal exclusion).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_rhetorical, empirical, 'Whether Prohibition suppression is structural or rhetorical').

omega_variable(
    reading_identity_distinct_from_siblings,
    'Is the constitutional overreach lesson (this reading) sufficiently distinct from the enforcement collapse reading and the organized crime reading, or are they alternative explanations of the same constraint rather than three separate constraints?',
    'Structural decomposition: if the three readings produce different epsilon values (e.g., overreach=0.38, enforcement_collapse=0.55, organized_crime=0.42), they are distinct constraints per ε-invariance principle. If epsilon ranges overlap significantly, reassess whether the kernel contest is a genuine constraint family or a single constraint with multiple interpretations.',
    'If distinct constraints: maintain three separate stories, linked via network.affects_constraints. If single constraint with multiple readings: consolidate into one story with alternate perspectival framings in commentary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity_distinct_from_siblings, conceptual, 'Whether the three Eighteenth Amendment readings are distinct constraints or interpretations of one').

omega_variable(
    amendment_minimalism_doctrine_natural_vs_constructed,
    'Is amendment minimalism (the doctrine that constitutions should address structure, not policy) a discovered natural principle of constitutional design, or a doctrine constructed to foreclose morals-amendment movements?',
    'Historical genealogy: does amendment minimalism appear in Founders'' writings and early jurisprudence, or does it emerge in post-Prohibition era as rhetorical device? Cross-cultural comparison: do other democracies'' constitutions reflect the same structural principle, or do policy-embedded amendments appear regularly?',
    'If natural principle: mountain classification is justified; the lesson reflects timeless constitutional truth. If constructed doctrine: false summit confirmed; suppression mechanism revealed as institutional gatekeeping.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_minimalism_doctrine_natural_vs_constructed, empirical, 'Whether amendment minimalism is a natural principle or constructed doctrine').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eighteenth_amendment__constitutional_overreach_lesson_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eighteenth_lesson_tr_t0, eighteenth_amendment__constitutional_overreach_lesson_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(eighteenth_lesson_tr_t25, eighteenth_amendment__constitutional_overreach_lesson_reading, theater_ratio, 25, 0.55).
narrative_ontology:measurement(eighteenth_lesson_tr_t50, eighteenth_amendment__constitutional_overreach_lesson_reading, theater_ratio, 50, 0.68).

% Extraction over time
narrative_ontology:measurement(eighteenth_lesson_be_t0, eighteenth_amendment__constitutional_overreach_lesson_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(eighteenth_lesson_be_t25, eighteenth_amendment__constitutional_overreach_lesson_reading, base_extractiveness, 25, 0.35).
narrative_ontology:measurement(eighteenth_lesson_be_t50, eighteenth_amendment__constitutional_overreach_lesson_reading, base_extractiveness, 50, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(eighteenth_lesson_su_t0, eighteenth_amendment__constitutional_overreach_lesson_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(eighteenth_lesson_su_t25, eighteenth_amendment__constitutional_overreach_lesson_reading, suppression_requirement, 25, 0.58).
narrative_ontology:measurement(eighteenth_lesson_su_t50, eighteenth_amendment__constitutional_overreach_lesson_reading, suppression_requirement, 50, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eighteenth_amendment__constitutional_overreach_lesson_reading, information_standard).
narrative_ontology:affects_constraint(eighteenth_amendment__constitutional_overreach_lesson_reading, eighteenth_amendment__enforcement_collapse_reading).
narrative_ontology:affects_constraint(eighteenth_amendment__constitutional_overreach_lesson_reading, eighteenth_amendment__organized_crime_externality_reading).

% DUAL FORMULATION NOTE:
% The three Eighteenth Amendment readings are structurally distinct constraints with different epsilon values and beneficiary/victim structures. The overreach reading (this constraint, ε=0.38) models doctrinal suppression of policy constitutionalization. The enforcement collapse reading (ε≈0.55) models the constraint that Volstead's reach exceeded enforcement capacity. The organized crime reading (ε≈0.42) models the constraint that Prohibition capitalized criminal markets. Each reading extracts different causal and institutional meanings from the same historical amendment. They are linked via network.affects_constraints because the alternative reading of Prohibition's failure affects the legitimacy and perpetuation mechanisms of this reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eighteenth_amendment__constitutional_overreach_lesson_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
