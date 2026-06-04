% ============================================================================
% CONSTRAINT STORY: free_exercise_clause__sherbert_compelling_interest_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_free_exercise_clause__sherbert_compelling_interest_reading, []).

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
 *   constraint_id: free_exercise_clause__sherbert_compelling_interest_reading
 *   human_readable: Free Exercise Clause (Sherbert Compelling Interest Reading)
 *   domain: constitutional_law/religious_liberty
 *
 * SUMMARY:
 *   The Sherbert compelling-interest reading of the Free Exercise Clause
 *   establishes that any substantial burden on sincere religious practice
 *   must be justified by a compelling state interest pursued by the least
 *   restrictive means. This reading, articulated in Sherbert v. Verner (1963)
 *   and applied through Employment Division v. Smith (1990), structures the
 *   constitutional relationship between religious practice and neutral state
 *   regulation. The constraint exhibits a tangled rope structure: it
 *   genuinely protects religious autonomy (coordination function,
 *   beneficiary-enabled principle) while imposing enforcement costs on
 *   regulators and litigation burdens on religious claimants (extraction
 *   function, enforcement-asymmetry). The doctrinal framework is internally
 *   coherent but practically sparse in successes — from 1963–1990, successful
 *   religious burden claims were rare despite the doctrine's principled
 *   scaffolding. Extractiveness has risen over the 27-year interval as the
 *   doctrine accumulated case law, regulatory resistance, and doctrinal
 *   complexity; suppression has intensified as government agencies learned to
 *   frame regulations in compelling-interest language; theater has increased
 *   as courts developed increasingly sophisticated burden-accommodation
 *   analyses. By Smith (1990), the doctrine was facing its foundational
 *   challenge: the competing neutral-law reading (Smith) argues that religion
 *   receives no special exemption from rules that do not target it,
 *   foreclosing or sharply constraining Sherbert's framework.
 *
 * KEY AGENTS:
 *   - Burdened Religious Practitioners: Primary beneficiary (powerless/trapped) — experience the constraint as protection against regulatory infringement; lack alternative exit routes but gain standing to claim burden-accommodation
 *   - Religious Institutions: Secondary beneficiary & plaintiff (organized/constrained) — access Sherbert framework to protect institutional practice; required to litigate and prove sincere belief; benefit from coordination principle but extract cost from enforcement asymmetry
 *   - State Regulatory Agencies: Primary victim (moderate/constrained) — must accommodate religious burdens or justify regulations through strict scrutiny; extract suppression costs and litigation expense; cannot apply neutral laws uniformly
 *   - Judicial System: Institutional arbiter (institutional/arbitrage) — gains coordination function (clarifying sincere belief, compelling interest, least restrictive means) but extracts cost from intensive burden-accommodation review; maintains doctrinal coherence through repeated case-law refinement
 *   - Smith Doctrine (Countervailing Reading): Competing constitutional vision (analytical/analytical) — claims that Sherbert's special protection for religion creates exemption-shopping and undermines neutral-law equality principle; forecloses or constrains Sherbert framework from alternative constitutional reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(free_exercise_clause__sherbert_compelling_interest_reading, 0.28).
domain_priors:suppression_score(free_exercise_clause__sherbert_compelling_interest_reading, 0.62).
domain_priors:theater_ratio(free_exercise_clause__sherbert_compelling_interest_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(free_exercise_clause__sherbert_compelling_interest_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(free_exercise_clause__sherbert_compelling_interest_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(free_exercise_clause__sherbert_compelling_interest_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(free_exercise_clause__sherbert_compelling_interest_reading, tangled_rope).
narrative_ontology:human_readable(free_exercise_clause__sherbert_compelling_interest_reading, "Free Exercise Clause (Sherbert Compelling Interest Reading)").
narrative_ontology:topic_domain(free_exercise_clause__sherbert_compelling_interest_reading, "constitutional_law/religious_liberty").

domain_priors:requires_active_enforcement(free_exercise_clause__sherbert_compelling_interest_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(free_exercise_clause__sherbert_compelling_interest_reading, '24653323-d75c-4d00-8bd0-82a637c772db').
narrative_ontology:cs_kernel_codification('24653323-d75c-4d00-8bd0-82a637c772db', formalized).
narrative_ontology:cs_authority_grounding('24653323-d75c-4d00-8bd0-82a637c772db', lineage).
narrative_ontology:cs_interpretation_layer_present('24653323-d75c-4d00-8bd0-82a637c772db').
narrative_ontology:cs_reading_relation('24653323-d75c-4d00-8bd0-82a637c772db', free_exercise_clause__smith_neutral_law_reading, forecloses).
narrative_ontology:cs_axiom('24653323-d75c-4d00-8bd0-82a637c772db', foundational, religious_practice_substantive_liberty).
narrative_ontology:cs_axiom_status(religious_practice_substantive_liberty, holdable).
narrative_ontology:cs_axiom_grounding('24653323-d75c-4d00-8bd0-82a637c772db', religious_practice_substantive_liberty, deontological).
narrative_ontology:cs_axiom('24653323-d75c-4d00-8bd0-82a637c772db', secondary, sincere_belief_epistemically_verifiable).
narrative_ontology:cs_axiom_status(sincere_belief_epistemically_verifiable, holdable).
narrative_ontology:cs_axiom_grounding('24653323-d75c-4d00-8bd0-82a637c772db', sincere_belief_epistemically_verifiable, empirically_contingent).
narrative_ontology:cs_reference_frame('24653323-d75c-4d00-8bd0-82a637c772db', strictest_scrutiny_religious_liberty).
narrative_ontology:cs_drift_state('24653323-d75c-4d00-8bd0-82a637c772db', smith_decision_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('24653323-d75c-4d00-8bd0-82a637c772db', '').
narrative_ontology:cs_kernel_id(free_exercise_clause__sherbert_compelling_interest_reading, free_exercise_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(free_exercise_clause__sherbert_compelling_interest_reading, burdened_religious_practitioners).
narrative_ontology:constraint_victim(free_exercise_clause__sherbert_compelling_interest_reading, regulatory_uniformity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BURDENED BELIEVER (ROPE) — Individual whose sincere religious practice is substantially burdened by a neutral law of general applicability. Under Sherbert strict scrutiny, this agent gains a coordination mechanism: the law must either accommodate the practice or justify its burden through compelling interest and least restrictive means. The agent experiences the constraint as enabling rather than extractive because it protects their religious liberty through a genuine coordination principle (burden-accommodation) rather than exempting them arbitrarily.
constraint_indexing:constraint_classification(free_exercise_clause__sherbert_compelling_interest_reading, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RELIGIOUS INSTITUTION (TANGLED ROPE) — Established religious bodies benefit from the Sherbert doctrine's protection of institutional practice (hiring, worship space access, sacramental activities). But they also experience extraction through the requirement to litigate each burden claim, the uncertainty of outcome, and the resource commitment needed to establish 'sincere religious belief.' Active enforcement (burden-accommodation litigation) is required; the constraint provides genuine coordination (protecting religious autonomy) alongside asymmetric extraction (religious institutions must prove sincerity and necessity; state proves compelling interest).
constraint_indexing:constraint_classification(free_exercise_clause__sherbert_compelling_interest_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REGULATING GOVERNMENT (SNARE) — Government agencies applying neutral laws (workplace safety, tax collection, healthcare licensing) face Sherbert's strict scrutiny when the law burdens religious practice. The state experiences extraction: it must either accommodate the burden or provide compelling interest justification and least restrictive means analysis. For routine regulations (unemployment insurance, public school attendance, workplace grooming codes), the state bears high litigation and compliance costs. The suppression is high (the state cannot simply apply its rule uniformly) but not total — the state can still apply the rule if it meets the strict scrutiny test.
constraint_indexing:constraint_classification(free_exercise_clause__sherbert_compelling_interest_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: DOCTRINE'S HISTORICAL ARCHIVE (PITON) — By the time Smith (1990) overruled Sherbert's framework for neutral laws, the Sherbert doctrine had been litigated for 30 years (1963–1990) but produced a relatively sparse body of successful religious exemptions. The doctrine's theater_ratio is moderate-high (0.35): the burden-accommodation framework sounds rigorous, but successful plaintiffs were rare (Sherbert herself, Yoder/Amish schooling, perhaps military chaplaincy), while many claims failed (unemployment for refusing to work Sabbath in general jobs; religious polygamy exemptions). By the 1980s, the doctrine persisted as a principled framework but functioned more as a formalized conversation about burden than as a reliable engine for exemptions.
constraint_indexing:constraint_classification(free_exercise_clause__sherbert_compelling_interest_reading, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

% PERSPECTIVE 5: DOCTRINE AS CONSTITUTIONAL LAW (MOUNTAIN) — From a purely doctrinal/logical perspective, the Sherbert framework instantiates a coherent constitutional principle: sincere religious practice occupies a special protected category, and any burden on it must meet strict scrutiny (compelling interest + least restrictive means). This logical structure is immutable within the framework itself — if you accept the premise (religion is a constitutionally protected category distinct from other conscience-based refusals), the strict scrutiny test follows necessarily. The mountain classification reflects the doctrinal inevitability given the foundational commitment.
constraint_indexing:constraint_classification(free_exercise_clause__sherbert_compelling_interest_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: JUDICIAL SYSTEM (TANGLED ROPE) — Courts applying Sherbert gain coordination function (clarifying what counts as sincere religious belief, what constitutes a substantial burden, what state interests are compelling) but also experience extraction pressure. The doctrine requires courts to make individualized burden assessments (high litigation cost) and to second-guess state regulatory judgments. Courts benefit from the doctrine's principled framework (institutional credibility, doctrinal coherence) but extract cost from every case where the burden-accommodation analysis is unclear. Active enforcement required; mixed coordination-extraction.
constraint_indexing:constraint_classification(free_exercise_clause__sherbert_compelling_interest_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(free_exercise_clause__sherbert_compelling_interest_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(free_exercise_clause__sherbert_compelling_interest_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(free_exercise_clause__sherbert_compelling_interest_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(free_exercise_clause__sherbert_compelling_interest_reading, TR),
    TR >= 0.70.

:- end_tests(free_exercise_clause__sherbert_compelling_interest_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Moderate-low. The Sherbert doctrine protects burdened believers through a genuine coordination mechanism — the burden-accommodation principle. However, extractiveness is not zero because: (1) burdened believers must litigate to access protection (cost of access); (2) religious institutions must prove sincere belief (epistemic gatekeeping); (3) regulators are extracted from (suppressed from applying neutral rules uniformly). The moderate value reflects mixed coordination-extraction. Extractiveness rises from 0.15 (early Sherbert, when doctrine was novel and protective) to 0.28 (approaching Smith, as doctrine accumulated complexity and regulatory resistance). Suppression (0.62): Moderate-high. Regulators face substantial suppression: they cannot apply neutral laws uniformly; they must accommodate burdens or justify regulations through strict scrutiny (high evidentiary bar). Suppression is not total because: (1) compelling interests can satisfy strict scrutiny; (2) least-restrictive-means analysis may affirm the regulation; (3) many religious burden claims fail on sincerity or burden grounds. Suppression rises from 0.50 to 0.62 over the interval as regulatory agencies learned to frame policies in compelling-interest language and accumulated litigation experience. Theater ratio (0.35): Moderate-low. The burden-accommodation framework is substantive (not purely performative) but generates considerable formalism: courts must conduct individualized sincerity assessments, compelling-interest analyses, and least-restrictive-means reviews even when outcomes are predictable. The framework's theater increases from 0.25 (early Sherbert, when burden-accommodation was novel) to 0.35 (approaching Smith, as the doctrine developed increasingly formalized procedures). Theater remains below the piton threshold (0.70) because the core coordination function (protecting religious practice from neutral-law infringement) is genuine, not merely performative.
 *
 * PERSPECTIVAL GAP:
 *   The Sherbert doctrine generates maximal perspectival divergence across structural positions. The burdened believer sees protection (Rope) — a coordination mechanism enabling religious practice. The religious institution sees mixed coordination and extraction (Tangled Rope) — protection alongside litigation burden. The regulator sees suppression and extraction (Snare) — inability to apply uniform rules. The judicial system sees institutional credibility and cost (Tangled Rope) — doctrinal coherence alongside intensive review burdens. The historical archive sees a formalized but sparse framework (Piton) — principled doctrine with rare success. The doctrinal logician sees an internally consistent constitutional principle (Mountain) — strict scrutiny for religion-burdening laws follows necessarily from the Free Exercise premise. The core perspectival gap: between the beneficiary's experience (protection) and the regulator's experience (suppression). This gap is the signature of tangled-rope structure — the constraint provides genuine coordination (burden-accommodation) that benefits one party while imposing extraction cost on another.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective derives directionality from the agent's structural position relative to the Sherbert constraint. Burdened believers (powerless/trapped) experience low directionality (d ≈ 0.10–0.20) because they are primary beneficiaries with no exit — the constraint protects them. Religious institutions (organized/constrained) experience moderate directionality (d ≈ 0.35–0.45) — they benefit from the doctrine but must litigate to access protection, creating mixed benefit-cost. Regulators (moderate/constrained) experience high directionality (d ≈ 0.65–0.75) — they bear suppression cost and must justify regulations through strict scrutiny, making them net targets of the constraint's extraction. The judicial system (institutional/arbitrage) experiences moderate directionality (d ≈ 0.50) — courts benefit from doctrinal clarity and institutional legitimacy but extract cost from intensive review burden. The committer frame (Smith competing reading) occupies analytical position (d ≈ 0.73) — it observes the structure without being embedded in it, but its own emergence (Smith's 1990 decision to overturn Sherbert for neutral laws) reveals that the constraint's directionality calculation shifted enough to justify a foundational doctrinal reversal.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING MANDATROPHY — FORECLOSE VS. COEXIST: The critical mandatrophy question is whether the Sherbert compelling-interest reading and the Smith neutral-law reading foreclose each other (logically incompatible) or coexist (live positions in different contexts). The Smith decision (1990) treated them as foreclosing: once Employment Division v. Smith established that neutral laws of general applicability do not trigger strict scrutiny for religious burdens, the Sherbert framework's applicability was sharply narrowed. However, Congress responded by enacting the Religious Freedom Restoration Act (1993), explicitly restoring Sherbert-like protections for federal regulations and spawning state-level equivalents. This pattern suggests that the readings coexist rather than foreclose: Sherbert remains live in statutory contexts (RFRA, state RFRAs, carve-outs for religious autonomy) while Smith governs core constitutional doctrine. The mandatrophy resolves by recognizing that the kernel (Free Exercise Clause) admits multiple readings, and the constraint's classification depends on which reading is ascendent. In Sherbert era (1963–1990), this constraint is tangled_rope. In Smith era (1990–present), the Sherbert reading persists in statutory RFRA contexts, maintaining tangled_rope structure for those domains while constitutional strict scrutiny no longer applies to neutral laws. The engine must track both: one story (this one) for Sherbert as a constitutional reading, another for RFRA as a statutory instantiation of Sherbert-like protection.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sincere_belief_verification_burden,
    'What epistemic standard determines whether a believer''s claimed burden on religious practice is ''sincere'' enough to trigger Sherbert review? Can courts reliably distinguish sincere religious burdens from contrived claims or philosophical objections?',
    'Empirical analysis of litigation patterns: correlation between judicial sincerity findings and subsequent believer compliance behavior; meta-analysis of appellate reversals on sincerity grounds; comparison of sincerity standards across denominational contexts',
    'If sincerity is verifiable at reasonable cost: the doctrine functions as intended, distributing protection fairly. If sincerity verification is unreliable or captures philosophical objections: the constraint drifts toward either under-protection (false negatives) or over-accommodation (false positives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sincere_belief_verification_burden, empirical, 'Epistemology of sincere religious belief for Sherbert purposes').

omega_variable(
    state_interest_stringency_empirical,
    'In practice, what counts as a ''compelling state interest'' sufficient to satisfy strict scrutiny? Do courts apply a genuinely exacting standard (requiring proof that the regulation is narrowly tailored to protect a vital government interest), or does ''compelling'' function as a lower threshold in practice?',
    'Quantitative analysis of appellate decisions: percentage of religious burden claims that survive strict scrutiny review; correlation between claimed state interests and actual judicial approval rates; historical trend analysis from Sherbert (1963) through Smith (1990) to post-Smith decisions that resurrect Sherbert-like analysis',
    'If compelling interest test is stringent in practice: Sherbert provides genuine protection (snare classification for regulators is accurate). If compelling interest becomes routine justification: the doctrine''s protection erodes and extractiveness decreases (shifts toward piton or rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_interest_stringency_empirical, empirical, 'Empirical stringency of compelling state interest test across appellate corpus').

omega_variable(
    neutral_law_doctrine_foreclosure,
    'Is the Sherbert compelling-interest reading logically foreclosed by the Smith neutral-law reading, or can both readings coexist as live judicial positions in different contexts?',
    'Doctrinal analysis of post-Smith jurisprudence: whether courts cite Sherbert as precedent in non-neutral-law contexts (e.g., unemployment, sanctuary policy); whether legislatures restore Sherbert-like protections through statute (RFRA, state RFRAs) despite Smith; whether the readings compete in the same cases or occupy different jurisdictional spaces',
    'If readings foreclose each other: one is discredited and the other prevails. If readings coexist: the constraint is realized differently in state vs federal contexts, or within carved-out statutory domains. Affects omega classification (conceptual vs empirical).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(neutral_law_doctrine_foreclosure, conceptual, 'Whether Sherbert and Smith readings are mutually exclusive or coexist as live positions').

omega_variable(
    kernel_reading_committer_frame,
    'Is this constraint best understood as a reading of a stable constitutional kernel (the Free Exercise Clause text), or as an interpretation that competes with equally valid readings (Smith) such that the kernel itself is contestable rather than stable?',
    'Textual analysis of the Free Exercise Clause (''Congress shall make no law... prohibiting the free exercise [of religion]''); examination of founding-era intent; assessment of whether the clause''s boundaries are semantically determinate or interpretively contested. Track whether subsequent legal authority (Sherbert, Smith, RFRA) treats the kernel as stable with competing readings, or treats the kernel itself as unstable.',
    'If kernel is stable and Sherbert is one reading: the committer frame (kernel_context, cs_structure) is correctly applied. If kernel is itself contested (whether ''free exercise'' includes substantive protection or only formal neutrality): the constraint may be misclassified as a reading when it is better understood as a foundational claim in a deeper kernel contest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_frame, conceptual, 'Whether free exercise clause is a stable kernel with competing readings or a foundational contest').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(free_exercise_clause__sherbert_compelling_interest_reading, 0, 27).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fec_sherbert_theater_t0, free_exercise_clause__sherbert_compelling_interest_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(fec_sherbert_theater_t10, free_exercise_clause__sherbert_compelling_interest_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(fec_sherbert_theater_t27, free_exercise_clause__sherbert_compelling_interest_reading, theater_ratio, 27, 0.35).

% Extraction over time
narrative_ontology:measurement(fec_sherbert_extract_t0, free_exercise_clause__sherbert_compelling_interest_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(fec_sherbert_extract_t10, free_exercise_clause__sherbert_compelling_interest_reading, base_extractiveness, 10, 0.22).
narrative_ontology:measurement(fec_sherbert_extract_t27, free_exercise_clause__sherbert_compelling_interest_reading, base_extractiveness, 27, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(fec_sherbert_supp_t0, free_exercise_clause__sherbert_compelling_interest_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(fec_sherbert_supp_t10, free_exercise_clause__sherbert_compelling_interest_reading, suppression_requirement, 10, 0.57).
narrative_ontology:measurement(fec_sherbert_supp_t27, free_exercise_clause__sherbert_compelling_interest_reading, suppression_requirement, 27, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(free_exercise_clause__sherbert_compelling_interest_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(free_exercise_clause__sherbert_compelling_interest_reading, free_exercise_clause__smith_neutral_law_reading).
narrative_ontology:affects_constraint(free_exercise_clause__sherbert_compelling_interest_reading, religious_freedom_restoration_act_statutory_reading).

% DUAL FORMULATION NOTE:
% The Sherbert compelling-interest reading and the Smith neutral-law reading are sibling readings of the same kernel (Free Exercise Clause). They have structurally different extractiveness, suppression, and beneficiary/victim profiles because they recognize different constitutional scope and protection level for religious practice. Sherbert treats religious burdens as triggering heightened scrutiny; Smith treats them as subject to rational-basis review unless the law targets religion specifically. The RFRA statutory reading instantiates Sherbert-like protection through statute, bypassing Smith's constitutional holding. All three stories (Sherbert-reading, Smith-reading, RFRA-reading) are linked via affects_constraints to show their doctrinal interdependence and to enable contamination analysis — shifts in one reading's legitimacy pressure the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
