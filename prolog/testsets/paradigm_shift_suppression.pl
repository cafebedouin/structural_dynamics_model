% ============================================================================
% CONSTRAINT STORY: paradigm_shift_suppression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_paradigm_shift_suppression, []).

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
 *   constraint_id: paradigm_shift_suppression
 *   human_readable: Paradigm Shift Suppression in Knowledge Systems
 *   domain: epistemology/institutional_governance
 *
 * SUMMARY:
 *   Paradigm shift suppression is the institutional mechanism by which
 *   knowledge systems resist claims that fundamentally challenge the
 *   conceptual foundations of an established field. This constraint operates
 *   across all knowledge domains — physics, biology, medicine, economics,
 *   psychology — and exhibits a characteristic structural pattern: a
 *   suppression mechanism enforced through peer review gatekeeping, funding
 *   rejection, career risk, and reputation management that benefits the
 *   incumbent paradigm defenders while extracting costs from paradigm shift
 *   proponents. The constraint is not inevitable but actively maintained
 *   through institutional structures (journals, funding agencies,
 *   professional societies, hiring committees) that have vested interests in
 *   the incumbent framework. The theater ratio of 0.65 reflects that much of
 *   the gatekeeping is justified through methodological critiques and quality
 *   concerns that mask paradigm-preference gatekeeping. The extractiveness of
 *   0.58 indicates that the suppression is substantial but not total —
 *   alternative paradigms eventually break through, typically over
 *   generational timescales. The tangled_rope classification reflects that
 *   the constraint contains both a genuine coordination function
 *   (establishing shared frameworks enables cumulative science) and
 *   asymmetric extraction (the coordination benefits are asymmetrically
 *   distributed toward incumbent defenders). The measurement trajectory shows
 *   increasing extractiveness (0.35 → 0.62) and increasing theater (0.50 →
 *   0.72) over a 30-year interval, indicating that the suppression mechanism
 *   has intensified as paradigm-challenging work has become more visible.
 *
 * KEY AGENTS:
 *   - Emerging Paradigm Proponents: Primary victims (powerless/trapped) — early-career researchers whose innovative ideas face systematic exclusion from publication, funding, and institutional prestige pathways
 *   - Incumbent Paradigm Establishment: Primary beneficiaries (institutional/arbitrage) — universities, funding agencies, journals, senior researchers whose institutional positions and intellectual capital are invested in the dominant framework
 *   - Transitional Researchers: Secondary victims (moderate/constrained) — mid-career scholars recognizing paradigm limitations but facing high costs to full commitment to alternatives
 *   - Alternative Publishing Coalition: Organized agents (organized/constrained) — preprint platforms, open-access journals, open-science advocates building sunset pathways around traditional gatekeeping
 *   - Peer Review System: Institutional actor (institutional/arbitrage) — maintenance mechanism that appears to institutional actors as quality control but functions as paradigm-preference gatekeeping
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing institutional inertia as law of nature rather than contingent design choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paradigm_shift_suppression, 0.58).
domain_priors:suppression_score(paradigm_shift_suppression, 0.68).
domain_priors:theater_ratio(paradigm_shift_suppression, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paradigm_shift_suppression, extractiveness, 0.58).
narrative_ontology:constraint_metric(paradigm_shift_suppression, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(paradigm_shift_suppression, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paradigm_shift_suppression, tangled_rope).
narrative_ontology:human_readable(paradigm_shift_suppression, "Paradigm Shift Suppression in Knowledge Systems").
narrative_ontology:topic_domain(paradigm_shift_suppression, "epistemology/institutional_governance").

domain_priors:requires_active_enforcement(paradigm_shift_suppression).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paradigm_shift_suppression, incumbent_paradigm_defenders).
narrative_ontology:constraint_beneficiary(paradigm_shift_suppression, established_institutional_gatekeepers).
narrative_ontology:constraint_victim(paradigm_shift_suppression, emerging_paradigm_proponents).
narrative_ontology:constraint_victim(paradigm_shift_suppression, scientific_progress).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMERGING PARADIGM PROPONENT (SNARE) — Early-career researchers advocating genuinely novel frameworks face systematic exclusion: peer review gatekeeping, funding rejection, conference rejection, journal desk-rejects, and career termination risk. No exit without abandoning the innovative claim. Experiences maximum extraction with suppression of alternatives to the dominant paradigm.
constraint_indexing:constraint_classification(paradigm_shift_suppression, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: TRANSITIONAL RESEARCHER (TANGLED ROPE) — Mid-career researchers who recognize limitations of the incumbent paradigm but face high costs to full commitment to a new one. They benefit from incremental work within the established framework (publications, funding, institutional support) while also bearing costs of professional risk if they shift commitments. Some coordination occurs (their incremental work does advance understanding) but asymmetric extraction favors those committed to the incumbent.
constraint_indexing:constraint_classification(paradigm_shift_suppression, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT PARADIGM ESTABLISHMENT (ROPE) — Universities, funding agencies, journals, and senior researchers benefit from the suppression mechanism. For them, the constraint functions as pure coordination: it maintains field coherence, directs resources efficiently (by their metrics), and preserves institutional reputation. The suppression appears to them as normal scientific process, not extraction. Arbitrage option allows them to adopt new paradigms when truly dominant (Kuhnian revolutions), capturing first-mover advantage.
constraint_indexing:constraint_classification(paradigm_shift_suppression, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ALTERNATIVE PUBLISHING COALITION (SCAFFOLD) — Preprints (arXiv, bioRxiv), open-access journals, ResearchGate, and direct-to-public communication platforms create sunset pathways around traditional gatekeeping. These platforms have sunset logic: as they gain credibility and adoption, they reduce the institutional gatekeepers' extraction power. Organized agents (researchers, technologists, funding agencies) are actively building these alternatives. Sunset timeline: 15-25 years for genuine parity in career prestige.
constraint_indexing:constraint_classification(paradigm_shift_suppression, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: PEER REVIEW RITUAL (PITON) — Blind peer review for paradigm-challenging claims is largely performative theater. Reviewers assess writing quality and internal logical consistency but cannot truly evaluate whether the new paradigm better captures reality — that requires generational time scales. The ritual persists through institutional inertia (journals need review to maintain legitimacy) despite low actual verification function. High theater ratio reflects that rejection rationale often masks paradigm-preference gatekeeping as methodological critique.
constraint_indexing:constraint_classification(paradigm_shift_suppression, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, paradigm shift suppression appears as an immutable feature of how knowledge systems work: established frameworks always resist new ones because they have invested intellectual and institutional capital. This perspective sees the suppression as a law of inertial systems, not a contingent extraction mechanism. However, the structural data (active enforcement, beneficiaries/victims, high theater) contradicts the mountain classification — the engine identifies this as a false summit, revealing that 'human nature' narratives naturalize what are actually institutional arrangements subject to redesign.
constraint_indexing:constraint_classification(paradigm_shift_suppression, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paradigm_shift_suppression_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(paradigm_shift_suppression, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(paradigm_shift_suppression, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(paradigm_shift_suppression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(paradigm_shift_suppression, TR),
    TR >= 0.70.

:- end_tests(paradigm_shift_suppression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The suppression mechanism extracts substantial costs from paradigm shift proponents through career risk, funding denial, and publication rejection, but the extraction is not total — some alternative paradigms eventually gain acceptance (Kuhn's revolutions do occur). The trajectory shows increasing extraction over 30 years, indicating that suppression has intensified as communication technologies and funding scales have concentrated decision-making power. Suppression (0.68): High. Barriers to exit are substantial: career commitment, institutional sunk costs, lack of alternative funding sources, publication bias against negative/paradigm-challenging results, and social proof effects (if prestigious researchers reject your work, others follow). These barriers are structural, not merely reputational. Theater ratio (0.65): Moderate-high. Peer review serves genuine quality-control functions (catching errors, improving clarity) but also serves paradigm-gatekeeping functions masked as methodology critiques. Reviewers assessing methodological soundness often conflate 'methods are unconventional' with 'methods are unsound.' The theater has increased over the interval as paradigm-challenging work has become more visible, requiring more elaborate gatekeeping justifications.
 *
 * PERSPECTIVAL GAP:
 *   The emerging paradigm proponent sees a snare — pure extraction with no coordination benefit and no exit. The incumbent establishment sees rope — they experience the constraint as legitimate scientific governance that maintains field coherence. The analytical observer risks seeing a mountain — paradigm resistance appears as an immutable law of how humans process information and defend invested positions — but the structural data reveals this as a false summit. The constraint requires active enforcement (peer review rejection, funding denial), has identifiable beneficiaries and victims, and shows increasing theater over time. These are hallmarks of a contingent institutional arrangement, not a law of nature. The alternative publishing coalition sees a scaffold — existing alternative mechanisms (preprints, open science) can eventually replace traditional gatekeeping, with a sunset timeline of 15-25 years. The perspectival gap reveals that the constraint's classification depends entirely on one's structural position: whether you benefit from the incumbent paradigm's protection (rope), whether you bear costs trying to challenge it (snare), or whether you can see the whole system as a contingent institutional arrangement (scaffold, piton, mountain).
 *
 * DIRECTIONALITY LOGIC:
 *   The emerging paradigm proponent (powerless/trapped) experiences d ≈ 0.95, producing f(d) ≈ 1.42 and high χ. They bear the full cost of suppression with no exit option short of abandoning their research agenda. The incumbent establishment (institutional/arbitrage) experiences d ≈ 0.05, producing f(d) ≈ -0.12 and negative χ — extraction flows toward them. The transitional researcher (moderate/constrained) experiences d ≈ 0.65, producing f(d) ≈ 1.00 and moderate χ. These directionality values are not overridden — they follow directly from the structural relationship: who benefits (incumbent defenders), who bears costs (paradigm shift proponents), and what exit options each has (none for proponents at early career, arbitrage for establishment). The constraint maintains its extraction force through these asymmetric exit options combined with institutional enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   EXEMPLAR OF INSTITUTIONALIZED FALSE MOUNTAIN: The most diagnostic feature of this constraint is the temptation to classify it as a mountain — to naturalize paradigm suppression as inevitable human psychology. 'People always defend their beliefs,' 'institutions always resist change,' 'paradigm shifts always take a generation' (waiting for old believers to retire). These are cover stories that naturalize what are actually institutional choices. The structural data contradicts the mountain classification: (1) active enforcement required (peer review gatekeeping, funding rejection), (2) identifiable beneficiaries (incumbent defenders) and victims (paradigm shift proponents), (3) increasing theater ratio over time, indicating ritualization, (4) alternative mechanisms (preprints, open science) demonstrating that suppression is not inevitable. The mandatrophy is resolved by recognizing that the suppression mechanism is institutionally contingent, not naturally inevitable. The snare and tangled_rope classifications from the powerless and moderate perspectives are the correct structural readings. The mountain classification from the civilizational analytical perspective is a false summit — an artifact of perspective collapse where 'generational timescales make change slow' is misread as 'change is impossible.' The correct analytical reading, from knowledge of the alternative publishing coalition's scaffold, is that the suppression mechanism is temporary and subject to institutional redesign.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    paradigm_quality_assessment,
    'How can reviewers assess whether a paradigm-challenging claim is truly superior versus merely novel and unfamiliar?',
    'Long-term empirical validation: track which paradigm-challenging claims rejected by peer review later became foundational; compare acceptance rates and rejection rationales for claims that succeeded vs failed',
    'If reviewers successfully filter: paradigm suppression is legitimate quality control (reclassify as higher rope component). If reviewers fail systematically: suppression mechanism is largely protective gatekeeping (reclassify as pure snare from beneficiary perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(paradigm_quality_assessment, empirical, 'Whether peer review can assess paradigm-challenging claims fairly').

omega_variable(
    alternative_publishing_credibility_parity,
    'Will preprints and open-access platforms achieve genuine parity with traditional journals in career prestige and resource allocation?',
    'Longitudinal tracking of hiring, promotion, and funding decisions: monitor whether CV lines from arXiv/bioRxiv preprints receive equal weight to journal publications in competitive evaluations',
    'If parity achieved: scaffold sunset is real, suppression mechanism loses enforceability (15-25 year timeline confirmed). If parity fails: alternative platforms remain secondary tier (scaffold perspective is aspirational, not structural).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_publishing_credibility_parity, empirical, 'Whether alternative publishing achieves career parity with traditional journals').

omega_variable(
    incumbent_paradigm_defensibility,
    'Does the incumbent paradigm face genuine empirical challenges, or is suppression of alternatives motivated primarily by institutional self-protection?',
    'Comparative analysis of unsolved problems: document anomalies within incumbent paradigm; assess whether emerging paradigm proponents address genuine empirical gaps or merely present alternative framings',
    'If incumbent faces genuine anomalies: suppression is extractive rent-seeking (snare/tangled_rope classifications confirmed). If incumbent is empirically adequate: suppression is legitimate quality filtering (reclassify snare to higher-stage rope).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(incumbent_paradigm_defensibility, conceptual, 'Whether suppression responds to genuine empirical challenges').

omega_variable(
    career_lock_irreversibility,
    'Is commitment to a suppressed paradigm functionally irreversible within a single career arc?',
    'Biographical tracking of researchers who switched paradigm commitment: measure career trajectory disruption, publication lag, funding recovery timeline, and institutional re-acceptance post-switch',
    'If irreversible: exit_options should be ''trapped'' rather than ''constrained'' for emerging paradigm proponents (reclassify snare as mountain of institutional structure). If reversible: constrained classification is accurate, and recovery pathways exist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(career_lock_irreversibility, empirical, 'Whether paradigm commitment creates irreversible career lock-in').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paradigm_shift_suppression, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(paradigm_tr_t0, paradigm_shift_suppression, theater_ratio, 0, 0.5).
narrative_ontology:measurement(paradigm_tr_t10, paradigm_shift_suppression, theater_ratio, 10, 0.6).
narrative_ontology:measurement(paradigm_tr_t20, paradigm_shift_suppression, theater_ratio, 20, 0.65).
narrative_ontology:measurement(paradigm_tr_t30, paradigm_shift_suppression, theater_ratio, 30, 0.72).

% Extraction over time
narrative_ontology:measurement(paradigm_be_t0, paradigm_shift_suppression, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(paradigm_be_t10, paradigm_shift_suppression, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(paradigm_be_t20, paradigm_shift_suppression, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(paradigm_be_t30, paradigm_shift_suppression, base_extractiveness, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paradigm_shift_suppression, information_standard).
narrative_ontology:affects_constraint(paradigm_shift_suppression, citation_privilege_concentration).
narrative_ontology:affects_constraint(paradigm_shift_suppression, funding_gatekeeping_inequality).
narrative_ontology:affects_constraint(paradigm_shift_suppression, conference_review_bottleneck).

% DUAL FORMULATION NOTE:
% Paradigm shift suppression decomposes into domain-specific instantiations. The core constraint (institutional resistance to paradigm-challenging claims) affects downstream constraints in funding allocation, publication gatekeeping, and career pathways. Each domain has its own extractiveness value reflecting local institutional structures, but all share the suppression mechanism and theater-ratio inflation over time.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(paradigm_shift_suppression, institutional, 0.08).
constraint_indexing:directionality_override(paradigm_shift_suppression, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
