% ============================================================================
% CONSTRAINT STORY: free_press_clause__prior_restraint_doctrine
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_free_press_clause__prior_restraint_doctrine, []).

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
 *   constraint_id: free_press_clause__prior_restraint_doctrine
 *   human_readable: Prior Restraint Doctrine Under the Free Press Clause
 *   domain: constitutional_law/press_freedom
 *
 * SUMMARY:
 *   The prior restraint doctrine is the constitutional rule that came
 *   earliest and has remained strongest in press freedom jurisprudence: the
 *   presumption against prior restraint (advance suppression of speech before
 *   publication) applies absolutely unless the government meets a nearly
 *   impossible burden of proving grave, imminent danger. Even the Pentagon
 *   Papers case—government's attempt to block publication of classified
 *   material revealing government deception about the Vietnam War—could not
 *   overcome the presumption. This reading of the Free Press Clause stands in
 *   tension with two other major readings: the press-as-technology reading
 *   (which protects every user of publication means equally, rejecting the
 *   institutional guild model) and the reporters' privilege question (which
 *   asks whether the clause protects journalists' sources, an issue left
 *   unresolved at the constitutional level despite state-law protections).
 *   The prior restraint doctrine instantiates a specific constitutional
 *   choice: the burden of suppression rests entirely on government;
 *   publishers face only post-publication remedies (damages, contempt, civil
 *   liability). This allocation is presented as the Framers' original
 *   intention but is actually a doctrinal reading that competes with
 *   alternatives.
 *
 * KEY AGENTS:
 *   - Publishers and News Organizations: Primary beneficiary (institutional/arbitrage) — the prior restraint presumption enables them to publish first, establish facts on the ground, and litigate afterwards if government seeks restraint
 *   - Government Executive: Powerful but constrained actor (powerful/mobile) — retains legitimate interests in prior restraint (national security, classified information) but must overcome heavy presumption; experiences the doctrine as both coordination and extraction
 *   - Public Discourse / General Audience: Beneficiary (analytical/analytical) — the doctrine's presumption enables broader information access by preventing executive suppression
 *   - Interpretive Tradition (Courts): Institutional enforcer (institutional/arbitrage) — applies the presumption against prior restraint; maintains the doctrine's boundaries through case law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(free_press_clause__prior_restraint_doctrine, 0.38).
domain_priors:suppression_score(free_press_clause__prior_restraint_doctrine, 0.08).
domain_priors:theater_ratio(free_press_clause__prior_restraint_doctrine, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(free_press_clause__prior_restraint_doctrine, extractiveness, 0.38).
narrative_ontology:constraint_metric(free_press_clause__prior_restraint_doctrine, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(free_press_clause__prior_restraint_doctrine, theater_ratio, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(free_press_clause__prior_restraint_doctrine, rope).
narrative_ontology:human_readable(free_press_clause__prior_restraint_doctrine, "Prior Restraint Doctrine Under the Free Press Clause").
narrative_ontology:topic_domain(free_press_clause__prior_restraint_doctrine, "constitutional_law/press_freedom").

domain_priors:requires_active_enforcement(free_press_clause__prior_restraint_doctrine).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(free_press_clause__prior_restraint_doctrine, 'fe83e6d6-8c7d-456d-8aa2-93b427d71021').
narrative_ontology:cs_kernel_codification('fe83e6d6-8c7d-456d-8aa2-93b427d71021', fixed_text).
narrative_ontology:cs_authority_grounding('fe83e6d6-8c7d-456d-8aa2-93b427d71021', lineage).
narrative_ontology:cs_interpretation_layer_present('fe83e6d6-8c7d-456d-8aa2-93b427d71021').
narrative_ontology:cs_reading_relation('fe83e6d6-8c7d-456d-8aa2-93b427d71021', free_press_clause__press_as_technology_reading, coexists_with).
narrative_ontology:cs_reading_relation('fe83e6d6-8c7d-456d-8aa2-93b427d71021', free_press_clause__reporters_privilege_question, influences).
narrative_ontology:cs_axiom('fe83e6d6-8c7d-456d-8aa2-93b427d71021', foundational, prior_restraint_presumptively_unconstitutional).
narrative_ontology:cs_axiom_status(prior_restraint_presumptively_unconstitutional, holdable).
narrative_ontology:cs_axiom_grounding('fe83e6d6-8c7d-456d-8aa2-93b427d71021', prior_restraint_presumptively_unconstitutional, deontological).
narrative_ontology:cs_axiom('fe83e6d6-8c7d-456d-8aa2-93b427d71021', foundational, government_burden_of_proof_near_absolute).
narrative_ontology:cs_axiom_status(government_burden_of_proof_near_absolute, holdable).
narrative_ontology:cs_axiom_grounding('fe83e6d6-8c7d-456d-8aa2-93b427d71021', government_burden_of_proof_near_absolute, deontological).
narrative_ontology:cs_reference_frame('fe83e6d6-8c7d-456d-8aa2-93b427d71021', framers_original_press_freedom_intent).
narrative_ontology:cs_drift_state('fe83e6d6-8c7d-456d-8aa2-93b427d71021', contemporary_digital_media_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('fe83e6d6-8c7d-456d-8aa2-93b427d71021', '').
narrative_ontology:cs_kernel_id(free_press_clause__prior_restraint_doctrine, free_press_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(free_press_clause__prior_restraint_doctrine, publishers).
narrative_ontology:constraint_beneficiary(free_press_clause__prior_restraint_doctrine, news_organizations).
narrative_ontology:constraint_beneficiary(free_press_clause__prior_restraint_doctrine, public_discourse).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PUBLISHERS AND NEWS ORGANIZATIONS (ROPE) — The prior restraint doctrine creates genuine coordination: it enables publishers to race injunctions, to publish first and litigate after, establishing newsworthiness and public interest as structural facts. The doctrine's presumption against prior restraint is not extraction but coordination protocol — publishers can move freely to the printing press. Experienced extractiveness is low; the constraint is functional coordination of pre-publication speech rights.
constraint_indexing:constraint_classification(free_press_clause__prior_restraint_doctrine, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 2: ANALYTICAL OBSERVER / DOCTRINAL VIEW (ROPE) — From a constitutional doctrine perspective, the prior restraint rule is pure coordination: it establishes a bright-line principle (presumption against prior restraint with narrow exceptions: national security, obscenity, incitement) and allocates burden of proof (government must meet heavy burden; publishers can publish pending adjudication). The doctrine has low theater — the rule is transparent and functionally clear. Low extractiveness. No identified victims.
constraint_indexing:constraint_classification(free_press_clause__prior_restraint_doctrine, rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: GOVERNMENT EXECUTIVE (TANGLED ROPE) — The government retains legitimate interests in prior restraint under narrow circumstances (national security, classified information protection). The doctrine permits restraint but requires demonstrating grave, imminent danger — a high bar. Government experiences the constraint as both coordination (it cannot arbitrarily suppress speech) and extraction (its legitimate secrecy interests are subordinated to presumption of publication). The mixing of coordination function (establishing rules both sides can rely on) and asymmetric extraction (favoring publication over prior restraint) makes this tangled rope.
constraint_indexing:constraint_classification(free_press_clause__prior_restraint_doctrine, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: NATURAL LAW READING (MOUNTAIN) — The prior restraint doctrine is presented as an immutable constitutional principle — the Framers recognized prior restraint as the paradigmatic abridgment of press freedom, and this recognition is irreducible to particular eras or technological contexts. The principle admits no compromise and allows no substitution. However, this perspective naturalizes what is actually a contingent doctrinal choice. The engine's false summit detector will flag this as a candidate for reclassification if beneficiaries (publishers) are identified as extracting value from the doctrine's structure.
constraint_indexing:constraint_classification(free_press_clause__prior_restraint_doctrine, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(free_press_clause__prior_restraint_doctrine_tests).
:- end_tests(free_press_clause__prior_restraint_doctrine_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-low. The prior restraint doctrine primarily enables publication through a presumption against advance suppression. Extractiveness arises from the asymmetry: publishers can publish and litigate, but government cannot use injunctions to prevent publication except under nearly impossible conditions. This is not high extraction because (a) government retains legitimate interests in narrow cases (national security, obscenity, incitement), (b) the rule is transparent and predictable, and (c) the coordination function is genuine—both publishers and government can rely on the bright-line presumption. The extractiveness value reflects the asymmetry toward publication in the allocation of burdens. Suppression (0.08): Very low. The doctrine explicitly minimizes suppression of pre-publication speech. The presumption against prior restraint removes prior restraint as a routine enforcement mechanism. Suppression is near-zero by design. Theater ratio (0.25): Low. The doctrine has minimal performative content; the rule is clear and functional. Courts do not theater-engage in applying prior restraint analysis—the legal test (government must show grave, imminent danger) is explicit and operationalizable. The low theater reflects that the doctrine does functional work, not performative work.
 *
 * PERSPECTIVAL GAP:
 *   The publishers' perspective (rope) sees coordination: the presumption enables them to act. The government's perspective (tangled rope) sees both coordination (clear rule) and extraction (their secrecy interests subordinated). The analytical observer's perspective (rope) sees functional coordination with low theater. The natural law perspective (mountain) sees an immutable constitutional principle—but this risks naturalizing what is actually a doctrinal choice that other readings contest. The perspectival gap reveals that the prior restraint rule is not self-evident from the text; it is an interpretive commitment that forecloses neither the press-as-technology reading (which applies the rule equally to all speakers) nor the reporters' privilege reading (which extends institutional protection to sources).
 *
 * DIRECTIONALITY LOGIC:
 *   Publishers experience the doctrine as coordination enabling their movement to the printing press (d ≈ 0.20, beneficiary with arbitrage exit → low f(d)). Government executives experience mixed coordination and extraction: the rule coordinates by establishing clear boundaries, but it extracts by subordinating government's legitimate secrecy interests to the presumption of publication (d ≈ 0.60, powerful actor with mobile exit but constrained by the presumption). The analytical observer experiences it as doctrinal coordination with minimal extractiveness (d ≈ 0.50, analytical context → moderate f(d)). No victims are identified in this reading—the doctrine is not framed as extraction from a powerless or trapped group. If the natural law perspective (mountain) is correct and the doctrine is immutable constitutional principle, then directionality derives from canonical falls (analytical → 0.73 → f(d) ≈ 1.15). But the false summit detector will flag this: if publishers benefit identifiably from the doctrine, the mountain classification becomes contestable.
 *
 * MANDATROPHY ANALYSIS:
 *   The prior restraint doctrine resolves the mandatrophy by showing that a clear doctrinal rule can produce different classifications from different structural positions. Publishers see rope (coordination enabling publication). Government sees tangled rope (coordination + extraction). Analytical observers see rope (functional doctrine). Natural law reading sees mountain (immutable principle)—but the structural beneficiary (publishers) suggests this is a false summit. The mandatrophy is not 'which classification is correct?' but 'which structural position are you analyzing from?' The doctrine is not intrinsically one type; it is multiply realized depending on whether you are analyzing it as a coordination mechanism (rope), an asymmetric allocation of burden (tangled rope), a doctrinal rule (rope), or a constitutional principle (mountain, likely false summit).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    national_security_exception_boundary,
    'Where is the boundary between legitimate government prior restraint (national security) and doctrinal suppression of legitimate government interests?',
    'Case-law analysis post-Pentagon Papers: frequency and success rate of government prior restraint requests; empirical assessment of harms to national security from publication vs. harms to press freedom from restraint',
    'If boundary is vague: suppression of government interests is higher than current measurement suggests (tangled_rope → snare). If boundary is clear: the coordination function is stronger and extractiveness remains low (rope confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(national_security_exception_boundary, empirical, 'Clarity and stability of the national security prior restraint exception').

omega_variable(
    technological_vs_doctrinal_reading,
    'Is the prior restraint doctrine grounded in the technology of printing (prior to digital) or in a principle about advance suppression that applies regardless of medium?',
    'Comparative analysis of how courts have applied the doctrine to digital media, social media, and networked publication; whether the doctrinal principle survives when the technological substrate changes',
    'If technology-bound: doctrine may be reclassified as a piton (degraded institutional form). If principle-bound: the mountain reading is more defensible; doctrine is invariant across media.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technological_vs_doctrinal_reading, conceptual, 'Whether prior restraint doctrine is grounded in printing technology or an abstract principle').

omega_variable(
    pentagon_papers_as_ceiling_vs_floor,
    'Does the Pentagon Papers precedent represent a floor (no prior restraint under any circumstance) or a ceiling (prior restraint only in the most extreme national security cases)?',
    'Historical review of government prior restraint attempts post-Pentagon Papers; analysis of which cases succeeded and under what doctrine',
    'If floor: extractiveness is zero — no restraint allowed. If ceiling: extractiveness is higher — government retains legitimate restraint authority in narrow cases. Affects classification boundaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pentagon_papers_as_ceiling_vs_floor, conceptual, 'Interpretive status of Pentagon Papers in the prior restraint doctrine framework').

omega_variable(
    kernel_reading_contest,
    'Is the prior restraint doctrine one reading of the Free Press Clause, or is it THE content of the Free Press Clause as the Framers understood it?',
    'Historical and originalist scholarship on the Framers'' understanding of press freedom; comparison with alternative readings (press as technology, reporters'' privilege, institutional vs. individual rights)',
    'If one reading: the doctrine is contestable (sibling readings coexist). If THE reading: the doctrine is authoritative (other readings are derivative or foreclosed). Affects whether reading_relations should use forecloses or coexists_with.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Authorial status of prior restraint doctrine within Free Press Clause interpretation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(free_press_clause__prior_restraint_doctrine, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(priorrest_tr_t0, free_press_clause__prior_restraint_doctrine, theater_ratio, 0, 0.2).
narrative_ontology:measurement(priorrest_tr_t50, free_press_clause__prior_restraint_doctrine, theater_ratio, 50, 0.22).
narrative_ontology:measurement(priorrest_tr_t100, free_press_clause__prior_restraint_doctrine, theater_ratio, 100, 0.25).

% Extraction over time
narrative_ontology:measurement(priorrest_be_t0, free_press_clause__prior_restraint_doctrine, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(priorrest_be_t50, free_press_clause__prior_restraint_doctrine, base_extractiveness, 50, 0.35).
narrative_ontology:measurement(priorrest_be_t100, free_press_clause__prior_restraint_doctrine, base_extractiveness, 100, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(free_press_clause__prior_restraint_doctrine, enforcement_mechanism).
narrative_ontology:affects_constraint(free_press_clause__prior_restraint_doctrine, free_press_clause__press_as_technology_reading).
narrative_ontology:affects_constraint(free_press_clause__prior_restraint_doctrine, free_press_clause__reporters_privilege_question).

% DUAL FORMULATION NOTE:
% The prior restraint doctrine is one reading of the Free Press Clause kernel. The press_as_technology_reading and reporters_privilege_question are sibling readings instantiating the same kernel with different structural implications. The prior restraint doctrine focuses on pre-publication control and its prohibition; the technology reading focuses on who holds the right (individual vs. institutional); the privilege question focuses on post-publication protection of sources. Each reading has its own extractiveness value and its own beneficiary/victim structure. Together they decompose the contested Free Press Clause into its structurally distinct components.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
