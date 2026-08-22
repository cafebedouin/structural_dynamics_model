% ============================================================================
% CONSTRAINT STORY: kodashim_obligation__study_as_archive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_obligation__study_as_archive, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: kodashim_obligation__study_as_archive
 *   human_readable: Kodashim Study as Historical-Preservation Archive (Non-Obligatory Reading)
 *   domain: religious/textual/institutional
 *
 * SUMMARY:
 *   This constraint instantiates the 'study as archive' reading of the
 *   kodashim_obligation kernel: within this reading, sustained institutional
 *   study of the sacrificial-law tractates (Kodashim) is understood as
 *   historical preservation and identity-maintenance work, not as
 *   satisfaction of a live legal obligation or as a cosmic act with present
 *   spiritual efficacy. The referent for extractiveness is the standing
 *   curricular arrangement as this reading's adherents see it — a system that
 *   allocates real scholarly and communal resources to a body of law its own
 *   framework holds to be functionally defunct. As Temple destruction recedes
 *   historically and restoration recedes as a live near-term expectation for
 *   most communities holding this reading, the archive function has
 *   increasingly become the entire justification, and theater_ratio rises
 *   correspondingly (from largely functional preservation early on toward
 *   heavier ceremonial/identity performance later).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__study_as_archive, 0.42).
domain_priors:suppression_score(kodashim_obligation__study_as_archive, 0.31).
domain_priors:theater_ratio(kodashim_obligation__study_as_archive, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, extractiveness, 0.42).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__study_as_archive, tangled_rope).
narrative_ontology:human_readable(kodashim_obligation__study_as_archive, "Kodashim Study as Historical-Preservation Archive (Non-Obligatory Reading)").
narrative_ontology:topic_domain(kodashim_obligation__study_as_archive, "religious/textual/institutional").

domain_priors:requires_active_enforcement(kodashim_obligation__study_as_archive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__study_as_archive, '013671fb-9fa5-4e70-84d2-a87a49ac1d33').
narrative_ontology:cs_kernel_codification('013671fb-9fa5-4e70-84d2-a87a49ac1d33', fixed_text).
narrative_ontology:cs_authority_grounding('013671fb-9fa5-4e70-84d2-a87a49ac1d33', lineage).
narrative_ontology:cs_interpretation_layer_present('013671fb-9fa5-4e70-84d2-a87a49ac1d33').
narrative_ontology:cs_reading_relation('013671fb-9fa5-4e70-84d2-a87a49ac1d33', kodashim_obligation__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('013671fb-9fa5-4e70-84d2-a87a49ac1d33', kodashim_obligation__study_as_preparation, influences).
narrative_ontology:cs_axiom('013671fb-9fa5-4e70-84d2-a87a49ac1d33', foundational, sacrificial_system_is_functionally_defunct).
narrative_ontology:cs_axiom_status(sacrificial_system_is_functionally_defunct, holdable).
narrative_ontology:cs_axiom_grounding('013671fb-9fa5-4e70-84d2-a87a49ac1d33', sacrificial_system_is_functionally_defunct, empirically_contingent).
narrative_ontology:cs_axiom('013671fb-9fa5-4e70-84d2-a87a49ac1d33', foundational, study_value_is_historical_and_identity_based_not_legal).
narrative_ontology:cs_axiom_status(study_value_is_historical_and_identity_based_not_legal, holdable).
narrative_ontology:cs_axiom_grounding('013671fb-9fa5-4e70-84d2-a87a49ac1d33', study_value_is_historical_and_identity_based_not_legal, conventional).
narrative_ontology:cs_reference_frame('013671fb-9fa5-4e70-84d2-a87a49ac1d33', temple_era_functional_sacrificial_system).
narrative_ontology:cs_drift_state('013671fb-9fa5-4e70-84d2-a87a49ac1d33', contemporary_post_exilic_diaspora, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('013671fb-9fa5-4e70-84d2-a87a49ac1d33', '').
narrative_ontology:cs_kernel_id(kodashim_obligation__study_as_archive, kodashim_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_archive, communal_identity_institutions).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_archive, yeshiva_curriculum_administrators).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_archive, applicable_halakha_scholarship).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_archive, students_diverted_from_practical_law).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_archive, textual_historians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the study cycle (daf yomi, seder learning schedules) that allocates a fixed proportion of communal learning hours to Kodashim tractates. On the archive reading, they justify this allocation as historical-continuity and identity preservation rather than legal necessity, and they control which tractates receive institutional resources, prestige, and instructional time.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, yeshiva_curriculum_administrators, agenda_setter,
    institutional, generational, arbitrage, national).

% Denominational and communal bodies draw continuity and legitimacy from unbroken transmission of the full Talmudic corpus, including the sacrificial orders. They benefit from the archive function even though it produces no applicable ruling — the study itself signals fidelity to tradition and binds the community's self-conception to an unbroken chain of learning.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, communal_identity_institutions, beneficiary,
    organized, generational, constrained, national).

% The body of scholarship addressing questions that actually arise in contemporary religious practice (kashrut, family law, civil law, medical ethics) competes for the same finite pool of scholarly attention, teaching hours, and institutional prestige that Kodashim study consumes. Under the archive reading, hours spent on sacrificial procedure are hours not spent developing or refining living legal doctrine.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, applicable_halakha_scholarship, payer,
    moderate, biographical, constrained, national).

% Students within the standard curriculum are required to master Kodashim tractates as part of ordination or advanced learning tracks, regardless of whether they will ever adjudicate a question touching sacrificial law. Their study time is fixed by institutional sequence, not by personal choice of applicable relevance; opting out risks credentialing and communal standing.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, students_diverted_from_practical_law, payer,
    powerless, biographical, trapped, regional).

% Scholars of ancient Israelite religion, Second Temple history, and comparative ritual systems benefit directly from the preservation function — Kodashim is a primary source for reconstructing Temple-period practice. Their interest is served regardless of whether the material carries live legal force.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, textual_historians, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(kodashim_obligation__study_as_archive, textual_historians, observer).

% Groups oriented toward literal Temple rebuilding and resumption of sacrificial practice would object to framing the material as a closed historical archive; on the archive reading their project is treated as structurally impossible or undesired, and their voice is not represented in the curricular decision to preserve-without-obligate.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, temple_restorationist_communities, excluded,
    moderate, civilizational, identity_locked, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_obligation__study_as_archive, communal_identity_institutions).
narrative_ontology:fixing_cost_class(kodashim_obligation__study_as_archive, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a community's relationship to its own textual past by keeping the full Talmudic corpus — including tractates with no current legal application — in continuous circulation, so that communal identity and scholarly literacy remain anchored to an unbroken transmission chain rather than a curated subset.
% TRANSFER_FUNCTION: Moves scholarly attention, teaching hours, and institutional prestige away from tractates and questions with live practical application and toward a defunct ritual system, in exchange for the intangible good of continuity and identity-signaling.
% ABSENT_VOICES: Temple-restorationist communities, who read the same texts as live and binding preparation, are not consulted when curricular administrators frame Kodashim as closed historical record; their objection — that this reading forecloses the possibility they are working toward — is not represented in the institutional decision.
% DISAPPEARANCE_RATIONALE: If Kodashim study vanished from the standard curriculum tomorrow, communal identity institutions and textual historians would experience a real loss of continuity and source material, and ordination tracks would need restructuring — but applicable halakhic scholarship would gain hours and resources currently diverted to non-actionable material. Whether the world 'rearranges' or 'stays the same' depends on which seat is asked, hence contested rather than settled.
% FOUNDING_PROBLEM: The original mandate was to preserve exhaustive knowledge of sacrificial law so that Temple service could resume without loss of technical detail if and when the Temple were rebuilt — a live functional and legal problem in antiquity and through much of the medieval period.
% FOUNDING_PROBLEM_CORROBORATION: Non-restorationist halakhic authorities and academic historians of religion (outside the communal-identity institutions that benefit from continued study) attest that Temple restoration is neither imminent nor, for many communities, doctrinally anticipated as a near-term practical event; the corroboration comes from outside the beneficiary set, though restorationist communities themselves dispute the 'dead' characterization entirely.
narrative_ontology:disappearance_verdict(kodashim_obligation__study_as_archive, contested).
narrative_ontology:founding_problem_status(kodashim_obligation__study_as_archive, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_obligation__study_as_archive, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kodashim_obligation__study_as_archive, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_obligation__study_as_archive, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_obligation__study_as_archive_tests).
:- end_tests(kodashim_obligation__study_as_archive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) rather than high: study genuinely produces a preserved, transmissible archive with real scholarly and historical value — this is not pure extraction. But it extracts legitimacy (communal-continuity credibility) from an activity structurally decoupled from any functional output the reading itself acknowledges as live, and it does so by consuming scholarly hours that could go to applicable law. Suppression is moderate (0.31): no one is coerced into believing the archive framing, but curricular sequencing effectively locks students into the allocation regardless of their view of its legal status. Theater ratio is authored high and rising (0.58 at T=100) because, absent restoration as a live prospect, an increasing share of the study's social function is performative continuity-signaling rather than either legal application or genuine historical-critical work.
 *
 * DIRECTIONALITY LOGIC:
 *   Communal identity institutions and curriculum administrators sit near the beneficiary end: they collect legitimacy and continuity capital from the arrangement and control its allocation. Applicable halakhic scholarship and diverted students sit near the target end: they bear an opportunity cost — hours, prestige, and institutional attention that could otherwise go to live legal questions — without a correspondingly available exit, since curricular sequence is largely fixed by ordination track rather than individual choice. Textual historians are a genuine beneficiary class outside the internal communal economy, benefiting from the archive regardless of its legal status.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents two mislabeling errors symmetrically: it does not treat Kodashim study as pure extraction with no coordination function (there is a real coordination good — preserved transmission, scholarly literacy, communal continuity — that a pure snare reading would miss), and it does not treat the arrangement as costless coordination (a pure rope reading would miss that the resource allocation has an identifiable payer class bearing real opportunity costs). Tangled rope captures both: genuine coordination (identity/continuity) riding alongside asymmetric cost-bearing (diverted scholarly attention) sustained by curricular enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    archive_vs_preparation_boundary,
    'Is the study genuinely oriented toward historical preservation with no restoration expectation, or does it retain latent preparatory function that the archive reading understates?',
    'Survey of stated institutional purpose across yeshivot and communal bodies; examination of whether curricular emphasis shifts when restorationist theology gains or loses adherents within a given community.',
    'If latent preparatory function is present, this constraint collapses toward the study_as_preparation reading and its extractiveness profile should be lower (the resource allocation would have a live future-directed justification this reading denies).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(archive_vs_preparation_boundary, conceptual, 'Whether the archive framing is a stable, distinct commitment or a soft version of the preparation reading.').

omega_variable(
    identity_value_measurability,
    'Can the communal-identity benefit this reading attributes to Kodashim study be measured independently of the institutions that assert it, or is the benefit claim self-reported by its own beneficiaries?',
    'Comparative study of communities that have reduced Kodashim curricular emphasis versus those that have not, measuring downstream identity/continuity outcomes (affiliation retention, textual literacy, institutional cohesion) by external sociological measures.',
    'If the identity benefit is not independently measurable, the beneficiary declaration rests on self-report from communal_identity_institutions, which weakens the coordination-function claim required for tangled_rope status and pushes the classification toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_value_measurability, empirical, 'Whether the claimed communal-identity benefit is externally verifiable or self-asserted by the beneficiary class.').

omega_variable(
    restoration_impossibility_claim,
    'Is Temple restoration genuinely structurally impossible/undesired within this reading''s own tradition, or is ''impossible'' doing normative work to justify present resource allocation away from restorationist projects?',
    'Textual and historical analysis of whether this reading''s proponents hold restoration as metaphysically impossible, practically improbable, or theologically undesired — these are distinct claims with different justificatory weight.',
    'If restoration is merely improbable rather than impossible, the archive reading''s foreclosure of the preparation reading is weaker than claimed, and the two readings are closer to coexisting within the same communities than the sharp reading-boundary suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restoration_impossibility_claim, conceptual, 'Whether the archive reading''s core premise is a strong metaphysical claim or a soft practical judgment doing stronger rhetorical work than its content supports.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__study_as_archive, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_obligation__study_as_archive, theater_ratio, 0, 0.1).
narrative_ontology:measurement(koda_tr_t20, kodashim_obligation__study_as_archive, theater_ratio, 20, 0.2).
narrative_ontology:measurement(koda_tr_t40, kodashim_obligation__study_as_archive, theater_ratio, 40, 0.33).
narrative_ontology:measurement(koda_tr_t60, kodashim_obligation__study_as_archive, theater_ratio, 60, 0.44).
narrative_ontology:measurement(koda_tr_t80, kodashim_obligation__study_as_archive, theater_ratio, 80, 0.52).
narrative_ontology:measurement(koda_tr_t100, kodashim_obligation__study_as_archive, theater_ratio, 100, 0.58).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_obligation__study_as_archive, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(koda_be_t20, kodashim_obligation__study_as_archive, base_extractiveness, 20, 0.22).
narrative_ontology:measurement(koda_be_t40, kodashim_obligation__study_as_archive, base_extractiveness, 40, 0.29).
narrative_ontology:measurement(koda_be_t60, kodashim_obligation__study_as_archive, base_extractiveness, 60, 0.34).
narrative_ontology:measurement(koda_be_t80, kodashim_obligation__study_as_archive, base_extractiveness, 80, 0.38).
narrative_ontology:measurement(koda_be_t100, kodashim_obligation__study_as_archive, base_extractiveness, 100, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kodashim_obligation__study_as_archive, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_obligation__study_as_archive, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_obligation__study_as_archive, 0.08).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_archive, kodashim_obligation__study_as_performance).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_archive, kodashim_obligation__study_as_preparation).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposing the natural-language concept 'the obligation to study Kodashim,' each instantiating a structurally distinct reading of the kodashim_obligation kernel with its own epsilon: study_as_archive (this story, moderate extraction ~0.42, tangled_rope) treats the material as closed historical record with identity-maintenance value only; study_as_performance (near-zero extraction, mountain-adjacent) treats study itself as cosmically efficacious sacrifice-substitute, immune to the opportunity-cost critique because there is no deferred function being substituted for; study_as_preparation (lower extraction than archive, scaffold-leaning) treats the law as live-but-unperformable, justifying resource allocation as maintained readiness for restoration. The three share the same underlying text corpus and institutional setting but diverge sharply on whether present resource allocation is justified by present spiritual function, future restoration, or backward-looking continuity alone — hence three separate epsilon values rather than one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
