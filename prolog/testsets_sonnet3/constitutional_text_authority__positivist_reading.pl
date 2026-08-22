% ============================================================================
% CONSTRAINT STORY: constitutional_text_authority__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text_authority__positivist_reading, []).

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
 *   constraint_id: constitutional_text_authority__positivist_reading
 *   human_readable: Constitutional Validity via Formal Pedigree (Positivist Reading)
 *   domain: legal/constitutional/jurisprudential
 *
 * SUMMARY:
 *   This constraint models legal positivism's account of constitutional
 *   validity: a norm is constitutionally valid because it was enacted through
 *   the correct formal procedure by an institution recognized as having
 *   authority to enact it (Hart's rule of recognition, Kelsen's Grundnorm),
 *   not because its content is morally correct. The law/morality separation
 *   thesis is the defining feature — moral argument is not a valid input to
 *   the validity test itself, however relevant it may be to criticism of the
 *   law from outside legal theory. This is one of three readings of the
 *   constitutional_text_authority kernel; it converges with originalism on
 *   text/procedure-fidelity but diverges sharply on moorings, since
 *   originalism roots authority in historical understanding while positivism
 *   roots it purely in institutional pedigree independent of any content,
 *   historical or moral. It diverges completely from living
 *   constitutionalism, which makes contemporary moral values a direct input
 *   to validity — exactly what positivism excludes by design.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text_authority__positivist_reading, 0.42).
domain_priors:suppression_score(constitutional_text_authority__positivist_reading, 0.38).
domain_priors:theater_ratio(constitutional_text_authority__positivist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text_authority__positivist_reading, rope).
narrative_ontology:human_readable(constitutional_text_authority__positivist_reading, "Constitutional Validity via Formal Pedigree (Positivist Reading)").
narrative_ontology:topic_domain(constitutional_text_authority__positivist_reading, "legal/constitutional/jurisprudential").

domain_priors:requires_active_enforcement(constitutional_text_authority__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text_authority__positivist_reading, 'a6fda32a-9b2f-4593-8899-b1b27c6bcb3e').
narrative_ontology:cs_kernel_codification('a6fda32a-9b2f-4593-8899-b1b27c6bcb3e', formalized).
narrative_ontology:cs_authority_grounding('a6fda32a-9b2f-4593-8899-b1b27c6bcb3e', practice).
narrative_ontology:cs_interpretation_layer_present('a6fda32a-9b2f-4593-8899-b1b27c6bcb3e').
narrative_ontology:cs_reading_relation('a6fda32a-9b2f-4593-8899-b1b27c6bcb3e', constitutional_text_authority__originalist_reading, influences).
narrative_ontology:cs_reading_relation('a6fda32a-9b2f-4593-8899-b1b27c6bcb3e', constitutional_text_authority__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_axiom('a6fda32a-9b2f-4593-8899-b1b27c6bcb3e', foundational, validity_independent_of_moral_content).
narrative_ontology:cs_axiom_status(validity_independent_of_moral_content, holdable).
narrative_ontology:cs_axiom_grounding('a6fda32a-9b2f-4593-8899-b1b27c6bcb3e', validity_independent_of_moral_content, conventional).
narrative_ontology:cs_axiom('a6fda32a-9b2f-4593-8899-b1b27c6bcb3e', foundational, authority_grounded_in_institutional_pedigree_not_history_or_morals).
narrative_ontology:cs_axiom_status(authority_grounded_in_institutional_pedigree_not_history_or_morals, holdable).
narrative_ontology:cs_axiom_grounding('a6fda32a-9b2f-4593-8899-b1b27c6bcb3e', authority_grounded_in_institutional_pedigree_not_history_or_morals, conventional).
narrative_ontology:cs_reference_frame('a6fda32a-9b2f-4593-8899-b1b27c6bcb3e', rule_of_recognition_pedigree_test).
narrative_ontology:cs_drift_state('a6fda32a-9b2f-4593-8899-b1b27c6bcb3e', contemporary_rights_litigation_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('a6fda32a-9b2f-4593-8899-b1b27c6bcb3e', '').
narrative_ontology:cs_kernel_id(constitutional_text_authority__positivist_reading, constitutional_text_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text_authority__positivist_reading, judicial_institutions).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__positivist_reading, legislative_drafters).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__positivist_reading, constitutional_law_faculties).
narrative_ontology:constraint_victim(constitutional_text_authority__positivist_reading, morally_grounded_claimants).
narrative_ontology:constraint_victim(constitutional_text_authority__positivist_reading, unwritten_rights_litigants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Courts applying the positivist reading validate constitutional norms by tracing them to a proper rule of recognition — enactment procedure, institutional pedigree, formal amendment process — rather than assessing whether the norm is morally correct. This gives courts a determinate, defensible test for validity that insulates rulings from being characterized as freelance moral reasoning, and it concentrates interpretive authority in institutions that can certify pedigree.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, judicial_institutions, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text_authority__positivist_reading, judicial_institutions, beneficiary).

% Legislators and constitutional drafters benefit because the positivist reading validates whatever they enact through proper procedure, regardless of its moral content, so long as the pedigree chain (rule of recognition, formal amendment procedure) is intact. Their political power to enact durable, morally controversial arrangements is protected by a validity test that does not second-guess content.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, legislative_drafters, beneficiary,
    institutional, generational, arbitrage, national).

% Legal academics and doctrine-builders who work within the positivist tradition gain a stable, teachable, professionally defensible framework — validity questions become tractable pedigree-tracing exercises rather than open moral contestation, which sustains an entire interpretive methodology and career structure built on the law/morality separation thesis.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, constitutional_law_faculties, beneficiary,
    organized, civilizational, arbitrage, national).

% Litigants who argue that a properly enacted provision is unjust or that an unenacted moral principle should be recognized as constitutionally binding find their claims structurally irrelevant to validity under this reading — no matter how compelling the moral argument, it cannot by itself invalidate a pedigreed rule or validate an unenacted one. They must instead work through formal channels (amendment, legislation) that they may lack the power to use.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, morally_grounded_claimants, payer,
    powerless, biographical, trapped, national).

% Parties seeking recognition of rights not traceable to an enacted source (unenumerated rights claims resting on moral or natural-law argument) bear the cost of the positivist validity test directly: their claims are dismissed as non-legal questions regardless of substantive merit, and their only remedy is the slow, resource-intensive route of formal constitutional amendment or persuading enactors to codify the right.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, unwritten_rights_litigants, payer,
    powerless, biographical, constrained, national).

% Scholars and advocates who hold that law's validity is partly a function of its moral content are structurally excluded from the positivist reading's own validity test — their objection (that separating law from morality produces formally valid but substantively unjust law) is a critique of the framework from outside it, not a move available within it.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, natural_law_theorists, excluded,
    organized, civilizational, analytical, global).

% Comparative jurisprudence scholars trace how the positivist reading interacts with originalist and living-constitutionalist readings across jurisdictions, without themselves being bound by any single reading's validity test.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, constitutional_theory_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text_authority__positivist_reading, diffuse).
narrative_ontology:fixing_cost_class(constitutional_text_authority__positivist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a determinate, procedure-based test for constitutional validity that lets officials, courts, and citizens identify what counts as binding law without requiring convergence on contested moral premises — a genuine solution to the problem of adjudicating validity across a morally pluralistic population.
% TRANSFER_FUNCTION: Moves interpretive authority and the power to settle validity questions toward institutions that can certify formal pedigree (courts, legislatures, drafting bodies) and away from claimants whose arguments rest on moral content unmoored from an enacted source.
% ABSENT_VOICES: Natural law theorists and litigants advancing unenumerated-rights or moral-content arguments are structurally unable to prevail on their own terms within this reading — the reading's own validity test excludes moral content as a criterion, so their objection can only be lodged as an external critique of the framework, never adjudicated within it.
% DISAPPEARANCE_RATIONALE: If the positivist validity test disappeared, courts would lose a determinate criterion for validity and would have to adjudicate constitutional questions by direct appeal to moral content or historical understanding — reshaping doctrine, opening currently foreclosed claims (unenumerated rights, moral invalidation of properly enacted provisions), and destabilizing settled expectations built on pedigree-based validity.
% FOUNDING_PROBLEM: Nineteenth and twentieth century jurisprudence needed a way to identify binding law in pluralistic, secularizing societies without requiring judges to adjudicate deep moral disagreement each time a provision's validity was questioned — the problem of distinguishing law from morality as a matter of legal theory, associated with Austin, Kelsen, and Hart's rule of recognition.
% FOUNDING_PROBLEM_CORROBORATION: Positivist jurists (Hart, Raz, and their judicial successors) attest the problem remains live — pluralistic societies still need a determinate validity criterion. Natural law critics (Fuller, Dworkin, and contemporary human-rights litigators) attest from outside the positivist tradition that the strict separation thesis has hardened into a device that immunizes morally arbitrary but procedurally valid enactments from legal challenge, corroborated by comparative constitutional scholarship documenting cases where formally valid provisions produced substantively unjust outcomes with no internal legal remedy.
narrative_ontology:disappearance_verdict(constitutional_text_authority__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text_authority__positivist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text_authority__positivist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_text_authority__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text_authority__positivist_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text_authority__positivist_reading_tests).
:- end_tests(constitutional_text_authority__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) because the reading does perform genuine coordination work (a determinate validity test avoiding endless moral relitigation) alongside real costs to claimants whose substantive claims are foreclosed purely on procedural grounds. Suppression is moderate (0.38) — the reading does not physically coerce anyone, but it structurally forecloses an entire category of argument (moral-content arguments) from being cognizable within the validity test, which functions as a soft suppression of that argument type within legal discourse. Accessibility collapse is set at the midpoint (0.5): alternative theories (natural law, living constitutionalism) remain fully articulable and academically live, but within legal practice governed by this reading, the alternative modes of argument collapse to irrelevance for validity purposes specifically. Resistance is moderate-high (0.55) reflecting sustained natural-law and critical-legal-studies challenge to the separation thesis across decades.
 *
 * PERSPECTIVAL GAP:
 *   From the judicial/legislative seat, the positivist test looks like principled restraint — a refusal to let judges' personal moral views override democratically enacted procedure, i.e., coordination against arbitrary judicial power. From the morally-grounded claimant's seat, the same test looks like an extraction mechanism that launders substantively unjust but procedurally clean enactments into binding law with no internal remedy. The engine should register this divergence structurally rather than resolve it — that divergence is what the framework exists to detect.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutions with authority to enact and certify pedigree (courts, legislatures, doctrinal faculties) are structural beneficiaries: the test gives their output automatic validity so long as procedure was followed, and it gives courts a defensible, non-moral basis for their rulings. Claimants whose arguments rest on moral content untethered to an enacted source are structural targets: the test is specifically constructed so their strongest argument type cannot bear on the validity question. This is a low-suppression-mechanism but high-structural-foreclosure situation — no one prevents the claimants from speaking, but the framework prevents their speech from mattering to the outcome.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to identify binding law without requiring moral consensus in pluralistic societies) remains genuinely live in some form, but its status is contested precisely because the same test that solves the coordination problem also forecloses substantive remedy for claimants harmed by procedurally valid injustice — the mandatrophy question here is whether the separation thesis has calcified into a shield for legislative and judicial institutions rather than remaining a live solution to a genuine coordination problem. The contested corroboration (Hart/Raz vs. Fuller/Dworkin) is exactly the kind of divergence the six_questions genealogy interview is designed to surface without resolving from inside the positivist reading itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    positivism_originalism_convergence_boundary,
    'Where exactly does positivism''s institutional-pedigree test diverge from originalism''s historical-understanding test, given that both exclude present-day moral content from validity?',
    'Trace hard cases where a provision satisfies formal enactment pedigree but the original public understanding of its meaning is contested or unknowable — positivism would still validate it via pedigree alone, while originalism would treat the validity of its APPLICATION as unresolved pending historical inquiry.',
    'If the boundary collapses in practice (courts using pedigree as a proxy for original understanding), the two readings may not be as structurally distinct as claimed, weakening the case for treating them as separate constraints rather than variants of one reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(positivism_originalism_convergence_boundary, conceptual, 'Whether positivism and originalism are genuinely distinct or converge in judicial practice.').

omega_variable(
    separation_thesis_naturalness,
    'Is the law/morality separation thesis a discovered structural feature of what law IS (a genuine conceptual truth about legal systems), or a constructed methodological choice that happens to benefit institutions whose authority depends on not having their outputs subjected to moral review?',
    'Comparative jurisprudence across legal systems with and without strong separation theses, examining whether systems that reject the separation thesis (e.g., some post-authoritarian transitional constitutions incorporating explicit moral/human-dignity clauses as validity conditions) function coherently as legal systems.',
    'If the separation thesis is a discovered necessity, the positivist reading is closer to a mountain-like structural claim about law''s nature; if it is a constructed methodological choice with identifiable institutional beneficiaries, the reading is better understood as tangled-rope-adjacent — coordination function real, but shaped by who benefits from moral-content exclusion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(separation_thesis_naturalness, conceptual, 'Whether the law/morality separation is conceptually necessary or an institutionally convenient construction.').

omega_variable(
    foreclosure_vs_coexistence_with_living_constitutionalism,
    'Does the positivist reading genuinely FORECLOSE living constitutionalism within a single legal framework, or can a legal system operate with positivist validity tests for MOST provisions while allowing living-constitutionalist interpretation of specific open-textured clauses (e.g., ''equal protection'', ''cruel and unusual'')?',
    'Examine actual constitutional practice in mixed systems (e.g., U.S. constitutional doctrine, which uses positivist-style validity tests for amendment procedure while permitting evolving interpretation of substantive clauses) to see whether the two readings operate in genuinely separate domains or in real tension over the same provisions.',
    'If mixed practice is coherent, the forecloses relation declared in cs_structure may be too strong for real-world legal systems, though it remains correct as a claim about the two readings'' core theoretical premises taken in pure form.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreclosure_vs_coexistence_with_living_constitutionalism, conceptual, 'Whether foreclosure holds only at the level of pure theory or also in mixed constitutional practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text_authority__positivist_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text_authority__positivist_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(cons_tr_t10, constitutional_text_authority__positivist_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(cons_tr_t20, constitutional_text_authority__positivist_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(cons_tr_t30, constitutional_text_authority__positivist_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement(cons_tr_t40, constitutional_text_authority__positivist_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement(cons_tr_t50, constitutional_text_authority__positivist_reading, theater_ratio, 50, 0.27).
narrative_ontology:measurement(cons_tr_t60, constitutional_text_authority__positivist_reading, theater_ratio, 60, 0.28).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text_authority__positivist_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(cons_be_t10, constitutional_text_authority__positivist_reading, base_extractiveness, 10, 0.33).
narrative_ontology:measurement(cons_be_t20, constitutional_text_authority__positivist_reading, base_extractiveness, 20, 0.36).
narrative_ontology:measurement(cons_be_t30, constitutional_text_authority__positivist_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(cons_be_t40, constitutional_text_authority__positivist_reading, base_extractiveness, 40, 0.4).
narrative_ontology:measurement(cons_be_t50, constitutional_text_authority__positivist_reading, base_extractiveness, 50, 0.41).
narrative_ontology:measurement(cons_be_t60, constitutional_text_authority__positivist_reading, base_extractiveness, 60, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text_authority__positivist_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(cons_su_t10, constitutional_text_authority__positivist_reading, suppression_requirement, 10, 0.32).
narrative_ontology:measurement(cons_su_t20, constitutional_text_authority__positivist_reading, suppression_requirement, 20, 0.33).
narrative_ontology:measurement(cons_su_t30, constitutional_text_authority__positivist_reading, suppression_requirement, 30, 0.34).
narrative_ontology:measurement(cons_su_t40, constitutional_text_authority__positivist_reading, suppression_requirement, 40, 0.36).
narrative_ontology:measurement(cons_su_t50, constitutional_text_authority__positivist_reading, suppression_requirement, 50, 0.37).
narrative_ontology:measurement(cons_su_t60, constitutional_text_authority__positivist_reading, suppression_requirement, 60, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text_authority__positivist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_text_authority__positivist_reading, constitutional_text_authority__originalist_reading).
narrative_ontology:affects_constraint(constitutional_text_authority__positivist_reading, constitutional_text_authority__living_constitutionalist_reading).

% DUAL FORMULATION NOTE:
% Three-story family decomposing the colloquial concept of 'constitutional authority' per the eps-invariance principle: positivist_reading (this story — validity from formal pedigree, moral content irrelevant), originalist_reading (validity/meaning from historical public understanding at ratification), living_constitutionalist_reading (validity/meaning from contemporary moral values). All three share the kernel constitutional_text_authority but instantiate structurally distinct constraints with different eps, different beneficiary/victim sets, and different foreclosure relations. Positivism converges partially with originalism (both exclude present-day moral content, both influence each other's legitimacy conditions in mixed practice) but forecloses living constitutionalism at the level of pure theory (contradictory claims about what validity consists in).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
