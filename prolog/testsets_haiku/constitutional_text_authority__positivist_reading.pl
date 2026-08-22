% ============================================================================
% CONSTRAINT STORY: constitutional_text_authority__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Constitutional Authority via Procedural Positivism
 *   domain: constitutional_law/legal_theory
 *
 * SUMMARY:
 *   The positivist reading of constitutional authority asserts that the
 *   Constitution's validity derives from its formal enactment via the
 *   procedures specified in Article V and from its status as foundational
 *   law, not from its moral content or alignment with natural law. Under this
 *   reading, courts apply constitutional rules because they are validly
 *   enacted law, not because they are just. This maintains a sharp
 *   distinction between law and morality: an unjust rule can still be valid
 *   constitutional law if properly enacted. The reading benefits
 *   institutional judges (whose authority is grounded in formal position) and
 *   textualist scholars (whose methodology is centered on statutory language
 *   analysis) by decoupling validity from moral argument. It excludes natural
 *   law theorists from the core validity conversation, though they remain
 *   able to argue moral concerns about constitutional rules. The claim/metric
 *   gap is intentional: positivism is CLAIMED as pure rope (genuine
 *   coordination function: a decision procedure that allows diverse moral
 *   communities to operate under shared validity criteria) while the
 *   measurements capture modest extractive effects (institutional actors gain
 *   interpretive authority, natural law voices lose standing in validity
 *   discourse). The engine computes this divergence; do not reconcile.
 *
 * KEY AGENTS:
 *   - Institutional judiciary: sets the validity procedure via formal interpretation; beneficiary of the authority grounding; enforces the law/morality distinction through decisional practice
 *   - Textualist legal scholars: benefit intellectually from text-fidelity framework; advance careers within positivist methodology; mobile within legal academia
 *   - Natural law theorists: pay the cost of exclusion from validity arguments; must either adopt positivist framework or argue from within it; institutional power but limited standing in positivist discourse
 *   - Moral philosophers: excluded from validity conversation but present in broader constitutional discourse; their objections are structural, not suppressive
 *   - Originalist judges: neighboring framework with partial overlap; differ on authority grounding (historical public meaning vs. formal procedure); compute divergent directionality
 *   - Living constitutionalist judges: competing reading that reintroduces morality as validity condition; contestation is direct on the core axiom
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text_authority__positivist_reading, 0.38).
domain_priors:suppression_score(constitutional_text_authority__positivist_reading, 0.22).
domain_priors:theater_ratio(constitutional_text_authority__positivist_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text_authority__positivist_reading, rope).
narrative_ontology:human_readable(constitutional_text_authority__positivist_reading, "Constitutional Authority via Procedural Positivism").
narrative_ontology:topic_domain(constitutional_text_authority__positivist_reading, "constitutional_law/legal_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text_authority__positivist_reading, '94089a86-7287-4a17-804c-951a5d87acdc').
narrative_ontology:cs_kernel_codification('94089a86-7287-4a17-804c-951a5d87acdc', fixed_text).
narrative_ontology:cs_authority_grounding('94089a86-7287-4a17-804c-951a5d87acdc', extraction).
narrative_ontology:cs_interpretation_layer_present('94089a86-7287-4a17-804c-951a5d87acdc').
narrative_ontology:cs_reading_relation('94089a86-7287-4a17-804c-951a5d87acdc', constitutional_text_authority__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('94089a86-7287-4a17-804c-951a5d87acdc', constitutional_text_authority__living_constitutionalist_reading, coexists_with).
narrative_ontology:cs_axiom('94089a86-7287-4a17-804c-951a5d87acdc', foundational, formal_enactment_determines_validity).
narrative_ontology:cs_axiom_status(formal_enactment_determines_validity, holdable).
narrative_ontology:cs_axiom_grounding('94089a86-7287-4a17-804c-951a5d87acdc', formal_enactment_determines_validity, conventional).
narrative_ontology:cs_axiom('94089a86-7287-4a17-804c-951a5d87acdc', foundational, law_morality_logical_distinction).
narrative_ontology:cs_axiom_status(law_morality_logical_distinction, holdable).
narrative_ontology:cs_axiom_grounding('94089a86-7287-4a17-804c-951a5d87acdc', law_morality_logical_distinction, deontological).
narrative_ontology:cs_reference_frame('94089a86-7287-4a17-804c-951a5d87acdc', proceduralist_legal_positivism).
narrative_ontology:cs_drift_state('94089a86-7287-4a17-804c-951a5d87acdc', contemporary_legal_realism_challenge, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('94089a86-7287-4a17-804c-951a5d87acdc', '').
narrative_ontology:cs_kernel_id(constitutional_text_authority__positivist_reading, constitutional_text_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text_authority__positivist_reading, institutional_judiciary).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__positivist_reading, textualist_legal_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(constitutional_text_authority__positivist_reading, natural_law_theorists).
narrative_ontology:constraint_vindicates(constitutional_text_authority__positivist_reading, law_morality_distinction).
narrative_ontology:constraint_vindicates(constitutional_text_authority__positivist_reading, formal_enactment_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and applies the Constitution via formal adjudication. Under the positivist reading, their authority derives from their institutional position as the authorized interpreter of enacted law, not from their moral reasoning or policy preferences. They enforce the law/morality distinction by excluding moral argument from the validity condition and grounding decisions in procedural source rules.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, institutional_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Benefit intellectually and professionally from the positivist reading's validation of textual analysis as a primary method. The reading privileges language-focused jurisprudence and makes moral philosophy contributions optional to legal argument. Their careers and publications advance within the positivist framework.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, textualist_legal_scholars, beneficiary,
    institutional, generational, mobile, national).

% Bear the intellectual cost of exclusion from validity arguments under positivism. Their core claim—that constitutional meaning is grounded in natural law or inherent moral principles—is ruled out as irrelevant to the question 'Is this law valid?' They must either adopt the positivist framework or argue from within it to recover natural law content.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, natural_law_theorists, payer,
    institutional, generational, mobile, national).

% Have standing in broader constitutional discourse but are structurally excluded from the positivist validity conversation. They can argue that a rule is unjust, but not that the injustice makes it invalid as constitutional law. Their exclusion is not coercive (they have alternatives in other frameworks) but is a structural boundary of the positivist reading.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, moral_philosophers, excluded,
    institutional, generational, mobile, national).

% Occupy a neighboring interpretive position with partial structural overlap to positivism (both text-centric) but grounded in a different authority source (historical public meaning rather than formal procedure). They compute divergent directionality depending on whether natural law foundations matter to the constitutional question.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, originalist_judges, observer,
    institutional, generational, analytical, national).

% Hold a competing reading that explicitly reintroduces moral principle as a validity condition, evolving with contemporary values. They see positivism as artificially constraining the Constitution's interpretive domain and argue the reading fails to capture how constitutional authority actually functions in practice.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, living_constitutionalist_judges, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text_authority__positivist_reading, institutional_judiciary).
narrative_ontology:fixing_cost_class(constitutional_text_authority__positivist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a decision procedure for constitutional validity that is decoupled from moral argument, enabling courts and legal actors to apply the law without resolving contested philosophical questions. The formal/procedural grounding allows diverse constitutional communities to operate under shared validity criteria despite disagreeing on underlying moral foundations.
% TRANSFER_FUNCTION: Moves interpretive authority away from natural law theorists and moral philosophers toward institutional actors (courts, legislature) operating through formal procedures. Moral argument becomes advisory rather than constitutive of validity; those whose authority derives from moral reasoning lose standing in the core validity question.
% ABSENT_VOICES: Natural law theorists and moral philosophers who argue constitutional meaning derives from pre-legal moral principles are structurally excluded from the positivist validity conversation. They can object that the reading artificially severs law from morality, but positivism defines them out of the question at the outset. Legislative minorities and historical actors who believed constitutional authority grounded in natural right are also excluded: their moral convictions, even if widespread, do not generate validity under formal procedure rules.
% DISAPPEARANCE_RATIONALE: If the positivist reading vanished and courts returned to mixed frameworks (text + morality + history), constitutional interpretation would reorganize around those mixed criteria; the change would be institutional, not world-rearranging. Some argue (living constitutionalists) that disappearance would restore what the Constitution actually requires; others (originalists) that it would introduce illegitimate moral reasoning; still others maintain the legal system already operates as a hybrid and the positivist claim never fully governed in practice. The disappearance would be more like a framework shift within jurisprudence than a collapse of material arrangements.
% FOUNDING_PROBLEM: Constitutional interpretation requires a mechanism to distinguish valid constitutional law from invalid or non-law. Early positivists posed the problem as: 'How can we ground constitutional validity on formal enactment and institutional source without collapsing into moral relativism or natural law?' The procedure-based answer was: validity derives from the pedigree of the rule (enacted via Article V or recognized as foundational law), not from its content.
% FOUNDING_PROBLEM_CORROBORATION: Legal positivist scholars (Hart, Raz, Green) and institutionalist judges consistently attest that the problem remains live: courts must apply law that participants may regard as unjust, and distinguishing 'valid law' from 'morally good law' remains a pressing interpretive problem. Living constitutionalists and natural law theorists dispute that this is the right problem to solve; they attest instead that the founding problem was misframed by excluding morality. Independent jurisprudential analysis confirms the problem is contested, not answered: no consensus exists on whether formal procedure alone suffices for constitutional validity.
narrative_ontology:disappearance_verdict(constitutional_text_authority__positivist_reading, contested).
narrative_ontology:founding_problem_status(constitutional_text_authority__positivist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text_authority__positivist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_text_authority__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text_authority__positivist_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate (0.38 endpoint) because the positivist reading does concentrate interpretive authority in institutions and excludes certain voices from the validity conversation, but this concentration is justified by a genuine coordination function (enabling legal actors with diverse moral views to apply law under shared criteria). The extraction is not suppressive in the classical sense—natural law theorists retain their institutional positions and can argue morality within a separate domain—but it is asymmetric in that institutional judges gain authority while natural law frameworks lose relevance to validity questions. Suppression is low (0.22) because the reading is sustained through formal institutional practice and intellectual argument, not through coercion; resistance is high (0.71) because living constitutionalists and natural law theorists actively contest the law/morality distinction and offer competing readings with substantial scholarly support. Theater ratio is low (0.18) because the reading's core function (providing a decision procedure decoupled from moral contest) is genuinely performed; the modest theater increase over the interval reflects growing emphasis on formalism as a rhetorical move to defend textualism against policy-motivated interpretation, but the underlying function persists. Accessibility collapse is moderate (0.65) because the positivist framework is intellectually accessible—the law/morality distinction is conceptually simple—but alternatives (originalism, living constitutionalism) remain available and intellectually sophisticated, so the positivist reading does not entirely collapse access to other interpretive paths.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional judiciary's position, the positivist reading is a genuine coordination function that allows them to adjudicate without resolving contested moral questions, which is both intellectually honest and institutionally necessary. From the natural law theorist's position, the same reading is extractive because it defines their core concern (the moral foundation of law) as irrelevant to validity, which loses them standing in the most consequential constitutional conversation. The engine computes this divergence from the structural data: institutional power + access to validity procedures (judiciary directionality near beneficiary) vs. institutional power + exclusion from validity procedures (natural law theorists near symmetric or slight-target). The reading itself does not adjudicate which perspective is correct; it structures the asymmetry. Originalist judges occupy a third position: they can accept the formal-procedure authority grounding positivism endorses while rejecting positivism's indifference to natural law, computing a different directionality based on whether natural law moorings matter to the constitutional question.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional judiciary: high institutional power + access to validity procedures + control over how the law/morality distinction is enforced in practice = directionality near beneficiary (d ≈ 0.2). They collect interpretive authority and can exclude moral argument from validity discourse. Textualist scholars: institutional power + methodological alignment with positivism + career advancement within the framework = mild beneficiary position (d ≈ 0.25). Natural law theorists: institutional power + mobile exit (can publish in non-positivist venues, argue for constitutional amendment, build alternative frameworks) but loss of standing in core validity conversation = symmetric to slight-target (d ≈ 0.45–0.55). The key asymmetry is not external suppression but structural exclusion from the validity domain. Moral philosophers: institutional power + complete exclusion from validity conversation + mobile exit (they have disciplinary homes outside law) = analytical observer position (d ≈ 0.5, they measure the constraint but are not contained by it). Originalist and living constitutionalist judges: institutional power + competing frameworks with real resources and adherents + active contestation = observer positions with residual measurement interest (they compute different directionalities in their own readings).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (distinguishing valid constitutional law from invalid or non-law in a way that works across moral disagreement) remains live, but the positivist answer to it is actively contested by living constitutionalists, who argue the problem was misdefined by excluding morality from validity in the first place. The reading sustains itself through institutional practice and scholarly work, not through claims about what the founding problem requires. This is a case of stable structural disagreement, not mandatrophy: the positivist reading has not outlived its founding problem, but the question of whether it correctly solves the problem is what drives the ongoing contest with sibling readings. The measurement series show extractiveness plateauing at the endpoint (0.38 by time 40, stable at 50), indicating the reading has reached a stable state where institutional authority is consolidated and natural law theorists have adapted to operating under positivist constraints in public legal discourse (though privately maintaining their alternative frameworks). Theater ratio remains low and stable, suggesting the positivism-vs-morality distinction is genuinely performed rather than theatrically maintained.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    law_morality_distinction_coherence,
    'Is the law/morality distinction sustainable within constitutional interpretation, or does constitutional practice inevitably reintroduce moral reasoning despite positivist claims to exclude it?',
    'Empirical analysis of published constitutional opinions coded for presence/absence of moral argument in validity reasoning; meta-analysis of judicial citations and precedent patterns to detect whether ''validity'' determinations correlate with moral reasoning even when explicitly denied.',
    'If moral argument is systematically present in validity determinations despite positivist claims to exclude it, the distinction is performative rather than structural, shifting the reading toward theater and reducing extractiveness. If the distinction holds in practice, positivism''s claim to a pure procedure-based validity is sustained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(law_morality_distinction_coherence, empirical, 'Whether courts actually exclude moral reasoning from constitutional validity or whether the distinction is maintained theatrically.').

omega_variable(
    institutional_authority_vs_moral_grounding,
    'Does the institutional judiciary''s authority to interpret the Constitution derive solely from its formal institutional position (positivist claim) or does it also depend on perceived legitimacy grounded in moral principles of justice and representation (alternative reading)?',
    'Comparative legal analysis of public confidence in constitutional courts across jurisdictions; polling data on whether citizens view courts as legitimate because they are properly appointed (institutional) or because they reach morally just outcomes (moral); historical analysis of court legitimacy crises and their relationship to institutional procedure vs. moral judgment.',
    'If institutional position alone confers legitimacy independent of moral reasoning, positivism''s grounding of judicial authority is vindicated. If legitimacy depends on perceived alignment with moral principles, the reading is partially undermined—institutional authority rides on moral credibility, not vice versa.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_authority_vs_moral_grounding, empirical, 'Whether institutional authority is independent of moral legitimacy or depends on it.').

omega_variable(
    natural_law_axiom_overriding,
    'Has the natural law axiom (that constitutional meaning derives from or is constrained by pre-legal moral principles) been formally overridden within positivism''s own tradition, or is it simply excluded from the validity question?',
    'Genealogical analysis of positivist jurisprudence to detect whether the axiom has been explicitly rejected as false (overridden) or merely defined out of scope (excluded from validity, but possibly true in the moral domain). Distinction turns on whether positivist theorists argue natural law is incoherent or merely irrelevant to law.',
    'If overridden: the natural law reading has been defeated on its own terms; if excluded: the readings coexist in different domains. Overriding suggests the axiom is holdable but false; exclusion suggests it is holdable but orthogonal to validity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_axiom_overriding, conceptual, 'Whether natural law axioms are overridden within positivist tradition or merely excluded from validity scope.').

omega_variable(
    reading_relations_foreclosure_test,
    'Does the positivist core axiom (formal enactment and institutional source determine validity) logically foreclose the originalist axiom (historical public meaning determines constitutional content), or do they coexist as distinct answers to different questions?',
    'Logical analysis of the two axioms to detect contradiction: does accepting formal procedure + institutional authority require denying historical public meaning was determinative? Empirical test: can a judge consistently hold both positivist authority grounding and originalist methodology in actual constitutional reasoning?',
    'If foreclosing: positivism should be marked as forecloses originalism in reading_relations (rare case). If coexisting: the current coexists_with marking is correct. The answer turns on whether procedure and meaning are independent axes or the same question.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_relations_foreclosure_test, conceptual, 'Whether positivist and originalist readings logically foreclose each other or coexist on distinct axes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text_authority__positivist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text_authority__positivist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(cons_tr_t0, observed).
narrative_ontology:measurement(cons_tr_t10, constitutional_text_authority__positivist_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement_basis(cons_tr_t10, observed).
narrative_ontology:measurement(cons_tr_t20, constitutional_text_authority__positivist_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement_basis(cons_tr_t20, observed).
narrative_ontology:measurement(cons_tr_t30, constitutional_text_authority__positivist_reading, theater_ratio, 30, 0.17).
narrative_ontology:measurement_basis(cons_tr_t30, observed).
narrative_ontology:measurement(cons_tr_t40, constitutional_text_authority__positivist_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement_basis(cons_tr_t40, observed).
narrative_ontology:measurement(cons_tr_t50, constitutional_text_authority__positivist_reading, theater_ratio, 50, 0.18).
narrative_ontology:measurement_basis(cons_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text_authority__positivist_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(cons_be_t0, observed).
narrative_ontology:measurement(cons_be_t10, constitutional_text_authority__positivist_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement_basis(cons_be_t10, observed).
narrative_ontology:measurement(cons_be_t20, constitutional_text_authority__positivist_reading, base_extractiveness, 20, 0.36).
narrative_ontology:measurement_basis(cons_be_t20, observed).
narrative_ontology:measurement(cons_be_t30, constitutional_text_authority__positivist_reading, base_extractiveness, 30, 0.37).
narrative_ontology:measurement_basis(cons_be_t30, observed).
narrative_ontology:measurement(cons_be_t40, constitutional_text_authority__positivist_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement_basis(cons_be_t40, observed).
narrative_ontology:measurement(cons_be_t50, constitutional_text_authority__positivist_reading, base_extractiveness, 50, 0.38).
narrative_ontology:measurement_basis(cons_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text_authority__positivist_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement_basis(cons_su_t0, observed).
narrative_ontology:measurement(cons_su_t10, constitutional_text_authority__positivist_reading, suppression_requirement, 10, 0.15).
narrative_ontology:measurement_basis(cons_su_t10, observed).
narrative_ontology:measurement(cons_su_t20, constitutional_text_authority__positivist_reading, suppression_requirement, 20, 0.18).
narrative_ontology:measurement_basis(cons_su_t20, observed).
narrative_ontology:measurement(cons_su_t30, constitutional_text_authority__positivist_reading, suppression_requirement, 30, 0.2).
narrative_ontology:measurement_basis(cons_su_t30, observed).
narrative_ontology:measurement(cons_su_t40, constitutional_text_authority__positivist_reading, suppression_requirement, 40, 0.21).
narrative_ontology:measurement_basis(cons_su_t40, observed).
narrative_ontology:measurement(cons_su_t50, constitutional_text_authority__positivist_reading, suppression_requirement, 50, 0.22).
narrative_ontology:measurement_basis(cons_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text_authority__positivist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_text_authority__positivist_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_text_authority__positivist_reading, constitutional_text_authority__originalist_reading).
narrative_ontology:affects_constraint(constitutional_text_authority__positivist_reading, constitutional_text_authority__living_constitutionalist_reading).

% DUAL FORMULATION NOTE:
% The kernel constitutional_text_authority instantiates as three separate constraint stories, one per reading: positivist (this story, authority via formal procedure), originalist (authority via historical public meaning), and living constitutionalist (authority via contemporary moral evolution). Each reading yields different beneficiaries, victims, and directionality structures. The positivist reading benefits institutional judiciary and textualist scholars; the originalist reading benefits historically-minded scholars and judges constrained to original public meaning; the living constitutionalist reading benefits moral philosophers and judges who value evolutionary interpretation. The three stories share a kernel (the Constitution itself as supreme law) but instantiate different structural relationships to that kernel's interpretation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
