% ============================================================================
% CONSTRAINT STORY: constitutional_text_authority__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text_authority__originalist_reading, []).

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
 *   constraint_id: constitutional_text_authority__originalist_reading
 *   human_readable: Constitutional Text Authority (Originalist Reading)
 *   domain: constitutional_law/interpretive_jurisprudence
 *
 * SUMMARY:
 *   The originalist reading of constitutional authority grounds legitimacy in
 *   the fixed meaning of the constitutional text as understood by the
 *   ratifying public in 1787-1791. Under this reading, the Constitution's
 *   meaning does not evolve with social attitudes or contemporary moral
 *   principles; instead, post-ratification social change requires formal
 *   amendment via Article V. This constraint operationalizes that framework,
 *   creating rigid doctrinal boundaries on judicial discretion while
 *   simultaneously extracting from those seeking constitutional protection
 *   for unenumerated or evolving rights. The constraint exhibits a
 *   perspectival split: originalist judges and jurisprudential institutions
 *   experience it as pure coordination (rope), enabling predictable,
 *   rule-based doctrine. Unenumerated rights claimants and social movements
 *   experience it as a snare, structurally barring recognition of their
 *   claims. The constraint's extractiveness has accumulated over 20 years
 *   (0.35 → 0.58) as originalist doctrine has consolidated and produced
 *   consequences (Dobbs, Bruen, etc.) that narrow the space of recognized
 *   constitutional liberties. The theater ratio (0.38) is relatively low,
 *   reflecting that originalist methodology is explicit, methodologically
 *   constrained, and not primarily performative — but the slight upward drift
 *   (0.32 → 0.38) reflects growing recognition that historical interpretation
 *   involves discretionary choices that undermine the appearance of
 *   mechanical constraint.
 *
 * KEY AGENTS:
 *   - Originalist Judicial Coalition: Institutional actor with arbitrage exit. Primary beneficiary — controls the interpretive framework and uses it to expand judicial power over regulatory agencies and legislative experimentation.
 *   - Unenumerated Rights Claimants: Powerless, trapped. Primary victims — structurally barred from constitutional protection unless they can demonstrate 18th-century historical pedigree.
 *   - Social Movements Seeking Rights Protection: Moderate, constrained. Secondary victims — can pursue Article V amendment (structurally costly, rarely successful) or accept non-recognition.
 *   - Federal Judiciary (Non-Originalist Justices): Institutional, constrained. Mixed experience — limited by originalist precedent but also benefit from the constraint's clarity and doctrinal stability.
 *   - Constraint-Preservation Institutions: Institutional, arbitrage. Beneficiaries — originalist think tanks and law schools benefit from the constraint's intellectual coherence and funding flows.
 *   - Legislative Bodies: Powerful, constrained. Mixed experience — can anticipate which statutes will survive originalist review, but face invalidation of innovative legislation lacking historical pedigree.
 *   - Analytical Observer: Analytical, universal scope. Risk of false summit — naturalizing a contestable hermeneutic choice as a constraint on meaning itself.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text_authority__originalist_reading, 0.58).
domain_priors:suppression_score(constitutional_text_authority__originalist_reading, 0.52).
domain_priors:theater_ratio(constitutional_text_authority__originalist_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text_authority__originalist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text_authority__originalist_reading, "Constitutional Text Authority (Originalist Reading)").
narrative_ontology:topic_domain(constitutional_text_authority__originalist_reading, "constitutional_law/interpretive_jurisprudence").

domain_priors:requires_active_enforcement(constitutional_text_authority__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text_authority__originalist_reading, '10e39bd9-f044-4509-a55d-e980a822bc41').
narrative_ontology:cs_kernel_codification('10e39bd9-f044-4509-a55d-e980a822bc41', formalized).
narrative_ontology:cs_authority_grounding('10e39bd9-f044-4509-a55d-e980a822bc41', lineage).
narrative_ontology:cs_interpretation_layer_present('10e39bd9-f044-4509-a55d-e980a822bc41').
narrative_ontology:cs_reading_relation('10e39bd9-f044-4509-a55d-e980a822bc41', constitutional_text_authority__living_constitutionalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('10e39bd9-f044-4509-a55d-e980a822bc41', constitutional_text_authority__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('10e39bd9-f044-4509-a55d-e980a822bc41', foundational, original_public_meaning_determines_constitutional_content).
narrative_ontology:cs_axiom_status(original_public_meaning_determines_constitutional_content, holdable).
narrative_ontology:cs_axiom_grounding('10e39bd9-f044-4509-a55d-e980a822bc41', original_public_meaning_determines_constitutional_content, deontological).
narrative_ontology:cs_axiom('10e39bd9-f044-4509-a55d-e980a822bc41', foundational, unenumerated_rights_require_historical_pedigree).
narrative_ontology:cs_axiom_status(unenumerated_rights_require_historical_pedigree, holdable).
narrative_ontology:cs_axiom_grounding('10e39bd9-f044-4509-a55d-e980a822bc41', unenumerated_rights_require_historical_pedigree, deontological).
narrative_ontology:cs_reference_frame('10e39bd9-f044-4509-a55d-e980a822bc41', ratification_era_constitutional_meaning).
narrative_ontology:cs_drift_state('10e39bd9-f044-4509-a55d-e980a822bc41', contemporary_social_change, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('10e39bd9-f044-4509-a55d-e980a822bc41', '2026-02-26T14:32:15Z').
narrative_ontology:cs_kernel_id(constitutional_text_authority__originalist_reading, constitutional_text_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, originalist_judicial_coalition).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, constraint_preservationists).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, unenumerated_rights_claimants).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, post_ratification_social_movements).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNENUMERATED RIGHTS CLAIMANTS (SNARE) — Cannot exit the constraint without abandoning their claim to constitutional protection. The originalist reading forecloses recognizing rights not explicitly enumerated or derivable from 18th-century public understanding. No alternatives exist within the constitutional framework they are appealing to. Maximum extraction: their substantive claims are structurally barred regardless of moral force.
constraint_indexing:constraint_classification(constitutional_text_authority__originalist_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SOCIAL MOVEMENTS SEEKING CONSTITUTIONAL PROTECTION (SNARE) — Must either accept non-recognition of emerging rights claims or pursue Article V amendment (structurally costly, rarely successful). Constrained by constitutional structure; trapped by the rigidity gate. Significant extraction: the originalist reading requires social movements to demonstrate historical pedigree for contemporary claims, imposing epistemic and political barriers that other readings would not.
constraint_indexing:constraint_classification(constitutional_text_authority__originalist_reading, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FEDERAL JUDICIARY — NON-ORIGINALIST JUSTICES (TANGLED ROPE) — Constrained by originalist precedent (stare decisis) and the threat of court-packing or constitutional amendment when they deviate. But also benefit from the constraint's clarity: the originalist framework provides workable rules for statutory interpretation and clear institutional boundaries, even when those boundaries conflict with their policy preferences. Mixed extraction and coordination: the constraint both limits their discretion and gives them clear doctrine to apply.
constraint_indexing:constraint_classification(constitutional_text_authority__originalist_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ORIGINALIST JUDICIAL COALITION (ROPE) — Primary beneficiary. The constraint operationalizes their interpretive theory and stabilizes constitutional doctrine according to originalist principles. They experience the constraint as coordination: fixing meaning at ratification enables predictable, rule-based adjudication. Exit cost is zero — they authored the framework. Effective extraction flows toward them in the form of expanded judicial power to invalidate statutes and limit regulatory discretion.
constraint_indexing:constraint_classification(constitutional_text_authority__originalist_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTRAINT PRESERVATION INSTITUTIONS (ROPE) — Benefit from intellectual coherence and funding flows from originalist supporters. The constraint generates continuous demand for historical and interpretive scholarship; it stabilizes a research program. They experience the constraint as their coordinating mechanism: originalism is their framework. Exit cost is zero — they authored it. Net beneficiary.
constraint_indexing:constraint_classification(constitutional_text_authority__originalist_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: LEGISLATIVE BODIES — SEEKING TO REGULATE NEW DOMAINS (TANGLED ROPE) — Constrained by originalist judicial review that invalidates statutes lacking historical pedigree (e.g., Dobbs overturning Roe, NFIB v. Sebelius striking regulatory authority). But legislatures also benefit from the constraint's clarity: they know which regulatory theories will survive judicial scrutiny and which will not. Mixed: significant extraction (invalidation of legislation) but also genuine coordination function (predictable constitutional law).
constraint_indexing:constraint_classification(constitutional_text_authority__originalist_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ORIGINALIST DOCTRINE AS INSTITUTIONAL RITUAL (PITON) — From a civilizational, analytical perspective, originalism functions as a legitimating ritual for judicial authority. The claim that meaning is 'fixed' and 'discoverable' via historical evidence serves to constrain judicial discretion in principle, but the actual practice of originalism involves significant interpretive choice (selecting among competing historical sources, weighting evidence, resolving ambiguities). The theater ratio reflects this: originalism appears rule-bound (theater low — 0.38) because the methodology is explicit and constrained, but the underlying practice has degrees of freedom that belies the appearance of mechanical constraint. The piton classification emerges from the civilization-scale view: originalism persists as doctrine despite persistent critiques of its historical methodology, because it serves institutional interests (constraining lower courts, enabling conservative outcomes, providing intellectual cover for judicial authority).
constraint_indexing:constraint_classification(constitutional_text_authority__originalist_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER — SEMANTIC CONSTRAINT VIEW (MOUNTAIN) — From a universal analytical perspective, the originalist reading reflects a fundamental constraint on meaning itself: any written text has a meaning fixed at the moment of authorship, and later interpreters cannot change that meaning without rewriting the text. This is a constraint on semantics and interpretation, not merely on constitutional law. However, this mountain perspective risks naturalizing what is actually a contestable hermeneutic choice (that meaning is fixed rather than evolving, that authorial intent rather than reader interpretation determines meaning, that historical public understanding is the relevant semantic fact rather than ratifier intent or textual purpose). The engine will identify this as a false summit candidate.
constraint_indexing:constraint_classification(constitutional_text_authority__originalist_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text_authority__originalist_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(constitutional_text_authority__originalist_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(constitutional_text_authority__originalist_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text_authority__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(constitutional_text_authority__originalist_reading, TR),
    TR >= 0.70.

:- end_tests(constitutional_text_authority__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, with clear upward trajectory. The originalist reading concentrates power in the hands of judges willing to overrule prior precedent and legislatures in the name of 'fixing' constitutional meaning. Extraction increases over time as the originalist coalition consolidates (appointing like-minded judges) and produces consequences that narrow recognized liberties (Dobbs on abortion, Bruen on gun rights, NFIB on regulatory authority). The metric does not reflect raw coercion but structural asymmetry: beneficiaries (originalists) can use the constraint to invalidate legislation; victims (social movements, rights claimants) cannot use it for expansion. Suppression (0.52): Moderate-high. The originalist reading suppresses alternatives — it forecloses living constitutionalism, blocks recognition of unenumerated rights, and requires post-ratification social movements to demonstrate 18th-century historical pedigree. The suppression is not total (legislative change is possible, and a constitutional coalition could shift doctrine), but it is substantial. Theater ratio (0.38): Low-moderate. Originalist methodology is explicit and methodologically rigorous (historical research, textual analysis, originalist canons of interpretation), making it less theatrical than pure policy-based jurisprudence. However, the slight upward drift (0.32 → 0.38) reflects growing recognition that historical interpretation involves discretionary choices — source selection, weighting evidence, resolving ambiguities — that create degrees of freedom beneath the appearance of constraint.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates classic extraction asymmetry. The beneficiary coalition (originalist judges and jurisprudential institutions) experiences the constraint as pure coordination: it provides workable rules, intellectual coherence, and doctrinal stability. They have zero exit cost and control the framework. Victims (unenumerated rights claimants) experience it as pure extraction: their claims are structurally barred regardless of moral force, and they cannot exit without abandoning the constitutional framework. Constrained agents (legislatures, non-originalist judges) experience tangled rope: the constraint does coordinate doctrine and make outcomes predictable, but it also extracts through invalidation of legislation and limits on judicial discretion. The piton perspective (viewing originalism as degraded institutional ritual) emerges from the observation that while originalist methodology appears rigorous and constraining, its actual practice involves significant interpretive choice, and it persists through institutional inertia rather than because its historical methodology has been vindicated by time.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (originalist coalition): institutional power + arbitrage exit → d ≈ 0.15 → f(d) ≈ -0.01 → negative/minimal effective extraction. They author the framework and experience it as coordination. Victims (unenumerated rights claimants): powerless + trapped → d ≈ 0.95 → f(d) ≈ 1.42 → maximum experienced extraction. Constrained agents (non-originalist judges, legislatures): institutional/powerful + constrained → d ≈ 0.50-0.55 → f(d) ≈ 0.65-0.75 → moderate experienced extraction. The perspectival gap reflects these structural positions: beneficiaries perceive rope (coordination), victims perceive snare (pure extraction), constrained agents perceive tangled rope (mixed).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through the kernel reading structure. The originalist reading is ONE of three competing readings of the contested constitutional authority kernel. Each reading is coherent and produces different structural consequences. The originalist reading constrains outcomes through historical evidence gates; the living constitutionalist reading enables outcome flexibility through contemporary moral principles; the positivist reading brackets the moral content and emphasizes formal procedures. No single reading is 'correct' — the mandatrophy is managed by recognizing that constitutional authority is fundamentally contested across multiple readings, each with structural validity within its own framework. The orignal reading's false summit risk (Perspective 8) is the danger that the semantic claim about fixed meaning naturalizes what is actually a jurisprudential choice about how to interpret contested texts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_public_meaning_identification,
    'What constitutes ''original public understanding'' and how do we distinguish it from ratifier intent, drafter intent, or textual purpose?',
    'Historical methodology comparison: constitutional scholars'' competing reconstructions of 18th-century public understanding; divergence analysis of outcomes when using ratifier vs drafter vs public understanding interpretive standards',
    'If ''original public meaning'' is indeterminate (multiple coherent reconstructions exist): the constraint is less rigid than claimed, and more extractive (outcomes depend on which reconstruction is chosen). If determinate and stable: the constraint is genuinely coordinating (provides clear, rule-based doctrine).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_public_meaning_identification, empirical, 'Determinacy of original public meaning across constitutional clauses').

omega_variable(
    historical_evidence_gaps,
    'For ambiguous constitutional clauses (e.g., ''cruel and unusual punishment''), what happens when historical evidence is sparse or contradictory?',
    'Cataloging originalist decisions in low-evidence contexts; correlation between evidence quality and outcome divergence among originalist judges; frequency of disagreement when historical record is silent or mixed',
    'If originalists diverge significantly in low-evidence contexts: the constraint is extractive — the appearance of determinacy masks discretionary choice. If originalists converge: the constraint is genuinely coordinating.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_evidence_gaps, empirical, 'Convergence of originalist interpretation in ambiguous historical contexts').

omega_variable(
    constraint_reading_distinction,
    'Is the originalist reading a constraint on constitutional MEANING (how we interpret the text) or a constraint on constitutional POWER (how the judiciary can exercise authority)?',
    'Logical analysis: does originalism''s force derive from a claim about what the Constitution means (semantic claim) or a claim about what judges are permitted to do (institutional claim)? Case law analysis: when originalists win, is it because the text had a fixed meaning, or because they convinced colleagues that judges should not invent new rights?',
    'If semantic: this is a true constraint on interpretation with universal scope. If institutional: this is a constraint on judicial behavior that could be overridden by a legislature or constitutional amendment changing the power structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constraint_reading_distinction, conceptual, 'Whether originalism is fundamentally a semantic or institutional constraint').

omega_variable(
    sibling_reading_distinctness,
    'Are the originalist reading and the living constitutionalist reading genuinely distinct frameworks, or do they differ only in emphasis and speed of adaptation?',
    'Identifying cases where the two frameworks produce different outcomes; testing whether a living constitutionalist could accept originalist methodology while applying it to contemporary contexts; comparing the actual doctrinal trajectories of originalists and living constitutionalists across decades',
    'If genuinely distinct: the readings foreclose each other (true contradiction). If largely overlapping: the readings coexist with different implementations — the coexists_with relation is correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_distinctness, conceptual, 'Distinctness of originalist vs living constitutionalist readings').

omega_variable(
    interpretive_methodology_transparency,
    'Does originalist interpretation actually produce more constrained, predictable, less discretionary outcomes than other methodologies, or does it merely hide discretionary choices behind historical methodology?',
    'Systematic analysis of originalist opinion outcomes: frequency of unexpected results, correlation with judge ideology, comparison of outcome variance to non-originalist methodologies, auditing of historical evidence selection (do originalists cite sources they could have cited differently?)',
    'If originalism truly constrains outcomes: the theater ratio should be low (genuine constraint). If originalism masks discretion: the theater ratio rises (performative constraint) and the extractiveness becomes purely institutional.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interpretive_methodology_transparency, empirical, 'Whether originalist methodology actually constrains judicial outcomes or masks discretion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text_authority__originalist_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(const_orig_theater_t0, constitutional_text_authority__originalist_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(const_orig_theater_t10, constitutional_text_authority__originalist_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(const_orig_theater_t20, constitutional_text_authority__originalist_reading, theater_ratio, 20, 0.38).

% Extraction over time
narrative_ontology:measurement(const_orig_extract_t0, constitutional_text_authority__originalist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(const_orig_extract_t10, constitutional_text_authority__originalist_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(const_orig_extract_t20, constitutional_text_authority__originalist_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(const_orig_suppress_t0, constitutional_text_authority__originalist_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(const_orig_suppress_t10, constitutional_text_authority__originalist_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(const_orig_suppress_t20, constitutional_text_authority__originalist_reading, suppression_requirement, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text_authority__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, constitutional_text_authority__living_constitutionalist_reading).
narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, constitutional_text_authority__positivist_reading).

% DUAL FORMULATION NOTE:
% The originalist reading is one constraint among three sibling readings of the same contested kernel. The three readings have different ε values, different beneficiary/victim structures, and different classifications. This story instantiates ONLY the originalist reading and treats it as a coherent, ε-invariant constraint (ε=0.58, Tangled Rope). The living constitutionalist reading (lower extractiveness, more flexible coordination) and positivist reading (emphasis on procedures rather than outcomes) are structurally distinct and require separate stories. All three are linked via network.affects_constraints to show the family relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
