% ============================================================================
% CONSTRAINT STORY: constitutional_text_authority__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: constitutional_text_authority__originalist_reading
 *   human_readable: Originalist Reading of Constitutional Textual Authority
 *   domain: constitutional_law/legal_theory/interpretive_jurisprudence
 *
 * SUMMARY:
 *   This constraint represents the 'originalist' reading of constitutional
 *   authority, where the meaning of the Constitution is fixed at the time of
 *   its ratification and derives its authority from the historical public
 *   understanding of its text. This reading imposes a rigid constraint on
 *   judicial discretion, requiring adherence to historical evidence and
 *   making it difficult to recognize unenumerated rights or adapt to
 *   post-ratification social change without formal amendment. The claimed
 *   type is 'tangled_rope' because it offers a coordination function (stable
 *   interpretation) but also involves significant extraction (from judicial
 *   discretion and advocates for evolving rights) and requires active
 *   enforcement through judicial appointments and legal arguments.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text_authority__originalist_reading, 0.65).
domain_priors:suppression_score(constitutional_text_authority__originalist_reading, 0.7).
domain_priors:theater_ratio(constitutional_text_authority__originalist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text_authority__originalist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text_authority__originalist_reading, "Originalist Reading of Constitutional Textual Authority").
narrative_ontology:topic_domain(constitutional_text_authority__originalist_reading, "constitutional_law/legal_theory/interpretive_jurisprudence").

domain_priors:requires_active_enforcement(constitutional_text_authority__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text_authority__originalist_reading, 'a0e7f366-9e93-4e8f-b357-7e7af33f25ae').
narrative_ontology:cs_kernel_codification('a0e7f366-9e93-4e8f-b357-7e7af33f25ae', fixed_text).
narrative_ontology:cs_authority_grounding('a0e7f366-9e93-4e8f-b357-7e7af33f25ae', lineage).
narrative_ontology:cs_interpretation_layer_present('a0e7f366-9e93-4e8f-b357-7e7af33f25ae').
narrative_ontology:cs_reading_relation('a0e7f366-9e93-4e8f-b357-7e7af33f25ae', constitutional_text_authority__living_constitutionalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('a0e7f366-9e93-4e8f-b357-7e7af33f25ae', constitutional_text_authority__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('a0e7f366-9e93-4e8f-b357-7e7af33f25ae', foundational, constitutional_meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(constitutional_meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('a0e7f366-9e93-4e8f-b357-7e7af33f25ae', constitutional_meaning_fixed_at_ratification, conventional).
narrative_ontology:cs_axiom('a0e7f366-9e93-4e8f-b357-7e7af33f25ae', foundational, judicial_role_limited_to_original_intent).
narrative_ontology:cs_axiom_status(judicial_role_limited_to_original_intent, holdable).
narrative_ontology:cs_axiom_grounding('a0e7f366-9e93-4e8f-b357-7e7af33f25ae', judicial_role_limited_to_original_intent, deontological).
narrative_ontology:cs_reference_frame('a0e7f366-9e93-4e8f-b357-7e7af33f25ae', founding_era_public_understanding).
narrative_ontology:cs_drift_state('a0e7f366-9e93-4e8f-b357-7e7af33f25ae', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a0e7f366-9e93-4e8f-b357-7e7af33f25ae', '').
narrative_ontology:cs_kernel_id(constitutional_text_authority__originalist_reading, constitutional_text_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, originalist_legal_scholars).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, conservative_judicial_activists).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, judicial_discretion).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, unenumerated_rights_advocates).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, social_progress_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and propagate the theoretical framework for originalism, influencing judicial appointments and legal education. Their careers and intellectual capital are deeply invested in this interpretive method.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, originalist_legal_scholars, agenda_setter,
    institutional, generational, identity_locked, national).

% Utilize originalism to justify judicial decisions that align with conservative political outcomes, often overturning precedents or limiting legislative power. The framework provides a 'neutral' basis for their activism.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, conservative_judicial_activists, beneficiary,
    institutional, biographical, constrained, national).

% The ability of judges to interpret the Constitution in light of contemporary circumstances is constrained by the originalist mandate to adhere strictly to historical meaning. This limits their flexibility and responsiveness to new legal questions.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, judicial_discretion, payer,
    moderate, immediate, trapped, national).
narrative_ontology:stakeholder_non_agent(constitutional_text_authority__originalist_reading, judicial_discretion).

% Seek to establish or protect rights not explicitly listed in the Constitution (e.g., privacy, reproductive rights). Originalism makes this extremely difficult, requiring them to either find historical 'original intent' or pursue the arduous Article V amendment process.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, unenumerated_rights_advocates, payer,
    organized, generational, constrained, national).

% Advocate for constitutional interpretations that adapt to evolving societal norms and values (e.g., LGBTQ+ rights, racial equality). Originalism often forces them into legislative or amendment battles, rather than judicial recognition of evolving rights.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, social_progress_advocates, payer,
    organized, generational, constrained, national).

% Propose that the Constitution's meaning evolves. They are often marginalized in originalist-dominated legal discourse and judicial appointments, despite offering a coherent alternative interpretive framework.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, living_constitutionalist_scholars, excluded,
    institutional, generational, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a purportedly stable and objective method for interpreting the Constitution, aiming to limit judicial activism and ensure fidelity to the founders' intent, thereby coordinating legal outcomes around a fixed historical baseline.
% TRANSFER_FUNCTION: Transfers interpretive authority from contemporary judicial discretion and evolving societal values to historical evidence and the perceived intent of the framers, effectively limiting the scope of rights and governmental powers to those understood at ratification.
% ABSENT_VOICES: Living constitutionalist scholars and advocates for a 'moral reading' of the Constitution are often excluded from the dominant discourse when originalism holds sway, as their interpretive methods are deemed illegitimate by the originalist framework itself.
% DISAPPEARANCE_RATIONALE: If originalism as a dominant interpretive method vanished, judicial decisions would immediately shift towards more flexible interpretations, potentially recognizing new rights or re-evaluating existing precedents based on contemporary values. The legal landscape would fundamentally reorganize.
% FOUNDING_PROBLEM: The problem of judicial overreach and the desire for a consistent, non-political method of constitutional interpretation that prevents judges from imposing their own policy preferences.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of originalism, including many legal scholars and politicians, attest that judicial activism remains a live problem. Critics, while acknowledging the historical concern, argue that originalism itself can be a form of activism, selectively applying history to achieve desired outcomes; independent legal analysis corroborates the ongoing debate about judicial role.
narrative_ontology:disappearance_verdict(constitutional_text_authority__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text_authority__originalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text_authority__originalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(constitutional_text_authority__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text_authority__originalist_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text_authority__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text_authority__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_text_authority__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because this reading significantly limits the ability of the legal system to adapt to modern challenges, imposing costs on those seeking to expand rights or address new social realities through judicial interpretation. Suppression is also high, as it actively delegitimizes alternative interpretive methods and requires constant defense against 'living constitutionalist' approaches. The theater ratio is moderate, reflecting that while there is genuine intellectual work in historical research, some of the performance serves to mask the political outcomes achieved through this interpretive method. The increasing extractiveness and suppression over time reflect the growing dominance and more aggressive application of originalist principles in recent decades.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of originalist proponents, this constraint is a 'rope' that ensures fidelity to the Constitution and prevents judicial activism. From the perspective of those advocating for evolving rights or judicial flexibility, it operates as a 'snare' that entrenches historical biases and extracts the ability to adapt the law to contemporary needs. The engine's classification as 'tangled_rope' reflects this hybrid nature, acknowledging both the coordination function and the asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist legal scholars and conservative judicial activists are beneficiaries, as this reading provides a powerful framework for their intellectual and political agendas. Judicial discretion, unenumerated rights advocates, and social progress advocates are victims, as their interpretive flexibility and policy goals are directly constrained. Living constitutionalist scholars are excluded, as their interpretive framework is fundamentally at odds with originalism and often marginalized in the discourse.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_meaning_determinacy,
    'Is ''original public meaning'' truly discoverable and determinate, or is historical evidence often ambiguous and subject to contemporary interpretive biases?',
    'Extensive empirical studies of historical legal texts and public discourse, cross-referenced with contemporary interpretive outcomes, to assess the degree of interpretive consensus among historians and legal scholars.',
    'If historical meaning is largely indeterminate, the constraint''s claim to objectivity is weakened, increasing its theater ratio and potentially reclassifying it towards a snare, as its ''coordination'' function becomes a cover for discretionary outcomes. If determinate, its legitimacy as a stable interpretive method is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_meaning_determinacy, empirical, 'The determinacy of original public meaning.').

omega_variable(
    originalism_as_activism,
    'Does originalism genuinely constrain judicial discretion, or does it provide a rhetorical cover for a different form of judicial activism that favors specific political outcomes?',
    'Comparative analysis of judicial decisions under originalist vs. non-originalist frameworks, controlling for judges'' political ideologies, to identify whether originalist methodology consistently leads to outcomes distinct from those predictable by ideology alone.',
    'If originalism is found to be a form of activism, its extractiveness and suppression would be re-evaluated as higher, and its coordination function as more theatrical, potentially shifting its classification towards a snare. If it genuinely constrains, its rope-like qualities are reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalism_as_activism, conceptual, 'Originalism''s role in constraining vs. enabling judicial activism.').

omega_variable(
    legitimacy_of_evolving_meaning,
    'Is the Constitution''s legitimacy derived solely from its original ratification, or does its continued legitimacy depend on its capacity to adapt to evolving societal values and contemporary moral principles?',
    'This is a preference-based question, resolvable only through societal consensus or a foundational shift in legal philosophy, not empirical data. It reflects a fundamental disagreement about the nature of constitutional authority.',
    'If legitimacy requires adaptation, the originalist reading''s suppression of evolving meaning would be seen as a greater cost, increasing its extractiveness. If legitimacy is purely historical, the originalist reading''s costs are seen as necessary for fidelity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_of_evolving_meaning, preference, 'Source of constitutional legitimacy: fixed historical intent vs. evolving societal values.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text_authority__originalist_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t1970, constitutional_text_authority__originalist_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(cons_tr_t1985, constitutional_text_authority__originalist_reading, theater_ratio, 1985, 0.15).
narrative_ontology:measurement(cons_tr_t2000, constitutional_text_authority__originalist_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(cons_tr_t2010, constitutional_text_authority__originalist_reading, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(cons_tr_t2024, constitutional_text_authority__originalist_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(cons_be_t1970, constitutional_text_authority__originalist_reading, base_extractiveness, 1970, 0.4).
narrative_ontology:measurement(cons_be_t1985, constitutional_text_authority__originalist_reading, base_extractiveness, 1985, 0.5).
narrative_ontology:measurement(cons_be_t2000, constitutional_text_authority__originalist_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(cons_be_t2010, constitutional_text_authority__originalist_reading, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(cons_be_t2024, constitutional_text_authority__originalist_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t1970, constitutional_text_authority__originalist_reading, suppression_requirement, 1970, 0.45).
narrative_ontology:measurement(cons_su_t1985, constitutional_text_authority__originalist_reading, suppression_requirement, 1985, 0.55).
narrative_ontology:measurement(cons_su_t2000, constitutional_text_authority__originalist_reading, suppression_requirement, 2000, 0.63).
narrative_ontology:measurement(cons_su_t2010, constitutional_text_authority__originalist_reading, suppression_requirement, 2010, 0.67).
narrative_ontology:measurement(cons_su_t2024, constitutional_text_authority__originalist_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
