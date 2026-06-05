% ============================================================================
% CONSTRAINT STORY: us_constitution_1787__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_1787__positivist_reading, []).

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
 *   constraint_id: us_constitution_1787__positivist_reading
 *   human_readable: Constitutional Positivism: Text + Amendment as Democratic Constraint on Judicial Interpretation
 *   domain: constitutional_law/legal_theory/political_philosophy
 *
 * SUMMARY:
 *   The positivist reading of the U.S. Constitution grounds constitutional
 *   meaning in two commitments: (1) the written text means what it says at
 *   the time of utterance — semantic content is fixed by the text's language
 *   as understood by educated contemporaries — and (2) constitutional change
 *   comes exclusively through democratic amendment (Article V), not through
 *   judicial reinterpretation. This reading constrains judges to interpret
 *   within textual bounds and delegates fundamental constitutional
 *   modification to the democratic amendment process. The constraint exhibits
 *   hybrid coordination and extraction: it coordinates legitimate boundaries
 *   on judicial power (preventing unilateral judicial rewriting) but
 *   simultaneously extracts by foreclosing adaptive interpretation and
 *   raising amendment barriers to levels that prevent democratic will from
 *   updating constitutional meaning even when broad consensus exists. The
 *   constraint's extractiveness has risen measurably from 1787 to the present
 *   (0.28 → 0.48) as constitutional text and social reality have diverged and
 *   the amendment process has proven increasingly difficult. Theater ratio
 *   (0.55) reflects that the originalist methodology for discovering
 *   'original public meaning' operates as both a real constraint and a
 *   performative artifact — historical sources are reconstructed in ways that
 *   correlate with judges' policy preferences.
 *
 * KEY AGENTS:
 *   - Lexically Bound Interpretation Community (beneficiary/institutional): Judges, law professors, jurisprudential traditions anchored in textualism and originalism. These agents benefit from the constraint's enforcement through doctrinal stability, institutional authority, and career advancement pathways aligned with conservative methodology.
 *   - Democratic Amendment Process (beneficiary/organized): The constitutional amendment mechanism itself. The constraint benefits this agent by establishing it as the legitimate pathway for constitutional change, preserving its monopoly on fundamental constitutional modification.
 *   - Adaptive Judicial Interpretation (victim/powerful): Judges and justices who believe the Constitution must adapt to changing circumstances and new moral understanding. Victims of the constraint because they are foreclosed from following evolving interpretation without facing charges of activism.
 *   - Unamendable Constitutional Drift (victim/powerless): The abstract phenomenon of constitutional meaning becoming detached from social reality. As the written text's language becomes antiquated or inadequate, the gap between constitutional law and contemporary understanding widens. The constraint prevents judicial bridge-building, leaving drift to accumulate.
 *   - Originalist Scholarly Establishment (beneficiary/institutional): Academic infrastructure interpreting constitutional history (Founding-era dictionaries, ratification records, historical context). The constraint's enforcement generates demand for their expertise.
 *   - Analytical Observer (neutral/analytical): Position capable of recognizing all perspectives as legitimate readings of the same constitutional text, but also capable of identifying the constraint as a false summit (natural law presentation masking institutional choice).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_1787__positivist_reading, 0.42).
domain_priors:suppression_score(us_constitution_1787__positivist_reading, 0.48).
domain_priors:theater_ratio(us_constitution_1787__positivist_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_1787__positivist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_1787__positivist_reading, "Constitutional Positivism: Text + Amendment as Democratic Constraint on Judicial Interpretation").
narrative_ontology:topic_domain(us_constitution_1787__positivist_reading, "constitutional_law/legal_theory/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_1787__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_1787__positivist_reading, '3035ddda-6617-43b4-acf6-a52b57a8d144').
narrative_ontology:cs_kernel_codification('3035ddda-6617-43b4-acf6-a52b57a8d144', formalized).
narrative_ontology:cs_authority_grounding('3035ddda-6617-43b4-acf6-a52b57a8d144', extraction).
narrative_ontology:cs_interpretation_layer_present('3035ddda-6617-43b4-acf6-a52b57a8d144').
narrative_ontology:cs_reading_relation('3035ddda-6617-43b4-acf6-a52b57a8d144', us_constitution_1787__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('3035ddda-6617-43b4-acf6-a52b57a8d144', us_constitution_1787__living_reading, coexists_with).
narrative_ontology:cs_axiom('3035ddda-6617-43b4-acf6-a52b57a8d144', foundational, constitutional_meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(constitutional_meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('3035ddda-6617-43b4-acf6-a52b57a8d144', constitutional_meaning_fixed_at_ratification, deontological).
narrative_ontology:cs_axiom('3035ddda-6617-43b4-acf6-a52b57a8d144', foundational, amendment_monopoly_on_constitutional_change).
narrative_ontology:cs_axiom_status(amendment_monopoly_on_constitutional_change, holdable).
narrative_ontology:cs_axiom_grounding('3035ddda-6617-43b4-acf6-a52b57a8d144', amendment_monopoly_on_constitutional_change, deontological).
narrative_ontology:cs_reference_frame('3035ddda-6617-43b4-acf6-a52b57a8d144', semantic_positivism_fixed_ratification).
narrative_ontology:cs_drift_state('3035ddda-6617-43b4-acf6-a52b57a8d144', contemporary_2026, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3035ddda-6617-43b4-acf6-a52b57a8d144', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(us_constitution_1787__positivist_reading, us_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, lexically_bound_interpretation_community).
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, democratic_amendment_process).
narrative_ontology:constraint_victim(us_constitution_1787__positivist_reading, adaptive_judicial_interpretation).
narrative_ontology:constraint_victim(us_constitution_1787__positivist_reading, unamendable_constitutional_drift).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISENFRANCHISED INTERPRETER (SNARE) — A judge, scholar, or justice who perceives the Constitution's meaning changing over time but is trapped by the text-binding constraint from articulating that change without triggering charges of judicial activism. Experiences pure extraction: foreclosed from following evolving moral understanding, forced to rationalize within textual bounds, bearing reputational cost of textual incoherence. No exit — the constraint's authority is total.
constraint_indexing:constraint_classification(us_constitution_1787__positivist_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DEMOCRATIC AMENDMENT COALITION (TANGLED ROPE) — Organized agents (legislatures, social movements, constitutional conventions) experience mixed coordination and extraction. The constraint coordinates legitimate democratic amendment as the mechanism for constitutional change — it prevents unilateral judicial rewriting. But it also extracts: the amendment supermajority barrier (Article V) is extremely high, requiring 2/3 of both houses plus 3/4 of states. This means constitutional adaptation requires extraordinary consensus, often impossible to achieve even when broad democratic agreement exists on policy. Benefits (legitimate amendment process) coexist with costs (structural barriers to democratic will).
constraint_indexing:constraint_classification(us_constitution_1787__positivist_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TEXTUALIST LEGAL ESTABLISHMENT (ROPE) — Judges, law professors, and jurisprudential traditions aligned with originalism and textualism experience this constraint as pure coordination. The text-binding rule is their interpretive framework; it provides doctrinal stability, career advancement pathways, and institutional authority. No extraction from their position — they are beneficiaries of the constraint's enforcement. The constraint solves their coordination problem: how to prevent judicial will from dominating constitutional meaning.
constraint_indexing:constraint_classification(us_constitution_1787__positivist_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: RIGHTS-EXPANDING JUDICIAL COALITION (TANGLED ROPE) — Justices and judges who believe the Constitution's purpose is to protect fundamental rights and adapt to changing circumstances experience the text-binding constraint as hybrid. It coordinates legitimate boundaries on judicial power (benefits the constraint). But it also extracts: confines their interpretive creativity, forces rationalization of constitutional evolution through textual doctrines (originalism, textualism, original public meaning), and creates reputational asymmetry where expansive reading is branded 'activism' but textual reading is branded 'neutral.' They have exit options (dissent, retire, voice criticism) but face reputational cost. Mixed extraction and coordination.
constraint_indexing:constraint_classification(us_constitution_1787__positivist_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: HISTORICAL ORIGINALIST MECHANISM (PITON) — The constraint's functional mechanism relies on identifying the Constitution's 'original public meaning' — what the text meant to educated readers at ratification in 1787-1791. But this mechanism is substantially performative: original public meaning is not directly accessible (historical sources are fragmentary, competing), and judges consistently reconstruct it in ways that align with modern policy preferences. The originalist ritual (citing Founding-era dictionaries, poring over state ratification records) has symbolic power but limited constraining force. The constraint persists through institutional prestige rather than functional verification. Theater ratio (0.55) reflects that the originalist methodology is a real constraint but also a performative artifact.
constraint_indexing:constraint_classification(us_constitution_1787__positivist_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, this constraint appears as an inherent property of written law: any fixed text must mean something determinate at the moment of utterance. The meaning cannot be completely open to reinterpretation without the text becoming meaningless. This perspective naturalizes text-binding as a logical necessity rather than a contingent institutional choice. However, the structural data reveals this as a false summit: identifiable beneficiaries (textualist establishment), active enforcement mechanisms, and measurable extraction all indicate this is a constructed constraint, not a natural law of meaning.
constraint_indexing:constraint_classification(us_constitution_1787__positivist_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_1787__positivist_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_constitution_1787__positivist_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_constitution_1787__positivist_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_constitution_1787__positivist_reading, TR),
    TR >= 0.70.

:- end_tests(us_constitution_1787__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42): Moderate. The constraint extracts from agents who perceive constitutional meaning as needing to evolve (Perspective 4: Rights-Expanding Coalition), but the extraction is not total because the democratic amendment process offers a formal exit pathway (albeit at very high cost). The extractiveness value reflects that the constraint is real and enforced, but not insurmountable. The rising trajectory (0.28 → 0.48 over 239 years) indicates that as the Constitution ages and social change accelerates, the fixed-text constraint becomes increasingly extractive — the gap between what the text says and what contemporary understanding requires grows wider. Suppression (0.48): Moderate-high. The constraint suppresses alternative interpretations through institutional prestige (originalism as 'neutral' methodology), reputational cost (charges of activism against adaptive judges), and doctrinal gatekeeping (originalist dominance in legal academia and judiciary). But suppression is not total — the living constitutionalism tradition survives, dissenting opinions articulate alternatives, and the constraint remains contested. Theater ratio (0.55): Moderate. The originalist methodology (discovery of original public meaning) is both a real constraint and a performative artifact. Historical sources are real, but 'original public meaning' is not directly accessible — it is reconstructed, and judges consistently reconstruct it in ways that align with modern preferences. The theater has increased over time as judges have become more sophisticated in originalist rhetoric while producing outcomes that reflect contemporary policy.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence. The textualist legal establishment (Perspective 3, Rope) sees the constraint as pure coordination — it solves the legitimate problem of preventing judicial will-worship. The democratic amendment process (Perspective 2, Tangled Rope) experiences real benefits (legitimate constitutional change mechanism) alongside real costs (supermajority barrier). The rights-expanding judiciary (Perspective 4, Tangled Rope) experiences extraction (interpretive foreclosure) alongside coordination benefits (constraint on other judges). The disenfranchised interpreter (Perspective 1, Snare) faces pure extraction with no exit. The originalist mechanism itself (Perspective 5, Piton) has degraded into performative theater as historical meaning has become harder to discover and judges have become more skilled at reconstructing it. The analytical observer (Perspective 6, Mountain, false summit) risks naturalizing the positivist reading as a logical necessity rather than a contingent institutional choice. The perspectival gap reveals that the constraint's beneficiaries and victims are sharply divided by institutional position and interpretive commitments.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by each agent's structural relationship to the constraint. Beneficiaries of text-binding (textualist establishment, democratic amendment process) have d ≈ 0.15-0.25 (low extraction flow toward them). Victims of text-binding (adaptive judiciary, constitutional drift) have d ≈ 0.70-0.85 (high extraction flow away from them). The powerless disenfranchised interpreter has d ≈ 0.95 (maximum target status). These values feed through the sigmoid f(d) to produce experienced extractiveness χ. Agents with arbitrage options (textualist establishment) experience negative χ (the constraint benefits them). Agents with trapped or constrained options (adaptive judges) experience high χ (they bear extraction). The democratic amendment coalition, despite their institutional power, experiences moderate extraction because the high supermajority barrier (2/3 + 3/4) forecloses many legitimate constitutional changes even when broad consensus exists.
 *
 * MANDATROPHY ANALYSIS:
 *   The positivist constraint avoids mandatrophy by cleanly separating judicial interpretation (constrained to text) from constitutional amendment (delegated to democracy). It does not claim that all constitutional problems can be solved by text-binding alone — it explicitly reserves adaptation to the amendment process. However, the constraint is vulnerable to a second-order mandatrophy: if the amendment process becomes so difficult that it effectively cannot function (if no constitutional amendment succeeds for 50+ years despite clear democratic pressure for change), then the constraint collapses. The amendment process is the safety valve; if it seals shut, the constraint becomes a pure Snare. The rising extractiveness measurements (0.28 → 0.48) model incipient mandatrophy risk — as drift accumulates without amendment relief, pressure on the constraint increases. See omega 'unwritten_constitution_feedback' for the endgame risk: if judicial practice develops a parallel unwritten constitution that supersedes the written text, the constraint fails entirely and is replaced by Piton (precedent theater) or Snare (locked-in judicial precedent).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_public_meaning_access,
    'Is ''original public meaning'' at the time of ratification (1787-1791) historically discoverable, or is it reconstructed to match modern interpretive goals?',
    'Systematic analysis of judicial disagreements on original public meaning for the same constitutional clause across different eras; correlation between judges'' policy preferences and their ''discovered'' original meanings; comparison of originalist interpretations across time (e.g., how did originalist scholars in 1987 read the Second Amendment vs. 2007 vs. 2024?)',
    'If discoverable: originalism provides a real constraint (mountain-adjacent). If reconstructed: originalism is performative theater masking modern policy choice (Piton confirmed). This directly determines whether Perspective 5 (piton) or an alternative classification (rope) is accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(original_public_meaning_access, empirical, 'Whether original public meaning is accessible or reconstructed').

omega_variable(
    amendment_process_functionality,
    'Is the Article V amendment process (2/3 + 3/4 supermajority) a legitimate democratic mechanism, or a structural barrier that prevents legitimate constitutional change?',
    'Historical comparison of constitutional amendments that passed vs. those that failed despite apparent broad democratic support; analysis of whether amendment failure reflects genuine disagreement or institutional barriers; comparison with other democracies'' amendment procedures and outcomes',
    'If legitimate: Tangled Rope classifications (Perspectives 2, 4) are correct — real coordination benefits and real but proportionate costs. If barrier: extractiveness rises substantially, and the constraint approaches Snare territory — the democratic amendment process becomes a facade masking judicial preservation of status quo.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_process_functionality, empirical, 'Whether Article V amendment process functions as legitimate democratic mechanism').

omega_variable(
    textualism_versus_living_constitution_empirical_outcomes,
    'Do textualist and originalist judges produce systematically different outcomes on cases with strong precedent than judges following living-constitution logic?',
    'Large-N empirical analysis of Supreme Court and appellate decisions: comparison of textualist vs. living-constitutionalist judges on cases where textual meaning is contested; analysis of whether different methodologies produce different holdings; measurement of citation patterns and doctrinal coherence across 50+ years',
    'If outcomes differ significantly: text-binding constraint has real force, confirming Rope perspective. If outcomes align: constraint is performative theater, confirming Piton perspective. This is diagnostic of whether the constraint actually constrains or merely rationalizes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textualism_versus_living_constitution_empirical_outcomes, empirical, 'Whether textualism and living constitutionalism produce different judicial outcomes').

omega_variable(
    reading_identity_uncertainty,
    'Is the positivist reading truly distinct from the originalist reading, or do they collapse into the same interpretive stance?',
    'Comparison of core axioms: positivism grounds constraint in democratic will + text (semantic positivity); originalism grounds constraint in historical intention + text. In cases where historical intention and current text clash, do the two readings recommend different interpretations?',
    'If distinct: two separate constraints with different ε values (positivism focuses on contemporary textual semantics; originalism focuses on 1787 intent). If collapsed: this story may be mischaracterized as a kernel reading when it is actually a variant of originalism. This omega documents the conceptual risk of the committer framing itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity_uncertainty, conceptual, 'Whether positivist reading is structurally distinct from originalist reading').

omega_variable(
    unwritten_constitution_feedback,
    'Does sustained judicial practice of adapting the Constitution through interpretation (living constitutionalism) eventually constitute an unwritten parallel constitution that supersedes the written text?',
    'Historical analysis of doctrinal evolution: measurement of deviation between written constitutional text and actual judicial practice over time; analysis of whether established doctrines contradict the text''s plain meaning; assessment of whether the unwritten Constitution (via precedent) constrains judges more than the written Constitution does',
    'If parallel unwritten constitution develops: the positivist constraint fails — democratic amendment becomes irrelevant because judicial precedent has already rewritten the Constitution. The constraint is forecast to collapse and be replaced by Piton (precedent-bound theater ritual) or Snare (precedent creates locked-in extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unwritten_constitution_feedback, empirical, 'Whether unwritten constitutional doctrine supersedes written text').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__positivist_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usplaw_theater_1787, us_constitution_1787__positivist_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(usplaw_theater_1987, us_constitution_1787__positivist_reading, theater_ratio, 4, 0.48).
narrative_ontology:measurement(usplaw_theater_2026, us_constitution_1787__positivist_reading, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(usplaw_extract_1787, us_constitution_1787__positivist_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(usplaw_extract_1887, us_constitution_1787__positivist_reading, base_extractiveness, 2, 0.35).
narrative_ontology:measurement(usplaw_extract_1987, us_constitution_1787__positivist_reading, base_extractiveness, 4, 0.42).
narrative_ontology:measurement(usplaw_extract_2026, us_constitution_1787__positivist_reading, base_extractiveness, 6, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(usplaw_supp_1787, us_constitution_1787__positivist_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(usplaw_supp_1920, us_constitution_1787__positivist_reading, suppression_requirement, 3, 0.45).
narrative_ontology:measurement(usplaw_supp_2026, us_constitution_1787__positivist_reading, suppression_requirement, 6, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_1787__positivist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_1787__positivist_reading, us_constitution_1787__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_1787__positivist_reading, us_constitution_1787__living_reading).
narrative_ontology:affects_constraint(us_constitution_1787__positivist_reading, judicial_amendment_power_constraint).
narrative_ontology:affects_constraint(us_constitution_1787__positivist_reading, article_v_supermajority_barrier).

% DUAL FORMULATION NOTE:
% The positivist reading is one of three kernel readings of the same constitutional text. The originalist reading (grounding in Framers' intent) and living reading (grounding in adaptive interpretation) are alternative constraint stories with different ε values, beneficiary/victim structures, and temporal trajectories. Each reading instantiates a different constraint despite sharing the same legal text as their kernel. They are linked via reading_relations (forecloses/coexists_with/influences) in cs_structure, not via classical network affects relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_1787__positivist_reading, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
