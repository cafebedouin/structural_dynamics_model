% ============================================================================
% CONSTRAINT STORY: us_constitution_meaning__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_meaning__positivist_reading, []).

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
 *   constraint_id: us_constitution_meaning__positivist_reading
 *   human_readable: Legal Positivist Reading of Constitutional Validity (Pedigree Over Morality)
 *   domain: constitutional_law/legal_theory/political_philosophy
 *
 * SUMMARY:
 *   This constraint isolates the positivist reading of the contested US
 *   constitutional meaning kernel: validity is a function of formal enactment
 *   pedigree — was this norm produced by an authorized body through the
 *   correct procedure — and NOT of whether it tracks external moral
 *   principles. This is distinct from originalism (which fixes semantic
 *   content at the ratification moment) and from living constitutionalism
 *   (which allows application to evolve with social attitudes). The
 *   positivist reading can, in principle, accommodate any content the formal
 *   process produces, including content that later evolves through Article V
 *   amendment; its distinguishing commitment is procedural, not semantic or
 *   evolutionary. As the amendment process becomes practically gridlocked
 *   (rising polarization over the interval), this reading's own internal
 *   machinery for legitimate change is starved, and it functionally converges
 *   toward the originalist reading's fixed-meaning posture — not because it
 *   changed its theory, but because its only sanctioned mechanism for change
 *   stopped operating.
 *
 * KEY AGENTS:
 *   - enacting_institutions: Primary agenda_setter (institutional/arbitrage) — administers the only channels of formal meaning-change
 *   - procedural_legitimacy_apparatus: Primary beneficiary (institutional/analytical) — collects predictability and adjudicative insulation from the pedigree test
 *   - substantive_justice_claimants: Primary payer (powerless/trapped) — bears the cost of claims that cannot be cashed out in enacted form
 *   - sitting_judiciary: Secondary agenda_setter/beneficiary (institutional/analytical) — administers and is shielded by the test
 *   - legal_academy_positivist_scholars: Analytical observer (analytical/analytical) — defends and theorizes the reading from within the benefiting tradition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_meaning__positivist_reading, 0.48).
domain_priors:suppression_score(us_constitution_meaning__positivist_reading, 0.55).
domain_priors:theater_ratio(us_constitution_meaning__positivist_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_meaning__positivist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_meaning__positivist_reading, "Legal Positivist Reading of Constitutional Validity (Pedigree Over Morality)").
narrative_ontology:topic_domain(us_constitution_meaning__positivist_reading, "constitutional_law/legal_theory/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_meaning__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_meaning__positivist_reading, '91da38c5-9cb4-4dc4-9209-634f9a9f94d5').
narrative_ontology:cs_kernel_codification('91da38c5-9cb4-4dc4-9209-634f9a9f94d5', fixed_text).
narrative_ontology:cs_authority_grounding('91da38c5-9cb4-4dc4-9209-634f9a9f94d5', extraction).
narrative_ontology:cs_interpretation_layer_present('91da38c5-9cb4-4dc4-9209-634f9a9f94d5').
narrative_ontology:cs_reading_relation('91da38c5-9cb4-4dc4-9209-634f9a9f94d5', us_constitution_meaning__originalist_reading, influences).
narrative_ontology:cs_reading_relation('91da38c5-9cb4-4dc4-9209-634f9a9f94d5', us_constitution_meaning__living_constitutionalist_reading, coexists_with).
narrative_ontology:cs_axiom('91da38c5-9cb4-4dc4-9209-634f9a9f94d5', foundational, validity_determined_by_pedigree_not_morality).
narrative_ontology:cs_axiom_status(validity_determined_by_pedigree_not_morality, holdable).
narrative_ontology:cs_axiom_grounding('91da38c5-9cb4-4dc4-9209-634f9a9f94d5', validity_determined_by_pedigree_not_morality, conventional).
narrative_ontology:cs_axiom('91da38c5-9cb4-4dc4-9209-634f9a9f94d5', secondary, amendment_process_is_exclusive_change_channel).
narrative_ontology:cs_axiom_status(amendment_process_is_exclusive_change_channel, holdable).
narrative_ontology:cs_axiom_grounding('91da38c5-9cb4-4dc4-9209-634f9a9f94d5', amendment_process_is_exclusive_change_channel, conventional).
narrative_ontology:cs_reference_frame('91da38c5-9cb4-4dc4-9209-634f9a9f94d5', formal_enactment_pedigree_test).
narrative_ontology:cs_drift_state('91da38c5-9cb4-4dc4-9209-634f9a9f94d5', post_1980s_polarization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('91da38c5-9cb4-4dc4-9209-634f9a9f94d5', '').
narrative_ontology:cs_kernel_id(us_constitution_meaning__positivist_reading, us_constitution_meaning).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_meaning__positivist_reading, procedural_legitimacy_apparatus).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__positivist_reading, enacting_institutions).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__positivist_reading, rule_of_law_predictability_seekers).
narrative_ontology:constraint_victim(us_constitution_meaning__positivist_reading, substantive_justice_claimants).
narrative_ontology:constraint_victim(us_constitution_meaning__positivist_reading, unenumerated_rights_claimants).
narrative_ontology:constraint_victim(us_constitution_meaning__positivist_reading, gridlock_era_reform_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__positivist_reading, sitting_judiciary).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__positivist_reading, legal_academy_positivist_scholars).
narrative_ontology:constraint_vindicates(us_constitution_meaning__positivist_reading, rule_of_recognition_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_meaning__positivist_reading, separation_of_law_and_morals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Congress, state ratifying conventions, and Article V amendment bodies control the only channels through which constitutional meaning may formally change. Positivism vests them with sole authority to alter validity conditions; they administer the pedigree rules (enactment, ratification, formal amendment) that make a norm law rather than mere argument.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, enacting_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% Courts, legal academies, and bar institutions that gain predictability, professional insulation, and adjudicative legitimacy from a validity test that can be checked by pedigree alone rather than contested moral argument. This apparatus collects the reputational and institutional stability that a rule-of-recognition test provides — judges can decide 'is this law' without deciding 'is this good.'
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, procedural_legitimacy_apparatus, beneficiary,
    institutional, civilizational, analytical, national).

% Litigants and movements whose claims rest on moral or justice-based arguments that lack a clear textual or formally-enacted anchor. Under this reading, a judge cannot rule for them merely because their claim is more just — the claim must be cashed out in terms of what was formally enacted. They bear the cost of the coordination gain in predictability: their substantively strong claims are procedurally unrecognizable.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, substantive_justice_claimants, payer,
    powerless, biographical, trapped, national).

% Groups asserting rights not textually enumerated (privacy, dignity, bodily autonomy in forms not tied to specific clauses) find the positivist reading unreceptive absent formal amendment or a clear enactment hook. Their only path to recognition is the amendment process itself, which is structurally difficult to invoke.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, unenumerated_rights_claimants, payer,
    powerless, biographical, constrained, national).

% Organized political movements seeking constitutional change through Article V find the amendment threshold (two-thirds of Congress, three-fourths of states) effectively unreachable in a polarized era. When formal amendment is gridlocked, the positivist reading offers no alternative validity channel — the reading collapses functionally toward originalism, since fixed enactment-moment meaning becomes the only stable pedigree available absent new formal action.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, gridlock_era_reform_movements, payer,
    organized, generational, constrained, national).

% Federal judges applying the positivist test decide validity by checking pedigree — was this enacted through the correct formal procedure by an authorized body — rather than weighing whether the outcome is just. This shields them from charges of imposing personal morality but also constrains them from remedying substantive injustice where no formal textual hook exists. They administer the test and benefit from its insulating function.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, sitting_judiciary, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_meaning__positivist_reading, sitting_judiciary, beneficiary).

% Scholars in the Hart/Raz tradition analyze and defend the separation of law and morals as descriptively accurate and normatively valuable for legal certainty. Their professional and intellectual capital is invested in the coherence of this reading; they observe the kernel contest from within a tradition that also benefits from the reading's institutional dominance in mainstream jurisprudence pedagogy.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, legal_academy_positivist_scholars, observer,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_meaning__positivist_reading, legal_academy_positivist_scholars, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_meaning__positivist_reading, procedural_legitimacy_apparatus).
narrative_ontology:fixing_cost_class(us_constitution_meaning__positivist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, checkable test for what counts as valid law — a rule of recognition — so that officials, citizens, and courts can identify binding constitutional norms without each having to independently adjudicate the underlying morality of every provision. This solves a genuine coordination problem: without a pedigree test, legal certainty collapses into perpetual moral relitigation of settled law.
% TRANSFER_FUNCTION: Moves interpretive authority away from claimants whose grievances rest on unenacted moral or justice principles and concentrates it in enacting institutions and the judges who verify formal pedigree. Predictability and institutional legitimacy accrue to the apparatus that administers the test; the cost is borne by those whose substantively meritorious claims cannot be cashed out in formally enacted terms.
% ABSENT_VOICES: Claimants for rights or remedies that were never formally enacted — because they were unimaginable, politically impossible, or deliberately excluded at the time of enactment — have no seat in the validity determination itself. Natural-law and living-constitutionalist theorists would object that this reading launders substantive political choices (whose injustices go unremedied) as neutral procedural findings; they are present in the broader jurisprudential debate but structurally excluded from influencing what counts as 'valid' under this reading's own test.
% DISAPPEARANCE_RATIONALE: If the positivist test vanished, courts would lose the primary vocabulary for distinguishing 'this is not yet law' from 'this is unjust law' — validity determinations would immediately import moral reasoning by default (converging toward the living-constitutionalist reading), enacting institutions would lose their monopoly on meaning-change, and settled expectations built on formal pedigree (statute of limitations regimes, contract enforcement premised on textual certainty, administrative law's reliance on enacted delegation) would face fresh moral relitigation.
% FOUNDING_PROBLEM: Legal positivism as applied to constitutional interpretation was built to solve the problem of naturalism's indeterminacy: if validity depends on external moral correctness, judges have no principled stopping point and every case becomes a referendum on first-order moral philosophy, undermining predictability, separation of powers, and democratic legitimacy of judicial review.
% FOUNDING_PROBLEM_CORROBORATION: Positivist legal scholars (Hart, Raz, and their contemporary successors) attest the problem remains live — moral indeterminacy in adjudication is treated as a permanent structural risk, not a historical artifact. Critical legal scholars and natural-law theorists, writing from outside the beneficiary tradition, corroborate only a narrower claim: that SOME formal-validity test was historically needed for coordination, while disputing that the strict separation from morality is still functionally necessary or that it does not itself smuggle in a political preference for status-quo distributions under the guise of neutrality. No source entirely outside jurisprudential academia (e.g., a purely empirical social-science account) independently corroborates the founding problem's continued necessity versus its current use as a shield for outcome preferences.
narrative_ontology:disappearance_verdict(us_constitution_meaning__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_meaning__positivist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_meaning__positivist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_meaning__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_meaning__positivist_reading, 0.48, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_meaning__positivist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_meaning__positivist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_meaning__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate 0.48 rather than low or high: the positivist test genuinely solves a coordination problem (predictable identification of binding law) but does so by structurally foreclosing an entire category of claim (unenacted moral/justice claims) regardless of merit — that asymmetry is real extraction, not merely coordination cost. Suppression (0.55) reflects that the reading is actively defended by an institutional apparatus (courts, bar associations, legal academy) against rival readings, and the rising suppression_requirement trajectory tracks a jurisprudential environment where gridlock has made the positivist/originalist boundary harder to maintain, requiring more active theoretical defense. Theater ratio (0.3) is moderate-low: much of the pedigree-checking function is real institutional work, though an increasing share of doctrinal writing defends the separation-of-law-and-morals boundary rhetorically as gridlock pressure mounts.
 *
 * PERSPECTIVAL GAP:
 *   From the enacting institutions' and sitting judiciary's seat, the positivist test looks like principled restraint — a discipline that prevents judges from imposing personal morality under color of law. From the seat of substantive_justice_claimants and unenumerated_rights_claimants, the identical structure looks like a formal gatekeeping mechanism that launders a substantive political choice (to not remedy their claim) as a neutral procedural finding. The engine should compute divergent seat classifications from these structurally different positions even though both examine the same enacted rule of recognition.
 *
 * DIRECTIONALITY LOGIC:
 *   Enacting institutions and the procedural legitimacy apparatus sit near the beneficiary end: they set and administer the validity test and collect its predictability and insulation benefits with maximal exit (arbitrage/analytical). Substantive justice claimants and unenumerated rights claimants sit near the target end: trapped or constrained exit, no channel to recognition outside the formal one, bearing the cost of claims foreclosed by the test. Gridlock-era reform movements are organized (some coalition power) but still constrained — their only sanctioned channel (Article V) is functionally closed, which is why the reading collapses toward originalism in practice for them specifically.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (moral indeterminacy undermining predictable adjudication) is contested rather than resolved: positivist scholars maintain it is permanently live, but critics note that a strict separation-of-law-and-morals rule, once useful for coordination, can persist as an unexamined default that shields particular substantive outcomes (status-quo distributions) from moral scrutiny long after the coordination need could be met by a more moderate test. Classifying this as tangled_rope rather than snare or mountain prevents two mislabelings: it would be wrong to call this pure extraction (the coordination function — a shared test for legal validity — is genuine and valuable), and it would be wrong to call it a mountain of pure logical necessity (it is a chosen jurisprudential commitment that could be, and is, contested by two live sibling readings).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    positivism_originalism_collapse_condition,
    'Under what precise conditions does the positivist reading functionally collapse into the originalist reading, and is that collapse a contingent political fact (Article V gridlock) or a structural inevitability of positivism itself once any amendment mechanism exists?',
    'Comparative constitutional analysis: examine polities with easier amendment thresholds to see whether their positivist-style validity tests avoid convergence with fixed-meaning originalism, or track US Article V invocation-success rates against measured doctrinal convergence between positivist and originalist opinions over time.',
    'If collapse is contingent on gridlock, the positivist reading remains a genuinely distinct constraint whose extraction profile would fall if amendment access were restored. If collapse is structural, the positivist and originalist readings are not truly independent siblings but the same reading under different amendment-access conditions, which would require re-examining the kernel decomposition itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(positivism_originalism_collapse_condition, conceptual, 'Whether positivist/originalist convergence under gridlock is contingent or structurally inevitable.').

omega_variable(
    procedural_neutrality_vs_outcome_laundering,
    'Is the strict separation of validity from morality a genuinely neutral coordination device, or does it systematically launder specific substantive outcomes (favoring status-quo distributions of rights and power) as procedurally neutral findings?',
    'Empirical study of case outcomes under positivist-framed reasoning versus living-constitutionalist reasoning on matched fact patterns, controlling for judicial ideology, to isolate whether the reading itself predicts systematically different distributive outcomes independent of the judge applying it.',
    'If the reading systematically favors status-quo distributions regardless of judge, that supports the tangled_rope classification''s victim declarations as structural rather than incidental. If outcomes track judicial ideology more than reading choice, the extraction attributed to the reading itself should be revised downward.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(procedural_neutrality_vs_outcome_laundering, empirical, 'Whether procedural neutrality is genuine or an outcome-laundering mechanism.').

omega_variable(
    kernel_framing_alternative_institutional_vs_legitimacy_layer,
    'Should this reading be framed at the level of the formal enactment procedures themselves (the institutional framing used here), or at the level of the deeper legitimacy claim that formal procedures ARE the correct locus of validity (a jurisprudential-commitment framing one layer up)?',
    'Compare classification outcomes: author a variant story treating ''the legitimacy claim that procedure exhausts validity'' as the kernel object itself, with the current enactment-procedure story as a downstream instantiation, and check whether ε and stakeholder structure diverge meaningfully between the two framings.',
    'If the two framings produce materially different ε or classification, that divergence itself is diagnostic evidence for further decomposition per the ε-invariance principle; if they converge, the current framing is safely stable and no further split is needed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_alternative_institutional_vs_legitimacy_layer, conceptual, 'Institutional-procedure framing versus legitimacy-claim framing of the same kernel reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_meaning__positivist_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_meaning__positivist_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(us_c_tr_t10, us_constitution_meaning__positivist_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(us_c_tr_t20, us_constitution_meaning__positivist_reading, theater_ratio, 20, 0.23).
narrative_ontology:measurement(us_c_tr_t30, us_constitution_meaning__positivist_reading, theater_ratio, 30, 0.25).
narrative_ontology:measurement(us_c_tr_t40, us_constitution_meaning__positivist_reading, theater_ratio, 40, 0.27).
narrative_ontology:measurement(us_c_tr_t50, us_constitution_meaning__positivist_reading, theater_ratio, 50, 0.29).
narrative_ontology:measurement(us_c_tr_t60, us_constitution_meaning__positivist_reading, theater_ratio, 60, 0.3).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_meaning__positivist_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(us_c_be_t10, us_constitution_meaning__positivist_reading, base_extractiveness, 10, 0.36).
narrative_ontology:measurement(us_c_be_t20, us_constitution_meaning__positivist_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(us_c_be_t30, us_constitution_meaning__positivist_reading, base_extractiveness, 30, 0.44).
narrative_ontology:measurement(us_c_be_t40, us_constitution_meaning__positivist_reading, base_extractiveness, 40, 0.46).
narrative_ontology:measurement(us_c_be_t50, us_constitution_meaning__positivist_reading, base_extractiveness, 50, 0.47).
narrative_ontology:measurement(us_c_be_t60, us_constitution_meaning__positivist_reading, base_extractiveness, 60, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_meaning__positivist_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(us_c_su_t10, us_constitution_meaning__positivist_reading, suppression_requirement, 10, 0.44).
narrative_ontology:measurement(us_c_su_t20, us_constitution_meaning__positivist_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(us_c_su_t30, us_constitution_meaning__positivist_reading, suppression_requirement, 30, 0.51).
narrative_ontology:measurement(us_c_su_t40, us_constitution_meaning__positivist_reading, suppression_requirement, 40, 0.53).
narrative_ontology:measurement(us_c_su_t50, us_constitution_meaning__positivist_reading, suppression_requirement, 50, 0.54).
narrative_ontology:measurement(us_c_su_t60, us_constitution_meaning__positivist_reading, suppression_requirement, 60, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_meaning__positivist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_meaning__positivist_reading, 0.12).
narrative_ontology:affects_constraint(us_constitution_meaning__positivist_reading, us_constitution_meaning__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_meaning__positivist_reading, us_constitution_meaning__living_constitutionalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the us_constitution_meaning kernel, decomposed per the ε-invariance principle because the colloquial label 'what makes the Constitution mean what it means' conflates three structurally distinct validity theories with different ε, different beneficiary/victim sets, and different failure conditions. The positivist_reading (this story) locates validity in formal enactment pedigree and excludes moral reasoning from the validity test itself. It structurally influences (rather than forecloses) the originalist_reading because gridlock in the amendment process pushes positivist practice toward originalist fixed-meaning outcomes by default, without positivism abandoning its own procedural premise. It coexists with the living_constitutionalist_reading as a live, mutually exclusive-in-application but simultaneously-held position across different judicial coalitions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
