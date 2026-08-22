% ============================================================================
% CONSTRAINT STORY: orthographic_kernel__modernization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_kernel__modernization_reading, []).

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
 *   constraint_id: orthographic_kernel__modernization_reading
 *   human_readable: 1928 Turkish Alphabet Reform (Latin Script) — Modernization Reading
 *   domain: political_linguistics/state_formation/commitment_systems
 *
 * SUMMARY:
 *   This story authors the modernization reading of the orthographic kernel
 *   underlying Turkey's 1928 alphabet reform: the claim that adopting the
 *   Latin script was justified primarily by its capacity to accelerate
 *   technological, scientific, and educational modernization while remaining
 *   faithful to Turkish (as opposed to Ottoman/Arabic) linguistic identity.
 *   This reading treats the reform as a coordination mechanism — solving a
 *   genuine phonetic mismatch between Turkish and the Arabic abjad — while
 *   acknowledging the reform's enforcement imposed real, asymmetric
 *   transition costs on the previously literate. It is one of three sibling
 *   readings of the same kernel (the 1928 script change): continuity_reading
 *   (Arabic script preserves Ottoman/Islamic textual continuity) and
 *   rupture_reading (script change as deliberate cultural rupture from the
 *   Ottoman/Islamic past). Per the ε-invariance discipline, this file does
 *   not average across those readings or hedge its ε against them — it
 *   authors ONE stable ε (0.42) for the standing arrangement (the enforced
 *   Latin-script mandate) as the modernization reading's own lights see it:
 *   real coordination benefit, real but time-limited extraction from the
 *   transitional generation.
 *
 * KEY AGENTS:
 *   - state_bureaucracy: agenda_setter/beneficiary (institutional/arbitrage) — designs and enforces the reform, gains administrative efficiency and legitimacy
 *   - new_literate_class: beneficiary (moderate/mobile) — gains literacy access and civil-service mobility under the new script
 *   - older_arabic_literate_generation: payer (powerless/trapped) — functionally delegitimized literacy, no exit from the mandate
 *   - religious_scholars_and_imams: payer (moderate/constrained) — lose interpretive centrality in secular administration
 *   - linguists_and_education_historians: analytical observer — assesses literacy outcomes independent of state self-reporting
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_kernel__modernization_reading, 0.42).
domain_priors:suppression_score(orthographic_kernel__modernization_reading, 0.58).
domain_priors:theater_ratio(orthographic_kernel__modernization_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_kernel__modernization_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(orthographic_kernel__modernization_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(orthographic_kernel__modernization_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_kernel__modernization_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(orthographic_kernel__modernization_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_kernel__modernization_reading, tangled_rope).
narrative_ontology:human_readable(orthographic_kernel__modernization_reading, "1928 Turkish Alphabet Reform (Latin Script) — Modernization Reading").
narrative_ontology:topic_domain(orthographic_kernel__modernization_reading, "political_linguistics/state_formation/commitment_systems").

domain_priors:requires_active_enforcement(orthographic_kernel__modernization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_kernel__modernization_reading, 'f2d25fd5-a632-4982-a00d-20c2b5a797eb').
narrative_ontology:cs_kernel_codification('f2d25fd5-a632-4982-a00d-20c2b5a797eb', formalized).
narrative_ontology:cs_authority_grounding('f2d25fd5-a632-4982-a00d-20c2b5a797eb', extraction).
narrative_ontology:cs_interpretation_layer_present('f2d25fd5-a632-4982-a00d-20c2b5a797eb').
narrative_ontology:cs_reading_relation('f2d25fd5-a632-4982-a00d-20c2b5a797eb', orthographic_kernel__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('f2d25fd5-a632-4982-a00d-20c2b5a797eb', orthographic_kernel__rupture_reading, coexists_with).
narrative_ontology:cs_axiom('f2d25fd5-a632-4982-a00d-20c2b5a797eb', foundational, script_choice_is_instrumentally_evaluable_by_functional_fit).
narrative_ontology:cs_axiom_status(script_choice_is_instrumentally_evaluable_by_functional_fit, holdable).
narrative_ontology:cs_axiom_grounding('f2d25fd5-a632-4982-a00d-20c2b5a797eb', script_choice_is_instrumentally_evaluable_by_functional_fit, instrumental).
narrative_ontology:cs_axiom('f2d25fd5-a632-4982-a00d-20c2b5a797eb', foundational, linguistic_identity_preservation_is_separable_from_script_form).
narrative_ontology:cs_axiom_status(linguistic_identity_preservation_is_separable_from_script_form, holdable).
narrative_ontology:cs_axiom_grounding('f2d25fd5-a632-4982-a00d-20c2b5a797eb', linguistic_identity_preservation_is_separable_from_script_form, conventional).
narrative_ontology:cs_reference_frame('f2d25fd5-a632-4982-a00d-20c2b5a797eb', ottoman_arabic_script_administrative_continuity).
narrative_ontology:cs_drift_state('f2d25fd5-a632-4982-a00d-20c2b5a797eb', post_1928_decree_enforcement, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('f2d25fd5-a632-4982-a00d-20c2b5a797eb', '').
narrative_ontology:cs_kernel_id(orthographic_kernel__modernization_reading, orthographic_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_kernel__modernization_reading, state_bureaucracy).
narrative_ontology:constraint_beneficiary(orthographic_kernel__modernization_reading, new_literate_class).
narrative_ontology:constraint_beneficiary(orthographic_kernel__modernization_reading, print_and_publishing_industry).
narrative_ontology:constraint_victim(orthographic_kernel__modernization_reading, older_arabic_literate_generation).
narrative_ontology:constraint_victim(orthographic_kernel__modernization_reading, religious_scholars_and_imams).
narrative_ontology:constraint_victim(orthographic_kernel__modernization_reading, rural_populations_facing_compressed_reeducation).
narrative_ontology:constraint_vindicates(orthographic_kernel__modernization_reading, script_rationalization_accelerates_literacy).
narrative_ontology:constraint_vindicates(orthographic_kernel__modernization_reading, phonetic_fit_of_latin_alphabet_to_turkish_phonology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs, decrees, and enforces the alphabet change through the Language Commission and mandatory schooling, banning official use of Arabic script within a compressed timeline. Gains a unified, phonetically transparent administrative and educational medium that lowers the cost of running schools, courts, and a modern press, and gains legitimacy as the vanguard of the modernization project.
narrative_ontology:constraint_stakeholder(orthographic_kernel__modernization_reading, state_bureaucracy, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(orthographic_kernel__modernization_reading, state_bureaucracy, beneficiary).

% Younger people and urban populations who learn to read for the first time or transition easily, gaining access to a simplified phonetic script, expanding literacy, and new civil-service and professional opportunities tied to the new orthography.
narrative_ontology:constraint_stakeholder(orthographic_kernel__modernization_reading, new_literate_class, beneficiary,
    moderate, biographical, mobile, national).

% Publishers, textbook producers, and typesetters who retool for Latin type gain a large captive market as the state mandates new textbooks, newspapers, and official documents be reissued in the new script.
narrative_ontology:constraint_stakeholder(orthographic_kernel__modernization_reading, print_and_publishing_industry, beneficiary,
    organized, generational, mobile, national).

% Adults literate in the Ottoman Arabic script suddenly find their reading and writing skill obsolete for official and increasingly social purposes; many never fully transition, becoming functionally illiterate in the new national medium despite decades of prior literacy. No feasible exit — the script is mandated nationwide.
narrative_ontology:constraint_stakeholder(orthographic_kernel__modernization_reading, older_arabic_literate_generation, payer,
    powerless, biographical, trapped, national).

% Scholars whose authority and function were tied to reading/teaching Arabic-script Ottoman and Quranic texts see their interpretive monopoly narrowed as the new script's civil dominance signals reduced state sponsorship of Arabic-script literacy; can continue religious instruction but lose centrality in secular administration and mainstream schooling.
narrative_ontology:constraint_stakeholder(orthographic_kernel__modernization_reading, religious_scholars_and_imams, payer,
    moderate, civilizational, constrained, national).

% Villagers with limited access to the crash literacy campaigns (Millet Mektepleri) bear the disruption of a mandated script change with the least institutional support to complete the transition, deepening urban-rural literacy gaps for a period.
narrative_ontology:constraint_stakeholder(orthographic_kernel__modernization_reading, rural_populations_facing_compressed_reeducation, payer,
    powerless, biographical, trapped, regional).

% Study literacy rate changes, transition costs, and the phonetic fit of the new orthography, producing the empirical record used to evaluate whether the modernization claims were vindicated.
narrative_ontology:constraint_stakeholder(orthographic_kernel__modernization_reading, linguists_and_education_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(orthographic_kernel__modernization_reading, state_bureaucracy).
narrative_ontology:fixing_cost_class(orthographic_kernel__modernization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Adopting a single, phonetically transparent script for a language chronically mismatched with the Arabic abjad solves a genuine coordination problem: mass literacy campaigns, standardized schooling, and a modern print/administrative apparatus all require one stable, teachable orthography rather than a script poorly suited to representing Turkish vowels.
% TRANSFER_FUNCTION: Moves administrative, educational, and cultural capital from those whose competence was vested in Ottoman Arabic-script literacy (scholars, older literate adults, established scribal/bureaucratic classes) to a new literate cohort and the reformed state apparatus that certifies and teaches the new script — measured in literacy credentials, civil-service access, and control over what counts as legible authority.
% ABSENT_VOICES: Older Arabic-script literate citizens and religious scholars were consulted little if at all in the Language Commission's design process; their functional literacy loss was treated as an acceptable transition cost rather than a negotiated one. They are structurally present as payers but excluded from the agenda-setting conversation.
% DISAPPEARANCE_RATIONALE: Had the Latin script mandate been reversed or never enforced, Ottoman Arabic-script literacy would likely have persisted alongside informal Latin use, the state's crash literacy campaigns would not have created the same generational literacy discontinuity, and the modern Turkish print/education apparatus would have developed on a different, more continuous orthographic base.
% FOUNDING_PROBLEM: Ottoman Turkish, written in a modified Arabic abjad poorly suited to Turkish's vowel-rich phonology, produced low literacy rates and inconsistent spelling, seen by reformers as an obstacle to mass education and participation in a modern, print-based, scientifically literate society.
% FOUNDING_PROBLEM_CORROBORATION: UNESCO literacy statistics and independent linguistic surveys from the mid-20th century onward corroborate that Turkish literacy rates rose substantially after the reform and that the Latin alphabet's phonetic fit is measurably closer to Turkish phonology than the Arabic abjad was — corroboration exists outside the Turkish state's own self-reporting. However, this corroborates only the literacy-mechanics founding problem; it does not by itself corroborate the broader modernization narrative's causal claims about the reform's contribution to scientific advancement, which remain contested among historians.
narrative_ontology:disappearance_verdict(orthographic_kernel__modernization_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_kernel__modernization_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_kernel__modernization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(orthographic_kernel__modernization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_kernel__modernization_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_kernel__modernization_reading_tests).
:- end_tests(orthographic_kernel__modernization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.42) is moderate, not high, because the modernization reading's own evidence base (literacy gains, phonetic fit) is real and independently corroborated — this is not a pure extraction story. But it is not negligible either: the reform was compulsory, timelines were compressed (roughly three months from decree to mandatory adoption), and it produced a measurable, unevenly distributed literacy discontinuity that fell hardest on those with no institutional path to re-training. Suppression starts high (0.75) reflecting the initial ban on Arabic-script use in official contexts and criminal penalties for non-compliant publishing, then declines over the interval (0.52) as the new script normalizes and enforcement machinery becomes less necessary. Theater ratio is low-to-moderate and rises slightly as the crash literacy campaigns (Millet Mektepleri) generate both genuine instruction and increasingly performative certification drives as political showcases of the republic's progress.
 *
 * PERSPECTIVAL GAP:
 *   The state_bureaucracy seat and the older_arabic_literate_generation seat compute this constraint very differently even though they inhabit the same historical event: from the bureaucracy's seat, the reform reads as successful coordination — it built literacy infrastructure it did not have before and consolidated administrative capacity. From the older generation's seat, structurally trapped with no path to exit the mandate, the same reform reads as an imposed cost with no compensating benefit to their existing skill set. The engine's per-seat computation is expected to diverge sharply here; that divergence is exactly what the tangled_rope classification names.
 *
 * DIRECTIONALITY LOGIC:
 *   State bureaucracy and the new literate class sit near the beneficiary end of directionality: they gain administrative capacity, legitimacy, and literacy/mobility respectively, with the state holding arbitrage-grade control over the mandate's design and timeline. The older Arabic-literate generation and rural populations sit near the full-target end: trapped exit options (no way to opt out of a national script mandate), immediate and involuntary bearing of the transition cost. Religious scholars sit closer to the target end but with somewhat more constrained (not fully trapped) exit, since religious instruction in Arabic script continued in a narrower institutional lane.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (poor phonetic fit between Turkish and the Arabic abjad, low resulting literacy) is corroborated as substantially resolved by independent literacy statistics — this is not a case of an obsolete mandate persisting by inertia; the mandate's core coordination function (a single national, phonetically transparent script) remains live and functioning today, which is why this reading does not classify as piton. Classifying it as tangled_rope rather than pure rope prevents mislabeling the coercive, unevenly distributed transition costs as costless coordination; classifying it as tangled_rope rather than pure snare prevents mislabeling a genuinely functional, independently corroborated literacy mechanism as pure extraction with no coordination substance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    modernization_vs_rupture_intent,
    'Was the primary intent behind the script reform genuinely instrumental (solving a literacy/phonetics problem) or was modernization framing a post-hoc justification for a deliberate act of cultural rupture from the Ottoman/Islamic past?',
    'Archival analysis of Language Commission deliberations, Atatürk''s private correspondence and speeches, and comparison of the compressed implementation timeline (which arguably exceeded what pure literacy goals required) against alternative reform designs that were considered and rejected.',
    'If intent was substantially rupture-driven, this modernization_reading''s own ε may understate the extraction directed at religious and Ottoman-literate communities, since the transition speed and severity would then reflect rupture goals rather than literacy-optimization goals — this would strengthen the case for weighting the rupture_reading constraint''s ε more heavily in any composite historical assessment, without changing this reading''s own internally-consistent ε.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modernization_vs_rupture_intent, conceptual, 'Whether the modernization framing is the genuine driving logic or a legitimating narrative for a different underlying intent (rupture).').

omega_variable(
    literacy_gain_causal_attribution,
    'How much of Turkey''s mid-20th century literacy rate increase is attributable to the script change itself (phonetic fit) versus concurrent state investment in mass schooling, the Millet Mektepleri campaigns, and other simultaneous modernization programs?',
    'Comparative studies against other contemporaneous Turkic or regional literacy campaigns that did not change scripts, controlling for schooling investment, urbanization, and print infrastructure growth.',
    'If script change contributed only marginally beyond what schooling investment alone would have achieved, the modernization reading''s justification for the transition costs imposed on the older generation is weaker than the vindicated_propositions here claim, which would push the effective ε upward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literacy_gain_causal_attribution, empirical, 'How much of the literacy gain is causally attributable to the script itself versus concurrent educational investment.').

omega_variable(
    kernel_framing_alternative,
    'Is the orthographic_kernel better framed as a single decree event (the 1928 law) or as the layered legitimacy narrative built up around it over subsequent decades (state historiography, textbook memory-making) that this modernization_reading partly reflects?',
    'Compare classification outcomes if ε and stakeholder structure were authored against the 1928 decree text alone versus against the accumulated mid-century state narrative about the decree''s success.',
    'If the kernel is the decree text alone, extraction may register lower (closer to the immediate literacy-mechanics problem); if the kernel includes the accumulated legitimizing narrative, extraction registers higher because the narrative itself performs ongoing suppression of the continuity_reading''s counter-claims in Turkish civic education. This story adopts the decree-plus-early-enforcement framing (1928-1960) as the more defensible referent for a bounded ε.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_alternative, conceptual, 'Whether the kernel is best framed as the discrete legal event or the accumulated legitimating narrative built around it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_kernel__modernization_reading, 1928, 1960).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_tr_t1928, orthographic_kernel__modernization_reading, theater_ratio, 1928, 0.15).
narrative_ontology:measurement(orth_tr_t1933, orthographic_kernel__modernization_reading, theater_ratio, 1933, 0.18).
narrative_ontology:measurement(orth_tr_t1938, orthographic_kernel__modernization_reading, theater_ratio, 1938, 0.2).
narrative_ontology:measurement(orth_tr_t1945, orthographic_kernel__modernization_reading, theater_ratio, 1945, 0.22).
narrative_ontology:measurement(orth_tr_t1952, orthographic_kernel__modernization_reading, theater_ratio, 1952, 0.24).
narrative_ontology:measurement(orth_tr_t1960, orthographic_kernel__modernization_reading, theater_ratio, 1960, 0.22).

% Extraction over time
narrative_ontology:measurement(orth_be_t1928, orthographic_kernel__modernization_reading, base_extractiveness, 1928, 0.55).
narrative_ontology:measurement(orth_be_t1933, orthographic_kernel__modernization_reading, base_extractiveness, 1933, 0.5).
narrative_ontology:measurement(orth_be_t1938, orthographic_kernel__modernization_reading, base_extractiveness, 1938, 0.46).
narrative_ontology:measurement(orth_be_t1945, orthographic_kernel__modernization_reading, base_extractiveness, 1945, 0.42).
narrative_ontology:measurement(orth_be_t1952, orthographic_kernel__modernization_reading, base_extractiveness, 1952, 0.4).
narrative_ontology:measurement(orth_be_t1960, orthographic_kernel__modernization_reading, base_extractiveness, 1960, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(orth_su_t1928, orthographic_kernel__modernization_reading, suppression_requirement, 1928, 0.75).
narrative_ontology:measurement(orth_su_t1933, orthographic_kernel__modernization_reading, suppression_requirement, 1933, 0.68).
narrative_ontology:measurement(orth_su_t1938, orthographic_kernel__modernization_reading, suppression_requirement, 1938, 0.62).
narrative_ontology:measurement(orth_su_t1945, orthographic_kernel__modernization_reading, suppression_requirement, 1945, 0.58).
narrative_ontology:measurement(orth_su_t1952, orthographic_kernel__modernization_reading, suppression_requirement, 1952, 0.55).
narrative_ontology:measurement(orth_su_t1960, orthographic_kernel__modernization_reading, suppression_requirement, 1960, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_kernel__modernization_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(orthographic_kernel__modernization_reading, 0.1).
narrative_ontology:affects_constraint(orthographic_kernel__modernization_reading, orthographic_kernel__continuity_reading).
narrative_ontology:affects_constraint(orthographic_kernel__modernization_reading, orthographic_kernel__rupture_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the orthographic_kernel (Turkey's 1928 script reform), decomposed per the ε-invariance principle because the three readings assign structurally different ε, beneficiary sets, and victim sets to what a single natural-language label ('the alphabet reform') would otherwise flatten into one measurement. continuity_reading treats Arabic-script preservation as the lost coordination good and authors the reform as primarily extractive against cultural/religious continuity. rupture_reading treats deliberate identity severance as the primary function, with modernization as secondary cover, and would author higher suppression/extraction directed at Ottoman-identified populations. This modernization_reading authors moderate ε reflecting a genuinely corroborated literacy-coordination function alongside real, time-bounded transition costs. All three are linked bidirectionally via affects_constraints since they describe the same underlying historical event contested across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
