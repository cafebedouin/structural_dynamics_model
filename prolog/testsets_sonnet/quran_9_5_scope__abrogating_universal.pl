% ============================================================================
% CONSTRAINT STORY: quran_9_5_scope__abrogating_universal
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_9_5_scope__abrogating_universal, []).

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
 *   constraint_id: quran_9_5_scope__abrogating_universal
 *   human_readable: Abrogating-Universal Reading of Q9:5 (Ayat al-Sayf) as Standing Offensive Jihad Obligation
 *   domain: religious/legal/political
 *
 * SUMMARY:
 *   This story instantiates ONE reading of a contested kernel: the claim that
 *   Quran 9:5 (the 'Sword Verse') abrogates all prior peaceful and
 *   coexistence-oriented verses and establishes a standing, universal legal
 *   obligation of offensive jihad against non-Muslims absent their submission
 *   or conversion. This is not the only defensible reading of the same text —
 *   sibling readings (contextual_defensive, progressive_synthesis) are
 *   separate constraints with different ε values, different
 *   beneficiary/victim structures, and different classifications, linked via
 *   network.affects_constraints per the ε-invariance principle. This file
 *   evaluates only the abrogating-universal reading on its own structural
 *   terms: what it authorizes, who benefits from its adoption as doctrine,
 *   and who bears its costs when organized actors act on it.
 *
 * KEY AGENTS:
 *   - expansionist_jihadist_movements: primary agenda-setter and beneficiary (organized/arbitrage) — cites the doctrine as unconditional textual warrant
 *   - caliphal_conquest_states: institutional beneficiary (institutional/arbitrage) — historical and claimant-state material gain from conquest framed as religious duty
 *   - hardline_clerical_authorities_claiming_abrogation_monopoly: interpretive agenda-setter (institutional/mobile) — authority depends on the abrogation hierarchy being treated as settled
 *   - non_muslim_populations_absent_formal_submission: primary target/payer (powerless/trapped) — bears the doctrine's direct violent application
 *   - muslim_reformist_scholars_advocating_contextual_reading: excluded dissenting voice (moderate/constrained) — textually grounded objection systematically marginalized
 *   - classical_and_contemporary_jurists_observing_the_dispute: analytical observer — documents the centuries-long juristic dispute over naskh's actual scope
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_9_5_scope__abrogating_universal, 0.86).
domain_priors:suppression_score(quran_9_5_scope__abrogating_universal, 0.88).
domain_priors:theater_ratio(quran_9_5_scope__abrogating_universal, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, extractiveness, 0.86).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_9_5_scope__abrogating_universal, snare).
narrative_ontology:human_readable(quran_9_5_scope__abrogating_universal, "Abrogating-Universal Reading of Q9:5 (Ayat al-Sayf) as Standing Offensive Jihad Obligation").
narrative_ontology:topic_domain(quran_9_5_scope__abrogating_universal, "religious/legal/political").

domain_priors:requires_active_enforcement(quran_9_5_scope__abrogating_universal).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_9_5_scope__abrogating_universal, 'd0a88fb9-0567-4f88-8eef-cc85d30aef71').
narrative_ontology:cs_kernel_codification('d0a88fb9-0567-4f88-8eef-cc85d30aef71', fixed_text).
narrative_ontology:cs_authority_grounding('d0a88fb9-0567-4f88-8eef-cc85d30aef71', extraction).
narrative_ontology:cs_interpretation_layer_present('d0a88fb9-0567-4f88-8eef-cc85d30aef71').
narrative_ontology:cs_reading_relation('d0a88fb9-0567-4f88-8eef-cc85d30aef71', quran_9_5_scope__contextual_defensive, forecloses).
narrative_ontology:cs_reading_relation('d0a88fb9-0567-4f88-8eef-cc85d30aef71', quran_9_5_scope__progressive_synthesis, forecloses).
narrative_ontology:cs_axiom('d0a88fb9-0567-4f88-8eef-cc85d30aef71', foundational, abrogation_is_total_and_perpetual).
narrative_ontology:cs_axiom_status(abrogation_is_total_and_perpetual, holdable).
narrative_ontology:cs_axiom_grounding('d0a88fb9-0567-4f88-8eef-cc85d30aef71', abrogation_is_total_and_perpetual, conventional).
narrative_ontology:cs_axiom('d0a88fb9-0567-4f88-8eef-cc85d30aef71', foundational, polytheist_status_absent_submission_is_default_hostility).
narrative_ontology:cs_axiom_status(polytheist_status_absent_submission_is_default_hostility, holdable).
narrative_ontology:cs_axiom_grounding('d0a88fb9-0567-4f88-8eef-cc85d30aef71', polytheist_status_absent_submission_is_default_hostility, deontological).
narrative_ontology:cs_reference_frame('d0a88fb9-0567-4f88-8eef-cc85d30aef71', classical_expansionist_conquest_jurisprudence).
narrative_ontology:cs_drift_state('d0a88fb9-0567-4f88-8eef-cc85d30aef71', contemporary_international_legal_order, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('d0a88fb9-0567-4f88-8eef-cc85d30aef71', '').
narrative_ontology:cs_kernel_id(quran_9_5_scope__abrogating_universal, quran_9_5_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_9_5_scope__abrogating_universal, expansionist_jihadist_movements).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__abrogating_universal, caliphal_conquest_states).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__abrogating_universal, hardline_clerical_authorities_claiming_abrogation_monopoly).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, non_muslim_populations_absent_formal_submission).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, conquered_dhimmi_communities).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, muslim_reformist_scholars_advocating_contextual_reading).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, religious_minorities_in_contested_territories).
narrative_ontology:constraint_vindicates(quran_9_5_scope__abrogating_universal, doctrine_of_naskh_hierarchy).
narrative_ontology:constraint_vindicates(quran_9_5_scope__abrogating_universal, sword_verse_supremacy_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Cite this reading as textual warrant for offensive campaigns against non-Muslim populations, framing conquest as a standing religious obligation rather than a discretionary political choice. Control how the abrogation doctrine is taught and applied within their sphere; benefit from the reading's claim of eternal, unconditional legal force because it removes any negotiated limit on the scope or duration of their campaigns.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, expansionist_jihadist_movements, agenda_setter,
    organized, civilizational, arbitrage, global).

% Historically and in some contemporary claimant forms, use the abrogating-universal reading to justify territorial expansion and taxation-or-conversion policy toward non-Muslim populations as the fulfillment of religious law rather than conquest for its own sake. Legitimacy and material revenue (tribute, land, labor) flow from treating the obligation as perpetually binding.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, caliphal_conquest_states, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(quran_9_5_scope__abrogating_universal, caliphal_conquest_states, agenda_setter).

% Adjudicate which verses are abrogated and which stand, positioning themselves as the sole legitimate interpreters of naskh. Their authority and institutional standing depend on the abrogation hierarchy being treated as settled law rather than contested interpretation; a contextual or progressive reading would strip them of this interpretive monopoly.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, hardline_clerical_authorities_claiming_abrogation_monopoly, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(quran_9_5_scope__abrogating_universal, hardline_clerical_authorities_claiming_abrogation_monopoly, beneficiary).

% Under this reading, are cast as legitimate military targets by default unless they submit, convert, or pay tribute under subjugated status. Have no standing to negotiate coexistence on equal terms; their only recognized exits are submission, conversion, tribute-paying subordination, or armed resistance. Bear the direct cost of the doctrine's operation historically and wherever contemporary movements invoke it.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, non_muslim_populations_absent_formal_submission, payer,
    powerless, immediate, trapped, global).

% Live under subordinated legal status (jizya, restricted rights) as the 'accepted' alternative to continued warfare once conquest under this doctrine succeeds. Their reduced status is presented as the merciful alternative to the violence the doctrine authorizes, but the underlying threat of that violence is what produces their compliance.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, conquered_dhimmi_communities, payer,
    powerless, biographical, constrained, regional).

% Argue historically and textually that 9:5 addresses specific treaty-breaking Meccan polytheists and does not abrogate the Quran's numerous peaceful and pluralistic verses. Are frequently marginalized, accused of heterodoxy, or physically threatened by proponents of the abrogating-universal reading, and largely excluded from institutions that set official doctrine in regions where hardline authorities dominate.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, muslim_reformist_scholars_advocating_contextual_reading, excluded,
    moderate, generational, constrained, global).

% In territories where organized movements act on this reading, face displacement, forced conversion, or violence justified by the doctrine's claim of standing obligation. Have essentially no institutional recourse against a claim framed as unappealable divine law.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, religious_minorities_in_contested_territories, payer,
    powerless, immediate, trapped, regional).

% Document the centuries-long juristic dispute over naskh's scope and application, including the significant minority and majority positions among classical scholars themselves on whether 9:5 abrogates prior peaceful verses at all, and on what conditions (treaty violation, active hostility) actually trigger the verse's application.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, classical_and_contemporary_jurists_observing_the_dispute, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unifying, absolutist legal framework that removes ambiguity about the treatment of non-Muslims, allowing organized movements and states to mobilize followers around a single, unconditional textual mandate rather than negotiating case-by-case political and military decisions.
% TRANSFER_FUNCTION: Moves security, autonomy, land, and physical safety away from non-Muslim and dissenting populations toward expansionist authorities and the clerical establishments that certify the doctrine's legitimacy; also moves interpretive authority away from contextualist scholars toward those claiming the abrogation monopoly.
% ABSENT_VOICES: The vast body of classical and modern Quranic exegetes who read 9:5 as addressing specific treaty-breaking parties are structurally excluded from doctrinal authority wherever the abrogating-universal reading dominates institutional power; non-Muslim populations subject to the doctrine's application have no seat in its interpretive process at all.
% DISAPPEARANCE_RATIONALE: If this specific reading lost doctrinal force, movements and states currently citing it as legal warrant for offensive campaigns and subjugation of non-Muslims would lose their primary textual justification, materially altering the legal architecture used to authorize such campaigns; contextualist and progressive readings would gain relative institutional ground.
% FOUNDING_PROBLEM: Framed by its proponents as resolving ambiguity in the Quranic corpus about the correct posture toward polytheists after the Meccan treaty violations of the 7th century, and as establishing a clear, permanent legal doctrine rather than leaving Muslim rulers to negotiate ad hoc.
% FOUNDING_PROBLEM_CORROBORATION: Classical jurists outside the modern expansionist movements that invoke this reading — including substantial strands of Hanafi, Maliki, and Shafi'i classical scholarship, along with contemporary mainstream Islamic institutions (e.g., Al-Azhar statements, various fiqh councils) — attest that the historically specific 'founding problem' (treaty-breaking Meccan tribes) was resolved within the 7th century itself and that treating the verse as a perpetual universal mandate is a later interpretive move, not the plain original occasion (asbab al-nuzul) of the text.
narrative_ontology:disappearance_verdict(quran_9_5_scope__abrogating_universal, world_rearranges).
narrative_ontology:founding_problem_status(quran_9_5_scope__abrogating_universal, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_9_5_scope__abrogating_universal, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quran_9_5_scope__abrogating_universal, 'none', 1).
narrative_ontology:epsilon_provenance(quran_9_5_scope__abrogating_universal, 0.86, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_9_5_scope__abrogating_universal_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quran_9_5_scope__abrogating_universal, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quran_9_5_scope__abrogating_universal_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.86 at interval end) because the doctrine, when operationalized, authorizes direct appropriation of life, land, and autonomy from populations with no negotiated exit besides submission or tribute. Suppression is authored even higher (0.88) because persistence of this specific reading depends on actively delegitimizing and marginalizing the substantial classical and contemporary scholarly tradition that reads 9:5 as addressing a specific historical conflict rather than as perpetual command — suppression here is a raw structural property of how the reading maintains itself against contrary textual and juristic evidence, not scaled by scope. Theater ratio is moderate (0.3): some invocation is genuinely operational (organized violence follows from the doctrine) rather than purely performative, though a portion of clerical certification activity functions as legitimating theater for decisions made on other grounds (political, economic, strategic).
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seats (expansionist movements, claimant conquest states, hardline clerical authorities) this reading presents as settled, authoritative religious law resolving textual ambiguity. From the payer seats (non-Muslim populations, dhimmi communities, excluded reformist scholars) the identical structure operates as an unappealable warrant for violence and subjugation with no negotiated standing. The engine computes these as different seat-level types from the same structural data; the divergence is the finding, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (expansionist movements, conquest states, monopoly-claiming clerical authorities) map to genuine structural gain: territorial, revenue, and authority gains flow to them specifically from the doctrine being treated as universally and perpetually binding. Victim declarations (non-Muslim populations, dhimmi communities, reformist scholars) map to genuine structural cost: they either face direct violence/subjugation or professional/physical marginalization for contesting the doctrine. Non-Muslim populations sit at trapped exit because the doctrine as authored recognizes no negotiated coexistence status prior to submission — this is precisely the expected structural delta for this reading versus its siblings.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem framing (resolving 7th-century treaty violations by specific Meccan polytheist tribes) is authored as contested and status-dead-per-corroborating-outside-sources: substantial classical and contemporary Islamic scholarship outside the movements that benefit from this reading attests the original occasion was historically bounded and resolved within the 7th century. The doctrine's persistence as a claimed perpetual, universal obligation despite this corroborated obsolescence is a textbook founding-problem-status mismatch (dead problem, world_rearranges verdict) — flagging exactly the capture/zombie pattern the R5 genealogy interview exists to surface, distinguishing this from a case where the coordination function genuinely remains live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naskh_doctrine_scope_contest,
    'Does the classical Islamic legal concept of naskh (abrogation) actually apply to 9:5 in the sweeping, universal sense claimed by this reading, or is the abrogation claim itself a later interpretive overlay not supported by the majority of classical exegetical method?',
    'Comparative textual-historical analysis of asbab al-nuzul (occasions of revelation) literature, cross-referencing early tafsir (Ibn Abbas, Tabari) against later juristic abrogation catalogs, and examining the internal consistency of claimed abrogated verses with verses revealed after 9:5.',
    'If the abrogation claim is a later doctrinal construction rather than an original exegetical consensus, this reading''s central textual premise collapses and the sibling contextual_defensive reading gains substantially stronger textual-historical grounding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naskh_doctrine_scope_contest, empirical, 'Whether the naskh mechanism genuinely applies to 9:5 at the scope this reading claims.').

omega_variable(
    which_reading_is_operative_where,
    'In any given historical or contemporary context, which of the three sibling readings (abrogating_universal, contextual_defensive, progressive_synthesis) is actually the operative doctrine driving observed behavior, versus which is cited post-hoc to legitimate decisions made on political, economic, or strategic grounds?',
    'Case-by-case historical and organizational analysis distinguishing doctrinal causation from doctrinal justification — did the reading precede and drive the action, or was the action decided first and the reading invoked afterward?',
    'If this reading functions primarily as post-hoc legitimation rather than as a genuine causal driver, the theater_ratio for this constraint should be revised substantially upward, and the true causal weight shifts toward political/economic constraints not modeled here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(which_reading_is_operative_where, conceptual, 'Whether the doctrine causes the extraction it is credited/blamed for, or merely legitimates extraction decided elsewhere.').

omega_variable(
    corroboration_source_bias,
    'Do the classical and contemporary scholarly sources cited as corroborating the founding_problem''s ''dead'' status themselves carry institutional incentives (state patronage, minority-protection advocacy, interfaith-relations funding) that could bias them toward the contextualist reading regardless of the text''s actual original meaning?',
    'Cross-check corroborating sources'' institutional funding and political context against their exegetical conclusions; compare with exegetes operating under no comparable incentive structure, if any can be identified.',
    'If corroborating sources are substantially incentive-biased, the founding_problem_status mismatch this story relies on for its mandatrophy analysis is weaker than claimed, and the abrogating_universal reading''s self-account should be weighted somewhat more heavily.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(corroboration_source_bias, empirical, 'Possible bias in the sources corroborating this reading''s founding-problem obsolescence claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_9_5_scope__abrogating_universal, 632, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t632, quran_9_5_scope__abrogating_universal, theater_ratio, 632, 0.15).
narrative_ontology:measurement(qura_tr_t900, quran_9_5_scope__abrogating_universal, theater_ratio, 900, 0.2).
narrative_ontology:measurement(qura_tr_t1250, quran_9_5_scope__abrogating_universal, theater_ratio, 1250, 0.25).
narrative_ontology:measurement(qura_tr_t1600, quran_9_5_scope__abrogating_universal, theater_ratio, 1600, 0.28).
narrative_ontology:measurement(qura_tr_t1900, quran_9_5_scope__abrogating_universal, theater_ratio, 1900, 0.22).
narrative_ontology:measurement(qura_tr_t1980, quran_9_5_scope__abrogating_universal, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(qura_tr_t2001, quran_9_5_scope__abrogating_universal, theater_ratio, 2001, 0.32).
narrative_ontology:measurement(qura_tr_t2024, quran_9_5_scope__abrogating_universal, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(qura_be_t632, quran_9_5_scope__abrogating_universal, base_extractiveness, 632, 0.55).
narrative_ontology:measurement(qura_be_t900, quran_9_5_scope__abrogating_universal, base_extractiveness, 900, 0.68).
narrative_ontology:measurement(qura_be_t1250, quran_9_5_scope__abrogating_universal, base_extractiveness, 1250, 0.62).
narrative_ontology:measurement(qura_be_t1600, quran_9_5_scope__abrogating_universal, base_extractiveness, 1600, 0.5).
narrative_ontology:measurement(qura_be_t1900, quran_9_5_scope__abrogating_universal, base_extractiveness, 1900, 0.45).
narrative_ontology:measurement(qura_be_t1980, quran_9_5_scope__abrogating_universal, base_extractiveness, 1980, 0.6).
narrative_ontology:measurement(qura_be_t2001, quran_9_5_scope__abrogating_universal, base_extractiveness, 2001, 0.82).
narrative_ontology:measurement(qura_be_t2024, quran_9_5_scope__abrogating_universal, base_extractiveness, 2024, 0.86).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t632, quran_9_5_scope__abrogating_universal, suppression_requirement, 632, 0.5).
narrative_ontology:measurement(qura_su_t900, quran_9_5_scope__abrogating_universal, suppression_requirement, 900, 0.65).
narrative_ontology:measurement(qura_su_t1250, quran_9_5_scope__abrogating_universal, suppression_requirement, 1250, 0.6).
narrative_ontology:measurement(qura_su_t1600, quran_9_5_scope__abrogating_universal, suppression_requirement, 1600, 0.5).
narrative_ontology:measurement(qura_su_t1900, quran_9_5_scope__abrogating_universal, suppression_requirement, 1900, 0.45).
narrative_ontology:measurement(qura_su_t1980, quran_9_5_scope__abrogating_universal, suppression_requirement, 1980, 0.62).
narrative_ontology:measurement(qura_su_t2001, quran_9_5_scope__abrogating_universal, suppression_requirement, 2001, 0.85).
narrative_ontology:measurement(qura_su_t2024, quran_9_5_scope__abrogating_universal, suppression_requirement, 2024, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_9_5_scope__abrogating_universal, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(quran_9_5_scope__abrogating_universal, 0.05).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, quran_9_5_scope__contextual_defensive).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, quran_9_5_scope__progressive_synthesis).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the quran_9_5_scope kernel. quran_9_5_scope__contextual_defensive treats 9:5 as addressing specific treaty-breaking Medinan polytheist tribes without abrogating peaceful verses (materially lower ε, victims limited to treaty-violating combatants rather than all non-Muslims). quran_9_5_scope__progressive_synthesis treats the verse as a time-bound political directive superseded by the Quran's broader ethical trajectory (lowest ε of the three, near-rope coordination framing around interfaith ethics). All three share the same underlying kernel text but instantiate structurally distinct constraints with different beneficiary/victim sets, different suppression profiles, and different classifications — they are not the same constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
