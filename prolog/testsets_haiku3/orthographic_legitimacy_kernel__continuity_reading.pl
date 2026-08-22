% ============================================================================
% CONSTRAINT STORY: orthographic_legitimacy_kernel__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_legitimacy_kernel__continuity_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: orthographic_legitimacy_kernel__continuity_reading
 *   human_readable: Orthographic Continuity: Script Access to Pre-Reform Tradition
 *   domain: political/linguistic/cultural
 *
 * SUMMARY:
 *   Between roughly 1928 and the present, a nation-state reformed its primary
 *   orthography from Arabic script to Latin-based characters, mandating the
 *   new script in education, administration, and public life. The continuity
 *   reading claims that this reform, while delivering efficiency and
 *   administrative gains, created an orthographic wall severing post-reform
 *   cohorts from direct access to the pre-reform corpus—religious texts,
 *   classical literature, historical records, legal precedents—that
 *   constitute the nation's accumulated spiritual and intellectual tradition.
 *   The constraint that emerges, under the continuity reading, is not the
 *   reform itself (which was a rational policy choice) but the ongoing
 *   requirement to read continuity with the past through the new script only,
 *   which makes direct textual access impossible for most citizens and thus
 *   severs them from unmediated tradition. The reading frames this as a
 *   Mountain (orthographic incompatibility is a physical fact) with a victim
 *   (post-reform generations) and a beneficiary (those who retain or learn
 *   Arabic script literacy). This is deliberately distinguished from the
 *   other readings: the instrumentalist reading treats the script change as
 *   solving an efficiency problem with no victims (all cohorts gain from
 *   faster literacy), and the modernist reading treats it as a feature—a
 *   healthy rupture from a colonial/Ottoman past.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_legitimacy_kernel__continuity_reading, 0.25).
domain_priors:suppression_score(orthographic_legitimacy_kernel__continuity_reading, 0.72).
domain_priors:theater_ratio(orthographic_legitimacy_kernel__continuity_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_legitimacy_kernel__continuity_reading, mountain).
narrative_ontology:human_readable(orthographic_legitimacy_kernel__continuity_reading, "Orthographic Continuity: Script Access to Pre-Reform Tradition").
narrative_ontology:topic_domain(orthographic_legitimacy_kernel__continuity_reading, "political/linguistic/cultural").

domain_priors:emerges_naturally(orthographic_legitimacy_kernel__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_legitimacy_kernel__continuity_reading, 'c4e39562-5ad9-4be6-9344-b37905f8631e').
narrative_ontology:cs_kernel_codification('c4e39562-5ad9-4be6-9344-b37905f8631e', fixed_text).
narrative_ontology:cs_authority_grounding('c4e39562-5ad9-4be6-9344-b37905f8631e', lineage).
narrative_ontology:cs_interpretation_layer_present('c4e39562-5ad9-4be6-9344-b37905f8631e').
narrative_ontology:cs_reading_relation('c4e39562-5ad9-4be6-9344-b37905f8631e', orthographic_legitimacy_kernel__instrumentalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('c4e39562-5ad9-4be6-9344-b37905f8631e', orthographic_legitimacy_kernel__modernist_reading, coexists_with).
narrative_ontology:cs_axiom('c4e39562-5ad9-4be6-9344-b37905f8631e', foundational, direct_textual_access_constitutes_legitimacy).
narrative_ontology:cs_axiom_status(direct_textual_access_constitutes_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('c4e39562-5ad9-4be6-9344-b37905f8631e', direct_textual_access_constitutes_legitimacy, deontological).
narrative_ontology:cs_axiom('c4e39562-5ad9-4be6-9344-b37905f8631e', secondary, tradition_transmission_requires_unmediated_reading).
narrative_ontology:cs_axiom_status(tradition_transmission_requires_unmediated_reading, holdable).
narrative_ontology:cs_axiom_grounding('c4e39562-5ad9-4be6-9344-b37905f8631e', tradition_transmission_requires_unmediated_reading, instrumental).
narrative_ontology:cs_reference_frame('c4e39562-5ad9-4be6-9344-b37905f8631e', unbroken_scriptural_transmission).
narrative_ontology:cs_drift_state('c4e39562-5ad9-4be6-9344-b37905f8631e', post_orthographic_reform_contemporaneity, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('c4e39562-5ad9-4be6-9344-b37905f8631e', '').
narrative_ontology:cs_kernel_id(orthographic_legitimacy_kernel__continuity_reading, orthographic_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__continuity_reading, arabic_script_readers).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__continuity_reading, post_reform_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__continuity_reading, religious_scholars_post_reform).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__continuity_reading, religious_scholars_post_reform).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The class of Islamic scholars and jurists trained in Arabic script who produced, transmitted, and validated the religious, legal, and literary corpus from which a nation's entire spiritual-intellectual framework derives. Their authority rests on unbroken lineage of transmission through texts in Arabic orthography. Once reform happens, they become historical reference points whose work is inaccessible to the post-reform cohort unless that cohort learns the old script separately—a cost the reform was designed to eliminate.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, pre_reform_ulama, observer,
    institutional, generational, analytical, national).

% Any cohort that retains Arabic script literacy (whether through private education, religious institution, or deliberate preservation effort). They can directly access the entire pre-reform corpus—Quran, hadith, jurisprudential tradition, classical literature, historical record—without translation mediation. Their access to tradition is uninterrupted; they inherit the full legitimacy chain.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, arabic_script_readers, beneficiary,
    powerful, generational, arbitrage, national).

% Cohorts educated entirely in the new orthography who comprise the bulk of the reformed nation-state's literate population. They cannot read pre-1928 texts directly. Access to religious, legal, and literary tradition requires either learning the old script (expensive, time-consuming, identity-displacing) or consuming mediated translations and interpretations (introducing selection and interpretation bias). Their identity as members of the reformed nation is constituted through the new script; learning the old script means partially rupturing that identity. They bear the cost of severed direct access.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, post_reform_generations, payer,
    organized, biographical, identity_locked, national).

% The state authority (government, educational bureaucracy, cultural establishment) that mandates, enforces, and administers the orthographic transition. Justifies the reform through efficiency, mass literacy, administrative streamlining, and alignment with 'modern' standards. Maintains the new orthography through educational policy, official documentation, and public institutional practice. Could theoretically reverse or accommodate bilingual provision but does not, on grounds that doing so would undermine the reform's intended effects.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, reform_authority, agenda_setter,
    institutional, civilizational, analytical, national).

% Factions (whether modernist or instrumentalist) that endorse the reform on grounds other than continuity preservation—those who see the old script as an Ottoman relic, or as an impediment to rapid literacy, or as symbolically bound to a past that must be ruptured. They would argue for permanent discontinuity and against maintaining bilingual or parallel-literacy infrastructure. Their disagreement with the continuity reading means they are excluded from the conversation about preserving access to tradition.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, competing_reform_readings, excluded,
    moderate, generational, constrained, national).

% Post-reform religious scholars (imams, muftis, Islamic educators) who adopt the new orthography while seeking to maintain doctrinal continuity with pre-reform tradition. They retain higher script literacy than the general population, but face the interpretive burden of mediating between texts available only in one script and a constituency literate only in another. They benefit from participating in the scholarly tradition but pay the cost of serving as the sole bridge to direct sources.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, religious_scholars_post_reform, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(orthographic_legitimacy_kernel__continuity_reading, religious_scholars_post_reform, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserving a continuous, unmediated reading path from post-reform individuals and institutions back to the full corpus of religious, legal, literary, and historical tradition—a path of direct textual access that requires no translation gate or interpretive intermediary.
% TRANSFER_FUNCTION: Access to historical tradition is transferred from direct comprehension (pre-reform cohorts reading original texts) to mediated comprehension (post-reform cohorts reading translations, excerpts, or re-narrated interpretations filtered through bridge scholars). The original texts remain readable only by those who invest in learning the old script separately—a minority. The majority inherits tradition only through selective reinterpretation.
% ABSENT_VOICES: The pre-reform cohorts themselves (the ulama, scribes, jurists, poets who produced the tradition) are literally absent—their voices are silenced not by exclusion from deliberation but by orthographic death. Those living post-reform who would argue for bilingual literacy provision—parallel instruction in both scripts to maintain access—are marginalized as reactionary or inefficient by the reform authority and its aligned constituencies.
% DISAPPEARANCE_RATIONALE: If the constraint (the requirement to read pre-1928 texts in their original script) disappeared—i.e., if all pre-reform texts were systematized translated into the new script—the world would NOT rearrange politically or administratively (the new orthography continues), BUT the continuity reading's entire claim would evaporate: there would be no 'loss' of access, because access would be restored by translation. The contested part: whether that translation-restoration is functionally equivalent to original-script access or a constitutive rupture. The continuity reading says rupture; the instrumentalist reading says equivalence; the modernist reading says intentional rupture is a feature, not a bug.
% FOUNDING_PROBLEM: A nation-state sought to consolidate identity, increase literacy rates, and align with 'modern' European standards by shifting from a script tied to Ottoman and Islamic institutional history to one that could be taught more rapidly and would mark a fresh national beginning. The old script was seen as a barrier to the literacy and efficiency the new nation-state required.
% FOUNDING_PROBLEM_CORROBORATION: The reform authority and modernist scholars attest the founding problem was real and has been solved: literacy rates rose, administrative efficiency improved, national identity consolidated around the new script. Post-reform generations took the new orthography as a given. The continuity reading's counter-attestation comes from religious scholars, archivists, and cultural historians (outside the benefiting reform coalition) who note that while the founding efficiency problem is solved, a NEW problem has been created: post-reform generations cannot read the corpus that defined the nation's spiritual and intellectual identity. This is not a recovery of the original problem; it is a NEW problem generated by the solution.
narrative_ontology:disappearance_verdict(orthographic_legitimacy_kernel__continuity_reading, contested).
narrative_ontology:founding_problem_status(orthographic_legitimacy_kernel__continuity_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_legitimacy_kernel__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(orthographic_legitimacy_kernel__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_legitimacy_kernel__continuity_reading, 0.25, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_legitimacy_kernel__continuity_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, ExtMetricName, E),
    domain_priors:suppression_score(orthographic_legitimacy_kernel__continuity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(orthographic_legitimacy_kernel__continuity_reading),
    narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(orthographic_legitimacy_kernel__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness is LOW (0.25) because the constraint is rooted in orthographic fact: script incompatibility is not a socially engineered extraction mechanism but a structural property of written language—you cannot read Arabic script without learning it, just as you cannot read Latin script without learning it. The mountain claim reflects this: the incompatibility persists regardless of anyone's intentions. HOWEVER, the suppression is HIGH (0.72) because the reform authority actively enforces the new orthography through education policy, administrative practice, and symbolic endorsement, making it difficult or impossible for post-reform cohorts to learn the old script even if they wished. The accessibility_collapse is very high (0.88): once the new script is the sole medium of instruction, alternatives (bilingual education, parallel script instruction, open archival access) collapse almost entirely—they become private, expensive, or symbolically marked as backward. The resistance is substantial (0.68): religious scholars, cultural conservatives, and archivists continuously argue for script preservation or bilingual literacy, generating institutional and intellectual resistance to the reform's totality. Theater rises over time (from 0.18 to 0.41) as the efficiency justification for the reform fades into background—the state continues enforcing monolingual new-script education not primarily because efficiency gains are still accruing but because reversing the reform would delegitimize the entire post-reform institutional structure. Extractiveness dips slightly in later periods (0.25 by T=100) as the constraint becomes normalized: there is no active 'extraction' happening, only the ongoing structural fact that post-reform cohorts cannot read their own pre-reform tradition without external investment.
 *
 * PERSPECTIVAL GAP:
 *   The reform authority (agenda_setter) experiences the constraint as a solved problem: the nation is literate, administrative efficient, unified around a single script. The continuity reading's claim that this creates victims is seen as reactionary nostalgia. Post-reform generations (payers) experience the constraint as invisible—they have never had access to the old texts, so they do not perceive loss. But the small cohort of post-reform scholars who HAVE learned Arabic script (the bridge scholars) experience the constraint as acutely extractive: they must do double work, maintaining two literacies, to serve as the sole bridge between the majority and the tradition. The pre-reform ulama (now deceased or extremely aged) experienced the constraint as catastrophic rupture: their life's work became inaccessible to their intellectual heirs. The continuity reading's claim to victim status is weakest where it is most true: the cohorts most victimized (pre-reform scholars, early post-reform cohorts who lived through the transition) are dead or too few to testify. The constraint's extractiveness appears low precisely because the victims have been displaced in time.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiaries (arabic_script_readers) have low directionality (d ≈ 0.2): they benefit from retaining access but did not cause the constraint and do not need to defend it actively—retention is passive. The victims (post_reform_generations) have high directionality (d ≈ 0.85): they bear the cost of severed access without choosing it, and identity-lock intensifies this (leaving the nation-state means abandoning the language itself, not just the script). The reform_authority has mixed directionality: they benefit from the constraint's existence (it consolidates identity, proves administrative control) and must actively suppress alternative script provision, placing them at d ≈ 0.15 from the beneficiary side—they are not fully captured by the constraint; they maintain it strategically. The directionality derivation chain: beneficiaries declare as arbitrage-exit (they can learn the new script if they wish but choose to retain the old), victims declare as identity-locked (they are constituted as post-reform subjects by the new script and cannot exit without existential rupture). No override needed; the structural data produces coherent directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The continuity reading navigates a subtle mandatrophy boundary. The founding problem (create efficient literacy, unify the nation, assert independence from Ottoman orthography) is DEAD—it has been solved, and the efficiency gains are real. However, a new problem has been created (severance from pre-reform tradition), which the continuity reading argues was not recognized as a cost at reform time. The constraint does NOT show classic mandatrophy (a structure whose original function has atrophied and is now maintained by inertia alone)—the reform authority remains actively invested in enforcing monolingual new-script education. Instead, the constraint shows what might be called 'transferred mandatrophy': the original justification (efficiency, literacy speed) no longer drives enforcement (because those gains are realized and stable), but enforcement continues because reversing it would delegitimize the entire institutional architecture built on the new script. The theater_ratio rising over time (from 0.18 to 0.41) captures this shift: early enforcement was functional (teaching efficiency matters), later enforcement is performative (maintaining the symbolic unity the reform created). This is NOT piton-grade theatrical maintenance—the constraint still serves real institutional interests—but it is close to the boundary. The continuity reading's claim is that mandatrophy-like dynamics are unfolding: the efficiency mandate is dead, yet the enforcement persists and hardens, suggesting the constraint is becoming increasingly extractive (benefiting those who retain script access) rather than coordinative (solving a collective literacy problem).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturalness_vs_choice_ambiguity,
    'Is the orthographic incompatibility that severs post-reform cohorts from pre-reform texts a natural law (script incompatibility is inherently physical) or a constructed constraint that could be reversed or mediated through bilingual education and archival access?',
    'Historical counterfactual analysis: if any nation-state had implemented bilingual script education post-reform while maintaining new-script administrative dominance, would post-reform cohorts have gained meaningful access to pre-reform texts? Alternatively, comparative analysis: do post-reform cohorts in societies that maintained bilingual literacy show different access patterns or cultural outcomes?',
    'If the constraint is natural law (orthographic incompatibility is brute fact), the mountain classification holds and victimization is unavoidable. If it is constructed (policy choice to enforce monolingual education, exclusion of bilingual pathways), the classification shifts toward snare or tangled_rope—the constraint persists because enforcing it benefits the reform authority and aligned constituencies, not because it is inevitable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(naturalness_vs_choice_ambiguity, empirical, 'Whether orthographic incompatibility is natural or constructed by enforcement choices.').

omega_variable(
    continuity_reading_vs_sibling_readings,
    'Which reading of the orthographic legitimacy kernel is structurally true: continuity (legitimacy requires unmediated access to pre-reform tradition), instrumentalist (legitimacy derives from mass literacy and administrative efficiency), or modernist (legitimacy derives from rupture with Ottoman past)?',
    'Empirical test: survey post-reform cohorts in societies with varying script policies on their sense of cultural legitimacy and connection to pre-reform tradition; correlate against literacy rates and administrative efficiency metrics. Alternatively, genealogical analysis: trace which framing was actually operative in reform decisions and institutional design.',
    'If continuity reading is correct, the constraint shows false-summit dynamics: a mountain claimed by the reform authority but actually a tangled-rope or snare benefiting script-literate elites at the cost of post-reform cohorts. If instrumentalist reading is correct, the constraint is functionally a rope (coordination solution with some asymmetric benefit to literacy pioneers). If modernist reading is correct, the constraint is a temporary scaffold that has become performative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(continuity_reading_vs_sibling_readings, conceptual, 'Which kernel reading reflects the actual structure of orthographic legitimacy in the nation-state.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the high suppression (0.72) structural (bilingual education is expensive, administratively difficult, excluded by policy) or internalized (post-reform cohorts have internalized the view that learning the old script is backward, unnecessary, or identity-betraying)?',
    'Post-suppression trajectory analysis: if access to Arabic script instruction were suddenly and cheaply available, would post-reform cohorts take it up? Or has the suppression been so sustained that it has become internalized as identity, making reversal psychologically difficult even after structural barriers lift?',
    'If suppression is structural, removing bilingual education bans and funding script instruction would restore access. If suppression is internalized, structural changes alone would not reverse the constraint; cognitive/identity work would be required. Internalization would indicate a deeper form of victimization than structural suppression alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of script access is structural policy or internalized identity barrier.').

omega_variable(
    false_summit_candidate,
    'Is the orthographic continuity constraint a genuine mountain (physical incompatibility of scripts) or a false summit—a constructed extraction mechanism benefiting script-literate elites and presented as natural law?',
    'Examine whether beneficiary groups (arabic_script_readers, bridge scholars, pre-reform institutional continuities) actively defend the constraint against bilingual alternatives, and whether they deploy ''naturalness'' rhetoric (you cannot read two scripts, literacy requires monolingual focus) to prevent policy changes that would erode their access monopoly.',
    'If false summit: FSM triggers, and the constraint reclassifies from mountain toward tangled_rope or snare, with culpability shifting to the beneficiary coalition that maintains the fiction of naturalness. This would require examining coupling between script retention and other status/power markers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_candidate, empirical, 'Whether naturalness claim masks extractive beneficiary interest in maintaining script monopoly.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_legitimacy_kernel__continuity_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_tr_t0, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(orth_tr_t0, observed).
narrative_ontology:measurement(orth_tr_t12, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement_basis(orth_tr_t12, observed).
narrative_ontology:measurement(orth_tr_t25, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 25, 0.31).
narrative_ontology:measurement_basis(orth_tr_t25, observed).
narrative_ontology:measurement(orth_tr_t50, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 50, 0.4).
narrative_ontology:measurement_basis(orth_tr_t50, observed).
narrative_ontology:measurement(orth_tr_t75, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 75, 0.41).
narrative_ontology:measurement_basis(orth_tr_t75, observed).
narrative_ontology:measurement(orth_tr_t100, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 100, 0.41).
narrative_ontology:measurement_basis(orth_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(orth_be_t0, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement_basis(orth_be_t0, observed).
narrative_ontology:measurement(orth_be_t12, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 12, 0.18).
narrative_ontology:measurement_basis(orth_be_t12, observed).
narrative_ontology:measurement(orth_be_t25, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 25, 0.24).
narrative_ontology:measurement_basis(orth_be_t25, observed).
narrative_ontology:measurement(orth_be_t50, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 50, 0.26).
narrative_ontology:measurement_basis(orth_be_t50, observed).
narrative_ontology:measurement(orth_be_t75, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 75, 0.25).
narrative_ontology:measurement_basis(orth_be_t75, observed).
narrative_ontology:measurement(orth_be_t100, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 100, 0.25).
narrative_ontology:measurement_basis(orth_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(orth_su_t0, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(orth_su_t0, observed).
narrative_ontology:measurement(orth_su_t12, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 12, 0.64).
narrative_ontology:measurement_basis(orth_su_t12, observed).
narrative_ontology:measurement(orth_su_t25, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 25, 0.69).
narrative_ontology:measurement_basis(orth_su_t25, observed).
narrative_ontology:measurement(orth_su_t50, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 50, 0.71).
narrative_ontology:measurement_basis(orth_su_t50, observed).
narrative_ontology:measurement(orth_su_t75, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 75, 0.72).
narrative_ontology:measurement_basis(orth_su_t75, observed).
narrative_ontology:measurement(orth_su_t100, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 100, 0.72).
narrative_ontology:measurement_basis(orth_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_legitimacy_kernel__continuity_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(orthographic_legitimacy_kernel__continuity_reading, 0.25).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__continuity_reading, orthographic_legitimacy_kernel__instrumentalist_reading).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__continuity_reading, orthographic_legitimacy_kernel__modernist_reading).

% DUAL FORMULATION NOTE:
% The orthographic legitimacy kernel decomposes into three structurally distinct constraint stories, each instantiating a different reading of what legitimacy MEANS in the orthographic domain. The continuity reading (this file) claims legitimacy requires access to historical tradition. The instrumentalist reading claims legitimacy derives from mass literacy and administrative efficiency (efficiency and access are inversely correlated given limited resources). The modernist reading claims legitimacy derives from rupture with Ottoman/Islamic tradition (continuity and rupture are logically incompatible as primary legitimacy vectors). These are not different views of the same constraint; they are different constraints rooted in the same kernel. Each has its own ε (low for continuity, very low for instrumentalist, moderate for modernist), its own victim/beneficiary structure, and its own cs_structure/axioms specifying the foundational commitments. Linking all three enables comparative analysis of how different readings of the same formal commitment (the nation-state's appeal to legitimacy) produce different classifications and different identified victims/beneficiaries.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
