% ============================================================================
% CONSTRAINT STORY: script_as_identity__ottoman_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_script_as_identity__ottoman_continuity_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: script_as_identity__ottoman_continuity_reading
 *   human_readable: Arabic Script as Constitutive of Turkish-Islamic Identity and Ottoman Continuity
 *   domain: political/cultural/linguistic
 *
 * SUMMARY:
 *   After the Ottoman Empire's collapse in 1923, the Turkish nation-state
 *   faced a succession crisis: on what grounds does the new regime claim
 *   legitimacy and continuity with the imperial past? Ottoman institutional
 *   memory and Islamic scholarship were encoded in Arabic script across
 *   centuries of governance, jurisprudence, and cultural transmission. The
 *   constraint instantiates one reading of a contested kernel: that Arabic
 *   script is constitutive of Turkish-Islamic identity and the legitimate
 *   link to Ottoman institutional history. This reading defends script
 *   continuity as a necessity for maintaining both religious authority
 *   (Islamic jurisprudence requires Arabic mastery) and state institutional
 *   legitimacy (succession to Ottoman administration requires script
 *   continuity). The measurement series tracks a slow escalation in
 *   enforcement suppression (from 0.68 to 0.79 over the interval) while
 *   extractiveness stabilizes (plateaus at 0.68 by interval end), suggesting
 *   enforcement hardening without corresponding increase in perceived
 *   necessity — a signature of theater_ratio drift. The constraint is CLAIMED
 *   as tangled_rope because it coordinates institutional continuity (real
 *   coordination function) while extracting exclusivity from
 *   non-Arabic-speaking populations (asymmetric cost). Sibling readings
 *   (kemalist_rupture_reading, phonetic_instrumentalism_reading) offer
 *   structurally different interpretations of the same kernel: the kemalist
 *   reading frames script as an obstacle to modernization (script change as
 *   liberation); the phonetic reading frames script as a neutral technology
 *   choice (script as efficiency problem, not identity problem). This
 *   constraint is one reading of that contest.
 *
 * KEY AGENTS:
 *   - ottoman_institutional_inheritors: State institutions claiming legitimacy through Ottoman succession; defend script as preservation of institutional memory and authority lineage.
 *   - islamic_religious_authority_structure: Religious scholars and legal authorities (mufti class) whose expertise and authority are inseparable from Arabic script mastery; beneficiary from script-identity fusion.
 *   - secular_modernizers: State reformers and technical administrators seeking rapid literacy and modernization via Latin script; see Arabic script as obstacle to their institutional project.
 *   - phonetically_optimized_literacy_advocates: Linguists and pedagogues arguing for Latin script on evidence of superior phonetic fit to Turkish vowel harmony; carry expert claims excluded from the identity debate.
 *   - non_arabic_speaking_populations: Majority populations facing learning barriers to institutional and religious participation due to script requirement; powerless victims with no vote in legitimacy claims.
 *   - ottoman_archive_custodians: Institutional actors whose expertise and funding depend on continued need to preserve and interpret Arabic-script records; beneficiary from constraint persistence.
 *   - secular_educational_reformers: Excluded actors who would propose mass literacy systems; blocked from the script debate by framing it as identity question rather than technical problem.
 *   - religious_modernizers: Excluded Islamic thinkers who might partition the script question; cannot propose script pluralism without appearing to betray religious continuity.
 *   - comparative_linguistic_observers: Analytical observers measuring the constraint's operation and costs without power to alter it; witness the fusion of script and identity.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(script_as_identity__ottoman_continuity_reading, 0.68).
domain_priors:suppression_score(script_as_identity__ottoman_continuity_reading, 0.79).
domain_priors:theater_ratio(script_as_identity__ottoman_continuity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(script_as_identity__ottoman_continuity_reading, tangled_rope).
narrative_ontology:human_readable(script_as_identity__ottoman_continuity_reading, "Arabic Script as Constitutive of Turkish-Islamic Identity and Ottoman Continuity").
narrative_ontology:topic_domain(script_as_identity__ottoman_continuity_reading, "political/cultural/linguistic").

domain_priors:requires_active_enforcement(script_as_identity__ottoman_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(script_as_identity__ottoman_continuity_reading, 'ab7e1ba0-433d-49d0-b687-13d01c3f17f5').
narrative_ontology:cs_kernel_codification('ab7e1ba0-433d-49d0-b687-13d01c3f17f5', fixed_text).
narrative_ontology:cs_authority_grounding('ab7e1ba0-433d-49d0-b687-13d01c3f17f5', lineage).
narrative_ontology:cs_interpretation_layer_present('ab7e1ba0-433d-49d0-b687-13d01c3f17f5').
narrative_ontology:cs_reading_relation('ab7e1ba0-433d-49d0-b687-13d01c3f17f5', script_as_identity__kemalist_rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('ab7e1ba0-433d-49d0-b687-13d01c3f17f5', script_as_identity__phonetic_instrumentalism_reading, influences).
narrative_ontology:cs_axiom('ab7e1ba0-433d-49d0-b687-13d01c3f17f5', foundational, script_identity_fusion_necessity).
narrative_ontology:cs_axiom_status(script_identity_fusion_necessity, holdable).
narrative_ontology:cs_axiom_grounding('ab7e1ba0-433d-49d0-b687-13d01c3f17f5', script_identity_fusion_necessity, deontological).
narrative_ontology:cs_axiom('ab7e1ba0-433d-49d0-b687-13d01c3f17f5', foundational, ottoman_institutional_continuity_requires_arabic_script).
narrative_ontology:cs_axiom_status(ottoman_institutional_continuity_requires_arabic_script, holdable).
narrative_ontology:cs_axiom_grounding('ab7e1ba0-433d-49d0-b687-13d01c3f17f5', ottoman_institutional_continuity_requires_arabic_script, empirically_contingent).
narrative_ontology:cs_reference_frame('ab7e1ba0-433d-49d0-b687-13d01c3f17f5', ottoman_institutional_legitimacy).
narrative_ontology:cs_drift_state('ab7e1ba0-433d-49d0-b687-13d01c3f17f5', contemporary_secular_modernization_pressure, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ab7e1ba0-433d-49d0-b687-13d01c3f17f5', '').
narrative_ontology:cs_kernel_id(script_as_identity__ottoman_continuity_reading, script_as_identity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(script_as_identity__ottoman_continuity_reading, ottoman_institutional_inheritors).
narrative_ontology:constraint_beneficiary(script_as_identity__ottoman_continuity_reading, islamic_religious_authority_structure).
narrative_ontology:constraint_victim(script_as_identity__ottoman_continuity_reading, secular_modernizers).
narrative_ontology:constraint_victim(script_as_identity__ottoman_continuity_reading, phonetically_optimized_literacy_advocates).
narrative_ontology:constraint_victim(script_as_identity__ottoman_continuity_reading, non_arabic_speaking_populations).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(script_as_identity__ottoman_continuity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(script_as_identity__ottoman_continuity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(script_as_identity__ottoman_continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(script_as_identity__ottoman_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(script_as_identity__ottoman_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) and relatively stable across the interval because the constraint persistently gates access to both state institutions and religious authority. The measurement shows extractiveness does NOT spike when enforcement increases — instead, extractiveness stabilizes while suppression_requirement rises (0.68→0.79). This dissociation is diagnostically important: the constraint is not becoming MORE extractive over time, but the cost to SUPPRESS ALTERNATIVES is rising. This is classic theater_ratio dynamics: the coordination function (institutional continuity) is the real and stable extraction mechanism; enforcement activity is increasingly theatrical — defending against challenges to the legitimacy claim rather than solving a genuine ongoing coordination problem. Theater_ratio drift (0.25→0.42) confirms this: early periods show higher functional content (script genuinely necessary for Ottoman archive access, religious scholarship); later periods show higher performative content (enforcement against script-reform movements that threaten the identity claim, not the coordination function). The accessibility_collapse is high (0.71) because once the identity-script fusion claim is established, alternatives appear not just different but identity-threatening — the cognitive cost to even imagine script change becomes prohibitive. Resistance is moderate (0.58) because secular reformers and linguistic experts mount genuine objections on technical grounds, but they are systematically excluded from the legitimacy debate (framed as enemies of continuity rather than technical partners). Suppression is high (0.79) because the constraint requires active institutional enforcement: education curricula must teach Arabic script, state employment must require it, religious authority must defer to Arabic-script expertise. Without this enforcement machinery, script change would happen rapidly (it eventually did, historically, in the Kemalist period).
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (secular_modernizers, non_arabic_speaking_populations, phonetically_optimized_literacy_advocates) and the agenda-setter seat (ottoman_institutional_inheritors) experience opposite types: the agenda-setters experience rope (genuine coordination of institutional succession), while the payers experience tangled_rope (coordination benefit for some, extraction for others) or snare (pure extraction for the powerless). This gap is not a defect — it is precisely what the per-seat classification architecture is designed to measure. The constraint is not ONE type; it is a different type from each seat. The engine computes this divergence from the stakeholder roles and exit options, and the measurement is what the corpus exists to record.
 *
 * DIRECTIONALITY LOGIC:
 *   Ottoman_institutional_inheritors are agenda-setters with identity_locked exit: they cannot abandon the continuity claim without losing their legitimacy foundation. This makes them low directionality beneficiaries (d ≈ 0.15-0.25) — they benefit substantially and have low exit cost. Islamic_religious_authority_structure is also identity_locked but as a beneficiary: religious authority is structurally dependent on Arabic script mastery, making them also low directionality (d ≈ 0.10-0.20). Secular_modernizers are powerful but constrained by the political project they cannot abandon (modernization requires confronting the continuity claim). They compute as moderate-to-high directionality targets (d ≈ 0.60-0.75). Non_arabic_speaking_populations are powerless and trapped: they cannot exit the script requirement without losing access to state institutions. High directionality targets (d ≈ 0.85-0.95). Phonetically_optimized_literacy_advocates are moderate-power moderate-exit actors whose expertise is suppressed: they should compute as moderate-high directionality (d ≈ 0.65-0.75) targets because their capacity to contribute is systematically excluded. No directionality overrides are needed; the structural data (identity_locked exit for beneficiaries, trapped exit for powerless populations, constrained exit for modernizers) drives the derivation correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is 'contested': Ottoman_institutional_inheritors attest the succession crisis is still live and requires script continuity for legitimacy. Secular reformers and comparative historians attest the problem was solved or is no longer binding — institutional legitimacy can be constructed on secular, democratic, or technical grounds. The measurement series shows extractiveness stabilizing (not growing) while suppression rises — a signature of mandatrophy drift. The founding problem (succession legitimacy in a post-imperial state) was LIVE in the 1920s (early interval); by mid-interval it is CONTESTED (some actors argue script continuity is optional); by late interval it is approaching DEAD (modern Turkish institutions have found other legitimacy grounds, and many operate without Arabic script). The theater_ratio spike (0.25→0.42) confirms mandatrophy: early enforcement was defending a real problem (Ottoman archives are inaccessible without script); late enforcement is performing continuity (defending against script change despite reduced need for the coordination function). The constraint's mandate has outlived its function. The classification prevents mislabeling this as rope (genuine ongoing coordination) or snare (pure extraction) — the tangled_rope type acknowledges both the real coordination (Ottoman institutional continuity) and the asymmetric extraction (gating state participation), while the mandatrophy signal flags that the founding problem that justified the coordination is now contested or dead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    script_identity_fusion_necessity,
    'Is the fusion of Arabic script with Turkish-Islamic identity an irreducible structural necessity, or a contingent historical choice that could be partitioned?',
    'Empirical case study from Islamic communities that maintain religious authority and scholarly continuity while using non-Arabic scripts (Bengali Islam, Persian translations, Malay Islam, Indonesian Islam). If continuity persists, the fusion is contingent rather than necessary.',
    'If contingent, the constraint is exposing itself as a choice by institutional continuity-defenders, not as an inherent property of Islamic tradition. Religious modernizers could partition the claim: maintain Arabic for formal jurisprudence, permit Latin for technical and educational domains. If necessary, the constraint reflects irreducible architectural features of religious knowledge transmission.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(script_identity_fusion_necessity, empirical, 'Whether script-identity fusion is structurally necessary or politically chosen.').

omega_variable(
    institutional_legitimacy_alternatives,
    'Could Turkish state institutions construct legitimate succession narratives without script continuity — via democratic procedure, technical competence, or social contract — or is Ottoman continuity the only available legitimacy source?',
    'Comparative historical analysis of post-imperial state formation (India, Indonesia, Iran, Egypt) and their script/legitimacy choices. If some states succeeded without script continuity, Ottoman continuity is not unique as a legitimacy anchor.',
    'If alternatives existed, the constraint represents a political choice by continuity-defenders to use script as the legitimacy anchor, rather than an inevitable necessity. Institutional authority could be reconstructed on other grounds. If no alternatives existed, the constraint reflects structural necessity in the post-imperial moment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_legitimacy_alternatives, empirical, 'Whether Ottoman continuity was the only available legitimacy source after empire collapse.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the suppression of script-reform movements structural (institutional gatekeeping, state enforcement, educational barriers) or internalized (populations have fused their identity with script such that abandonment feels like self-erasure)?',
    'Post-enforcement trajectory: in communities where script restrictions are lifted (e.g., diaspora populations with Latin-script access), does suppression persist? Do second-generation diaspora Turks maintain Arabic script loyalty despite exposure to alternatives?',
    'If structural, removing enforcement would enable rapid script change and expose the constraint as a political choice. If internalized, script abandonment carries psychological and identity costs even after enforcement ends — the constraint has colonized the desire structure itself, making it harder to dislodge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs. internalized suppression mechanism in script identity constraint.').

omega_variable(
    kernel_reading_contest_frame,
    'This constraint is one reading of a contested kernel. How does instantiating THIS reading (ottoman_continuity_reading) rather than a sibling reading (kemalist_rupture_reading or phonetic_instrumentalism_reading) shape the structural properties of the constraint?',
    'Generate the sibling readings as separate constraint stories (already in the corpus, per kernel_context). Compare their ε values, suppression profiles, beneficiary structures, and measurement trajectories. The differences will show which aspects of the constraint are reading-dependent vs. reading-independent.',
    'If ε differs substantially across readings, the constraint-identity itself depends on the reading — there is no single ''script constraint,'' only reading-specific constraint instances. If suppression differs, different enforcement costs apply to defending each reading. If beneficiaries differ, the constraint extracts from different populations depending on which reading is operative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_frame, conceptual, 'Kernel reading contest: how this reading shapes constraint structure vs. siblings.').

omega_variable(
    access_barrier_vs_legitimacy_claim,
    'Is Arabic script enforced as a practical barrier to state and religious participation, or is it enforced as a legitimacy claim that institutional actors make about themselves (even while providing alternative access mechanisms)?',
    'Observe: (1) Do non-Arabic-speaking populations have documented pathways to state employment, religious authority, or education without Arabic script mastery? (2) Do institutional actors accept secular credentials (literacy in Latin script, technical competence) as alternative grounds for authority, or do they insist on Arabic script specifically?',
    'If practical barrier, the constraint is Snare-shaped: victims are excluded regardless of competence. If legitimacy claim, it is Tangled-Rope-shaped: coordination benefits exist (institutional continuity) alongside asymmetric extraction (only those who master Arabic script claim full legitimacy), and both persist simultaneously.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(access_barrier_vs_legitimacy_claim, empirical, 'Whether script enforcement is practical gate or legitimacy claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(script_as_identity__ottoman_continuity_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(script_identity_ottoman_tr_t0, script_as_identity__ottoman_continuity_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(script_identity_ottoman_tr_t15, script_as_identity__ottoman_continuity_reading, theater_ratio, 15, 0.29).
narrative_ontology:measurement(script_identity_ottoman_tr_t30, script_as_identity__ottoman_continuity_reading, theater_ratio, 30, 0.34).
narrative_ontology:measurement(script_identity_ottoman_tr_t50, script_as_identity__ottoman_continuity_reading, theater_ratio, 50, 0.38).
narrative_ontology:measurement(script_identity_ottoman_tr_t75, script_as_identity__ottoman_continuity_reading, theater_ratio, 75, 0.41).
narrative_ontology:measurement(script_identity_ottoman_tr_t100, script_as_identity__ottoman_continuity_reading, theater_ratio, 100, 0.42).

% Extraction over time
narrative_ontology:measurement(script_identity_ottoman_be_t0, script_as_identity__ottoman_continuity_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(script_identity_ottoman_be_t15, script_as_identity__ottoman_continuity_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(script_identity_ottoman_be_t30, script_as_identity__ottoman_continuity_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(script_identity_ottoman_be_t50, script_as_identity__ottoman_continuity_reading, base_extractiveness, 50, 0.66).
narrative_ontology:measurement(script_identity_ottoman_be_t75, script_as_identity__ottoman_continuity_reading, base_extractiveness, 75, 0.67).
narrative_ontology:measurement(script_identity_ottoman_be_t100, script_as_identity__ottoman_continuity_reading, base_extractiveness, 100, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(script_identity_ottoman_su_t0, script_as_identity__ottoman_continuity_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(script_identity_ottoman_su_t15, script_as_identity__ottoman_continuity_reading, suppression_requirement, 15, 0.71).
narrative_ontology:measurement(script_identity_ottoman_su_t30, script_as_identity__ottoman_continuity_reading, suppression_requirement, 30, 0.73).
narrative_ontology:measurement(script_identity_ottoman_su_t50, script_as_identity__ottoman_continuity_reading, suppression_requirement, 50, 0.76).
narrative_ontology:measurement(script_identity_ottoman_su_t75, script_as_identity__ottoman_continuity_reading, suppression_requirement, 75, 0.78).
narrative_ontology:measurement(script_identity_ottoman_su_t100, script_as_identity__ottoman_continuity_reading, suppression_requirement, 100, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(script_as_identity__ottoman_continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(script_as_identity__ottoman_continuity_reading, 0.12).
narrative_ontology:affects_constraint(script_as_identity__ottoman_continuity_reading, script_as_identity__kemalist_rupture_reading).
narrative_ontology:affects_constraint(script_as_identity__ottoman_continuity_reading, script_as_identity__phonetic_instrumentalism_reading).

% DUAL FORMULATION NOTE:
% Part of the script_as_identity kernel family. This reading (ottoman_continuity_reading) frames script preservation as identity-constitutive and institutionally necessary. Sibling kemalist_rupture_reading frames script change as modernization and secularization. Phonetic_instrumentalism_reading frames script as neutral technology choice. The three readings have structurally distinct ε values, beneficiary sets, and suppression profiles. They compete as different framings of the same contested kernel: what does script mean in Turkish identity and state legitimacy? Each instantiates a different constraint story. All three must be linked via affects_constraints for the kernel contest to be fully modeled.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
