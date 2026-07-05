% ============================================================================
% CONSTRAINT STORY: orthographic_kernel__modernization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Latin Script Reform as Modernization Instrument (Turkey, 1928)
 *   domain: political_linguistics/state_formation/commitment_systems
 *
 * SUMMARY:
 *   This story instantiates the modernization reading of the
 *   orthographic_kernel contested by three structurally distinct claims about
 *   the 1928 Turkish script reform. Under this reading, the Latin alphabet is
 *   read primarily as an instrument that accelerates mass literacy and
 *   technical/scientific integration while the reformers' own framing
 *   insisted continuity of Turkish linguistic identity (as distinct from
 *   Ottoman/Islamic textual identity) was preserved — only the writing system
 *   changed, not the language. This is NOT the continuity_reading (which
 *   holds Arabic script itself, not Latin script, as the identity-preserving
 *   choice) nor the rupture_reading (which reads the same script change as a
 *   deliberate act of civilizational severance rather than a neutral
 *   technical upgrade). Each reading carries its own beneficiary/victim
 *   structure and its own epsilon; this reading's epsilon (0.42) reflects
 *   moderate literacy-transition costs borne mostly by the older generation
 *   and religious-textual specialists, coordinated against a real and
 *   substantial gain in mass-literacy velocity and technical interoperability
 *   that the modernization reading foregrounds and the rupture reading treats
 *   as secondary to the symbolic break.
 *
 * KEY AGENTS:
 *   - state_bureaucracy: Primary agenda_setter (institutional/arbitrage) — designs and enforces the reform, captures legitimacy and administrative-capacity gains
 *   - new_literate_class: Primary beneficiary (moderate/mobile) — literacy acquisition accelerated by phonetic fit
 *   - arabic_script_literate_elders: Primary payer (powerless/trapped) — functional illiteracy imposed overnight
 *   - religious_scholars: Secondary payer (moderate/constrained) — authority decoupled from expanding civic sphere
 *   - comparative_language_policy_analysts: Analytical observer — assesses literacy-outcome evidence independent of state narrative
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_kernel__modernization_reading, 0.42).
domain_priors:suppression_score(orthographic_kernel__modernization_reading, 0.58).
domain_priors:theater_ratio(orthographic_kernel__modernization_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_kernel__modernization_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(orthographic_kernel__modernization_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(orthographic_kernel__modernization_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_kernel__modernization_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(orthographic_kernel__modernization_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_kernel__modernization_reading, tangled_rope).
narrative_ontology:human_readable(orthographic_kernel__modernization_reading, "Latin Script Reform as Modernization Instrument (Turkey, 1928)").
narrative_ontology:topic_domain(orthographic_kernel__modernization_reading, "political_linguistics/state_formation/commitment_systems").

domain_priors:requires_active_enforcement(orthographic_kernel__modernization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_kernel__modernization_reading, 'a01300e2-dc83-484b-b695-a3ffacf7a616').
narrative_ontology:cs_kernel_codification('a01300e2-dc83-484b-b695-a3ffacf7a616', formalized).
narrative_ontology:cs_authority_grounding('a01300e2-dc83-484b-b695-a3ffacf7a616', extraction).
narrative_ontology:cs_interpretation_layer_present('a01300e2-dc83-484b-b695-a3ffacf7a616').
narrative_ontology:cs_reading_relation('a01300e2-dc83-484b-b695-a3ffacf7a616', orthographic_kernel__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('a01300e2-dc83-484b-b695-a3ffacf7a616', orthographic_kernel__rupture_reading, coexists_with).
narrative_ontology:cs_axiom('a01300e2-dc83-484b-b695-a3ffacf7a616', foundational, script_is_severable_from_linguistic_identity).
narrative_ontology:cs_axiom_status(script_is_severable_from_linguistic_identity, holdable).
narrative_ontology:cs_axiom_grounding('a01300e2-dc83-484b-b695-a3ffacf7a616', script_is_severable_from_linguistic_identity, conventional).
narrative_ontology:cs_axiom('a01300e2-dc83-484b-b695-a3ffacf7a616', secondary, phonetic_fit_drives_literacy_velocity).
narrative_ontology:cs_axiom_status(phonetic_fit_drives_literacy_velocity, holdable).
narrative_ontology:cs_axiom_grounding('a01300e2-dc83-484b-b695-a3ffacf7a616', phonetic_fit_drives_literacy_velocity, empirically_contingent).
narrative_ontology:cs_reference_frame('a01300e2-dc83-484b-b695-a3ffacf7a616', ottoman_arabic_script_administrative_norm).
narrative_ontology:cs_drift_state('a01300e2-dc83-484b-b695-a3ffacf7a616', post_1928_republican_consolidation, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('a01300e2-dc83-484b-b695-a3ffacf7a616', '').
narrative_ontology:cs_kernel_id(orthographic_kernel__modernization_reading, orthographic_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_kernel__modernization_reading, state_bureaucracy).
narrative_ontology:constraint_beneficiary(orthographic_kernel__modernization_reading, new_literate_class).
narrative_ontology:constraint_beneficiary(orthographic_kernel__modernization_reading, technical_professional_class).
narrative_ontology:constraint_victim(orthographic_kernel__modernization_reading, arabic_script_literate_elders).
narrative_ontology:constraint_victim(orthographic_kernel__modernization_reading, religious_scholars).
narrative_ontology:constraint_victim(orthographic_kernel__modernization_reading, ottoman_archive_dependent_scholars).
narrative_ontology:constraint_vindicates(orthographic_kernel__modernization_reading, phonetic_orthography_accelerates_mass_literacy).
narrative_ontology:constraint_vindicates(orthographic_kernel__modernization_reading, national_language_standardization_aids_state_capacity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs, mandates, and enforces the script reform through the Language Commission and compulsory Millet Mektepleri (Nation's Schools). Reissues all administrative, legal, and educational documents in Latin script, criminalizes continued use of Arabic script in official contexts within a short transition window, and captures the legitimacy gains of appearing decisively modern to European powers and domestic reformist constituencies.
narrative_ontology:constraint_stakeholder(orthographic_kernel__modernization_reading, state_bureaucracy, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(orthographic_kernel__modernization_reading, state_bureaucracy, beneficiary).

% Younger, urban, and previously illiterate or semi-literate populations acquire reading and writing faster under a phonetic Latin alphabet than they could have under Arabic script's non-phonetic mapping to Turkish vowels. They gain access to civil service jobs, newspapers, and technical education that increasingly exist only in the new script.
narrative_ontology:constraint_stakeholder(orthographic_kernel__modernization_reading, new_literate_class, beneficiary,
    moderate, generational, mobile, national).

% Engineers, scientists, and technicians benefit from a script compatible with Western scientific notation, typewriters, and printing technology, easing import of technical literature and international correspondence without transliteration overhead.
narrative_ontology:constraint_stakeholder(orthographic_kernel__modernization_reading, technical_professional_class, beneficiary,
    moderate, biographical, mobile, continental).

% Adults who spent decades achieving literacy in Arabic script become functionally illiterate overnight in the new official orthography. They cannot read state communications, newspapers, or their own children's schoolbooks. Re-learning literacy in old age with limited state support is their only path back to functional literacy; most simply lose access to the written public sphere.
narrative_ontology:constraint_stakeholder(orthographic_kernel__modernization_reading, arabic_script_literate_elders, payer,
    powerless, biographical, trapped, local).

% Ulema and religious teachers whose authority rested partly on privileged access to Arabic-script religious and legal texts see that access decoupled from the new civic literacy. They can continue functioning within religious institutions but are cut off from the state's expanding secular administrative and educational apparatus, and their students face a bifurcated literacy that isolates religious training from civic advancement.
narrative_ontology:constraint_stakeholder(orthographic_kernel__modernization_reading, religious_scholars, payer,
    moderate, civilizational, constrained, national).

% Historians, archivists, and legal scholars whose work depends on centuries of Ottoman administrative and literary documents written in Arabic script face a widening skills gap: new generations trained only in Latin-script Turkish require specialized paleographic training to access their own documentary heritage, professionalizing what was once ordinary civic literacy.
narrative_ontology:constraint_stakeholder(orthographic_kernel__modernization_reading, ottoman_archive_dependent_scholars, payer,
    moderate, civilizational, constrained, continental).

% Advocates of reformed Arabic-script orthographies (modified vowel-marking proposals circulated in the 1920s as an alternative modernization path) were not seriously considered once the state committed to Latin script; they had no institutional venue to press their alternative once the Language Commission's direction was set.
narrative_ontology:constraint_stakeholder(orthographic_kernel__modernization_reading, rival_script_traditions, excluded,
    powerless, generational, trapped, national).

% Historians and linguists studying the reform's outcomes: literacy rate trajectories, intergenerational knowledge transmission costs, and comparison to other 20th-century script reforms (Vietnamese quoc ngu, Soviet Central Asian Cyrillicization).
narrative_ontology:constraint_stakeholder(orthographic_kernel__modernization_reading, comparative_language_policy_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(orthographic_kernel__modernization_reading, state_bureaucracy).
narrative_ontology:fixing_cost_class(orthographic_kernel__modernization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine mass-literacy coordination problem: Arabic script's consonantal structure poorly represents Turkish vowel harmony, making literacy acquisition slower under the old orthography; a phonetic Latin alphabet standardizes spelling-to-sound mapping across the population and interoperates with imported printing, typewriting, and scientific notation infrastructure.
% TRANSFER_FUNCTION: Moves functional literacy, administrative access, and civic legibility from the older Arabic-script-literate population and religious-textual authorities toward the state bureaucracy and a new generation trained exclusively in the Latin alphabet; moves interpretive authority over the written past from ordinary literate citizens to a shrinking specialist class of Ottoman paleographers.
% ABSENT_VOICES: Rural populations far from the Millet Mektepleri campaign, elderly Arabic-script literates without access to re-education programs, and proponents of a reformed-Arabic-script middle path were not meaningfully represented in the Language Commission's deliberations, which were dominated by reformist elites already committed to the Latin alternative.
% DISAPPEARANCE_RATIONALE: Had the reform not occurred, Turkey's literacy expansion would have followed a different trajectory tied to Arabic-script pedagogy reform or a hybrid path; the state bureaucracy's legitimacy narrative of decisive modernization, the newspaper and publishing industry's technical base, and the intergenerational literacy discontinuity that now defines access to Ottoman-era documents would not exist in their current form.
% FOUNDING_PROBLEM: Turkish literacy rates under Ottoman Arabic-script orthography were low (widely cited estimates near 10%), partly attributed to the script's poor fit for Turkish phonology, and the new Republic sought both a legibility boost and a decisive symbolic break enabling rapid Westernizing modernization.
% FOUNDING_PROBLEM_CORROBORATION: Independent linguists and literacy researchers outside the Turkish state (UNESCO literacy studies, comparative orthography scholarship) corroborate that Turkish literacy rates rose substantially over subsequent decades and attribute meaningful acceleration to the phonetic Latin orthography; the founding problem of low mass literacy is broadly treated as solved by external assessment, not merely by the Turkish state's own historiography, though scholars dispute how much credit belongs to the script change versus concurrent compulsory schooling and print investment.
narrative_ontology:disappearance_verdict(orthographic_kernel__modernization_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_kernel__modernization_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_kernel__modernization_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.42) is moderate rather than low or high: there is a genuine, well-evidenced coordination gain (phonetic script materially eases Turkish literacy acquisition and technical/scientific text interoperability), which caps how extractive this reading can honestly be scored, but the transition was compulsory and abrupt, with a short window before Arabic-script materials lost official standing entirely — imposing real, uncompensated costs on a specific cohort (script-literate elders, religious scholars) who did not choose the transition and could not adapt at the state's imposed pace. Suppression (0.58) reflects that alternatives — gradual bilingual transition periods, reformed Arabic orthography, voluntary adoption — were foreclosed by legal mandate (criminalization of official Arabic-script use) rather than allowed to compete. Theater ratio starts elevated (0.40) reflecting the reform's substantial ceremonial/nationalist performance component (Atatürk's nationwide 'blackboard tours') alongside its genuine pedagogical infrastructure, and declines as the schools and print infrastructure matured into functioning literacy institutions rather than remaining primarily symbolic.
 *
 * PERSPECTIVAL GAP:
 *   From the state bureaucracy's agenda-setting seat, this reads as successful coordination: a real literacy and modernization problem, solved. From the arabic_script_literate_elders' payer seat, the same mandate reads as an imposed, uncompensated loss of functional literacy with no meaningful transition support — the engine's per-seat computation should diverge sharply between these two positions even though both are looking at the identical structural mandate.
 *
 * DIRECTIONALITY LOGIC:
 *   State bureaucracy sits at the full beneficiary end: it designed the mandate, controls enforcement, and captures both administrative-capacity and international-legitimacy gains — d near 0.0. New literate class and technical professionals are genuine beneficiaries with mobile exit (they can choose whether/how fast to engage with new institutions) — d moderate-low. Arabic-script-literate elders are trapped targets: no meaningful exit, high identity and functional cost, d near the full-target end. Religious scholars and Ottoman-archive-dependent scholars are constrained rather than trapped — they retain institutional footing but lose relative civic standing, d moderate-high.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (low mass literacy under a poorly-fitting script) is corroborated as substantially resolved by external literacy researchers, which argues against reading the ongoing orthographic mandate itself as an active mandatrophy case — the mandate's core coordination function (a single standard national orthography) remains live even after the founding literacy-crisis problem receded, because a standard orthography is a continuing coordination good, not merely a transitional literacy intervention. This is what prevents mislabeling the modernization reading as pure extraction: unlike a scaffold whose sunset condition has passed, the orthographic standard converted from crisis-response to steady-state coordination infrastructure, which is why this reading is authored as tangled_rope (ongoing genuine coordination plus asymmetric extraction from the transition cohort) rather than scaffold or snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    modernization_vs_rupture_intent,
    'Was the primary intent of the 1928 reform genuine literacy/technical modernization with cultural rupture as a side effect, or was cultural rupture the primary strategic goal with modernization serving as the legitimizing cover story?',
    'Close reading of Language Commission internal deliberations, Atatürk''s private and public statements distinguishing pedagogical from civilizational arguments, and comparison of resource allocation between literacy infrastructure investment versus symbolic/ceremonial reform promotion.',
    'If rupture was primary, this modernization_reading materially understates the constraint''s function and the rupture_reading is the structurally dominant framing; if modernization was primary, this reading''s classification and beneficiary structure stand as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modernization_vs_rupture_intent, conceptual, 'Whether modernization or civilizational rupture was the reform''s true primary function, versus its stated justification.').

omega_variable(
    literacy_gain_attribution,
    'How much of Turkey''s subsequent literacy rate increase is attributable to the phonetic advantage of Latin script itself, versus concurrent compulsory schooling expansion, print investment, and state literacy campaigns that could have occurred under any script?',
    'Comparative analysis against literacy trajectories in comparable states that expanded compulsory schooling without changing script, controlling for investment levels.',
    'If script contributed little beyond concurrent schooling investment, the coordination-function claim underlying this reading''s tangled_rope classification weakens, pushing the reading toward snare; if script contribution was substantial, the tangled_rope classification is well-supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literacy_gain_attribution, empirical, 'Causal attribution of literacy gains between script phonetics and concurrent education investment.').

omega_variable(
    orthographic_kernel_framing_choice,
    'Is the orthographic_kernel best understood as a single contested policy event (the 1928 reform) read three ways, or as three genuinely distinct historical claims about different aspects of the same event (pedagogical efficacy, cultural continuity, and political intent) that happen to share a common trigger?',
    'N/A — this is a framing choice inherent to how the kernel was decomposed into readings; resolving it would require the corpus maintainers to either merge or further split the three-reading structure.',
    'If the three readings are genuinely orthogonal claims rather than competing interpretations of one event, network edges among them should be characterized as complementary rather than competitive, changing how contamination/support propagates between them.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(orthographic_kernel_framing_choice, conceptual, 'Whether the kernel''s three readings compete over one event''s meaning or address orthogonal structural questions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_kernel__modernization_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_tr_t0, orthographic_kernel__modernization_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(orth_tr_t8, orthographic_kernel__modernization_reading, theater_ratio, 8, 0.35).
narrative_ontology:measurement(orth_tr_t16, orthographic_kernel__modernization_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement(orth_tr_t24, orthographic_kernel__modernization_reading, theater_ratio, 24, 0.29).
narrative_ontology:measurement(orth_tr_t32, orthographic_kernel__modernization_reading, theater_ratio, 32, 0.28).
narrative_ontology:measurement(orth_tr_t40, orthographic_kernel__modernization_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(orth_be_t0, orthographic_kernel__modernization_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(orth_be_t8, orthographic_kernel__modernization_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(orth_be_t16, orthographic_kernel__modernization_reading, base_extractiveness, 16, 0.46).
narrative_ontology:measurement(orth_be_t24, orthographic_kernel__modernization_reading, base_extractiveness, 24, 0.44).
narrative_ontology:measurement(orth_be_t32, orthographic_kernel__modernization_reading, base_extractiveness, 32, 0.43).
narrative_ontology:measurement(orth_be_t40, orthographic_kernel__modernization_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(orth_su_t0, orthographic_kernel__modernization_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(orth_su_t8, orthographic_kernel__modernization_reading, suppression_requirement, 8, 0.68).
narrative_ontology:measurement(orth_su_t16, orthographic_kernel__modernization_reading, suppression_requirement, 16, 0.6).
narrative_ontology:measurement(orth_su_t24, orthographic_kernel__modernization_reading, suppression_requirement, 24, 0.55).
narrative_ontology:measurement(orth_su_t32, orthographic_kernel__modernization_reading, suppression_requirement, 32, 0.58).
narrative_ontology:measurement(orth_su_t40, orthographic_kernel__modernization_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_kernel__modernization_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(orthographic_kernel__modernization_reading, 0.1).
narrative_ontology:affects_constraint(orthographic_kernel__modernization_reading, orthographic_kernel__continuity_reading).
narrative_ontology:affects_constraint(orthographic_kernel__modernization_reading, orthographic_kernel__rupture_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the orthographic_kernel (the 1928 Turkish script reform). continuity_reading holds Arabic script as the identity-preserving choice and treats Latin adoption as loss; modernization_reading (this file) treats Turkish linguistic identity as decoupled from script and reads the reform primarily as a literacy/technical-interoperability coordination mechanism with moderate transition costs; rupture_reading treats the same script change as a deliberate act of civilizational severance whose cultural-break function is primary rather than incidental. All three share the same historical event but diverge on beneficiary/victim structure, epsilon, and primary function — they are linked via network edges rather than merged into one constraint, per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
