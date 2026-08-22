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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: 1928 Turkish Alphabet Reform — Modernization Reading
 *   domain: political_linguistics/state_formation
 *
 * SUMMARY:
 *   This story instantiates the modernization reading of the contested
 *   orthographic kernel surrounding Turkey's 1928 alphabet reform: the claim
 *   that Latin script was adopted primarily to enable technological,
 *   scientific, and educational modernization while remaining compatible with
 *   (indeed strengthening) Turkish linguistic identity, distinct from
 *   continuity claims about preserving Ottoman heritage and distinct from
 *   rupture claims that frame the change as a deliberate civilizational
 *   break. Under this reading, the coordination function (a phonetic script
 *   better suited to Turkish, compatible with European print/telegraph
 *   technology) is real and substantial, but the transition was mandated with
 *   real enforcement and produced real, unevenly distributed costs — hence
 *   tangled_rope rather than a pure rope.
 *
 * KEY AGENTS:
 *   - republican_state_bureaucracy: agenda-setter and primary beneficiary, mandates and enforces the script change
 *   - new_literate_class and technical_professions: beneficiaries who gain literacy and technical access under the new regime
 *   - older_generation_arabic_literate and religious_scholars: primary payers, bearing relearning costs and lost institutional authority
 *   - rural_populations_far_from_literacy_campaigns: secondary payers, bearing delay and access costs from uneven rollout
 *   - linguistic_historians: analytical observers assessing the technical-necessity claim against comparative evidence
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
narrative_ontology:constraint_metric(orthographic_kernel__modernization_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(orthographic_kernel__modernization_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_kernel__modernization_reading, tangled_rope).
narrative_ontology:human_readable(orthographic_kernel__modernization_reading, "1928 Turkish Alphabet Reform — Modernization Reading").
narrative_ontology:topic_domain(orthographic_kernel__modernization_reading, "political_linguistics/state_formation").

domain_priors:requires_active_enforcement(orthographic_kernel__modernization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_kernel__modernization_reading, 'c4c5ad87-7578-4aa6-9886-0eaf9f21b3c1').
narrative_ontology:cs_kernel_codification('c4c5ad87-7578-4aa6-9886-0eaf9f21b3c1', formalized).
narrative_ontology:cs_authority_grounding('c4c5ad87-7578-4aa6-9886-0eaf9f21b3c1', lineage).
narrative_ontology:cs_interpretation_layer_present('c4c5ad87-7578-4aa6-9886-0eaf9f21b3c1').
narrative_ontology:cs_reading_relation('c4c5ad87-7578-4aa6-9886-0eaf9f21b3c1', orthographic_kernel__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('c4c5ad87-7578-4aa6-9886-0eaf9f21b3c1', orthographic_kernel__rupture_reading, coexists_with).
narrative_ontology:cs_axiom('c4c5ad87-7578-4aa6-9886-0eaf9f21b3c1', foundational, script_reform_serves_technical_literacy_and_national_progress).
narrative_ontology:cs_axiom_status(script_reform_serves_technical_literacy_and_national_progress, holdable).
narrative_ontology:cs_axiom_grounding('c4c5ad87-7578-4aa6-9886-0eaf9f21b3c1', script_reform_serves_technical_literacy_and_national_progress, instrumental).
narrative_ontology:cs_axiom('c4c5ad87-7578-4aa6-9886-0eaf9f21b3c1', foundational, turkish_national_identity_is_separable_from_arabic_script_heritage).
narrative_ontology:cs_axiom_status(turkish_national_identity_is_separable_from_arabic_script_heritage, holdable).
narrative_ontology:cs_axiom_grounding('c4c5ad87-7578-4aa6-9886-0eaf9f21b3c1', turkish_national_identity_is_separable_from_arabic_script_heritage, conventional).
narrative_ontology:cs_reference_frame('c4c5ad87-7578-4aa6-9886-0eaf9f21b3c1', ottoman_arabic_script_administrative_norm).
narrative_ontology:cs_drift_state('c4c5ad87-7578-4aa6-9886-0eaf9f21b3c1', post_1928_decree_enforcement, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('c4c5ad87-7578-4aa6-9886-0eaf9f21b3c1', '').
narrative_ontology:cs_kernel_id(orthographic_kernel__modernization_reading, orthographic_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_kernel__modernization_reading, republican_state_bureaucracy).
narrative_ontology:constraint_beneficiary(orthographic_kernel__modernization_reading, new_literate_class).
narrative_ontology:constraint_beneficiary(orthographic_kernel__modernization_reading, technical_professions).
narrative_ontology:constraint_victim(orthographic_kernel__modernization_reading, older_generation_arabic_literate).
narrative_ontology:constraint_victim(orthographic_kernel__modernization_reading, religious_scholars).
narrative_ontology:constraint_victim(orthographic_kernel__modernization_reading, rural_populations_far_from_literacy_campaigns).
narrative_ontology:constraint_vindicates(orthographic_kernel__modernization_reading, phonetic_orthography_improves_literacy_rates).
narrative_ontology:constraint_vindicates(orthographic_kernel__modernization_reading, national_modernization_requires_technical_alignment_with_europe).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mandates the Latin alphabet by decree in 1928, establishes Millet Mektepleri (Nation's Schools) to retrain the adult population, and criminalizes continued official use of Arabic script within a short transition window. Gains a legible, centrally administrable citizenry and severs a channel of religious-institutional authority that had rivaled state authority under the old script's association with Quranic literacy.
narrative_ontology:constraint_stakeholder(orthographic_kernel__modernization_reading, republican_state_bureaucracy, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(orthographic_kernel__modernization_reading, republican_state_bureaucracy, beneficiary).

% Younger, urban, and school-age Turks who learn the phonetic Latin script quickly and gain rapid literacy, access to state employment, and entry into the emerging technical and clerical professions. The reform's timing especially favors those still in the education pipeline rather than already literate in Arabic script.
narrative_ontology:constraint_stakeholder(orthographic_kernel__modernization_reading, new_literate_class, beneficiary,
    moderate, biographical, mobile, national).

% Engineers, scientists, and technicians benefit from typewriters, printing presses, and technical vocabularies built around Latin characters compatible with European scientific and industrial equipment, easing import of technology and training materials.
narrative_ontology:constraint_stakeholder(orthographic_kernel__modernization_reading, technical_professions, beneficiary,
    moderate, generational, mobile, continental).

% Adults who spent years achieving literacy in Ottoman Arabic script find their skill suddenly obsolete overnight; official documents, newspapers, and correspondence shift to Latin script, and many become functionally illiterate again in their own language, unable to read their own prior letters, records, or religious texts without relearning from scratch.
narrative_ontology:constraint_stakeholder(orthographic_kernel__modernization_reading, older_generation_arabic_literate, payer,
    powerless, biographical, trapped, national).

% Ulema and madrasa-trained scholars whose authority rested partly on exclusive fluency in Arabic-script religious and legal texts lose institutional standing as the state simultaneously abolishes the caliphate and religious courts and delegitimizes the script that carried their textual tradition; they can continue practicing privately but lose public/legal authority.
narrative_ontology:constraint_stakeholder(orthographic_kernel__modernization_reading, religious_scholars, payer,
    moderate, civilizational, constrained, national).

% Villagers distant from the Nation's Schools infrastructure receive the mandate but not the resources to comply quickly; many remain illiterate in either script for years, cut off from state paperwork, land registries, and official communication until literacy campaigns eventually reach them.
narrative_ontology:constraint_stakeholder(orthographic_kernel__modernization_reading, rural_populations_far_from_literacy_campaigns, payer,
    powerless, biographical, trapped, regional).

% Centuries of Ottoman administrative, legal, and literary documents become inaccessible to the newly-schooled population without specialist training, effectively walling off a textual heritage from ordinary future citizens even though no party intended to destroy the documents themselves.
narrative_ontology:constraint_stakeholder(orthographic_kernel__modernization_reading, ottoman_administrative_archive, excluded,
    powerless, civilizational, trapped, national).
narrative_ontology:stakeholder_non_agent(orthographic_kernel__modernization_reading, ottoman_administrative_archive).

% Study the reform's literacy statistics, script-adoption speed, and comparative outcomes against other 20th-century script reforms to assess whether the modernization framing accurately describes the reform's function versus its nation-building or rupture functions.
narrative_ontology:constraint_stakeholder(orthographic_kernel__modernization_reading, linguistic_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(orthographic_kernel__modernization_reading, republican_state_bureaucracy).
narrative_ontology:fixing_cost_class(orthographic_kernel__modernization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine technical-literacy problem: the Arabic script's consonant-heavy orthography poorly represents Turkish vowel harmony, producing high illiteracy and making mechanized printing, telegraphy, and typewriting cumbersome; a phonetic Latin alphabet is coordinated across the entire population simultaneously so that schools, presses, and state records converge on one system rather than fragmenting.
% TRANSFER_FUNCTION: Moves literacy capital from the pre-1928 Arabic-script-literate population (whose skill is devalued) to the newly-schooled generation and state-aligned technical class; moves interpretive authority over legal and religious texts from the ulema toward secular state institutions administering the new script and the new legal codes issued in it.
% ABSENT_VOICES: Elderly rural citizens and religious scholars affected most severely were not the deciding parties — the reform was designed and mandated by the Language Commission and the state under Atatürk's direct sponsorship, with no formal mechanism for those bearing the relearning cost to object or slow implementation.
% DISAPPEARANCE_RATIONALE: Had the Latin alphabet mandate lapsed, Turkey's administrative, educational, and print apparatus would likely have continued with a modified Arabic script (as briefly proposed by reform committees), literacy campaigns would have taken a different technical shape, and the symbolic break from the caliphate/Ottoman religious establishment would have lacked its most visible instrument — the state's self-presentation as thoroughly modern and European-aligned depends structurally on the script change.
% FOUNDING_PROBLEM: Turkish written in Arabic script had persistently low literacy rates (estimated under 10-20% in the late Ottoman period) because Arabic's consonantal orthography does not represent Turkish's eight vowels well, and multiple competing forms of each letter made mechanized typesetting and telegraphy inefficient.
% FOUNDING_PROBLEM_CORROBORATION: UNESCO literacy-history analyses and independent linguists outside the Turkish state corroborate that Arabic script was a poor phonetic fit for Turkish and that literacy rates rose substantially after 1928-1935 Nation's Schools campaigns, supporting the modernization reading's technical premise as at least partly live rather than pure post-hoc justification; however, the same outside scholarship also notes literacy gains were comparable to contemporaneous reforms elsewhere that did not change scripts, so the technical-necessity claim is corroborated but not uniquely determinative — the founding problem was real but not sufficient on its own to require Latin over a reformed Arabic script.
narrative_ontology:disappearance_verdict(orthographic_kernel__modernization_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_kernel__modernization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_kernel__modernization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is set moderate (0.42-0.50, declining over the interval) reflecting real transitional costs to non-beneficiary groups that diminish as literacy campaigns mature and the older Arabic-literate cohort ages out of the affected population. Suppression starts high (0.75) reflecting the compressed enforcement window (Arabic-script use in official contexts was rapidly phased out) and declines as the new script normalizes and enforcement need drops. Theater ratio is kept low-moderate (0.18-0.22) because the coordination function (literacy campaigns, printing infrastructure, technical vocabulary alignment) was substantively real, not merely performative, though some campaign activity (mass rallies, symbolic events) had a genuine theatrical component distinct from literacy delivery itself.
 *
 * DIRECTIONALITY LOGIC:
 *   The state bureaucracy sits at the beneficiary end: it designs, enforces, and administratively benefits from the reform, with arbitrage-level exit (it can adjust enforcement pace at will). The new literate class and technical professions are moderate beneficiaries with mobile exit options — their gains are real but contingent on their position in the age/education pipeline at the time of reform. Older Arabic-literate adults and religious scholars are targets: trapped or constrained exit, bearing devaluation of an existing skill/authority base they cannot easily replace. Rural populations are targets by neglect rather than by design — the extraction here is a resourcing failure (campaigns not reaching them) rather than deliberate targeting, which is why their power/exit profile differs from the ulema's despite similar payer status.
 *
 * MANDATROPHY ANALYSIS:
 *   The modernization reading resists mandatrophy misclassification in both directions: it does not treat the reform as pure extraction (the technical literacy problem was real and independently corroborated by outside linguists), nor does it treat it as costless pure coordination (the enforcement window, the older generation's functional re-illiteracy, and the ulema's institutional displacement are real, asymmetric costs riding on the same mandate). The declining extractiveness/suppression trajectory reflects the founding problem transitioning from live-and-urgent (1928-1935) toward substantially-resolved-but-institutionally-retained (post-1940), which is the tangled_rope signature: coordination function intact, extraction persisting via retained enforcement infrastructure and continued devaluation of the older cohort's now-permanently-obsolete skill.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    modernization_vs_rupture_intent,
    'Was the primary intent behind the script reform genuinely technical/modernizing (better literacy, technology compatibility), or was modernization the public justification for a deliberate cultural rupture whose real target was severing ties to the Ottoman/Islamic past?',
    'Comparative analysis of Language Commission internal deliberation records, the speed and manner of the caliphate''s abolition relative to the script reform''s timeline, and Atatürk''s own stated rationale across public and private communications, cross-checked against comparative cases of script reform without accompanying religious-institutional abolition.',
    'If rupture was the dominant intent, this modernization_reading constraint would be better understood as a legitimating narrative laid over the rupture_reading''s constraint rather than a structurally independent claim; if modernization was genuinely dominant, the two readings describe partially overlapping but analytically separable functions of the same historical act.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modernization_vs_rupture_intent, conceptual, 'Whether the modernization framing is the reform''s real function or a legitimating narrative over a rupture function.').

omega_variable(
    literacy_gains_attributable_to_script_vs_campaign,
    'How much of Turkey''s post-1928 literacy gains are attributable to the script''s improved phonetic fit for Turkish specifically, versus the concurrent mass literacy campaign infrastructure (Nation''s Schools, compulsory education expansion) that could have delivered similar gains under a reformed Arabic script?',
    'Comparative study against contemporaneous literacy campaigns in other nations that expanded schooling infrastructure without changing scripts, controlling for GDP, urbanization, and campaign intensity.',
    'If literacy gains are mostly attributable to campaign infrastructure rather than script change itself, the modernization reading''s core technical-necessity claim weakens substantially, shifting weight toward viewing script change as a symbolic/rupture act riding on a genuine but script-independent literacy campaign.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literacy_gains_attributable_to_script_vs_campaign, empirical, 'Whether literacy gains stem from the script change itself or from concurrent campaign infrastructure.').

omega_variable(
    cs_framing_kernel_vs_layered_legitimacy,
    'Should the analyzed kernel be the script-choice decree itself (the obvious framing), or the layered legitimacy claim that positions the Republican state as the sole authoritative interpreter of Turkish modernization — of which the script decree is one instrument among several (secular legal code, dress reform, calendar reform)?',
    'Examine whether the same authority_grounding and enforcement pattern recurs across the other Kemalist reforms of the same period (dress, calendar, legal code); if the pattern is consistent across all of them, the broader legitimacy-claim framing is more explanatory than treating script reform as a standalone kernel.',
    'Under the narrower framing (script decree only), this story''s kernel_codification is formalized with lineage-adjacent state authority; under the broader framing (state modernization legitimacy claim), authority_grounding would lean more toward extraction (the state extracting legitimacy from claimed exclusive modernization authority) and interpretation_layer_present would extend across multiple simultaneous reform instruments rather than this one alone. This story adopts the narrower framing because the SCOPE manifest isolates the orthographic kernel specifically.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_kernel_vs_layered_legitimacy, conceptual, 'Whether the kernel is the script decree alone or the broader Kemalist state-legitimacy claim it instantiates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_kernel__modernization_reading, 1928, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_tr_t1928, orthographic_kernel__modernization_reading, theater_ratio, 1928, 0.18).
narrative_ontology:measurement(orth_tr_t1931, orthographic_kernel__modernization_reading, theater_ratio, 1931, 0.2).
narrative_ontology:measurement(orth_tr_t1935, orthographic_kernel__modernization_reading, theater_ratio, 1935, 0.21).
narrative_ontology:measurement(orth_tr_t1940, orthographic_kernel__modernization_reading, theater_ratio, 1940, 0.22).
narrative_ontology:measurement(orth_tr_t1945, orthographic_kernel__modernization_reading, theater_ratio, 1945, 0.22).
narrative_ontology:measurement(orth_tr_t1950, orthographic_kernel__modernization_reading, theater_ratio, 1950, 0.22).

% Extraction over time
narrative_ontology:measurement(orth_be_t1928, orthographic_kernel__modernization_reading, base_extractiveness, 1928, 0.5).
narrative_ontology:measurement(orth_be_t1931, orthographic_kernel__modernization_reading, base_extractiveness, 1931, 0.47).
narrative_ontology:measurement(orth_be_t1935, orthographic_kernel__modernization_reading, base_extractiveness, 1935, 0.44).
narrative_ontology:measurement(orth_be_t1940, orthographic_kernel__modernization_reading, base_extractiveness, 1940, 0.42).
narrative_ontology:measurement(orth_be_t1945, orthographic_kernel__modernization_reading, base_extractiveness, 1945, 0.42).
narrative_ontology:measurement(orth_be_t1950, orthographic_kernel__modernization_reading, base_extractiveness, 1950, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(orth_su_t1928, orthographic_kernel__modernization_reading, suppression_requirement, 1928, 0.75).
narrative_ontology:measurement(orth_su_t1931, orthographic_kernel__modernization_reading, suppression_requirement, 1931, 0.68).
narrative_ontology:measurement(orth_su_t1935, orthographic_kernel__modernization_reading, suppression_requirement, 1935, 0.62).
narrative_ontology:measurement(orth_su_t1940, orthographic_kernel__modernization_reading, suppression_requirement, 1940, 0.58).
narrative_ontology:measurement(orth_su_t1945, orthographic_kernel__modernization_reading, suppression_requirement, 1945, 0.58).
narrative_ontology:measurement(orth_su_t1950, orthographic_kernel__modernization_reading, suppression_requirement, 1950, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_kernel__modernization_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(orthographic_kernel__modernization_reading, 0.1).
narrative_ontology:affects_constraint(orthographic_kernel__modernization_reading, orthographic_kernel__continuity_reading).
narrative_ontology:affects_constraint(orthographic_kernel__modernization_reading, orthographic_kernel__rupture_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the orthographic_kernel (Turkish 1928 alphabet reform). continuity_reading authors high ε for the same event centered on loss of Ottoman/Islamic textual continuity as the primary harm. rupture_reading authors the script change as an intentional civilizational break with a different beneficiary emphasis (nation-builders and secularizing elites specifically, rather than 'state bureaucracy' broadly) and typically a distinct suppression profile emphasizing symbolic severance over technical transition costs. This modernization_reading is deliberately the most 'coordination-forward' of the three, authoring the lowest ε among the family, per the expected structural delta of moderate ε driven by literacy-expansion costs rather than cultural-loss or rupture costs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
