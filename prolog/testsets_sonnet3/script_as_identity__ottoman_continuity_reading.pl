% ============================================================================
% CONSTRAINT STORY: script_as_identity__ottoman_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-01
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
 *   constraint_id: script_as_identity__ottoman_continuity_reading
 *   human_readable: Arabic Script as Constitutive of Turkish-Islamic Identity (Ottoman Continuity Reading)
 *   domain: linguistics/religious authority/state-building
 *
 * SUMMARY:
 *   This story instantiates the ottoman_continuity_reading of the
 *   script_as_identity kernel: the claim that Arabic script is constitutive
 *   of Turkish-Islamic identity, not merely an instrumental writing system,
 *   and that abandoning it severs access to Ottoman institutional memory and
 *   religious-legal authority structures. From this reading's own lights, the
 *   standing arrangement under contest is the late-Ottoman/early-reform-era
 *   status quo in which Arabic-script literacy gates religious, legal, and
 *   administrative authority. Extraction, suppression, and the
 *   beneficiary/victim structure are all authored as this reading assesses
 *   the arrangement it defends — not as the kemalist_rupture_reading or
 *   phonetic_instrumentalism_reading would assess it, and not as this
 *   reading's own endorsed alternative (script continuity without the
 *   literacy gap) would be scored, which is not the referent here.
 *
 * KEY AGENTS:
 *   - ulema_religious_scholars: agenda_setter/beneficiary (institutional/arbitrage) — administers the credentialing function the script anchors
 *   - ottoman_bureaucratic_class: beneficiary (institutional/constrained) — specialized textual capital tied to script continuity
 *   - rural_literacy_seekers: payer (powerless/trapped) — bears the orthographic mismatch with no alternative
 *   - comparative_linguists: analytical observer — assesses orthographic fit and literacy outcomes independent of identity stakes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(script_as_identity__ottoman_continuity_reading, 0.42).
domain_priors:suppression_score(script_as_identity__ottoman_continuity_reading, 0.81).
domain_priors:theater_ratio(script_as_identity__ottoman_continuity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(script_as_identity__ottoman_continuity_reading, tangled_rope).
narrative_ontology:human_readable(script_as_identity__ottoman_continuity_reading, "Arabic Script as Constitutive of Turkish-Islamic Identity (Ottoman Continuity Reading)").
narrative_ontology:topic_domain(script_as_identity__ottoman_continuity_reading, "linguistics/religious authority/state-building").

domain_priors:requires_active_enforcement(script_as_identity__ottoman_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(script_as_identity__ottoman_continuity_reading, '50ef2983-7a20-43b6-9760-fb7a53b08bb5').
narrative_ontology:cs_kernel_codification('50ef2983-7a20-43b6-9760-fb7a53b08bb5', fixed_text).
narrative_ontology:cs_authority_grounding('50ef2983-7a20-43b6-9760-fb7a53b08bb5', lineage).
narrative_ontology:cs_interpretation_layer_present('50ef2983-7a20-43b6-9760-fb7a53b08bb5').
narrative_ontology:cs_reading_relation('50ef2983-7a20-43b6-9760-fb7a53b08bb5', script_as_identity__kemalist_rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('50ef2983-7a20-43b6-9760-fb7a53b08bb5', script_as_identity__phonetic_instrumentalism_reading, coexists_with).
narrative_ontology:cs_axiom('50ef2983-7a20-43b6-9760-fb7a53b08bb5', foundational, script_constitutes_religious_legal_continuity).
narrative_ontology:cs_axiom_status(script_constitutes_religious_legal_continuity, holdable).
narrative_ontology:cs_axiom_grounding('50ef2983-7a20-43b6-9760-fb7a53b08bb5', script_constitutes_religious_legal_continuity, conventional).
narrative_ontology:cs_axiom('50ef2983-7a20-43b6-9760-fb7a53b08bb5', secondary, credentialed_lineage_transmission_requires_script_stability).
narrative_ontology:cs_axiom_status(credentialed_lineage_transmission_requires_script_stability, holdable).
narrative_ontology:cs_axiom_grounding('50ef2983-7a20-43b6-9760-fb7a53b08bb5', credentialed_lineage_transmission_requires_script_stability, instrumental).
narrative_ontology:cs_reference_frame('50ef2983-7a20-43b6-9760-fb7a53b08bb5', ottoman_script_lineage_continuity).
narrative_ontology:cs_drift_state('50ef2983-7a20-43b6-9760-fb7a53b08bb5', post_1928_alphabet_reform, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('50ef2983-7a20-43b6-9760-fb7a53b08bb5', '').
narrative_ontology:cs_kernel_id(script_as_identity__ottoman_continuity_reading, script_as_identity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(script_as_identity__ottoman_continuity_reading, ulema_religious_scholars).
narrative_ontology:constraint_beneficiary(script_as_identity__ottoman_continuity_reading, ottoman_bureaucratic_class).
narrative_ontology:constraint_beneficiary(script_as_identity__ottoman_continuity_reading, sufi_orders).
narrative_ontology:constraint_beneficiary(script_as_identity__ottoman_continuity_reading, arabic_literate_elite).
narrative_ontology:constraint_victim(script_as_identity__ottoman_continuity_reading, rural_literacy_seekers).
narrative_ontology:constraint_victim(script_as_identity__ottoman_continuity_reading, women_excluded_from_madrasa_education).
narrative_ontology:constraint_victim(script_as_identity__ottoman_continuity_reading, non_arabic_literate_turkish_speakers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer religious education, legal interpretation, and textual authority through Arabic-script literacy in Ottoman Turkish, Arabic, and Persian. Their institutional standing depends on script continuity: mastery of Arabic orthography is the credentialing mechanism for religious and legal authority, and any script change threatens to devalue decades of accumulated textual capital and dilute their monopoly on interpreting sacred and legal texts.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, ulema_religious_scholars, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(script_as_identity__ottoman_continuity_reading, ulema_religious_scholars, beneficiary).

% State administrators and scribes whose careers rest on facility with Ottoman Turkish written in Arabic script, including its Persian and Arabic loan-vocabulary layers. Script continuity preserves access to centuries of archived state records, legal precedent, and diplomatic correspondence; a script change would strand this administrative class's specialized skill relative to a newly literate generation trained differently.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, ottoman_bureaucratic_class, beneficiary,
    institutional, generational, constrained, national).

% Transmit devotional and mystical literature through Arabic-script manuscripts and calligraphic tradition; the script itself carries devotional weight (calligraphy as sacred art, letter-mysticism in some orders). Their continuity as institutions is bound to script continuity in a way that is not merely practical but constitutive of practice.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, sufi_orders, beneficiary,
    organized, civilizational, identity_locked, regional).

% Merchants, notables, and educated urban families whose literacy in Arabic script grants access to religious texts, legal documents, poetry, and inter-elite correspondence across the wider Islamic world. Script continuity preserves this literacy's exchange value; script change would require costly re-skilling but these actors have resources to manage the transition or maintain parallel literacies.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, arabic_literate_elite, beneficiary,
    powerful, generational, arbitrage, national).

% Rural Turkish speakers attempting literacy face an orthography poorly matched to Turkish vowel harmony and phonology, developed for Arabic's consonantal structure. Learning to read requires mastering a system that does not transparently represent their spoken language, producing persistently low literacy rates outside religious/administrative institutions. They have no practical alternative script available to them and no institutional path to challenge the arrangement.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, rural_literacy_seekers, payer,
    powerless, biographical, trapped, regional).

% Formal Arabic-script literacy is transmitted principally through madrasa and mosque-affiliated education, from which women are largely excluded. The script's institutional transmission pathway compounds an existing exclusion: literacy is not merely difficult to acquire but structurally routed through spaces closed to them, entrenching a literacy gap along gender lines.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, women_excluded_from_madrasa_education, payer,
    powerless, biographical, trapped, national).

% The vast majority of Ottoman Turkish speakers who never acquire functional literacy in the constitutive script remain locked out of legal documents, state notices, and the accumulated textual tradition the script is said to preserve for them. The continuity the script is said to protect is, from where they stand, continuity of their own exclusion from it.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, non_arabic_literate_turkish_speakers, payer,
    powerless, generational, trapped, national).

% Late Ottoman intellectuals and officials who argued for orthographic reform or alternative scripts to raise literacy rates were present in policy debate but structurally marginalized by an authority structure (ulema, court, entrenched bureaucracy) whose legitimacy and livelihood depended on the existing script's constitutive status. Their proposals were absorbed into commissions and shelved rather than adjudicated on literacy-outcome evidence.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, reform_minded_bureaucrats, excluded,
    moderate, biographical, constrained, national).

% Assess orthographic fit between script and phonology, compare literacy outcomes across script transitions in other Turkic and non-Turkic languages, and evaluate the empirical claims embedded in identity-continuity arguments without holding a stake in the outcome.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, comparative_linguists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(script_as_identity__ottoman_continuity_reading, diffuse).
narrative_ontology:fixing_cost_class(script_as_identity__ottoman_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The script coordinates a religious-legal-administrative textual tradition: a single orthographic system lets law, scripture, scholarship, and state record accumulate and remain mutually legible across centuries, and lets a credentialed interpretive class transmit authority through demonstrated mastery of that system.
% TRANSFER_FUNCTION: The arrangement moves interpretive and administrative authority toward those already literate in Arabic script (ulema, bureaucratic class, urban elite) and away from the Turkish-speaking majority, whose access to law, scripture, and state communication depends on institutions that do not serve them on favorable terms.
% ABSENT_VOICES: Reform-minded bureaucrats and literacy advocates raised orthographic-fit objections within Ottoman policy circles but were structurally outvoted by the authority whose legitimacy rested on the status quo; the rural and female populations most affected by the literacy gap were not represented in the debate at all.
% DISAPPEARANCE_RATIONALE: If Arabic-script constitutive status vanished overnight, the ulema's credentialing monopoly, the bureaucratic class's specialized textual capital, and the institutional link between religious authority and legal interpretation would all lose their organizing anchor; access to law and scripture would need to be rebuilt through whatever literacy system replaced it — which is precisely what did happen in the 1928 Turkish alphabet reform, in the sibling kemalist_rupture_reading of this same kernel.
% FOUNDING_PROBLEM: Establishing a stable, transmissible orthography for administering an Islamic empire whose legal and scholarly tradition was rooted in Arabic and Persian textual corpora, and whose legitimacy derived from continuity with the wider Islamic scholarly and juridical tradition.
% FOUNDING_PROBLEM_CORROBORATION: Ottoman-era literacy statistics (external to the ulema and bureaucratic beneficiaries) show literacy rates far below comparable Latin-script-adopting states of the period, corroborating the reform bureaucrats' contemporaneous complaints; comparative linguists external to the tradition affirm the orthographic mismatch with Turkish phonology independently of any identity claim. No corroboration from outside the beneficiary set affirms that the founding problem — administrative-religious continuity — required this specific script rather than any script capable of representing the same textual corpus, which is itself part of what remains contested.
narrative_ontology:disappearance_verdict(script_as_identity__ottoman_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(script_as_identity__ottoman_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(script_as_identity__ottoman_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(script_as_identity__ottoman_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(script_as_identity__ottoman_continuity_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(script_as_identity__ottoman_continuity_reading_tests).
:- end_tests(script_as_identity__ottoman_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.42) rather than high: the reading holds that script continuity performs a genuine coordination function (preserving legibility of an enormous accumulated textual and legal corpus, credentialing religious-legal authority through demonstrated mastery) and is not purely extractive cover. But the beneficiary structure is concentrated (ulema, bureaucracy, urban elite) while costs (persistent low literacy, gendered exclusion from transmission pathways) fall on a powerless, trapped population, which is why suppression is authored high (0.81): maintaining the script's constitutive status requires actively resisting orthographic reform proposals regardless of literacy-outcome evidence, because reform threatens the credentialing monopoly itself, not merely convenience.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (ulema, bureaucratic class, sufi orders, arabic-literate elite) sit near the low-d end: the arrangement subsidizes their institutional standing and they hold arbitrage or constrained exit precisely because their position inside the system is not something they need to escape. Victims (rural literacy seekers, excluded women, non-literate Turkish speakers) sit near the high-d end: trapped exit options, powerless standing, and no institutional channel to convert the 'continuity' claim into their own literacy access. The directionality gap here is exactly what the coordination-vs-extraction tension in tangled_rope requires: genuine coordination function for the credentialed class, real extraction borne by everyone outside it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a stable orthography for administering and transmitting an Islamic-Ottoman legal/scholarly tradition) is contested rather than flatly dead or live: the reading holds the problem remains live (continuity of religious-legal interpretive authority still depends on this specific textual tradition), while external literacy data corroborate that whatever the founding problem was, this script was not solving a literacy-access problem for the broader population it also governs. The tangled_rope classification, rather than snare, registers that a real coordination function persists for the credentialed seats even as extraction is authored for the excluded ones — collapsing this into pure extraction would erase the genuine textual-continuity function the ulema and bureaucratic class rely on; collapsing it into pure rope would erase the trapped, powerless victim seats this reading itself names.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutive_vs_instrumental_ambiguity,
    'Is Arabic script genuinely constitutive of Turkish-Islamic identity and religious-legal authority (such that changing script severs real institutional continuity), or is it an instrumental technology whose ''constitutive'' status is itself constructed and defended by the class that benefits from the credentialing monopoly it enables?',
    'Comparative analysis of script transitions in other Muslim-majority or Islamic-legal-tradition societies (e.g., Malay/Jawi to Rumi, Swahili Arabic to Latin) that assesses whether religious-legal continuity and institutional authority survived script change intact, partially, or not at all.',
    'If continuity demonstrably survives script transitions elsewhere without loss of religious-legal function, the constitutive claim in this reading is substantially weakened and the arrangement looks more like credentialing-monopoly protection than genuine identity constitution; if continuity does not survive, the reading''s core premise is corroborated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutive_vs_instrumental_ambiguity, conceptual, 'Whether script constitutiveness is a genuine structural fact or a defensible-but-constructed claim serving the credentialed beneficiary class.').

omega_variable(
    literacy_cost_attribution,
    'How much of the historically low Ottoman-Turkish literacy rate is attributable to the Arabic-Turkish orthographic mismatch specifically, versus other factors (rural poverty, absence of universal schooling infrastructure, deliberate elite non-diffusion of literacy)?',
    'Comparative literacy-rate analysis controlling for schooling infrastructure and socioeconomic factors across Ottoman provinces and comparable non-Ottoman states of the same period.',
    'If the orthographic mismatch is a minor factor relative to infrastructure and elite non-diffusion, the extraction attributed to the script itself in this story should be lower and more of the story''s suppression should be attributed to other institutional gatekeeping; if orthography is a major factor, the current extraction/suppression split is well-calibrated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(literacy_cost_attribution, empirical, 'Disentangling script-specific literacy cost from general Ottoman literacy-infrastructure deficits.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the ''kernel'' here best framed as the script itself (Arabic vs. Latin as competing orthographic technologies), or as the deeper claim of what continuity with Ottoman-Islamic institutional authority requires (a claim the script merely instantiates)? The obvious framing treats the script as the contested object; a less obvious framing treats the legitimacy claim of religious-legal authority as the actual kernel, with script choice as a downstream proxy fight.',
    'Trace whether Ottoman-era reform debates that proposed retaining Arabic script but reforming orthography (rather than switching scripts entirely) were treated as acceptable within the ulema''s authority framework, versus debates proposing full Latinization while retaining religious-legal continuity through translation. If orthographic reform-within-Arabic-script was tolerated but full Latinization was not, the deeper kernel is institutional authority, not script per se.',
    'If the deeper-authority framing is correct, this story''s cs_structure and reading_relations should be understood as one layer of a two-layer kernel structure (script-choice riding on top of authority-legitimacy), which would suggest a further decomposition rather than treating script_as_identity as the base kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether script choice is the kernel itself or a downstream proxy for a deeper religious-legal authority legitimacy claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(script_as_identity__ottoman_continuity_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scri_tr_t0, script_as_identity__ottoman_continuity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(scri_tr_t20, script_as_identity__ottoman_continuity_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement(scri_tr_t40, script_as_identity__ottoman_continuity_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(scri_tr_t60, script_as_identity__ottoman_continuity_reading, theater_ratio, 60, 0.23).
narrative_ontology:measurement(scri_tr_t80, script_as_identity__ottoman_continuity_reading, theater_ratio, 80, 0.26).
narrative_ontology:measurement(scri_tr_t100, script_as_identity__ottoman_continuity_reading, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(scri_be_t0, script_as_identity__ottoman_continuity_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(scri_be_t20, script_as_identity__ottoman_continuity_reading, base_extractiveness, 20, 0.33).
narrative_ontology:measurement(scri_be_t40, script_as_identity__ottoman_continuity_reading, base_extractiveness, 40, 0.37).
narrative_ontology:measurement(scri_be_t60, script_as_identity__ottoman_continuity_reading, base_extractiveness, 60, 0.4).
narrative_ontology:measurement(scri_be_t80, script_as_identity__ottoman_continuity_reading, base_extractiveness, 80, 0.41).
narrative_ontology:measurement(scri_be_t100, script_as_identity__ottoman_continuity_reading, base_extractiveness, 100, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(scri_su_t0, script_as_identity__ottoman_continuity_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(scri_su_t20, script_as_identity__ottoman_continuity_reading, suppression_requirement, 20, 0.73).
narrative_ontology:measurement(scri_su_t40, script_as_identity__ottoman_continuity_reading, suppression_requirement, 40, 0.75).
narrative_ontology:measurement(scri_su_t60, script_as_identity__ottoman_continuity_reading, suppression_requirement, 60, 0.77).
narrative_ontology:measurement(scri_su_t80, script_as_identity__ottoman_continuity_reading, suppression_requirement, 80, 0.79).
narrative_ontology:measurement(scri_su_t100, script_as_identity__ottoman_continuity_reading, suppression_requirement, 100, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(script_as_identity__ottoman_continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(script_as_identity__ottoman_continuity_reading, 0.1).
narrative_ontology:affects_constraint(script_as_identity__ottoman_continuity_reading, kemalist_rupture_reading).
narrative_ontology:affects_constraint(script_as_identity__ottoman_continuity_reading, phonetic_instrumentalism_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the script_as_identity kernel. kemalist_rupture_reading claims the opposite normative valence for the same historical script transition (rupture as liberation rather than loss); phonetic_instrumentalism_reading denies the constitutive premise altogether, treating script as neutral technology. Each reading authors its own ε against the arrangement it takes as its referent under its own lights: this reading's ε (0.42) describes the Ottoman-era Arabic-script arrangement as a partly-coordinating, partly-extractive credentialing structure; it is not commensurable with, and should not be averaged against, the siblings' ε values for what may be structurally different referent arrangements.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
