% ============================================================================
% CONSTRAINT STORY: turkish_graphemic_substrate__ottoman_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_turkish_graphemic_substrate__ottoman_continuity_reading, []).

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
 *   constraint_id: turkish_graphemic_substrate__ottoman_continuity_reading
 *   human_readable: Ottoman-Continuity Reading of Turkish Graphemic Legitimacy (Arabic Script)
 *   domain: political_linguistics/state_formation/cultural_engineering
 *
 * SUMMARY:
 *   This constraint represents the ottoman_continuity_reading of the
 *   contested turkish_graphemic_substrate kernel: the claim that Turkish
 *   linguistic identity is continuous with Ottoman-Islamic civilization and
 *   that Arabic script is the sole legitimate graphemic substrate for that
 *   identity. Under this reading, the Ottoman literary canon, the
 *   religious-legal interpretive apparatus, and the pan-Islamic
 *   civilizational frame all remain intact and mutually reinforcing, because
 *   they are all denominated in the same script and the same institutional
 *   credentialing structure (madrasa, ulema, waqf). The reading is generated
 *   here as a single, ε-invariant constraint on its own terms — its endorsed
 *   continuity, not any rival script's costs or benefits, is what is
 *   evaluated. Sibling readings (secular_nationalist_reading,
 *   gradual_transition_reading) are separate constraint files with their own
 *   ε and stakeholder structures; they are referenced only via network links
 *   and omega variables, never folded into this story's classification.
 *
 * KEY AGENTS:
 *   - ulema_religious_establishment: institutional agenda-setter and beneficiary — controls interpretive and educational credentialing
 *   - ottoman_literary_elite: powerful beneficiary — cultural capital denominated in the existing script
 *   - sufi_orders: organized beneficiary — transmission chains anchored in Arabic-script manuscripts
 *   - traditional_madrasa_network: institutional agenda-setter and beneficiary — operates the schooling gate
 *   - rural_turkish_speaking_peasantry: powerless payer — locked out of literacy by script-phonetic mismatch
 *   - women_excluded_from_religious_schooling: powerless payer — doubly excluded from continuity and literacy
 *   - non_arabic_literate_provincial_administrators: moderate payer — career ceiling from script gatekeeping
 *   - print_publishers_and_early_literacy_reformers: excluded voice — no institutional standing to press reform
 *   - future_historians_and_linguists: analytical observer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(turkish_graphemic_substrate__ottoman_continuity_reading, 0.58).
domain_priors:suppression_score(turkish_graphemic_substrate__ottoman_continuity_reading, 0.44).
domain_priors:theater_ratio(turkish_graphemic_substrate__ottoman_continuity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(turkish_graphemic_substrate__ottoman_continuity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__ottoman_continuity_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__ottoman_continuity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(turkish_graphemic_substrate__ottoman_continuity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__ottoman_continuity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(turkish_graphemic_substrate__ottoman_continuity_reading, tangled_rope).
narrative_ontology:human_readable(turkish_graphemic_substrate__ottoman_continuity_reading, "Ottoman-Continuity Reading of Turkish Graphemic Legitimacy (Arabic Script)").
narrative_ontology:topic_domain(turkish_graphemic_substrate__ottoman_continuity_reading, "political_linguistics/state_formation/cultural_engineering").

domain_priors:requires_active_enforcement(turkish_graphemic_substrate__ottoman_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(turkish_graphemic_substrate__ottoman_continuity_reading, 'b6163798-6454-4c75-a95a-b153533817d0').
narrative_ontology:cs_kernel_codification('b6163798-6454-4c75-a95a-b153533817d0', fixed_text).
narrative_ontology:cs_authority_grounding('b6163798-6454-4c75-a95a-b153533817d0', lineage).
narrative_ontology:cs_interpretation_layer_present('b6163798-6454-4c75-a95a-b153533817d0').
narrative_ontology:cs_reading_relation('b6163798-6454-4c75-a95a-b153533817d0', turkish_graphemic_substrate__secular_nationalist_reading, forecloses).
narrative_ontology:cs_reading_relation('b6163798-6454-4c75-a95a-b153533817d0', turkish_graphemic_substrate__gradual_transition_reading, influences).
narrative_ontology:cs_axiom('b6163798-6454-4c75-a95a-b153533817d0', foundational, ottoman_islamic_civilizational_continuity).
narrative_ontology:cs_axiom_status(ottoman_islamic_civilizational_continuity, holdable).
narrative_ontology:cs_axiom_grounding('b6163798-6454-4c75-a95a-b153533817d0', ottoman_islamic_civilizational_continuity, conventional).
narrative_ontology:cs_axiom('b6163798-6454-4c75-a95a-b153533817d0', foundational, arabic_script_as_sole_legitimate_substrate).
narrative_ontology:cs_axiom_status(arabic_script_as_sole_legitimate_substrate, overridden).
narrative_ontology:cs_axiom_grounding('b6163798-6454-4c75-a95a-b153533817d0', arabic_script_as_sole_legitimate_substrate, conventional).
narrative_ontology:cs_axiom('b6163798-6454-4c75-a95a-b153533817d0', secondary, religious_credentialing_requires_script_continuity).
narrative_ontology:cs_axiom_status(religious_credentialing_requires_script_continuity, holdable).
narrative_ontology:cs_axiom_grounding('b6163798-6454-4c75-a95a-b153533817d0', religious_credentialing_requires_script_continuity, instrumental).
narrative_ontology:cs_reference_frame('b6163798-6454-4c75-a95a-b153533817d0', islamic_ecumene_textual_continuity).
narrative_ontology:cs_drift_state('b6163798-6454-4c75-a95a-b153533817d0', post_1928_alphabet_reform, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('b6163798-6454-4c75-a95a-b153533817d0', '').
narrative_ontology:cs_kernel_id(turkish_graphemic_substrate__ottoman_continuity_reading, turkish_graphemic_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__ottoman_continuity_reading, ulema_religious_establishment).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__ottoman_continuity_reading, ottoman_literary_elite).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__ottoman_continuity_reading, sufi_orders).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__ottoman_continuity_reading, traditional_madrasa_network).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__ottoman_continuity_reading, rural_turkish_speaking_peasantry).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__ottoman_continuity_reading, women_excluded_from_religious_schooling).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__ottoman_continuity_reading, non_arabic_literate_provincial_administrators).
narrative_ontology:constraint_vindicates(turkish_graphemic_substrate__ottoman_continuity_reading, civilizational_continuity_doctrine).
narrative_ontology:constraint_vindicates(turkish_graphemic_substrate__ottoman_continuity_reading, islamic_ecumene_membership).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls Quranic and madrasa education, fatwa issuance, and the interpretive apparatus that renders Ottoman-Islamic legal, theological, and literary corpus authoritative. Arabic script literacy is the credentialing mechanism for religious and juridical authority; its continued legitimacy directly preserves the ulema's monopoly on interpretation and its social standing.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, ulema_religious_establishment, agenda_setter,
    institutional, civilizational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(turkish_graphemic_substrate__ottoman_continuity_reading, ulema_religious_establishment, beneficiary).

% Court poets, chroniclers, and bureaucratic scribes whose accumulated cultural capital is denominated in Ottoman Turkish written in the Arabic-Persian script tradition. Their prestige, patronage networks, and readership depend on the script remaining the recognized literary substrate; a script change would devalue decades of accumulated authorship and training.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, ottoman_literary_elite, beneficiary,
    powerful, generational, mobile, regional).

% Transmit devotional and mystical texts through hereditary and initiatory chains that are textually anchored in Arabic-script manuscripts. Their institutional continuity and the perceived unbroken chain of transmission (silsila) rest on the reading and copying of these documents remaining a living practice, not an archival one.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, sufi_orders, beneficiary,
    organized, civilizational, constrained, regional).

% Operates the schooling infrastructure that both requires and reproduces Arabic-script literacy as the entry credential to religious, legal, and much civil administrative employment. Continuation of the constraint sustains enrollment, endowment revenue (waqf), and social relevance of the network.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, traditional_madrasa_network, beneficiary,
    institutional, civilizational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(turkish_graphemic_substrate__ottoman_continuity_reading, traditional_madrasa_network, agenda_setter).

% Speak Turkish but have no realistic access to the years of specialized instruction required to become literate in Arabic script, whose orthography poorly represents Turkish vowel harmony. Under this reading they remain functionally illiterate for life, locked out of legal documents, correspondence, and print media that could otherwise be made accessible through a phonetically matched script.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, rural_turkish_speaking_peasantry, payer,
    powerless, biographical, trapped, local).

% Largely barred from madrasa instruction that would confer Arabic-script literacy, they are doubly excluded — from the religious-civilizational continuity this reading celebrates and from any literacy at all, since the reading resists the phonetic simplification that could have widened access outside formal religious schooling.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, women_excluded_from_religious_schooling, payer,
    powerless, generational, trapped, local).

% Capable civil functionaries whose administrative competence does not translate into career advancement without costly script literacy, since the reading ties bureaucratic legitimacy to the same graphemic substrate as religious authority. They bear a career ceiling that competitors trained in the madrasa system do not face.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, non_arabic_literate_provincial_administrators, payer,
    moderate, biographical, constrained, regional).

% Advocate script reform or supplementary phonetic systems to expand mass literacy and print circulation. Under the Ottoman-continuity reading their proposals are treated as civilizational rupture rather than technical improvement, and they have no institutional standing to press the case within religious or court structures.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, print_publishers_and_early_literacy_reformers, excluded,
    moderate, generational, constrained, regional).

% Assess, from outside the contest, whether continuity claims track genuine cultural function or serve incumbent institutional interests, and compare literacy outcomes across the three kernel readings once each was or was not adopted.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, future_historians_and_linguists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves a single interpretive and educational apparatus (script, corpus, credentialing) that lets religious law, devotional practice, and administrative precedent remain mutually legible across generations without requiring costly re-transcription or reinterpretation of the existing corpus.
% TRANSFER_FUNCTION: Moves literacy access, administrative advancement, and interpretive authority toward those already positioned within religious and court educational institutions, and away from the rural, non-elite, and female population who cannot access that institutional pathway.
% ABSENT_VOICES: Rural Turkish speakers, women excluded from religious schooling, and print-literacy reformers advocating phonetic scripts have no seat in the ulema-court-madrasa nexus that adjudicates graphemic legitimacy; their objections surface only later, externally, through nationalist reform movements.
% DISAPPEARANCE_RATIONALE: If the Ottoman-continuity reading's institutional hold collapsed, madrasa credentialing, waqf-funded religious education, and court literary patronage built on Arabic-script exclusivity would lose their exclusive gatekeeping function; mass literacy campaigns and civil administration could reorganize around a phonetically matched script, as later happened under the rival reading's ascendance.
% FOUNDING_PROBLEM: How to maintain interpretive and legal continuity with an accumulated Islamic-Ottoman textual and legal corpus across generations, ensuring jurists, scribes, and religious authorities could read, cite, and extend centuries of prior rulings and literature without rupture.
% FOUNDING_PROBLEM_CORROBORATION: The ulema and madrasa network attest the problem remains live — Quranic recitation and classical jurisprudence still require Arabic-script literacy for authenticity. Outside observers — comparative literacy statistics from the 1928 Turkish reform and independent linguistic analyses of Arabic script's phonetic mismatch with Turkish vowel harmony — corroborate that the same continuity could have been partially preserved through translation and specialist training without gatekeeping mass civil literacy on the same script.
narrative_ontology:disappearance_verdict(turkish_graphemic_substrate__ottoman_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(turkish_graphemic_substrate__ottoman_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(turkish_graphemic_substrate__ottoman_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(turkish_graphemic_substrate__ottoman_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(turkish_graphemic_substrate__ottoman_continuity_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(turkish_graphemic_substrate__ottoman_continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(turkish_graphemic_substrate__ottoman_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(turkish_graphemic_substrate__ottoman_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate-high 0.58: the coordination function (preserving legal and religious interpretive continuity) is genuine, but it rides on an asymmetric structure that channels literacy access and administrative advancement toward an already-credentialed religious-literary elite while locking out the rural and female population from any literacy at all, since the reading resists phonetic simplification that would widen access. Suppression is moderate (0.44) — enforcement here is largely institutional inertia and credentialing gatekeeping rather than direct coercion, though court and religious authority backed it. Accessibility collapse is moderate (0.4): alternatives (phonetic scripts, translated corpora) existed and were technically known even within the period, so the collapse is institutional choice, not natural necessity. Resistance is substantial (0.55) reflecting growing print-reform and literacy-expansion pressure across the measured interval.
 *
 * PERSPECTIVAL GAP:
 *   From the ulema/madrasa seat, this arrangement is functioning coordination — an unbroken interpretive chain preserving legal and devotional continuity across centuries. From the rural peasantry and excluded women's seat, the identical arrangement operates as an extractive literacy barrier with no offsetting benefit, since they were never positioned to access the religious-educational credentialing that makes the corpus continuity meaningful to them. The tangled_rope classification is meant to hold both readings as structurally true simultaneously rather than resolving them into a single verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   The ulema, madrasa network, literary elite, and Sufi orders are declared beneficiaries because their institutional standing, credentialing monopoly, and cultural capital are directly denominated in Arabic-script literacy remaining the sole legitimate substrate — the engine should derive low d (near-beneficiary) for these seats. Rural peasantry, excluded women, and provincial administrators are declared victims/payers because the same script requirement forecloses their literacy or career mobility without offering them any of the corresponding institutional access — the engine should derive high d (near-target) for these seats, amplified by their trapped or constrained exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — maintaining interpretive continuity with an accumulated Islamic-Ottoman legal and literary corpus — was genuinely live at the constraint's origin and remains partially live for specialist religious and legal scholarship today. But the constraint's application to mass civil literacy (rather than specialist religious training alone) increasingly outlived that founding problem: once print technology and administrative modernization created a plausible route to broad literacy via a phonetically matched script, continuing to gate ALL literacy (not just specialist religious literacy) through the Arabic-script/Ottoman-continuity apparatus shifted the constraint's function from genuine cultural transmission toward incumbent credentialing protection. Classifying this as tangled_rope rather than snare or mountain avoids two errors: treating it as pure natural civilizational necessity (mountain, ignoring the real victims) or as purely cynical extraction with no genuine coordination content (snare, ignoring the real interpretive-continuity function it performed for specialist religious and legal transmission).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    continuity_claim_vs_incumbent_protection,
    'Is the Ottoman-continuity reading''s insistence on Arabic script primarily a genuine claim about civilizational and interpretive continuity, or primarily a mechanism protecting the ulema/madrasa network''s credentialing monopoly and the literary elite''s accumulated cultural capital?',
    'Compare literacy outcomes and interpretive-continuity outcomes in cases where specialist religious/legal training retained Arabic-script instruction while mass civil literacy moved to a phonetic script (a natural experiment partially realized by later Turkish transliteration projects and by other Islamicate societies that retained diglossic script arrangements). If specialist continuity survived undiminished under partial script separation, the mass-literacy application of this reading was primarily incumbent protection, not necessity.',
    'If resolved toward incumbent protection, the tangled_rope classification is strongly supported and the beneficiary/victim asymmetry deepens; if resolved toward genuine indivisible necessity, the constraint moves closer to a scaffold or contested coordination mechanism with weaker extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuity_claim_vs_incumbent_protection, conceptual, 'Whether Arabic-script exclusivity for mass literacy was necessary to continuity or primarily protected incumbent religious-literary institutions.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly do the three kernel readings (ottoman_continuity, secular_nationalist, gradual_transition) structurally diverge — is it the empirical claim about civilizational continuity, the normative claim about which script best serves modernization, or the practical claim about transition costs?',
    'This is committer structure, not resolvable within a single reading. Documented here per Rule 2: the ottoman_continuity_reading and secular_nationalist_reading disagree at the level of foundational premise (is Turkish identity continuous with or distinct from the Ottoman-Islamic past — a direct contradiction). The gradual_transition_reading disagrees at the level of policy instrument (does the switch need to be immediate/exclusive or phased), accepting elements of both underlying identity claims without committing to either exclusively.',
    'If the disagreement is genuinely at the foundational-premise level between ottoman_continuity and secular_nationalist, no single legal or educational framework can hold both simultaneously (supports a forecloses relation between those two specific readings). The gradual_transition_reading''s instrumental disagreement supports an influences or coexists_with relation rather than forecloses.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Locating where the three sibling readings of the graphemic-substrate kernel actually diverge structurally.').

omega_variable(
    gender_exclusion_naturalization_check,
    'Is the exclusion of women from the madrasa-based literacy pathway treated within this reading as an incidental social fact or as a naturalized, near-permanent feature of the continuity claim itself?',
    'Examine whether Ottoman-continuity advocacy literature and later apologetics for the reading treat female literacy exclusion as a contingent policy failure worth remedying within the reading, or as consistent with / required by the civilizational continuity the reading defends.',
    'If naturalized within the reading''s own framework, accessibility_collapse should be scored higher for this sub-population than the story''s aggregate figure suggests, sharpening the tangled_rope''s victim asymmetry specifically along gender lines.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gender_exclusion_naturalization_check, empirical, 'Whether female literacy exclusion is incidental or structurally naturalized within the Ottoman-continuity reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(turkish_graphemic_substrate__ottoman_continuity_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(turk_tr_t0, turkish_graphemic_substrate__ottoman_continuity_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(turk_tr_t10, turkish_graphemic_substrate__ottoman_continuity_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(turk_tr_t20, turkish_graphemic_substrate__ottoman_continuity_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(turk_tr_t30, turkish_graphemic_substrate__ottoman_continuity_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement(turk_tr_t40, turkish_graphemic_substrate__ottoman_continuity_reading, theater_ratio, 40, 0.26).
narrative_ontology:measurement(turk_tr_t50, turkish_graphemic_substrate__ottoman_continuity_reading, theater_ratio, 50, 0.27).
narrative_ontology:measurement(turk_tr_t60, turkish_graphemic_substrate__ottoman_continuity_reading, theater_ratio, 60, 0.28).

% Extraction over time
narrative_ontology:measurement(turk_be_t0, turkish_graphemic_substrate__ottoman_continuity_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(turk_be_t10, turkish_graphemic_substrate__ottoman_continuity_reading, base_extractiveness, 10, 0.46).
narrative_ontology:measurement(turk_be_t20, turkish_graphemic_substrate__ottoman_continuity_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(turk_be_t30, turkish_graphemic_substrate__ottoman_continuity_reading, base_extractiveness, 30, 0.53).
narrative_ontology:measurement(turk_be_t40, turkish_graphemic_substrate__ottoman_continuity_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(turk_be_t50, turkish_graphemic_substrate__ottoman_continuity_reading, base_extractiveness, 50, 0.57).
narrative_ontology:measurement(turk_be_t60, turkish_graphemic_substrate__ottoman_continuity_reading, base_extractiveness, 60, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(turk_su_t0, turkish_graphemic_substrate__ottoman_continuity_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(turk_su_t10, turkish_graphemic_substrate__ottoman_continuity_reading, suppression_requirement, 10, 0.33).
narrative_ontology:measurement(turk_su_t20, turkish_graphemic_substrate__ottoman_continuity_reading, suppression_requirement, 20, 0.36).
narrative_ontology:measurement(turk_su_t30, turkish_graphemic_substrate__ottoman_continuity_reading, suppression_requirement, 30, 0.39).
narrative_ontology:measurement(turk_su_t40, turkish_graphemic_substrate__ottoman_continuity_reading, suppression_requirement, 40, 0.41).
narrative_ontology:measurement(turk_su_t50, turkish_graphemic_substrate__ottoman_continuity_reading, suppression_requirement, 50, 0.43).
narrative_ontology:measurement(turk_su_t60, turkish_graphemic_substrate__ottoman_continuity_reading, suppression_requirement, 60, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(turkish_graphemic_substrate__ottoman_continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(turkish_graphemic_substrate__ottoman_continuity_reading, 0.1).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__ottoman_continuity_reading, secular_nationalist_reading).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__ottoman_continuity_reading, gradual_transition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the turkish_graphemic_substrate kernel. Each reading is authored as an independent, ε-invariant constraint per DP-001: ottoman_continuity_reading (ε=0.58, tangled_rope, this file), secular_nationalist_reading (a separate file with its own ε and stakeholder structure, claiming Latin script as legitimate and Ottoman continuity as illegitimate), and gradual_transition_reading (a separate file authoring a scaffold-shaped managed-transition arrangement). The three do not share ε or classification; they are linked here so contamination/propagation analysis can trace how one reading's institutional collapse (e.g., the 1928 alphabet reform reflecting an ascendant secular_nationalist_reading) restructures the resource and legitimacy conditions for the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
