% ============================================================================
% CONSTRAINT STORY: script_as_identity__ottoman_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: script_as_identity__ottoman_continuity_reading
 *   human_readable: Arabic Script as Constitutive of Turkish-Islamic Identity (Ottoman Continuity Reading)
 *   domain: linguistics/political_authority/religious_institutions
 *
 * SUMMARY:
 *   This story instantiates one reading of a contested kernel: whether the
 *   script Turkish is written in bears constitutively on Turkish-Islamic
 *   identity, or is a substitutable technical medium. Under the Ottoman
 *   continuity reading, Arabic script is treated as the vessel of an unbroken
 *   chain of religious, legal, and administrative transmission stretching
 *   back through Ottoman and classical Islamic tradition; abandoning it is
 *   not a technical simplification but a rupture with that chain. This
 *   reading is defended most forcefully by the institutions whose authority
 *   is anchored in Arabic-script literacy: the ulema, the sufi orders, the
 *   bureaucratic literati, and the broader Arabic-literate elite. As authored
 *   here, this is a distinct constraint from the sibling readings — it has
 *   its own beneficiary/victim structure, its own extraction profile, and its
 *   own persistence mechanism (defense of interpretive and administrative
 *   monopoly), and should not be blended with the kemalist rupture reading
 *   (which treats the same script choice as an obstacle to secular
 *   modernization) or the phonetic instrumentalist reading (which treats
 *   script as ideologically neutral technology). Those are different
 *   constraints, linked by network edges, not alternate measurements of this
 *   one.
 *
 * KEY AGENTS:
 *   - ulema_religious_establishment: Primary agenda-setter and beneficiary (institutional/arbitrage) — administers religious-legal authority gated by Arabic-script literacy
 *   - ottoman_bureaucratic_literati: Beneficiary (powerful/constrained) — career monopoly built on composite Ottoman Turkish literacy
 *   - rural_turkish_speakers: Primary target (powerless/trapped) — locked out of literacy by orthographic-phonetic mismatch
 *   - women_excluded_from_religious_education: Target (powerless/trapped) — structurally barred from the literacy that confers authority
 *   - comparative_script_historians: Analytical observer — traces literacy outcomes and institutional function across script regimes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(script_as_identity__ottoman_continuity_reading, 0.58).
domain_priors:suppression_score(script_as_identity__ottoman_continuity_reading, 0.71).
domain_priors:theater_ratio(script_as_identity__ottoman_continuity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(script_as_identity__ottoman_continuity_reading, tangled_rope).
narrative_ontology:human_readable(script_as_identity__ottoman_continuity_reading, "Arabic Script as Constitutive of Turkish-Islamic Identity (Ottoman Continuity Reading)").
narrative_ontology:topic_domain(script_as_identity__ottoman_continuity_reading, "linguistics/political_authority/religious_institutions").

domain_priors:requires_active_enforcement(script_as_identity__ottoman_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(script_as_identity__ottoman_continuity_reading, 'd58f5368-1db3-4b62-8c94-6cc0a296b83a').
narrative_ontology:cs_kernel_codification('d58f5368-1db3-4b62-8c94-6cc0a296b83a', fixed_text).
narrative_ontology:cs_authority_grounding('d58f5368-1db3-4b62-8c94-6cc0a296b83a', lineage).
narrative_ontology:cs_interpretation_layer_present('d58f5368-1db3-4b62-8c94-6cc0a296b83a').
narrative_ontology:cs_reading_relation('d58f5368-1db3-4b62-8c94-6cc0a296b83a', script_as_identity__kemalist_rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('d58f5368-1db3-4b62-8c94-6cc0a296b83a', script_as_identity__phonetic_instrumentalism_reading, influences).
narrative_ontology:cs_axiom('d58f5368-1db3-4b62-8c94-6cc0a296b83a', foundational, script_continuity_constitutes_religious_legal_legitimacy).
narrative_ontology:cs_axiom_status(script_continuity_constitutes_religious_legal_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('d58f5368-1db3-4b62-8c94-6cc0a296b83a', script_continuity_constitutes_religious_legal_legitimacy, deontological).
narrative_ontology:cs_axiom('d58f5368-1db3-4b62-8c94-6cc0a296b83a', secondary, transmission_chain_requires_orthographic_identity_not_mere_translatability).
narrative_ontology:cs_axiom_status(transmission_chain_requires_orthographic_identity_not_mere_translatability, holdable).
narrative_ontology:cs_axiom_grounding('d58f5368-1db3-4b62-8c94-6cc0a296b83a', transmission_chain_requires_orthographic_identity_not_mere_translatability, conventional).
narrative_ontology:cs_reference_frame('d58f5368-1db3-4b62-8c94-6cc0a296b83a', unbroken_ottoman_islamic_textual_transmission).
narrative_ontology:cs_drift_state('d58f5368-1db3-4b62-8c94-6cc0a296b83a', late_ottoman_reform_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('d58f5368-1db3-4b62-8c94-6cc0a296b83a', '').
narrative_ontology:cs_kernel_id(script_as_identity__ottoman_continuity_reading, script_as_identity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(script_as_identity__ottoman_continuity_reading, ulema_religious_establishment).
narrative_ontology:constraint_beneficiary(script_as_identity__ottoman_continuity_reading, ottoman_bureaucratic_literati).
narrative_ontology:constraint_beneficiary(script_as_identity__ottoman_continuity_reading, sufi_orders).
narrative_ontology:constraint_beneficiary(script_as_identity__ottoman_continuity_reading, arabic_literate_elite).
narrative_ontology:constraint_victim(script_as_identity__ottoman_continuity_reading, rural_turkish_speakers).
narrative_ontology:constraint_victim(script_as_identity__ottoman_continuity_reading, women_excluded_from_religious_education).
narrative_ontology:constraint_victim(script_as_identity__ottoman_continuity_reading, non_elite_literacy_seekers).
narrative_ontology:constraint_vindicates(script_as_identity__ottoman_continuity_reading, script_continuity_preserves_institutional_memory).
narrative_ontology:constraint_vindicates(script_as_identity__ottoman_continuity_reading, religious_textual_authority_requires_arabic_orthography).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls religious and legal interpretation through exclusive command of Arabic-script Ottoman Turkish and classical Arabic texts. Administers madrasa education, issues fatwas, and adjudicates sharia matters. Arabic script literacy is the credential that gates entry into this authority structure; the establishment enforces script continuity through control of religious schooling and canonical text transmission, and its institutional standing depends on the script remaining the exclusive medium of religious and legal legitimacy.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, ulema_religious_establishment, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(script_as_identity__ottoman_continuity_reading, ulema_religious_establishment, beneficiary).

% Scribes, court officials, and administrators whose careers depend on mastery of Ottoman Turkish's Arabic-Persian-Turkish admixture written in Arabic script. This composite literacy is scarce, took years to acquire, and forms the basis of their bureaucratic monopoly. A script change would devalue their accumulated human capital and open administrative posts to newly literate cohorts.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, ottoman_bureaucratic_literati, beneficiary,
    powerful, generational, constrained, national).

% Maintain devotional and mystical literature, lodge records, and initiatory texts in Arabic script; the script itself carries devotional weight (calligraphy as spiritual practice, numerological readings of letterforms). Their continuity as institutions is bound to script continuity in a way that is not purely practical but constitutive of practice itself.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, sufi_orders, beneficiary,
    organized, civilizational, constrained, regional).

% Merchants, landowners, and provincial notables literate in Arabic script who use that literacy to access legal documents, religious texts, and correspondence with the wider Islamic world (Cairo, Damascus, Mecca). Script continuity preserves their existing advantage and their connection to a transnational Islamic literate culture.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, arabic_literate_elite, beneficiary,
    powerful, generational, mobile, national).

% Turkish speakers with no access to the years of study required to master Arabic-script Ottoman orthography, which does not map cleanly onto Turkish's vowel system. They remain functionally illiterate not for lack of a spoken language but because the script demanded for full civic and religious participation is a poor phonetic fit for their spoken Turkish. Literacy under this regime is priced out of reach for the majority.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, rural_turkish_speakers, payer,
    powerless, biographical, trapped, regional).

% Systematically excluded from madrasa education that would confer Arabic-script literacy, and thus excluded from the religious and legal authority structures that literacy gates. The script's constitutive role in identity and continuity is asserted by an establishment they cannot enter to contest.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, women_excluded_from_religious_education, payer,
    powerless, biographical, trapped, regional).

% Urban artisans, small traders, and others who would benefit from mass literacy for commerce and civic participation but face a script whose acquisition cost (compounded by Ottoman Turkish's mixed vocabulary) keeps literacy rates low across the empire relative to phonetically transparent alternatives. They bear the ongoing cost of a literacy bottleneck defended as identity-preservation.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, non_elite_literacy_seekers, payer,
    powerless, biographical, constrained, national).

% Late Ottoman modernizers who argued for script reform or simplification to raise literacy and administrative efficiency were marginalized within a discourse that treated script continuity as inseparable from religious and dynastic legitimacy; their proposals could not be seriously entertained within the ulema-dominated framing of what the script meant.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, reformist_bureaucrats_and_intellectuals, excluded,
    organized, generational, constrained, national).

% Study script reform movements across the late Ottoman and early Turkish Republic period, comparing literacy outcomes, institutional continuity, and the political functions served by treating a script as constitutive of identity rather than as a contingent technology.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, comparative_script_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(script_as_identity__ottoman_continuity_reading, diffuse).
narrative_ontology:fixing_cost_class(script_as_identity__ottoman_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, shared orthographic medium that links Ottoman Turkish administrative, legal, and religious documents across centuries, allowing continuity of legal precedent, religious scholarship, and bureaucratic record-keeping without translation loss.
% TRANSFER_FUNCTION: Moves literacy-gated authority (religious interpretation, legal adjudication, administrative office, transnational Islamic textual access) toward those already possessing costly Arabic-script literacy, and moves the burden of exclusion onto rural Turkish speakers, women, and non-elite literacy seekers who cannot afford the acquisition cost.
% ABSENT_VOICES: Rural Turkish speakers and women excluded from religious education would object that the script's difficulty is not incidental but functions as a gatekeeping mechanism; they are not represented in the ulema-dominated discourse that frames script continuity as identity-preservation rather than as a literacy barrier.
% DISAPPEARANCE_RATIONALE: If Arabic script ceased to be treated as constitutive of identity and continuity, the ulema's exclusive interpretive authority, the bureaucratic literati's career monopoly, and the transnational literate elite's advantage would all lose their institutional anchor; mass literacy campaigns using a phonetically transparent script would become far more feasible, restructuring who can access religious, legal, and civic participation.
% FOUNDING_PROBLEM: Islamic religious and legal authority in the Ottoman world required continuity with an unbroken chain of textual transmission from the Quran and classical Arabic-Islamic jurisprudence; Arabic script was the medium through which that unbroken chain was maintained and verified.
% FOUNDING_PROBLEM_CORROBORATION: The ulema and sufi orders attest the founding problem (preservation of unbroken religious-textual transmission) remains live and central. Independent historians of Ottoman literacy and reformist bureaucrats of the late empire attested even before the Republican reform that the practical literacy-transmission problem had become severable from script choice, and that script defense had become substantially about preserving interpretive monopoly rather than preserving access to the textual tradition itself, which could in principle be transliterated.
narrative_ontology:disappearance_verdict(script_as_identity__ottoman_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(script_as_identity__ottoman_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(script_as_identity__ottoman_continuity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(script_as_identity__ottoman_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(script_as_identity__ottoman_continuity_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored at a moderate-high 0.58: the coordination function (preserving an unbroken religious-legal-administrative textual tradition) is genuine, but it runs through a literacy bottleneck that concentrates interpretive and administrative authority in a narrow class while imposing a real, non-trivial acquisition cost on everyone else, particularly Turkish speakers for whom the Arabic abjad is a poor phonetic fit. Suppression is authored higher (0.71) and rising over the interval because as literacy movements and administrative reform pressure grew across the late Ottoman period, defending script continuity increasingly required active doctrinal and institutional argument rather than passive inheritance — the suppression_requirement series models this hardening. Theater ratio rises moderately (0.42 at interval end) reflecting that some defense of the script became performative reassertion of identity-continuity rather than functional necessity, though the underlying religious-textual function remains real, not fabricated — this is not a pure piton.
 *
 * PERSPECTIVAL GAP:
 *   From the ulema's seat, script continuity is the coordination structure itself — the thing that makes religious and legal authority legible and legitimate across generations. From the seat of rural Turkish speakers or women excluded from religious education, the same arrangement operates as a closed gate: the 'identity' being preserved is inseparable from the exclusion that makes their access to authority impossible. The engine computes these as different seat-level classifications from the same structural data; the divergence is not a contradiction to resolve but the measurement itself.
 *
 * DIRECTIONALITY LOGIC:
 *   The ulema, sufi orders, bureaucratic literati, and Arabic-literate elite are declared beneficiaries: each holds the scarce literacy the constraint requires, and each derives standing, income, or spiritual authority from a barrier that non-elites cannot easily cross. Rural Turkish speakers, women, and non-elite literacy seekers are declared victims: excluded not by choice but by the compounding costs of the script's poor phonetic fit for spoken Turkish and, for women, outright exclusion from the institutions that teach it. Exit options track this: beneficiaries hold arbitrage-to-mobile exit (their literacy is portable across the Islamic world), while the powerless payer groups are trapped or constrained.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preserving unbroken religious-textual transmission — was genuinely live at the script's founding as a functional problem. By the late Ottoman period, however, it had become at least partially severable: transliteration and orthographic reform proposals existed and were technically viable without severing access to the textual tradition, yet script defense persisted, increasingly through active argument (rising suppression_requirement) tied to interpretive monopoly rather than to transmission integrity per se. The tangled_rope classification captures this: coordination and extraction sitting inside the same structure, both real, requiring active enforcement to hold together, rather than either a pure Mountain (natural, non-agentic) or pure Snare (no genuine coordination function).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disambiguation,
    'Is ''Arabic script and Turkish-Islamic identity'' one constraint measurable multiple ways, or three structurally distinct constraints (Ottoman continuity, Kemalist rupture, phonetic instrumentalism) that happen to share a natural-language label?',
    'This story treats it as the latter, per the ε-invariance principle: each reading is authored as its own constraint with its own ε, beneficiary/victim structure, and network linkage rather than as alternate measurements of one constraint.',
    'If treated as one constraint with an observable-dependent ε, the classification would be incoherent (ε would shift depending on whether one measured literacy access, religious continuity, or phonetic transparency). Decomposing into three linked stories preserves a single stable ε per story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disambiguation, conceptual, 'Whether the Ottoman continuity reading is a separate constraint from its sibling readings or one constraint viewed differently.').

omega_variable(
    constitutive_vs_instrumental_script_claim,
    'Is Arabic script genuinely constitutive of Turkish-Islamic identity (such that changing it changes the identity itself), or is the constitutive claim itself a legitimating argument produced by those whose authority depends on script continuity?',
    'Comparative analysis of Islamic communities that adopted Latin, Cyrillic, or other scripts (e.g. Bosnian Muslims, Central Asian Turkic peoples post-Soviet) and assessment of whether their religious and cultural continuity was in fact severed or substantially preserved through transliteration and institutional adaptation.',
    'If continuity proves substantially preservable across script changes, the ''constitutive'' claim is better modeled as ideological cover for a literacy-gated authority structure (strengthening the tangled_rope/snare reading); if continuity proves to depend intrinsically on the script, the coordination function is more genuinely load-bearing (strengthening a closer-to-rope reading).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutive_vs_instrumental_script_claim, conceptual, 'Whether the constitutive-identity claim is empirically load-bearing or primarily legitimating.').

omega_variable(
    reading_framing_alternative_kernel_element,
    'Is the kernel here best framed as ''the script itself'' (the physical orthographic system) or as ''the interpretive authority structure layered on top of the script'' (who is licensed to read/write/adjudicate using it)? The obvious framing treats Arabic script as the kernel; a less obvious framing treats the ulema''s exclusive interpretive license as the actual kernel, with script merely its visible marker.',
    'Trace whether historical script-simplification proposals (e.g. simplified Arabic orthographies for Turkish proposed by some late Ottoman reformers) were resisted primarily on script-continuity grounds or on interpretive-authority grounds; if resistance tracked authority loss more than script loss, the authority-layer framing is the truer kernel.',
    'Under the script-as-kernel framing, this story''s cs_pattern centers on kernel_codification=fixed_text (the Arabic-script Ottoman/Quranic corpus). Under the authority-layer framing, the kernel would center more heavily on lineage/practice grounding independent of the specific orthography, which could shift the reading_relations analysis, particularly the ''forecloses'' judgment against phonetic_instrumentalism_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_framing_alternative_kernel_element, conceptual, 'Whether the true kernel is the script itself or the interpretive authority structure that uses the script as its marker.').

omega_variable(
    literacy_barrier_intentionality,
    'Was the Arabic-script literacy barrier for Turkish speakers a foreseen and actively maintained feature of the constraint, or an unintended side effect of a script chosen for religious continuity reasons with no attention to phonetic fit?',
    'Archival review of Ottoman-era literacy and education policy debates for explicit discussion of the script''s phonetic mismatch with Turkish and whether reform proposals were suppressed for stated authority-preservation reasons versus religious-continuity reasons.',
    'If intentional, the tangled_rope classification''s asymmetric-extraction element is strongly corroborated; if unintended side effect, the constraint tilts closer to an unfortunate-but-genuine coordination mechanism (closer to a degraded rope) rather than active extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(literacy_barrier_intentionality, empirical, 'Whether the literacy exclusion was a designed feature or an unintended consequence of the script choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(script_as_identity__ottoman_continuity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scri_tr_t0, script_as_identity__ottoman_continuity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(scri_tr_t8, script_as_identity__ottoman_continuity_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement(scri_tr_t16, script_as_identity__ottoman_continuity_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement(scri_tr_t24, script_as_identity__ottoman_continuity_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(scri_tr_t32, script_as_identity__ottoman_continuity_reading, theater_ratio, 32, 0.39).
narrative_ontology:measurement(scri_tr_t40, script_as_identity__ottoman_continuity_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(scri_be_t0, script_as_identity__ottoman_continuity_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(scri_be_t8, script_as_identity__ottoman_continuity_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(scri_be_t16, script_as_identity__ottoman_continuity_reading, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(scri_be_t24, script_as_identity__ottoman_continuity_reading, base_extractiveness, 24, 0.54).
narrative_ontology:measurement(scri_be_t32, script_as_identity__ottoman_continuity_reading, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(scri_be_t40, script_as_identity__ottoman_continuity_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(scri_su_t0, script_as_identity__ottoman_continuity_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(scri_su_t8, script_as_identity__ottoman_continuity_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(scri_su_t16, script_as_identity__ottoman_continuity_reading, suppression_requirement, 16, 0.63).
narrative_ontology:measurement(scri_su_t24, script_as_identity__ottoman_continuity_reading, suppression_requirement, 24, 0.67).
narrative_ontology:measurement(scri_su_t32, script_as_identity__ottoman_continuity_reading, suppression_requirement, 32, 0.7).
narrative_ontology:measurement(scri_su_t40, script_as_identity__ottoman_continuity_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(script_as_identity__ottoman_continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(script_as_identity__ottoman_continuity_reading, 0.08).
narrative_ontology:affects_constraint(script_as_identity__ottoman_continuity_reading, kemalist_rupture_reading).
narrative_ontology:affects_constraint(script_as_identity__ottoman_continuity_reading, phonetic_instrumentalism_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the script_as_identity kernel. kemalist_rupture_reading treats the 1928 Turkish alphabet reform as enabling secular modernization by severing Ottoman-Islamic continuity (a different beneficiary/victim structure: Republican state-builders and mass-literacy seekers benefit, the ulema and Ottoman-literate elite bear the cost — structurally the mirror image of this story's beneficiary/victim assignment). phonetic_instrumentalism_reading treats script choice as ideologically neutral engineering, with ε driven purely by measured literacy-acquisition efficiency rather than by identity or authority claims. All three are linked via affects_constraints because they share the same underlying historical event (the 1928 script reform) and contest its meaning; they are NOT alternate measurements of one constraint — each has a stable, independently authored ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
