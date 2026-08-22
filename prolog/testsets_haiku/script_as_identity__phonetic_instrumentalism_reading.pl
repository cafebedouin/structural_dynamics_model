% ============================================================================
% CONSTRAINT STORY: script_as_identity__phonetic_instrumentalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_script_as_identity__phonetic_instrumentalism_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: script_as_identity__phonetic_instrumentalism_reading
 *   human_readable: Script Selection as Phonetic Optimization (Phonetic Instrumentalism Reading)
 *   domain: linguistic/political
 *
 * SUMMARY:
 *   The phonetic-instrumentalism reading frames the Turkish script transition
 *   (from Arabic to Latin script, primarily 1928–1935) as a technical
 *   linguistic optimization: Latin script's dedicated vowel graphemes provide
 *   superior phonetic transparency for Turkish vowel harmony compared to
 *   Arabic script's vowel-weak representation. This reading depoliticizes the
 *   decision by treating script as a neutral container for language rather
 *   than as a carrier of identity, history, and religious-textual continuity.
 *   The reading is structurally distinct from two sibling readings: the
 *   kemalist-rupture reading, which narrates the transition as deliberate
 *   ideological break from Ottoman-Islamic past, and the ottoman-continuity
 *   reading, which treats Arabic script as constitutive of Turkish-Islamic
 *   identity. Each reading instantiates a different constraint on the same
 *   kernel (the decision about which script).
 *
 * KEY AGENTS:
 *   - Linguistic modernists: frame script as neutral technology, benefit from technical framing
 *   - Ottoman continuity advocates: experience the framing as delegitimizing their identity claim, trapped by identity-lock
 *   - Kemalist rupture advocates: benefit from phonetic framing as political cover for ideological transition
 *   - Linguistic engineers: set technical research agenda, validate phonetic-transparency metrics
 *   - Turkish literacy learners: trapped by state monopoly, benefit from official literacy pathway
 *   - Historical text custodians: bear diffuse cost of archive-access degradation, not addressed by phonetic framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(script_as_identity__phonetic_instrumentalism_reading, 0.28).
domain_priors:suppression_score(script_as_identity__phonetic_instrumentalism_reading, 0.12).
domain_priors:theater_ratio(script_as_identity__phonetic_instrumentalism_reading, 0.67).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 0.67).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, accessibility_collapse, 0.41).
narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(script_as_identity__phonetic_instrumentalism_reading, rope).
narrative_ontology:human_readable(script_as_identity__phonetic_instrumentalism_reading, "Script Selection as Phonetic Optimization (Phonetic Instrumentalism Reading)").
narrative_ontology:topic_domain(script_as_identity__phonetic_instrumentalism_reading, "linguistic/political").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(script_as_identity__phonetic_instrumentalism_reading, '570555df-1b78-4ee8-b8c5-022ab8eba21b').
narrative_ontology:cs_kernel_codification('570555df-1b78-4ee8-b8c5-022ab8eba21b', formalized).
narrative_ontology:cs_authority_grounding('570555df-1b78-4ee8-b8c5-022ab8eba21b', extraction).
narrative_ontology:cs_interpretation_layer_present('570555df-1b78-4ee8-b8c5-022ab8eba21b').
narrative_ontology:cs_reading_relation('570555df-1b78-4ee8-b8c5-022ab8eba21b', script_as_identity__kemalist_rupture_reading, coexists_with).
narrative_ontology:cs_reading_relation('570555df-1b78-4ee8-b8c5-022ab8eba21b', script_as_identity__ottoman_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('570555df-1b78-4ee8-b8c5-022ab8eba21b', foundational, script_technological_neutrality).
narrative_ontology:cs_axiom_status(script_technological_neutrality, holdable).
narrative_ontology:cs_axiom_grounding('570555df-1b78-4ee8-b8c5-022ab8eba21b', script_technological_neutrality, empirically_contingent).
narrative_ontology:cs_axiom('570555df-1b78-4ee8-b8c5-022ab8eba21b', foundational, phonetic_transparency_as_optimization_metric).
narrative_ontology:cs_axiom_status(phonetic_transparency_as_optimization_metric, holdable).
narrative_ontology:cs_axiom_grounding('570555df-1b78-4ee8-b8c5-022ab8eba21b', phonetic_transparency_as_optimization_metric, instrumental).
narrative_ontology:cs_reference_frame('570555df-1b78-4ee8-b8c5-022ab8eba21b', technical_linguistic_optimization).
narrative_ontology:cs_drift_state('570555df-1b78-4ee8-b8c5-022ab8eba21b', contemporary_identity_politics_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('570555df-1b78-4ee8-b8c5-022ab8eba21b', '').
narrative_ontology:cs_kernel_id(script_as_identity__phonetic_instrumentalism_reading, script_as_identity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(script_as_identity__phonetic_instrumentalism_reading, linguistic_modernists).
narrative_ontology:constraint_beneficiary(script_as_identity__phonetic_instrumentalism_reading, technical_standardizers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(script_as_identity__phonetic_instrumentalism_reading, kemalist_rupture_advocates).
narrative_ontology:constraint_beneficiary(script_as_identity__phonetic_instrumentalism_reading, turkish_literacy_learners).
narrative_ontology:constraint_victim(script_as_identity__phonetic_instrumentalism_reading, ottoman_continuity_advocates).
narrative_ontology:constraint_victim(script_as_identity__phonetic_instrumentalism_reading, turkish_literacy_learners).
narrative_ontology:constraint_victim(script_as_identity__phonetic_instrumentalism_reading, historical_text_custodians).
narrative_ontology:constraint_vindicates(script_as_identity__phonetic_instrumentalism_reading, script_neutrality_thesis).
narrative_ontology:constraint_vindicates(script_as_identity__phonetic_instrumentalism_reading, phonetic_transparency_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Scholars, educators, and policy advocates who frame script selection as a technical matter of phonetic fit. They benefit from the framing because it appears to bracket the identity question and allows them to argue for Latin script on instrumental grounds alone. Their position relies on the premise that scripts are neutral containers for language, not carriers of cultural meaning.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, linguistic_modernists, beneficiary,
    organized, generational, mobile, national).

% Conservative scholars, religious authorities, and cultural custodians who experience the phonetic-neutrality framing as delegitimizing their reading of script as identity-constitutive. They bear the cost of being excluded from the technical conversation — their claim that script embeds centuries of Ottoman-Islamic textual tradition is reframed as emotional attachment rather than structural fact. Exit would require abandoning their reading of Turkish identity entirely.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, ottoman_continuity_advocates, payer,
    moderate, generational, identity_locked, national).

% State-building elites and secularists who benefit from the phonetic-instrumentalism framing because it provides technical cover for a decision that is primarily about political rupture from the Ottoman past. The neutrality claim allows them to impose script change without explicitly narrating it as ideological replacement. They can simultaneously hold the rupture reading (internally) and publicly defend the decision as phonetic optimization.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, kemalist_rupture_advocates, beneficiary,
    institutional, generational, arbitrage, national).

% Citizens required to learn the new script and abandon literacy in the old one. They benefit from whichever reading is officially endorsed (their educational pathway is set by state policy). They also bear the cost of script transition — older populations lose access to historical texts, younger populations lose connection to pre-transition written culture. Their exit options are constrained by the state's territorial monopoly on education.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, turkish_literacy_learners, beneficiary,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(script_as_identity__phonetic_instrumentalism_reading, turkish_literacy_learners, payer).

% Librarians, archivists, and scholars managing Ottoman-era texts. They bear the diffuse cost of the script transition: entire archives become literacy-gated for the next generation unless translation/transcription infrastructure is maintained. The phonetic-instrumentalism framing obscures this cost by treating script as purely forward-looking, not as a stewardship question about historical access.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, historical_text_custodians, payer,
    powerless, generational, trapped, national).

% Government linguists and orthography-design experts who frame the decision in technical terms: vowel harmony, phoneme-to-grapheme regularity, diacritical economy. They set the research agenda and validate competing proposals by phonetic-transparency metrics. They do not directly advocate for political rupture, but their technical framing makes that rupture legible as inevitable rather than chosen.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, linguistic_engineers, agenda_setter,
    institutional, generational, arbitrage, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(script_as_identity__phonetic_instrumentalism_reading, linguistic_engineers).
narrative_ontology:fixing_cost_class(script_as_identity__phonetic_instrumentalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, standardized orthographic system for the Turkish language with regular phoneme-to-grapheme correspondences, enabling consistent literacy instruction, text production, and information exchange across the nation-state.
% TRANSFER_FUNCTION: Transfers epistemic authority from historical-textual custodians (who validated Arabic-script literacy as constitutive of Turkish identity) to technical linguists and modernist scholars (who validate Latin-script literacy as phonetically optimal). It also transfers cultural continuity from Ottoman-Islamic tradition to secular-nationalist framing.
% ABSENT_VOICES: Religious scholars, Ottoman historians, and custodians of pre-transition texts who would argue that script encodes centuries of legal, theological, and literary tradition — that the transition severs not just orthography but interpretive communities and canonical reference. They are excluded because the phonetic-instrumentalism framing redefines their objections as 'attachment to the old' rather than structural claims about meaning-preservation.
% DISAPPEARANCE_RATIONALE: If the phonetic-instrumentalism framing disappeared but the Latin script remained, the Turkish language system would not physically change, but the legitimacy of the script choice would require explicit rearticulation: either as a decision about rupture from Ottoman identity (kemalist framing) or as a loss of Ottoman-Islamic textual continuity (ottoman continuity framing). The constraint's function is to bracket that rearticulation — if the framing is gone, the identity politics it obscured would reorganize the whole conversation about why this script, for whom, at what cost.
% FOUNDING_PROBLEM: Turkish language contains complex vowel-harmony patterns (front/back, rounded/unrounded vowels) that Arabic script represents ambiguously or inconsistently because Arabic does not distinguish front/back or round/unrounded vowels systematically. Latin script, with dedicated graphemes for each Turkish vowel, provides more transparent phoneme-to-grapheme mapping, reducing ambiguity in written transmission and making literacy instruction more efficient and uniform.
% FOUNDING_PROBLEM_CORROBORATION: Linguists specializing in Turkish phonology (Deny, Haspelmath, and others) attested the phonetic-transparency difference between Arabic and Latin script representation of Turkish vowels as a technical fact independent of political preference. However, they also note (in audits and secondary analysis outside the benefiting parties) that the phonetic advantage alone would not have driven the transition without concurrent political rupture — the technical superiority was real but not sufficient to explain the speed and totality of the change.
narrative_ontology:disappearance_verdict(script_as_identity__phonetic_instrumentalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(script_as_identity__phonetic_instrumentalism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(script_as_identity__phonetic_instrumentalism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(script_as_identity__phonetic_instrumentalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(script_as_identity__phonetic_instrumentalism_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(script_as_identity__phonetic_instrumentalism_reading_tests).
:- end_tests(script_as_identity__phonetic_instrumentalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-low (0.28) because the phonetic-transparency claim has genuine technical validity — the constraint does solve a real coordination problem (consistent literacy instruction). However, it is not zero because the reading obscures an extraction: it transfers epistemic authority from historical custodians (who validated Arabic-script literacy) to technical linguists (who validate Latin-script literacy), and it brackets the identity-encoding function that sibling readings foreground. Suppression is very low (0.12) because the phonetic framing requires minimal coercion — it operates by redefining what counts as a valid objection (from 'this severs our tradition' to 'you're just attached to the old'). Theater ratio is high and rising (0.45→0.67) because the phonetic-justification apparatus grows over time: linguistic research programs, standardization bodies, and educational measurement systems accumulate around the technical claim, while the identity and rupture questions recede from official discourse. The rising theater indicates the constraint's function increasingly shifts from solving a technical problem to legitimating a political decision through technical language. Accessibility collapse is moderate (0.41) because alternatives (reformed Arabic script, hybrid systems) remain technically conceivable even if politically foreclosed; the phonetic claim makes Latin seem inevitable but not mathematically necessary. Resistance is moderate-high (0.58) because ottoman-continuity advocates and religious scholars maintain active objection to the reading, even as they are excluded from official policy channels.
 *
 * PERSPECTIVAL GAP:
 *   The linguistic-modernist and kemalist seats compute the constraint differently from the ottoman-continuity seat. For modernists and kemalists, the constraint is a rope — genuine coordination (standardized orthography) without significant coercion. For ottoman-continuity advocates, the same constraint operates as enforced extraction: their reading of script as identity-constitutive is reframed as non-technical sentiment, their historical-textual tradition is devalued, and their exit options (holding both the old script and the national identity) are structurally barred. The engine's per-seat computation should capture this divergence: the phonetic-instrumentalism reading produces lower ε and lower suppression from the beneficiary seats but higher effective extraction and higher identity-lock-modulated suppression from the payer seat. The committer structure is invisible in any single seat's perception — only cross-seat comparison reveals it.
 *
 * DIRECTIONALITY LOGIC:
 *   Linguistic modernists and kemalist elites are beneficiaries (d near 0.0–0.2): they collect the benefit of a standardized, modern-appearing literacy system and the political cover of technical justification. Ottoman-continuity advocates are targets (d near 0.8–1.0): they bear the cost of having their identity claim reframed as non-technical attachment, their textual tradition is devalued, and their exit is identity-locked (holding ottoman-continuity identity while embracing Latin script is framed as contradictory). Turkish literacy learners are near-symmetric (d ~0.5): they benefit from having a consistent, state-standardized literacy system but also bear the cost of severed access to pre-transition texts and the cultural rupture the phonetic framing obscures. The linguistic engineers sit as agenda-setters (d ~0.3): they frame the problem technically and benefit from research funding and authority, but they are not primary extractors — they serve the kemalist agenda without necessarily being conscious of its identity-political dimensions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live (Turkish vowel harmony does require script representation) but historically insufficient (the phonetic advantage alone did not drive the speed/totality of the transition). This marks a mandatrophy candidate: the phonetic-transparency justification persists in official discourse even as the driving political problem (Ottoman rupture) has become less salient over time. The constraint does NOT resolve mandatrophy — it persists because the technical justification is real enough and the political rupture is now sedimented into institutions, not because the founding technical problem still requires active solution. The theater-ratio rise (0.45→0.67) reflects this: the constraint increasingly operates by institutional inertia and legitimation apparatus rather than by solving the original coordination problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    phonetic_necessity_vs_political_sufficiency,
    'Is the phonetic-transparency advantage of Latin script sufficient to explain the historical speed and totality of the Turkish script transition, or is the phonetic advantage real but politically insufficient without concurrent ideological rupture?',
    'Comparative historical analysis: examining other languages with similar vowel-harmony patterns (e.g., Hungarian, Finnish) and their script-adoption histories; analyzing archival evidence of decision-maker reasoning in the 1920s Turkish context; examining counterfactual scenarios (what would have happened if only the phonetic advantage existed without the political rupture agenda).',
    'If phonetic advantage is sufficient, the constraint is indeed a rope (technical coordination without significant extraction). If insufficient, the constraint is tangled rope or snare — the phonetic framing provides cover for extraction (political rupture, identity devaluation) that would be contested if explicitly narrated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(phonetic_necessity_vs_political_sufficiency, empirical, 'Whether the founding problem (phonetic transparency) is the primary driver of the constraint''s persistence or a secondary justification for a primarily political decision.').

omega_variable(
    script_neutrality_thesis_contestation,
    'Is the premise that scripts are neutral containers for language structurally defensible, or do scripts inherently encode and transmit cultural, epistemic, and identity meanings?',
    'Linguistic-philosophical analysis and ethnographic examination of literacy practices: Does learning to read and write in a script train certain cognitive patterns and identity framings? Do pre-transition readers report script-change as identity-discontinuous, and post-transition readers as script-natural? Does switching scripts alter which historical texts are accessed, and does that access-pattern shape identity narratives?',
    'If scripts are neutral, the phonetic-instrumentalism reading is correct and the constraint is genuinely rope-type coordination. If scripts encode identity-meaning, the neutrality claim is an ideological move that obscures extraction — the constraint would reclassify as tangled rope or snare, and the theater_ratio ascent reflects increasing awareness that the technical claim does not fully explain the decision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(script_neutrality_thesis_contestation, conceptual, 'Whether scripts are epistemically and culturally neutral or identity-constitutive.').

omega_variable(
    alternative_script_foreclosure,
    'Could Turkish vowel harmony have been adequately represented by a reformed Arabic script (keeping Ottoman continuity while gaining phonetic clarity), or does the phonetic argument structurally necessitate Latin script specifically?',
    'Technical design analysis: expert linguists design a vowel-distinct Arabic-script system for Turkish and compare its phonetic properties against both the actual Latin choice and the pre-transition Arabic script. Examine whether Turkish linguistic modernists actively considered but rejected reformed-Arabic alternatives, or whether those alternatives were never formally examined.',
    'If reformed Arabic could meet the phonetic requirement, the Latin-specificity is a political choice (kemalist rupture agenda), not a technical necessity — the constraint reclassifies as snare (the phonetic claim masks identity-political extraction). If only Latin meets the phonetic criterion, the phonetic reading is structurally sound and extraction is genuinely lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_script_foreclosure, empirical, 'Whether the phonetic advantage necessitates Latin specifically or could be achieved within alternative script families.').

omega_variable(
    reading_incommensurability,
    'Can the phonetic-instrumentalism reading and the ottoman-continuity reading coexist within a single framework of legitimate discourse, or does accepting one logically foreclose the other?',
    'Conceptual analysis of what each reading''s core premises require: Does the phonetic reading entail that script is purely functional (which would foreclose the identity-constitutive claim)? Does the ottoman-continuity reading entail that script-change is identity-annihilation (which would foreclose any technical re-evaluation)? Test with contemporary Turkish scholars: can someone hold both ''Latin script is phonetically superior'' AND ''Arabic script constitutes our identity''?',
    'If they coexist, the readings are Coexists_with relations (different parties hold different readings, both live). If one forecloses the other, the relation is Forecloses (which would be rare and requires logical contradiction). This affects how the engine routes multiple kernel-readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_incommensurability, conceptual, 'Whether the phonetic-instrumentalism reading and ottoman-continuity reading are logically coexistent or mutually exclusive.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the low measured suppression (0.12) a structural fact (the phonetic framing requires little coercion because it is technically sound), or is suppression internalized in the ottoman-continuity advocates themselves (they have internalized the delegitimizing framing and no longer voice objections in official channels)?',
    'Post-constraint ethnography: measure active resistance when ottoman-continuity advocates are given voice (in confidential interviews, religious contexts, underground publications). Compare the level of active objection in contexts where the phonetic-neutrality framing is not enforced. If objection rises sharply in those contexts, suppression is partially internalized.',
    'If suppression is structural, the constraint''s low suppression score is accurate and it is indeed rope-type. If suppression is internalized, the effective suppression is higher than measured, and the constraint functions more like snare than rope — the phonetic framing operates by making identity objections unsayable, not by meeting them on technical grounds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether the low suppression measure reflects genuine technical neutrality or internalized delegitimization of alternative readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(script_as_identity__phonetic_instrumentalism_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scri_tr_t0, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement_basis(scri_tr_t0, observed).
narrative_ontology:measurement(scri_tr_t10, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 10, 0.52).
narrative_ontology:measurement_basis(scri_tr_t10, observed).
narrative_ontology:measurement(scri_tr_t20, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 20, 0.58).
narrative_ontology:measurement_basis(scri_tr_t20, observed).
narrative_ontology:measurement(scri_tr_t40, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 40, 0.65).
narrative_ontology:measurement_basis(scri_tr_t40, observed).
narrative_ontology:measurement(scri_tr_t60, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 60, 0.67).
narrative_ontology:measurement_basis(scri_tr_t60, observed).
narrative_ontology:measurement(scri_tr_t100, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 100, 0.67).
narrative_ontology:measurement_basis(scri_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(scri_be_t0, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(scri_be_t0, observed).
narrative_ontology:measurement(scri_be_t10, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 10, 0.22).
narrative_ontology:measurement_basis(scri_be_t10, observed).
narrative_ontology:measurement(scri_be_t20, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 20, 0.26).
narrative_ontology:measurement_basis(scri_be_t20, observed).
narrative_ontology:measurement(scri_be_t40, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 40, 0.28).
narrative_ontology:measurement_basis(scri_be_t40, observed).
narrative_ontology:measurement(scri_be_t60, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 60, 0.28).
narrative_ontology:measurement_basis(scri_be_t60, observed).
narrative_ontology:measurement(scri_be_t100, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 100, 0.28).
narrative_ontology:measurement_basis(scri_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(scri_su_t0, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement_basis(scri_su_t0, observed).
narrative_ontology:measurement(scri_su_t10, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 10, 0.09).
narrative_ontology:measurement_basis(scri_su_t10, observed).
narrative_ontology:measurement(scri_su_t20, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 20, 0.1).
narrative_ontology:measurement_basis(scri_su_t20, observed).
narrative_ontology:measurement(scri_su_t40, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 40, 0.12).
narrative_ontology:measurement_basis(scri_su_t40, observed).
narrative_ontology:measurement(scri_su_t60, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 60, 0.12).
narrative_ontology:measurement_basis(scri_su_t60, observed).
narrative_ontology:measurement(scri_su_t100, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 100, 0.12).
narrative_ontology:measurement_basis(scri_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(script_as_identity__phonetic_instrumentalism_reading, information_standard).
narrative_ontology:boltzmann_floor_override(script_as_identity__phonetic_instrumentalism_reading, 0.05).
narrative_ontology:affects_constraint(script_as_identity__phonetic_instrumentalism_reading, script_as_identity__kemalist_rupture_reading).
narrative_ontology:affects_constraint(script_as_identity__phonetic_instrumentalism_reading, script_as_identity__ottoman_continuity_reading).

% DUAL FORMULATION NOTE:
% The kernel 'script_as_identity' (Turkish state decision to adopt Latin script, 1928–1935) generates three structurally distinct constraint stories, one for each live reading of the kernel. Each reading instantiates a different ε, different beneficiary/victim structure, and different classification. The phonetic-instrumentalism reading (this constraint) treats script as neutral technology with low extraction (ε~0.28). The kemalist-rupture reading (sibling) treats the transition as deliberate ideological break with moderate extraction. The ottoman-continuity reading (sibling) treats Arabic script as identity-constitutive with high extraction from continuity advocates. No single constraint story can adjudicate between them — they coexist as live competing readings held by different parties. The network relations link the three as a kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(script_as_identity__phonetic_instrumentalism_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
