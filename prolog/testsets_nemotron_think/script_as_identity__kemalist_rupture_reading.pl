% ============================================================================
% CONSTRAINT STORY: script_as_identity__kemalist_rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_script_as_identity__kemalist_rupture_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: script_as_identity__kemalist_rupture_reading
 *   human_readable: Kemalist Latin Script Mandate as Secularizing Rupture
 *   domain: political/linguistic/cultural
 *
 * SUMMARY:
 *   The 1928 Turkish script reform replaced the Arabic script with a modified
 *   Latin alphabet, mandated by the Kemalist state as a cornerstone of
 *   secular modernization. The reform is presented in official historiography
 *   as a phonetic necessity and a liberation from illiteracy. This reading —
 *   the kemalist_rupture_reading — frames the textual rupture as a feature:
 *   severing the population from the Ottoman-Islamic past was the mechanism
 *   of secularization. The state monopolized the literacy apparatus (schools,
 *   press, bureaucracy) to ensure the transition was total and irreversible.
 *   The constraint is the standing arrangement: Latin script as mandatory,
 *   exclusive, state-enforced orthography.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(script_as_identity__kemalist_rupture_reading, 0.78).
domain_priors:suppression_score(script_as_identity__kemalist_rupture_reading, 0.82).
domain_priors:theater_ratio(script_as_identity__kemalist_rupture_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(script_as_identity__kemalist_rupture_reading, tangled_rope).
narrative_ontology:human_readable(script_as_identity__kemalist_rupture_reading, "Kemalist Latin Script Mandate as Secularizing Rupture").
narrative_ontology:topic_domain(script_as_identity__kemalist_rupture_reading, "political/linguistic/cultural").

domain_priors:requires_active_enforcement(script_as_identity__kemalist_rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(script_as_identity__kemalist_rupture_reading, 'f81eb126-3b36-409e-975c-5352d59fcdb0').
narrative_ontology:cs_kernel_codification('f81eb126-3b36-409e-975c-5352d59fcdb0', fixed_text).
narrative_ontology:cs_authority_grounding('f81eb126-3b36-409e-975c-5352d59fcdb0', extraction).
narrative_ontology:cs_interpretation_layer_present('f81eb126-3b36-409e-975c-5352d59fcdb0').
narrative_ontology:cs_reading_relation('f81eb126-3b36-409e-975c-5352d59fcdb0', script_as_identity__ottoman_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('f81eb126-3b36-409e-975c-5352d59fcdb0', script_as_identity__phonetic_instrumentalism_reading, coexists_with).
narrative_ontology:cs_axiom('f81eb126-3b36-409e-975c-5352d59fcdb0', foundational, script_rupture_enables_modernization).
narrative_ontology:cs_axiom_status(script_rupture_enables_modernization, holdable).
narrative_ontology:cs_axiom_grounding('f81eb126-3b36-409e-975c-5352d59fcdb0', script_rupture_enables_modernization, instrumental).
narrative_ontology:cs_axiom('f81eb126-3b36-409e-975c-5352d59fcdb0', foundational, state_monopolizes_literacy_apparatus).
narrative_ontology:cs_axiom_status(state_monopolizes_literacy_apparatus, holdable).
narrative_ontology:cs_axiom_grounding('f81eb126-3b36-409e-975c-5352d59fcdb0', state_monopolizes_literacy_apparatus, conventional).
narrative_ontology:cs_reference_frame('f81eb126-3b36-409e-975c-5352d59fcdb0', kemalist_revolutionary_rupture).
narrative_ontology:cs_drift_state('f81eb126-3b36-409e-975c-5352d59fcdb0', contemporary_neo_ottoman_revival, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('f81eb126-3b36-409e-975c-5352d59fcdb0', '').
narrative_ontology:cs_kernel_id(script_as_identity__kemalist_rupture_reading, script_as_identity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(script_as_identity__kemalist_rupture_reading, kemalist_state).
narrative_ontology:constraint_beneficiary(script_as_identity__kemalist_rupture_reading, secular_modernist_elite).
narrative_ontology:constraint_beneficiary(script_as_identity__kemalist_rupture_reading, new_literate_generations).
narrative_ontology:constraint_victim(script_as_identity__kemalist_rupture_reading, ottoman_literate_classes).
narrative_ontology:constraint_victim(script_as_identity__kemalist_rupture_reading, religious_establishment).
narrative_ontology:constraint_victim(script_as_identity__kemalist_rupture_reading, traditional_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(script_as_identity__kemalist_rupture_reading, new_literate_generations).
narrative_ontology:constraint_vindicates(script_as_identity__kemalist_rupture_reading, secular_modernization_thesis).
narrative_ontology:constraint_vindicates(script_as_identity__kemalist_rupture_reading, national_unification_through_script).
narrative_ontology:constraint_vindicates(script_as_identity__kemalist_rupture_reading, phonetic_transparency_for_turkish).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacted the 1928 script law, monopolized the education apparatus, mandated Latin script in all official domains, and enforced the transition through state schools, bureaucracy, and press. The state positioned itself as the sole legitimate author of the new literacy order.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, kemalist_state, agenda_setter,
    institutional, generational, arbitrage, national).

% Gained exclusive access to the new state apparatus, bureaucracy, and modern professions through Latin-script education. Their cultural capital became the only legitimate capital; they did not bear the transition cost — they designed it.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, secular_modernist_elite, beneficiary,
    powerful, biographical, mobile, national).

% Acquired mass literacy through state schools in Latin script, enabling social mobility and participation in the modern economy. They also bear the cost of severed access to the Ottoman textual heritage — family letters, religious texts, historical archives — which they cannot read without specialist mediation.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, new_literate_generations, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(script_as_identity__kemalist_rupture_reading, new_literate_generations, payer).

% Lost their literacy monopoly and cultural authority overnight. Their professional skills (scribe, medrese teacher, court clerk) were devalued; their personal libraries and family archives became unreadable to their children. Exit was structurally blocked: the new script was mandatory, the old script banned from public life, and their identity was fused with the Arabic script tradition.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, ottoman_literate_classes, payer,
    moderate, biographical, identity_locked, national).

% Lost control over religious education and textual interpretation. The Latin-script Qur'an and Latin-script religious instruction transferred exegetical authority to state-appointed theologians. Their identity as guardians of revelation was structurally bound to Arabic script; the rupture made their authority illegible to the new generations.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, religious_establishment, payer,
    organized, generational, identity_locked, national).

% Scholars of Ottoman history, literature, and Islamic sciences found their primary sources locked behind a script barrier. They could not transmit their knowledge to students educated only in Latin script. Many were forced into retirement or marginal academic niches; the state did not create parallel Ottoman-script chairs.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, traditional_scholars, payer,
    moderate, biographical, trapped, national).

% The script reform was coupled with Turkish-only education policy. Kurdish communities lost both the Arabic-script medrese tradition (which had served Kurdish-language instruction) and were denied mother-tongue literacy in any script. They were structurally excluded from the literacy bargain.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, kurdish_minority_communities, excluded,
    powerless, generational, trapped, national).

% Analyze the reform as a case study in script change, language planning, and state-building. They document the phonetic fit of Latin script for Turkish vowel harmony but also record the deliberate cultural rupture and its intergenerational effects.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, international_linguists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Created a unified national script enabling mass literacy, standardized education, bureaucratic communication, and print culture across the new Turkish Republic — replacing the multi-script, multi-lingual Ottoman system that limited literacy to a narrow elite.
% TRANSFER_FUNCTION: Transferred textual authority and cultural capital from the Ottoman-Islamic literate classes (scribes, ulema, traditional scholars) to the new secular state apparatus and its educated cadres. Access to the pre-1928 textual corpus — archives, literature, religious texts, family records — was severed for the general population, making it dependent on state-mediated translation and selection.
% ABSENT_VOICES: Kurdish and other minority language communities who lost Arabic-script medrese education without gaining mother-tongue literacy; Ottoman diaspora communities maintaining Arabic script in the Balkans and Middle East; women in traditional households whose domestic literacy practices (prayer books, letters) were invalidated without replacement.
% DISAPPEARANCE_RATIONALE: If the Latin script mandate vanished overnight, the entire modern Turkish textual infrastructure — education, law, administration, media, digital systems — would become inoperable. A new script transition would be required, and the century of Latin-script textual production would face the same accessibility collapse the Ottoman corpus suffered.
% FOUNDING_PROBLEM: The Ottoman Empire's fragmented literacy landscape — Arabic script for Turkish, Persian, and Arabic; Armenian, Greek, Hebrew scripts for minorities — prevented mass education and national communication. Arabic script was phonetically inadequate for Turkish vowel harmony, and the literate class was a narrow, religiously defined elite.
% FOUNDING_PROBLEM_CORROBORATION: Independent linguists (e.g., Geoffrey Lewis, Bernard Comrie) confirm Arabic script's poor phonetic fit for Turkish. However, historians (e.g., Nükhet Sirman, Irvin Cemil Schick) document that the reform exceeded linguistic necessity: the specific Latin alphabet chosen, the abrupt ban on Arabic script, and the coupling with Turkish-only education served a political rupture agenda. The corroboration comes from outside the Kemalist beneficiary set.
narrative_ontology:disappearance_verdict(script_as_identity__kemalist_rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(script_as_identity__kemalist_rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(script_as_identity__kemalist_rupture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(script_as_identity__kemalist_rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(script_as_identity__kemalist_rupture_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(script_as_identity__kemalist_rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(script_as_identity__kemalist_rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(script_as_identity__kemalist_rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the constraint transferred the entire textual patrimony of a civilization to a new elite while severing the general population from direct access to their own archives, religious texts, and family records. Suppression is higher (0.82) because the Arabic script was actively banned from public life, education, and print — not merely displaced. Theater ratio is moderate (0.32): the literacy expansion was real and measurable, but the performative framing of 'liberation' masks the political extraction. Accessibility collapse is very high (0.85): alternatives (Arabic script, minority scripts) were legally and practically eliminated. Resistance is moderate (0.55): organized religious opposition was crushed early; residual resistance persists in cultural memory and neo-Ottoman revivalism.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat, the constraint is a rope (coordination solved, minimal extraction). From the payer seats (Ottoman literate classes, religious establishment), it is a snare (extraction via cultural rupture, enforced by state power). The new_literate_generations seat experiences a tangled_rope: genuine coordination benefit (literacy) fused with asymmetric extraction (severed heritage). The engine computes this divergence from the structural data; the claimed_type 'tangled_rope' reflects the analytical seat's synthesis.
 *
 * DIRECTIONALITY LOGIC:
 *   The kemalist_state is the agenda-setter and primary beneficiary (d ≈ 0.1 — collects the literacy monopoly, sets the rules). The secular_modernist_elite are beneficiaries with mobile exit (d ≈ 0.2). New_literate_generations are dual-positioned: beneficiaries of mass literacy but payers of cultural severance (d ≈ 0.45). Ottoman_literate_classes, religious_establishment, and traditional_scholars are payers with identity_locked or trapped exit (d ≈ 0.85–0.95). Kurdish_minority_communities are excluded and trapped (d ≈ 0.9). International_linguists are analytical observers (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Ottoman literacy fragmentation) was real but the solution (total script rupture with cultural severance) exceeded what linguistic necessity required. The mandate persists not because the phonetic problem recurs — Turkish phonology hasn't changed — but because the state's legitimacy is bound to the rupture narrative. The arrangement has outlived its linguistic function; it now serves as a legitimacy anchor for the secularist state project. This is mandatrophy: the coordination function (phonetic transparency) is live but the extraction function (cultural rupture) has become the primary driver of persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_kemalist_rupture,
    'How does this reading''s structural classification change if the kernel is framed as a contested commitment rather than a settled fact?',
    'Compare the ε and stakeholder structures across all three readings of script_as_identity. If sibling readings produce divergent classifications from the same referent, the kernel itself is the irreducible unit of contestation.',
    'If the kernel framing is accepted, this constraint''s ε is reading-indexed — the extraction measured here is the kemalist reading''s assessment of the standing arrangement, not an observer-independent property. The engine must compute per-reading classifications, not per-kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kemalist_rupture, conceptual, 'Committer-frame structural delta: this reading instantiates zero transition cost, rupture-as-feature, state literacy monopoly.').

omega_variable(
    linguistic_necessity_vs_political_rupture,
    'Was the specific Latin alphabet adopted (with its diacritics and letter choices) phonetically necessary, or did it encode a political rupture beyond vowel harmony?',
    'Comparative analysis of alternative Latin alphabets proposed for Turkish (e.g., the 1923 Istanbul alphabet, the 1926 Armenian-alphabet proposal) and their phonetic adequacy scores.',
    'If the adopted alphabet was not uniquely phonetically optimal, the extraction component (cultural rupture) is structurally separable from the coordination component (phonetic fit), confirming tangled_rope. If it was uniquely optimal, the coordination function absorbs more of the measured extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(linguistic_necessity_vs_political_rupture, empirical, 'Whether the script choice was overdetermined by linguistics or contained surplus political extraction.').

omega_variable(
    suppression_mechanism_ottoman_literate,
    'Was the suppression of the Ottoman literate classes structural (legal bans, state monopoly) or internalized (self-censorship, identity fusion with Arabic script)?',
    'Post-1950 relaxation of script restrictions: did former Ottoman literates or their descendants reclaim Arabic-script literacy voluntarily, or did the suppression persist without active enforcement?',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression after exit. This would increase χ for identity_locked payers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ottoman_literate, empirical, 'Structural vs. internalized suppression for identity_locked payer seats.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(script_as_identity__kemalist_rupture_reading, 0, 96).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(script_as_identity__kemalist_rupture_reading_tr_t0, script_as_identity__kemalist_rupture_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(script_as_identity__kemalist_rupture_reading_tr_t10, script_as_identity__kemalist_rupture_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(script_as_identity__kemalist_rupture_reading_tr_t25, script_as_identity__kemalist_rupture_reading, theater_ratio, 25, 0.3).
narrative_ontology:measurement(script_as_identity__kemalist_rupture_reading_tr_t50, script_as_identity__kemalist_rupture_reading, theater_ratio, 50, 0.35).
narrative_ontology:measurement(script_as_identity__kemalist_rupture_reading_tr_t75, script_as_identity__kemalist_rupture_reading, theater_ratio, 75, 0.38).
narrative_ontology:measurement(script_as_identity__kemalist_rupture_reading_tr_t96, script_as_identity__kemalist_rupture_reading, theater_ratio, 96, 0.32).

% Extraction over time
narrative_ontology:measurement(script_as_identity__kemalist_rupture_reading_be_t0, script_as_identity__kemalist_rupture_reading, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(script_as_identity__kemalist_rupture_reading_be_t10, script_as_identity__kemalist_rupture_reading, base_extractiveness, 10, 0.78).
narrative_ontology:measurement(script_as_identity__kemalist_rupture_reading_be_t25, script_as_identity__kemalist_rupture_reading, base_extractiveness, 25, 0.72).
narrative_ontology:measurement(script_as_identity__kemalist_rupture_reading_be_t50, script_as_identity__kemalist_rupture_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement(script_as_identity__kemalist_rupture_reading_be_t75, script_as_identity__kemalist_rupture_reading, base_extractiveness, 75, 0.75).
narrative_ontology:measurement(script_as_identity__kemalist_rupture_reading_be_t96, script_as_identity__kemalist_rupture_reading, base_extractiveness, 96, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(script_as_identity__kemalist_rupture_reading_su_t0, script_as_identity__kemalist_rupture_reading, suppression_requirement, 0, 0.9).
narrative_ontology:measurement(script_as_identity__kemalist_rupture_reading_su_t10, script_as_identity__kemalist_rupture_reading, suppression_requirement, 10, 0.85).
narrative_ontology:measurement(script_as_identity__kemalist_rupture_reading_su_t25, script_as_identity__kemalist_rupture_reading, suppression_requirement, 25, 0.75).
narrative_ontology:measurement(script_as_identity__kemalist_rupture_reading_su_t50, script_as_identity__kemalist_rupture_reading, suppression_requirement, 50, 0.7).
narrative_ontology:measurement(script_as_identity__kemalist_rupture_reading_su_t75, script_as_identity__kemalist_rupture_reading, suppression_requirement, 75, 0.78).
narrative_ontology:measurement(script_as_identity__kemalist_rupture_reading_su_t96, script_as_identity__kemalist_rupture_reading, suppression_requirement, 96, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(script_as_identity__kemalist_rupture_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(script_as_identity__kemalist_rupture_reading, 0.08).
narrative_ontology:affects_constraint(script_as_identity__kemalist_rupture_reading, script_as_identity__ottoman_continuity_reading).
narrative_ontology:affects_constraint(script_as_identity__kemalist_rupture_reading, script_as_identity__phonetic_instrumentalism_reading).

% DUAL FORMULATION NOTE:
% The script_as_identity kernel decomposes into three constraint stories. This reading (kemalist_rupture) claims the rupture enables modernization; ottoman_continuity_reading claims the rupture destroys identity continuity; phonetic_instrumentalism_reading claims script is neutral technology. Their ε values differ: kemalist reading assesses high extraction on the standing arrangement (from payer seats), ottoman reading assesses even higher extraction (cultural genocide framing), phonetic reading assesses low extraction (coordination only). They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(script_as_identity__kemalist_rupture_reading, organized, 0.85).
constraint_indexing:directionality_override(script_as_identity__kemalist_rupture_reading, moderate, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
