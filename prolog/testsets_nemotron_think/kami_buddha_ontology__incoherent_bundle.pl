% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology__incoherent_bundle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kami_buddha_ontology__incoherent_bundle, []).

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
 *   constraint_id: kami_buddha_ontology__incoherent_bundle
 *   human_readable: Shinbutsu-shugo as Institutionally Sustained Incoherent Bundle
 *   domain: religious_studies/japanese_cultural_history
 *
 * SUMMARY:
 *   Shinbutsu-shugo (kami-buddha fusion) operated in Japan from roughly the
 *   8th to 19th century as a system where kami and buddhas were
 *   simultaneously identified and distinguished, hierarchically ordered and
 *   reciprocally related, systematized in elite doctrine and unsystematized
 *   in folk practice. The 'incoherent_bundle' reading holds that no single
 *   ontology governed this system; rather, institutional inertia and
 *   practical ritual efficacy sustained contradictory commitments. The
 *   constraint is the requirement that temples, shrines, court, and
 *   communities participate in this bundle — performing rites that fuse and
 *   separate, hierarchize and equalize, without resolving the contradictions.
 *   Meiji's forced separation (shinbutsu bunri) demonstrated the bundle's
 *   enforcement dependence: when state power withdrew support, the system
 *   fractured but did not cleanly resolve into either monism or partition.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__incoherent_bundle, 0.65).
domain_priors:suppression_score(kami_buddha_ontology__incoherent_bundle, 0.6).
domain_priors:theater_ratio(kami_buddha_ontology__incoherent_bundle, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, extractiveness, 0.65).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__incoherent_bundle, tangled_rope).
narrative_ontology:human_readable(kami_buddha_ontology__incoherent_bundle, "Shinbutsu-shugo as Institutionally Sustained Incoherent Bundle").
narrative_ontology:topic_domain(kami_buddha_ontology__incoherent_bundle, "religious_studies/japanese_cultural_history").

domain_priors:requires_active_enforcement(kami_buddha_ontology__incoherent_bundle).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__incoherent_bundle, 'c16a3a1c-206f-481b-8fee-44184b320f52').
narrative_ontology:cs_kernel_codification('c16a3a1c-206f-481b-8fee-44184b320f52', distributed).
narrative_ontology:cs_authority_grounding('c16a3a1c-206f-481b-8fee-44184b320f52', practice).
narrative_ontology:cs_interpretation_layer_present('c16a3a1c-206f-481b-8fee-44184b320f52').
narrative_ontology:cs_reading_relation('c16a3a1c-206f-481b-8fee-44184b320f52', kami_buddha_ontology__honji_suijaku_monism, coexists_with).
narrative_ontology:cs_reading_relation('c16a3a1c-206f-481b-8fee-44184b320f52', kami_buddha_ontology__domain_partition, coexists_with).
narrative_ontology:cs_axiom('c16a3a1c-206f-481b-8fee-44184b320f52', foundational, ritual_efficacy_trumps_theoretical_coherence).
narrative_ontology:cs_axiom_status(ritual_efficacy_trumps_theoretical_coherence, holdable).
narrative_ontology:cs_axiom_grounding('c16a3a1c-206f-481b-8fee-44184b320f52', ritual_efficacy_trumps_theoretical_coherence, conventional).
narrative_ontology:cs_axiom('c16a3a1c-206f-481b-8fee-44184b320f52', secondary, institutional_sustainability_requires_contradiction_tolerance).
narrative_ontology:cs_axiom_status(institutional_sustainability_requires_contradiction_tolerance, holdable).
narrative_ontology:cs_axiom_grounding('c16a3a1c-206f-481b-8fee-44184b320f52', institutional_sustainability_requires_contradiction_tolerance, instrumental).
narrative_ontology:cs_reference_frame('c16a3a1c-206f-481b-8fee-44184b320f52', syncretic_ritual_practice).
narrative_ontology:cs_drift_state('c16a3a1c-206f-481b-8fee-44184b320f52', meiji_separation_era, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('c16a3a1c-206f-481b-8fee-44184b320f52', '').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__incoherent_bundle, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__incoherent_bundle, buddhist_institutions).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__incoherent_bundle, shinto_institutions).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__incoherent_bundle, imperial_court).
narrative_ontology:constraint_victim(kami_buddha_ontology__incoherent_bundle, local_folk_practitioners).
narrative_ontology:constraint_victim(kami_buddha_ontology__incoherent_bundle, excluded_groups).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__incoherent_bundle, ritual_efficacy_over_doctrinal_purity).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__incoherent_bundle, institutional_pragmatism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Major temples (Todai-ji, Kofuku-ji, Enryaku-ji) controlled vast landholdings, managed syncretic shrine-temple complexes (jingiji), and produced the doctrinal frameworks (honji suijaku, ryobu shinto) that justified the bundle. They administered ordination, ritual calendars, and pilgrimage networks. Exit meant losing institutional infrastructure built over centuries; constrained by sunk costs in physical plant and doctrinal authority.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, buddhist_institutions, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__incoherent_bundle, buddhist_institutions, beneficiary).

% Major shrines (Ise, Hachiman, Inari networks) gained Buddhist patronage, architectural sophistication, and state recognition through syncretic association. Shrine priests (shinshoku) often held Buddhist ranks. They participated in joint rites and shared pilgrimage circuits. Exit was constrained by the same institutional sunk costs; some shrine lineages fully Buddhistized, others maintained distinct identity within the bundle.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, shinto_institutions, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__incoherent_bundle, shinto_institutions, agenda_setter).

% The court sponsored syncretic rites (Daijosai, Niinamesai), appointed temple-shrine administrators, and used the bundle to legitimize central authority. It could arbitrage between Buddhist and Shinto factions. Exit options were high: the court could (and eventually did) impose a new framework. Its power derived from being the only actor that could rewrite the rules.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, imperial_court, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__incoherent_bundle, imperial_court, beneficiary).

% Village communities maintained yama no kami (mountain deities), ta no kami (field deities), and ancestor rites that blended Buddhist and Shinto elements pragmatically. They funded temple-shrine complexes through labor and taxes, participated in mandatory festivals, and had their local cults absorbed into institutional hierarchies. Exit was identity-locked: their self-understanding, agricultural calendar, and social cohesion were constituted through these rites. Leaving the bundle meant leaving their world.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, local_folk_practitioners, payer,
    powerless, biographical, identity_locked, local).

% Women were barred from certain syncretic sacred mountains (nyonin kekkai). Outcaste groups (eta, hinin) performed ritually impure tasks (leather, execution, disposal) that the bundle's purity logic required but its status hierarchy excluded. Non-affiliated mountain ascetics (yamabushi) operated at the margins, sometimes incorporated, sometimes suppressed. They had no voice in the institutional negotiations that shaped the bundle.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, excluded_groups, excluded,
    powerless, biographical, trapped, local).

% Historians of religion, anthropologists, and philosophers analyze the bundle from outside. They produce the typologies (honji suijaku, han-honji suijaku, shinbutsu shugo) that the historical actors never used as self-descriptions. Their constraint is analytical coherence; they gain academic capital from resolving the incoherence the actors lived.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, modern_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a ritual and institutional framework that allowed diverse communities across the archipelago to share sacred space, coordinate agricultural calendars, manage death and ancestry, and legitimate local authority through connection to central institutions — all without requiring doctrinal agreement.
% TRANSFER_FUNCTION: Moved land, labor, and ritual legitimacy from local cults and village communities to central temple-shrine complexes and the court; moved ideological authority from diverse practice to institutional doctrine; moved purity/pollution burdens onto excluded groups.
% ABSENT_VOICES: Local cult leaders whose deities were reclassified as buddha-traces or subordinate kami; women excluded from mountain rites that structured communal identity; outcaste groups whose labor sustained the purity system but who were barred from its benefits; independent yamabushi whose syncretic knowledge was institutionalized without their consent.
% DISAPPEARANCE_RATIONALE: When Meiji forcibly separated kami and buddhas (1868-1872), temple-shrine complexes were dismantled, Buddhist assets seized, Shinto priests laicized, and new State Shinto invented. Village ritual calendars were rewritten. The rearrangement was violent and incomplete: folk syncretism persisted underground, new sects formed, and the 'separation' created new contradictions. The world did not stay the same.
% FOUNDING_PROBLEM: Early Japanese state formation (7th-8th century) required a unified ritual framework to integrate clan-based kami worship with imported Buddhist soteriology, legitimize imperial authority, and coordinate agricultural and funerary rites across diverse local practices.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the Kojiki/Nihon Shoki (court perspective) and by the archaeological record of temple-shrine construction (institutional perspective). That the problem is dead is corroborated by the Meiji government's explicit rejection of the bundle as 'superstition' and its construction of State Shinto as a modern replacement — a verdict from outside the beneficiary institutions. No contemporary Buddhist or Shinto institution claims the Nara-Heian state-formation problem as its current justification.
narrative_ontology:disappearance_verdict(kami_buddha_ontology__incoherent_bundle, world_rearranges).
narrative_ontology:founding_problem_status(kami_buddha_ontology__incoherent_bundle, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__incoherent_bundle, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kami_buddha_ontology__incoherent_bundle, 'none', 1).
narrative_ontology:epsilon_provenance(kami_buddha_ontology__incoherent_bundle, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kami_buddha_ontology__incoherent_bundle_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kami_buddha_ontology__incoherent_bundle, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kami_buddha_ontology__incoherent_bundle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.65) reflects institutional capture of land, labor, and legitimacy from local cults. Suppression (0.6) reflects active exclusion of non-conforming practices and enforcement of institutional orthopraxy. Theater (0.45) reflects the gap between elaborate doctrinal justifications (honji suijaku theory, ryobu shinto) and the pragmatic ritual mixing that actually occurred. Accessibility collapse (0.5) and resistance (0.45) are moderate: folk practitioners maintained alternative practices (yama no kami, folk buddhism) but within a narrowing institutional envelope. The claimed type is tangled_rope because the system genuinely coordinated ritual efficacy and social cohesion across diverse communities while extracting resources for central institutions.
 *
 * PERSPECTIVAL GAP:
 *   From the Buddhist institution seat, the constraint is genuine coordination: it secured temple networks, pilgrimage routes, and doctrinal frameworks. From the local practitioner seat, the same structure is extraction: mandatory participation in rites they did not choose, supporting institutions they could not influence. The court seat sees ideological infrastructure; the excluded seat sees the violence of categorization. The engine computes these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Buddhist institutions and Shinto institutions are primary beneficiaries (d ~ 0.15-0.25): they gained land, protection, and doctrinal authority. The imperial court is agenda_setter and beneficiary (d ~ 0.1): it gained ideological unity and ritual legitimacy. Local folk practitioners are payers (d ~ 0.75): they supplied labor, taxes, and ritual participation with constrained exit. Excluded groups (women in certain rites, outcastes, non-affiliated cults) are trapped (d ~ 0.9). Modern scholars are analytical observers (d = 0.5). The derivation chain from beneficiary/victim declarations plus power/exit data produces these directionalities without overrides.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — unified ritual legitimacy for a centralizing state — was live in the Nara-Heian period but dead by the Edo period. Yet the arrangement persisted and intensified (rising theater, rising suppression) because institutions had fused their identity with the bundle. The constraint became a Piton in its late phase: no party benefited enough to reform it, no party was hurt enough to overthrow it, until external force (Meiji) broke the equilibrium. The incoherent_bundle reading captures this mandatrophy: the system outlived its founding problem because its contradictions were the glue holding institutions together.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    incoherence_functionality,
    'Was the theoretical incoherence of shinbutsu-shugo functionally adaptive for institutional survival, or an accidental byproduct of pragmatic layering?',
    'Comparative analysis of institutional longevity: systems that maintain productive ambiguity vs. systems that collapse under contradiction. Archaeological and textual evidence for whether contradictory rituals produced better social cohesion than coherent alternatives would have.',
    'If adaptive, the bundle is a Tangled Rope where incoherence is the coordination mechanism; if accidental, it is a Snare where institutions exploit confusion for extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incoherence_functionality, empirical, 'Whether contradiction was a feature or bug of the institutional design.').

omega_variable(
    ritual_success_vs_coherence,
    'Did ritual success mask theoretical incoherence, or did the performance of contradictory rites generate the very efficacy practitioners experienced?',
    'Phenomenological analysis of practitioner accounts vs. institutional doctrine; experimental reconstruction of syncretic rituals measuring participant outcomes.',
    'If efficacy derives from contradiction itself, the constraint''s coordination function is inseparable from its incoherence; if masking, the coordination function could be preserved without the extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ritual_success_vs_coherence, conceptual, 'The causal relationship between ritual performance and theoretical incoherence.').

omega_variable(
    meiji_separation_continuity,
    'Was the Meiji shinbutsu bunri a genuine rupture with the incoherent bundle, or a reconfiguration that preserved its institutional logic in new forms?',
    'Institutional genealogy tracing personnel, land holdings, and ritual forms from pre-Meiji syncretic institutions into State Shinto and modern Buddhist sects.',
    'If continuity, the constraint persists in transformed guise (Piton dynamics); if rupture, the bundle was genuinely contingent on pre-modern institutional ecology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_separation_continuity, empirical, 'Whether the Meiji separation resolved or displaced the incoherent bundle.').

omega_variable(
    kernel_reading_location,
    'This constraint is one reading (incoherent_bundle) of the contested kernel kami_buddha_ontology. The sibling readings are honji_suijaku_monism (ontological identity) and domain_partition (functional separation). Where exactly does the structural disagreement lie?',
    'Map each reading''s predicted stakeholder situations, beneficiary/victim structures, and enforcement mechanisms against the historical record. The disagreement is located in whether the system''s persistence required a single ontology (honji_suijaku), a clean partition (domain_partition), or no ontology at all (incoherent_bundle).',
    'If the historical record shows institutions operating without ontological commitment, incoherent_bundle is the only reading that fits the structural data. If institutions explicitly taught honji_suijaku or domain_partition as doctrine, those readings capture the operative constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Commitment frame: this reading instantiates the kernel as ''no ontology, only institutional practice''; siblings instantiate it as ''identity ontology'' and ''partition ontology'' respectively.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__incoherent_bundle, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kb_incoherent_tr_t0, kami_buddha_ontology__incoherent_bundle, theater_ratio, 0, 0.2).
narrative_ontology:measurement(kb_incoherent_tr_t200, kami_buddha_ontology__incoherent_bundle, theater_ratio, 200, 0.3).
narrative_ontology:measurement(kb_incoherent_tr_t400, kami_buddha_ontology__incoherent_bundle, theater_ratio, 400, 0.4).
narrative_ontology:measurement(kb_incoherent_tr_t600, kami_buddha_ontology__incoherent_bundle, theater_ratio, 600, 0.5).
narrative_ontology:measurement(kb_incoherent_tr_t800, kami_buddha_ontology__incoherent_bundle, theater_ratio, 800, 0.55).
narrative_ontology:measurement(kb_incoherent_tr_t1000, kami_buddha_ontology__incoherent_bundle, theater_ratio, 1000, 0.5).
narrative_ontology:measurement(kb_incoherent_tr_t1200, kami_buddha_ontology__incoherent_bundle, theater_ratio, 1200, 0.45).

% Extraction over time
narrative_ontology:measurement(kb_incoherent_be_t0, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(kb_incoherent_be_t200, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 200, 0.45).
narrative_ontology:measurement(kb_incoherent_be_t400, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 400, 0.55).
narrative_ontology:measurement(kb_incoherent_be_t600, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 600, 0.62).
narrative_ontology:measurement(kb_incoherent_be_t800, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 800, 0.68).
narrative_ontology:measurement(kb_incoherent_be_t1000, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 1000, 0.65).
narrative_ontology:measurement(kb_incoherent_be_t1200, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 1200, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(kb_incoherent_su_t0, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(kb_incoherent_su_t200, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 200, 0.4).
narrative_ontology:measurement(kb_incoherent_su_t400, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 400, 0.5).
narrative_ontology:measurement(kb_incoherent_su_t600, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 600, 0.65).
narrative_ontology:measurement(kb_incoherent_su_t800, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 800, 0.7).
narrative_ontology:measurement(kb_incoherent_su_t1000, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 1000, 0.65).
narrative_ontology:measurement(kb_incoherent_su_t1200, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 1200, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__incoherent_bundle, identity_coordination).
narrative_ontology:boltzmann_floor_override(kami_buddha_ontology__incoherent_bundle, 0.08).
narrative_ontology:affects_constraint(kami_buddha_ontology__incoherent_bundle, kami_buddha_ontology__honji_suijaku_monism).
narrative_ontology:affects_constraint(kami_buddha_ontology__incoherent_bundle, kami_buddha_ontology__domain_partition).
narrative_ontology:affects_constraint(kami_buddha_ontology__incoherent_bundle, meiji_shinbutsu_bunri).

% DUAL FORMULATION NOTE:
% The kami_buddha_ontology kernel decomposes into three constraint stories: honji_suijaku_monism (identity ontology, low extraction, Mountain-like from elite seat), domain_partition (functional separation, moderate extraction, Scaffold-like with Meiji sunset), and incoherent_bundle (this story — no ontology, high extraction, Tangled Rope). The upstream Mountain-like reading (honji_suijaku) is often cited as evidence for the system's coherence, creating a false summit that masks the downstream extraction. This story links to both siblings and to the Meiji separation constraint that attempted to resolve the bundle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kami_buddha_ontology__incoherent_bundle, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
