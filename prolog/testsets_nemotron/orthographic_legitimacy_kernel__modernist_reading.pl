% ============================================================================
% CONSTRAINT STORY: orthographic_legitimacy_kernel__modernist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_legitimacy_kernel__modernist_reading, []).

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
 *   constraint_id: orthographic_legitimacy_kernel__modernist_reading
 *   human_readable: Orthographic Legitimacy — Modernist Reading (Script as Rupture with Ottoman/Islamic Past)
 *   domain: political_linguistics/state_formation/commitment_systems
 *
 * SUMMARY:
 *   The 1928 Turkish Alphabet Law replaced the Perso-Arabic script with a
 *   Latin-based alphabet, framed as a modernization measure to raise literacy
 *   and align Turkey with Europe. The modernist reading holds that
 *   orthographic legitimacy derives from this alignment — the script change
 *   IS the rupture that constitutes the nation as modern, Western, and
 *   secular. This reading extracts heavily from the Ottoman literate class
 *   and religious scholars (rendered illiterate in their own heritage) while
 *   benefiting the modernizing state apparatus and secular intelligentsia.
 *   The constraint is a tangled rope: it coordinates a genuine national
 *   literacy standard (coordination function) but does so through asymmetric
 *   extraction that required active enforcement (suppression of Arabic
 *   script, criminalization of Ottoman Turkish instruction, purges of
 *   resistant educators). The coordination function is real — mass literacy
 *   did rise — but the extraction is structural, not incidental.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_legitimacy_kernel__modernist_reading, 0.82).
domain_priors:suppression_score(orthographic_legitimacy_kernel__modernist_reading, 0.75).
domain_priors:theater_ratio(orthographic_legitimacy_kernel__modernist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_legitimacy_kernel__modernist_reading, tangled_rope).
narrative_ontology:human_readable(orthographic_legitimacy_kernel__modernist_reading, "Orthographic Legitimacy — Modernist Reading (Script as Rupture with Ottoman/Islamic Past)").
narrative_ontology:topic_domain(orthographic_legitimacy_kernel__modernist_reading, "political_linguistics/state_formation/commitment_systems").

domain_priors:requires_active_enforcement(orthographic_legitimacy_kernel__modernist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_legitimacy_kernel__modernist_reading, '6624314d-abef-4db1-94c5-2f12623dd02e').
narrative_ontology:cs_kernel_codification('6624314d-abef-4db1-94c5-2f12623dd02e', formalized).
narrative_ontology:cs_authority_grounding('6624314d-abef-4db1-94c5-2f12623dd02e', extraction).
narrative_ontology:cs_interpretation_layer_present('6624314d-abef-4db1-94c5-2f12623dd02e').
narrative_ontology:cs_reading_relation('6624314d-abef-4db1-94c5-2f12623dd02e', orthographic_legitimacy_kernel__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('6624314d-abef-4db1-94c5-2f12623dd02e', orthographic_legitimacy_kernel__instrumentalist_reading, coexists_with).
narrative_ontology:cs_axiom('6624314d-abef-4db1-94c5-2f12623dd02e', foundational, western_modernity_as_teleological_destiny).
narrative_ontology:cs_axiom_status(western_modernity_as_teleological_destiny, holdable).
narrative_ontology:cs_axiom_grounding('6624314d-abef-4db1-94c5-2f12623dd02e', western_modernity_as_teleological_destiny, deontological).
narrative_ontology:cs_axiom('6624314d-abef-4db1-94c5-2f12623dd02e', foundational, national_identity_requires_script_rupture).
narrative_ontology:cs_axiom_status(national_identity_requires_script_rupture, holdable).
narrative_ontology:cs_axiom_grounding('6624314d-abef-4db1-94c5-2f12623dd02e', national_identity_requires_script_rupture, instrumental).
narrative_ontology:cs_axiom('6624314d-abef-4db1-94c5-2f12623dd02e', secondary, latin_script_as_civilizational_alignment).
narrative_ontology:cs_axiom_status(latin_script_as_civilizational_alignment, holdable).
narrative_ontology:cs_axiom_grounding('6624314d-abef-4db1-94c5-2f12623dd02e', latin_script_as_civilizational_alignment, conventional).
narrative_ontology:cs_reference_frame('6624314d-abef-4db1-94c5-2f12623dd02e', republican_founding_moment_1923_1928).
narrative_ontology:cs_drift_state('6624314d-abef-4db1-94c5-2f12623dd02e', contemporary_akp_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6624314d-abef-4db1-94c5-2f12623dd02e', '').
narrative_ontology:cs_kernel_id(orthographic_legitimacy_kernel__modernist_reading, orthographic_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__modernist_reading, modernizing_state_apparatus).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__modernist_reading, secular_nationalist_intelligentsia).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__modernist_reading, ottoman_literate_class).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__modernist_reading, religious_scholars_ulema).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__modernist_reading, arabic_script_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__modernist_reading, western_diplomatic_commercial_actors).
narrative_ontology:constraint_vindicates(orthographic_legitimacy_kernel__modernist_reading, western_modernity_as_teleological_destiny).
narrative_ontology:constraint_vindicates(orthographic_legitimacy_kernel__modernist_reading, national_identity_requires_script_rupture).
narrative_ontology:constraint_vindicates(orthographic_legitimacy_kernel__modernist_reading, latin_script_as_civilizational_alignment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decrees and enforces script reform (e.g., 1928 Turkish Alphabet Law), controlling education, bureaucracy, publishing, and legal registers. Collects legitimacy capital from Western alignment and breaks the textual authority of the old elite. Can pivot policy but bears low exit cost — the reform IS the state's founding performance.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, modernizing_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Gains professional monopoly over the new script's knowledge economy: teaching, journalism, translation, administration. Their cultural capital is denominated in Latin-script literacy. Exit is mobile — they can operate in European languages or emigrate — but their domestic status depends on the reform's persistence.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, secular_nationalist_intelligentsia, beneficiary,
    organized, biographical, mobile, national).

% Ottoman Turkish (Arabic script) literate bureaucrats, scribes, journalists, and landholders. Rendered functionally illiterate overnight by the reform; their archives, correspondence, and professional credentials become inaccessible without retraining. Exit is constrained — retraining is possible but costly, and their social authority is tied to the old script.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, ottoman_literate_class, payer,
    moderate, biographical, constrained, national).

% Guardians of Quranic Arabic and Ottoman scholarly tradition. The script reform severs direct textual access to the religious-legal corpus, forcing dependence on transliteration or translation controlled by the new secular elite. Identity-locked: their authority IS the chain of textual transmission; exit means abandoning the epistemic ground of their vocation.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, religious_scholars_ulema, payer,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(orthographic_legitimacy_kernel__modernist_reading, religious_scholars_ulema, excluded).

% Calligraphers, Quranic copyists, madrasa teachers, and provincial notaries whose livelihood and social role are bound to Arabic script. No institutional pathway to retraining; the reform criminalizes their practice in official domains. Trapped — no exit without abandoning craft and community.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, arabic_script_practitioners, payer,
    powerless, immediate, trapped, local).

% European states, firms, and cultural institutes gain reduced transaction costs: treaties, contracts, and correspondence no longer require Ottoman-script intermediaries. Their benefit is incidental to the reform's internal logic but structurally reinforcing. Arbitrage-grade exit — they operate across scripts regardless.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, western_diplomatic_commercial_actors, beneficiary,
    powerful, biographical, arbitrage, global).

% Analyze the reform as a case study in script politics, nationalist myth-making, and literacy engineering. No material stake; their frame is the long-term comparative record of script changes (Turkey 1928, USSR 1920s-30s, Vietnam, Azerbaijan, Kazakhstan).
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, comparative_historical_linguists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a unified national literacy standard aligned with European scientific, legal, and commercial communication, enabling mass schooling, bureaucratic interoperability, and integration into Western-dominated global information flows — but only by severing the population from its own textual heritage.
% TRANSFER_FUNCTION: Moves epistemic authority, bureaucratic legibility, and cultural capital from the Ottoman literate class and religious scholars to the secular nationalist intelligentsia and state apparatus, via the mechanism of script replacement that renders the old elite's textual capital worthless.
% ABSENT_VOICES: Provincial madrasa networks, Sufi orders maintaining Arabic-script textual practices, minority communities (Armenian, Greek, Jewish) whose own script traditions were pressured by the monolingual Latin-script mandate, and the illiterate peasant majority who experienced the reform as another top-down imposition without consultation — all structurally excluded from the constituent assembly that ratified the Alphabet Law.
% DISAPPEARANCE_RATIONALE: If the Latin-script mandate and its enforcement vanished overnight, Turkish would not spontaneously revert to Arabic script — but the legal, educational, and publishing infrastructure built on Latin script would face a legitimacy crisis. Competing script claims (Arabic for religious/traditional legitimacy, Latin for modernity/Western alignment) would reopen the orthographic field, triggering a struggle over which script confers authentic national belonging.
% FOUNDING_PROBLEM: The Ottoman Empire's multi-script, multi-lingual textual order was incompatible with the homogenized, centralized nation-state the republican elite sought to build. The Arabic script indexed a cosmopolitan Islamic ecumene; the Latin script indexed a bounded European national future. The reform was built to solve the problem of 'how to make the population legible to a secular, Western-oriented state apparatus.'
% FOUNDING_PROBLEM_CORROBORATION: Republican historiography (e.g., Atatürk's Nutuk, early CHP congress records) attests the founding problem as live and solved by the reform. Ottomanist historians (e.g., Carter Findley, Edhem Eldem) and Islamist intellectuals (e.g., Ali Bulaç) attest the problem was fabricated — the Ottoman state was already centralizing literacy in Ottoman Turkish, and the reform's real function was breaking the ulema's textual authority. The corroboration split maps exactly to the modernist vs. continuity reading divide.
narrative_ontology:disappearance_verdict(orthographic_legitimacy_kernel__modernist_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_legitimacy_kernel__modernist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_legitimacy_kernel__modernist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(orthographic_legitimacy_kernel__modernist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_legitimacy_kernel__modernist_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_legitimacy_kernel__modernist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(orthographic_legitimacy_kernel__modernist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(orthographic_legitimacy_kernel__modernist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.82 at interval end) reflects the reform's constitutive extraction: the Ottoman literate class lost not just a script but their epistemic authority, professional standing, and intergenerational transmission. Suppression (0.75) captures the active enforcement: Arabic script banned in public, Ottoman Turkish purged from education, religious instruction restricted. Theater ratio (0.28) is moderate — the literacy campaign was real and effective, but the performative 'modernity' signaling (Latin script as proof of Westernness) grew over time, especially post-1980 as the reform's instrumental justification weakened. Accessibility collapse (0.68) is high but not total: Ottoman Turkish remains accessible via transliteration and specialized training, but the barrier is severe for the general population. Resistance (0.55) reflects ongoing contestation: Islamist and conservative currents never fully accepted the rupture; recent Ottoman Turkish elective courses and Erdoğan's 'we will teach Ottoman in schools' rhetoric signal persistent resistance.
 *
 * PERSPECTIVAL GAP:
 *   From the state/intelligentsia seat: a successful coordination problem solved (mass literacy, Western integration). From the ulema/Ottoman literate seat: an epistemic dispossession enforced at gunpoint. From the analytical seat: a tangled rope whose coordination function (literacy standardization) is real but whose extraction function (breaking the old elite's textual capital) is constitutive, not accidental. The engine computes this divergence from the structural data — the modernist reading's claimed_type (tangled_rope) admits both functions; the continuity reading would claim mountain (script as natural tradition); the instrumentalist reading would claim rope (efficiency only).
 *
 * DIRECTIONALITY LOGIC:
 *   The modernizing state apparatus (agenda_setter, institutional, arbitrage exit) sits at d ≈ 0.1 — full beneficiary, the constraint subsidizes its legitimacy. Secular nationalist intelligentsia (beneficiary, organized, mobile) at d ≈ 0.25 — collects professional rents from the new script economy. Western actors (beneficiary, powerful, arbitrage) at d ≈ 0.15 — incidental beneficiaries with zero stake in enforcement. Ottoman literate class (payer, moderate, constrained) at d ≈ 0.75 — bears high extraction with difficult but possible retraining. Religious scholars (payer/excluded, organized, identity_locked) at d ≈ 0.95 — identity-locked target; their authority is the textual chain the reform severs. Arabic script practitioners (payer, powerless, trapped) at d ≈ 0.98 — no exit, total extraction. Observers (analytical) at d = 0.5 by definition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (making the population legible to a secular state) is contested — republican heirs say solved; critics say fabricated. The mandate has not atrophied into a piton because the script remains a live front in the culture war: AKP's Ottoman Turkish revivalism and CHP's defensive Latin-script sacralization both invest in the constraint's persistence. If the founding problem is dead (literacy achieved, Western integration achieved), the constraint persists as identity theater — but the theater is contested, not inert. Mandatrophy unresolved: the constraint's legitimacy still depends on which founding problem you credit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    literacy_gains_vs_epistemic_loss,
    'Do the measured literacy gains under Latin script outweigh the epistemic loss of severing the population from 600 years of Ottoman textual production?',
    'Counterfactual modeling: compare literacy trajectories in Turkey vs. countries that retained Arabic script (e.g., Iran, Egypt) controlling for development; assess accessibility of Ottoman archives to contemporary researchers vs. Persian/Arabic archives.',
    'If literacy gains are comparable without script rupture, the extraction was unnecessary — the constraint collapses toward snare. If gains are uniquely attributable to Latin script, the coordination function bears more weight — tangled_rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literacy_gains_vs_epistemic_loss, empirical, 'Whether the coordination benefit (literacy) required the extraction mechanism (script rupture).').

omega_variable(
    identity_lock_mechanism_ulema,
    'Is the ulema''s identity-locked exit (d ≈ 0.95) a structural feature of the constraint or a contingent outcome of their own refusal to adapt?',
    'Trace the historical record: did the early republican state offer pathways for ulema to retrain and participate in the new system (e.g., theology faculties, religious affairs directorate), or were they structurally excluded from the outset?',
    'If exclusion was structural (state policy), the constraint''s extraction is intentional — snare/tangled_rope confirmed. If ulema self-excluded, the identity lock is partly endogenous — extraction still high but agency distribution shifts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_ulema, conceptual, 'Whether the religious scholars'' trapped position was imposed or chosen.').

omega_variable(
    modernist_reading_kernel_location,
    'Is the modernist reading''s core premise (Western modernity as teleological destiny requiring script rupture) a founding axiom of the Turkish Republic or a post-hoc rationalization of a power grab?',
    'Analyze the 1920s parliamentary debates, Atatürk''s private correspondence, and the Law''s preparatory commission records for the sequence: did the rupture rationale precede the decision, or was it constructed to justify a decision taken on other grounds?',
    'If post-hoc, the modernist reading is a cover story — the constraint''s true type is snare (extraction masked as coordination). If genuine founding axiom, tangled_rope stands: coordination and extraction are genuinely hybrid.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(modernist_reading_kernel_location, conceptual, 'Whether the modernist reading''s normative core is authentic or fabricated.').

omega_variable(
    continuity_reading_foreclosure_structure,
    'Does the modernist reading''s core premise (rupture as legitimacy) logically foreclose the continuity reading (tradition as legitimacy), or do they coexist as competing legitimacies in a fragmented polity?',
    'Examine whether any single political actor or institution can simultaneously legitimize itself through BOTH rupture-from-Ottoman and continuity-with-Ottoman claims without contradiction — e.g., AKP''s simultaneous invocation of Ottoman grandeur and Turkish national sovereignty.',
    'If forecloses: the kernel has a structural fault line; no stable framework can hold both readings. If coexists_with: the kernel sustains a permanent legitimacy competition — the constraint family is a site of ongoing contestation, not resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuity_reading_foreclosure_structure, conceptual, 'Structural relationship between modernist and continuity readings within the kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_legitimacy_kernel__modernist_reading, 1928, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_leg_mod_tr_t1928, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 1928, 0.12).
narrative_ontology:measurement(orth_leg_mod_tr_t1950, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 1950, 0.18).
narrative_ontology:measurement(orth_leg_mod_tr_t1980, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(orth_leg_mod_tr_t2000, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 2000, 0.22).
narrative_ontology:measurement(orth_leg_mod_tr_t2024, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(orth_leg_mod_be_t1928, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 1928, 0.88).
narrative_ontology:measurement(orth_leg_mod_be_t1950, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 1950, 0.72).
narrative_ontology:measurement(orth_leg_mod_be_t1980, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 1980, 0.65).
narrative_ontology:measurement(orth_leg_mod_be_t2000, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(orth_leg_mod_be_t2024, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 2024, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(orth_leg_mod_su_t1928, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 1928, 0.9).
narrative_ontology:measurement(orth_leg_mod_su_t1950, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 1950, 0.7).
narrative_ontology:measurement(orth_leg_mod_su_t1980, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 1980, 0.6).
narrative_ontology:measurement(orth_leg_mod_su_t2000, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(orth_leg_mod_su_t2024, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_legitimacy_kernel__modernist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(orthographic_legitimacy_kernel__modernist_reading, 0.1).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__modernist_reading, orthographic_legitimacy_kernel__continuity_reading).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__modernist_reading, orthographic_legitimacy_kernel__instrumentalist_reading).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__modernist_reading, turkish_language_reform_1928).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__modernist_reading, secularism_establishment_constraint).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__modernist_reading, ottoman_archive_access_constraint).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the orthographic_legitimacy_kernel. The continuity_reading claims legitimacy from preserving textual tradition (claimed_type: mountain or rope). The instrumentalist_reading claims legitimacy from literacy/efficiency (claimed_type: rope). This modernist_reading claims legitimacy from Western alignment/rupture (claimed_type: tangled_rope). The ε values differ radically: continuity ≈ 0.1, instrumentalist ≈ 0.3, modernist ≈ 0.8. They share the same referent (Turkish script politics) but instantiate different constraints with different beneficiary/victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(orthographic_legitimacy_kernel__modernist_reading, organized, 0.95).
constraint_indexing:directionality_override(orthographic_legitimacy_kernel__modernist_reading, powerless, 0.98).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
