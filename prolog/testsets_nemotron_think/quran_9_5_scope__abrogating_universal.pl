% ============================================================================
% CONSTRAINT STORY: quran_9_5_scope__abrogating_universal
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_9_5_scope__abrogating_universal, []).

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
 *   constraint_id: quran_9_5_scope__abrogating_universal
 *   human_readable: Universal Offensive Jihad Mandate (Abrogating Reading of Q9:5)
 *   domain: religious/political/theological
 *
 * SUMMARY:
 *   This constraint story models the 'abrogating_universal' reading of Quran
 *   9:5 (the 'Verse of the Sword'): 'When the sacred months have passed, kill
 *   the polytheists wherever you find them...' In this reading, 9:5 acts as
 *   nasikh (abrogator) that permanently cancels all prior Quranic verses
 *   commanding patience, defensive-only warfare, or peaceful coexistence
 *   (e.g., 2:256 'no compulsion in religion,' 8:61 'incline to peace,' 60:8
 *   'God does not forbid you from those who do not fight you'). The
 *   constraint establishes offensive jihad against all non-Muslims (primarily
 *   polytheists, extended to People of the Book by most classical schools) as
 *   a standing, universal legal obligation until global submission (Islam) or
 *   subjugation (dhimma/jizya). It is enforced by caliphal states and
 *   non-state jihadist movements through military conquest, dhimma
 *   restrictions, apostasy laws, and ideological indoctrination. The
 *   coordination story — unifying the ummah under divine sovereignty —
 *   functions as cover for a structure that extracts lives, labor, land, and
 *   autonomy from non-Muslims and conscripts Muslims into perpetual warfare.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_9_5_scope__abrogating_universal, 0.85).
domain_priors:suppression_score(quran_9_5_scope__abrogating_universal, 0.9).
domain_priors:theater_ratio(quran_9_5_scope__abrogating_universal, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, extractiveness, 0.85).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_9_5_scope__abrogating_universal, snare).
narrative_ontology:human_readable(quran_9_5_scope__abrogating_universal, "Universal Offensive Jihad Mandate (Abrogating Reading of Q9:5)").
narrative_ontology:topic_domain(quran_9_5_scope__abrogating_universal, "religious/political/theological").

domain_priors:requires_active_enforcement(quran_9_5_scope__abrogating_universal).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_9_5_scope__abrogating_universal, '8214cfc3-8b70-4bb0-93bf-781dba5d8567').
narrative_ontology:cs_kernel_codification('8214cfc3-8b70-4bb0-93bf-781dba5d8567', formalized).
narrative_ontology:cs_authority_grounding('8214cfc3-8b70-4bb0-93bf-781dba5d8567', lineage).
narrative_ontology:cs_interpretation_layer_present('8214cfc3-8b70-4bb0-93bf-781dba5d8567').
narrative_ontology:cs_reading_relation('8214cfc3-8b70-4bb0-93bf-781dba5d8567', quran_9_5_scope__contextual_defensive, forecloses).
narrative_ontology:cs_reading_relation('8214cfc3-8b70-4bb0-93bf-781dba5d8567', quran_9_5_scope__progressive_synthesis, forecloses).
narrative_ontology:cs_axiom('8214cfc3-8b70-4bb0-93bf-781dba5d8567', foundational, verse_9_5_abrogates_all_peaceful_verses).
narrative_ontology:cs_axiom_status(verse_9_5_abrogates_all_peaceful_verses, holdable).
narrative_ontology:cs_axiom_grounding('8214cfc3-8b70-4bb0-93bf-781dba5d8567', verse_9_5_abrogates_all_peaceful_verses, conventional).
narrative_ontology:cs_axiom('8214cfc3-8b70-4bb0-93bf-781dba5d8567', foundational, offensive_jihad_universal_obligation).
narrative_ontology:cs_axiom_status(offensive_jihad_universal_obligation, holdable).
narrative_ontology:cs_axiom_grounding('8214cfc3-8b70-4bb0-93bf-781dba5d8567', offensive_jihad_universal_obligation, theological).
narrative_ontology:cs_reference_frame('8214cfc3-8b70-4bb0-93bf-781dba5d8567', universal_abrogating_command).
narrative_ontology:cs_drift_state('8214cfc3-8b70-4bb0-93bf-781dba5d8567', classical_fiqh_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8214cfc3-8b70-4bb0-93bf-781dba5d8567', '').
narrative_ontology:cs_kernel_id(quran_9_5_scope__abrogating_universal, quran_9_5_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_9_5_scope__abrogating_universal, expansionist_movements).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__abrogating_universal, caliphate_claimants).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, non_muslim_polytheists).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, people_of_the_book).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, muslim_civilians).
narrative_ontology:constraint_vindicates(quran_9_5_scope__abrogating_universal, nasikh_doctrine).
narrative_ontology:constraint_vindicates(quran_9_5_scope__abrogating_universal, divine_sovereignty_manifest).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Jihadist groups (e.g., ISIS, Al-Qaeda, historical Kharijites) and caliphal claimants who propagate and enforce this reading. They recruit, wage offensive warfare, administer conquered territories, and extract resources (spoils, jizya, land, labor) from non-Muslim populations. Their legitimacy and material sustenance depend on the constraint's enforcement. Exit requires abandoning the core ideological commitment that defines their organizational identity.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, expansionist_movements, agenda_setter,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(quran_9_5_scope__abrogating_universal, expansionist_movements, beneficiary).

% State-like entities (historical Umayyad/Abbasid/Ottoman caliphates, ISIS 'caliphate') that institutionalize the constraint as positive law. They maintain armies, courts, and tax systems (jizya, kharaj) structured around the offensive jihad mandate. They collect the bulk of material extraction and coordinate enforcement. Exit means relinquishing the theological basis of their sovereignty.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, caliphate_claimants, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(quran_9_5_scope__abrogating_universal, caliphate_claimants, beneficiary).

% Primary target population designated as 'mushrikun' (polytheists/idolaters). Under this reading, they face three options: conversion to Islam, death, or (in some schools) enslavement — no protected status (dhimma) is available. Their lives, property, families, and religious autonomy are directly exposed to seizure. Exit is structurally blocked: conversion is coerced, flight is prevented by territorial control, resistance is met with overwhelming force.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, non_muslim_polytheists, payer,
    powerless, biographical, trapped, global).

% Christians, Jews, Zoroastrians, and others granted theoretical 'dhimmi' status in classical fiqh, but under this reading their protection is conditional and revocable. They pay jizya (poll tax) and kharaj (land tax), suffer legal disabilities (court testimony restrictions, building restrictions, dress codes), and face periodic revocation of protection when rulers choose 'purification.' Exit options: conversion (coerced), flight (dangerous), endurance (costly).
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, people_of_the_book, payer,
    moderate, biographical, constrained, global).

% Ordinary Muslims living under movements or states enforcing this reading. They bear costs: conscription into offensive wars, taxation to fund jihad, social enforcement of conformity, blowback violence from targeted populations, and economic isolation from non-Muslim trade. Dissent is treated as apostasy or hypocrisy (nifaq). Exit requires hijra (migration) to non-enforcing lands — often dangerous, costly, and religiously contested.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, muslim_civilians, payer,
    moderate, biographical, constrained, global).

% Classical ulema (Al-Azhar, Deoband, Najaf, Zaytuna) who maintain the defensive-only or restricted-offensive jihad doctrine with conditions (Caliphal authority, invitation to Islam first, proportionality). They are marginalized, denounced as 'court scholars' or 'murji'a,' and sometimes physically threatened. Their exit is constrained by institutional position and communal ties; open opposition risks delegitimization or violence.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, traditional_scholars, excluded,
    institutional, generational, constrained, global).

% Scholars of Islamic law, political theorists, historians, intelligence analysts, and interfaith diplomats who analyze the constraint from outside the commitment structure. They document its operation, track its enforcement, and assess its impact on international order. They neither collect nor pay; their situation is epistemic.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, external_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the ummah toward the divine mandate of universal Islamic governance by providing a clear, non-negotiable legal framework for relations with non-Muslim polities, resolving ambiguity about war/peace boundaries and legitimizing centralized command over violence.
% TRANSFER_FUNCTION: Transfers physical security, political autonomy, property rights, and religious freedom from non-Muslim populations to expansionist movements and caliphal institutions; transfers the burden of offensive warfare (lives, wealth, social stability) onto Muslim populations as a non-optional religious obligation.
% ABSENT_VOICES: Non-Muslim populations subject to the constraint (polytheists denied any protected status, People of the Book whose dhimma is treated as revocable concession), moderate Muslim scholars who maintain defensive-only jihad doctrine (Hanafi restriction to Caliphal authority, Maliki defensive prioritization), contemporary Muslim reformers (Fazlur Rahman, Abdullahi An-Na'im, Khaled Abou El Fadl) who argue for historical contextualization, and the silent majority of Muslims who do not support offensive jihad but lack safe channels to dissent.
% DISAPPEARANCE_RATIONALE: Removal would eliminate the primary theological authorization for offensive jihad used by Salafi-jihadist groups and historical caliphates, forcing reversion to defensive-only or coexistence frameworks (as in contextual_defensive and progressive_synthesis readings). This would fundamentally alter inter-civilizational relations, dismantle the legal architecture of dhimma/subjugation, and trigger a theological crisis in movements whose identity is fused to the mandate.
% FOUNDING_PROBLEM: The problem of legitimate political relations between the nascent Muslim community and surrounding non-Muslim polities after the Prophet's death: whether to consolidate defensively within Arabia or expand universally as a divinely mandated mission to establish God's sovereignty on earth.
% FOUNDING_PROBLEM_CORROBORATION: Classical fiqh authorities (al-Shafi'i in Kitab al-Umm, Ibn Taymiyyah in Al-Sarim al-Maslul, al-Mawardi in Al-Ahkam al-Sultaniyya) corroborate the expansionist reading as mainstream in medieval Sunnism. Modern critical historians (Patricia Crone, Fred Donner, Michael Cook) and reformist scholars (Fazlur Rahman, Abdullahi An-Na'im, Mohammad Hashim Kamali) corroborate that the founding problem was specific to 7th-century Arabian tribal politics and the Prophet's specific political context, not a universal theological mandate.
narrative_ontology:disappearance_verdict(quran_9_5_scope__abrogating_universal, world_rearranges).
narrative_ontology:founding_problem_status(quran_9_5_scope__abrogating_universal, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_9_5_scope__abrogating_universal, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quran_9_5_scope__abrogating_universal, 'none', 1).
narrative_ontology:epsilon_provenance(quran_9_5_scope__abrogating_universal, 0.85, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_9_5_scope__abrogating_universal_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quran_9_5_scope__abrogating_universal, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quran_9_5_scope__abrogating_universal_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.85) is very high: the constraint transfers existential goods (life, liberty, property, religious freedom) from vast non-Muslim populations to a narrow set of expansionist actors, with no reciprocity. Suppression (0.9) is near-total: alternatives (coexistence, defensive-only doctrines, pluralism) are theologically delegitimized as 'kufr' or 'munafiq,' and exit (conversion, flight, dissent) is blocked by force or identity-fusion. Theater ratio (0.4) reflects genuine coordination of believers (shared identity, purpose, logistical mobilization) mixed with performative brutality (staged executions, propaganda) that serves internal cohesion more than strategic effect. Accessibility collapse (0.7) is high but not absolute: the contextual_defensive and progressive_synthesis readings persist as live alternatives, preventing total closure. Resistance (0.6) is substantial: classical scholars imposed conditions (Caliphal authority, invitation requirement, proportionality) that limited the constraint; modern states, moderate scholars, and targeted populations actively resist.
 *
 * PERSPECTIVAL GAP:
 *   From the expansionist seat, the constraint appears as Rope: it solves the genuine coordination problem of mobilizing a dispersed ummah against fragmentation, providing clear rules for war/peace/taxation. From the non-Muslim payer seats, it appears as Snare: pure extraction enforced by violence, with the coordination story as transparent cover. From the Muslim_civilian seat, it appears as Tangled Rope: they receive identity/belonging (coordination) but pay blood/treasure (extraction). From traditional_scholars, it appears as a corrupted Piton: a once-functional defensive doctrine degraded into offensive overreach. The engine computes these divergences; the authored claim (snare) reflects the structural reality from the victim seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Expansionist_movements and caliphate_claimants are structural beneficiaries (d ~ 0.1): they collect extraction (spoils, jizya, recruits, legitimacy) and set the agenda. Non_muslim_polytheists are full targets (d ~ 1.0): trapped, identity-denied, facing death/conversion. People_of_the_book are high-target payers (d ~ 0.8): constrained exit (dhimma), conditional protection, heavy extraction. Muslim_civilians are moderate payers (d ~ 0.6): conscripted, taxed, socially policed, but hold some in-group status. Traditional_scholars are excluded (d ~ 0.7): institutional power but ideologically marginalized by the reading. External_observers are analytical (d = 0.5): symmetric epistemic position. The engine computes per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate (universal divine sovereignty) has outlived its founding problem (7th-century Arabian tribal consolidation). Classical fiqh partially resolved mandatrophy by adding restrictive conditions (Caliphal authority, defensive prioritization in later schools). Modern jihadist movements re-activated the raw mandate by stripping those conditions, claiming 'return to origins.' The snare classification captures this: the coordination function is vestigial (no genuine collective-action problem requires offensive jihad today), while extraction persists and intensifies. The mandate is not 'resolved' — it is weaponized anew.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nasikh_doctrine_historicity,
    'Is the naskh (abrogation) doctrine applied to 9:5 historically authentic to the Prophet/companions, or a later scholarly construction (2nd/3rd century AH) to resolve textual tensions?',
    'Critical isnad analysis of early abrogation reports; comparison of early Quranic manuscripts and companion codices for evidence of verse ordering awareness; historical linguistics of ''nasikh'' terminology development.',
    'If naskh is a later construction, the constraint''s claim to divine authority collapses — it becomes a human hermeneutic choice, not revelation. This would reclassify the constraint from ''divine mandate'' to ''scholarly construct,'' shifting its epistemic ground and potentially its extraction profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nasikh_doctrine_historicity, empirical, 'Historical authenticity of the abrogation doctrine applied to 9:5.').

omega_variable(
    verse_9_5_original_scope,
    'Was 9:5 understood by the Prophet and immediate companions as a universal, eternal command against all polytheists, or as a specific response to the violation of the Treaty of Hudaybiyyah by specific tribes (Bakr, Kinana)?',
    'Sira/maghazi literature analysis for companion practice post-9:5; early tafsir (Ibn Abbas, Mujahid, Qatadah) on the verse''s occasion of revelation (asbab al-nuzul); comparison with 9:4 (treaty completion) and 9:6 (protection for seekers) which survive in the same chapter.',
    'If the original scope was specific, the universal reading is an expansionist extrapolation — the constraint''s ε reflects hermeneutic inflation, not textual necessity. This would support the contextual_defensive reading as historically prior.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verse_9_5_original_scope, empirical, 'Original historical scope of 9:5: universal vs. contextual.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the constraint''s suppression of coexistence frameworks primarily structural (state enforcement: apostasy laws, blasphemy codes, dhimma restrictions, military conscription) or internalized (theological internalization by believers who self-police, view dissent as apostasy, fuse identity to the mandate)?',
    'Post-exit suppression trajectory study: track individuals who leave jihadist movements or reject the reading — does suppression persist internally (guilt, fear, social death) after structural enforcement is removed? Compare with de-radicalization program outcomes.',
    'If internalized, effective suppression is higher than structural measures suggest — the constraint survives de-territorialization because targets carry it psychologically. This would increase the constraint''s resilience metric and explain its revival after colonial-era dormancy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism.').

omega_variable(
    coordination_extraction_boundary,
    'Is there any genuine coordination function (solving a collective-action problem for Muslims) that is structurally separable from the extraction function (transfer from non-Muslims), or is the coordination story entirely instrumental to extraction?',
    'Counterfactual analysis: if offensive jihad were removed but defensive coordination (mutual defense, zakat distribution, dispute resolution) remained, would Muslim communities face worse collective outcomes? Historical comparison: Muslim polities that adopted defensive-only postures (Ottoman later period, modern nation-states) vs. expansionist ones.',
    'If coordination is separable, the constraint is a Snare with a removable coordination veneer. If inseparable, part of measured ε is the price of the coordination itself (Tangled Rope). This determines whether reform is possible within the tradition or requires exit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Separability of coordination and extraction functions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_9_5_scope__abrogating_universal, 0, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(q95au_tr_t0, quran_9_5_scope__abrogating_universal, theater_ratio, 0, 0.2).
narrative_ontology:measurement(q95au_tr_t100, quran_9_5_scope__abrogating_universal, theater_ratio, 100, 0.3).
narrative_ontology:measurement(q95au_tr_t300, quran_9_5_scope__abrogating_universal, theater_ratio, 300, 0.35).
narrative_ontology:measurement(q95au_tr_t600, quran_9_5_scope__abrogating_universal, theater_ratio, 600, 0.45).
narrative_ontology:measurement(q95au_tr_t1000, quran_9_5_scope__abrogating_universal, theater_ratio, 1000, 0.55).
narrative_ontology:measurement(q95au_tr_t1300, quran_9_5_scope__abrogating_universal, theater_ratio, 1300, 0.6).
narrative_ontology:measurement(q95au_tr_t1400, quran_9_5_scope__abrogating_universal, theater_ratio, 1400, 0.4).

% Extraction over time
narrative_ontology:measurement(q95au_be_t0, quran_9_5_scope__abrogating_universal, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(q95au_be_t100, quran_9_5_scope__abrogating_universal, base_extractiveness, 100, 0.75).
narrative_ontology:measurement(q95au_be_t300, quran_9_5_scope__abrogating_universal, base_extractiveness, 300, 0.85).
narrative_ontology:measurement(q95au_be_t600, quran_9_5_scope__abrogating_universal, base_extractiveness, 600, 0.8).
narrative_ontology:measurement(q95au_be_t1000, quran_9_5_scope__abrogating_universal, base_extractiveness, 1000, 0.7).
narrative_ontology:measurement(q95au_be_t1300, quran_9_5_scope__abrogating_universal, base_extractiveness, 1300, 0.4).
narrative_ontology:measurement(q95au_be_t1400, quran_9_5_scope__abrogating_universal, base_extractiveness, 1400, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(q95au_su_t0, quran_9_5_scope__abrogating_universal, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(q95au_su_t100, quran_9_5_scope__abrogating_universal, suppression_requirement, 100, 0.8).
narrative_ontology:measurement(q95au_su_t300, quran_9_5_scope__abrogating_universal, suppression_requirement, 300, 0.9).
narrative_ontology:measurement(q95au_su_t600, quran_9_5_scope__abrogating_universal, suppression_requirement, 600, 0.85).
narrative_ontology:measurement(q95au_su_t1000, quran_9_5_scope__abrogating_universal, suppression_requirement, 1000, 0.8).
narrative_ontology:measurement(q95au_su_t1300, quran_9_5_scope__abrogating_universal, suppression_requirement, 1300, 0.6).
narrative_ontology:measurement(q95au_su_t1400, quran_9_5_scope__abrogating_universal, suppression_requirement, 1400, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_9_5_scope__abrogating_universal, identity_coordination).
narrative_ontology:boltzmann_floor_override(quran_9_5_scope__abrogating_universal, 0.08).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, islamic_jihad_doctrine).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, dhimma_system).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, apostasy_laws).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, caliphate_institution).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, quran_9_29_jizya).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, hudud_penalties).

% DUAL FORMULATION NOTE:
% This constraint is one member of the quran_9_5_scope kernel family. The kernel decomposes into three structurally distinct constraints with divergent ε: (1) abrogating_universal (this story, ε=0.85, snare) — universal offensive mandate; (2) contextual_defensive (ε≈0.15, rope) — specific defensive response to treaty violation; (3) progressive_synthesis (ε≈0.05, mountain) — time-bound directive superseded by ethical trajectory. The ε-invariance principle requires separate stories because the same verse label yields radically different extraction profiles under different hermeneutics. This story's high ε reflects the reading's own assessment of the standing arrangement (universal offensive jihad), not the siblings' alternatives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quran_9_5_scope__abrogating_universal, institutional, 0.15).
constraint_indexing:directionality_override(quran_9_5_scope__abrogating_universal, organized, 0.1).
constraint_indexing:directionality_override(quran_9_5_scope__abrogating_universal, powerless, 0.95).
constraint_indexing:directionality_override(quran_9_5_scope__abrogating_universal, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
