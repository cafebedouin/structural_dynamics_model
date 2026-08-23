% ============================================================================
% CONSTRAINT STORY: turkish_graphemic_substrate__gradual_transition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_turkish_graphemic_substrate__gradual_transition_reading, []).

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
 *   constraint_id: turkish_graphemic_substrate__gradual_transition_reading
 *   human_readable: Managed Dual-Script Transition Policy (1928-1940s)
 *   domain: political_linguistics/state_formation/cultural_engineering
 *
 * SUMMARY:
 *   The 1928 Turkish Script Law mandated Latin script for Turkish, but a de
 *   facto managed transition persisted through the 1930s-40s: dual-script
 *   newspapers, bilingual education tracts, Arabic-script religious
 *   publications tolerated under supervision, and family archives remaining
 *   in Arabic script. This constraint story models the
 *   *gradual_transition_reading* — the claim that the state deliberately
 *   managed a 5-15 year coexistence period to preserve intergenerational
 *   knowledge transfer. The reading sits between the
 *   ottoman_continuity_reading (Arabic script as legitimate substrate) and
 *   the secular_nationalist_reading (Latin script as immediate rupture with
 *   the past). The constraint is claimed as a Scaffold (transitional,
 *   sunsetted coordination) but the metrics reveal substantial extraction:
 *   the state used the transition period to dismantle the Islamic scholarly
 *   establishment's epistemic monopoly while presenting the process as
 *   benevolent management.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(turkish_graphemic_substrate__gradual_transition_reading, 0.65).
domain_priors:suppression_score(turkish_graphemic_substrate__gradual_transition_reading, 0.55).
domain_priors:theater_ratio(turkish_graphemic_substrate__gradual_transition_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(turkish_graphemic_substrate__gradual_transition_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(turkish_graphemic_substrate__gradual_transition_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__gradual_transition_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(turkish_graphemic_substrate__gradual_transition_reading, scaffold).
narrative_ontology:human_readable(turkish_graphemic_substrate__gradual_transition_reading, "Managed Dual-Script Transition Policy (1928-1940s)").
narrative_ontology:topic_domain(turkish_graphemic_substrate__gradual_transition_reading, "political_linguistics/state_formation/cultural_engineering").

domain_priors:requires_active_enforcement(turkish_graphemic_substrate__gradual_transition_reading).
narrative_ontology:has_sunset_clause(turkish_graphemic_substrate__gradual_transition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(turkish_graphemic_substrate__gradual_transition_reading, '2183fa11-92e2-4c33-bc5d-d2af8d562c8e').
narrative_ontology:cs_kernel_codification('2183fa11-92e2-4c33-bc5d-d2af8d562c8e', formalized).
narrative_ontology:cs_authority_grounding('2183fa11-92e2-4c33-bc5d-d2af8d562c8e', extraction).
narrative_ontology:cs_interpretation_layer_present('2183fa11-92e2-4c33-bc5d-d2af8d562c8e').
narrative_ontology:cs_reading_relation('2183fa11-92e2-4c33-bc5d-d2af8d562c8e', turkish_graphemic_substrate__ottoman_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('2183fa11-92e2-4c33-bc5d-d2af8d562c8e', turkish_graphemic_substrate__secular_nationalist_reading, coexists_with).
narrative_ontology:cs_axiom('2183fa11-92e2-4c33-bc5d-d2af8d562c8e', foundational, managed_transition_preserves_continuity).
narrative_ontology:cs_axiom_status(managed_transition_preserves_continuity, holdable).
narrative_ontology:cs_axiom_grounding('2183fa11-92e2-4c33-bc5d-d2af8d562c8e', managed_transition_preserves_continuity, conventional).
narrative_ontology:cs_axiom('2183fa11-92e2-4c33-bc5d-d2af8d562c8e', foundational, state_authority_over_script_evolution).
narrative_ontology:cs_axiom_status(state_authority_over_script_evolution, holdable).
narrative_ontology:cs_axiom_grounding('2183fa11-92e2-4c33-bc5d-d2af8d562c8e', state_authority_over_script_evolution, conventional).
narrative_ontology:cs_reference_frame('2183fa11-92e2-4c33-bc5d-d2af8d562c8e', managed_bilingual_transition).
narrative_ontology:cs_drift_state('2183fa11-92e2-4c33-bc5d-d2af8d562c8e', transition_end, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2183fa11-92e2-4c33-bc5d-d2af8d562c8e', '').
narrative_ontology:cs_kernel_id(turkish_graphemic_substrate__gradual_transition_reading, turkish_graphemic_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__gradual_transition_reading, state_reformers).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__gradual_transition_reading, younger_generation).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__gradual_transition_reading, european_aligned_intellectuals).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__gradual_transition_reading, arabic_script_literate_generation).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__gradual_transition_reading, islamic_scholarly_establishment).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__gradual_transition_reading, conservative_population).
narrative_ontology:constraint_vindicates(turkish_graphemic_substrate__gradual_transition_reading, state_authority_over_script_evolution).
narrative_ontology:constraint_vindicates(turkish_graphemic_substrate__gradual_transition_reading, managed_transition_preserves_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and enforce the dual-script transition through the 1928 Script Law and subsequent education decrees. Control the timeline, curriculum, and publication standards. Gain political legitimacy as modernizers and break the religious establishment's monopoly on literacy. Can pivot to full Latin enforcement if transition stalls.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, state_reformers, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(turkish_graphemic_substrate__gradual_transition_reading, state_reformers, beneficiary).

% Adults educated in Ottoman Arabic script (pre-1928) who face functional illiteracy in the new system. Bear costs of relearning or exclusion from official life. Some maintain private Arabic-script networks (religious study, family correspondence). Cannot fully exit — state employment, legal documents, and public education require Latin literacy.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, arabic_script_literate_generation, payer,
    organized, biographical, constrained, national).

% Children educated in Latin-script schools during transition. Gain access to European-style education, modern professions, and state bureaucracy. Also serve as literacy bridges for older family members. Exit is easy — they are the intended subjects of the new script; their mobility is the policy's success condition.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, younger_generation, beneficiary,
    moderate, biographical, mobile, national).

% Ulema, medrese teachers, and religious publication networks whose authority rests on Arabic-script textual tradition. Lose control over religious education, fatwa dissemination, and Quranic instruction as state takes over. Cannot exit without abandoning their institutional identity — the script IS their epistemic authority. Some attempt parallel Arabic-script education (temporarily tolerated, then suppressed).
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, islamic_scholarly_establishment, payer,
    organized, generational, identity_locked, national).

% Writers, journalists, academics who publish in Latin script and gain access to European intellectual networks. Benefit from state patronage (Turkish Language Association, publishing subsidies). Their cultural capital rises as Latin script becomes the marker of modernity. Can exit to European academia if domestic politics shift.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, european_aligned_intellectuals, beneficiary,
    moderate, biographical, mobile, national).

% Rural and small-town populations with limited schooling, attached to Arabic script through religious practice (Quran, prayer books) and family archives. Bear transition costs disproportionately — fewer educational resources, deeper identity attachment to Arabic script. Exit is constrained by geography, poverty, and communal pressure. Some resist through clandestine Arabic-script teaching.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, conservative_population, payer,
    moderate, biographical, constrained, national).

% Ottomanists, Turkologists, and sociolinguists who study the transition as a case of engineered script change. No material stake in outcome; their authority comes from methodological rigor. Observe from outside the constraint's enforcement — can access both script archives. Their analysis shapes retrospective classification of the transition.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, linguistic_scholars, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Manages script transition without catastrophic generational knowledge loss: maintains dual-script publishing, education, and legal documentation during a declared sunset period so that the existing literate cohort is not instantly obsolete while the new cohort acquires Latin literacy.
% TRANSFER_FUNCTION: Transfers linguistic authority from the Arabic-script literate establishment (ulema, Ottoman bureaucrats, traditional presses) to the Latin-script state education system and its allied intellectual class. Transfers literacy acquisition costs onto the older generation (relearning or exclusion) and onto conservative communities (erasure of communal textual practices). Transfers cultural capital toward European-aligned modernity.
% ABSENT_VOICES: Kurdish, Armenian, Greek, and Ladino speakers whose communal scripts (Arabic, Armenian, Greek, Hebrew) were not part of the binary Turkish Arabic/Latin framework — their literacy traditions were marginalized by both the Ottoman and Republican monolingualisms. Women in conservative households whose literacy access was mediated by male relatives and Quranic schools — they had no independent voice in the transition design. Rural populations in Arabic-script regions (e.g., Kurdish provinces) where state education arrived late and the transition was experienced as imposed illiteracy.
% DISAPPEARANCE_RATIONALE: If the managed transition policy vanished overnight in 1928, two counterfactuals dominate: (1) immediate Latin-only enforcement (the secular_nationalist_reading's preference) — causing mass functional illiteracy among the 1928 adult population, severing religious and family textual continuity, likely triggering widespread resistance; (2) Arabic script persistence (the ottoman_continuity_reading's preference) — maintaining the existing literate cohort's authority but blocking the state's Europeanization project. The transition policy's existence is what structures the middle ground; its removal forces a binary rupture.
% FOUNDING_PROBLEM: How to modernize Turkish script and align with European scientific modernity without severing the intergenerational transmission of religious, legal, and literary knowledge encoded in the Arabic script — a problem the Ottoman reform attempts (1860s-1910s) failed to solve because they lacked state enforcement capacity and faced entrenched scholarly opposition.
% FOUNDING_PROBLEM_CORROBORATION: State reformers (Atatürk, Turkish Language Association founders) attest the problem was solved by the transition — citing literacy rate increases and European integration. Islamic scholars and conservative intellectuals (e.g., Said Nursi, Necip Fazıl Kısakürek) attest the problem was manufactured — the real rupture was the state's seizure of religious education authority, not script per se. Western orientalists (Lewis, Mardin) corroborate the state's modernization framing but note the transition's coercive character. No neutral arbiter exists; the founding problem's status mirrors the kernel contest itself.
narrative_ontology:disappearance_verdict(turkish_graphemic_substrate__gradual_transition_reading, world_rearranges).
narrative_ontology:founding_problem_status(turkish_graphemic_substrate__gradual_transition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(turkish_graphemic_substrate__gradual_transition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(turkish_graphemic_substrate__gradual_transition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(turkish_graphemic_substrate__gradual_transition_reading, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(turkish_graphemic_substrate__gradual_transition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(turkish_graphemic_substrate__gradual_transition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(turkish_graphemic_substrate__gradual_transition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65 at peak) reflects the state's capture of linguistic authority and the asymmetric cost burden on the Arabic-script literate generation. Suppression (0.55) is moderate — the state did not ban private Arabic script use but monopolized public space (schools, courts, official press). Theater ratio (0.38 peak) captures the performative 'respect for tradition' (bilingual Quran editions, calligraphy preservation) while the functional trajectory was elimination. Accessibility collapse (0.58) reflects the gradual closure of Arabic-script public life — by 1950, a person knowing only Arabic script was functionally excluded from civic participation. Resistance (0.68) is high — the ulema, conservative press, and rural communities resisted through parallel education, petitions, and cultural persistence.
 *
 * PERSPECTIVAL GAP:
 *   From the state_reformers seat, the constraint is genuine coordination (Scaffold) — a difficult transition managed humanely. From the islamic_scholarly_establishment seat, it is a Snare — the transition period is a facade for dismantling their authority. From the arabic_script_literate_generation seat, it is a Tangled Rope — they get some accommodation (private use, family teaching) but the public coordination function extracts their literacy capital. The engine computes this divergence; the authored claim (Scaffold) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   State reformers are structural beneficiaries (d ≈ 0.1) — they control the transition, gain legitimacy, and can accelerate or decelerate. Arabic-script literate generation and Islamic establishment are targets (d ≈ 0.8-0.9) — identity-locked for the establishment, constrained for the generation; they bear costs without commensurate benefits. Younger generation and European-aligned intellectuals are beneficiaries (d ≈ 0.2) — they gain the new script's advantages. Conservative population sits near symmetric but slightly target (d ≈ 0.55) — some gain literacy access but lose communal textual world. The derivation chain produces this gradient from beneficiary/victim declarations plus exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate (manage transition to prevent rupture) was live in 1928-1935 but atrophied as the state shifted from 'managing coexistence' to 'enforcing Latin monopoly.' By 1940, the sunset clause had functionally expired but the dual-script infrastructure was not dismantled — it was repurposed for controlled elimination (e.g., Arabic-script religious publishing only through state-approved channels). The classification prevents mislabeling: calling this pure coordination ignores the extraction; calling it pure extraction ignores the genuine transitional function that protected a generation from instant obsolescence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_relationship,
    'Does the gradual_transition_reading represent a genuine historical policy option, or a retrospective rationalization of what was always a predetermined Latin-script outcome?',
    'Archival analysis of 1928-1935 internal state communications (Atatürk''s directives, Ministry of Education minutes, Turkish Language Association records) — were sunset dates and dual-script provisions treated as genuine commitments or tactical concessions?',
    'If tactical, the constraint is a Snare wearing a Scaffold''s clothing — the transition period was theater for extraction. If genuine, the Scaffold claim holds and the extraction metrics reflect unavoidable transition costs rather than predatory design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_relationship, empirical, 'Whether the managed transition was a real policy option or a predetermined extraction mechanism.').

omega_variable(
    transition_genuineness,
    'Was the 5-15 year transition period a good-faith attempt to preserve intergenerational knowledge, or a pretext for gradual elimination of Arabic script?',
    'Compare the actual duration of dual-script provisions in education, publishing, and law against the declared sunset; measure state investment in Arabic-script materials (Quran editions, religious textbooks, legal commentaries) over time — declining investment indicates pretext.',
    'If pretext, theater_ratio should be higher and the constraint reclassifies toward Snare/Tangled Rope. If good-faith, the Scaffold classification is validated and extraction metrics reflect implementation costs.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transition_genuineness, conceptual, 'Whether the transition''s declared purpose matches its operational trajectory.').

omega_variable(
    cost_distribution_symmetry,
    'Were the transition costs symmetrically distributed across social groups, or concentrated on the Islamic scholarly establishment and conservative populations?',
    'Quantitative analysis of literacy rates, school enrollment, and publication volumes by region, class, and religious affiliation 1928-1950; qualitative analysis of petitions, resistance movements, and state repression records.',
    'Asymmetric cost concentration supports Tangled Rope/Snare classification (coordination for some, extraction for others). Symmetric distribution supports Scaffold (shared transition burden).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_distribution_symmetry, empirical, 'Whether the constraint''s extraction falls disproportionately on identity-locked groups.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (state enforcement of Latin script in schools, courts, press) or internalized (populations adopting Latin script as status marker, self-censoring Arabic script use)?',
    'Post-transition suppression trajectory: if Arabic script use rebounds in private/religious spheres after state enforcement relaxes (post-1950), suppression was primarily structural. If Arabic script remains marginal even without enforcement, internalization occurred.',
    'If internalized, effective suppression is higher than structural measure suggests — the constraint reshaped identity such that the target population polices itself. This would increase χ for identity-locked agents beyond the structural derivation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the script transition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(turkish_graphemic_substrate__gradual_transition_reading, 1928, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tgs_gtr_tr_t1928, turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 1928, 0.2).
narrative_ontology:measurement(tgs_gtr_tr_t1932, turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 1932, 0.28).
narrative_ontology:measurement(tgs_gtr_tr_t1936, turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 1936, 0.35).
narrative_ontology:measurement(tgs_gtr_tr_t1940, turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 1940, 0.42).
narrative_ontology:measurement(tgs_gtr_tr_t1945, turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 1945, 0.38).
narrative_ontology:measurement(tgs_gtr_tr_t1950, turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 1950, 0.3).

% Extraction over time
narrative_ontology:measurement(tgs_gtr_be_t1928, turkish_graphemic_substrate__gradual_transition_reading, base_extractiveness, 1928, 0.45).
narrative_ontology:measurement(tgs_gtr_be_t1932, turkish_graphemic_substrate__gradual_transition_reading, base_extractiveness, 1932, 0.55).
narrative_ontology:measurement(tgs_gtr_be_t1936, turkish_graphemic_substrate__gradual_transition_reading, base_extractiveness, 1936, 0.62).
narrative_ontology:measurement(tgs_gtr_be_t1940, turkish_graphemic_substrate__gradual_transition_reading, base_extractiveness, 1940, 0.68).
narrative_ontology:measurement(tgs_gtr_be_t1945, turkish_graphemic_substrate__gradual_transition_reading, base_extractiveness, 1945, 0.65).
narrative_ontology:measurement(tgs_gtr_be_t1950, turkish_graphemic_substrate__gradual_transition_reading, base_extractiveness, 1950, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(tgs_gtr_su_t1928, turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 1928, 0.4).
narrative_ontology:measurement(tgs_gtr_su_t1932, turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 1932, 0.5).
narrative_ontology:measurement(tgs_gtr_su_t1936, turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 1936, 0.58).
narrative_ontology:measurement(tgs_gtr_su_t1940, turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 1940, 0.62).
narrative_ontology:measurement(tgs_gtr_su_t1945, turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 1945, 0.55).
narrative_ontology:measurement(tgs_gtr_su_t1950, turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 1950, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(turkish_graphemic_substrate__gradual_transition_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(turkish_graphemic_substrate__gradual_transition_reading, 0.1).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__gradual_transition_reading, turkish_graphemic_substrate__ottoman_continuity_reading).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__gradual_transition_reading, turkish_graphemic_substrate__secular_nationalist_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the 'Turkish script question' into three readings of the same kernel (turkish_graphemic_substrate). The gradual_transition_reading claims a managed Scaffold; ottoman_continuity_reading claims the Arabic script is a Mountain (natural substrate of Turkish-Islamic identity); secular_nationalist_reading claims Latin script is a Rope (coordination for European modernity). Their ε values differ: continuity reading ε ≈ 0.1 (negligible extraction for its adherents), nationalist reading ε ≈ 0.3 (coordination cost), this reading ε = 0.65 (substantial extraction during transition). Linked via affects_constraints for contamination analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(turkish_graphemic_substrate__gradual_transition_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
