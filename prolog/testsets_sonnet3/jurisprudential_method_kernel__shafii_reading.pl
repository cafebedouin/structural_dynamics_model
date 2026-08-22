% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__shafii_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__shafii_reading, []).

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
 *   constraint_id: jurisprudential_method_kernel__shafii_reading
 *   human_readable: Shafi'i Methodological Hierarchy (Usul al-Fiqh) — Hadith-Transmission Arbitration
 *   domain: religious/legal/institutional
 *
 * SUMMARY:
 *   This story instantiates the Shafi'i reading of the jurisprudential method
 *   kernel: al-Shafi'i's Risala fixes a strict four-tier source hierarchy
 *   (Qur'an, Hadith, Ijma, Qiyas) and, critically, makes authenticated hadith
 *   transmission — rather than regional custom, generalist analogy, or
 *   juristic discretion — the primary post-Qur'anic arbiter. The reading
 *   presents itself as resolving 'inconsistencies' among earlier schools, but
 *   that framing is itself a partisan move within the contest: Hanafi jurists
 *   did not experience istihsan as incoherent, and Maliki jurists did not
 *   experience Medinan communal practice as unreliable. Those are separate
 *   constraints (hanafi_reading, maliki_reading, hanbali_reading) with their
 *   own ε values and beneficiary/victim structures — this story covers ONLY
 *   the Shafi'i reading's operation, assessed by its own lights, on the
 *   standing arrangement it describes (the pre-standardization methodological
 *   plurality it displaces).
 *
 * KEY AGENTS:
 *   - hadith_transmission_scholars: primary beneficiary (institutional/arbitrage) — gatekeeping authority over what counts as valid legal evidence
 *   - shafii_school_jurists: agenda-setters (institutional/arbitrage) — administer and transmit the method
 *   - medinan_customary_practitioners: primary target (moderate/constrained) — lose independent evidentiary status for communal practice
 *   - independent_qiyas_jurists: primary target (moderate/constrained) — lose interpretive latitude to strict textual priority
 *   - muslim_laypeople: diffuse beneficiary/payer (powerless/constrained) — gain predictability, lose locally-fit customary solutions
 *   - hanafi_and_maliki_school_adherents: excluded from the framing (organized/constrained) — cast as the 'inconsistency' being resolved
 *   - later_usul_al_fiqh_scholars: analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__shafii_reading, 0.58).
domain_priors:suppression_score(jurisprudential_method_kernel__shafii_reading, 0.52).
domain_priors:theater_ratio(jurisprudential_method_kernel__shafii_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__shafii_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__shafii_reading, "Shafi'i Methodological Hierarchy (Usul al-Fiqh) — Hadith-Transmission Arbitration").
narrative_ontology:topic_domain(jurisprudential_method_kernel__shafii_reading, "religious/legal/institutional").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__shafii_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__shafii_reading, '8960c294-cafb-48a4-ad44-f181a34fb0db').
narrative_ontology:cs_kernel_codification('8960c294-cafb-48a4-ad44-f181a34fb0db', formalized).
narrative_ontology:cs_authority_grounding('8960c294-cafb-48a4-ad44-f181a34fb0db', lineage).
narrative_ontology:cs_interpretation_layer_present('8960c294-cafb-48a4-ad44-f181a34fb0db').
narrative_ontology:cs_reading_relation('8960c294-cafb-48a4-ad44-f181a34fb0db', jurisprudential_method_kernel__hanafi_reading, influences).
narrative_ontology:cs_reading_relation('8960c294-cafb-48a4-ad44-f181a34fb0db', jurisprudential_method_kernel__maliki_reading, influences).
narrative_ontology:cs_reading_relation('8960c294-cafb-48a4-ad44-f181a34fb0db', jurisprudential_method_kernel__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('8960c294-cafb-48a4-ad44-f181a34fb0db', foundational, hadith_transmission_is_decisive_arbiter).
narrative_ontology:cs_axiom_status(hadith_transmission_is_decisive_arbiter, holdable).
narrative_ontology:cs_axiom_grounding('8960c294-cafb-48a4-ad44-f181a34fb0db', hadith_transmission_is_decisive_arbiter, conventional).
narrative_ontology:cs_axiom('8960c294-cafb-48a4-ad44-f181a34fb0db', secondary, customary_practice_lacks_independent_evidentiary_status).
narrative_ontology:cs_axiom_status(customary_practice_lacks_independent_evidentiary_status, holdable).
narrative_ontology:cs_axiom_grounding('8960c294-cafb-48a4-ad44-f181a34fb0db', customary_practice_lacks_independent_evidentiary_status, conventional).
narrative_ontology:cs_reference_frame('8960c294-cafb-48a4-ad44-f181a34fb0db', risala_methodological_synthesis).
narrative_ontology:cs_drift_state('8960c294-cafb-48a4-ad44-f181a34fb0db', contemporary_islamic_legal_scholarship, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8960c294-cafb-48a4-ad44-f181a34fb0db', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__shafii_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__shafii_reading, hadith_transmission_scholars).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__shafii_reading, isnad_critics).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__shafii_reading, shafii_school_jurists).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__shafii_reading, medinan_customary_practitioners).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__shafii_reading, independent_qiyas_jurists).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__shafii_reading, regional_juristic_preference_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__shafii_reading, muslim_laypeople).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__shafii_reading, muslim_laypeople).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__shafii_reading, hadith_authentication_as_final_arbiter).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__shafii_reading, methodological_uniformity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Occupy the newly decisive tier of the hierarchy: their expertise in isnad (chain of transmission) criticism becomes the gatekeeping function through which any competing legal claim must pass. Their authority is structurally elevated by al-Shafi'i's standardization — a school of law now rises or falls on whether its rulings survive hadith-authentication scrutiny administered by this class.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, hadith_transmission_scholars, beneficiary,
    institutional, civilizational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__shafii_reading, hadith_transmission_scholars, agenda_setter).

% Administer and teach the methodology itself — the Risala's four-tier ordering and its rules for resolving conflicting hadith. They train qadis and muftis in the method, adjudicate what counts as valid qiyas (restricted to narrow analogical extension bounded by authenticated hadith, not the broader istihsan tradition), and benefit from the prestige of having supplied the field's foundational procedural text.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, shafii_school_jurists, agenda_setter,
    institutional, civilizational, arbitrage, continental).

% Held that the living practice of the Medinan community ('amal ahl al-Madina) transmitted the Prophet's example more reliably than isolated hadith chains vulnerable to transmission error or fabrication. Under the Shafi'i hierarchy, this communal-practice evidence is demoted below individually authenticated hadith reports, stripping it of independent evidentiary status regardless of how continuously it was observed.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, medinan_customary_practitioners, payer,
    moderate, generational, constrained, regional).

% Practiced broad analogical reasoning and juristic discretion (istihsan) to extend legal principles to novel cases based on underlying rationale rather than narrow textual parallel. The Shafi'i method subordinates their reasoning to a strict textual hierarchy, permitting qiyas only as a last-resort, tightly bounded tool — their interpretive latitude is the thing the standardization exists to constrain.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, independent_qiyas_jurists, payer,
    moderate, generational, constrained, continental).

% Gain a more predictable, less regionally fragmented legal system — a ruling issued under Shafi'i method in one province follows a recognizable procedure elsewhere. They also bear costs where locally adapted customary solutions (Medinan practice, regional istihsan) that fit their circumstances are displaced by rulings hinging on hadith chains debated among specialists far from their daily life.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, muslim_laypeople, beneficiary,
    powerless, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__shafii_reading, muslim_laypeople, payer).

% Represent rival methodological commitments (broad qiyas/istihsan for Hanafis, living Medinan practice for Malikis) that predate al-Shafi'i's synthesis. They are not consulted as co-authors of the four-tier hierarchy; the standardization is presented as a resolution of their 'inconsistencies' rather than as one contested position among several, though their schools persist and contest the ordering.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, hanafi_and_maliki_school_adherents, excluded,
    organized, civilizational, constrained, continental).

% Study the historical formation of legal theory across schools, tracing how al-Shafi'i's Risala reshaped the field's terms of debate even for schools that did not adopt his ordering outright. They document the institutional consequences without being bound by any single school's method.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, later_usul_al_fiqh_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, teachable, cross-regional procedure for resolving disputes about what the law requires when sources conflict — before this standardization, competing regional schools produced divergent rulings from ad hoc method, undermining predictability and mutual recognition of legal outcomes across the early Islamic world.
% TRANSFER_FUNCTION: Moves interpretive authority from regionally rooted customary transmission and generalist analogical reasoning toward a specialist class of hadith critics and the jurists trained in isnad evaluation; moves legitimacy away from 'amal ahl al-Madina and istihsan as independent sources and concentrates it in textual chain-authentication expertise.
% ABSENT_VOICES: Hanafi and Maliki jurists whose methods are recast as 'inconsistencies' the new hierarchy resolves are not co-authors of the resolution; Companions' descendants and Medinan community elders whose lived transmission grounded Maliki practice have no seat in an isnad-centered adjudication process that treats their communal continuity as unverifiable relative to chain-documented hadith.
% DISAPPEARANCE_RATIONALE: If the Shafi'i four-tier hierarchy vanished as a governing method, jurists would revert to (or elevate) competing methodologies already alive in the tradition — broader qiyas/istihsan, Medinan communal practice, or Hanbali literalism — and rulings currently defended by appeal to authenticated-hadith priority would lose their principal justification, reopening questions the standardization was built to close.
% FOUNDING_PROBLEM: Early Islamic legal schools reached divergent, sometimes contradictory rulings using inconsistent and unstated methodological principles — some relying on regional custom, some on loose analogy, some on individual scholarly discretion — producing a legitimacy crisis about which rulings actually reflected divine intent.
% FOUNDING_PROBLEM_CORROBORATION: Shafi'i jurists and hadith scholars attest the founding problem (methodological chaos) was real and remains live wherever unstandardized reasoning persists. Historians of Islamic legal thought, including scholars outside any single school's lineage, corroborate that pre-Shafi'i method was genuinely heterogeneous, but many also document that Hanafi, Maliki, and Hanbali jurists did not experience their own methods as incoherent — the 'inconsistency' framing is itself a product of the Shafi'i synthesis's own rhetorical positioning, not a neutral external diagnosis.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__shafii_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__shafii_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__shafii_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jurisprudential_method_kernel__shafii_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__shafii_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__shafii_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisprudential_method_kernel__shafii_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jurisprudential_method_kernel__shafii_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   ε (0.58) reflects medium-high extraction specifically on the hadith-authentication axis: once isnad criticism becomes decisive, control over what counts as an authenticated hadith becomes control over legal outcomes, and that control concentrates in a specialist class rather than remaining distributed across regional custom-bearers or generalist jurists. Suppression (0.52) is moderate — the standardization does not physically coerce adherence, but it does structurally demote rival evidentiary categories (communal practice, broad istihsan) to non-independent status, which is a real narrowing of what counts as a legitimate legal argument. Theater ratio is low-moderate (0.28) because the coordination function is substantially genuine: cross-regional legal predictability was a real problem, and the four-tier method is a real, teachable answer to it, not mere performance. accessibility_collapse (0.62) is meaningfully high because once the hierarchy is institutionally adopted and taught, alternative methodologies become progressively harder to argue for within Shafi'i-trained institutions, even though they remain alive in rival schools — the collapse is local to the reading's own institutional space, not global to the whole legal tradition.
 *
 * PERSPECTIVAL GAP:
 *   From the Shafi'i jurist seat, this is straightforwardly coordination: a principled, teachable resolution to a real methodological crisis. From the Medinan-practice or istihsan-jurist seat, the same instrument reads as the imposition of one regional/methodological tradition's priorities over others under the cover of neutral procedural resolution. The engine computes both seats' types from the same structural data; the divergence is expected and is not evidence of authoring error.
 *
 * DIRECTIONALITY LOGIC:
 *   Hadith transmission scholars and Shafi'i jurists sit near the beneficiary end: the standardization is their methodological export, and their specialist skill becomes structurally load-bearing for the whole system. Medinan customary practitioners and independent qiyas jurists sit near the target end: their prior sources of authority are demoted from independent status to subordinate or excluded status by the same instrument that claims to be merely resolving inconsistency. Muslim laypeople are genuinely mixed — real coordination benefit from predictability, real cost from loss of locally responsive customary rulings — hence a directionality near symmetric rather than at either pole.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (methodological incoherence across early schools) is genuinely contested as to whether it is dead: Shafi'i-tradition jurists hold it live wherever unstandardized method persists; historians and adherents of rival schools note that the 'incoherence' diagnosis was itself part of the Shafi'i school's rhetorical self-positioning rather than a problem those other schools recognized in themselves. This keeps founding_problem_status as contested rather than dead or live — the classification should not treat the Shafi'i self-narrative as the final word on its own necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inconsistency_framing_is_partisan,
    'Is the premise that earlier schools'' methods were genuinely ''inconsistent'' (requiring Shafi''i resolution) a neutral historical observation, or is it itself a rhetorical move internal to the Shafi''i school''s bid for methodological authority?',
    'Comparative analysis of Hanafi and Maliki jurists'' own self-understanding of their methods prior to and independent of Shafi''i critique — do they describe their pre-existing practice as incoherent, or only as different?',
    'If the inconsistency framing is substantially rhetorical, the coordination-function claim underlying this reading''s tangled_rope classification weakens, and the extraction component (elevating hadith-transmission scholars) becomes proportionally more central to the constraint''s actual operation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inconsistency_framing_is_partisan, conceptual, 'Whether ''resolving inconsistency'' is a real coordination problem or a partisan framing device.').

omega_variable(
    isnad_criticism_epistemic_reliability,
    'Does isnad (chain-of-transmission) criticism actually produce more reliable determinations of prophetic intent than communal practice transmission or generalist analogical reasoning, or does it merely relocate the locus of uncertainty to a different, less visible layer (the biographical evaluation of transmitters)?',
    'Historical and methodological analysis of documented cases where isnad-authenticated hadith conflicted with strong communal practice or with each other, examining how such conflicts were actually resolved in practice.',
    'If isnad criticism is not more reliable than the sources it displaces, the elevation of hadith-transmission scholars functions substantially as extraction (specialist gatekeeping without a corresponding reliability gain) rather than as a genuine epistemic improvement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(isnad_criticism_epistemic_reliability, empirical, 'Whether hadith-transmission-based arbitration is epistemically superior to the sources it subordinates.').

omega_variable(
    kernel_framing_underdetermination,
    'Should the kernel here be framed as ''the correct method for deriving Islamic law'' (a first-order legal-theory question) or as ''which specialist class holds interpretive authority over Islamic law'' (a second-order institutional-power question)? The Shafi''i reading as authored takes the first framing; a second framing focused on interpretive authority would likely produce a starker beneficiary/victim asymmetry and a higher ε.',
    'N/A — this is a framing choice, not an empirically resolvable question; document which framing was selected and why.',
    'Under the first-order framing (adopted here), this reads as tangled_rope: genuine coordination value plus real extraction. Under a second-order institutional-power framing, the coordination function would recede further into cover-story status and the reading would sit closer to snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Alternative framing of the kernel as legal-theory question versus institutional-authority question, and the classification shift this would produce.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__shafii_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t0, jurisprudential_method_kernel__shafii_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(juri_tr_t20, jurisprudential_method_kernel__shafii_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(juri_tr_t40, jurisprudential_method_kernel__shafii_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement(juri_tr_t60, jurisprudential_method_kernel__shafii_reading, theater_ratio, 60, 0.21).
narrative_ontology:measurement(juri_tr_t80, jurisprudential_method_kernel__shafii_reading, theater_ratio, 80, 0.24).
narrative_ontology:measurement(juri_tr_t100, jurisprudential_method_kernel__shafii_reading, theater_ratio, 100, 0.26).
narrative_ontology:measurement(juri_tr_t120, jurisprudential_method_kernel__shafii_reading, theater_ratio, 120, 0.28).

% Extraction over time
narrative_ontology:measurement(juri_be_t0, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(juri_be_t20, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 20, 0.47).
narrative_ontology:measurement(juri_be_t40, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 40, 0.51).
narrative_ontology:measurement(juri_be_t60, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 60, 0.54).
narrative_ontology:measurement(juri_be_t80, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 80, 0.56).
narrative_ontology:measurement(juri_be_t100, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 100, 0.57).
narrative_ontology:measurement(juri_be_t120, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 120, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t0, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(juri_su_t20, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(juri_su_t40, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 40, 0.45).
narrative_ontology:measurement(juri_su_t60, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 60, 0.48).
narrative_ontology:measurement(juri_su_t80, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 80, 0.5).
narrative_ontology:measurement(juri_su_t100, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 100, 0.51).
narrative_ontology:measurement(juri_su_t120, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 120, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__shafii_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__shafii_reading, hanafi_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__shafii_reading, maliki_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__shafii_reading, hanbali_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of jurisprudential_method_kernel, each authored as an independent, ε-invariant constraint per DP-001. The shafii_reading elevates hadith-isnad authentication as the decisive arbiter above the sources the other readings treat as independently valid (Hanafi's broad qiyas/istihsan, Maliki's 'amal ahl al-Madina, Hanbali's Companion-literalism plus unanimous ijma). Each sibling has its own ε, beneficiary/victim structure, and claimed_type; none is derived from or averages the others. Network edges here record structural influence (the Shafi'i synthesis reshaped the terms even schools that rejected it had to respond to), not shared classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
