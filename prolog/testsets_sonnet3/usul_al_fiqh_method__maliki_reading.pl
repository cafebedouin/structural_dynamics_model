% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__maliki_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__maliki_reading, []).

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
 *   constraint_id: usul_al_fiqh_method__maliki_reading
 *   human_readable: Maliki Usul al-Fiqh: Medinan Practice, Maslaha Mursala, and 'Urf as Independent Sources
 *   domain: religious_legal/comparative_law
 *
 * SUMMARY:
 *   This constraint models the Maliki reading of the usul al-fiqh kernel: the
 *   methodological commitment that treats the continuous practice of the
 *   people of Medina ('amal ahl al-Madina) as an independent evidentiary
 *   source alongside hadith, that admits maslaha mursala (unrestricted public
 *   interest reasoning) as a valid derivation tool even where no text speaks,
 *   and that integrates regional custom ('urf) into binding law wherever it
 *   does not contradict explicit text. This is one of four structurally
 *   distinct readings of the same contested kernel — the proper hierarchy and
 *   admissibility of legal sources in Islamic jurisprudence. The Hanafi
 *   reading (expansive qiyas, ra'y, istihsan), the Shafii reading
 *   (hadith-authentication primacy, restricted ijma, systematized
 *   meta-discipline), and the Hanbali reading (maximal textual restriction,
 *   minimized qiyas, sadd al-dhara'i) are separate constraints, each with its
 *   own ε, beneficiaries, and victims — not measurement variants of this one.
 *   Where those siblings ground legitimacy in verified transmission chains or
 *   restrictive textual fidelity, the Maliki reading grounds part of its
 *   legitimacy in living communal continuity and discretionary
 *   public-interest reasoning, which is why its beneficiary/victim structure
 *   diverges sharply from theirs.
 *
 * KEY AGENTS:
 *   - medinan_juristic_tradition: Primary beneficiary (institutional/identity_locked) — the source of the elevated evidentiary claim
 *   - maliki_qadis_and_muftis: Agenda-setters administering maslaha mursala discretion across a continental jurisdiction
 *   - non_medinan_muslim_communities: Primary bearers of the cost of universalizing a regional practice
 *   - textualist_minority_dissenters_within_maliki_lands: Powerless minority whose hadith-based objections are structurally overridden
 *   - comparative_fiqh_scholars: Analytical observers documenting the four-school divergence without adjudicating it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__maliki_reading, 0.42).
domain_priors:suppression_score(usul_al_fiqh_method__maliki_reading, 0.38).
domain_priors:theater_ratio(usul_al_fiqh_method__maliki_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__maliki_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__maliki_reading, "Maliki Usul al-Fiqh: Medinan Practice, Maslaha Mursala, and 'Urf as Independent Sources").
narrative_ontology:topic_domain(usul_al_fiqh_method__maliki_reading, "religious_legal/comparative_law").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__maliki_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__maliki_reading, '08cb35b2-12b5-4886-a563-58d14b77f4f4').
narrative_ontology:cs_kernel_codification('08cb35b2-12b5-4886-a563-58d14b77f4f4', distributed).
narrative_ontology:cs_authority_grounding('08cb35b2-12b5-4886-a563-58d14b77f4f4', lineage).
narrative_ontology:cs_interpretation_layer_present('08cb35b2-12b5-4886-a563-58d14b77f4f4').
narrative_ontology:cs_reading_relation('08cb35b2-12b5-4886-a563-58d14b77f4f4', usul_al_fiqh_method__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('08cb35b2-12b5-4886-a563-58d14b77f4f4', usul_al_fiqh_method__shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('08cb35b2-12b5-4886-a563-58d14b77f4f4', usul_al_fiqh_method__hanbali_reading, influences).
narrative_ontology:cs_axiom('08cb35b2-12b5-4886-a563-58d14b77f4f4', foundational, continuous_communal_practice_as_independent_evidence).
narrative_ontology:cs_axiom_status(continuous_communal_practice_as_independent_evidence, holdable).
narrative_ontology:cs_axiom_grounding('08cb35b2-12b5-4886-a563-58d14b77f4f4', continuous_communal_practice_as_independent_evidence, conventional).
narrative_ontology:cs_axiom('08cb35b2-12b5-4886-a563-58d14b77f4f4', foundational, unrestricted_public_interest_admissible_absent_text).
narrative_ontology:cs_axiom_status(unrestricted_public_interest_admissible_absent_text, holdable).
narrative_ontology:cs_axiom_grounding('08cb35b2-12b5-4886-a563-58d14b77f4f4', unrestricted_public_interest_admissible_absent_text, instrumental).
narrative_ontology:cs_reference_frame('08cb35b2-12b5-4886-a563-58d14b77f4f4', medinan_consensus_as_living_transmission).
narrative_ontology:cs_drift_state('08cb35b2-12b5-4886-a563-58d14b77f4f4', post_classical_codification_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('08cb35b2-12b5-4886-a563-58d14b77f4f4', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, medinan_juristic_tradition).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, local_customary_authorities).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, maliki_qadis_and_muftis).
narrative_ontology:constraint_victim(usul_al_fiqh_method__maliki_reading, non_medinan_muslim_communities).
narrative_ontology:constraint_victim(usul_al_fiqh_method__maliki_reading, textualist_minority_dissenters_within_maliki_lands).
narrative_ontology:constraint_victim(usul_al_fiqh_method__maliki_reading, converts_and_migrants_with_divergent_local_custom).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__maliki_reading, living_transmitted_practice_as_evidentiary_source).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__maliki_reading, unrestricted_public_interest_as_legitimate_derivation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The inherited body of Medinan communal practice is treated as an independent evidentiary source on par with, and sometimes weightier than, individual hadith reports — on the reasoning that the continuous practice of the Prophet's own city could not have deviated from his example. This elevates a specific regional consensus to near-canonical status and gives the Medinan school interpretive priority over jurists elsewhere who rely purely on transmitted hadith chains.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, medinan_juristic_tradition, beneficiary,
    institutional, civilizational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__maliki_reading, medinan_juristic_tradition, agenda_setter).

% Regional custom ('urf) that does not contradict explicit text is folded directly into the legal framework, giving local elites, guild norms, and inherited commercial or marital practices a route into binding law without needing textual derivation. Their authority over local affairs is reinforced rather than displaced by the formal legal system.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, local_customary_authorities, beneficiary,
    moderate, generational, constrained, regional).

% Judges and jurists trained in the Maliki method administer maslaha mursala (unrestricted public interest) as a live discretionary tool, deciding cases where no text speaks directly by weighing perceived communal benefit. They set the boundaries of what counts as legitimate custom or interest, and their rulings carry institutional force across Maliki-governed territories from West Africa to Andalusia to the Hijaz.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, maliki_qadis_and_muftis, agenda_setter,
    institutional, generational, arbitrage, continental).

% Muslims living far from Medina, under Maliki jurisdiction, find their own local practices subordinated to Medinan precedent even when their communities' own inherited custom conflicts with it. They bear the cost of a regional practice being universalized as quasi-textual authority, with limited practical ability to contest a ruling grounded in 'what the people of Medina do.'
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, non_medinan_muslim_communities, payer,
    moderate, generational, trapped, continental).

% Jurists and laypeople within Maliki territories who hold that authenticated hadith alone (or hadith plus narrow qiyas) should govern find their preferred derivations overridden when Medinan practice or maslaha is invoked against a hadith they consider sound. They can appeal to competing schools' scholarship, but within Maliki courts their textualist objections carry little formal weight.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, textualist_minority_dissenters_within_maliki_lands, payer,
    powerless, biographical, trapped, regional).

% New converts or migrants arriving in Maliki-governed regions bring customary practices from elsewhere that may be incompatible with the locally sanctioned 'urf now folded into binding law. Their own custom has no standing unless it happens to match what local qadis already recognize, forcing assimilation to a customary baseline they had no part in establishing.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, converts_and_migrants_with_divergent_local_custom, payer,
    powerless, biographical, constrained, local).

% Historians and jurists studying the four schools comparatively examine how Maliki source-theory diverges from Hanafi qiyas-expansion, Shafii hadith-primacy, and Hanbali textual restriction. They document the tradeoffs but do not adjudicate which school's method should prevail.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, comparative_fiqh_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent method for deriving rulings when explicit text is silent or ambiguous, by treating the continuously transmitted communal practice of Medina (understood as an unbroken chain from the Prophet's own community) and considerations of unrestricted public interest as legitimate, independently weighty sources — allowing law to remain responsive to lived practice and social welfare rather than requiring an explicit textual warrant for every ruling.
% TRANSFER_FUNCTION: Moves interpretive authority from purely text-transmission-based schools (which locate authority in verified individual hadith chains) toward locally rooted communal and juristic authority. In practice this transfers legal deference from distant reporters of Prophetic sayings to living regional practice and the discretionary judgment of Maliki jurists, and moves the burden of legal justification away from strict textual proof-texting.
% ABSENT_VOICES: Muslim communities outside the Hijaz whose own customary practices differ from Medina's are structurally absent from the deliberation that decided Medinan practice deserves near-canonical status — they inherit the ruling without having contributed to the practice being canonized. Hadith specialists who regard weak or unauthenticated Medinan consensus as inferior evidence to a sound individual hadith are also functionally excluded once a Maliki court has ruled.
% DISAPPEARANCE_RATIONALE: Maliki jurists and the regions historically organized under Maliki qada would argue the legal-social fabric of North and West Africa, Andalusia, and parts of the Gulf would substantially rearrange without 'amal ahl al-Madina and maslaha mursala as sources — centuries of rulings on marriage, land tenure, and commercial custom rest on them. Hanbali and Shafii-aligned critics would argue the underlying textual sources (Quran, sound hadith) remain and could ground equivalent or better rulings through qiyas and ijma alone, making the loss a change in method rather than a rearrangement of substantive law. Both positions are genuinely held within the tradition itself.
% FOUNDING_PROBLEM: Early Muslim jurists in Medina faced legal questions the Quran and available hadith reports did not directly resolve, and needed a principled way to derive rulings that remained faithful to Prophetic intent without either freezing law entirely or admitting unconstrained speculation. Treating the continuous practice of the Prophet's own city as evidentiary, and permitting judgment on unrestricted public interest, was proposed as a disciplined middle path.
% FOUNDING_PROBLEM_CORROBORATION: Maliki scholars from Malik ibn Anas onward attest the founding problem remains live wherever text is silent, citing ongoing novel cases (finance, medicine, governance) that require maslaha-based reasoning. Shafii and Hanbali critics, writing from outside the Maliki tradition, attest that the founding problem was substantially addressed by systematized qiyas and rigorous hadith authentication (al-Shafii's own Risala was written partly as a critique of Medinan-practice-as-source), and that reliance on regional practice persists now more as a marker of school identity and jurisdictional authority than as a live methodological necessity. Comparative historians of fiqh corroborate that the debate has continued largely unresolved across centuries rather than being settled by either side.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__maliki_reading, contested).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__maliki_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__maliki_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(usul_al_fiqh_method__maliki_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__maliki_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__maliki_reading_tests).
:- end_tests(usul_al_fiqh_method__maliki_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) rather than high: the coordination function is genuine — courts need a working method when text is silent, and 'urf/maslaha give real, non-arbitrary decision procedures rooted in observable communal practice rather than pure judicial fiat. But it is not negligible, because elevating one region's practice to quasi-canonical status structurally disadvantages every other region's custom and every jurist who prioritizes hadith authentication over communal continuity — that asymmetry is real and requires active judicial enforcement (qadis actually ruling against textualist claims) to persist, which is why requires_active_enforcement is true and this is authored as tangled_rope rather than rope. Suppression (0.38) is moderate: dissenting textualist positions are not criminalized, but within Maliki courts they carry little formal weight, which is a real if soft form of foreclosure. Theater ratio is low-moderate (0.22): maslaha mursala is a live discretionary tool actually used in derivation, not mostly ceremonial, though its use as a marker of school identity (rather than pure legal necessity) has grown over the centuries, which the modest upward drift in theater_ratio reflects. Accessibility collapse (0.48) and resistance (0.55) are mid-range, appropriate to a contested methodological commitment rather than a settled natural fact or a raw extraction mechanism — real competing methods (Hanafi, Shafii, Hanbali) remain fully live alternatives, which keeps collapse well below mountain-level and resistance meaningfully above zero.
 *
 * DIRECTIONALITY LOGIC:
 *   Medinan juristic tradition and local customary authorities sit near the beneficiary end: the constraint's structure grants their practices interpretive priority without them bearing the cost of that priority. Maliki qadis and muftis are agenda-setters with genuine institutional power and mobile/arbitrage exit (they can draw on multiple sources within their own discretion), placing them close to the beneficiary end but with real administrative burden. Non-Medinan communities, textualist dissenters, and converts/migrants are pushed toward the target end: they bear the cost of a regional practice or discretionary interest-balancing overriding their own preferred method or custom, and their exit options range from trapped to constrained — they cannot simply relocate out of Maliki jurisdiction or unilaterally invoke a different school's ruling in a Maliki court.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is authored as contested precisely to prevent this constraint from being mislabeled either as pure ossified extraction (a snare) or as costless natural coordination (a rope). The genealogy shows a genuine founding problem — legal derivation under textual silence — that Maliki jurists say remains live and Shafii/Hanbali critics say was substantively resolved by alternative, more textually disciplined methods centuries ago. Because corroboration comes from both inside and outside the Maliki tradition and the two sides disagree, this is exactly the kind of case where a flat rope or flat snare claim would erase real structure; tangled_rope with a contested founding-problem status keeps the coordination function and the extraction asymmetry both visible without forcing premature resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    medinan_practice_kernel_reading_identity,
    'Is ''amal ahl al-Madina best understood as one legitimate reading among four coequal methodological traditions within a shared kernel, or as the historically prior/most authentic reading from which the other three schools represent later departures?',
    'This is a committer-frame question rather than an empirically resolvable one: it depends on which historiographical account of early Islamic legal development one accepts (Medina-priority narratives vs. multi-regional-origin narratives of fiqh). No single empirical test resolves it; it is intrinsic to which reading a jurist or historian already holds.',
    'If Medinan priority is accepted, the Maliki reading''s elevation of local practice looks like continuity with original transmission rather than a regionally particular innovation, which would lower the effective extractiveness attributed to universalizing it. If treated as one coequal reading among four, the current tangled_rope classification with genuine cross-regional cost asymmetry stands as authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(medinan_practice_kernel_reading_identity, conceptual, 'Whether Medinan practice is the historically privileged reading or one of four coequal kernel readings.').

omega_variable(
    maslaha_mursala_discretion_boundary,
    'Where exactly does maslaha mursala''s discretionary public-interest reasoning stop functioning as principled derivation and start functioning as unconstrained judicial preference that happens to track the interests of whoever administers it?',
    'Comparative study of historical maslaha-based rulings against outcomes for the parties involved, checking whether rulings systematically favored locally powerful custom-holders (merchants, landholders, established communities) over outsiders, converts, and minorities across a large sample of Maliki court records.',
    'If rulings systematically favor locally entrenched interests, the tangled_rope classification understates extraction and the constraint drifts toward snare in the specific domain of maslaha-based adjudication. If rulings show no such systematic bias, the current moderate extractiveness score is well-calibrated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maslaha_mursala_discretion_boundary, empirical, 'Whether maslaha mursala discretion is principled or systematically favors locally powerful custom-holders.').

omega_variable(
    urf_versus_universal_text_priority,
    'Does integrating non-contradicting ''urf into binding law genuinely preserve textual supremacy (custom only fills genuine textual silence) or does it function to quietly displace textual universalism by defining ''contradiction'' narrowly enough that most local custom passes?',
    'Analysis of how Maliki jurisprudence historically defines ''contradicts text'' — whether the bar is set high (custom rarely excluded) or low (custom frequently excluded) — compared against the Shafii and Hanbali readings'' narrower admission of custom.',
    'A permissive contradiction bar would mean ''urf functions as a significant erosion of textual universalism in practice, strengthening the case that non-Medinan/non-local communities are structural victims of the reading rather than incidental ones.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(urf_versus_universal_text_priority, empirical, 'How permissively Maliki method defines textual contradiction when admitting custom as a source.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__maliki_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t0, usul_al_fiqh_method__maliki_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(usul_tr_t0, projected).
narrative_ontology:measurement(usul_tr_t200, usul_al_fiqh_method__maliki_reading, theater_ratio, 200, 0.13).
narrative_ontology:measurement_basis(usul_tr_t200, projected).
narrative_ontology:measurement(usul_tr_t400, usul_al_fiqh_method__maliki_reading, theater_ratio, 400, 0.16).
narrative_ontology:measurement_basis(usul_tr_t400, projected).
narrative_ontology:measurement(usul_tr_t600, usul_al_fiqh_method__maliki_reading, theater_ratio, 600, 0.18).
narrative_ontology:measurement_basis(usul_tr_t600, projected).
narrative_ontology:measurement(usul_tr_t900, usul_al_fiqh_method__maliki_reading, theater_ratio, 900, 0.2).
narrative_ontology:measurement_basis(usul_tr_t900, projected).
narrative_ontology:measurement(usul_tr_t1200, usul_al_fiqh_method__maliki_reading, theater_ratio, 1200, 0.22).
narrative_ontology:measurement_basis(usul_tr_t1200, projected).

% Extraction over time
narrative_ontology:measurement(usul_be_t0, usul_al_fiqh_method__maliki_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(usul_be_t0, projected).
narrative_ontology:measurement(usul_be_t200, usul_al_fiqh_method__maliki_reading, base_extractiveness, 200, 0.32).
narrative_ontology:measurement_basis(usul_be_t200, projected).
narrative_ontology:measurement(usul_be_t400, usul_al_fiqh_method__maliki_reading, base_extractiveness, 400, 0.36).
narrative_ontology:measurement_basis(usul_be_t400, projected).
narrative_ontology:measurement(usul_be_t600, usul_al_fiqh_method__maliki_reading, base_extractiveness, 600, 0.39).
narrative_ontology:measurement_basis(usul_be_t600, projected).
narrative_ontology:measurement(usul_be_t900, usul_al_fiqh_method__maliki_reading, base_extractiveness, 900, 0.41).
narrative_ontology:measurement_basis(usul_be_t900, projected).
narrative_ontology:measurement(usul_be_t1200, usul_al_fiqh_method__maliki_reading, base_extractiveness, 1200, 0.42).
narrative_ontology:measurement_basis(usul_be_t1200, projected).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t0, usul_al_fiqh_method__maliki_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement_basis(usul_su_t0, projected).
narrative_ontology:measurement(usul_su_t200, usul_al_fiqh_method__maliki_reading, suppression_requirement, 200, 0.26).
narrative_ontology:measurement_basis(usul_su_t200, projected).
narrative_ontology:measurement(usul_su_t400, usul_al_fiqh_method__maliki_reading, suppression_requirement, 400, 0.3).
narrative_ontology:measurement_basis(usul_su_t400, projected).
narrative_ontology:measurement(usul_su_t600, usul_al_fiqh_method__maliki_reading, suppression_requirement, 600, 0.33).
narrative_ontology:measurement_basis(usul_su_t600, projected).
narrative_ontology:measurement(usul_su_t900, usul_al_fiqh_method__maliki_reading, suppression_requirement, 900, 0.36).
narrative_ontology:measurement_basis(usul_su_t900, projected).
narrative_ontology:measurement(usul_su_t1200, usul_al_fiqh_method__maliki_reading, suppression_requirement, 1200, 0.38).
narrative_ontology:measurement_basis(usul_su_t1200, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__maliki_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(usul_al_fiqh_method__maliki_reading, 0.12).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method__hanafi_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method__shafii_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method__hanbali_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four sibling stories decomposing the single natural-language label 'usul al-fiqh method' into structurally distinct kernel readings, per the ε-invariance principle. Each reading (Hanafi, Shafii, Maliki, Hanbali) carries its own ε, beneficiary/victim structure, and classification, reflecting genuinely different commitments about legitimate legal sources. All four are linked via affects_constraints because a shift in one school's practical dominance (e.g., through state adoption or colonial-era codification) structurally pressures the operating space and legitimacy claims of the others — a legal-pluralist coupling rather than a shared ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
