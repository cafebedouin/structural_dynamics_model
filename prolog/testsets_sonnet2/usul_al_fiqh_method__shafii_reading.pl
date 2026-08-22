% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__shafii_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__shafii_reading, []).

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
 *   constraint_id: usul_al_fiqh_method__shafii_reading
 *   human_readable: Shafi'i Usul al-Fiqh: Hadith-Priority Source Hierarchy
 *   domain: religious_legal/epistemic_authority
 *
 * SUMMARY:
 *   This constraint models the Shafi'i reading of the usul al-fiqh kernel:
 *   the systematization (credited to al-Shafi'i's Risala) that makes hadith
 *   authentication a gatekeeping prerequisite to legal derivation, restricts
 *   qiyas to cases where no authenticated hadith exists, restricts binding
 *   ijma to the Companions' generation, and installs usul al-fiqh itself as a
 *   meta-discipline governing the ranking of all other sources. This is one
 *   of four sibling readings of the same underlying kernel (the proper
 *   structure and hierarchy of Islamic legal sources); the Hanafi, Maliki,
 *   and Hanbali readings are separate constraints with their own ε values,
 *   not alternative measurements of this one. The genuine coordination
 *   problem — replacing chaotic, inconsistent regional legal reasoning with a
 *   shared, teachable procedure — is real and is why this is authored as
 *   tangled_rope rather than snare: a coordination function exists
 *   (beneficiaries: hadith specialists and the school's jurists) alongside
 *   asymmetric extraction (victims: rationalist and custom-based jurists
 *   whose evidentiary methods are subordinated or delegitimized by the same
 *   hierarchy).
 *
 * KEY AGENTS:
 *   - hadith_transmission_specialists: primary beneficiary (institutional/arbitrage) — become structurally indispensable gatekeepers
 *   - shafii_school_jurists: agenda_setter (institutional/identity_locked) — administer and are professionally fused to the hierarchy
 *   - rationalist_jurists: primary target (moderate/constrained) — their method is subordinated whenever hadith exists
 *   - regional_custom_based_practitioners: primary target (powerless/trapped) — their evidentiary basis has no standing under this hierarchy
 *   - later_companions_generation_claimants: excluded — restricted ijma forecloses their argument by definition
 *   - comparative_legal_historians: analytical observer — sees the cross-school structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__shafii_reading, 0.42).
domain_priors:suppression_score(usul_al_fiqh_method__shafii_reading, 0.38).
domain_priors:theater_ratio(usul_al_fiqh_method__shafii_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__shafii_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__shafii_reading, "Shafi'i Usul al-Fiqh: Hadith-Priority Source Hierarchy").
narrative_ontology:topic_domain(usul_al_fiqh_method__shafii_reading, "religious_legal/epistemic_authority").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__shafii_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__shafii_reading, '4bd5cf86-fced-41c4-b9db-fef8d89bbf25').
narrative_ontology:cs_kernel_codification('4bd5cf86-fced-41c4-b9db-fef8d89bbf25', formalized).
narrative_ontology:cs_authority_grounding('4bd5cf86-fced-41c4-b9db-fef8d89bbf25', lineage).
narrative_ontology:cs_interpretation_layer_present('4bd5cf86-fced-41c4-b9db-fef8d89bbf25').
narrative_ontology:cs_reading_relation('4bd5cf86-fced-41c4-b9db-fef8d89bbf25', usul_al_fiqh_method__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('4bd5cf86-fced-41c4-b9db-fef8d89bbf25', usul_al_fiqh_method__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('4bd5cf86-fced-41c4-b9db-fef8d89bbf25', usul_al_fiqh_method__hanbali_reading, influences).
narrative_ontology:cs_axiom('4bd5cf86-fced-41c4-b9db-fef8d89bbf25', foundational, authenticated_hadith_precedes_qiyas_categorically).
narrative_ontology:cs_axiom_status(authenticated_hadith_precedes_qiyas_categorically, holdable).
narrative_ontology:cs_axiom_grounding('4bd5cf86-fced-41c4-b9db-fef8d89bbf25', authenticated_hadith_precedes_qiyas_categorically, conventional).
narrative_ontology:cs_axiom('4bd5cf86-fced-41c4-b9db-fef8d89bbf25', foundational, binding_ijma_restricted_to_companions_generation).
narrative_ontology:cs_axiom_status(binding_ijma_restricted_to_companions_generation, holdable).
narrative_ontology:cs_axiom_grounding('4bd5cf86-fced-41c4-b9db-fef8d89bbf25', binding_ijma_restricted_to_companions_generation, conventional).
narrative_ontology:cs_reference_frame('4bd5cf86-fced-41c4-b9db-fef8d89bbf25', shafii_risala_systematization).
narrative_ontology:cs_drift_state('4bd5cf86-fced-41c4-b9db-fef8d89bbf25', post_classical_consolidation, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('4bd5cf86-fced-41c4-b9db-fef8d89bbf25', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__shafii_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__shafii_reading, hadith_transmission_specialists).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__shafii_reading, shafii_school_jurists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__shafii_reading, rationalist_jurists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__shafii_reading, regional_custom_based_practitioners).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__shafii_reading, textual_authentication_precedes_derivation).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__shafii_reading, companions_era_consensus_uniquely_binding).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Muhaddithun who develop and apply isnad-criticism (chain authentication) and matn evaluation. Under the Shafi'i hierarchy their technical verdicts on hadith authenticity become the gatekeeping step that must be cleared before any other source can be consulted; their scholarly output becomes structurally indispensable to legal derivation, and their institutional prestige and patronage flow from this indispensability.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, hadith_transmission_specialists, beneficiary,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__shafii_reading, hadith_transmission_specialists, agenda_setter).

% Jurists who systematized usul al-fiqh as a governing meta-discipline (following al-Shafi'i's Risala). They administer the source hierarchy itself, ruling on when qiyas is permissible (only after exhausting authenticated hadith) and what counts as valid ijma. Their professional identity and school affiliation are constituted by defending this hierarchy; abandoning it would dissolve the school's distinct claim to method.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, shafii_school_jurists, agenda_setter,
    institutional, civilizational, identity_locked, regional).

% Jurists (paradigmatically associated with Hanafi/Iraqi method) whose legal reasoning relies on expansive qiyas and ra'y even where hadith of contested strength exists. Under this reading, their rulings are subordinated or dismissed whenever any authenticated hadith can be produced against them, regardless of the hadith's practical fit to the case. Their exit is constrained: they can migrate to a rival school's jurisdiction but cannot alter the hierarchy from within Shafi'i institutions.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, rationalist_jurists, payer,
    moderate, biographical, constrained, regional).

% Jurists and communities (paradigmatically Medinan-practice adherents) whose evidentiary source is lived communal practice or unrestricted public interest reasoning. Under the Shafi'i hierarchy this evidentiary basis has no formal standing at all unless it can be re-derived from authenticated hadith or narrow analogy; their local practice-based rulings are structurally delegitimized wherever they diverge from a hadith the muhaddithun accept.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, regional_custom_based_practitioners, payer,
    powerless, biographical, trapped, local).

% Scholars who would argue that binding consensus (ijma) should extend beyond the Companions to later generations of recognized jurists (a broader ijma claim held in some other schools). This reading restricts ijma strictly to Companions' consensus, which forecloses their argument by definition; they have no seat in the hierarchy's own adjudication process.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, later_companions_generation_claimants, excluded,
    powerless, generational, trapped, regional).

% Scholars of comparative Islamic law who trace how the Shafi'i systematization of usul al-fiqh reshaped source hierarchies across the other schools, and who can compare its structural effects against the Hanafi, Maliki, and Hanbali readings of the same underlying kernel.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, comparative_legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(usul_al_fiqh_method__shafii_reading, hadith_transmission_specialists).
narrative_ontology:fixing_cost_class(usul_al_fiqh_method__shafii_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, teachable, rank-ordered procedure (Quran, then authenticated Sunna, then Companions' ijma, then qiyas only in their absence) for deriving law, which reduces jurisprudential chaos, allows disputes to be adjudicated by reference to a shared method rather than ad hoc reasoning, and gives students of law a portable discipline (usul al-fiqh) transferable across substantive legal questions.
% TRANSFER_FUNCTION: Moves interpretive authority and the associated social and institutional capital (teaching posts, judicial appointments, patronage) from jurists whose method rests on regional custom or expansive analogical/rational reasoning toward specialists in hadith authentication and the jurists who administer the source-ranking procedure built on their findings.
% ABSENT_VOICES: Rationalist jurists and custom-based practitioners are present as historical interlocutors in the broader tradition but are structurally excluded from adjudicating this reading's own hierarchy — the hierarchy's authority to rank sources is precisely what their objection would need to challenge, and the ranking procedure does not grant them standing to challenge it from within. Later-generation ijma claimants have no seat at all once ijma is restricted to Companions.
% DISAPPEARANCE_RATIONALE: If the Shafi'i source-hierarchy discipline vanished, hadith authentication would lose its gatekeeping monopoly over legal derivation; qiyas and regional practice-based reasoning could be invoked without first clearing a hadith-absence threshold; the professional indispensability of hadith-verification specialists to legal outcomes would sharply diminish, and school identity built around 'usul al-fiqh as meta-discipline' would need a new organizing principle.
% FOUNDING_PROBLEM: Early Islamic legal reasoning (2nd century AH) had become fragmented across regional schools using inconsistent mixtures of local custom, individual juristic opinion (ra'y), and scattered hadith citation, with no shared procedure for weighing sources against each other or resolving disagreements about which source should control a given case.
% FOUNDING_PROBLEM_CORROBORATION: Shafi'i jurists themselves attest the problem (methodological chaos, arbitrary ra'y) as their Risala's founding justification. Independent corroboration exists in the historical record of contemporaneous critiques from Hanafi and Maliki jurists conceding that some systematization was needed, though they dispute that hadith-priority was the correct solution; modern comparative legal historians (outside all four schools) corroborate that pre-systematic legal reasoning was genuinely heterogeneous, while also documenting that the specific hadith-priority ranking simultaneously consolidated power for a particular scholarly guild — so the founding problem is real but its resolution is not neutral among the schools it displaced.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__shafii_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__shafii_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__shafii_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(usul_al_fiqh_method__shafii_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__shafii_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__shafii_reading_tests).
:- end_tests(usul_al_fiqh_method__shafii_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) rather than high because the hierarchy genuinely solves a real coordination problem — a shared, portable, teachable method for legal derivation across a fragmented tradition — and its 'extraction' operates through prestige and interpretive authority rather than direct material rent. Suppression is likewise moderate (0.38): the hierarchy does not physically coerce rationalist or custom-based jurists, but it does structurally delegitimize their evidentiary claims within any institution that adopts the Shafi'i method, and this delegitimization has hardened over the historical record as the discipline became canonical (reflected in the rising suppression_requirement series). Theater ratio stays low-to-moderate (0.22) because the authentication and ranking apparatus performs genuine analytical work (isnad criticism is not mere theater) even as some proportion of its activity has become guild-boundary maintenance over centuries.
 *
 * DIRECTIONALITY LOGIC:
 *   Hadith transmission specialists sit near the beneficiary end: the hierarchy makes their technical output a load-bearing, indispensable input to all downstream legal reasoning, and their institutional standing derives directly from this dependency. Shafi'i school jurists administer the hierarchy and are identity-locked to it — the school's entire methodological identity is constituted by hadith-priority, so abandoning the hierarchy would dissolve the school's distinguishing claim, which is a stronger bind than ordinary institutional interest. Rationalist jurists and custom-based practitioners sit near the target end: their reasoning methods are subordinated by the same structure that elevates hadith authentication, and their exit options are constrained (they can practice under a rival school's jurisdiction) or trapped (local custom has no portable alternative venue) respectively.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — fragmented, inconsistent early legal reasoning — is genuinely contested as live or dead: hadith corpora are now largely stabilized and catalogued (arguably reducing the ongoing need for the hierarchy's authentication-gatekeeping function), yet many practitioners and the Shafi'i tradition itself maintain the hierarchy is still necessary because new legal questions continually require the same disciplined source-ranking. Because the founding problem is genuinely contested rather than simply dead, this does not resolve to pure inertial performance (piton); the tangled_rope classification preserves the coordination function's continuing partial validity while still registering the asymmetric extraction directed at rationalist and custom-based methods.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gatekeeping_vs_epistemic_necessity,
    'Is hadith-authentication-as-prerequisite a genuinely necessary epistemic safeguard against fabricated or unreliable legal sources, or is it a gatekeeping mechanism that happens to concentrate authority in the hands of hadith specialists regardless of its epistemic merit?',
    'Comparative analysis of legal outcomes across schools that weight hadith authentication differently (Hanafi''s more permissive qiyas use vs. Hanbali''s more restrictive text-priority) to assess whether authentication-first procedures produce measurably different or better-calibrated legal rulings, independent of the guild interests of those administering authentication.',
    'If authentication-first is epistemically load-bearing independent of guild interest, the extraction component shrinks toward genuine coordination cost; if the procedure''s stringency tracks guild boundary-maintenance more than epistemic gain, extraction is higher than currently authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeping_vs_epistemic_necessity, conceptual, 'Whether hadith-authentication gatekeeping is epistemically necessary or primarily guild-protective.').

omega_variable(
    companions_only_ijma_restriction_rationale,
    'Is restricting binding ijma to the Companions'' generation a principled claim about unique proximity to revelation, or a strategic move that forecloses later, potentially destabilizing consensus claims (including claims that might have empowered rationalist or custom-based jurists)?',
    'Textual-historical analysis of al-Shafi''i''s own stated rationale in the Risala compared against the practical effect of the restriction on contemporaneous rival methodological claims.',
    'If principled, the restriction is a defensible axiom within the reading; if strategic, the ijma-restriction axiom itself becomes evidence of asymmetric extraction rather than neutral method.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(companions_only_ijma_restriction_rationale, conceptual, 'Whether the Companions-only ijma restriction is principled or strategically foreclosing.').

omega_variable(
    kernel_framing_alternative,
    'Should this constraint be framed around the source-ranking procedure itself (as authored), or around the higher-order legitimacy claim that ''a systematic meta-discipline is necessary at all'' — a claim all four sibling readings share and that could itself be a separate, prior constraint?',
    'Decompose further: author a fifth story for the shared meta-level claim (''usul al-fiqh as a discipline is necessary and legitimate'') and test whether its ε and beneficiary structure differ meaningfully from any single school''s reading.',
    'If the meta-level claim has its own distinct beneficiary/victim structure (e.g., ''jurist-scholars as a class'' vs. ''lay religious authority''), the current four-reading decomposition is incomplete and a prior kernel layer should be added; if not, the current framing (four sibling readings, no prior layer) is sufficient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_alternative, conceptual, 'Whether an additional prior-layer constraint is needed above the four school-specific readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__shafii_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t0, usul_al_fiqh_method__shafii_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(usul_tr_t40, usul_al_fiqh_method__shafii_reading, theater_ratio, 40, 0.13).
narrative_ontology:measurement(usul_tr_t80, usul_al_fiqh_method__shafii_reading, theater_ratio, 80, 0.16).
narrative_ontology:measurement(usul_tr_t120, usul_al_fiqh_method__shafii_reading, theater_ratio, 120, 0.18).
narrative_ontology:measurement(usul_tr_t160, usul_al_fiqh_method__shafii_reading, theater_ratio, 160, 0.2).
narrative_ontology:measurement(usul_tr_t200, usul_al_fiqh_method__shafii_reading, theater_ratio, 200, 0.22).

% Extraction over time
narrative_ontology:measurement(usul_be_t0, usul_al_fiqh_method__shafii_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(usul_be_t40, usul_al_fiqh_method__shafii_reading, base_extractiveness, 40, 0.33).
narrative_ontology:measurement(usul_be_t80, usul_al_fiqh_method__shafii_reading, base_extractiveness, 80, 0.37).
narrative_ontology:measurement(usul_be_t120, usul_al_fiqh_method__shafii_reading, base_extractiveness, 120, 0.39).
narrative_ontology:measurement(usul_be_t160, usul_al_fiqh_method__shafii_reading, base_extractiveness, 160, 0.41).
narrative_ontology:measurement(usul_be_t200, usul_al_fiqh_method__shafii_reading, base_extractiveness, 200, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t0, usul_al_fiqh_method__shafii_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(usul_su_t40, usul_al_fiqh_method__shafii_reading, suppression_requirement, 40, 0.29).
narrative_ontology:measurement(usul_su_t80, usul_al_fiqh_method__shafii_reading, suppression_requirement, 80, 0.32).
narrative_ontology:measurement(usul_su_t120, usul_al_fiqh_method__shafii_reading, suppression_requirement, 120, 0.34).
narrative_ontology:measurement(usul_su_t160, usul_al_fiqh_method__shafii_reading, suppression_requirement, 160, 0.36).
narrative_ontology:measurement(usul_su_t200, usul_al_fiqh_method__shafii_reading, suppression_requirement, 200, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__shafii_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(usul_al_fiqh_method__shafii_reading, 0.1).
narrative_ontology:affects_constraint(usul_al_fiqh_method__shafii_reading, hanafi_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__shafii_reading, maliki_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__shafii_reading, hanbali_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four sibling readings of the usul_al_fiqh_method kernel. All four decompose the natural-language concept 'Islamic legal source hierarchy' into structurally distinct claims about how Quran, hadith, ijma, and qiyas should be ranked and who administers that ranking. Each reading has its own ε, its own beneficiary/victim structure, and its own claimed type — they are not the same constraint measured differently. Network edges here connect this reading to the three siblings for contamination-propagation and cross-reading comparison purposes only; they are not evidence that the readings share an ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
