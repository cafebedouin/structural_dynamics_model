% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__shafii_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: Shafi'i Usul al-Fiqh: Hadith-Authentication-Prerequisite Source Hierarchy
 *   domain: Islamic Jurisprudence / Legal Theory / Comparative Law
 *
 * SUMMARY:
 *   This story authors the Shafi'i reading of the usul al-fiqh kernel: the
 *   methodological commitment that hadith authentication is a prerequisite
 *   gate for legal derivation, that analogical reasoning (qiyas) is licit
 *   only once the absence of authenticated hadith is established, that
 *   binding consensus (ijma) is restricted to the Companions' generation, and
 *   that these rules are systematized into a meta-discipline governing the
 *   hierarchy of legal sources. This is one reading among the four schools'
 *   readings of the same underlying kernel (which legal-source-hierarchy
 *   governs derivation); the Hanafi, Maliki, and Hanbali readings are
 *   separate constraints with their own epsilon and stakeholder structures,
 *   linked here only through cs_structure and network fields, never folded
 *   into this constraint's classification.
 *
 * KEY AGENTS:
 *   - hadith_transmission_specialists: gatekeeping beneficiary (institutional/arbitrage) — controls the authentication gate
 *   - rationalist_jurists: subordinated payer (moderate/constrained) — analogy devalued relative to transmission expertise
 *   - regional_practice_based_jurists: subordinated payer (moderate/constrained) — communal practice loses independent evidentiary status
 *   - shafii_school_jurists: identity-locked beneficiary (organized/identity_locked) — professional standing constituted by the hierarchy
 *   - later_generation_consensus_claimants: excluded (powerless/trapped) — foreclosed from generating new binding consensus
 *   - comparative_legal_historians: analytical observer — sees the four-reading structure as a whole
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__shafii_reading, 0.42).
domain_priors:suppression_score(usul_al_fiqh_method__shafii_reading, 0.38).
domain_priors:theater_ratio(usul_al_fiqh_method__shafii_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__shafii_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__shafii_reading, "Shafi'i Usul al-Fiqh: Hadith-Authentication-Prerequisite Source Hierarchy").
narrative_ontology:topic_domain(usul_al_fiqh_method__shafii_reading, "Islamic Jurisprudence / Legal Theory / Comparative Law").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__shafii_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__shafii_reading, '05639a25-87b7-44b5-876e-b7de42edc9ef').
narrative_ontology:cs_kernel_codification('05639a25-87b7-44b5-876e-b7de42edc9ef', formalized).
narrative_ontology:cs_authority_grounding('05639a25-87b7-44b5-876e-b7de42edc9ef', lineage).
narrative_ontology:cs_interpretation_layer_present('05639a25-87b7-44b5-876e-b7de42edc9ef').
narrative_ontology:cs_reading_relation('05639a25-87b7-44b5-876e-b7de42edc9ef', usul_al_fiqh_method__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('05639a25-87b7-44b5-876e-b7de42edc9ef', usul_al_fiqh_method__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('05639a25-87b7-44b5-876e-b7de42edc9ef', usul_al_fiqh_method__hanbali_reading, influences).
narrative_ontology:cs_axiom('05639a25-87b7-44b5-876e-b7de42edc9ef', foundational, authenticated_hadith_prerequisite_to_derivation).
narrative_ontology:cs_axiom_status(authenticated_hadith_prerequisite_to_derivation, holdable).
narrative_ontology:cs_axiom_grounding('05639a25-87b7-44b5-876e-b7de42edc9ef', authenticated_hadith_prerequisite_to_derivation, conventional).
narrative_ontology:cs_axiom('05639a25-87b7-44b5-876e-b7de42edc9ef', foundational, ijma_restricted_to_companion_generation).
narrative_ontology:cs_axiom_status(ijma_restricted_to_companion_generation, holdable).
narrative_ontology:cs_axiom_grounding('05639a25-87b7-44b5-876e-b7de42edc9ef', ijma_restricted_to_companion_generation, conventional).
narrative_ontology:cs_axiom('05639a25-87b7-44b5-876e-b7de42edc9ef', secondary, qiyas_subordinate_to_textual_silence).
narrative_ontology:cs_axiom_status(qiyas_subordinate_to_textual_silence, holdable).
narrative_ontology:cs_axiom_grounding('05639a25-87b7-44b5-876e-b7de42edc9ef', qiyas_subordinate_to_textual_silence, instrumental).
narrative_ontology:cs_reference_frame('05639a25-87b7-44b5-876e-b7de42edc9ef', shafii_systematized_source_hierarchy).
narrative_ontology:cs_drift_state('05639a25-87b7-44b5-876e-b7de42edc9ef', post_classical_taqlid_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('05639a25-87b7-44b5-876e-b7de42edc9ef', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__shafii_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__shafii_reading, hadith_transmission_specialists).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__shafii_reading, isnad_critics).
narrative_ontology:constraint_victim(usul_al_fiqh_method__shafii_reading, rationalist_jurists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__shafii_reading, regional_practice_based_jurists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__shafii_reading, shafii_school_jurists).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__shafii_reading, lay_petitioners_and_litigants).
narrative_ontology:constraint_victim(usul_al_fiqh_method__shafii_reading, lay_petitioners_and_litigants).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__shafii_reading, textual_traceability_doctrine).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__shafii_reading, companion_era_consensus_finality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Muhaddithun who develop and administer isnad-criticism (jarh wa ta'dil) and hadith-grading science. Under the Shafi'i method, no legal ruling can bypass their authentication work, since qiyas is only licit once the absence of an authenticated hadith is established. They occupy the gate: a jurist must pass through their verdicts on chain-reliability before reasoning independently is even permitted.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, hadith_transmission_specialists, agenda_setter,
    institutional, civilizational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__shafii_reading, hadith_transmission_specialists, beneficiary).

% Jurists whose authority rested on ra'y and expansive qiyas (broadly the proto-Hanafi orientation) now find their reasoning subordinated: they may only exercise analogy after certifying the textual record is silent, a certification controlled by the hadith specialists. Their intellectual capital in reasoned jurisprudence is devalued relative to transmission expertise; exit means either submitting to the hierarchy or working outside the increasingly dominant Shafi'i-systematized mainstream, at the cost of legitimacy.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, rationalist_jurists, payer,
    moderate, generational, constrained, continental).

% Jurists (broadly the proto-Maliki orientation) who grounded rulings in the living practice of a community (Medinan 'amal) rather than isnad-verified transmission. The Shafi'i hierarchy displaces communal practice as an independent source, requiring it be justified through, or subordinated to, authenticated hadith and Companion-era consensus. Their local evidentiary tradition loses standing as a freestanding source.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, regional_practice_based_jurists, payer,
    moderate, generational, constrained, regional).

% Jurists trained in the systematized usul al-fiqh who derive their professional standing and interpretive authority from mastery of the source hierarchy itself (Quran, authenticated Sunna, Companion ijma, then qiyas). Their disciplinary identity is constituted by the hierarchy's legitimacy; abandoning it would dissolve the basis of their expertise.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, shafii_school_jurists, beneficiary,
    organized, civilizational, identity_locked, continental).

% Later scholarly communities who might have claimed their own broad ijma as binding are foreclosed by the restriction of authoritative consensus to the Companions' generation. They have no route to generate new binding consensus under this reading; their potential voice on emergent questions is structurally excluded from the top of the source hierarchy.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, later_generation_consensus_claimants, excluded,
    powerless, generational, trapped, continental).

% Ordinary Muslims seeking rulings depend entirely on jurists trained in the now-dominant methodology to access legal remedies. They benefit from a more rigorous, traceable evidentiary standard reducing arbitrary rulings, but bear the cost when a locally sensible custom or reasoned solution is unavailable because it cannot clear the hadith-authentication and restricted-ijma gates.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, lay_petitioners_and_litigants, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__shafii_reading, lay_petitioners_and_litigants, beneficiary).

% Scholars of comparative Islamic law who study how the Shafi'i systematization reshaped juristic authority relative to Hanafi, Maliki, and Hanbali methods, without themselves being bound by any single school's hierarchy.
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
% COORDINATION_FUNCTION: Provides a disciplined, sequenced procedure for deriving law that constrains arbitrary or purely subjective reasoning: rulings must trace to Quran, then authenticated hadith, then narrowly-defined Companion consensus, before independent analogy is permitted — reducing juristic disagreement driven by unverifiable or idiosyncratic reasoning.
% TRANSFER_FUNCTION: Moves interpretive authority and the social/institutional standing that follows from it away from jurists whose expertise was rationalist reasoning or regional communal practice, and toward specialists in hadith transmission and chain-criticism, who become the indispensable gatekeepers any legal derivation must pass through.
% ABSENT_VOICES: Rationalist jurists and regional-practice jurists are structurally present but subordinated rather than fully excluded; the genuinely excluded voice is later generations who might claim their own consensus — the restriction of ijma to the Companions forecloses their participation in the highest tier of authority by definitional fiat, and they have no seat from which to contest it within this framework.
% DISAPPEARANCE_RATIONALE: If the Shafi'i hierarchy (authentication-first, restricted ijma, qiyas-as-last-resort) vanished, hadith specialists would lose their gatekeeping monopoly over legal derivation, rationalist and practice-based jurists would regain standing as independent sources of law, and the entire disciplinary architecture of usul al-fiqh as currently systematized in Shafi'i-influenced legal education would need to be rebuilt around a different source hierarchy.
% FOUNDING_PROBLEM: Early juristic disagreement (ikhtilaf) had grown wide enough that rulings varied dramatically by region and by individual jurist's reasoning, some resting on weakly-attested reports and expansive personal opinion; the founding problem was to discipline legal derivation so that rulings could be defended as traceable to reliably transmitted revelation rather than idiosyncratic judgment.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Islamic legal theory (including scholars outside any of the four schools, working in comparative and historical-critical traditions) corroborate that the founding problem of unchecked ikhtilaf was real in the 8th-9th centuries; the same historians, however, note that the specific hierarchy the Shafi'i method fixed is itself a contingent methodological choice among viable alternatives (the Hanafi, Maliki, and Hanbali readings), not a solution uniquely dictated by the problem, so the standing hierarchy's continued exclusivity is contested by evidence that competing hierarchies solved comparable disagreement through different means.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__shafii_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__shafii_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__shafii_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate (0.42) rather than high: the coordination function (disciplining ikhtilaf through traceable evidentiary standards) is real and substantial, not mere cover, but the hierarchy also concentrates gatekeeping power in hadith specialists at the structural expense of rationalist and practice-based jurists, which is genuine asymmetric extraction riding on the same structure. Suppression (0.38) reflects active enforcement of the source-hierarchy sequencing (qiyas is impermissible until textual absence is certified) but not brute coercion — the suppression operates through disciplinary legitimacy rather than force. Accessibility collapse (0.55) is moderate: alternative methodologies (Hanafi, Maliki) remain live and practiced by other communities, so alternatives have not vanished, only been subordinated within the Shafi'i framework. Resistance (0.5) reflects the genuine historical contestation among schools over exactly this hierarchy.
 *
 * PERSPECTIVAL GAP:
 *   The hadith specialist's seat is likely to compute this constraint as much closer to a rope (or even mountain-adjacent, given the framing of hadith authenticity as approaching settled fact) than the rationalist jurist's seat, which experiences the same rules as an imposed subordination of their own reasoned methodology. The engine computing divergent per-seat classifications from these structural declarations, rather than a single verdict, is the correct behavior here.
 *
 * DIRECTIONALITY LOGIC:
 *   Hadith transmission specialists sit near the beneficiary end: the hierarchy makes their expertise structurally indispensable and they administer the gate (arbitrage-grade exit — their skills transfer across any school that values authentication). Rationalist jurists and regional-practice jurists sit near the target end: their independent evidentiary bases are subordinated, and their exit options are constrained by the increasing institutional dominance of the systematized method in legal education and judicial appointment. Later-generation consensus claimants are the clearest excluded/trapped case: the restriction of ijma to the Companions is a structural foreclosure with no available exit, since the foreclosure is definitional rather than a resource constraint they could work around.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents two symmetric misreadings: treating the hierarchy as pure disinterested method (ignoring that it reallocates authority toward a specific specialist class) and treating it as pure power-grab by hadith scholars (ignoring that it responds to a genuine problem of undisciplined ikhtilaf and unverifiable reasoning). The coordination function is real and the extraction is real; both must be held simultaneously, which is exactly the tangled_rope signature — coordination and extraction through the same structure, not sequential phases.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_inseparability,
    'Is the hadith-authentication prerequisite structurally necessary to solve the ikhtilaf problem (undisciplined juristic disagreement), or is authentication-gating a separable extraction layered onto a coordination function that could be achieved through looser evidentiary standards, as the Hanafi and Maliki readings attempt?',
    'Comparative historical analysis of legal outcome consistency across regions that adopted stricter versus looser evidentiary hierarchies (Shafi''i/Hanbali-influenced regions vs. Hanafi/Maliki-influenced regions) during the same period, controlling for other institutional factors.',
    'If separable, the strict authentication gate is substantially extraction riding on a genuine but achievable-by-other-means coordination goal, raising effective epsilon; if inseparable, more of the measured extraction is properly attributed to necessary coordination cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_inseparability, conceptual, 'Whether authentication-gating is necessary to the coordination function or a separable extractive layer.').

omega_variable(
    companion_consensus_boundary_naturalness,
    'Is the restriction of binding ijma to the Companions'' generation a principled epistemic claim (their proximity to revelation makes their consensus uniquely reliable) or a constructed boundary that happens to close off any future generation''s ability to generate competing binding authority?',
    'Textual-historical analysis of when and by whom the Companion-restriction was first argued, and whether contemporaneous jurists proposed alternative consensus boundaries that were suppressed rather than out-argued.',
    'If principled, later exclusion is a side effect of an epistemically defensible position; if constructed for closure, the exclusion of later_generation_consensus_claimants is a more central feature of the constraint''s function than incidental.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(companion_consensus_boundary_naturalness, conceptual, 'Whether restricting ijma to Companions is epistemically principled or a constructed authority-closure device.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Given that all four schools'' readings of usul al-fiqh solve overlapping versions of the same ikhtilaf-discipline problem, is there a framing under which the Shafi''i reading''s specific hierarchy is the historically dominant and therefore de facto authoritative reading of the kernel, versus a framing under which all four readings remain genuinely co-equal and no single reading has priority?',
    'Track institutional adoption patterns (which reading became dominant in which judicial and educational systems, and when) versus doctrinal self-understanding within each school (whether each school regarded itself as one of several valid readings or as uniquely correct).',
    'Under the dominance framing, this reading''s classification might carry more weight as a de facto governing standard (higher effective suppression on rival readings); under the co-equal framing, this story''s epsilon and suppression values describe only this reading''s own community and should not be read as claims about the kernel''s overall resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether the Shafi''i reading should be understood as historically dominant or as one of several co-equal readings, and what that implies for interpreting this story''s metrics relative to the kernel as a whole.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__shafii_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t0, usul_al_fiqh_method__shafii_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(usul_tr_t0, projected).
narrative_ontology:measurement(usul_tr_t200, usul_al_fiqh_method__shafii_reading, theater_ratio, 200, 0.13).
narrative_ontology:measurement_basis(usul_tr_t200, projected).
narrative_ontology:measurement(usul_tr_t400, usul_al_fiqh_method__shafii_reading, theater_ratio, 400, 0.16).
narrative_ontology:measurement_basis(usul_tr_t400, projected).
narrative_ontology:measurement(usul_tr_t600, usul_al_fiqh_method__shafii_reading, theater_ratio, 600, 0.18).
narrative_ontology:measurement_basis(usul_tr_t600, projected).
narrative_ontology:measurement(usul_tr_t900, usul_al_fiqh_method__shafii_reading, theater_ratio, 900, 0.19).
narrative_ontology:measurement_basis(usul_tr_t900, projected).
narrative_ontology:measurement(usul_tr_t1200, usul_al_fiqh_method__shafii_reading, theater_ratio, 1200, 0.2).
narrative_ontology:measurement_basis(usul_tr_t1200, projected).

% Extraction over time
narrative_ontology:measurement(usul_be_t0, usul_al_fiqh_method__shafii_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(usul_be_t0, projected).
narrative_ontology:measurement(usul_be_t200, usul_al_fiqh_method__shafii_reading, base_extractiveness, 200, 0.3).
narrative_ontology:measurement_basis(usul_be_t200, projected).
narrative_ontology:measurement(usul_be_t400, usul_al_fiqh_method__shafii_reading, base_extractiveness, 400, 0.34).
narrative_ontology:measurement_basis(usul_be_t400, projected).
narrative_ontology:measurement(usul_be_t600, usul_al_fiqh_method__shafii_reading, base_extractiveness, 600, 0.38).
narrative_ontology:measurement_basis(usul_be_t600, projected).
narrative_ontology:measurement(usul_be_t900, usul_al_fiqh_method__shafii_reading, base_extractiveness, 900, 0.4).
narrative_ontology:measurement_basis(usul_be_t900, projected).
narrative_ontology:measurement(usul_be_t1200, usul_al_fiqh_method__shafii_reading, base_extractiveness, 1200, 0.42).
narrative_ontology:measurement_basis(usul_be_t1200, projected).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t0, usul_al_fiqh_method__shafii_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(usul_su_t0, projected).
narrative_ontology:measurement(usul_su_t200, usul_al_fiqh_method__shafii_reading, suppression_requirement, 200, 0.3).
narrative_ontology:measurement_basis(usul_su_t200, projected).
narrative_ontology:measurement(usul_su_t400, usul_al_fiqh_method__shafii_reading, suppression_requirement, 400, 0.33).
narrative_ontology:measurement_basis(usul_su_t400, projected).
narrative_ontology:measurement(usul_su_t600, usul_al_fiqh_method__shafii_reading, suppression_requirement, 600, 0.35).
narrative_ontology:measurement_basis(usul_su_t600, projected).
narrative_ontology:measurement(usul_su_t900, usul_al_fiqh_method__shafii_reading, suppression_requirement, 900, 0.37).
narrative_ontology:measurement_basis(usul_su_t900, projected).
narrative_ontology:measurement(usul_su_t1200, usul_al_fiqh_method__shafii_reading, suppression_requirement, 1200, 0.38).
narrative_ontology:measurement_basis(usul_su_t1200, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__shafii_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(usul_al_fiqh_method__shafii_reading, 0.12).
narrative_ontology:affects_constraint(usul_al_fiqh_method__shafii_reading, hanafi_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__shafii_reading, maliki_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__shafii_reading, hanbali_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the usul_al_fiqh_method kernel (hanafi_reading, maliki_reading, hanbali_reading, and this shafii_reading). Each reading carries its own epsilon, beneficiary/victim structure, and classification per the ε-invariance principle; none is a measurement of the others taken from a different angle. The Shafi'i reading's distinguishing structural move — hadith-authentication as strict prerequisite plus Companion-restricted ijma — is causally influential on the Hanbali reading's textual-maximalism (both privilege authenticated text over reasoned analogy, though Hanbali goes further in preferring weak hadith over qiyas), which is why this story declares 'influences' rather than 'coexists_with' toward hanbali_reading, while treating hanafi_reading and maliki_reading as co-equal, non-foreclosing alternatives adopted by different jurist communities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
