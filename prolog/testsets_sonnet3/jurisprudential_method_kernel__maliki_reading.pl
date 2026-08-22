% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__maliki_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__maliki_reading, []).

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
 *   constraint_id: jurisprudential_method_kernel__maliki_reading
 *   human_readable: Maliki Reading: 'Amal Ahl al-Madina as Living Source of Law
 *   domain: religious/legal/institutional
 *
 * SUMMARY:
 *   This story instantiates the Maliki reading of the jurisprudential method
 *   kernel: that law derives from Qur'an and Hadith, but crucially, that the
 *   continuous, living practice of the Medinan community ('amal ahl
 *   al-Madina) constitutes a valid — at times superior — source of legal
 *   knowledge because Medina, as the city of the Prophet's own governance,
 *   preserved his practice through unbroken communal transmission rather than
 *   through fallible individual chains. This is a distinct constraint from
 *   the Hanafi reading (which routes divine intent through analogical
 *   extension), the Shafi'i reading (which subordinates all sources to a
 *   strict hierarchy arbitrated by hadith transmission), and the Hanbali
 *   reading (which treats any reasoning beyond literal text and unanimous
 *   consensus as corrupting innovation). Each of these is authored as its own
 *   constraint story with its own ε; this file does not average across them.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__maliki_reading, 0.48).
domain_priors:suppression_score(jurisprudential_method_kernel__maliki_reading, 0.42).
domain_priors:theater_ratio(jurisprudential_method_kernel__maliki_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__maliki_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__maliki_reading, "Maliki Reading: 'Amal Ahl al-Madina as Living Source of Law").
narrative_ontology:topic_domain(jurisprudential_method_kernel__maliki_reading, "religious/legal/institutional").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__maliki_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__maliki_reading, 'cb0bde29-7a18-455f-a3ab-9666efaeea04').
narrative_ontology:cs_kernel_codification('cb0bde29-7a18-455f-a3ab-9666efaeea04', distributed).
narrative_ontology:cs_authority_grounding('cb0bde29-7a18-455f-a3ab-9666efaeea04', lineage).
narrative_ontology:cs_interpretation_layer_present('cb0bde29-7a18-455f-a3ab-9666efaeea04').
narrative_ontology:cs_reading_relation('cb0bde29-7a18-455f-a3ab-9666efaeea04', jurisprudential_method_kernel__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('cb0bde29-7a18-455f-a3ab-9666efaeea04', jurisprudential_method_kernel__shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('cb0bde29-7a18-455f-a3ab-9666efaeea04', jurisprudential_method_kernel__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('cb0bde29-7a18-455f-a3ab-9666efaeea04', foundational, continuous_communal_practice_is_transmitted_evidence).
narrative_ontology:cs_axiom_status(continuous_communal_practice_is_transmitted_evidence, holdable).
narrative_ontology:cs_axiom_grounding('cb0bde29-7a18-455f-a3ab-9666efaeea04', continuous_communal_practice_is_transmitted_evidence, conventional).
narrative_ontology:cs_axiom('cb0bde29-7a18-455f-a3ab-9666efaeea04', secondary, medina_geographic_proximity_confers_evidentiary_priority).
narrative_ontology:cs_axiom_status(medina_geographic_proximity_confers_evidentiary_priority, holdable).
narrative_ontology:cs_axiom_grounding('cb0bde29-7a18-455f-a3ab-9666efaeea04', medina_geographic_proximity_confers_evidentiary_priority, empirically_contingent).
narrative_ontology:cs_reference_frame('cb0bde29-7a18-455f-a3ab-9666efaeea04', medinan_practice_as_primary_evidentiary_source).
narrative_ontology:cs_drift_state('cb0bde29-7a18-455f-a3ab-9666efaeea04', post_isnad_science_maturation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('cb0bde29-7a18-455f-a3ab-9666efaeea04', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__maliki_reading, medinan_scholarly_lineage).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__maliki_reading, maliki_jurists).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__maliki_reading, non_medinan_interpretive_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__maliki_reading, lay_muslims_in_maliki_territories).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__maliki_reading, lay_muslims_in_maliki_territories).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Transmits and administers the doctrine that Medina's collective practice preserves the Prophet's actual conduct more reliably than isolated hadith chains from elsewhere. Adjudicates which practices count as authentic 'amal, and thereby controls what counts as valid law within the school. Its authority is self-referential: the community's practice is evidence for itself.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, medinan_scholarly_lineage, agenda_setter,
    institutional, civilizational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__maliki_reading, medinan_scholarly_lineage, beneficiary).

% Build careers, fatwas, and institutional standing on the legitimacy of 'amal as a source superior to isolated hadith. Their interpretive authority depends on the Medinan-practice premise holding; abandoning it would collapse the distinctiveness of the school's method relative to rivals.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, maliki_jurists, beneficiary,
    organized, generational, constrained, continental).

% Jurists and communities outside Medina who transmitted authentic Prophetic practice through hadith chains rather than through continuous residence in Medina. Under this reading, their transmissions can be overridden when they conflict with Medinan practice, regardless of chain quality — their claim to equal authenticity is structurally discounted by geography, not by evidentiary weakness.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, non_medinan_interpretive_communities, payer,
    organized, generational, constrained, continental).

% Receive a stable, locally-legitimated legal framework for daily religious and civil life (marriage, inheritance, transactions) without needing to adjudicate hadith authenticity disputes themselves. Pay the cost of reduced access to legitimate rulings drawn from non-Medinan practice, even where such rulings might better fit local custom.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, lay_muslims_in_maliki_territories, beneficiary,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__maliki_reading, lay_muslims_in_maliki_territories, payer).

% Specialists in chain-of-transmission verification whose methodological standard (rigorous isnad criticism) is partially displaced by the 'amal doctrine's claim that communal practice can outweigh or corroborate individual hadith. Their objection — that practice is not self-authenticating and can drift from origin — is acknowledged in classical debate but structurally subordinated within Maliki method.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, hadith_critics_isnad_scholars, excluded,
    moderate, generational, constrained, continental).

% Study how each of the four Sunni schools grounds its epistemic authority and trace how 'amal ahl al-Madina functioned historically — including instances where it diverged from documented hadith and instances where it plausibly preserved otherwise-lost practice.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, comparative_legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a workable, non-infinitely-regressive method for settling legal questions in the first few generations after the Prophet's death, when hadith transmission was still being systematized and disputed: treat the continuous, observable practice of the city where the Prophet lived and legislated as itself a form of transmitted evidence, resolving disputes that isolated chains could not settle.
% TRANSFER_FUNCTION: Moves interpretive authority and the resulting legal, social, and economic deference toward Medinan-descended scholarship and away from jurists and communities whose authenticity claims rest on hadith transmission alone, regardless of the comparative strength of those chains.
% ABSENT_VOICES: Isnad-critical hadith scholars and non-Medinan legal communities (early Kufan and Basran jurists, later Hanafi and Shafi'i methodologists) would object that communal practice is not self-authenticating and can drift, be locally corrupted, or reflect post-Prophetic accretion — but the doctrine's own internal test (is this practice continuous and traceable to Medina) structurally cannot be evaluated by those outside the tradition it privileges.
% DISAPPEARANCE_RATIONALE: If 'amal ahl al-Madina lost its status as a valid source, Maliki jurisprudence would lose its principal methodological distinctiveness from Hanafi qiyas-based and Shafi'i hadith-hierarchy-based reasoning; rulings currently justified by appeal to Medinan practice would need re-derivation from hadith and qiyas alone, and the school's institutional authority in North and West Africa would face a genealogical crisis.
% FOUNDING_PROBLEM: In the first two centuries after the Prophet's death, the Islamic community needed a reliable way to determine authentic Prophetic practice before hadith science had matured its full critical apparatus, and before geographically scattered transmission chains could be systematically cross-verified.
% FOUNDING_PROBLEM_CORROBORATION: Maliki jurists and Medinan scholarly lineage attest the founding problem remains structurally live — that continuous communal practice still corroborates or outweighs isolated transmission in edge cases. Comparative legal historians and hadith-critical scholars, writing from outside the Maliki tradition, note that mature isnad science largely solved the verification problem the doctrine was built for, and that 'amal's continued authority now functions more to preserve school identity and juristic lineage than to solve an unsolved evidentiary gap; no fully independent (non-Sunni-school-affiliated) corroborating source was identified.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__maliki_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__maliki_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__maliki_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jurisprudential_method_kernel__maliki_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__maliki_reading, 0.48, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__maliki_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisprudential_method_kernel__maliki_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jurisprudential_method_kernel__maliki_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.48 by the interval's end) because the doctrine's cost is not direct material rent extraction but epistemic-authority concentration: it structurally privileges one geographic community's transmission as a species of evidence unto itself, discounting equally rigorous non-Medinan chains on grounds of geography rather than chain quality. Suppression is moderate (0.42) — the doctrine does not forcibly prevent non-Medinan jurisprudence from existing, but it does subordinate it within any legal system that adopts Maliki method, and school loyalty functions as a soft suppressive mechanism against methodological defection. Theater ratio rises gradually (0.10 to 0.28) as the doctrine ages: in the earliest generations 'amal plausibly tracked live, observable practice; over centuries it increasingly functions as an inherited justificatory frame for rulings whose original evidentiary basis in observed Medinan practice is no longer independently checkable.
 *
 * PERSPECTIVAL GAP:
 *   From the Medinan-lineage seat, 'amal is simply better evidence — the city where the Prophet legislated preserved his practice more faithfully than any single chain could. From the non-Medinan seat, this is a geographic accident elevated to an epistemic privilege: equally rigorous transmission is discounted not for evidentiary weakness but for where it was transmitted. The engine computes these as structurally different experiences of the same arrangement from the declared power/exit data; neither seat's self-description settles which computation is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Medinan scholarly lineage and Maliki jurists sit near the beneficiary end: their interpretive authority, career structures, and institutional standing are constituted by the validity of the 'amal doctrine. Non-Medinan interpretive communities sit near the target end: their equally-transmitted practices can be overridden or discounted purely on the geographic-transmission premise, which is a structural cost imposed regardless of the actual reliability of their sources. Lay Muslims in Maliki territories are genuinely mixed — they get a coherent, locally legitimate legal system (real coordination benefit) but pay in reduced access to potentially better-fitting rulings from outside the school's privileged sources.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — settling authentic practice before hadith science matured — was substantially real in the first two centuries. Whether it remains live today, after centuries of isnad-critical scholarship largely solved the verification problem the doctrine addressed, is the central mandatrophy question this story leaves open via the founding_problem fields: the doctrine may have transitioned from solving a genuine evidentiary gap to serving as an identity-preserving methodological marker for an established school. Classifying this as tangled_rope rather than snare or mountain reflects that it retains a real coordination function (a workable, non-infinitely-regressive method for a functioning legal system) alongside a genuine asymmetric cost (geographic discounting of non-Medinan authenticity claims) that requires active institutional maintenance (school loyalty, juristic training, doctrinal transmission) to persist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    amal_self_authentication_ambiguity,
    'Does ''amal ahl al-Madina function as genuinely independent corroborating evidence of Prophetic practice, or is it a self-referential doctrine where the community''s own practice validates itself without external check?',
    'Historical-critical comparison of specific rulings justified by ''amal against independently attested early hadith and non-Medinan practice, looking for cases where ''amal diverged from or overrode stronger isnad chains without independent corroboration.',
    'If ''amal frequently diverges from well-attested hadith without independent corroboration, the doctrine functions more as an authority-concentration mechanism than as genuine additional evidence, supporting a higher extraction reading. If it consistently aligns with or fills genuine gaps in hadith transmission, it supports the coordination-function reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amal_self_authentication_ambiguity, empirical, 'Whether Medinan practice is independent evidence or self-validating doctrine.').

omega_variable(
    kernel_reading_framing_alternative,
    'Is the more defensible framing of this constraint ''Medinan practice as a fifth evidentiary source alongside Qur''an/Hadith/Ijma/Qiyas'' (a methodological addition) or ''Medinan communal identity elevated to epistemic authority'' (an identity-coordination mechanism wearing methodological cover)?',
    'Trace whether classical Maliki jurists themselves treated ''amal as falsifiable by contrary hadith evidence (methodological reading) or as trumping contrary hadith categorically (identity reading) across documented juristic disputes (e.g. Malik''s own recorded departures from hadith on grounds of ''amal).',
    'The methodological framing supports classification nearer rope (genuine, checkable evidentiary supplement); the identity framing supports classification nearer tangled_rope or snare (geographic identity dressed as epistemology). This story adopts the methodological framing as primary but flags the identity-coordination alternative as live.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_framing_alternative, conceptual, 'Whether ''amal is best read as evidentiary method or identity-coordination mechanism.').

omega_variable(
    founding_problem_obsolescence,
    'Has the maturation of isnad-critical hadith science over subsequent centuries rendered the original evidentiary problem ''amal was built to solve substantially solved, such that the doctrine''s persistence is now primarily identity/institutional rather than evidentiary?',
    'Compare the rate of unresolved hadith-authenticity disputes in the early period (when ''amal was formulated) against the rate in later centuries after isnad science matured; a sharp decline would support obsolescence.',
    'If the founding problem is substantially solved, the doctrine''s continued operation reads as mandatrophy — institutional persistence past functional necessity — strengthening the case for theater_ratio''s rising trajectory and for treating current-era invocations of ''amal as more extractive than early-era invocations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether hadith science''s maturation obsoleted the evidentiary gap ''amal was built to fill.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__maliki_reading, 0, 1300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t0, jurisprudential_method_kernel__maliki_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(juri_tr_t200, jurisprudential_method_kernel__maliki_reading, theater_ratio, 200, 0.14).
narrative_ontology:measurement(juri_tr_t400, jurisprudential_method_kernel__maliki_reading, theater_ratio, 400, 0.18).
narrative_ontology:measurement(juri_tr_t700, jurisprudential_method_kernel__maliki_reading, theater_ratio, 700, 0.22).
narrative_ontology:measurement(juri_tr_t1000, jurisprudential_method_kernel__maliki_reading, theater_ratio, 1000, 0.25).
narrative_ontology:measurement(juri_tr_t1300, jurisprudential_method_kernel__maliki_reading, theater_ratio, 1300, 0.28).

% Extraction over time
narrative_ontology:measurement(juri_be_t0, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(juri_be_t200, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 200, 0.35).
narrative_ontology:measurement(juri_be_t400, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 400, 0.4).
narrative_ontology:measurement(juri_be_t700, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 700, 0.44).
narrative_ontology:measurement(juri_be_t1000, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 1000, 0.46).
narrative_ontology:measurement(juri_be_t1300, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 1300, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t0, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(juri_su_t200, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 200, 0.3).
narrative_ontology:measurement(juri_su_t400, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 400, 0.34).
narrative_ontology:measurement(juri_su_t700, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 700, 0.38).
narrative_ontology:measurement(juri_su_t1000, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 1000, 0.4).
narrative_ontology:measurement(juri_su_t1300, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 1300, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__maliki_reading, identity_coordination).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel__hanafi_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel__shafii_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel__hanbali_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of jurisprudential_method_kernel, each a separate constraint with its own ε, beneficiary/victim structure, and classification (per the ε-invariance decomposition principle). The maliki_reading privileges continuous Medinan communal practice as a source; hanafi_reading privileges analogical extension; shafii_reading privileges a strict source hierarchy arbitrated by hadith transmission; hanbali_reading privileges literal text and unanimous consensus while rejecting reasoning-based extension. All four compete for the same underlying legitimacy space (authentic derivation of law from the Prophet's example) without any one foreclosing the others within the broader Sunni legal tradition — they coexist as live methodological traditions held by different scholarly communities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
