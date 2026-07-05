% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__maliki_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: jurisprudential_method_kernel__maliki_reading
 *   human_readable: Maliki Reading: 'Amal Ahl al-Madina as Living Legal Source
 *   domain: religious/legal/institutional
 *
 * SUMMARY:
 *   This constraint models the Maliki school's foundational methodological
 *   claim: that the continuous, observed legal practice of the Medinan
 *   community ('amal ahl al-Madina) constitutes a valid and privileged source
 *   of Islamic law, alongside Qur'an and Hadith, because Medina preserved the
 *   Prophet's actual practice more faithfully than any other early Muslim
 *   community. This is one reading among four sibling readings of a shared
 *   jurisprudential-method kernel — the underlying question of how divine law
 *   is properly derived from revelation. The Hanafi, Shafi'i, and Hanbali
 *   readings answer this differently (analogical extension, hierarchical
 *   source-ranking, and literalist consensus, respectively) and are modeled
 *   as separate constraint stories, per the ε-invariance principle: each
 *   reading has a distinct beneficiary structure, distinct extraction
 *   profile, and distinct persistence mechanism, even though colloquially all
 *   four are 'Sunni jurisprudence.'
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
narrative_ontology:human_readable(jurisprudential_method_kernel__maliki_reading, "Maliki Reading: 'Amal Ahl al-Madina as Living Legal Source").
narrative_ontology:topic_domain(jurisprudential_method_kernel__maliki_reading, "religious/legal/institutional").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__maliki_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__maliki_reading, '29077fdf-d481-4d7c-93ab-7dc2832886bc').
narrative_ontology:cs_kernel_codification('29077fdf-d481-4d7c-93ab-7dc2832886bc', distributed).
narrative_ontology:cs_authority_grounding('29077fdf-d481-4d7c-93ab-7dc2832886bc', lineage).
narrative_ontology:cs_interpretation_layer_present('29077fdf-d481-4d7c-93ab-7dc2832886bc').
narrative_ontology:cs_reading_relation('29077fdf-d481-4d7c-93ab-7dc2832886bc', jurisprudential_method_kernel__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('29077fdf-d481-4d7c-93ab-7dc2832886bc', jurisprudential_method_kernel__shafii_reading, influences).
narrative_ontology:cs_reading_relation('29077fdf-d481-4d7c-93ab-7dc2832886bc', jurisprudential_method_kernel__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('29077fdf-d481-4d7c-93ab-7dc2832886bc', foundational, medinan_practice_evidentiary_primacy).
narrative_ontology:cs_axiom_status(medinan_practice_evidentiary_primacy, holdable).
narrative_ontology:cs_axiom_grounding('29077fdf-d481-4d7c-93ab-7dc2832886bc', medinan_practice_evidentiary_primacy, empirically_contingent).
narrative_ontology:cs_axiom('29077fdf-d481-4d7c-93ab-7dc2832886bc', secondary, continuous_communal_practice_as_transmitted_evidence).
narrative_ontology:cs_axiom_status(continuous_communal_practice_as_transmitted_evidence, holdable).
narrative_ontology:cs_axiom_grounding('29077fdf-d481-4d7c-93ab-7dc2832886bc', continuous_communal_practice_as_transmitted_evidence, conventional).
narrative_ontology:cs_reference_frame('29077fdf-d481-4d7c-93ab-7dc2832886bc', medinan_communal_practice_as_lived_prophetic_transmission).
narrative_ontology:cs_drift_state('29077fdf-d481-4d7c-93ab-7dc2832886bc', post_isnad_critical_scholarship_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('29077fdf-d481-4d7c-93ab-7dc2832886bc', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__maliki_reading, medinan_scholarly_lineage).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__maliki_reading, maliki_jurists).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__maliki_reading, non_medinan_interpretive_communities).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__maliki_reading, iraqi_analogical_jurists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Transmits and administers 'amal ahl al-Madina as a chain of witnessed communal practice traced to the Prophet's own city. Their proximity to the Prophet's lifetime and burial site is treated as evidentiary — they set the terms of what counts as authentic practice and are structurally positioned to always win disputes about what Medina 'actually did.'
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, medinan_scholarly_lineage, agenda_setter,
    institutional, civilizational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__maliki_reading, medinan_scholarly_lineage, beneficiary).

% Build careers, courts, and fatwa authority on the premise that Medinan practice is a privileged source. Their professional standing depends on the continued acceptance of 'amal as authoritative; abandoning the premise would collapse the distinctiveness of their school relative to Hanafi or Shafi'i method.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, maliki_jurists, beneficiary,
    organized, generational, constrained, regional).

% Communities and jurists outside Medina who preserved their own transmitted practices and hadith chains find their claims to equal authenticity structurally discounted by a kernel that privileges geography over independent transmission. They can dispute the ruling but cannot relocate their community's history to Medina.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, non_medinan_interpretive_communities, payer,
    moderate, generational, constrained, regional).

% Kufan and Basran jurists who developed qiyas and istihsan as tools for extending law to novel cases in a different social environment. The Maliki reading's insistence that Medinan practice trumps reasoned extension delegitimizes their methodological innovations as provincial deviations, even though they operate in a different empire with different administrative needs. They retain mobility — their method spreads regardless — but lose standing within any framework that adopts the Maliki premise.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, iraqi_analogical_jurists, payer,
    organized, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__maliki_reading, iraqi_analogical_jurists, excluded).

% Historians and comparative jurists across centuries who examine the four schools' foundational premises without a stake in any single school's authority claim. They note that 'amal ahl al-Madina functions simultaneously as a genuine transmission-preservation mechanism and as a geography-based legitimacy monopoly.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, later_muslim_jurists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jurisprudential_method_kernel__maliki_reading, medinan_scholarly_lineage).
narrative_ontology:fixing_cost_class(jurisprudential_method_kernel__maliki_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for resolving legal questions the Qur'an and Hadith text underdetermine, by treating the observed continuous practice of the community closest to the Prophet as itself a form of transmitted evidence — solving the problem of legal gaps without requiring speculative reasoning.
% TRANSFER_FUNCTION: Moves interpretive authority and the resulting fatwa/judicial revenue and prestige toward Medinan-lineage scholars and jurists who adopt their method, and away from jurists whose authority rests on independently transmitted practice or reasoned extension elsewhere in the early Islamic world.
% ABSENT_VOICES: Jurists from Kufa, Basra, and other early centers who preserved their own continuous transmission chains are structurally out-argued by a premise that privileges one city's practice as uniquely authoritative; they are present in the historical record disputing this but are not the ones whose disputation the Maliki kernel treats as dispositive.
% DISAPPEARANCE_RATIONALE: Maliki jurists would say the loss of 'amal ahl al-Madina as a source removes a genuine evidentiary channel for reconstructing early practice, degrading the accuracy of legal derivation. Rival schools and historians would say the legal conclusions reached via 'amal are substantially reachable through hadith and qiyas alone, and that the school's distinct identity — not the underlying law — is what would rearrange.
% FOUNDING_PROBLEM: Early legal questions arose faster than authenticated hadith could settle them, and different regions preserved different practices claiming Prophetic origin with no neutral arbiter; Medina's proximity to the Prophet's life and continuous scholarly presence offered one candidate solution to the authenticity gap.
% FOUNDING_PROBLEM_CORROBORATION: Maliki jurists and their historical chroniclers attest the problem remains live — hadith authentication is never fully closed. Hanafi and Shafi'i jurists, along with modern historians of early Islamic law working from manuscript and isnad-critical methods outside the Maliki tradition, attest that the authenticity gap is better addressed through formal hadith criticism and analogical method, and that geographic proximity does not itself establish transmission fidelity — this corroboration comes from outside the beneficiary lineage.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__maliki_reading, contested).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__maliki_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__maliki_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extraction (0.48, medium) reflects a genuine coordination function — resolving legal gaps through observed continuous practice — layered with an asymmetric authority claim that privileges one city's transmission chain over equally plausible chains elsewhere. Suppression (0.42) is moderate: the claim was contested from the outset by other early schools and was never enforced by state coercion in the way a legal code might be, but Maliki courts in regions under Maliki jurisdiction (notably later in North Africa and Andalusia) did institutionally exclude rival methodological premises. Theater ratio (0.28) is low-moderate: 'amal ahl al-Madina genuinely functioned as an evidentiary practice for generations of Medinan scholars, not merely as a rhetorical shield, though its evidentiary force for questions with no Medinan-specific practice record is thinner than its proponents claim.
 *
 * PERSPECTIVAL GAP:
 *   From the Medinan/Maliki seat, this is a rope: a genuine solution to the problem of underdetermined revelation, grounded in the best available evidence (continuous communal memory). From the Iraqi analogical jurist seat, this is closer to a tangled rope or even snare: coordination language dressing up a geography-based legitimacy monopoly that delegitimizes equally rigorous methods practiced elsewhere. The engine should compute these divergently from the same structural data — the claimed_type of tangled_rope reflects the authoring judgment that both a real coordination function AND an asymmetric extraction dynamic are present, not an attempt to average the two seats' views.
 *
 * DIRECTIONALITY LOGIC:
 *   Medinan scholarly lineage and Maliki jurists sit near the beneficiary end: their interpretive authority, judicial appointments, and school identity are constituted by the acceptance of this premise. Non-Medinan interpretive communities and Iraqi analogical jurists sit near the target end: their own transmission claims and reasoning methods are structurally discounted whenever the Maliki premise governs a dispute, even though their communities also plausibly preserved Prophetic practice through independent chains. Iraqi jurists retain some exit via mobility of their method across the wider Muslim world, which the derivation reflects with `mobile` rather than `trapped` exit options — the constraint is a competitive disadvantage within Maliki-governed jurisdictions, not a global cage.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — resolving legal questions faster than hadith authentication could settle them — remains partially live (hadith criticism is still an active, unresolved discipline), which cuts against treating this as pure mandatrophy. But the specific claim that ONLY Medina preserved practice with sufficient fidelity to ground a legal source is harder to sustain once modern isnad-critical scholarship demonstrates comparable transmission rigor in Kufa and elsewhere. The classification as tangled_rope rather than snare preserves the genuine coordination function (using observed practice as evidence) while still registering the asymmetric extraction (privileging one lineage's practice claims over structurally comparable ones).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    medinan_practice_authenticity_ambiguity,
    'Does ''amal ahl al-Madina reflect genuinely superior preservation of Prophetic practice, or does it reflect the contingent historical fact that Medina retained political and scholarly prominence after the Prophet''s death, making its practice more visible and better documented rather than more authentic?',
    'Comparative isnad-critical analysis across early legal centers (Medina, Kufa, Basra, Damascus) evaluating transmission chain density, chain corroboration, and independent attestation rather than geographic proximity alone.',
    'If Medinan practice is not demonstrably more reliably transmitted than comparable regional practices, the coordination-function claim weakens substantially and the constraint reads closer to snare (extraction dressed as superior evidentiary access); if the transmission advantage is real, the tangled_rope classification''s coordination component strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(medinan_practice_authenticity_ambiguity, conceptual, 'Whether Medinan evidentiary privilege reflects genuine transmission superiority or contingent historical prominence.').

omega_variable(
    kernel_reading_relation_ambiguity,
    'Does the Maliki reading''s privileging of communal practice logically foreclose the Hanafi reading''s privileging of reasoned analogical extension, or can a single jurist coherently hold that BOTH observed practice AND analogical reasoning are legitimate supplementary sources when hadith is silent?',
    'Examination of historical cases where Maliki and Hanafi jurists reached identical rulings through different methods — convergence would suggest the readings are compatible in outcome even where incompatible in justificatory structure.',
    'If the readings are logically compatible in practice despite differing justificatory premises, `coexists_with` is the correct relation to hanafi_reading; if the premises are mutually exclusive as foundational commitments (practice-as-evidence vs. reason-as-evidence cannot both ground the same ruling), a stronger `influences` or partial-foreclosure relation would be more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_relation_ambiguity, conceptual, 'Whether practice-based and reason-based source premises are logically compatible or mutually displacing.').

omega_variable(
    geographic_privilege_vs_theological_claim,
    'Is the geographic privileging of Medina a theological claim (the city itself has sanctified status making its practice normative) or an epistemic claim (Medina merely happened to have better evidence access)? These have different defensibility profiles.',
    'Textual analysis of early Maliki jurisprudential writing (e.g. al-Muwatta, later Maliki usul al-fiqh texts) to determine whether the justification offered is sanctity-based or evidence-based.',
    'A theological grounding is far less vulnerable to comparative-transmission counterevidence than an epistemic grounding; this affects whether the axiom `medinan_practice_evidentiary_primacy` should be read as deontological/theological or empirically_contingent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geographic_privilege_vs_theological_claim, conceptual, 'Whether Medinan privilege rests on sanctity or on evidentiary claims, with different vulnerability to counterevidence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__maliki_reading, 700, 900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t700, jurisprudential_method_kernel__maliki_reading, theater_ratio, 700, 0.15).
narrative_ontology:measurement_basis(juri_tr_t700, projected).
narrative_ontology:measurement(juri_tr_t740, jurisprudential_method_kernel__maliki_reading, theater_ratio, 740, 0.18).
narrative_ontology:measurement_basis(juri_tr_t740, projected).
narrative_ontology:measurement(juri_tr_t780, jurisprudential_method_kernel__maliki_reading, theater_ratio, 780, 0.21).
narrative_ontology:measurement_basis(juri_tr_t780, projected).
narrative_ontology:measurement(juri_tr_t820, jurisprudential_method_kernel__maliki_reading, theater_ratio, 820, 0.24).
narrative_ontology:measurement_basis(juri_tr_t820, projected).
narrative_ontology:measurement(juri_tr_t860, jurisprudential_method_kernel__maliki_reading, theater_ratio, 860, 0.26).
narrative_ontology:measurement_basis(juri_tr_t860, projected).
narrative_ontology:measurement(juri_tr_t900, jurisprudential_method_kernel__maliki_reading, theater_ratio, 900, 0.28).
narrative_ontology:measurement_basis(juri_tr_t900, projected).

% Extraction over time
narrative_ontology:measurement(juri_be_t700, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 700, 0.32).
narrative_ontology:measurement_basis(juri_be_t700, projected).
narrative_ontology:measurement(juri_be_t740, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 740, 0.38).
narrative_ontology:measurement_basis(juri_be_t740, projected).
narrative_ontology:measurement(juri_be_t780, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 780, 0.42).
narrative_ontology:measurement_basis(juri_be_t780, projected).
narrative_ontology:measurement(juri_be_t820, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 820, 0.45).
narrative_ontology:measurement_basis(juri_be_t820, projected).
narrative_ontology:measurement(juri_be_t860, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 860, 0.47).
narrative_ontology:measurement_basis(juri_be_t860, projected).
narrative_ontology:measurement(juri_be_t900, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 900, 0.48).
narrative_ontology:measurement_basis(juri_be_t900, projected).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t700, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 700, 0.3).
narrative_ontology:measurement_basis(juri_su_t700, projected).
narrative_ontology:measurement(juri_su_t740, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 740, 0.33).
narrative_ontology:measurement_basis(juri_su_t740, projected).
narrative_ontology:measurement(juri_su_t780, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 780, 0.36).
narrative_ontology:measurement_basis(juri_su_t780, projected).
narrative_ontology:measurement(juri_su_t820, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 820, 0.39).
narrative_ontology:measurement_basis(juri_su_t820, projected).
narrative_ontology:measurement(juri_su_t860, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 860, 0.41).
narrative_ontology:measurement_basis(juri_su_t860, projected).
narrative_ontology:measurement(juri_su_t900, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 900, 0.42).
narrative_ontology:measurement_basis(juri_su_t900, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__maliki_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jurisprudential_method_kernel__maliki_reading, 0.1).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel__hanafi_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel__shafii_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel__hanbali_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of jurisprudential_method_kernel, decomposed per the ε-invariance principle rather than treated as a single 'Islamic jurisprudence' constraint with an observable-dependent ε. Each reading names a different evidentiary source as primary (Medinan practice, analogical reasoning, hierarchical hadith transmission, literalist consensus) and carries a distinct beneficiary lineage and distinct extraction profile. The Shafi'i reading is marked as receiving `influences` from this reading because al-Shafi'i's methodological standardization was partly a direct response to perceived inconsistency in earlier practice-based and preference-based methods, including the Maliki approach — the Shafi'i hierarchy's legitimacy conditions shifted in reaction to disputes this reading generated, without the Shafi'i reading being logically foreclosed by it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
