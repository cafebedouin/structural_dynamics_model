% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__maliki_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:coordination_type/2,
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
 *   human_readable: Maliki Jurisprudential Method: 'Amal Ahl al-Madina
 *   domain: legal/religious/institutional
 *
 * SUMMARY:
 *   This constraint instantiates the Maliki reading of the Islamic
 *   jurisprudential method kernel, which holds that law derives not only from
 *   Qur'an and Hadith but from the living tradition ('amal ahl al-Madina) of
 *   the Medinan community, on the premise that Medina most faithfully
 *   preserved Prophetic practice. The constraint functions as a
 *   methodological gate: it coordinates Islamic legal derivation by providing
 *   a stable, geographically anchored continuity mechanism, while
 *   asymmetrically extracting epistemic authority from non-Medinan jurists
 *   and regional custom, concentrating legitimacy in the Medinan scholarly
 *   lineage. It is one of four major readings of the same kernel, alongside
 *   Hanafi, Shafi'i, and Hanbali methods. The authored claim is tangled_rope:
 *   genuine coordination in legal stability, but with identifiable extractive
 *   asymmetry privileging one lineage's customs over others' equal claims to
 *   authenticity.
 *
 * KEY AGENTS:
 *   - medinan_scholarly_lineage: Primary beneficiary and agenda-setter (institutional/generational/identity_locked) â inherits and monopolizes the authority of Medinan practice.
 *   - non_medinan_jurists: Primary target (organized/generational/constrained) â bear the epistemic cost of subordinated interpretive status.
 *   - state_judiciaries: Secondary beneficiary (institutional/biographical/constrained) â benefit from methodological stability without controlling the source.
 *   - regional_custom_practitioners: Excluded victim (moderate/generational/trapped) â living customary law from outside Medina is barred from source-status.
 *   - comparative_legal_historians: Analytical observer (analytical/civilizational/analytical) â traces the institutional history and geographic economy of the method.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__maliki_reading, 0.58).
domain_priors:suppression_score(jurisprudential_method_kernel__maliki_reading, 0.72).
domain_priors:theater_ratio(jurisprudential_method_kernel__maliki_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__maliki_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__maliki_reading, "Maliki Jurisprudential Method: 'Amal Ahl al-Madina").
narrative_ontology:topic_domain(jurisprudential_method_kernel__maliki_reading, "legal/religious/institutional").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__maliki_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__maliki_reading, 'c90b0330-d136-4eed-b2a6-c1464e39e9ec').
narrative_ontology:cs_kernel_codification('c90b0330-d136-4eed-b2a6-c1464e39e9ec', fixed_text).
narrative_ontology:cs_authority_grounding('c90b0330-d136-4eed-b2a6-c1464e39e9ec', lineage).
narrative_ontology:cs_interpretation_layer_present('c90b0330-d136-4eed-b2a6-c1464e39e9ec').
narrative_ontology:cs_reading_relation('c90b0330-d136-4eed-b2a6-c1464e39e9ec', jurisprudential_method_kernel__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('c90b0330-d136-4eed-b2a6-c1464e39e9ec', jurisprudential_method_kernel__shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('c90b0330-d136-4eed-b2a6-c1464e39e9ec', jurisprudential_method_kernel__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('c90b0330-d136-4eed-b2a6-c1464e39e9ec', foundational, amal_ahl_al_madina_legislative_authority).
narrative_ontology:cs_axiom_status(amal_ahl_al_madina_legislative_authority, holdable).
narrative_ontology:cs_axiom_grounding('c90b0330-d136-4eed-b2a6-c1464e39e9ec', amal_ahl_al_madina_legislative_authority, empirically_contingent).
narrative_ontology:cs_axiom('c90b0330-d136-4eed-b2a6-c1464e39e9ec', secondary, geographic_epistemic_hierarchy).
narrative_ontology:cs_axiom_status(geographic_epistemic_hierarchy, holdable).
narrative_ontology:cs_axiom_grounding('c90b0330-d136-4eed-b2a6-c1464e39e9ec', geographic_epistemic_hierarchy, conventional).
narrative_ontology:cs_reference_frame('c90b0330-d136-4eed-b2a6-c1464e39e9ec', medinan_legal_continuity).
narrative_ontology:cs_drift_state('c90b0330-d136-4eed-b2a6-c1464e39e9ec', contemporary_salafi_challenge, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('c90b0330-d136-4eed-b2a6-c1464e39e9ec', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__maliki_reading, medinan_scholarly_lineage).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__maliki_reading, state_judiciaries).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__maliki_reading, non_medinan_jurists).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__maliki_reading, regional_custom_practitioners).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__maliki_reading, amal_ahl_al_madina_authenticity).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__maliki_reading, medinan_practice_preservation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Inherit and transmit the living legal tradition of Medina, grounding their authority in the claim that the Prophetic city preserved the sunna most faithfully. They authenticate legal norms by reference to continuous local practice, train successive jurists, and adjudicate which customs carry binding force. Exit is unthinkable because their professional and spiritual identity is fused with this geographic lineage.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, medinan_scholarly_lineage, beneficiary,
    institutional, generational, identity_locked, universal).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__maliki_reading, medinan_scholarly_lineage, agenda_setter).

% Derive law from textual sources and regional reasoning, but within Maliki methodology their interpretive claims are subordinated to Medinan reports. They must accept the epistemic priority of a city they do not inhabit, or abandon Maliki affiliation and lose institutional standing. Their customary knowledge is treated as supplemental at best.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, non_medinan_jurists, payer,
    organized, generational, constrained, continental).

% Apply Maliki-derived rulings in state courts and administrative tribunals, relying on the methodological stability of the school to produce predictable legal outcomes. They benefit from a ready-made framework but do not control which sources are valid; switching to another school would require retraining judges and rewriting codes.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, state_judiciaries, beneficiary,
    institutional, biographical, constrained, national).

% Maintain living customary law outside Medina that often predates or parallels Islamic legal development. Their practices are structurally barred from independent source-status in Maliki fiqh; they would argue for the legitimacy of local continuity if admitted to the methodological conversation, but they are not.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, regional_custom_practitioners, excluded,
    moderate, generational, trapped, regional).

% Study the institutional development of the Maliki school, the political economy of Medinan scholarly authority, and the comparative evidence for unique Prophetic preservation in Medina. They do not participate in the legal system but provide the analytical frame in which the constraint's historical claims are tested.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, comparative_legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jurisprudential_method_kernel__maliki_reading, medinan_scholarly_lineage).
narrative_ontology:fixing_cost_class(jurisprudential_method_kernel__maliki_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, continuous mechanism for deriving Islamic legal rulings by anchoring interpretive authority in the lived practice of the Prophetic city, reducing arbitrary individual reasoning and ensuring geographical continuity with the Prophet's community.
% TRANSFER_FUNCTION: Moves epistemic authority and source-status from non-Medinan regional jurists and their customs to the Medinan scholarly lineage, which alone authenticates legal norms by reference to local practice.
% ABSENT_VOICES: Non-Medinan regional jurists and customary law practitioners whose interpretive claims to equal authenticity are structurally excluded from the source-hierarchy; they would argue that piety and legal competence are not geographically concentrated in Medina alone.
% DISAPPEARANCE_RATIONALE: If the methodological priority of Medinan practice vanished, Maliki jurisprudence would lose its primary source-axis; legal training, judicial appointment criteria, and the genealogical authority of Medinan scholars would reorganize around hadith-alone or regional-custom methodologies, redistributing institutional power across the Muslim world.
% FOUNDING_PROBLEM: The need for a reliable, continuous source of law beyond Qur'an and isolated hadith reports that could resist arbitrary individual reasoning and anchor legal norms in demonstrable continuity with the Prophet's actual community.
% FOUNDING_PROBLEM_CORROBORATION: Medinan scholars attest the problem is live, citing the unbroken chain of practice. Hanafi and Shafi'i jurists from outside the beneficiary set attest that the problem of legal derivation has been substantially solved by text and reason, and that the Medinan privilege now functions as institutional extraction masquerading as historical fidelity; external historians of Islamic law note the political economy of Abbasid and later states in privileging Medinan continuity.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__maliki_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__maliki_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__maliki_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jurisprudential_method_kernel__maliki_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__maliki_reading, 0.58, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.58) is medium: the coordination function (legal continuity, anti-arbitrary stability) is genuine and historically significant, but the same structure concentrates source-authority in one geographic lineage, which collects epistemic rents through exclusive authentication rights. Suppression (0.72 at interval end) is high because the constraint's persistence requires the active methodological exclusion of non-Medinan custom and the institutional enforcement of Medinan priority in curricula, judicial appointment, and fatwa authority. Theater_ratio (0.50) rises over the interval: modern codification and the defensive reaction to Salafi hadith-only movements increase the share of performative maintenance relative to living practice. Accessibility_collapse (0.70) is high because once inside the Maliki framework, alternatives (non-Medinan custom as primary source) are nearly unthinkable; resistance (0.55) reflects persistent challenges from other schools and modernist movements that prevent total closure.
 *
 * PERSPECTIVAL GAP:
 *   From the Medinan scholarly seat, the arrangement is continuous fidelity to the Prophetic community; from the non-Medinan jurist seat, it is a geographically arbitrary monopoly on source-status. The engine computes this divergence from the same structural data. The gap is cross-generational: the lineage's identity fusion across centuries contrasts with the constrained mobility of jurists born into Maliki-dominant regions who cannot easily switch methodological frameworks without losing institutional standing.
 *
 * DIRECTIONALITY LOGIC:
 *   The Medinan scholarly lineage sits at the beneficiary pole: the constraint subsidizes their exclusive epistemic authority and institutional role (low d). Non-Medinan jurists and regional custom practitioners sit at the target pole: the constraint extracts from their interpretive legitimacy by structurally devaluing non-Medinan claims (high d). State judiciaries sit near symmetric: they receive coordination benefits (stable law) but are constrained to apply a methodology they do not control. The directionality follows from the structural declarations: beneficiaries/victims plus exit options (identity_locked for the lineage, constrained/trapped for the targets).
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the constraint as pure coordination (rope) â which would ignore the epistemic extraction from non-Medinan claims â or as pure extraction (snare) â which would ignore the genuine legal stability and continuity the Medinan anchor provided for centuries. The founding problem (how to anchor law beyond isolated texts) was live in the formative period; its status is contested because modern textual and comparative historical methods suggest the problem is now solvable through broader hadith verification, yet the Medinan privilege persists. The R5 mismatch (contested status + world_rearranges disappearance) flags the mandatrophy risk: the arrangement persists beyond its obsolescence horizon, drifting toward higher theater and extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    medinan_historical_uniqueness,
    'Is the claim that Medina uniquely preserved Prophetic practice an empirically recoverable historical fact, or a post-hoc legitimating narrative constructed by the Medinan scholarly class?',
    'Archaeological and textual analysis of early Medinan legal practice compared to Meccan, Kufan, and Syrian contemporaneous practice.',
    'If Medinan practice is not uniquely preservative, the constraint''s coordination function collapses into pure lineage extraction; if it is, the extraction is the price of genuine historical continuity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medinan_historical_uniqueness, empirical, 'Whether Medinan uniqueness is historically verifiable or constructed.').

omega_variable(
    custom_extraction_boundary,
    'At what point does the methodological reliance on living custom become indistinguishable from the entrenchment of a regional scholarly cartel?',
    'Comparative analysis of legal outcomes: do Maliki rulings diverge systematically from textual evidence in ways that advantage Medinan institutional continuity over textual fidelity?',
    'Would clarify whether the constraint sits closer to rope or snare on the spectrum; high systematic divergence suggests the coordination story is cover.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(custom_extraction_boundary, conceptual, 'Boundary between coordination and cartelization in custom-based law.').

omega_variable(
    sibling_reading_structural_pressure,
    'Does the Maliki reading create structural downstream pressure that influences or forecloses Hanafi, Shafii, and Hanbali methodological claims?',
    'Historical analysis of institutional patronage, judicial appointment patterns, and cross-school curriculum adoption in regions where Maliki methodology became dominant.',
    'Would determine whether the relation is influence (structural pressure) or coexistence (parallel live options); affects network coupling analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_structural_pressure, empirical, 'Structural relationship from Maliki reading to sibling kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__maliki_reading, 0, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maliki_jurisprudence_tr_t0, jurisprudential_method_kernel__maliki_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(maliki_jurisprudence_tr_t200, jurisprudential_method_kernel__maliki_reading, theater_ratio, 200, 0.15).
narrative_ontology:measurement(maliki_jurisprudence_tr_t400, jurisprudential_method_kernel__maliki_reading, theater_ratio, 400, 0.2).
narrative_ontology:measurement(maliki_jurisprudence_tr_t600, jurisprudential_method_kernel__maliki_reading, theater_ratio, 600, 0.25).
narrative_ontology:measurement(maliki_jurisprudence_tr_t800, jurisprudential_method_kernel__maliki_reading, theater_ratio, 800, 0.3).
narrative_ontology:measurement(maliki_jurisprudence_tr_t1000, jurisprudential_method_kernel__maliki_reading, theater_ratio, 1000, 0.38).
narrative_ontology:measurement(maliki_jurisprudence_tr_t1200, jurisprudential_method_kernel__maliki_reading, theater_ratio, 1200, 0.45).
narrative_ontology:measurement(maliki_jurisprudence_tr_t1400, jurisprudential_method_kernel__maliki_reading, theater_ratio, 1400, 0.5).

% Extraction over time
narrative_ontology:measurement(maliki_jurisprudence_be_t0, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(maliki_jurisprudence_be_t200, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 200, 0.4).
narrative_ontology:measurement(maliki_jurisprudence_be_t400, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 400, 0.5).
narrative_ontology:measurement(maliki_jurisprudence_be_t600, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 600, 0.55).
narrative_ontology:measurement(maliki_jurisprudence_be_t800, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 800, 0.58).
narrative_ontology:measurement(maliki_jurisprudence_be_t1000, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 1000, 0.62).
narrative_ontology:measurement(maliki_jurisprudence_be_t1200, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 1200, 0.65).
narrative_ontology:measurement(maliki_jurisprudence_be_t1400, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 1400, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(maliki_jurisprudence_su_t0, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(maliki_jurisprudence_su_t200, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 200, 0.45).
narrative_ontology:measurement(maliki_jurisprudence_su_t400, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 400, 0.55).
narrative_ontology:measurement(maliki_jurisprudence_su_t600, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 600, 0.6).
narrative_ontology:measurement(maliki_jurisprudence_su_t800, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 800, 0.58).
narrative_ontology:measurement(maliki_jurisprudence_su_t1000, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 1000, 0.65).
narrative_ontology:measurement(maliki_jurisprudence_su_t1200, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 1200, 0.7).
narrative_ontology:measurement(maliki_jurisprudence_su_t1400, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 1400, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__maliki_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
