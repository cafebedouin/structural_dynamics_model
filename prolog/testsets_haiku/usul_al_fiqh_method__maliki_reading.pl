% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__maliki_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: usul_al_fiqh_method__maliki_reading
 *   human_readable: Maliki Jurisprudential Method: Integration of Medinan Practice, Public Interest, and Custom
 *   domain: religious/legal/institutional
 *
 * SUMMARY:
 *   The Maliki school of Islamic jurisprudence (usul al-fiqh) establishes a
 *   distinctive methodological framework that elevates Medinan practice
 *   ('amal ahl al-Madina), public interest reasoning (maslaha mursala), and
 *   customary law ('urf) to independent evidentiary weight alongside textual
 *   sources (Quran and hadith). This constraint is ONE READING of the
 *   contested kernel 'usul_al_fiqh_method' — the foundational question of how
 *   Islamic law is derived. This reading emphasizes flexibility, regional
 *   variation, and integration of local norms. It contrasts with Hanbali
 *   textualism (which minimizes discretion), Shafi'i systematization (which
 *   imposes rigid hierarchy), and Hanafi analogism (which expands reasoning
 *   but less systematically). The constraint coordinates Islamic legal
 *   authority with regional practice, permitting law to adapt to diverse
 *   territories while maintaining Islamic legitimacy. The claim/metric gap is
 *   intentional: this is structured as ROPE (genuine coordination of
 *   universal principle with local practice) while the metrics reflect
 *   moderate extractiveness (qadis and regional authorities gain power to
 *   override stricter readings) and low suppression (the method remains
 *   internally debated and competing schools persist alongside it).
 *
 * KEY AGENTS:
 *   - Maliki juridical school: agenda_setter, institutional power, civilizational horizon — maintains the methodological framework through transmission chains
 *   - Medinan practitioners: beneficiary, organized power, generational horizon — gain legitimacy for regional practice
 *   - Regional customary authorities: beneficiary, powerful, generational horizon — custom integrates into formal jurisprudence
 *   - Textualist universalists: payer, powerful, civilizational horizon — universalist approaches subordinated to regional variation
 *   - Qadis and judges: beneficiary/payer dual role, institutional power, biographical horizon — gain discretion but bear complexity
 *   - Hanbali textualists: excluded, institutional power, civilizational horizon — argue for textual restrictiveness
 *   - Shafi'i systematizers: excluded, institutional power, civilizational horizon — systematize rigid source hierarchy
 *   - Hanafi analogists: observer, institutional power, civilizational horizon — occupy neighboring middle position
 *   - Comparative legal scholars: observer, analytical power, generational horizon — study as historical and structural object
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__maliki_reading, 0.38).
domain_priors:suppression_score(usul_al_fiqh_method__maliki_reading, 0.21).
domain_priors:theater_ratio(usul_al_fiqh_method__maliki_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, suppression_requirement, 0.21).
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__maliki_reading, rope).
narrative_ontology:human_readable(usul_al_fiqh_method__maliki_reading, "Maliki Jurisprudential Method: Integration of Medinan Practice, Public Interest, and Custom").
narrative_ontology:topic_domain(usul_al_fiqh_method__maliki_reading, "religious/legal/institutional").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__maliki_reading, '5999fd3f-dd6f-47c5-80de-33d7fc9fc63d').
narrative_ontology:cs_kernel_codification('5999fd3f-dd6f-47c5-80de-33d7fc9fc63d', fixed_text).
narrative_ontology:cs_authority_grounding('5999fd3f-dd6f-47c5-80de-33d7fc9fc63d', lineage).
narrative_ontology:cs_interpretation_layer_present('5999fd3f-dd6f-47c5-80de-33d7fc9fc63d').
narrative_ontology:cs_reading_relation('5999fd3f-dd6f-47c5-80de-33d7fc9fc63d', usul_al_fiqh_method__hanbali_reading, coexists_with).
narrative_ontology:cs_reading_relation('5999fd3f-dd6f-47c5-80de-33d7fc9fc63d', usul_al_fiqh_method__shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('5999fd3f-dd6f-47c5-80de-33d7fc9fc63d', usul_al_fiqh_method__hanafi_reading, coexists_with).
narrative_ontology:cs_axiom('5999fd3f-dd6f-47c5-80de-33d7fc9fc63d', foundational, customary_practice_independent_source).
narrative_ontology:cs_axiom_status(customary_practice_independent_source, holdable).
narrative_ontology:cs_axiom_grounding('5999fd3f-dd6f-47c5-80de-33d7fc9fc63d', customary_practice_independent_source, deontological).
narrative_ontology:cs_axiom('5999fd3f-dd6f-47c5-80de-33d7fc9fc63d', foundational, public_interest_unrestricted_by_text).
narrative_ontology:cs_axiom_status(public_interest_unrestricted_by_text, holdable).
narrative_ontology:cs_axiom_grounding('5999fd3f-dd6f-47c5-80de-33d7fc9fc63d', public_interest_unrestricted_by_text, instrumental).
narrative_ontology:cs_reference_frame('5999fd3f-dd6f-47c5-80de-33d7fc9fc63d', medinan_legal_tradition_as_source).
narrative_ontology:cs_drift_state('5999fd3f-dd6f-47c5-80de-33d7fc9fc63d', contemporary_nation_state_legal_centralization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5999fd3f-dd6f-47c5-80de-33d7fc9fc63d', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, medinan_practitioners).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, regional_customary_authorities).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, contextualized_legal_interpretation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, qadis_and_judges).
narrative_ontology:constraint_victim(usul_al_fiqh_method__maliki_reading, textualist_universalists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__maliki_reading, qadis_and_judges).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__maliki_reading, regional_legal_pluralism_doctrine).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__maliki_reading, customary_law_legitimacy_in_islamic_jurisprudence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Establishes and transmits the interpretive method through scholarly transmission chains (isnad), jurisprudential literature (al-mudawwana, al-muwatta), and pedagogical practice. Maintains the method's coherence by resolving internal disputes about scope of maslaha and integration of 'urf. The school's authority depends on the method remaining internally coherent and responsive to regional legal variation.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, maliki_juridical_school, agenda_setter,
    institutional, civilizational, analytical, continental).

% Benefit from legal judgments grounded in Medinan practice ('amal ahl al-Madina). Their inherited legal norms receive independent evidentiary weight in formal Islamic jurisprudence rather than being treated as deviations from text or errors to be corrected. This elevation of local practice to source status permits law to function within their communities without requiring abandonment of established customs.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, medinan_practitioners, beneficiary,
    organized, generational, mobile, local).

% Integrate their customary norms ('urf) into formal jurisprudential reasoning when such customs do not contradict explicit textual sources. Regional governors, market judges, community councils, and local leaders can justify their legal decisions by appeal to established custom, which gains Islamic jurisprudential legitimacy through the Maliki method. This permits regional law-making to operate with Islamic authority.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, regional_customary_authorities, beneficiary,
    powerful, generational, constrained, regional).

% Bear the constraint's cost by having their universalist, text-centric jurisprudence subordinated to regional variation and discretionary reasoning. They argue that maslaha mursala and 'urf dilute textual authority and undermine legal uniformity across Islamic territories. Their jurisprudential competitors (Hanbali, Shafi'i schools) impose tighter textual restrictions; they experience the Maliki method's flexibility as a loss of exclusive interpretive authority.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, textualist_universalists, payer,
    powerful, civilizational, mobile, global).

% Gain substantial discretion to ground their legal judgments in local established practice ('amal) and community interest (maslaha) when textual sources do not clearly govern. They also bear the burden of this discretion: managing the complexity of integrating custom and interest with text, defending decisions that may diverge from stricter interpretations, navigating disagreement with qadis from competing jurisprudential schools, and ensuring their reasoning remains grounded in Islamic principle rather than pure preference. Their authority depends on mastering both the Maliki method and the region's established legal norms.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, qadis_and_judges, beneficiary,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__maliki_reading, qadis_and_judges, payer).

% Are structurally positioned in opposition to this constraint: their method minimizes qiyas (analogical reasoning) and maslaha (public interest) in favor of direct, restrictive application of textual sources. They would argue vehemently that the Maliki method's elevation of custom and discretionary reasoning undermines the Quran and hadith's binding authority. Their voices are excluded from Maliki school deliberation but remain institutionally present in competing jurisprudential schools and in regions where Hanbali law prevails.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, hanbali_textualists, excluded,
    institutional, civilizational, mobile, global).

% Have systematized usul al-fiqh as a meta-discipline with a rigid source hierarchy (Quran > authenticated Sunnah > ijma of Companions > qiyas) that leaves minimal room for maslaha mursala or 'urf independent of textual constraint. They would argue that the Maliki method's flexibility lacks the epistemic discipline and logical rigor of their systematized hierarchy. Their methodological commitments exclude them from endorsing Maliki reasoning. They are excluded from Maliki deliberation but influence the broader jurisprudential discourse.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, shafii_systematizers, excluded,
    institutional, civilizational, mobile, global).

% Occupy a methodologically neighboring position: they expand the scope of qiyas (analogical reasoning) and istihsan (juristic preference) to permit more discretion where textual sources are silent, which partially parallels the Maliki allowance for maslaha and 'urf. However, their discretion is grounded in reasoning-from-text (analogy) rather than in custom itself. They observe the Maliki constraint as a comparable but distinct approach with overlapping but different methodological commitments. Neither direct competitor nor structural dependency.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, hanafi_analogists, observer,
    institutional, civilizational, analytical, global).

% Study the jurisprudential method as a historical and comparative phenomenon. They analyze how the Maliki reading permits legal pluralism and contextual adaptation while maintaining Islamic authority as its grounding. They observe the constraint as a structural feature of Islamic jurisprudence worthy of scholarly analysis, not as a position to defend or reject from within the tradition.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, comparative_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Islamic legal authority with regional customary practices by establishing interpretive sources (Medinan practice, maslaha mursala, 'urf) that permit law to vary by locale while remaining grounded in Islamic tradition. Solves the problem of reconciling universal Islamic principles with locally embedded legal norms: permits regions to maintain their established practices and judges to reason about public interest without requiring all law to derive from literal textual sources or global consensus.
% TRANSFER_FUNCTION: Transfers jurisprudential authority FROM universalist textualism TO regional custom and discretionary reasoning. Qadis, regional customary authorities, and community leaders gain the power to justify legal decisions by appeal to local practice and public interest rather than requiring all authority to come from text or centralized jurisprudential consensus. Textualists lose exclusive interpretive authority in favor of distributed, region-responsive legal reasoning.
% ABSENT_VOICES: Hanbali textualists and Shafi'i systematizers are structurally excluded from the Maliki deliberation — they would argue that the method dilutes textual fidelity and introduces unacceptable legal inconsistency across regions. Scholars in regions where Hanbali or Shafi'i jurisprudence dominates would have fundamentally different claims about what Islamic law permits. Strictest textualist voices that reject maslaha and 'urf entirely are not part of the deliberative conversation within the Maliki school.
% DISAPPEARANCE_RATIONALE: If this jurisprudential method and its intellectual transmission vanished overnight, the regions where Maliki law governed (historically North Africa, Al-Andalus, parts of West Africa, and East Africa) would be forced to reorganize around competing jurisprudential methods (Hanbali, Shafi'i, Hanafi) or abandon Islamic jurisprudential legitimacy for their customary legal systems. Judges would lack the framework validating local adaptation. Communities would face a choice: adopt stricter jurisprudential discipline or develop non-Islamic legal authority for their customary law.
% FOUNDING_PROBLEM: Early Islamic jurisprudence faced a crisis of scale and plurality: the Quran and authenticated hadith do not address the full range of legal questions a functioning society must resolve across diverse territories, and different regions had pre-Islamic customary practices (legal norms, market customs, family practices) that communities wished to preserve within an Islamic framework rather than abandon. The Maliki method developed to solve this by legitimating regional practice and reasoned judgment about public interest alongside textual sources, permitting Islam to govern law at scale across diverse territories while preserving local legal variation.
% FOUNDING_PROBLEM_CORROBORATION: Islamic legal historians and comparative law scholars (non-beneficiary voices) confirm that the foundational problem — reconciling universal Islamic principles with regional legal practice — remains live in Islamic-majority jurisdictions and in diaspora communities. The problem persists wherever Islamic and customary law coexist. However, Hanbali and Shafi'i schools attest that the problem is better solved through stricter textual discipline and global scholarly consensus (ijma), not regional variation — their corroboration comes from within the methodological contest, not from outside the jurisprudential framework.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__maliki_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__maliki_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__maliki_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(usul_al_fiqh_method__maliki_reading, 'none', 1).

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
 *   Extractiveness is moderate (0.38 at interval end) because the constraint privileges regional practice and judge discretion over textual universalism, which redistributes jurisprudential authority from centralized textualists to regional authorities and qadis. This is real power transfer, not zero-sum coordination. The metric shows an initial rise (0.22 to 0.38 from t=0 to t=700) as the method matured and gained institutional consolidation in North African and Al-Andalusian regions, then stabilizes (0.36 to 0.38 from t=700 to t=1400) as the method's reach plateaued and competing schools persisted. Suppression is low (0.21) because the method remains internally contested — competing jurisprudential schools continue to operate and argue against the Maliki approach. The method does not require active suppression of alternatives; it coexists with them. Theater ratio is very low (0.12) because the jurisprudential method is genuinely functional: judges actually use maslaha and 'urf reasoning to resolve disputes, not as performative cover. The method is not maintained theatrically. Resistance is high (0.62) because Hanbali and Shafi'i schools mount active intellectual resistance to the Maliki elevation of discretion and custom; the method's persistence requires continuous scholarly defense. Accessibility collapse is moderate (0.45) because the method remains accessible to competing interpretations — alternatives exist, and exit from the Maliki framework to Hanbali or Shafi'i jurisprudence is possible for judges and communities, though path-dependent and costly.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (Maliki school) and beneficiary seats (medinan practitioners, regional authorities) perceive this constraint as enabling coordination: it permits Islamic law to function across diverse regions without requiring all norms to derive from literal text. Textualist universalists perceive the same structure as extraction: qadis and regions gain power to override stricter readings, which they experience as a loss of interpretive authority. The engine should compute different types from these seats. From the Maliki school's and regional authorities' structural position, the constraint solves a coordination problem and distributes beneficially. From textualists' position (high power, threatened by jurisdictional expansion of discretion), the constraint extracts authority. The divided perception reflects genuine structural asymmetry: some actors are moved toward autonomy (regions, judges), others toward subordination (universal textualists).
 *
 * DIRECTIONALITY LOGIC:
 *   Medinan practitioners and regional customary authorities are structural beneficiaries (d near 0.0): they gain jurisprudential legitimacy for their practices. The Maliki school benefits as well (d near 0.15): it maintains authority as the method's custodian and expands influence by permitting regional adaptation. Textualist universalists are structural targets (d near 0.85): their exclusive claim on Islamic legal authority is subordinated to regional variation and discretion. Qadis occupy the middle (d near 0.5): they benefit from discretion but bear the burden of managing complexity and navigating disagreement with stricter schools. The directionality derives from beneficiary/victim structure: those whose norms or power expand under this constraint sit at low d; those whose universalist authority is diluted sit at high d.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy because the founding problem (reconciling universal Islamic principles with regional legal practice) remains genuinely live. Maliki scholars, qadis, and regional authorities continue to use the method to resolve disputes where text is silent or where custom would improve application. The method has not become purely theatrical — judges actually consult established practices and reason about public interest when making legal decisions. The constraint is not a dead mandate clinging to institutional inertia. However, there is a tension worth flagging: in regions where legal centralization advanced and uniform codes were imposed (late Ottoman period, modern nation-states), the practical space for regional variation contracted, though the jurisprudential method remained formally intact. The measurement series shows extractiveness rising from t=0 to t=700 (the method's institutional consolidation) then stabilizing, which would be consistent with the method becoming more entrenched but not more functional. This is not mandatrophy yet — the method remains in use — but the trend suggests monitoring whether the constraint becomes increasingly formulaic as legal centralization proceeds.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    custom_vs_textual_contradiction,
    'When established custom (''urf) appears to contradict a textual source, how is the conflict resolved in practice within the Maliki framework?',
    'Scholarly and judicial rulings across Maliki regions can be analyzed to establish patterns of precedence. Historical legal disputes where this tension arose and how judges resolved them provide evidence.',
    'If textual sources consistently override custom in practice, the Maliki method''s elevation of ''urf is more theoretical than functional, shifting classification toward piton or theatrical constraint. If custom genuinely modifies textual application, the method is functionally integrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(custom_vs_textual_contradiction, empirical, 'Whether custom truly carries independent weight when it conflicts with text, or whether the method''s flexibility is limited by hidden textual priority.').

omega_variable(
    maslaha_mursala_scope_ambiguity,
    'What counts as ''public interest'' (maslaha mursala) that justifies departure from textual strictness? How is maslaha bounded to prevent arbitrary judge-made law?',
    'Documented jurisprudential discussion of what maslaha encompasses and what limits apply. Analysis of judicial decisions invoking maslaha to establish patterns of constraint.',
    'If maslaha is narrowly bounded by recognized interests and subject to community standards, it remains a source grounded in Islamic reasoning. If maslaha becomes open-ended justification for judge preference, the method drifts toward pure judicial discretion, increasing extractiveness and extractive behavior.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maslaha_mursala_scope_ambiguity, conceptual, 'Whether maslaha mursala is a structured source or a blank check for judicial innovation.').

omega_variable(
    regional_variation_coexistence,
    'Can different regions applying the Maliki method to the same legal question produce divergent outcomes without undermining the method''s coherence, or does the method require some uniformity?',
    'Comparative analysis of Maliki jurisprudence across regions (North Africa, Al-Andalus, Egypt) to establish whether variation is systematic or whether regional differences are treated as errors to be corrected.',
    'If variation is systematic and embraced, the method is pluralist and fully integrative. If variation is tolerated but regretted, the method aspires to uniformity and treats regional practice as a constraint to be accommodated, not endorsed — shifting closer to universalism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_variation_coexistence, empirical, 'Whether the method is structurally pluralist or universalist with accommodations for practice.').

omega_variable(
    institutional_suppression_under_centralization,
    'As Islamic legal systems became centralized in nation-states and modern codes, did the practical space for Maliki-style regional variation contract even while the jurisprudential method remained formally transmitted?',
    'Historical analysis of legal codification and centralization in Maliki-influenced regions (Morocco, Algeria, Libya, Tunisia) to track when and how regional customary authority was subordinated to uniform codes.',
    'If centralization eroded the method''s functional scope while the method persisted as intellectual tradition, the constraint approaches mandatrophy: formally alive but practically inert. This would support reclassification toward piton or theater-heavy constraint after a certain point.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_suppression_under_centralization, empirical, 'Whether legal centralization has made the Maliki method increasingly theatrical even as it remains intellectually transmitted.').

omega_variable(
    kernel_reading_foreclosure,
    'Do the Hanbali and Maliki readings of the jurisprudential kernel logically foreclose each other (no single framework could hold both), or do they coexist as competing but live positions?',
    'Philosophical analysis of the core premises: Hanbali minimization of discretion (textual sources govern directly) vs. Maliki expansion of maslaha (reasoned judgment supplements text). If a party could coherently adopt both premises in sequence or context-dependent, they coexist; if they directly contradict, they foreclose.',
    'If readings foreclose each other, the kernel is one of genuine logical closure where only one reading can be true within a framework. If they coexist, the kernel admits multiple live interpretations and the contest is political/institutional, not logical. This affects how the engine should model the constraint''s persistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether the kernel''s readings are logically foreclosed or institutionally coexistent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__maliki_reading, 0, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t0, usul_al_fiqh_method__maliki_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(usul_tr_t0, observed).
narrative_ontology:measurement(usul_tr_t200, usul_al_fiqh_method__maliki_reading, theater_ratio, 200, 0.09).
narrative_ontology:measurement_basis(usul_tr_t200, observed).
narrative_ontology:measurement(usul_tr_t400, usul_al_fiqh_method__maliki_reading, theater_ratio, 400, 0.1).
narrative_ontology:measurement_basis(usul_tr_t400, observed).
narrative_ontology:measurement(usul_tr_t700, usul_al_fiqh_method__maliki_reading, theater_ratio, 700, 0.11).
narrative_ontology:measurement_basis(usul_tr_t700, observed).
narrative_ontology:measurement(usul_tr_t1000, usul_al_fiqh_method__maliki_reading, theater_ratio, 1000, 0.12).
narrative_ontology:measurement_basis(usul_tr_t1000, observed).
narrative_ontology:measurement(usul_tr_t1400, usul_al_fiqh_method__maliki_reading, theater_ratio, 1400, 0.12).
narrative_ontology:measurement_basis(usul_tr_t1400, observed).

% Extraction over time
narrative_ontology:measurement(usul_be_t0, usul_al_fiqh_method__maliki_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(usul_be_t0, observed).
narrative_ontology:measurement(usul_be_t200, usul_al_fiqh_method__maliki_reading, base_extractiveness, 200, 0.28).
narrative_ontology:measurement_basis(usul_be_t200, observed).
narrative_ontology:measurement(usul_be_t400, usul_al_fiqh_method__maliki_reading, base_extractiveness, 400, 0.35).
narrative_ontology:measurement_basis(usul_be_t400, observed).
narrative_ontology:measurement(usul_be_t700, usul_al_fiqh_method__maliki_reading, base_extractiveness, 700, 0.38).
narrative_ontology:measurement_basis(usul_be_t700, observed).
narrative_ontology:measurement(usul_be_t1000, usul_al_fiqh_method__maliki_reading, base_extractiveness, 1000, 0.36).
narrative_ontology:measurement_basis(usul_be_t1000, observed).
narrative_ontology:measurement(usul_be_t1400, usul_al_fiqh_method__maliki_reading, base_extractiveness, 1400, 0.38).
narrative_ontology:measurement_basis(usul_be_t1400, observed).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t0, usul_al_fiqh_method__maliki_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(usul_su_t0, observed).
narrative_ontology:measurement(usul_su_t200, usul_al_fiqh_method__maliki_reading, suppression_requirement, 200, 0.17).
narrative_ontology:measurement_basis(usul_su_t200, observed).
narrative_ontology:measurement(usul_su_t400, usul_al_fiqh_method__maliki_reading, suppression_requirement, 400, 0.19).
narrative_ontology:measurement_basis(usul_su_t400, observed).
narrative_ontology:measurement(usul_su_t700, usul_al_fiqh_method__maliki_reading, suppression_requirement, 700, 0.21).
narrative_ontology:measurement_basis(usul_su_t700, observed).
narrative_ontology:measurement(usul_su_t1000, usul_al_fiqh_method__maliki_reading, suppression_requirement, 1000, 0.2).
narrative_ontology:measurement_basis(usul_su_t1000, observed).
narrative_ontology:measurement(usul_su_t1400, usul_al_fiqh_method__maliki_reading, suppression_requirement, 1400, 0.21).
narrative_ontology:measurement_basis(usul_su_t1400, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__maliki_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(usul_al_fiqh_method__maliki_reading, 0.12).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method__hanbali_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method__shafii_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method__hanafi_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, islamic_regional_legal_pluralism).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, qadi_judicial_discretion_islamic_law).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'usul_al_fiqh_method'. The kernel unifies four jurisprudential schools (Hanafi, Hanbali, Maliki, Shafi'i) around the foundational question: how is Islamic law derived? Each school instantiates a different constraint by advancing a different answer. The Maliki reading elevates customary practice and public interest reasoning; the Hanbali reading restricts to text; the Shafi'i reading systematizes hierarchy; the Hanafi reading expands analogical reasoning. These are not the same constraint viewed differently — they have different ε values, different beneficiaries/victims, and different structural relationships. The family is linked via network.affects_constraints: each reading influences the others by establishing competing legitimacy standards for legal derivation. The upstream constraint is the kernel itself (usul_al_fiqh_method as abstract commitment to derived-law structure), which all four readings presuppose; the downstream constraints are domain applications (qadi discretion, regional legal pluralism) that depend on which reading governs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
