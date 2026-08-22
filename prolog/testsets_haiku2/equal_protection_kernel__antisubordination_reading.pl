% ============================================================================
% CONSTRAINT STORY: equal_protection_kernel__antisubordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_kernel__antisubordination_reading, []).

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
 *   constraint_id: equal_protection_kernel__antisubordination_reading
 *   human_readable: Equal Protection Clause: Antisubordination Reading
 *   domain: constitutional_law/civil_rights
 *
 * SUMMARY:
 *   The Equal Protection Clause of the Fourteenth Amendment (1868) is
 *   contested across three distinct readings: antisubordination (targets
 *   caste-like hierarchy; permits race-conscious remedy), colorblind (forbids
 *   all racial classification regardless of purpose), and remedial (permits
 *   race-conscious action when narrowly tailored to remedy documented
 *   historical harm). This story instantiates the antisubordination
 *   reading—the claim that the clause's core function is to dismantle
 *   inherited subordination, not to police classification itself. The reading
 *   authorizes state action to remedy caste-like subordination and denies
 *   dominant groups equal protection claims against remedial measures. The
 *   ε-invariance principle applies: each reading is a structurally distinct
 *   constraint with different beneficiary/victim sets, different state
 *   authority boundaries, and different χ profiles. The antisubordination
 *   reading targets a standing arrangement (caste-like subordination enforced
 *   through law and custom) assessed by the reading's own lights—not the
 *   colorblind reading's claim that race-consciousness itself is the harm,
 *   nor the remedial reading's narrower focus on historical causation.
 *
 * KEY AGENTS:
 *   - historically_subordinated_castes: beneficiary (reading authorizes remedies on their behalf)
 *   - dominant_racial_groups_claiming_colorblind_protection: victim/payer (denied equal protection against remedial measures)
 *   - state_legislatures_and_agencies: agenda_setter (enact and defend race-conscious remedial measures)
 *   - federal_courts: agenda_setter + observer (interpret the clause and adjudicate measures)
 *   - colorblind_reading_advocates: excluded (powerful but not authorized within this reading's framework)
 *   - remedial_reading_advocates: observer (see but do not author the reading)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_kernel__antisubordination_reading, 0.31).
domain_priors:suppression_score(equal_protection_kernel__antisubordination_reading, 0.44).
domain_priors:theater_ratio(equal_protection_kernel__antisubordination_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_kernel__antisubordination_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_kernel__antisubordination_reading, "Equal Protection Clause: Antisubordination Reading").
narrative_ontology:topic_domain(equal_protection_kernel__antisubordination_reading, "constitutional_law/civil_rights").

domain_priors:requires_active_enforcement(equal_protection_kernel__antisubordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_kernel__antisubordination_reading, '06f5d85a-2271-4742-8918-36586b505fdf').
narrative_ontology:cs_kernel_codification('06f5d85a-2271-4742-8918-36586b505fdf', fixed_text).
narrative_ontology:cs_authority_grounding('06f5d85a-2271-4742-8918-36586b505fdf', lineage).
narrative_ontology:cs_interpretation_layer_present('06f5d85a-2271-4742-8918-36586b505fdf').
narrative_ontology:cs_reading_relation('06f5d85a-2271-4742-8918-36586b505fdf', equal_protection_kernel__colorblind_reading, coexists_with).
narrative_ontology:cs_reading_relation('06f5d85a-2271-4742-8918-36586b505fdf', equal_protection_kernel__remedial_reading, influences).
narrative_ontology:cs_axiom('06f5d85a-2271-4742-8918-36586b505fdf', foundational, subordination_not_classification_as_target).
narrative_ontology:cs_axiom_status(subordination_not_classification_as_target, holdable).
narrative_ontology:cs_axiom_grounding('06f5d85a-2271-4742-8918-36586b505fdf', subordination_not_classification_as_target, deontological).
narrative_ontology:cs_axiom('06f5d85a-2271-4742-8918-36586b505fdf', foundational, state_remedial_authority_for_caste_dismantling).
narrative_ontology:cs_axiom_status(state_remedial_authority_for_caste_dismantling, holdable).
narrative_ontology:cs_axiom_grounding('06f5d85a-2271-4742-8918-36586b505fdf', state_remedial_authority_for_caste_dismantling, instrumental).
narrative_ontology:cs_reference_frame('06f5d85a-2271-4742-8918-36586b505fdf', reconstruction_era_caste_dismantling).
narrative_ontology:cs_drift_state('06f5d85a-2271-4742-8918-36586b505fdf', contemporary_colorblind_institutional_dominance, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('06f5d85a-2271-4742-8918-36586b505fdf', '').
narrative_ontology:cs_kernel_id(equal_protection_kernel__antisubordination_reading, equal_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_kernel__antisubordination_reading, historically_subordinated_castes).
narrative_ontology:constraint_victim(equal_protection_kernel__antisubordination_reading, dominant_racial_groups_claiming_colorblind_protection).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(equal_protection_kernel__antisubordination_reading, colorblind_reading_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Groups with centuries of documented systemic exclusion and subordination (African Americans, Native Americans, Latinos in specific regional contexts, immigrant groups facing discriminatory immigration policy). The antisubordination reading permits state action to dismantle inherited hierarchy: affirmative action, remedial education funding, targeted health access, school desegregation. They remain structurally disadvantaged even as the clause is invoked on their behalf; the constraint's coordinating function is to authorize state action that works in their favor against entrenchment. Exit looks like assimilation into dominant groups (which many resist) or geographic/social departure from contexts of subordination (which structural barriers constrain).
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, historically_subordinated_castes, beneficiary,
    powerless, generational, trapped, national).

% Members of historically dominant groups who challenge race-conscious state measures, typically in litigation (white applicants to universities or employees alleging reverse discrimination). The antisubordination reading structurally denies them equal protection claims against remedial measures because the clause targets subordination, not classification per se. They bear direct costs: ineligibility for certain opportunities, higher admission standards when remedial preferences apply, exclusion from certain targeted benefits. Exit is constrained because the reach of equal protection doctrine is national; they cannot simply move to avoid it, though they can argue for colorblind recasting in court.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, dominant_racial_groups_claiming_colorblind_protection, payer,
    powerful, generational, constrained, national).

% Enact and administer race-conscious policies (affirmative action, targeted funding, school desegregation plans) authorized under the antisubordination reading. They must diagnose caste-like subordination, design remedies narrowly tailored to dismantle it (per omega 3 constraint), and defend measures against colorblind and remedial-limiting challenges. They gain authority to act but incur political and legal risk—measures are frequently litigated, and their success in courts depends on which reading the judiciary adopts. Arbitrage exists because different states can adopt different remedial strategies, and federal versus state authority over education, health, criminal justice creates variation.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, state_legislatures_and_agencies, agenda_setter,
    institutional, generational, arbitrage, national).

% Interpret the Equal Protection Clause and adjudicate state measures under competing readings (antisubordination, colorblind, remedial). They define what counts as caste-like subordination, what remedies are permissible, and whether state action entrenches or dismantles hierarchy. Their interpretive choices directly shape the constraint's enforcement and have oscillated between readings (Warren Court → colorblind → selective antisubordination). They occupy analytical position because their role is to adjudicate; they are also agenda-setters because their interpretations become binding doctrine.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, federal_courts, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_kernel__antisubordination_reading, federal_courts, observer).

% Judges (Supreme Court majority post-1989), constitutional scholars, and conservative civil rights organizations who read the Equal Protection Clause as categorically prohibiting racial classification. They argue the clause protects individual dignity independent of group history and that remedial race-consciousness replicates the constitutional wrong. They are excluded from the antisubordination reading's authority structure—their interpretation is not legitimate within this reading's framework. However, they maintain substantial institutional power through the Supreme Court and legal academia to contest antisubordination measures in court. They experience the antisubordination reading as denial of colorblind principle and as invading reserved state authority to police classifications.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, colorblind_reading_advocates, excluded,
    powerful, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_kernel__antisubordination_reading, colorblind_reading_advocates, payer).

% Judges and legal scholars who accept race-conscious remedies but ground them in historical injustice remediation rather than antisubordination principle. They see antisubordination as broader and less moored to specific documented harm; they prefer narrow tailoring to historical causation. They observe the antisubordination reading (do not author its core premise) and potentially constrain its scope through the remedial limits they insist on (see omega 3). They are not excluded but are distinct—they are the mediating position between antisubordination and colorblind.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, remedial_reading_advocates, observer,
    powerful, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_kernel__antisubordination_reading, historically_subordinated_castes).
narrative_ontology:fixing_cost_class(equal_protection_kernel__antisubordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Defines and coordinates the permissible scope of state action on matters affecting racial hierarchy. The reading solves the collective-action problem: subordinated groups need legal authorization to pursue remedies against entrenched caste systems; dominant groups have incentive to claim colorblind protection against those remedies; courts must adjudicate which interpretation of the clause prevails. The antisubordination reading coordinates around the principle that the clause targets inherited subordination, not classification, and authorizes state remediation while denying colorblind counterclaims.
% TRANSFER_FUNCTION: Transfers legal standing and authority asymmetrically: subordinated castes gain the right to invoke equal protection in their favor and the right to claim state remedies. Dominant racial groups lose the right to invoke equal protection against remedial measures. What moves is permission to act—authorization vectors flow in opposite directions for the two stakeholder sets. The transfer is non-monetary but has material consequences: remedial access, ineligibility for certain positions, litigation risk.
% ABSENT_VOICES: Colorblind reading advocates are excluded from the antisubordination reading's interpretive authority, though they remain powerful in institutions (Supreme Court, major law schools). They would argue that race-consciousness violates individual dignity and that the clause protects rights independent of group history. Their exclusion is structural—the reading's core premise (subordination is the target, not classification) forecloses their framing within antisubordination logic. They are heard in litigation and public discourse but not as legitimate interpreters of the clause's meaning in this reading.
% DISAPPEARANCE_RATIONALE: If the antisubordination reading evaporated—e.g., Supreme Court decisively adopted colorblind doctrine and repudiated antisubordination language—the legal authority for race-conscious remedies would collapse. Affirmative action, targeted school funding, desegregation plans, remedial hiring would be invalidated or severely constrained. Subordinated groups would lose a primary constitutional tool for state-sanctioned dismantling of inherited subordination; dominant groups would gain colorblind standing to block remedial measures. The civil rights law landscape reorganizes: remedies become available only through narrow-tailoring language tied to specific past harms (remedial reading) or would be unavailable entirely (colorblind reading). The distribution of legal authority shifts decisively.
% FOUNDING_PROBLEM: The Fourteenth Amendment (1868) was adopted to dismantle slavery and the legal caste system it created. Subsequent judicial doctrine—especially the colorblind reading—degraded the clause's function into formal neutrality, permitting inherited subordination to persist. The antisubordination reading recovers the clause's founding purpose: preventing state action that entrenches caste-like hierarchy and authorizing state action that dismantles it.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Reconstruction (Eric Foner, James McPherson, Michael W. McConnell) corroborate that the Fourteenth Amendment was drafted to dismantle slavery and its successor systems of legal subordination, and to authorize federal intervention to remedy subordination. Colorblind reading advocates argue the founding intent was to protect individual rights neutral to race, citing text and some founding-era statements. The empirical fact of colorblind doctrine's institutional dominance (post-Plessy, intensifying post-1989) is documented across Supreme Court opinions. Corroboration from outside the antisubordination advocates' circle comes from legal historians and Reconstruction scholars (not uniformly aligned with civil rights advocacy). The remedial reading accepts historical subordination as a fact but argues the clause's remedy is narrower than antisubordination allows.
narrative_ontology:disappearance_verdict(equal_protection_kernel__antisubordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_kernel__antisubordination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_kernel__antisubordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equal_protection_kernel__antisubordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_kernel__antisubordination_reading, 0.31, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_kernel__antisubordination_reading_tests).
:- end_tests(equal_protection_kernel__antisubordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is moderate-low (0.31) because the antisubordination reading is itself a claim about justice and remedy, not a pure extraction mechanism. However, extraction is non-zero because (1) the reading's adoption requires denying dominant groups colorblind protection they would claim, (2) state measures applying the reading impose real costs on individuals ineligible for certain opportunities or facing remedial preferences, and (3) the reading's success depends on suppression of competing interpretations (colorblind doctrine). Suppression is moderate (0.44) because the colorblind reading remains institutionally powerful—it dominates Supreme Court jurisprudence post-1989 (the measured decline from 0.76 in 1960 reflects this shift). Theater is low (0.12) because the constraint's function is substantively about dismantling subordination, not theatrical performance; the reading does not persist by inertia. Accessibility collapse (0.68) reflects that once the antisubordination reading is understood, the subordinated-group beneficiaries have access to a legal framework that can authorize state remedies—alternatives (individual merit, pure neutrality) collapse as justifications for inherited caste advantage. Resistance (0.72) is high because the colorblind reading's institutional power generates substantial resistance to antisubordination claims; the reading must actively win interpretive authority in courts. The measurement series track 156 years of interpretive contestation: extractiveness rose from 1868 to 1920 (colorblind doctrine became entrenched), peaked in 1960 (Civil Rights era contestation), then declined as antisubordination framing gained traction post-1980s (Brown aftermath, affirmative action litigation). Suppression followed the reverse arc: maximal (0.88) in 1920 when colorblind doctrine was locked in, declined as the reading pushed back (1960–2000), stabilizing at 0.44 as the colorblind counter-reading regained Supreme Court dominance (2000–2024).
 *
 * PERSPECTIVAL GAP:
 *   The colorblind reading advocates occupy a powerful institutional position (Supreme Court majority post-1989, major law school traditions) but are not authorized as framers within the antisubordination reading's authority structure. They see the reading as permitting racial discrimination; the reading sees colorblind doctrine as institutionalizing caste subordination. This is not a difference of opinion but a difference of kernel readings—two incompatible authorizations of state power derived from the same constitutional text.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary set (historically subordinated castes) is identified by the reading as having suffered caste-like subordination—the clause targets their situation. Directionality for this set approaches zero (full beneficiary): the reading authorizes state action on their behalf, permits remedial measures, and provides legal standing to invoke the clause. The victim set (dominant racial groups claiming colorblind protection) experiences the reading as a denial—they lose the colorblind claim and bear costs when remedial preferences are applied. Directionality for this set is high (approaching target): the reading structurally excludes their claims and imposes costs. State legislatures and agencies sit near symmetric (d ≈ 0.5): they gain authority to act but must defend measures against multiple readings; courts sit at analytical (observer position with interpretive authority).
 *
 * MANDATROPHY ANALYSIS:
 *   The antisubordination reading faces a mandatrophy question: the Fourteenth Amendment was adopted to dismantle slavery and caste; the colorblind reading has degraded that mandate into a formal neutrality that permits subordination to persist. The antisubordination reading explicitly treats this degradation as the problem—it recovers the clause's founding function against its distortion. The measured decline in extractiveness from 1920 to 2024 reflects the reading's partial success in re-asserting this mandate, though it remains contested. No mandatrophy is resolved here; rather, the reading's function is mandatrophy-resistance: it insists the clause's original mandate (dismantle caste) has not been superseded by colorblind ideology, even though colorblind doctrine institutionally dominates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subordination_identification_ambiguity,
    'What counts as caste-like subordination versus individual disadvantage or group underrepresentation? Where is the boundary between inherited systemic subordination and historical inequity that does not rise to subordination level?',
    'Empirical analysis of systemic exclusion depth: intergenerational wealth gaps, occupational segregation, educational tracking, residential segregation, criminal justice system disparities. Conceptual analysis of which groups suffered continuous legal subordination (slavery, Jim Crow, genocidal removal, forced assimilation) versus groups facing documented discrimination without legal subordination status.',
    'If the boundary is drawn narrowly (only slavery/Jim Crow legacy), fewer groups qualify for antisubordination remedies and the reading''s scope contracts. If drawn broadly (any historical systemic disadvantage), more groups claim remedial authority and colorblind challenges become harder to sustain. The reading''s coherence depends on this boundary being stable and defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subordination_identification_ambiguity, conceptual, 'The definitional boundary between caste-like subordination and non-subordination disadvantage.').

omega_variable(
    colorblind_vs_antisubordination_foreclosure,
    'Do the colorblind reading and the antisubordination reading logically foreclose each other, or can they coexist as different framings held by different parties?',
    'Jurisprudential analysis: if the readings share premises (e.g., both ground legitimacy in the same constitutional text and authority chain), they are competitors and neither forecloses. If one reading requires denying the other''s core premise (colorblind requires denying that classification can serve justice; antisubordination requires denying that color-blindness is constitutional), they foreclose within a single framework. The measure is whether a judge can consistently apply both or must choose.',
    'If they foreclose: the Supreme Court''s oscillation between readings is incoherent and one must eventually prevail institutionally. If they coexist: sustained contestation is the normal state and neither reading can claim final victory. This affects whether the constraint is better classified as a stable coordination mechanism (coexist) or an unstable extraction covering contestation (foreclose).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(colorblind_vs_antisubordination_foreclosure, conceptual, 'Whether the antisubordination and colorblind readings are logically incompatible or can both remain live positions.').

omega_variable(
    state_remedial_authority_limits,
    'How far may state action go in dismantling subordination? Are there constraints on remedial scope—e.g., must remedies be tied to specific historical harms, must they sunset, must they avoid burdening uninvolved third parties?',
    'Comparison with remedial reading''s narrowing doctrine (narrow tailoring, compelling interest, timing restrictions). Empirical study of remedial measures that courts have sustained under antisubordination framing versus those invalidated. Conceptual argument about whether subordination-dismantling logically requires temporal limits or scope limits.',
    'If state remedial authority is unlimited (only constraint is that it serves subordination-dismantling), the reading''s extraction on dominant groups is high and suppression is low—it becomes nearly a snare from their perspective. If remedial authority is narrowly constrained, extraction moderates and the reading looks more like tangled_rope (genuine coordination + asymmetric cost-sharing). The measurement profile depends on this boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_remedial_authority_limits, conceptual, 'The scope and limits of state authority to remedy subordination under the antisubordination reading.').

omega_variable(
    reading_institutional_adoption_path,
    'Has the antisubordination reading ever achieved stable institutional adoption as the Supreme Court''s primary framework, or is it structurally constrained to remain a challenger reading?',
    'Historical analysis of Supreme Court doctrine: the antisubordination reading achieved partial institutional purchase in some Warren Court opinions and survived selectively in post-1980 affirmative action jurisprudence, but colorblind doctrine dominated post-1989. Examine whether institutional obstacles (text emphasis on ''equal protection,'' originalist methodology, five-justice limits on remedial authority) systematically prevent antisubordination from settling as doctrine.',
    'If antisubordination is institutionally blocked from full adoption, the reading''s long-term persistence depends on extra-institutional contestation (scholarship, civil rights advocacy, public pressure). This affects whether the constraint is best classified as an entrenched coordination mechanism or a perpetually defended alternative. The theatre_ratio may rise if antisubordination framing becomes performative compensation for institutional weakness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_institutional_adoption_path, empirical, 'Whether the antisubordination reading can achieve institutional dominance or remains constrained to challenger status.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_kernel__antisubordination_reading, 1868, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1868, equal_protection_kernel__antisubordination_reading, theater_ratio, 1868, 0.05).
narrative_ontology:measurement(equa_tr_t1920, equal_protection_kernel__antisubordination_reading, theater_ratio, 1920, 0.08).
narrative_ontology:measurement(equa_tr_t1960, equal_protection_kernel__antisubordination_reading, theater_ratio, 1960, 0.18).
narrative_ontology:measurement(equa_tr_t1980, equal_protection_kernel__antisubordination_reading, theater_ratio, 1980, 0.14).
narrative_ontology:measurement(equa_tr_t2000, equal_protection_kernel__antisubordination_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(equa_tr_t2024, equal_protection_kernel__antisubordination_reading, theater_ratio, 2024, 0.12).

% Extraction over time
narrative_ontology:measurement(equa_be_t1868, equal_protection_kernel__antisubordination_reading, base_extractiveness, 1868, 0.15).
narrative_ontology:measurement(equa_be_t1920, equal_protection_kernel__antisubordination_reading, base_extractiveness, 1920, 0.42).
narrative_ontology:measurement(equa_be_t1960, equal_protection_kernel__antisubordination_reading, base_extractiveness, 1960, 0.51).
narrative_ontology:measurement(equa_be_t1980, equal_protection_kernel__antisubordination_reading, base_extractiveness, 1980, 0.38).
narrative_ontology:measurement(equa_be_t2000, equal_protection_kernel__antisubordination_reading, base_extractiveness, 2000, 0.34).
narrative_ontology:measurement(equa_be_t2024, equal_protection_kernel__antisubordination_reading, base_extractiveness, 2024, 0.31).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1868, equal_protection_kernel__antisubordination_reading, suppression_requirement, 1868, 0.72).
narrative_ontology:measurement(equa_su_t1920, equal_protection_kernel__antisubordination_reading, suppression_requirement, 1920, 0.88).
narrative_ontology:measurement(equa_su_t1960, equal_protection_kernel__antisubordination_reading, suppression_requirement, 1960, 0.76).
narrative_ontology:measurement(equa_su_t1980, equal_protection_kernel__antisubordination_reading, suppression_requirement, 1980, 0.58).
narrative_ontology:measurement(equa_su_t2000, equal_protection_kernel__antisubordination_reading, suppression_requirement, 2000, 0.48).
narrative_ontology:measurement(equa_su_t2024, equal_protection_kernel__antisubordination_reading, suppression_requirement, 2024, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_kernel__antisubordination_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(equal_protection_kernel__antisubordination_reading, 0.12).
narrative_ontology:affects_constraint(equal_protection_kernel__antisubordination_reading, equal_protection_kernel__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_kernel__antisubordination_reading, equal_protection_kernel__remedial_reading).

% DUAL FORMULATION NOTE:
% The equal_protection_kernel decomposes into three constraint stories corresponding to three readings of the Fourteenth Amendment's Equal Protection Clause. Each reading instantiates a distinct constraint with different beneficiary/victim sets, different ε values, and different state authority boundaries. This story (antisubordination_reading) treats caste-like subordination as the clause's target and authorizes race-conscious remedies; it is linked to colorblind_reading (which forecloses it in a single framework) and remedial_reading (which influences it by narrowing remedial scope). The readings are sibling constraints in a constraint family; all are authorable only by treating the clause's meaning as reading-indexed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
