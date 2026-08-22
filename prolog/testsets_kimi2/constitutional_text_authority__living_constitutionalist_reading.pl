% ============================================================================
% CONSTRAINT STORY: constitutional_text_authority__living_constitutionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text_authority__living_constitutionalist_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: constitutional_text_authority__living_constitutionalist_reading
 *   human_readable: Living Constitutionalist Interpretive Framework
 *   domain: legal/constitutional
 *
 * SUMMARY:
 *   This constraint instantiates the living_constitutionalist_reading of the
 *   constitutional_text_authority kernel. The standing arrangement is a
 *   binding interpretive practice in which federal constitutional meaning is
 *   updated by the judiciary in light of contemporary moral principles and
 *   evolving social attitudes, without recourse to Article V amendment.
 *   Sibling readings include originalist_reading (meaning fixed at
 *   ratification) and positivist_reading (validity from formal enactment
 *   procedures alone). The constraint is claimed as tangled_rope: it provides
 *   genuine coordination against constitutional sclerosis while
 *   asymmetrically concentrating quasi-amendment authority in the federal
 *   judiciary and marginalizing fixed-text interpretive communities.
 *
 * KEY AGENTS:
 *   - federal_judiciary: Primary agenda-setter (institutional/constrained) â administers the evolving-values framework and captures institutional authority
 *   - progressive_litigants: Primary beneficiary (moderate/constrained) â gains judicial recognition of unenumerated rights
 *   - originalist_jurists: Primary payer (moderate/identity_locked) â bears professional marginalization in elite institutions
 *   - state_legislatures: Secondary payer (institutional/constrained) â democratic lawmaking overridden by judicial updating
 *   - legal_academy_mainstream: Secondary beneficiary (institutional/mobile) â supplies normative theory that feeds judicial opinions
 *   - conservative_religious_groups: Payer (organized/constrained) â substantive moral legislation treated as constitutionally obsolete
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text_authority__living_constitutionalist_reading, 0.58).
domain_priors:suppression_score(constitutional_text_authority__living_constitutionalist_reading, 0.48).
domain_priors:theater_ratio(constitutional_text_authority__living_constitutionalist_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text_authority__living_constitutionalist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text_authority__living_constitutionalist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(constitutional_text_authority__living_constitutionalist_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text_authority__living_constitutionalist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text_authority__living_constitutionalist_reading, "Living Constitutionalist Interpretive Framework").
narrative_ontology:topic_domain(constitutional_text_authority__living_constitutionalist_reading, "legal/constitutional").

domain_priors:requires_active_enforcement(constitutional_text_authority__living_constitutionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text_authority__living_constitutionalist_reading, 'd7ca82b0-bc54-4740-bfc0-e7a603b9b5fd').
narrative_ontology:cs_kernel_codification('d7ca82b0-bc54-4740-bfc0-e7a603b9b5fd', fixed_text).
narrative_ontology:cs_authority_grounding('d7ca82b0-bc54-4740-bfc0-e7a603b9b5fd', lineage).
narrative_ontology:cs_interpretation_layer_present('d7ca82b0-bc54-4740-bfc0-e7a603b9b5fd').
narrative_ontology:cs_reading_relation('d7ca82b0-bc54-4740-bfc0-e7a603b9b5fd', constitutional_text_authority__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('d7ca82b0-bc54-4740-bfc0-e7a603b9b5fd', constitutional_text_authority__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('d7ca82b0-bc54-4740-bfc0-e7a603b9b5fd', foundational, constitutional_meaning_evolves_with_societal_values).
narrative_ontology:cs_axiom_status(constitutional_meaning_evolves_with_societal_values, holdable).
narrative_ontology:cs_axiom_grounding('d7ca82b0-bc54-4740-bfc0-e7a603b9b5fd', constitutional_meaning_evolves_with_societal_values, deontological).
narrative_ontology:cs_axiom('d7ca82b0-bc54-4740-bfc0-e7a603b9b5fd', foundational, unenumerated_rights_judicially_recognizable).
narrative_ontology:cs_axiom_status(unenumerated_rights_judicially_recognizable, holdable).
narrative_ontology:cs_axiom_grounding('d7ca82b0-bc54-4740-bfc0-e7a603b9b5fd', unenumerated_rights_judicially_recognizable, deontological).
narrative_ontology:cs_reference_frame('d7ca82b0-bc54-4740-bfc0-e7a603b9b5fd', continuing_constitutional_enterprise).
narrative_ontology:cs_drift_state('d7ca82b0-bc54-4740-bfc0-e7a603b9b5fd', contemporary_political_polarization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d7ca82b0-bc54-4740-bfc0-e7a603b9b5fd', '').
narrative_ontology:cs_kernel_id(constitutional_text_authority__living_constitutionalist_reading, constitutional_text_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text_authority__living_constitutionalist_reading, federal_judiciary).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__living_constitutionalist_reading, progressive_litigants).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__living_constitutionalist_reading, civil_rights_advocates).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__living_constitutionalist_reading, legal_academy_mainstream).
narrative_ontology:constraint_victim(constitutional_text_authority__living_constitutionalist_reading, originalist_jurists).
narrative_ontology:constraint_victim(constitutional_text_authority__living_constitutionalist_reading, state_legislatures).
narrative_ontology:constraint_victim(constitutional_text_authority__living_constitutionalist_reading, conservative_religious_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercises broad interpretive discretion to update constitutional meaning in light of evolving values and contemporary moral principles; issues landmark rulings on unenumerated rights; enjoys institutional centrality and prestige from being the primary locus of constitutional adaptation in the political system.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text_authority__living_constitutionalist_reading, federal_judiciary, beneficiary).

% Invoke evolving constitutional norms to challenge existing legislation; benefit from judicial recognition of unenumerated rights (privacy, autonomy, equality) that lack explicit textual basis but are justified by contemporary moral understanding and social progress.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, progressive_litigants, beneficiary,
    moderate, biographical, constrained, national).

% Organize litigation and public campaigns around the promise of an evolving Constitution; use the framework to secure protections for marginalized groups through judicial channels rather than waiting for legislative majorities in hostile jurisdictions.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, civil_rights_advocates, beneficiary,
    organized, generational, constrained, national).

% Produces the moral-philosophical frameworks (autonomy, dignity, equality as evolving concepts) that supply content for judicial opinions; benefits from the interpretive method because it makes constitutional law a field of normative theory and policy argument rather than historical excavation.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, legal_academy_mainstream, beneficiary,
    institutional, generational, mobile, national).

% Advance arguments based on fixed historical meaning and ratifier intent; face professional marginalization in elite legal institutions where living constitutionalism dominates methodological discourse; their interpretive framework is frequently characterized as illegitimate or backward in hiring, publication, and clerkship culture.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, originalist_jurists, payer,
    moderate, biographical, identity_locked, national).

% Enact laws reflecting local majoritarian preferences and traditional moral frameworks; face federal judicial override when courts invalidate those laws based on newly recognized evolving constitutional norms; the formal Article V amendment process is effectively bypassed by judicial updating.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, state_legislatures, payer,
    institutional, generational, constrained, national).

% Hold traditional moral views codified in state law and community practice; see those laws overturned or narrowed by federal courts citing evolving national values; their substantive moral vision is treated as constitutionally obsolete without legislative repeal or democratic deliberation.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, conservative_religious_groups, payer,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text_authority__living_constitutionalist_reading, federal_judiciary).
narrative_ontology:fixing_cost_class(constitutional_text_authority__living_constitutionalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the adaptation of an 18th-century written text to 21st-century governance problems without requiring supermajoritarian political consensus for every constitutional update; provides a unified interpretive framework for judicial resolution of fundamental rights disputes in a society whose technology, demographics, and moral commitments have radically changed.
% TRANSFER_FUNCTION: Transfers quasi-amendment authority from the Article V process and the fixed historical meaning of the constitutional text to the federal judiciary and contemporary elite moral consensus; transfers interpretive legitimacy and professional status from historical-exegetical methods to normative-theoretical methods.
% ABSENT_VOICES: Originalist jurists and populist constitutional movements are formally present in public discourse but structurally excluded from elite interpretive institutions (top-14 law schools, feeder judgeships, flagship journals); lay citizens who understand the Constitution as a fixed text they encountered in civic education have no seat in the interpretive process that updates its meaning overnight through judicial decision.
% DISAPPEARANCE_RATIONALE: If the living constitutionalist interpretive framework vanished overnight, federal courts would lose the methodological basis for recognizing unenumerated rights and updating constitutional meaning; landmark precedents spanning privacy, substantive due process, and equal protection would lack doctrinal grounding; constitutional politics would shift toward Article V amendment or originalist adjudication; the legal academy's normative-theory enterprise would collapse; progressive litigation strategies would be forced into legislative channels.
% FOUNDING_PROBLEM: A written constitution drafted in 1787 cannot practically govern a radically transformed industrial, digital, and pluralistic society through formal amendment alone; rigid textualism would produce sclerosis or force constant supermajoritarian political crises.
% FOUNDING_PROBLEM_CORROBORATION: Living constitutionalist jurists and mainstream legal historians attest the problem remains live due to Article V's difficulty. Originalist jurists and comparative constitutional scholars attest the founding problem is overstated â Article V has been used successfully when consensus exists, and judicial updating creates democratic deficits that are worse than sclerosis. No party outside the methodological dispute offers neutral corroboration; the status is itself contested by the structurally opposed seats.
narrative_ontology:disappearance_verdict(constitutional_text_authority__living_constitutionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text_authority__living_constitutionalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text_authority__living_constitutionalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_text_authority__living_constitutionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text_authority__living_constitutionalist_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text_authority__living_constitutionalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text_authority__living_constitutionalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_text_authority__living_constitutionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is substantial because the framework transfers amendment-equivalent authority from Article V to the judiciary, bypassing supermajoritarian checks. Suppression (0.48) is moderate: originalism is not outlawed but is structurally disadvantaged in hiring, clerkship pipelines, and elite discourse. Theater_ratio (0.58) is elevated because judicial opinions perform extensive moral-philosophical reasoning that often masks straightforward value choices. Accessibility_collapse (0.45) reflects that originalism remains intellectually available but carries rising professional costs. Resistance (0.52) captures the sustained originalist counter-movement and political backlash against judicial moral updating. The measurement series tracks the Warren Court revolution through the contemporary polarization era on a single shared grid.
 *
 * PERSPECTIVAL GAP:
 *   The federal judiciary experiences the constraint as necessary coordination: without evolving interpretation, the Constitution would fail to govern a transformed society. Progressive litigants experience it as a rights-protective mechanism. Originalist jurists and state legislatures experience the identical constraint as extraction: interpretive authority and democratic lawmaking capacity are transferred to an unelected judiciary. The engine computes this divergence from the structural data (beneficiary/victim declarations, identity_locked exit for originalists, constrained exit for legislatures) rather than from the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (judiciary, progressive litigants, civil rights advocates, mainstream legal academy) sit at low directionality: the constraint subsidizes their institutional power, litigation strategies, and scholarly enterprise. Victims (originalist jurists, state legislatures, conservative religious groups) sit at high directionality: the constraint extracts interpretive legitimacy and lawmaking authority from them. Originalist jurists are identity_locked because professional identity is fused to a methodological frame that the dominant institutions treat as illegitimate, amplifying their effective extraction. State legislatures have constrained exit because override is immediate and Article V is prohibitively difficult.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â constitutional sclerosis in a rapidly changing society â was genuinely live in the mid-20th century. The framework solved it. Today the founding problem status is contested: originalists argue Article V remains viable when consensus exists, while living constitutionalists argue adaptation remains essential. The tangled_rope classification prevents mislabeling the arrangement as a pure snare (there is real coordination against obsolescence) or as a pure rope (the asymmetric concentration of amendment authority in the judiciary is structurally extractive). The R5 genealogy (founding_problem_status contested + disappearance_verdict world_rearranges) flags that the constraint's persistence may now exceed its uncontested mandate, but the continued reality of novel governance problems (digital surveillance, genomic privacy) sustains the live-coordination defense.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    living_constitution_naturalness,
    'Is the living constitutionalist framework a structurally necessary adaptation mechanism for written constitutions, or a constructed transfer of amendment authority to the judiciary?',
    'Historical comparison of constitutional regimes with and without rigid amendment rules; measurement of legislative sclerosis versus judicial override rates.',
    'If necessary adaptation, the coordination component dominates and extraction is bounded by the survival function; if constructed power transfer, the extraction component dominates and the constraint functions as a snare of judicial supremacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(living_constitution_naturalness, conceptual, 'Whether living constitutionalism is necessary coordination or judicial extraction').

omega_variable(
    evolving_values_elite_capture,
    'Does the ''evolving values'' framework track genuine broad social consensus, or is it captured by educated elite moral consensus that diverges from majority attitudes?',
    'Empirical mapping of judicial outcomes against polling data, state legislative trends, and demographic attitude surveys over time.',
    'If elite capture, the framework extracts from democratic majorities and disfavored moral communities; if consensus-tracking, the coordination function is more legitimate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(evolving_values_elite_capture, empirical, 'Whether evolving values represent social consensus or elite capture').

omega_variable(
    committer_sibling_boundary,
    'Does the living constitutionalist reading foreclose the originalist reading within a single interpretive framework, or can a pluralist judge hold both methodologies for different clauses?',
    'Jurisprudential analysis of whether methodological pluralism is internally coherent; survey of judicial opinions mixing originalist and living constitutionalist reasoning.',
    'If strict foreclosure, the kernel generates zero-sum interpretive politics; if pluralism is coherent, coexists_with is the more accurate relation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_sibling_boundary, conceptual, 'Logical relationship between living constitutionalism and originalism in single-framework holding').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text_authority__living_constitutionalist_reading, 0, 74).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(living_const_tr_t0, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(living_const_tr_t12, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 12, 0.32).
narrative_ontology:measurement(living_const_tr_t24, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 24, 0.45).
narrative_ontology:measurement(living_const_tr_t36, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 36, 0.5).
narrative_ontology:measurement(living_const_tr_t48, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 48, 0.54).
narrative_ontology:measurement(living_const_tr_t60, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 60, 0.56).
narrative_ontology:measurement(living_const_tr_t74, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 74, 0.58).

% Extraction over time
narrative_ontology:measurement(living_const_be_t0, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(living_const_be_t12, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 12, 0.38).
narrative_ontology:measurement(living_const_be_t24, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 24, 0.48).
narrative_ontology:measurement(living_const_be_t36, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 36, 0.5).
narrative_ontology:measurement(living_const_be_t48, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 48, 0.53).
narrative_ontology:measurement(living_const_be_t60, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 60, 0.56).
narrative_ontology:measurement(living_const_be_t74, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 74, 0.58).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(constitutional_text_authority__living_constitutionalist_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(constitutional_text_authority__living_constitutionalist_reading, originalist_reading).
narrative_ontology:affects_constraint(constitutional_text_authority__living_constitutionalist_reading, positivist_reading).

% DUAL FORMULATION NOTE:
% One reading of the constitutional_text_authority kernel. The living_constitutionalist_reading, originalist_reading, and positivist_reading form a constraint family decomposed per the epsilon-invariance principle: each instantiates a distinct structural claim about the source of constitutional authority and produces a different epsilon profile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
