% ============================================================================
% CONSTRAINT STORY: us_constitution_interpretive__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_interpretive__originalist_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: us_constitution_interpretive__originalist_reading
 *   human_readable: Originalist Constitutional Interpretation: Fixed Meaning at Ratification
 *   domain: constitutional_law/legal_interpretation/political_theory
 *
 * SUMMARY:
 *   The originalist reading of the U.S. Constitution holds that
 *   constitutional meaning is fixed at the moment of ratification and that
 *   judges must interpret the document according to Framers' intent or
 *   original public meaning. This reading constrains federal power to
 *   enumerated authorities, limits unenumerated rights doctrine, and enforces
 *   structural federalism. It is one reading of the same constitutional text
 *   that living constitutionalists and popular constitutionalists read very
 *   differently. The originalist reading benefits federalism advocates,
 *   religious liberty claimants under narrow historical scope, and property
 *   rights defenders; it extracts costs from constituencies seeking to expand
 *   federal civil rights authority, assert unenumerated rights, and adapt
 *   constitutional doctrine to contemporary conditions. The constraint is
 *   CLAIMED as tangled_rope (real coordination function: fixing meaning to
 *   prevent interpretive chaos; real enforcement: judges must suppress
 *   alternative methodologies) and the metrics are authored to reflect
 *   substantial extraction, moderate suppression, and rising theater
 *   ratio—the engine will compute whether the claim and metrics cohere.
 *
 * KEY AGENTS:
 *   - Originalist federal judiciary: agenda-setter, institutional power, sets the interpretive framework and enforces it through opinions constraining federal authority and unenumerated rights
 *   - Federalism advocates: beneficiary, powerful, win outcomes when originalism constrains federal enumerated powers and strengthens state reserved authority
 *   - Religious liberty claimants under original understanding: beneficiary, organized, benefit from narrow historical scope of free exercise and establishment clause limitations
 *   - Property rights defenders: beneficiary, powerful, benefit from constrained regulatory authority and narrow takings clause jurisprudence
 *   - Unenumerated rights claimants: victim, moderate power but identity-locked exit, lose judicial vindication when originalism forecloses privacy/autonomy rights
 *   - Federal regulatory and civil rights advocates: victim, organized, face constraints when originalism narrows federal enumerated powers and Section Five remedial scope
 *   - Civil rights movement constituencies: victim, powerless but identity-locked, structurally dependent on federal authority doctrine that originalism constrains
 *   - Living constitutionalist legal academy: excluded, organized, structurally excluded from the originalist framework's legitimacy though they argue against its premises
 *   - Federalist Society network: agenda-setter + beneficiary, institutional, architect of originalist doctrine and institutional beneficiary of its judicial dominance
 *   - Constitutional scholars: observer, organized, track the methodological contest and produce evidence about historical meaning and interpretive fidelity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_interpretive__originalist_reading, 0.58).
domain_priors:suppression_score(us_constitution_interpretive__originalist_reading, 0.42).
domain_priors:theater_ratio(us_constitution_interpretive__originalist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_interpretive__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_interpretive__originalist_reading, "Originalist Constitutional Interpretation: Fixed Meaning at Ratification").
narrative_ontology:topic_domain(us_constitution_interpretive__originalist_reading, "constitutional_law/legal_interpretation/political_theory").

domain_priors:requires_active_enforcement(us_constitution_interpretive__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_interpretive__originalist_reading, '643ed4f6-1eea-4336-94d6-82d46d0e4ff4').
narrative_ontology:cs_kernel_codification('643ed4f6-1eea-4336-94d6-82d46d0e4ff4', fixed_text).
narrative_ontology:cs_authority_grounding('643ed4f6-1eea-4336-94d6-82d46d0e4ff4', lineage).
narrative_ontology:cs_interpretation_layer_present('643ed4f6-1eea-4336-94d6-82d46d0e4ff4').
narrative_ontology:cs_reading_relation('643ed4f6-1eea-4336-94d6-82d46d0e4ff4', us_constitution_interpretive__living_constitution_reading, coexists_with).
narrative_ontology:cs_reading_relation('643ed4f6-1eea-4336-94d6-82d46d0e4ff4', us_constitution_interpretive__popular_constitutionalism_reading, coexists_with).
narrative_ontology:cs_axiom('643ed4f6-1eea-4336-94d6-82d46d0e4ff4', foundational, constitutional_meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(constitutional_meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('643ed4f6-1eea-4336-94d6-82d46d0e4ff4', constitutional_meaning_fixed_at_ratification, deontological).
narrative_ontology:cs_axiom('643ed4f6-1eea-4336-94d6-82d46d0e4ff4', foundational, framers_intent_or_original_public_meaning_determines_scope).
narrative_ontology:cs_axiom_status(framers_intent_or_original_public_meaning_determines_scope, holdable).
narrative_ontology:cs_axiom_grounding('643ed4f6-1eea-4336-94d6-82d46d0e4ff4', framers_intent_or_original_public_meaning_determines_scope, empirically_contingent).
narrative_ontology:cs_reference_frame('643ed4f6-1eea-4336-94d6-82d46d0e4ff4', framers_intent_authority_paradigm).
narrative_ontology:cs_drift_state('643ed4f6-1eea-4336-94d6-82d46d0e4ff4', contemporary_judicial_appointments_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('643ed4f6-1eea-4336-94d6-82d46d0e4ff4', '').
narrative_ontology:cs_kernel_id(us_constitution_interpretive__originalist_reading, us_constitution_interpretive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, federalism_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, original_understanding_religious_liberty_claimants).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, property_rights_defenders).
narrative_ontology:constraint_victim(us_constitution_interpretive__originalist_reading, unenumerated_rights_claimants).
narrative_ontology:constraint_victim(us_constitution_interpretive__originalist_reading, federal_regulatory_expansion_advocates).
narrative_ontology:constraint_victim(us_constitution_interpretive__originalist_reading, civil_rights_movement_constituencies_relying_on_evolving_doctrine).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_interpretive__originalist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(us_constitution_interpretive__originalist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_interpretive__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_interpretive__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_interpretive__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.35 (1987, when originalism was ascendant but not yet dominant in appointments) to 0.58 (2026, after systematic originalist judicial appointments). The rise reflects the constraint's strengthening grip on doctrine—more federal statutes struck down for exceeding enumerated authority, more unenumerated rights claims foreclosed, more federalism victories. Theater ratio climbs from 0.12 to 0.28, reflecting increasing performative maintenance: originalist judges emphasize historical fidelity even where historical sources are ambiguous or where alternative historical readings would reach different conclusions (Dobbs decision on abortion is exemplary—claims originalism leads inescapably to state-level abortion bans, when historical evidence is substantially contested). Suppression requirement rises from 0.25 to 0.42, reflecting the increasing energy required to maintain originalism against a growing scholarly and political challenge. Living constitutionalism has strengthened its institutional positions (dissenting opinions, law review dominance in some schools, Democratic appointment strategy), so originalism must more actively police the boundary against competing methodologies. All measurements share the 1987-2026 interval grid, so temporal analysis can examine whether extractiveness-theater-suppression move together (they do, suggesting a single underlying dynamic: institutional dominance requiring more performative and suppressive maintenance as it grows more extractive).
 *
 * PERSPECTIVAL GAP:
 *   From the originalist judiciary and federalism-advocate seats, the constraint is genuine coordination: it prevents judges from rewriting the Constitution to match contemporary politics, it respects the Framers' allocation of powers, and it provides a stable interpretive rule. From the victim seats (unenumerated rights claimants, federal regulatory advocates, civil rights constituencies), the same structure is extractive imposition: it forecloses doctrinal paths that would protect their interests, it privileges one historical narrative over alternative reconstructions, and it requires them to win supermajority amendment support to undo what they see as interpretive infidelity. The engine should compute this divergence per-seat directionality: beneficiary seats get d near 0.0 (low effective extraction); victim seats get d near 1.0 (high effective extraction). Excluded seats (living constitutionalists) sit at an ambiguous position—they are excluded from the legitimacy framework but retain institutional voice, so d depends on whether we weight their theoretical resources or their actual decision-power in the constraint's operation.
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist federal judiciary: d ≈ 0.35–0.45 (institutional power, constrained exit, sets rules but is constrained by prior precedent and must maintain the appearance of historical fidelity—not purely beneficiary, partly administrator). Federalism advocates: d ≈ 0.1–0.2 (beneficiary, powerful power, arbitrage-grade exit to other coalition strategies; they benefit but are not trapped). Religious liberty claimants: d ≈ 0.15–0.25 (beneficiary, but identity-locked to the legal framework and religiously motivated exit from constitutional claim-making is not genuine exit). Property rights defenders: d ≈ 0.1–0.2 (beneficiary, powerful, mobile exit to alternative regulatory strategies if originalism fails). Unenumerated rights claimants: d ≈ 0.75–0.85 (victim, identity-locked, no real exit from constitutional claim-making). Federal regulatory advocates: d ≈ 0.65–0.75 (victim, organized, constrained by the constitutional system). Civil rights constituencies: d ≈ 0.80–0.90 (victim, powerless + identity-locked, fully dependent on federal doctrine). Living constitutionalist scholars: d ≈ 0.55–0.65 (excluded but influential, not trapped in the sense of victims, but excluded from the dominant interpretive authority, which generates extraction-type pressure on their intellectual work).
 *
 * MANDATROPHY ANALYSIS:
 *   Originalism is classified as tangled_rope because it performs a genuine coordination function (fixing constitutional meaning to prevent interpretive chaos) while simultaneously extracting costs from constituencies that lose doctrinal pathways. The mandatrophy risk is whether the coordination function has atrophied relative to the extraction. If originalism is maintained primarily through political control of judicial appointments (which it is—appointment became central after the Reagan administration's systematic originalist vetting), then the coordination function weakens and the constraint approaches snare (pure extraction defended by a cover story). However, the coordination function is not yet dead: originalism does provide a publicly intelligible rule for interpretation, it does constrain judicial willfulness, and it does prevent the Constitution from meaning whatever a judicial majority prefers. The theater ratio rising suggests the coordination function is becoming increasingly performative relative to actual constraint, which is a mandatrophy signal. The comment's role is to flag this: originalism could degrade into Piton (atrophied coordination, maintained theatrically through appointment control, with no party actually benefiting enough to defend it and no party hurt enough to fix it) if the judicial and political investment required to maintain it grows beyond the actual benefits to beneficiary constituencies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    framers_intent_determinacy,
    'Is Framers'' intent a stable historical fact, or is it a construct inevitably shaped by the interpreter''s own methodological choices and prior assumptions about what counts as authentic historical evidence?',
    'Comparative historical analysis across originalist judges: if different originalists reach divergent conclusions about the same historical question despite claiming fidelity to intent, the instability of intent-based interpretation becomes empirically visible. Alternatively, direct engagement with the epistemology of historical reconstruction in constitutional law scholarship.',
    'If intent is indeterminate or irreducibly constructed, originalism''s claim to uniquely constrain judicial will collapses, and the constraint becomes a cover for one coalition''s preferred historical narrative. The classification would shift from tangled_rope (real coordination function + asymmetric extraction) toward snare (extraction defended by false naturalness).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framers_intent_determinacy, empirical, 'Whether originalist methodology successfully recovers a determinate historical meaning or imports contemporary preferences into historical reconstruction.').

omega_variable(
    original_public_meaning_vs_framers_intent_divergence,
    'When original public meaning and Framers'' intent diverge—when what the public understood the Constitution to mean differs from what the Framers intended—which standard does the originalist constraint privilege, and does the choice reflect methodological commitment or political outcome preference?',
    'Documented cases where originalists split on public meaning vs. intent and analysis of whether the choice correlates with outcomes favoring beneficiary or victim constituencies. The Dobbs decision''s handling of the abortion question (what did the public understand the Fourteenth Amendment to protect?) versus Framers'' likely intent (narrow) offers empirical data.',
    'If originalists systematically choose the meaning-source that favors beneficiaries, the constraint is not fidelity-based but outcome-driven, and extraction rises. If the choices vary and reflect genuine methodological wrestling, the constraint preserves its coordination function and extraction remains moderate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_public_meaning_vs_framers_intent_divergence, empirical, 'Whether originalist methodology contains internal mechanisms to resolve meaning-source conflicts, or whether choice-points systematically track political outcomes.').

omega_variable(
    kernel_reading_contest_resolution,
    'Is the originalist reading a stable methodological framework, or is it locked in mutual foreclosure with living constitutionalism and popular constitutionalism such that the contest can only be resolved by political control of judicial appointments, not by methodological argument within a shared framework?',
    'Analysis of cross-framework engagement in constitutional law scholarship and judiciary: do living constitutionalists and originalists genuinely argue each other''s premises, or do they inhabit incommensurable interpretive worlds? Can a judge move from one reading to another through reasoned argument, or only through ideological conversion? The institutional stability of the contest (no resolution in sight after 40+ years) suggests structural rather than argumentative barriers.',
    'If the readings coexist without rational resolution-paths, the contest is structural (political), and the constraint''s extractiveness reflects the fact that originalism benefits from institutional control of appointment rather than from demonstrated interpretive superiority. The theater ratio might understate the performative maintenance of originalism against a never-resolved methodological opponent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_resolution, conceptual, 'Whether the originalist constraint can be argued to validity or must be imposed through institutional dominance.').

omega_variable(
    civil_rights_expansion_causal_path,
    'Did civil rights victories (Voting Rights Act, Civil Rights Act, desegregation doctrine) depend specifically on rejecting originalist interpretation of the Fourteenth Amendment, or would originalists have reached similar results through their own methodology if the historical record were different or differently weighted?',
    'Originalist scholars'' own accounts of what the Fourteenth Amendment originally protected. If originalists argue the Amendment''s original meaning does encompass voting rights, desegregation, and equal protection as broad guarantees, then originalism does not necessarily foreclose civil rights expansion—extraction from that constituency is lower. If originalists maintain that the Amendment''s original meaning was narrower and contemporary protection requires evolution beyond originalism, then the victims'' cost is genuinely structural to the reading.',
    'If originalism can accommodate civil rights doctrine through its own methodology, the beneficiary/victim asymmetry softens. If originalism structurally forecloses the doctrinal basis for civil rights victories, the extraction from that constituency becomes a necessary feature of the constraint, not a choice-point within it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civil_rights_expansion_causal_path, empirical, 'Whether originalism necessarily narrows civil rights doctrine or whether different historical premises could support broad protection within originalist methodology.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_interpretive__originalist_reading, 1987, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1987, us_constitution_interpretive__originalist_reading, theater_ratio, 1987, 0.12).
narrative_ontology:measurement(us_c_tr_t1995, us_constitution_interpretive__originalist_reading, theater_ratio, 1995, 0.15).
narrative_ontology:measurement(us_c_tr_t2005, us_constitution_interpretive__originalist_reading, theater_ratio, 2005, 0.19).
narrative_ontology:measurement(us_c_tr_t2015, us_constitution_interpretive__originalist_reading, theater_ratio, 2015, 0.24).
narrative_ontology:measurement(us_c_tr_t2020, us_constitution_interpretive__originalist_reading, theater_ratio, 2020, 0.28).
narrative_ontology:measurement(us_c_tr_t2026, us_constitution_interpretive__originalist_reading, theater_ratio, 2026, 0.28).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1987, us_constitution_interpretive__originalist_reading, base_extractiveness, 1987, 0.35).
narrative_ontology:measurement(us_c_be_t1995, us_constitution_interpretive__originalist_reading, base_extractiveness, 1995, 0.42).
narrative_ontology:measurement(us_c_be_t2005, us_constitution_interpretive__originalist_reading, base_extractiveness, 2005, 0.48).
narrative_ontology:measurement(us_c_be_t2015, us_constitution_interpretive__originalist_reading, base_extractiveness, 2015, 0.54).
narrative_ontology:measurement(us_c_be_t2020, us_constitution_interpretive__originalist_reading, base_extractiveness, 2020, 0.58).
narrative_ontology:measurement(us_c_be_t2026, us_constitution_interpretive__originalist_reading, base_extractiveness, 2026, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1987, us_constitution_interpretive__originalist_reading, suppression_requirement, 1987, 0.25).
narrative_ontology:measurement(us_c_su_t1995, us_constitution_interpretive__originalist_reading, suppression_requirement, 1995, 0.32).
narrative_ontology:measurement(us_c_su_t2005, us_constitution_interpretive__originalist_reading, suppression_requirement, 2005, 0.38).
narrative_ontology:measurement(us_c_su_t2015, us_constitution_interpretive__originalist_reading, suppression_requirement, 2015, 0.41).
narrative_ontology:measurement(us_c_su_t2020, us_constitution_interpretive__originalist_reading, suppression_requirement, 2020, 0.42).
narrative_ontology:measurement(us_c_su_t2026, us_constitution_interpretive__originalist_reading, suppression_requirement, 2026, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_interpretive__originalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_interpretive__originalist_reading, 0.12).
narrative_ontology:affects_constraint(us_constitution_interpretive__originalist_reading, us_constitution_interpretive__living_constitution_reading).
narrative_ontology:affects_constraint(us_constitution_interpretive__originalist_reading, us_constitution_interpretive__popular_constitutionalism_reading).
narrative_ontology:affects_constraint(us_constitution_interpretive__originalist_reading, federal_enumerated_powers_scope).
narrative_ontology:affects_constraint(us_constitution_interpretive__originalist_reading, unenumerated_rights_protection_doctrine).
narrative_ontology:affects_constraint(us_constitution_interpretive__originalist_reading, fourteenth_amendment_remedial_scope).

% DUAL FORMULATION NOTE:
% Originalism and living constitutionalism are readings of the same constitutional kernel ('us_constitution_interpretive'). They differ fundamentally on what determines constitutional meaning: fixed historical intention vs. evolving contemporary understanding. Each reading generates a distinct constraint story with distinct beneficiaries, victims, and extraction profiles. They coexist as empirical positions held by different institutional actors (originalist and progressive judicial coalitions) but foreclose each other logically—they cannot both be true as accounts of what the Constitution means, though they can both be held by different parties in the same system. The constraint family structure maps the dependencies: originalist victories constraining federal power affect downstream doctrinal constraints on enumerated authority, unenumerated rights, and Fourteenth Amendment scope. Living constitutionalist victories would reverse those effects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_interpretive__originalist_reading, institutional, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
