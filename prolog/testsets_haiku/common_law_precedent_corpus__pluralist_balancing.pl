% ============================================================================
% CONSTRAINT STORY: common_law_precedent_corpus__pluralist_balancing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_law_precedent_corpus__pluralist_balancing, []).

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
 *   constraint_id: common_law_precedent_corpus__pluralist_balancing
 *   human_readable: Common Law Precedent Corpus: Pluralist Balancing Reading
 *   domain: legal/constitutional
 *
 * SUMMARY:
 *   The common law precedent corpus governs how prior judicial decisions
 *   constrain or permit future decisions. This is ONE reading of that
 *   contested kernel: the pluralist balancing reading holds that precedent
 *   weight varies by domain and context—high rigidity in stable,
 *   well-crystallized fields (constitutional property, established contract
 *   law), lower rigidity in novel domains (emerging technology regulation,
 *   new civil rights frameworks). This reading creates a tangled rope
 *   structure: genuine coordination function (preventing doctrinal whipsaw,
 *   protecting reliance) coupled with asymmetric extraction (litigants in
 *   novel domains bear unpredictability costs; appellate judiciary benefits
 *   from discretion to manage change). The constraint is CLAIMED as
 *   tangled_rope; the metrics describe substantially extractive operation
 *   with moderate suppression (the opacity of domain-specific precedent
 *   weight acts as suppression mechanism), and rising theater ratio
 *   (increasing proportion of opinions justify precedent weight choice ex
 *   post rather than applying transparent rules ex ante).
 *
 * KEY AGENTS:
 *   - Appellate Judiciary: Institutional agenda-setter; controls precedent weight calculus; benefits from flexibility to reinterpret doctrine in selected domains.
 *   - Established Doctrines: Institutional beneficiary; protected by presumption of high precedent weight in crystallized fields; stabilized by the constraint.
 *   - Litigants in Novel Domains: Moderate-power payers; face unpredictable precedent burden in domains where weight is context-dependent; higher preparation cost due to doctrinal volatility.
 *   - Marginalized Legal Traditions: Powerless victims; trapped by weight doctrine that privileges canonical traditions; perpetually higher burden to establish legitimacy.
 *   - Trial Courts: Moderate-power dual-positioned; benefit from appellate guidance in salient domains; bear cost of opacity in emerging areas; must predict appellate reasoning about weight.
 *   - Legal Academy: Organized observers; produce jurisprudential theory; influence judicial reasoning and litigant strategy over generational timescale.
 *   - Legislative Bodies: Institutional excluded; structurally locked out of precedent doctrine by common-law tradition self-governance assumption.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__pluralist_balancing, 0.58).
domain_priors:suppression_score(common_law_precedent_corpus__pluralist_balancing, 0.52).
domain_priors:theater_ratio(common_law_precedent_corpus__pluralist_balancing, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, extractiveness, 0.58).
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__pluralist_balancing, tangled_rope).
narrative_ontology:human_readable(common_law_precedent_corpus__pluralist_balancing, "Common Law Precedent Corpus: Pluralist Balancing Reading").
narrative_ontology:topic_domain(common_law_precedent_corpus__pluralist_balancing, "legal/constitutional").

domain_priors:requires_active_enforcement(common_law_precedent_corpus__pluralist_balancing).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__pluralist_balancing, '4ab57f28-5a95-43ea-b8d7-7d85b9703929').
narrative_ontology:cs_kernel_codification('4ab57f28-5a95-43ea-b8d7-7d85b9703929', distributed).
narrative_ontology:cs_authority_grounding('4ab57f28-5a95-43ea-b8d7-7d85b9703929', lineage).
narrative_ontology:cs_interpretation_layer_present('4ab57f28-5a95-43ea-b8d7-7d85b9703929').
narrative_ontology:cs_reading_relation('4ab57f28-5a95-43ea-b8d7-7d85b9703929', common_law_precedent_corpus__strict_stare_decisis, coexists_with).
narrative_ontology:cs_reading_relation('4ab57f28-5a95-43ea-b8d7-7d85b9703929', common_law_precedent_corpus__evolutionary_framework, influences).
narrative_ontology:cs_axiom('4ab57f28-5a95-43ea-b8d7-7d85b9703929', foundational, domain_context_determines_weight).
narrative_ontology:cs_axiom_status(domain_context_determines_weight, holdable).
narrative_ontology:cs_axiom_grounding('4ab57f28-5a95-43ea-b8d7-7d85b9703929', domain_context_determines_weight, conventional).
narrative_ontology:cs_axiom('4ab57f28-5a95-43ea-b8d7-7d85b9703929', foundational, incremental_doctrinal_change_permitted).
narrative_ontology:cs_axiom_status(incremental_doctrinal_change_permitted, holdable).
narrative_ontology:cs_axiom_grounding('4ab57f28-5a95-43ea-b8d7-7d85b9703929', incremental_doctrinal_change_permitted, deontological).
narrative_ontology:cs_reference_frame('4ab57f28-5a95-43ea-b8d7-7d85b9703929', common_law_adaptive_tradition).
narrative_ontology:cs_drift_state('4ab57f28-5a95-43ea-b8d7-7d85b9703929', contemporary_legal_pluralism_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4ab57f28-5a95-43ea-b8d7-7d85b9703929', '').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__pluralist_balancing, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__pluralist_balancing, appellate_judiciary).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__pluralist_balancing, established_doctrines).
narrative_ontology:constraint_victim(common_law_precedent_corpus__pluralist_balancing, litigants_in_novel_domains).
narrative_ontology:constraint_victim(common_law_precedent_corpus__pluralist_balancing, marginalized_legal_traditions).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_law_precedent_corpus__pluralist_balancing, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(common_law_precedent_corpus__pluralist_balancing, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_law_precedent_corpus__pluralist_balancing_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_law_precedent_corpus__pluralist_balancing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_law_precedent_corpus__pluralist_balancing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures 0.58 at interval end, indicating substantial extraction. The trajectory rises from 0.42 to 0.58 over the interval, suggesting accumulating extraction as domains proliferate and precedent weight opacity increases (new tech, new social arrangements create unpredictability litigants must absorb). Suppression measures 0.52: the opacity of domain-specific weight standards acts as suppression—litigants cannot fully anticipate how appellate courts will weight precedent in their domain without costly litigation testing the boundaries. Theater ratio measures 0.41 at interval end, having risen from 0.28, indicating increased proportional performativity: appellate opinions increasingly invoke 'balancing' and 'context-sensitivity' language to justify precedent choices that often seem post-hoc (rationalizing domain assignments after doctrinal direction is chosen). Measurement series share one time grid: every metric is authored at every examined point (0, 5, 10, 15, 20, 25, 30, 40), enabling temporal drift analysis. The plateau in extractiveness from t=25 to t=40 reflects stabilization: the constraint reaches equilibrium as domain-weight mappings become semi-familiar to litigants, reducing volatility surprises, though unpredictability remains structural.
 *
 * PERSPECTIVAL GAP:
 *   The appellate judiciary and litigants-in-novel-domains would experience this constraint divergently from structural perspective. From the judiciary's seat, pluralist balancing is adaptive governance—precision in stable domains, flexibility where doctrine needs evolution. From the novel-domain litigant's seat, it is enforced ambiguity: they cannot predict how their domain will be weighted without costly exploration. The engine captures this in per-seat classification: judicial seat perceives coordination (rope-like), litigant seat perceives extraction (snare-like). The true classification is tangled rope because both perceptions are structurally real.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: appellate judiciary (controls weight calculus, benefits from flexibility), established doctrines (protected by high presumptive weight in stable domains). Victims: litigants-in-novel-domains (unpredictability cost), marginalized-legal-traditions (perpetually higher burden). The directionality for appellate judiciary is near 0.0 (full beneficiary): they set the rules, collect the benefit (doctrinal control), and have lowest exit cost (they exit by overruling or reinterpreting). Directionality for litigants-in-novel-domains is near 1.0 (full target): they face unpredictability, constrained exit (the domain is where they practice), and bear costs without setting rules. Trial courts sit near 0.5 (symmetric): they benefit from appellate guidance in some domains, bear cost of opacity in others. The engine derives directionality from beneficiary/victim declarations and exit modulation; no override is needed because the structural data accurately encodes the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents misclassification in two directions: (1) It is not a pure rope (genuine coordination without asymmetry) because the precedent weight opacity creates real costs for certain seats that do not participate in setting the rules. A pure rope would permit exit or offer symmetric benefit; litigants-in-novel-domains have neither. (2) It is not a pure snare (extraction with coordination cover story) because the stabilization of doctrine in high-salience fields is a genuine coordination good—preventing constitutional law from oscillating protects reliance interests and enables coherent rights development. Tangled rope is the correct classification: real coordination function (preventing doctrinal whipsaw) married to asymmetric extraction (cost borne by those not setting the weight rules). The classification also prevents the constraint from being read as a piton (inert maintenance with no beneficiary), because appellate judiciary actively benefits from flexibility and actively enforce domain-specific weight through opinion doctrine.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domain_classification_opacity,
    'How is a domain''s classification (stable vs. emerging) determined, and by whom? Is the classification itself subject to precedent, or can appellate courts unilaterally shift a domain between weight tiers?',
    'Empirical analysis of domain reclassification: track appellate decisions where a previously stable domain is treated as emergent (e.g., digital privacy treated as novel despite analogy to wiretapping law), and identify the criteria courts deploy. Examine whether criteria are transparent in advance or justified post-hoc.',
    'If domain classification is opaque and unilateral, extractiveness increases (litigants cannot predict domain assignment). If criteria are transparent and stable, extractiveness decreases and suppression drops (litigants can predict precedent weight). The distinction between a tangled rope and a snare turns on this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_classification_opacity, empirical, 'Whether domain classification is transparent and rule-governed or opaque and discretionary.').

omega_variable(
    pluralist_vs_strict_foreclosure,
    'Does this reading (pluralist balancing) logically foreclose the strict_stare_decisis reading, or do they coexist as genuinely available positions that different judges can hold?',
    'Jurisprudential analysis: if a judge can consistently hold that ''precedent weight varies by domain'' and another can hold ''precedent weight is uniform and high,'' without either violating logical coherence, they coexist. If one position requires denying premises the other depends on, one forecloses the other.',
    'If strict_stare_decisis is foreclosed by pluralist balancing, the constraint should reclassify as partly mountain-like (one reading is ruled out by physics/logic, not by choice). If they coexist, the constraint remains a tangled rope where different judicial factions hold different readings. The corpus design depends on this: do we model two separate constraints (pluralist and strict) or do we model the kernel and its competing readings?',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pluralist_vs_strict_foreclosure, conceptual, 'Whether pluralist balancing logically forecloses or coexists with strict stare decisis within a coherent framework.').

omega_variable(
    marginalized_traditions_suppression_mechanism,
    'Is the marginalization of non-canonical legal traditions (indigenous law, critical jurisprudence) a structural feature of the precedent weight doctrine, or a contingent historical artifact of common-law tradition?',
    'Comparative jurisprudence: examine whether constraints on non-canonical precedent exist in systems that explicitly value pluralist legal traditions (e.g., South African constitutional law post-apartheid, plurinational Latin American constitutions). If non-canonical traditions can be integrated without structural barrier, the suppression is contingent and remediable. If integration requires overturning precedent doctrine itself, suppression is structural.',
    'If suppression of marginalized traditions is structural, the constraint''s victims list and suppression score are correctly identified; the constraint is a genuine snare relative to non-canonical traditions. If contingent, remedies (explicit precedent hierarchy for pluralist traditions, constitutional recognition of indigenous law) could lower suppression without overturning the constraint. Impacts classification toward piton (inertia) vs. snare (engineered exclusion).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginalized_traditions_suppression_mechanism, empirical, 'Whether non-canonical legal traditions are structurally suppressed by precedent doctrine or merely historically marginalized.').

omega_variable(
    appellate_coordination_vs_appellate_extraction,
    'How much of the extractiveness measured in this constraint is genuine coordination cost (the necessity of appellate discretion to manage doctrine), and how much is appellate judicial self-dealing (keeping flexibility that serves the judiciary''s institutional interests more than litigants'')?',
    'Doctrinal analysis coupled with empirical outcome study: compare domains where appellate courts have adopted explicit, transparent precedent-weight rules with domains relying on opaque balancing. If transparent-rule domains show lower litigant preparation cost and stable outcomes, the opacity is self-interested extraction, not necessary coordination cost. If transparent-rule domains show doctrinal rigidity harm (inability to correct injustice), opacity is necessary coordination.',
    'If extraction is self-interested, the constraint reclassifies toward snare (enforcement is primarily suppressing alternatives that would reduce judicial flexibility). If extraction is necessary coordination cost, the tangled rope classification is correct. The distinction shifts fixing cost and policy recommendations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(appellate_coordination_vs_appellate_extraction, empirical, 'The balance between genuine coordination necessity and institutional self-interest in precedent opacity.').

omega_variable(
    reading_contest_temporal_dynamics,
    'Is the contest between strict_stare_decisis and pluralist_balancing readings itself subject to precedent dynamics, or is it a meta-level jurisprudential debate that sits outside the precedent corpus?',
    'Historical analysis of how the two readings compete over time. Track appellate opinions that cite precedent for their precedent-weight doctrine itself (e.g., citing prior stare decisis holdings to justify contemporary precedent practice). If the readings have precedent precedent (precedent for how to treat precedent), the contest is recursive and the precedent corpus governs itself. If the contest is purely philosophical, it is outside the constraint.',
    'If readings are subject to precedent precedent, then strict_stare_decisis establishes itself through its own logic (prior stare decisis precedents constrain future precedent doctrine), creating a self-enforcing stability. Pluralist balancing would then be a challenge to precedent precedent itself—reclassifying from coexists_with to forecloses (or partially forecloses). This feeds back into the constraint''s classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_temporal_dynamics, conceptual, 'Whether the contest between readings is recursive (governed by precedent) or meta-level.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__pluralist_balancing, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 0, 0.28).
narrative_ontology:measurement(comm_tr_t5, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 5, 0.33).
narrative_ontology:measurement(comm_tr_t10, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 10, 0.37).
narrative_ontology:measurement(comm_tr_t15, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 15, 0.4).
narrative_ontology:measurement(comm_tr_t20, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 20, 0.41).
narrative_ontology:measurement(comm_tr_t25, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 25, 0.42).
narrative_ontology:measurement(comm_tr_t30, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 30, 0.42).
narrative_ontology:measurement(comm_tr_t40, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(comm_be_t5, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(comm_be_t10, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 10, 0.53).
narrative_ontology:measurement(comm_be_t15, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 15, 0.56).
narrative_ontology:measurement(comm_be_t20, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(comm_be_t25, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 25, 0.59).
narrative_ontology:measurement(comm_be_t30, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 30, 0.59).
narrative_ontology:measurement(comm_be_t40, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(comm_su_t5, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 5, 0.43).
narrative_ontology:measurement(comm_su_t10, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 10, 0.47).
narrative_ontology:measurement(comm_su_t15, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 15, 0.5).
narrative_ontology:measurement(comm_su_t20, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 20, 0.51).
narrative_ontology:measurement(comm_su_t25, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 25, 0.52).
narrative_ontology:measurement(comm_su_t30, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 30, 0.53).
narrative_ontology:measurement(comm_su_t40, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_law_precedent_corpus__pluralist_balancing, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(common_law_precedent_corpus__pluralist_balancing, 0.12).
narrative_ontology:affects_constraint(common_law_precedent_corpus__pluralist_balancing, common_law_precedent_corpus__strict_stare_decisis).
narrative_ontology:affects_constraint(common_law_precedent_corpus__pluralist_balancing, common_law_precedent_corpus__evolutionary_framework).
narrative_ontology:affects_constraint(common_law_precedent_corpus__pluralist_balancing, constitutional_amendment_precedent_weight).
narrative_ontology:affects_constraint(common_law_precedent_corpus__pluralist_balancing, judicial_discretion_legitimacy).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested common_law_precedent_corpus kernel. The strict_stare_decisis reading enforces uniform high precedent weight (lower extraction, higher accessibility collapse, less flexibility). The evolutionary_framework reading permits widespread reinterpretation in light of contemporary evolution (higher extraction, lower accessibility collapse, high flexibility). This pluralist_balancing reading splits the difference: high precedent weight where doctrine is crystallized, lower weight where new conditions demand adaptation. Each reading instantiates a different constraint with different ε, beneficiary/victim structure, and stakeholder experience. The three readings coexist as live positions held by different judicial factions and jurisprudential schools; they are linked via network.affects_constraints to enable contamination propagation analysis—if one reading's legitimacy degrades, the others shift predictably.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(common_law_precedent_corpus__pluralist_balancing, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
