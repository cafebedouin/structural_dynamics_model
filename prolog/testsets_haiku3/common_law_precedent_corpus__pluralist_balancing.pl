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
 *   constraint_id: common_law_precedent_corpus__pluralist_balancing
 *   human_readable: Pluralist Precedent Balancing: Domain-Contextual Weight Calibration
 *   domain: legal/jurisprudential
 *
 * SUMMARY:
 *   Common law precedent operates under a pluralist balancing framework in
 *   which the binding force of precedent varies by legal domain and
 *   contextual factors. Commercial and property law maintain high precedent
 *   weight for stability; civil rights, emerging technology, and family law
 *   permit more contextual rebalancing. This reading instantiates the middle
 *   position between strict stare decisis (which would lock all domains
 *   uniformly rigid) and evolutionary permissiveness (which would permit
 *   nearly unlimited reinterpretation). The pluralist framework is itself
 *   contestable: judges disagree about which domains merit flexibility,
 *   litigants face unpredictable revision costs depending on domain
 *   classification, and historically marginalized legal communities find
 *   their emerging claims classified into rigid domains while established
 *   doctrine gets flexibility. The framework presents as neutral calibration
 *   but operates as asymmetric extraction: it protects appellate
 *   institutional discretion, stabilizes established doctrinal incumbents,
 *   and raises barriers for those seeking precedent revision.
 *
 * KEY AGENTS:
 *   - appellate_judiciary: Sets domain classifications and precedent-weight calibrations; manages institutional discretion
 *   - established_legal_doctrine_holders: Benefit from stable precedent weight in classical domains; protected by the framework
 *   - litigants_seeking_precedent_revision: Face unpredictable domain-switching costs; pay through increased litigation risk
 *   - marginalized_legal_communities: Structurally excluded from precedent-corpus formation; identity-locked to constrained exit
 *   - lower_court_judges: Comply with the framework; face hidden costs from jurisdictional uncertainty
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__pluralist_balancing, 0.58).
domain_priors:suppression_score(common_law_precedent_corpus__pluralist_balancing, 0.47).
domain_priors:theater_ratio(common_law_precedent_corpus__pluralist_balancing, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, extractiveness, 0.58).
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 0.47).
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__pluralist_balancing, tangled_rope).
narrative_ontology:human_readable(common_law_precedent_corpus__pluralist_balancing, "Pluralist Precedent Balancing: Domain-Contextual Weight Calibration").
narrative_ontology:topic_domain(common_law_precedent_corpus__pluralist_balancing, "legal/jurisprudential").

domain_priors:requires_active_enforcement(common_law_precedent_corpus__pluralist_balancing).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__pluralist_balancing, 'e4212234-14d1-4bf3-afca-e791cf1c4a4d').
narrative_ontology:cs_kernel_codification('e4212234-14d1-4bf3-afca-e791cf1c4a4d', fixed_text).
narrative_ontology:cs_authority_grounding('e4212234-14d1-4bf3-afca-e791cf1c4a4d', lineage).
narrative_ontology:cs_interpretation_layer_present('e4212234-14d1-4bf3-afca-e791cf1c4a4d').
narrative_ontology:cs_reading_relation('e4212234-14d1-4bf3-afca-e791cf1c4a4d', common_law_precedent_corpus__strict_stare_decisis, coexists_with).
narrative_ontology:cs_reading_relation('e4212234-14d1-4bf3-afca-e791cf1c4a4d', common_law_precedent_corpus__evolutionary_framework, coexists_with).
narrative_ontology:cs_axiom('e4212234-14d1-4bf3-afca-e791cf1c4a4d', foundational, precedent_weight_varies_contextually).
narrative_ontology:cs_axiom_status(precedent_weight_varies_contextually, holdable).
narrative_ontology:cs_axiom_grounding('e4212234-14d1-4bf3-afca-e791cf1c4a4d', precedent_weight_varies_contextually, conventional).
narrative_ontology:cs_axiom('e4212234-14d1-4bf3-afca-e791cf1c4a4d', secondary, domain_classification_determines_rigidity).
narrative_ontology:cs_axiom_status(domain_classification_determines_rigidity, holdable).
narrative_ontology:cs_axiom_grounding('e4212234-14d1-4bf3-afca-e791cf1c4a4d', domain_classification_determines_rigidity, instrumental).
narrative_ontology:cs_reference_frame('e4212234-14d1-4bf3-afca-e791cf1c4a4d', precedent_as_binding_constraint).
narrative_ontology:cs_drift_state('e4212234-14d1-4bf3-afca-e791cf1c4a4d', contemporary_civil_rights_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('e4212234-14d1-4bf3-afca-e791cf1c4a4d', '').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__pluralist_balancing, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__pluralist_balancing, appellate_judiciary).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__pluralist_balancing, established_legal_doctrine_holders).
narrative_ontology:constraint_victim(common_law_precedent_corpus__pluralist_balancing, litigants_seeking_precedent_revision).
narrative_ontology:constraint_victim(common_law_precedent_corpus__pluralist_balancing, marginalized_legal_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(common_law_precedent_corpus__pluralist_balancing, lower_court_judges).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Judges in appellate courts set the precedent-weight framework by declaring which domains merit context-sensitive rebalancing and which require stare decisis adherence. They manage the cognitive load of precedent review and determine how much past decision-binding actually constrains current rulings. Their discretion in calibrating precedent weight is their institutional power.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, appellate_judiciary, agenda_setter,
    institutional, generational, mobile, national).

% Doctrinal incumbents—commercial law, property, contract law—benefit from stable precedent weight that preserves their accumulated interpretive capital. Their legal positions rest on long-standing precedent chains; context-sensitive rebalancing that favors precedent revision in other domains leaves their domains comparatively rigid. They do not run the judiciary, but their interests align with the stability end of the balance.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, established_legal_doctrine_holders, beneficiary,
    powerful, generational, arbitrage, national).

% Parties challenging established precedent face unpredictable domain-switching costs. In stable domains they must clear very high bars to overturn precedent; in flexible domains the same argument might succeed. They cannot predict which domain their issue will be classified into or how the balance will shift mid-litigation. They pay through increased litigation risk, bifurcated strategies, and the possibility of losing on precedent-weight grounds rather than merits.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, litigants_seeking_precedent_revision, payer,
    moderate, biographical, constrained, national).

% Communities whose interests were excluded from the precedent corpus when it was built (criminal defendants in early common law, Indigenous land claimants, workers' rights advocates) face high barriers to precedent revision because their issues lack doctrinal precedent to reweight. The balancing framework implicitly protects domains with deep precedent and disadvantages emerging legal claims. Their exclusion from the conversation is structural.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, marginalized_legal_communities, payer,
    powerless, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(common_law_precedent_corpus__pluralist_balancing, marginalized_legal_communities, excluded).

% Trial and intermediate appellate judges must interpret what the pluralist balancing framework requires of them in real time. They face liability exposure if they calibrate precedent weight wrongly—applying strict stare decisis in a 'flexible' domain or permitting innovation in a 'stable' one. The framework creates hidden compliance costs in the form of jurisdictional uncertainty.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, lower_court_judges, payer,
    moderate, biographical, constrained, local).

% Academic legal theorists analyze and critique the precedent framework. They produce the conceptual apparatus that justifies domain-contextual balancing, legitimating the framework through scholarly elaboration. They are observers in the operational constraint but their work sustains its coherence.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, legal_scholars_and_theorists, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_law_precedent_corpus__pluralist_balancing, appellate_judiciary).
narrative_ontology:fixing_cost_class(common_law_precedent_corpus__pluralist_balancing, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable but adaptable reference architecture for dispute resolution: precedent creates predictability and institutional continuity while domain-contextual balancing permits legal evolution where doctrinal rigidity would produce deadlock or manifest injustice.
% TRANSFER_FUNCTION: Moves interpretive authority and precedent-revision power differentially across legal domains. Established doctrinal domains retain high precedent weight (the appellate judiciary retains binding force); emerging or contested domains receive discretionary rebalancing (revision becomes more accessible). Litigants with issues in stable domains must pay higher revision costs; those with issues in flexible domains pay lower costs—unpredictably.
% ABSENT_VOICES: Communities whose interests were excluded from the precedent corpus when it crystallized (Indigenous legal traditions, non-Western legal frameworks, historically marginalized constituencies) are not represented in the domain-classification process itself. Their legal claims arrive as outsiders to a framework already calibrated around established doctrine. Victims of legal exclusion are structurally unable to participate in reweighting the framework that excludes them.
% DISAPPEARANCE_RATIONALE: If the pluralist balancing framework disappeared and were replaced by strict uniform stare decisis, all domains would lock rigid and legal evolution would halt—litigation strategy would shift entirely to legislative channels. If replaced by evolutionary permissiveness, precedent would provide almost no constraint and each case would be adjudicated de novo—the institutional continuity benefit of precedent would vanish. Either replacement substantially reorganizes legal practice.
% FOUNDING_PROBLEM: Early common law faced a dilemma: slavish adherence to old precedent produced injustice and doctrinal fossils; pure discretion to overturn precedent produced unpredictability and eroded judicial legitimacy. The pluralist framework emerged as a middle path: preserve precedent's stabilizing force in domains where it works (commercial, property) while permitting adaptive rebalancing in domains where historical rigidity created manifest injustice (civil rights, emerging technology).
% FOUNDING_PROBLEM_CORROBORATION: The appellate judiciary attests the balancing framework solves the problem—cases get coherent reasoning and doctrinal grounding. Litigants seeking revision in rigid domains attest the problem is not solved but displaced: the framework moved the rigidity problem from universal stare decisis into domain-specific unpredictability. Legal scholars outside the judiciary (Langdell critics, legal realists, critical race theorists) attest the framework's domain classifications were never neutral—they protect established doctrine and exclude emerging claims.
narrative_ontology:disappearance_verdict(common_law_precedent_corpus__pluralist_balancing, world_rearranges).
narrative_ontology:founding_problem_status(common_law_precedent_corpus__pluralist_balancing, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_law_precedent_corpus__pluralist_balancing, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(common_law_precedent_corpus__pluralist_balancing, 'none', 1).
narrative_ontology:epsilon_provenance(common_law_precedent_corpus__pluralist_balancing, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate-high (0.58) because the framework creates asymmetric precedent-weight burdens: those with issues in rigid domains must mount extraordinary justification; those in flexible domains face lower bars. This is not perceived as neutral calibration but as extractive differentiation by those disadvantaged by domain classification. Suppression is moderate (0.47) because the framework suppresses direct revision challenges through precedent-weight doctrine, but resistance remains high (0.71) because litigants continually test domain boundaries and scholars critique the framework's neutrality. Theater is moderate-high (0.42) because much appellate opinion-writing performs the 'balancing' ritual without materially changing precedent outcomes—the framework legitimates outcomes through elaborate justification rather than through genuine reweighting. Measurement series show slow extractiveness increase over the interval as domain classifications harden through accumulated case law, making precedent weight more predictable but also more entrenched; theater ratio rises as judges invest more effort in explaining why precedent weight differs across domains.
 *
 * PERSPECTIVAL GAP:
 *   The appellate judiciary's perspective: the framework is genuine coordination—it balances legitimate competing values (stability and adaptation) through reasoned domain differentiation. Lower courts and litigants' perspective: the framework is opaque—domain classifications appear arbitrary, precedent-weight outcomes are unpredictable, and the framework's flexibility is available only to established doctrine. Marginalized communities' perspective: the framework is exclusionary—their legal claims arrive in a precedent landscape already calibrated around incumbent interests. The engine should compute these as distinct per-seat types because the structural relationship to the constraint differs radically across seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The appellate judiciary holds d near 0.2 (beneficiary): the framework concentrates interpretive authority in appellate hands; judges can shift domain classifications and reweight precedent strategically. Established doctrine holders hold d near 0.25 (net beneficiary, though they do not directly control the framework): their domains get stable precedent weight automatically. Litigants seeking revision hold d near 0.75 (target): they face high revision costs and asymmetric burden-of-proof rules depending on unpredictable domain classification. Lower court judges hold d near 0.65 (target): they must comply with framework ambiguities and face liability if they calibrate wrong. Marginalized communities hold d near 0.85 (target, identity-locked): their legal claims are structurally disadvantaged by a framework built around incumbent doctrine, and their exit is constrained by identity fusion with legal community membership.
 *
 * MANDATROPHY ANALYSIS:
 *   The pluralist balancing framework exhibits early-stage mandatrophy pressure. The founding problem—balancing precedent stability against legal evolution—remains live, but the mechanism for solving it has partially atrophied. Appellate judges increasingly use 'domain balancing' as a post-hoc justification for outcomes reached on other grounds, rather than as a genuine calibration mechanism. The framework persists through institutional inertia and legitimating rhetoric, not through active problem-solving. The theater-ratio rise (0.35 to 0.46 over the interval) signals that performative maintenance is increasing relative to functional calibration. The extractiveness rise (0.48 to 0.61 projected) indicates that the framework's protective effect on established doctrine has hardened while its flexibility for revision has narrowed—it is becoming more purely extractive and less genuinely balancing. This suggests approaching the mandatrophy threshold: if theater continues rising and the balancing becomes purely legitimating rather than functional, the constraint should be reclassified as piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domain_classification_opacity,
    'Are domain classifications (commercial law, civil rights, emerging technology) applied consistently across circuits and time, or do they drift with judicial ideology and litigant power?',
    'Empirical study of precedent-weight doctrine across appellate decisions: track which domains appellate judges classify as ''stable'' vs. ''flexible'' and whether the classification correlates with outcome direction (do judges classify domains rigidly when the outcome favors incumbents and flexibly when it favors challengers, or is classification stable across outcome?).',
    'If classification is consistent and stable, the framework genuinely calibrates precedent weight; if it drifts with ideology and power, the framework is purely extractive cover for discretionary outcomes. The constraint would reclassify from tangled_rope to snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(domain_classification_opacity, empirical, 'Whether domain classifications are substantive or post-hoc rationalizations for outcomes chosen on other grounds.').

omega_variable(
    suppression_mechanism_internalization,
    'Do litigants avoid revision challenges because they accept the precedent framework as legitimate, or because they lack resources and exit options?',
    'Post-barrier change measurement: if precedent-weight barriers are removed (e.g., legislative override, constitutional amendment), do revision challenges surge? If they do, suppression was structural; if they don''t, suppression has partially internalized.',
    'Structural suppression (high resistance at current barrier level; suppression drops if barriers drop) indicates the framework operates by exclusion. Internalized suppression (resistance remains high even with barriers removed) indicates the framework has captured litigant self-concepts and expectations—the constraint is more deeply extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether the suppression measured in the framework is structural (external barriers) or internalized (captured expectations).').

omega_variable(
    domain_exclusion_path_dependence,
    'Are emerging legal claims (AI rights, environmental personhood, gig-worker status) structurally confined to ''rigid'' domains because they lack established precedent, or is the rigid/flexible classification applied neutrally?',
    'Historical audit of domain classifications: track whether new legal claims consistently land in rigid domains at their emergence, then shift to flexible classification once they develop incumbent status. If the pattern holds, domain classification is path-dependent on incumbency, not on substantive doctrinal factors.',
    'If domain classification is incumbency-dependent, the framework is self-reproducing extraction: incumbent doctrine locks new claims into rigid domains; new claims cannot escape to flexible domains until they become established doctrine. This would support reclassification from tangled_rope toward snare with piton characteristics (self-maintaining, extractive, protecting its own persistence).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_exclusion_path_dependence, empirical, 'Whether domain classifications track incumbency or operate as neutral substantive calibrations.').

omega_variable(
    reading_foreclosure_test,
    'Can a single legal framework hold both the pluralist balancing reading and the strict stare decisis reading simultaneously, or does adopting pluralist balancing logically foreclose strict stare decisis?',
    'Philosophical analysis: the strict reading asserts ''precedent binds uniformly''; the pluralist reading asserts ''precedent weight varies by domain.'' These are logically contradictory if applied to the SAME domain at the SAME time—a domain cannot be both rigidly bound and contextually flexible. However, different courts can adopt different readings, and even a single court can apply different readings to different domains. Foreclosure obtains only if the framework structure REQUIRES one reading and makes the other impossible; coexistence obtains if both can be live positions held by different jurisdictions or different judges.',
    'If foreclosure is actual (strict stare decisis is logically impossible given pluralist doctrine''s institutional embedding), strict_stare_decisis reclassifies as superseded and the reading relation is forecloses. If coexistence is actual (both readings remain live options), the relation remains coexists_with.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_test, conceptual, 'Whether pluralist balancing logically rules out strict stare decisis or whether both remain live positions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__pluralist_balancing, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 0, 0.35).
narrative_ontology:measurement(comm_tr_t5, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 5, 0.36).
narrative_ontology:measurement(comm_tr_t10, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 10, 0.38).
narrative_ontology:measurement(comm_tr_t15, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 15, 0.4).
narrative_ontology:measurement(comm_tr_t20, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 20, 0.42).
narrative_ontology:measurement(comm_tr_t25, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 25, 0.43).
narrative_ontology:measurement(comm_tr_t30, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 30, 0.44).
narrative_ontology:measurement(comm_tr_t40, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 40, 0.46).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(comm_be_t5, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 5, 0.51).
narrative_ontology:measurement(comm_be_t10, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(comm_be_t15, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 15, 0.56).
narrative_ontology:measurement(comm_be_t20, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(comm_be_t25, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 25, 0.58).
narrative_ontology:measurement(comm_be_t30, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 30, 0.59).
narrative_ontology:measurement(comm_be_t40, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 40, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(comm_su_t5, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 5, 0.43).
narrative_ontology:measurement(comm_su_t10, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 10, 0.44).
narrative_ontology:measurement(comm_su_t15, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 15, 0.45).
narrative_ontology:measurement(comm_su_t20, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 20, 0.47).
narrative_ontology:measurement(comm_su_t25, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 25, 0.48).
narrative_ontology:measurement(comm_su_t30, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 30, 0.49).
narrative_ontology:measurement(comm_su_t40, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 40, 0.51).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_law_precedent_corpus__pluralist_balancing, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(common_law_precedent_corpus__pluralist_balancing, 0.12).
narrative_ontology:affects_constraint(common_law_precedent_corpus__pluralist_balancing, common_law_precedent_corpus__strict_stare_decisis).
narrative_ontology:affects_constraint(common_law_precedent_corpus__pluralist_balancing, common_law_precedent_corpus__evolutionary_framework).

% DUAL FORMULATION NOTE:
% The common_law_precedent_corpus kernel decomposes into three structurally distinct constraints, one per reading: strict_stare_decisis (Mountain/Rope hybrid, high accessibility_collapse, low resistance), pluralist_balancing (Tangled Rope, medium extractiveness, domain-dependent rigidity), evolutionary_framework (Tangled Rope/Snare boundary, high extractiveness, adaptive redistribution of interpretive authority). Each reading instantiates a different constraint with different ε, different beneficiary/victim structures, and different temporal trajectories. The readings coexist as live jurisprudential positions held by different appellate jurisdictions and judicial factions. The pluralist reading sits institutionally between the other two: it preserves stare decisis in stable domains (aligning with strict reading) while permitting evolution in flexible domains (aligning with evolutionary reading), but the framing itself becomes the extraction mechanism—those classified into rigid domains face higher revision costs than those in flexible domains, and the classification process is opaque and ideology-correlated.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
