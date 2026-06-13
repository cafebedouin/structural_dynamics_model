% ============================================================================
% CONSTRAINT STORY: constitutional_interpretive_authority__parliamentary_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_interpretive_authority__parliamentary_supremacy_reading, []).

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
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: constitutional_interpretive_authority__parliamentary_supremacy_reading
 *   human_readable: Parliamentary Supremacy in Constitutional Interpretation
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   This story instantiates the parliamentary supremacy reading of the
 *   contested constitutional kernel: interpretive authority over fundamental
 *   law rests exclusively with the elected legislature; courts possess no
 *   power to nullify parliamentary acts or override legislative
 *   constitutional interpretation. This reading claims to ground legitimacy
 *   in electoral mandate and popular sovereignty. The constraint operates as
 *   a tangled rope: it coordinates a stable locus of constitutional authority
 *   (genuine coordination benefit to the system) AND asymmetrically extracts
 *   interpretive prerogative from minorities, opposition coalitions, and the
 *   judiciary (who are excluded from ultimate authority). The constraint is
 *   actively enforced through doctrinal rules that prohibit judicial review
 *   of parliamentary constitutional acts and through institutional mechanisms
 *   that suppress alternative constitutional frameworks. The claim/metric gap
 *   is intentional: the constraint is CLAIMED as a coordination solution
 *   (stable authority, prevented deadlock) while the metrics describe
 *   substantially extractive, suppression-heavy operation—the engine measures
 *   that asymmetry.
 *
 * KEY AGENTS:
 *   - elected_legislature: Sets interpretive agenda, claims electoral legitimacy, captures benefits of authority monopoly
 *   - constitutional_minorities: Powerless, trapped exit, bear cost of majoritarian authority without recourse
 *   - judiciary: Excluded from final authority, constrained by supremacy doctrine to statutory interpretation only
 *   - opposition_political_coalitions: Moderate power, constrained by electoral cycle, no judicial appeal mechanism
 *   - governing_executive_coalition: Institutional power, benefits when aligned with legislature, constrained when not
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.62).
domain_priors:suppression_score(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.71).
domain_priors:theater_ratio(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_interpretive_authority__parliamentary_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_interpretive_authority__parliamentary_supremacy_reading, "Parliamentary Supremacy in Constitutional Interpretation").
narrative_ontology:topic_domain(constitutional_interpretive_authority__parliamentary_supremacy_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(constitutional_interpretive_authority__parliamentary_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_interpretive_authority__parliamentary_supremacy_reading, 'bf7178d6-011b-4b8e-a4d0-39fcb4793a97').
narrative_ontology:cs_kernel_codification('bf7178d6-011b-4b8e-a4d0-39fcb4793a97', fixed_text).
narrative_ontology:cs_authority_grounding('bf7178d6-011b-4b8e-a4d0-39fcb4793a97', lineage).
narrative_ontology:cs_interpretation_layer_present('bf7178d6-011b-4b8e-a4d0-39fcb4793a97').
narrative_ontology:cs_reading_relation('bf7178d6-011b-4b8e-a4d0-39fcb4793a97', constitutional_interpretive_authority__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('bf7178d6-011b-4b8e-a4d0-39fcb4793a97', constitutional_interpretive_authority__coordinate_construction_reading, coexists_with).
narrative_ontology:cs_axiom('bf7178d6-011b-4b8e-a4d0-39fcb4793a97', foundational, electoral_mandate_supreme_interpretive_authority).
narrative_ontology:cs_axiom_status(electoral_mandate_supreme_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('bf7178d6-011b-4b8e-a4d0-39fcb4793a97', electoral_mandate_supreme_interpretive_authority, deontological).
narrative_ontology:cs_axiom('bf7178d6-011b-4b8e-a4d0-39fcb4793a97', foundational, judicial_review_violates_popular_sovereignty).
narrative_ontology:cs_axiom_status(judicial_review_violates_popular_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('bf7178d6-011b-4b8e-a4d0-39fcb4793a97', judicial_review_violates_popular_sovereignty, deontological).
narrative_ontology:cs_reference_frame('bf7178d6-011b-4b8e-a4d0-39fcb4793a97', legislative_supremacy_framework).
narrative_ontology:cs_drift_state('bf7178d6-011b-4b8e-a4d0-39fcb4793a97', contemporary_human_rights_pressure, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('bf7178d6-011b-4b8e-a4d0-39fcb4793a97', '').
narrative_ontology:cs_kernel_id(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__parliamentary_supremacy_reading, elected_legislature).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__parliamentary_supremacy_reading, governing_executive_coalition).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_minorities).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__parliamentary_supremacy_reading, excluded_political_coalitions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__parliamentary_supremacy_reading, governing_executive_coalition).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__parliamentary_supremacy_reading, opposition_political_coalitions).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__parliamentary_supremacy_reading, subnational_governments).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__parliamentary_supremacy_reading, electoral_mandate_grounds_legitimacy).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__parliamentary_supremacy_reading, legislative_sovereignty_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds final authority to interpret and amend the constitutional framework through legislative acts. Claims this authority is grounded in electoral mandate and popular sovereignty. Sets the interpretive agenda without fear of judicial nullification. Enforces compliance by the executive and subordinate bodies via parliamentary supremacy doctrine.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, elected_legislature, agenda_setter,
    institutional, generational, analytical, national).

% Exercises executive power within the scope of legislative authorization. Benefits from legislative supremacy when the legislature is aligned with coalition interests; constrained when opposition controls parliament. The framework offers no recourse to courts if the legislature restricts executive discretion constitutionally.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, governing_executive_coalition, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__parliamentary_supremacy_reading, governing_executive_coalition, payer).

% Groups whose interests may be overridden by parliamentary majorities (religious minorities, ethnic minorities, dissident political minorities, groups lacking electoral organization). They bear the cost of majoritarian legislation without institutional recourse. Their only formal remedy is electoral defeat of the majority, which offers no protection before the next election cycle.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_minorities, payer,
    powerless, biographical, trapped, national).

% Lack legislative power to set interpretive agenda when out of office. Subject to the governing coalition's constitutional interpretations without judicial override. Their leverage is electoral competition; they cannot appeal to courts to nullify majority legislation or constrain legislative interpretation.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, opposition_political_coalitions, payer,
    moderate, biographical, constrained, national).

% Formally excluded from final constitutional interpretive authority. Courts interpret statutes and apply the law as legislated, but possess no power to void parliamentary constitutional acts or override legislative interpretations of fundamental rights. The constraint's enforcement machinery exists precisely to maintain this exclusion.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, judiciary, excluded,
    institutional, generational, trapped, national).

% In federal or quasi-federal systems, subnational units are subordinate to parliamentary interpretation of the constitutional order. The legislature can redefine federalism, override subnational autonomy claims, or centralize power through constitutional legislation without judicial review.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, subnational_governments, payer,
    powerful, generational, constrained, national).

% Document and analyze the constraint's operation and contestation. Produce expert testimony in legislative proceedings and academic literature. Influential in shaping narrative justifications but possess no institutional authority to override parliamentary interpretation.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_scholars, observer,
    moderate, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_interpretive_authority__parliamentary_supremacy_reading, elected_legislature).
narrative_ontology:fixing_cost_class(constitutional_interpretive_authority__parliamentary_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, stable locus of constitutional authority—the elected legislature—to prevent deadlock, dueling interpretations, and judicial-legislative conflict over fundamental law. Enables decisive constitutional adaptation without courts blocking change.
% TRANSFER_FUNCTION: Transfers interpretive discretion and constitutional authority from dispersed institutions (courts, executives, subnational bodies) to the centralized elected legislature. Moves political power from unelected judicial bodies to those claiming electoral legitimacy. Minorities and opposition coalitions bear the cost of majoritarian authority; the governing legislative coalition captures the interpretive prerogative.
% ABSENT_VOICES: Constitutional minorities and opposition coalitions are structurally excluded from interpretive authority. Judges who would argue for judicial review and rights-protective review are kept out of final authority. Alternative constitutional framings (coordinate construction, judicial guardianship) are suppressed by the operational constraint itself.
% DISAPPEARANCE_RATIONALE: If parliamentary supremacy vanished and judicial review or coordinate construction replaced it, constitutional contests would be adjudicated differently: judicial nullification of parliamentary acts would become possible, subnational autonomy claims would gain enforceability, minority rights would have a new forum. The system of political contestation and constitutional change would reorganize fundamentally.
% FOUNDING_PROBLEM: Early constitutional arrangements produced deadlock between branches and instability when courts claimed interpretive authority co-equal with legislatures. Parliamentary supremacy was adopted to establish decisive authority and prevent judicial overreach into political questions.
% FOUNDING_PROBLEM_CORROBORATION: Jurisdictions practicing parliamentary supremacy (UK, some Commonwealth traditions) attest the original problem was judicial-legislative conflict and the solution maintains stability. Jurisdictions with judicial supremacy (US, some European constitutional courts) attest the founding problem was majority tyranny and that courts are necessary to check it. No external corroboration supports one reading as universal truth—the problem statement itself is the subject of the contest.
narrative_ontology:disappearance_verdict(constitutional_interpretive_authority__parliamentary_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_interpretive_authority__parliamentary_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_interpretive_authority__parliamentary_supremacy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(constitutional_interpretive_authority__parliamentary_supremacy_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_interpretive_authority__parliamentary_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_interpretive_authority__parliamentary_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_interpretive_authority__parliamentary_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62 final) reflects that legislative supremacy concentrates interpretive discretion—those excluded from the legislature lose recourse to courts or other institutional checks. Extraction rises over the interval as the constraint's operation accumulates: minority protections are eroded through legislation, courts internalize deference to parliament, opposition coalitions gradually accept the legitimacy frame. Suppression (0.71 final) is high because the constraint's persistence requires active institutional work: doctrines of parliamentary sovereignty must be taught and enforced, alternative frameworks (judicial review, coordinate construction) must be defeated when they emerge, courts must be disciplined to accept their exclusion. Theater ratio (0.41) indicates that roughly 41% of the constraint's maintenance activity is performative rather than functional: ceremonial appeals to electoral mandate, rhetorical defenses of sovereignty against international pressure, elaborate justifications for why judicial review would be anti-democratic. The genuine coordination function (stability, avoided deadlock) is real but increasingly overlaid with theatrical defense of majoritarian power against challenge.
 *
 * PERSPECTIVAL GAP:
 *   The legislature and the governing coalition experience this constraint as legitimate coordination—they obtained authority through elections and wield it for decisive governance. Constitutional minorities and the judiciary experience it as coercive suppression—they lack meaningful exit and their preferences are systematically overridden. The engine computes these as different directionalities from the same structural facts: legislature gets d near 0.1 (beneficiary end), minorities get d near 0.85 (target end), courts get d near 0.75 (excluded, constrained). The perspectival gap is the measurement—the divergence is what the corpus exists to detect. The claim that parliamentary supremacy is legitimate coordination is the legisl ature's narrative; the metrics show extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The legislature is the structural beneficiary: it captures interpretive authority without institutional limit, claims legitimacy via electoral mandate, faces no constraint from courts. d approaches 0.0 (full beneficiary). Constitutional minorities are the clear targets: they pay the cost of majoritarian authority, have trapped exit (cannot leave the national polity), and gain no benefit from the constraint's operation. d approaches 0.9 (full target). The judiciary is excluded rather than coordinated: it is formally denied authority it would claim if permitted. d is moderately high (~0.65) because exclusion from authority is itself a form of extraction—courts are constrained institutions within a hierarchy. Opposition coalitions hold moderate power and constrained exit; they are secondary targets. The governing executive coalition is complex: it benefits when aligned with parliament but is also constrained by legislative supremacy; d is near symmetric (~0.50).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits mandatrophy tension: it claims to solve a coordination problem (deadlock between branches) and to ground legitimacy in popular sovereignty (the founding problem). However, the measured extraction (0.62) and suppression (0.71) suggest the mandate has drifted: the constraint now operates primarily to protect majoritarian power against challenge, not to solve deadlock. The theater ratio (0.41) captures this drift—ceremonial appeals to electoral mandate increasingly defend against international human-rights criticism, constitutional court challenges, and judicial review expansion, not against judicial overreach. The founding problem (deadlock, judicial usurpation) is contestable: jurisdictions with judicial review claim the real founding problem was majority tyranny. The classification prevents false attribution: calling this 'rope' (pure coordination) would hide the asymmetric extraction; calling it 'snare' (pure extraction) would miss the genuine stability benefit. Tangled rope is correct—it genuinely coordinates authority AND extracts from those excluded from it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    electoral_legitimacy_grounding,
    'Does electoral mandates—obtaining a legislative majority through democratic process—sufficiently ground constitutional interpretive authority, or are there structural limits on what electoral majorities may do constitutionally?',
    'Empirical: examine whether legislatures claim unlimited interpretive power or acknowledge constitutional constraints (bills of rights, supermajority requirements for certain changes, federalism limits). Conceptual: whether ''popular sovereignty'' can coherently limit its own exercise.',
    'If electoral legitimacy is unlimited, parliamentary supremacy is complete. If majority rule is itself constitutionally constrained, the constraint is more limited than claimed—minorities retain structural protections even under parliamentary supremacy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(electoral_legitimacy_grounding, conceptual, 'Whether electoral mandate grounds unlimited interpretive authority or is itself constitutionally limited.').

omega_variable(
    judicial_restraint_vs_exclusion,
    'Is judicial exclusion from final interpretive authority sustained by genuine judicial restraint (courts voluntarily accepting limits), by institutional suppression (courts are prevented from claiming authority), or by both?',
    'Observe whether courts resist the supremacy doctrine or accept it as legitimate. Track instances of judicial pushback (constitutional courts challenging parliamentary supremacy in other jurisdictions, courts asserting rights review). Examine whether doctrinal justifications for exclusion are internally coherent or require continuous enforcement.',
    'If courts genuinely accept the framework, the suppression is lower and the constraint is more stable. If courts resist and are suppressed, the suppression is structural and must be continuously enforced—piton risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_restraint_vs_exclusion, empirical, 'Whether judicial exclusion is accepted legitimacy or maintained suppression.').

omega_variable(
    minority_protection_under_supremacy,
    'Can minorities obtain constitutional protection against majoritarian legislation under parliamentary supremacy, or are they structurally defenseless?',
    'Examine constitutional traditions that practice parliamentary supremacy: do they include entrenched bills of rights, supermajority requirements for certain changes, federalism protections, or other structural limits on majoritarian power? Compare minority outcomes across parliamentary-supremacy and judicial-review systems.',
    'If structural protections exist and function, minorities are not fully trapped—the extraction is lower and more nuanced. If minorities are truly undefended, the trap is complete and the victim characterization is accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(minority_protection_under_supremacy, empirical, 'Whether minorities retain structural constitutional protection under parliamentary supremacy.').

omega_variable(
    kernel_reading_contest,
    'Is parliamentary supremacy the correct reading of the constitutional kernel, or is the kernel better read as judicial supremacy or coordinate construction?',
    'This is the constitutive question of the kernel contest itself. Resolution is not empirical but rather a matter of constitutional interpretation, institutional design choice, and political legitimacy claim. Different jurisdictions have adopted different readings; no external fact settles the contest. The constraint''s structure depends on which reading is taken as binding.',
    'This omega names the intra-kernel uncertainty itself. If judicial supremacy becomes dominant (e.g., through constitutional amendment or judicial assertion), this reading becomes subordinate, and the constraint reclassifies. This is not a measurement error—it is the contested nature of the kernel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of the constitutional kernel is authoritative—parliamentary, judicial, or coordinate?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(cons_tr_t0, observed).
narrative_ontology:measurement(cons_tr_t5, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(cons_tr_t5, observed).
narrative_ontology:measurement(cons_tr_t10, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(cons_tr_t10, observed).
narrative_ontology:measurement(cons_tr_t15, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement_basis(cons_tr_t15, observed).
narrative_ontology:measurement(cons_tr_t20, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement_basis(cons_tr_t20, observed).
narrative_ontology:measurement(cons_tr_t25, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(cons_tr_t25, observed).
narrative_ontology:measurement(cons_tr_t30, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(cons_tr_t30, observed).
narrative_ontology:measurement(cons_tr_t40, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(cons_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(cons_be_t0, observed).
narrative_ontology:measurement(cons_be_t5, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement_basis(cons_be_t5, observed).
narrative_ontology:measurement(cons_be_t10, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(cons_be_t10, observed).
narrative_ontology:measurement(cons_be_t15, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 15, 0.56).
narrative_ontology:measurement_basis(cons_be_t15, observed).
narrative_ontology:measurement(cons_be_t20, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 20, 0.59).
narrative_ontology:measurement_basis(cons_be_t20, observed).
narrative_ontology:measurement(cons_be_t25, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 25, 0.61).
narrative_ontology:measurement_basis(cons_be_t25, observed).
narrative_ontology:measurement(cons_be_t30, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement_basis(cons_be_t30, observed).
narrative_ontology:measurement(cons_be_t40, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(cons_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(cons_su_t0, observed).
narrative_ontology:measurement(cons_su_t5, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(cons_su_t5, observed).
narrative_ontology:measurement(cons_su_t10, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement_basis(cons_su_t10, observed).
narrative_ontology:measurement(cons_su_t15, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(cons_su_t15, observed).
narrative_ontology:measurement(cons_su_t20, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(cons_su_t20, observed).
narrative_ontology:measurement(cons_su_t25, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(cons_su_t25, observed).
narrative_ontology:measurement(cons_su_t30, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(cons_su_t30, observed).
narrative_ontology:measurement(cons_su_t40, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(cons_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_interpretive_authority__parliamentary_supremacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_interpretive_authority__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_interpretive_authority__coordinate_construction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel constitutional_interpretive_authority. The kernel reflects a fundamental disagreement about which institution(s) possess final authority to interpret the constitution. This reading (parliamentary_supremacy_reading) claims the elected legislature holds final authority. Sibling readings instantiate coordinate_construction_reading (no single branch holds final authority; constitution is constructed through inter-branch dialogue) and judicial_supremacy_reading (courts possess ultimate interpretive authority via rights guardianship). These are NOT alternative observables of one constraint—they are structurally distinct constraints with different ε values, different beneficiary/victim sets, and different classifications. The decomposition follows ε-invariance principle (DP-001): reading the kernel through different authority frames yields different extraction structures and therefore different constraints. Each reading should be authored separately with its own metrics, stakeholders, and type classification. All three readings are linked via network.affects_constraints to indicate they form a constraint family and influence each other's operation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_interpretive_authority__parliamentary_supremacy_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
