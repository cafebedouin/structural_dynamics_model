% ============================================================================
% CONSTRAINT STORY: constitutional_authority_boundary__parliamentary_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_authority_boundary__parliamentary_primacy_reading, []).

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
 *   constraint_id: constitutional_authority_boundary__parliamentary_primacy_reading
 *   human_readable: Parliamentary Sovereignty Over Constitutional Text
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the parliamentary primacy reading of a
 *   contested kernel: constitutional authority. The kernel is the persisting
 *   commitment that defines how constitutions ground legitimacy in
 *   democracies. Under the parliamentary primacy reading, the constitutional
 *   text (where codified) is subordinate to the will of the elected
 *   legislature, which retains the final and unchallengeable power to define
 *   constitutional meaning through legislation. The reading competes with two
 *   sibling readings: coordinate construction (which distributes authority
 *   among three co-equal branches) and judicial supremacy (which vests final
 *   authority in courts). All three readings ground themselves in the same
 *   constitutional text or political tradition; they disagree on who the
 *   ultimate arbiter is. This story models the parliamentary reading as a
 *   constraint on institutional behavior: it specifies who may
 *   authoritatively interpret, and it enforces subordination of other
 *   interpreters (especially courts) to legislative will.
 *
 * KEY AGENTS:
 *   - elected_legislature: Sets constitutional meaning through legislation; possesses final, unchallengeable authority; the primary beneficiary of the arrangement
 *   - judicial_review_authority: Interprets the constitution in cases but remains subordinate to legislative override; constrained by the structural superiority of parliament
 *   - executive_authority: Operates under delegated legislative authority; no independent constitutional voice; institutionally constrained
 *   - individual_rights_bearers: Powerless; identity-locked to their role as rights subjects; their constitutional position depends entirely on legislative goodwill
 *   - coordinate_construction_advocates: Excluded from the framework; would assert distributed authority among branches; structurally foreclosed by the reading's core premise
 *   - judicial_supremacy_advocates: Excluded from the framework; would assert courts as final arbiters; structurally foreclosed by the reading's core premise
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_authority_boundary__parliamentary_primacy_reading, 0.18).
domain_priors:suppression_score(constitutional_authority_boundary__parliamentary_primacy_reading, 0.22).
domain_priors:theater_ratio(constitutional_authority_boundary__parliamentary_primacy_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_authority_boundary__parliamentary_primacy_reading, rope).
narrative_ontology:human_readable(constitutional_authority_boundary__parliamentary_primacy_reading, "Parliamentary Sovereignty Over Constitutional Text").
narrative_ontology:topic_domain(constitutional_authority_boundary__parliamentary_primacy_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(constitutional_authority_boundary__parliamentary_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_authority_boundary__parliamentary_primacy_reading, '8ff2d9d7-99ac-458b-988f-e605074979d8').
narrative_ontology:cs_kernel_codification('8ff2d9d7-99ac-458b-988f-e605074979d8', formalized).
narrative_ontology:cs_authority_grounding('8ff2d9d7-99ac-458b-988f-e605074979d8', lineage).
narrative_ontology:cs_interpretation_layer_present('8ff2d9d7-99ac-458b-988f-e605074979d8').
narrative_ontology:cs_reading_relation('8ff2d9d7-99ac-458b-988f-e605074979d8', constitutional_authority_boundary__coordinate_construction_reading, forecloses).
narrative_ontology:cs_reading_relation('8ff2d9d7-99ac-458b-988f-e605074979d8', constitutional_authority_boundary__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_axiom('8ff2d9d7-99ac-458b-988f-e605074979d8', foundational, legislature_final_arbiter).
narrative_ontology:cs_axiom_status(legislature_final_arbiter, holdable).
narrative_ontology:cs_axiom_grounding('8ff2d9d7-99ac-458b-988f-e605074979d8', legislature_final_arbiter, deontological).
narrative_ontology:cs_axiom('8ff2d9d7-99ac-458b-988f-e605074979d8', foundational, text_subordinate_to_sovereign_will).
narrative_ontology:cs_axiom_status(text_subordinate_to_sovereign_will, holdable).
narrative_ontology:cs_axiom_grounding('8ff2d9d7-99ac-458b-988f-e605074979d8', text_subordinate_to_sovereign_will, conventional).
narrative_ontology:cs_reference_frame('8ff2d9d7-99ac-458b-988f-e605074979d8', parliamentary_constitutional_supremacy).
narrative_ontology:cs_drift_state('8ff2d9d7-99ac-458b-988f-e605074979d8', contemporary_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8ff2d9d7-99ac-458b-988f-e605074979d8', '').
narrative_ontology:cs_kernel_id(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_authority_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__parliamentary_primacy_reading, elected_legislature).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__parliamentary_primacy_reading, judicial_review_authority).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__parliamentary_primacy_reading, executive_authority).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__parliamentary_primacy_reading, individual_rights_bearers).
narrative_ontology:constraint_victim(constitutional_authority_boundary__parliamentary_primacy_reading, judicial_review_authority).
narrative_ontology:constraint_victim(constitutional_authority_boundary__parliamentary_primacy_reading, executive_authority).
narrative_ontology:constraint_victim(constitutional_authority_boundary__parliamentary_primacy_reading, individual_rights_bearers).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__parliamentary_primacy_reading, parliamentary_supremacy_doctrine).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__parliamentary_primacy_reading, democratic_will_primacy).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__parliamentary_primacy_reading, text_subordination_to_sovereign_power).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possesses the final and unchallengeable authority to define constitutional meaning through ordinary legislation or entrenched constitutional amendment. Interprets the constitutional text, modifies its application, or overrides judicial interpretations through legislative act. Collects the rents of unconstrained democratic will-making.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, elected_legislature, agenda_setter,
    institutional, generational, analytical, national).

% Retains the formal power to interpret and apply the constitution in cases brought before it, but its interpretations are subordinate to legislative authority and can be overridden by subsequent ordinary or entrenched legislation. Benefits from institutional legitimacy and the appearance of independent review; constrained by the structural superiority of the legislature. Acts as a filter for legislative consistency with constitutional text, but has no final word.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, judicial_review_authority, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__parliamentary_primacy_reading, judicial_review_authority, payer).

% Operates under delegated legislative authority and subject to legislative override. Benefits from legislative direction that clarifies executive mandate; constrained by legislative sovereignty over executive powers. No independent constitutional voice; interprets the constitution only as the legislature permits.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, executive_authority, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__parliamentary_primacy_reading, executive_authority, payer).

% Benefit from whatever rights protections the legislature codifies; constrained to whatever rights the legislature recognizes or the judiciary can defend against legislative override. Their constitutional position shifts with legislative will; they cannot appeal to an entrenched constitutional text superior to legislative power.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, individual_rights_bearers, beneficiary,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__parliamentary_primacy_reading, individual_rights_bearers, payer).

% Argue for distributed interpretive authority among the three branches; are structurally excluded from this reading's framework because parliamentary primacy denies them the coordinate authority they claim. Would contest the legislative supremacy reading by asserting the judiciary and executive have independent constitutional voices.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, coordinate_construction_advocates, excluded,
    moderate, generational, constrained, national).

% Argue courts are the final arbiters of constitutional meaning; are structurally excluded from this reading's framework because parliamentary primacy subordinates judicial interpretation to legislative override. Would contest parliamentary primacy by asserting courts possess unchallengeable constitutional authority.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, judicial_supremacy_advocates, excluded,
    moderate, generational, constrained, national).

% Monitor parliamentary compliance with international human rights treaties and customary norms. Observe from outside the national constitutional framework; lack enforcement power over legislative acts within the nation-state. Can issue opinions and recommendations but cannot override the legislature's final authority.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, supranational_rights_institutions, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single final locus of constitutional authority (the legislature) capable of binding all other institutional actors to a coherent interpretation and application of constitutional principles. Solves the coordination problem of having multiple potential interpreters (courts, executive, legislature) claim authority simultaneously by vesting supremacy in the democratically elected body.
% TRANSFER_FUNCTION: Transfers interpretive authority and the power to bind all other institutional actors from the constitutional text itself (or from distributed interpretation) to the elected legislature. The legislature gains the authority to decide what the constitution means; courts and other actors lose the power to assert independent or final constitutional interpretations.
% ABSENT_VOICES: Advocates for judicial supremacy and coordinate construction of authority are structurally excluded. They would argue that concentrating constitutional interpretation in the legislature violates the separation of powers, erodes rule of law, and leaves individual rights unprotected by an independent judiciary. Their exclusion is enforced by the reading's core premise — that parliament is sovereign and courts are subordinate — which logically prevents their alternative interpretations from being entertained as structurally valid within this framework.
% DISAPPEARANCE_RATIONALE: If parliamentary primacy as a binding constitutional arrangement disappeared, the interpretive authority held exclusively by the legislature would fragment. Courts would begin asserting independent constitutional judgments; the executive might claim its own constitutional mandate; and individual rights bearers would no longer face a single final arbiter but rather competing institutional voices. The very coordination problem the constraint was designed to solve would re-emerge, requiring a new institutional settlement around interpretation authority.
% FOUNDING_PROBLEM: Early constitutional democracies faced a crisis of interpretive authority: if the constitutional text could be read in multiple ways, and multiple institutional actors claimed the right to declare the true reading, constitutional law became the battlefield of institutional power struggles rather than a stable framework. Parliamentary primacy solves this by vesting final authority in the elected legislature — the seat of democratic will — ensuring one voice speaks with final constitutional authority.
% FOUNDING_PROBLEM_CORROBORATION: The legislature and its institutional allies attest the founding problem remains live and parliamentary primacy is essential to stable governance. Courts, rights advocates, and international human rights bodies attest the founding problem has shifted: the modern risk is not interpretive chaos but parliamentary overreach unchecked by independent judicial review. Comparative constitutional law scholarship documents both living and superseded versions of the arrangement.
narrative_ontology:disappearance_verdict(constitutional_authority_boundary__parliamentary_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_authority_boundary__parliamentary_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_authority_boundary__parliamentary_primacy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(constitutional_authority_boundary__parliamentary_primacy_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_authority_boundary__parliamentary_primacy_reading_tests).
:- end_tests(constitutional_authority_boundary__parliamentary_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18 at interval end) because the constraint operates as genuine coordination: it solves the problem of interpretive authority without requiring coercion beyond the legislative sovereignty mechanism itself. The legislature benefits from unconstrained democratic will-making, but this benefit flows from the democratic process, not from extraction. Suppression is correspondingly low (0.22): the constraint is enforced through structural subordination of courts and the exclusion of rival interpretations from legitimate contention, not through active coercive machinery. Theater is minimal (0.12): the constraint is mostly functional; there is little performative activity beyond the ordinary exercise of legislative power. Resistance is moderate (0.45) because courts and individual rights advocates actively contest parliamentary primacy through litigation strategy, constitutional argument, and international human rights advocacy — they refuse complete acquiescence. The measurement series show low drift over the interval: extractiveness and suppression fluctuate slightly but remain stable, suggesting the constraint's operation is relatively constant (unlike constraints undergoing ratcheting or decay).
 *
 * PERSPECTIVAL GAP:
 *   The legislature and the judiciary perceive this arrangement in structurally opposite ways. The legislature sees a coordination solution: clarity of authority, predictable override power, democratic legitimacy. The judiciary sees a subordination: interpretive authority that can be revoked, legitimacy that is conditional on legislative tolerance, a role that is ultimately performative. This is not a failure of description — it is the constitutional structure itself that is asymmetric. The engine computes per-seat types from the structural data (power, exit_options, beneficiary/victim); the authored claim (rope) is the legislature's self-description, and the metrics are the constraint's actual operation across all seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The elected legislature is the structural beneficiary (d near 0.0): it gains interpretive authority and can override any judicial interpretation through subsequent legislation. Its exit options are analytical (it is the final arbiter, not subject to override). Courts are partly constrained (d around 0.35-0.40): they benefit from institutional legitimacy and the real coordination function, but suffer subordination to legislative override and identity-lock to their inferior role. Individual rights bearers are the targets (d near 1.0): they are powerless, identity-locked to their constitutional position, and can only appeal to a legislature that is their ultimate superior — no independent recourse exists. Coordinate and judicial supremacy advocates are excluded, not measured on the directionality scale. The beneficiary declaration (elected_legislature) is vindicated through the mechanism of democratic will; the low extraction and suppression scores reflect the genuine coordination function, not absence of power asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not carry the mandatrophy signature. Its founding problem (interpretive authority crisis) remains live and contested; the constraint is actively maintained through legislative assertion of supremacy and judicial self-restraint. The founding problem has not atrophied into pure performance. However, in jurisdictions where the founding problem has genuinely been solved (unified institutional practice around parliamentary primacy has become normalized and uncontested), the theater_ratio would rise and the constraint might approach piton classification — a rule maintained by habit rather than active enforcement. The present measurement series shows modest theatrical activity (theater_ratio ~0.12) and real resistance (0.45), suggesting the constraint remains functionally alive rather than degraded into performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legislative_override_finality,
    'Is the legislature''s power to override judicial interpretation truly final and unchallengeable, or does it remain subject to some form of entrenched constraint (supermajority requirements, popular ratification, constitutional amendment formality)?',
    'Examine the formal procedures required for legislative override; assess whether entrenched amendment requirements create a functional boundary on ordinary legislative power; compare jurisdictions with different entrenchment regimes.',
    'If override is subject to supermajority or amendment procedures that significantly raise the cost of override, the legislature''s authority is partly constrained, and the constraint shifts from pure parliamentary primacy toward coordinate construction. If override is unrestricted, parliamentary primacy is confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legislative_override_finality, empirical, 'Whether legislative override of judicial interpretation is truly unconstrained or subject to procedural limits.').

omega_variable(
    judicial_role_internalization,
    'Do courts genuinely accept subordination to legislative authority as a legitimate constitutional principle, or do they strategically constrain their deference to legislative will while formally acknowledging supremacy?',
    'Analyze patterns of judicial decision-making: compare the frequency and strength of judicial rulings that contest or limit legislative acts; assess whether courts are applying meaningful standards of review or performing performative deference; examine judicial rhetoric about legislative deference versus actual practice.',
    'If courts genuinely subordinate their judgment to legislative authority, parliamentary primacy operates as described. If courts apply searching review and strategically limit legislative power while maintaining rhetorical deference, the constraint is more extractive and performative than the low metrics suggest, and might approach snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_role_internalization, empirical, 'Whether judicial subordination to parliamentary authority is genuine or performatively maintained while courts exercise real check on legislation.').

omega_variable(
    kernel_reading_contention,
    'Which of the three kernel readings (parliamentary primacy, coordinate construction, judicial supremacy) is structurally true of the constitutional arrangement in a given jurisdiction?',
    'This omega marks the essential reading-level contention: the three readings cannot all be true in the same framework; the parties to constitutional politics contest which reading correctly describes the legitimate structure. Resolution occurs at the level of constitutional practice and judicial doctrine — which reading the courts, legislature, and practice actually enact. Different jurisdictions (or the same jurisdiction at different historical moments) may instantiate different readings.',
    'The reading that wins institutional acceptance becomes the operative constraint. A jurisdiction may formally espouse parliamentary primacy while practicing coordinate construction (formal doctrine vs. actual practice divergence). A shift in reading produces a different constraint with different beneficiaries and different suppression requirements.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contention, conceptual, 'Which kernel reading describes the true constitutional structure; reading-level contention cannot be resolved within any single reading''s framework.').

omega_variable(
    democratic_will_versus_rights_protection,
    'Are rights protected better under parliamentary primacy (because the legislature is closest to democratic will and can respond to evolving values) or under judicial supremacy (because courts can protect minority rights against majoritarian overreach)?',
    'Comparative empirical study of rights outcomes across jurisdictions with different constitutional readings; historical analysis of when parliamentary versus judicial authority produced stronger rights protection; analysis of whether democratic responsiveness or institutional independence is more protective of rights in practice.',
    'This is a foundational normative question about the reading''s legitimacy. If parliamentary primacy produces better rights outcomes, it is vindicated on instrumental grounds. If judicial supremacy produces better rights outcomes, parliamentary primacy loses legitimacy as a framework for rights protection, even if it retains legitimacy as a coordination mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(democratic_will_versus_rights_protection, preference, 'Whether parliamentary primacy or judicial supremacy better protects constitutional rights in practice; depends on empirical outcomes and values about rights priority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_authority_boundary__parliamentary_primacy_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(cons_tr_t0, projected).
narrative_ontology:measurement(cons_tr_t10, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement_basis(cons_tr_t10, observed).
narrative_ontology:measurement(cons_tr_t20, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 20, 0.11).
narrative_ontology:measurement_basis(cons_tr_t20, observed).
narrative_ontology:measurement(cons_tr_t30, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 30, 0.13).
narrative_ontology:measurement_basis(cons_tr_t30, observed).
narrative_ontology:measurement(cons_tr_t40, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement_basis(cons_tr_t40, observed).
narrative_ontology:measurement(cons_tr_t50, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 50, 0.12).
narrative_ontology:measurement_basis(cons_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(cons_be_t0, projected).
narrative_ontology:measurement(cons_be_t10, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 10, 0.16).
narrative_ontology:measurement_basis(cons_be_t10, observed).
narrative_ontology:measurement(cons_be_t20, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 20, 0.18).
narrative_ontology:measurement_basis(cons_be_t20, observed).
narrative_ontology:measurement(cons_be_t30, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 30, 0.19).
narrative_ontology:measurement_basis(cons_be_t30, observed).
narrative_ontology:measurement(cons_be_t40, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 40, 0.17).
narrative_ontology:measurement_basis(cons_be_t40, observed).
narrative_ontology:measurement(cons_be_t50, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 50, 0.18).
narrative_ontology:measurement_basis(cons_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement_basis(cons_su_t0, projected).
narrative_ontology:measurement(cons_su_t10, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 10, 0.2).
narrative_ontology:measurement_basis(cons_su_t10, observed).
narrative_ontology:measurement(cons_su_t20, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 20, 0.22).
narrative_ontology:measurement_basis(cons_su_t20, observed).
narrative_ontology:measurement(cons_su_t30, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 30, 0.24).
narrative_ontology:measurement_basis(cons_su_t30, observed).
narrative_ontology:measurement(cons_su_t40, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 40, 0.23).
narrative_ontology:measurement_basis(cons_su_t40, observed).
narrative_ontology:measurement(cons_su_t50, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 50, 0.22).
narrative_ontology:measurement_basis(cons_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_authority_boundary__parliamentary_primacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_authority_boundary__parliamentary_primacy_reading, 0.08).
narrative_ontology:affects_constraint(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_authority_boundary__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_authority_boundary__coordinate_construction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the constitutional_authority_boundary kernel. The sibling readings (judicial_supremacy_reading and coordinate_construction_reading) are separate constraint stories modeling alternative institutional settlements of the same constitutional question. All three readings ground themselves in the same constitutional text or foundational political tradition; they disagree on the hierarchy of interpretive authority. The three readings are related by kernel_reading_contention: they cannot all be true simultaneously within any single jurisdiction's constitutional framework, though different jurisdictions may instantiate different readings at different historical moments. The parliamentary primacy reading forecloses strong-form judicial review and coordinate construction authority distribution.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
