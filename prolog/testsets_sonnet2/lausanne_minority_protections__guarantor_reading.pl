% ============================================================================
% CONSTRAINT STORY: lausanne_minority_protections__guarantor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lausanne_minority_protections__guarantor_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: lausanne_minority_protections__guarantor_reading
 *   human_readable: Lausanne Minority Protections as Internationally Supervised Obligation (Guarantor Reading)
 *   domain: international_law/religious_governance/minority_rights
 *
 * SUMMARY:
 *   This story authors the guarantor reading of the Lausanne
 *   minority-protection kernel: the claim that Lausanne obligations are
 *   internationally supervised, not solely subject to domestic Turkish
 *   interpretation, because guarantor-state diplomacy and European human
 *   rights mechanisms (principally the ECHR) supply an external adjudication
 *   pathway. This is distinct from the expansive reading (which claims a
 *   substantive guarantee of institutional continuity — self-administration,
 *   property, clergy formation) and the restrictive reading (which confines
 *   Lausanne to individual worship rights and treats institutional questions
 *   as purely domestic). The guarantor reading makes no claim about the
 *   SUBSTANCE of what is protected; it makes a claim about WHO gets to
 *   authoritatively interpret that substance and through what venue. Its ε is
 *   authored for the standing arrangement as this reading's own proponents
 *   see it: a genuine but weak external-review scaffold that has drifted
 *   toward diplomatic theater because guarantor states rarely invoke it and
 *   ECHR remedies, even when granted, do not reliably convert into domestic
 *   institutional change.
 *
 * KEY AGENTS:
 *   - non_muslim_minority_communities: primary intended beneficiary of the supervisory pathway (powerless/trapped) — bears the cost of the pathway's weak enforcement
 *   - turkish_state: agenda_setter administering first-instance interpretation and retaining practical control (institutional/arbitrage)
 *   - guarantor_states: hold standing to invoke the mechanism but exercise it selectively based on bilateral interest (institutional/mobile)
 *   - european_court_of_human_rights: supplies the external adjudicative venue this reading depends on (institutional/analytical)
 *   - domestic_turkish_courts: gatekeep whether disputes ever mature to international review (institutional/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lausanne_minority_protections__guarantor_reading, 0.28).
domain_priors:suppression_score(lausanne_minority_protections__guarantor_reading, 0.35).
domain_priors:theater_ratio(lausanne_minority_protections__guarantor_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lausanne_minority_protections__guarantor_reading, scaffold).
narrative_ontology:human_readable(lausanne_minority_protections__guarantor_reading, "Lausanne Minority Protections as Internationally Supervised Obligation (Guarantor Reading)").
narrative_ontology:topic_domain(lausanne_minority_protections__guarantor_reading, "international_law/religious_governance/minority_rights").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lausanne_minority_protections__guarantor_reading, '248883c9-777a-4a72-9572-26113e5dfce5').
narrative_ontology:cs_kernel_codification('248883c9-777a-4a72-9572-26113e5dfce5', fixed_text).
narrative_ontology:cs_authority_grounding('248883c9-777a-4a72-9572-26113e5dfce5', distributed).
narrative_ontology:cs_reading_relation('248883c9-777a-4a72-9572-26113e5dfce5', lausanne_minority_protections__expansive_reading, coexists_with).
narrative_ontology:cs_reading_relation('248883c9-777a-4a72-9572-26113e5dfce5', lausanne_minority_protections__restrictive_reading, influences).
narrative_ontology:cs_axiom('248883c9-777a-4a72-9572-26113e5dfce5', foundational, interpretive_authority_is_internationally_distributed).
narrative_ontology:cs_axiom_status(interpretive_authority_is_internationally_distributed, holdable).
narrative_ontology:cs_axiom_grounding('248883c9-777a-4a72-9572-26113e5dfce5', interpretive_authority_is_internationally_distributed, conventional).
narrative_ontology:cs_axiom('248883c9-777a-4a72-9572-26113e5dfce5', secondary, domestic_courts_are_not_final_arbiters_of_treaty_scope).
narrative_ontology:cs_axiom_status(domestic_courts_are_not_final_arbiters_of_treaty_scope, holdable).
narrative_ontology:cs_axiom_grounding('248883c9-777a-4a72-9572-26113e5dfce5', domestic_courts_are_not_final_arbiters_of_treaty_scope, conventional).
narrative_ontology:cs_reference_frame('248883c9-777a-4a72-9572-26113e5dfce5', treaty_supervised_minority_regime).
narrative_ontology:cs_drift_state('248883c9-777a-4a72-9572-26113e5dfce5', post_echr_jurisprudence_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('248883c9-777a-4a72-9572-26113e5dfce5', '').
narrative_ontology:cs_kernel_id(lausanne_minority_protections__guarantor_reading, lausanne_minority_protections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__guarantor_reading, non_muslim_minority_communities).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__guarantor_reading, guarantor_states).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__guarantor_reading, european_human_rights_mechanisms).
narrative_ontology:constraint_victim(lausanne_minority_protections__guarantor_reading, minority_communities_awaiting_remedy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(lausanne_minority_protections__guarantor_reading, non_muslim_minority_communities).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__guarantor_reading, internationalized_treaty_supervision_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Greek Orthodox, Armenian, and Jewish communities in Turkey whose institutional standing (schools, church property, clergy training) depends on how Lausanne is read. Under the guarantor reading they gain a nominal external appeal pathway through guarantor states and Strasbourg, but domestic courts still adjudicate first and the external pathway is slow, diplomatically contingent, and produces no automatic remedy. They cannot exit the jurisdiction whose interpretation binds their daily institutional life.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, non_muslim_minority_communities, beneficiary,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(lausanne_minority_protections__guarantor_reading, non_muslim_minority_communities, payer).

% Administers domestic law over minority institutions and controls first-instance interpretation of what Lausanne requires. Treats the guarantor-state and ECHR pathway as an external nuisance to be managed diplomatically rather than a binding constraint, and retains substantial practical control over pacing, procedure, and remedy even after adverse international findings.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, turkish_state, agenda_setter,
    institutional, civilizational, arbitrage, national).

% The original treaty guarantor powers (successor governments to the 1923 signatories) retain a diplomatic standing to raise minority-protection questions, but exercise this selectively based on their own bilateral interests with Turkey. They can invoke the mechanism or let it lapse into dormancy without cost to themselves.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, guarantor_states, agenda_setter,
    institutional, civilizational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(lausanne_minority_protections__guarantor_reading, guarantor_states, observer).

% Adjudicates individual complaints framed under the European Convention that intersect with Lausanne-protected minority institutions, but its judgments bind through the Council of Europe's enforcement architecture, which itself depends on state compliance rather than direct execution. It supplies the external adjudication pathway this reading depends on.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, european_court_of_human_rights, observer,
    institutional, generational, analytical, continental).

% Hear minority-institution disputes first and interpret Lausanne obligations through a domestic sovereignty lens that frequently narrows or defers the international supervision claim, effectively gatekeeping whether a case ever reaches the guarantor or ECHR track at all.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, domestic_turkish_courts, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(lausanne_minority_protections__guarantor_reading, domestic_turkish_courts, excluded).

% Specific congregations, foundations, and schools with pending property or governance disputes who experience the guarantor-reading pathway as a years-long, diplomatically mediated process that rarely converts into concrete restitution or institutional recognition within a useful timeframe, while the underlying domestic restriction continues to operate.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, minority_communities_awaiting_remedy, payer,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lausanne_minority_protections__guarantor_reading, diffuse).
narrative_ontology:fixing_cost_class(lausanne_minority_protections__guarantor_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides an external adjudication and diplomatic-escalation pathway so that minority protection disputes are not left solely to the interpretation of the state whose conduct is in question, coordinating oversight across guarantor states, Turkey, and the European human rights system.
% TRANSFER_FUNCTION: Notionally shifts interpretive authority over minority protections from purely domestic Turkish adjudication toward a supervised, internationally reviewable process; in practice moves very little — diplomatic attention and occasional ECHR findings flow toward affected communities, but enforceable remedy does not reliably follow.
% ABSENT_VOICES: The affected religious communities themselves have no standing to invoke guarantor-state diplomacy directly; they depend on foreign governments choosing to raise their case, and on domestic courts first allowing a claim to mature to a point where international review becomes relevant. Their voice enters the process only derivatively, through governments and courts that have their own competing interests.
% DISAPPEARANCE_RATIONALE: If the guarantor/ECHR supervisory pathway were formally repudiated, Turkey's domestic courts would continue adjudicating minority institution disputes exactly as they largely do now, so proponents of the restrictive reading would say the world stays unchanged. Minority communities and human rights observers would say something real is lost: the diplomatic leverage point and occasional external findings that currently produce incremental pressure, however weak, would vanish entirely.
% FOUNDING_PROBLEM: In 1923, the Lausanne Conference needed to resolve what would happen to non-Muslim communities remaining inside the new Turkish state after the population exchanges, and the guarantor powers wanted assurance that minority treatment would not be left entirely to unilateral domestic discretion.
% FOUNDING_PROBLEM_CORROBORATION: Council of Europe monitoring bodies and independent minority-rights NGOs (outside both the Turkish state and the guarantor states) attest that unresolved property and institutional-recognition disputes persist, supporting a 'still live' reading; Turkish government representatives attest the matter is fully internalized into domestic law and that continued international invocation is an unwarranted revival of a settled question.
narrative_ontology:disappearance_verdict(lausanne_minority_protections__guarantor_reading, contested).
narrative_ontology:founding_problem_status(lausanne_minority_protections__guarantor_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lausanne_minority_protections__guarantor_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(lausanne_minority_protections__guarantor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lausanne_minority_protections__guarantor_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lausanne_minority_protections__guarantor_reading_tests).
:- end_tests(lausanne_minority_protections__guarantor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.28) because this reading does not claim a substantive transfer of resources or institutional control — it claims a procedural pathway. The modest upward drift in extraction over the interval reflects that as domestic courts have narrowed the practical availability of the international pathway, communities bear a rising cost of navigating a claim that mostly does not resolve. Theater ratio is authored substantially higher (0.58 by interval end) and rising, because the diplomatic-invocation and ECHR-referral machinery increasingly functions as a symbolic gesture — guarantor states cite 'ongoing supervision' as a diplomatic talking point while rarely following through with material pressure, and this performative function has grown relative to the mechanism's genuine remedial output. Suppression (0.35) is moderate: there is no active coercion preventing communities from pursuing the international pathway, but domestic procedural gatekeeping functions as a structural barrier. Accessibility collapse (0.4) and resistance (0.55) reflect that the alternative (direct international enforcement without domestic mediation) has not fully collapsed as a concept — it remains actively argued for by minority-rights advocates — but is not currently available in practice.
 *
 * DIRECTIONALITY LOGIC:
 *   Non-muslim minority communities are both the notional beneficiary (the pathway exists on their behalf) and a payer (they bear the cost of a slow, unreliable process while institutional harms continue) — this dual role is captured with a secondary_role. Guarantor states and the ECHR sit near the beneficiary/observer end: they hold standing and adjudicative authority without bearing the costs of non-remedy. The Turkish state and domestic courts sit as agenda-setters with high exit options (arbitrage/constrained-but-controlling) because they administer first-instance interpretation and can effectively determine how much of the international pathway ever becomes operative. Minority communities awaiting remedy are the clearest payer seat: trapped, powerless, bearing the accumulated cost of unresolved disputes.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — assuring minority communities were not left to unilateral domestic discretion — is contested rather than resolved: the mismatch between founding_problem_status=contested and disappearance_verdict=contested itself signals that this is not a settled scaffold whose function has clearly expired, but one whose value is actively disputed along the same lines as the underlying kernel contest. Classifying this as scaffold (rather than snare or piton) prevents two errors: treating a genuinely weak but non-zero external check as pure extraction (which would ignore the real, if intermittent, diplomatic and judicial pressure it has generated), and treating it as fully functional binding law (which would overstate its enforceability and obscure the rising theater ratio documented in the measurements).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    venue_scope_orthogonality,
    'Is the guarantor/ECHR supervisory pathway (a venue claim) genuinely separable from the substantive scope dispute between the expansive and restrictive readings (what is protected), or does the choice of venue implicitly resolve the scope question by determining which body''s precedents govern?',
    'Track ECHR case law to see whether cases reaching the international venue are decided using expansive-reading-consistent reasoning (institutional continuity) or restrictive-reading-consistent reasoning (individual worship only); if ECHR jurisprudence consistently imports one substantive reading, the venue and scope questions are not independent.',
    'If venue determines scope outcomes, the guarantor reading is not a neutral procedural claim but a covert vehicle for one of the substantive readings, which would change its beneficiary structure and its relationship to the sibling readings from coexists_with toward influences or forecloses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(venue_scope_orthogonality, conceptual, 'Whether the procedural (who-adjudicates) claim is separable from the substantive (what-is-protected) claim.').

omega_variable(
    guarantor_state_invocation_incentive,
    'Do guarantor states invoke their Lausanne standing based on genuine assessment of minority-rights conditions, or primarily as diplomatic leverage tied to unrelated bilateral disputes with Turkey?',
    'Compare timing and content of guarantor-state diplomatic statements on Lausanne minority issues against concurrent bilateral disputes (EU accession negotiations, defense agreements, migration deals) to test correlation.',
    'If invocation tracks unrelated bilateral leverage rather than minority-rights conditions, the theater_ratio is understated and the mechanism is closer to a dormant diplomatic instrument than an active supervisory scaffold — pushing the classification toward piton.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(guarantor_state_invocation_incentive, empirical, 'Whether guarantor-state invocation is rights-driven or leverage-driven.').

omega_variable(
    echr_remedy_conversion_rate,
    'What proportion of ECHR findings favorable to Lausanne-protected minority communities have converted into actual domestic institutional or property remedy within a reasonable timeframe?',
    'Systematic review of ECHR judgments concerning Turkish minority institutions and their domestic implementation status, tracked longitudinally.',
    'A low conversion rate would confirm the scaffold has degraded toward theater (supporting the rising theater_ratio trend); a substantial conversion rate would support treating the mechanism as a functioning, if slow, coordination structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(echr_remedy_conversion_rate, empirical, 'Whether ECHR findings produce actual domestic remedy for minority institutions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lausanne_minority_protections__guarantor_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(laus_tr_t0, lausanne_minority_protections__guarantor_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(laus_tr_t8, lausanne_minority_protections__guarantor_reading, theater_ratio, 8, 0.38).
narrative_ontology:measurement(laus_tr_t16, lausanne_minority_protections__guarantor_reading, theater_ratio, 16, 0.45).
narrative_ontology:measurement(laus_tr_t24, lausanne_minority_protections__guarantor_reading, theater_ratio, 24, 0.5).
narrative_ontology:measurement(laus_tr_t32, lausanne_minority_protections__guarantor_reading, theater_ratio, 32, 0.55).
narrative_ontology:measurement(laus_tr_t40, lausanne_minority_protections__guarantor_reading, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(laus_be_t0, lausanne_minority_protections__guarantor_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(laus_be_t8, lausanne_minority_protections__guarantor_reading, base_extractiveness, 8, 0.2).
narrative_ontology:measurement(laus_be_t16, lausanne_minority_protections__guarantor_reading, base_extractiveness, 16, 0.22).
narrative_ontology:measurement(laus_be_t24, lausanne_minority_protections__guarantor_reading, base_extractiveness, 24, 0.25).
narrative_ontology:measurement(laus_be_t32, lausanne_minority_protections__guarantor_reading, base_extractiveness, 32, 0.27).
narrative_ontology:measurement(laus_be_t40, lausanne_minority_protections__guarantor_reading, base_extractiveness, 40, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(lausanne_minority_protections__guarantor_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lausanne_minority_protections__guarantor_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(lausanne_minority_protections__guarantor_reading, 0.12).
narrative_ontology:affects_constraint(lausanne_minority_protections__guarantor_reading, lausanne_minority_protections__expansive_reading).
narrative_ontology:affects_constraint(lausanne_minority_protections__guarantor_reading, lausanne_minority_protections__restrictive_reading).

% DUAL FORMULATION NOTE:
% Three linked stories decompose the natural-language 'Lausanne minority protections' claim: expansive_reading (substantive guarantee of institutional continuity), restrictive_reading (individual worship rights only, institutional matters are domestic), and this guarantor_reading (a venue/authority claim about WHO adjudicates, orthogonal in principle to the substantive scope debate but empirically entangled per the venue_scope_orthogonality omega). Each carries its own ε: the guarantor reading is authored at low extraction (0.28, procedural scaffold with weak enforcement) versus what would likely be a higher ε for the expansive reading's contested substantive claim and a near-zero ε for the restrictive reading's narrower, less contested claim. The three are linked bidirectionally in network.affects_constraints because a shift in which venue prevails structurally affects which substantive reading gets adjudicated.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
