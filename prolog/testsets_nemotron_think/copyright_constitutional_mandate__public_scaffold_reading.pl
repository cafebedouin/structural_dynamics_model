% ============================================================================
% CONSTRAINT STORY: copyright_constitutional_mandate__public_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_copyright_constitutional_mandate__public_scaffold_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: copyright_constitutional_mandate__public_scaffold_reading
 *   human_readable: Copyright as Temporary Scaffold for Public Domain Enrichment
 *   domain: intellectual_property/constitutional_law/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the public_scaffold_reading of the
 *   copyright_constitutional_mandate kernel. The Progress Clause (Article I,
 *   Section 8, Clause 8) authorizes Congress to grant 'exclusive Rights' to
 *   'Authors' for 'limited Times' to 'promote the Progress of Science and
 *   useful Arts.' This reading takes the clause's stated purpose — public
 *   enrichment through eventual public domain entry — as the constraint's
 *   telos. The temporary monopoly is a scaffold: it exists to be dismantled
 *   once the work enters the public domain. The structural beneficiaries are
 *   the public and the public domain itself; creators are instrumental
 *   beneficiaries (the means, not the end). No victim class exists in this
 *   reading — the arrangement is a coordination regime solving the public
 *   goods problem of creative production. Historical drift shows term
 *   extensions (1790: 14+14 years → 1998: life+70) increasing extractiveness
 *   and theater while the scaffold's sunset function attenuates.
 *
 * KEY AGENTS:
 *   - the_public: Primary beneficiary (powerless/constrained) — receives enriched public domain
 *   - public_domain: Institutional beneficiary (analytical/universal) — the commons that accumulates works
 *   - creators_authors: Instrumental beneficiary (moderate/constrained) — granted temporary monopoly as incentive
 *   - congress: Agenda setter (institutional/arbitrage) — sets term lengths and scope
 *   - courts: Observer (institutional/analytical) — interprets 'limited Times' and 'Progress'
 *   - public_domain_advocates: Beneficiary (organized/mobile) — litigate and lobby for term limits and fair use
 *   - corporate_rightsholders: Excluded (powerful/trapped) — benefit from term extensions but not recognized as intended beneficiaries in this reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__public_scaffold_reading, 0.25).
domain_priors:suppression_score(copyright_constitutional_mandate__public_scaffold_reading, 0.4).
domain_priors:theater_ratio(copyright_constitutional_mandate__public_scaffold_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__public_scaffold_reading, scaffold).
narrative_ontology:human_readable(copyright_constitutional_mandate__public_scaffold_reading, "Copyright as Temporary Scaffold for Public Domain Enrichment").
narrative_ontology:topic_domain(copyright_constitutional_mandate__public_scaffold_reading, "intellectual_property/constitutional_law/political_economy").

domain_priors:requires_active_enforcement(copyright_constitutional_mandate__public_scaffold_reading).
narrative_ontology:has_sunset_clause(copyright_constitutional_mandate__public_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__public_scaffold_reading, '7bb61209-e995-4959-ae43-552dce7d7433').
narrative_ontology:cs_kernel_codification('7bb61209-e995-4959-ae43-552dce7d7433', fixed_text).
narrative_ontology:cs_authority_grounding('7bb61209-e995-4959-ae43-552dce7d7433', lineage).
narrative_ontology:cs_interpretation_layer_present('7bb61209-e995-4959-ae43-552dce7d7433').
narrative_ontology:cs_reading_relation('7bb61209-e995-4959-ae43-552dce7d7433', copyright_constitutional_mandate__corporate_enclosure_reading, coexists_with).
narrative_ontology:cs_reading_relation('7bb61209-e995-4959-ae43-552dce7d7433', copyright_constitutional_mandate__judicial_ambiguity_reading, influences).
narrative_ontology:cs_axiom('7bb61209-e995-4959-ae43-552dce7d7433', foundational, copyright_serves_public_domain_enrichment).
narrative_ontology:cs_axiom_status(copyright_serves_public_domain_enrichment, holdable).
narrative_ontology:cs_axiom_grounding('7bb61209-e995-4959-ae43-552dce7d7433', copyright_serves_public_domain_enrichment, deontological).
narrative_ontology:cs_axiom('7bb61209-e995-4959-ae43-552dce7d7433', foundational, limited_times_requires_actual_limitation).
narrative_ontology:cs_axiom_status(limited_times_requires_actual_limitation, holdable).
narrative_ontology:cs_axiom_grounding('7bb61209-e995-4959-ae43-552dce7d7433', limited_times_requires_actual_limitation, conventional).
narrative_ontology:cs_reference_frame('7bb61209-e995-4959-ae43-552dce7d7433', progress_clause_original_purpose).
narrative_ontology:cs_drift_state('7bb61209-e995-4959-ae43-552dce7d7433', post_sonny_bono_extension, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7bb61209-e995-4959-ae43-552dce7d7433', '').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__public_scaffold_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, the_public).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, public_domain).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, creators_authors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, public_domain_advocates).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__public_scaffold_reading, progress_clause_purpose).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__public_scaffold_reading, limited_times_doctrine).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__public_scaffold_reading, fair_use_expansion_norm).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__public_scaffold_reading, anti_enclosure_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ultimate beneficiary of the constitutional bargain: receives an enriched public domain after temporary monopoly expires. Exit is constrained — cannot individually opt out of copyright term extensions, but benefits from fair use, public domain dedications, and creative commons alternatives. The public's enrichment is the constraint's telos in this reading.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, the_public, beneficiary,
    powerless, generational, constrained, national).

% The institutional commons that accumulates works upon term expiration. Not an actor but the structural recipient of the scaffold's sunset function. Its enrichment is the measure of the constraint's success. In this reading, the public domain is a vindicated proposition (progress_clause_purpose) made concrete.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, public_domain, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(copyright_constitutional_mandate__public_scaffold_reading, public_domain).

% Granted temporary exclusive rights as incentive to create. They benefit from the monopoly during its term but are not the constraint's end beneficiary — their reward is the means, not the purpose. Exit options are constrained: they can choose open licensing (CC0, public domain dedication) but cannot unilaterally shorten statutory terms.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, creators_authors, beneficiary,
    moderate, biographical, constrained, national).

% Sets copyright term lengths and scope through legislation. Has arbitrage-grade exit: can shorten terms, expand fair use, or add formalities at any time. In practice, has extended terms repeatedly (1790, 1831, 1909, 1976, 1998) under lobbying pressure. The scaffold reading holds Congress to the Progress Clause's purpose as a binding constraint on its discretion.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, congress, agenda_setter,
    institutional, generational, arbitrage, national).

% Interpret 'limited Times' and 'promote the Progress' through judicial review. In this reading, courts should enforce the scaffold's sunset function (striking down perpetual terms) and expand fair use as the scaffold's adaptive mechanism. Historically, courts have deferred to Congress (Eldred v. Ashcroft), functioning as observers rather than active scaffold-maintainers.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, courts, observer,
    institutional, generational, analytical, national).

% Organizations (EFF, Public Knowledge, Creative Commons, Internet Archive) that litigate for term limits, fair use expansion, and public domain preservation. They have mobile exit: can shift venues (legislative, judicial, international), build alternative commons (CC licensing), and mobilize public opinion. They are the scaffold's maintenance crew.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, public_domain_advocates, beneficiary,
    organized, biographical, mobile, national).

% Large copyright holders (Disney, Warner, major publishers) who benefit from term extensions and narrow fair use. In this reading, they are excluded from the beneficiary set — the constraint's purpose is not their enrichment. They would object to term shortening and fair use expansion. Their 'trapped' exit reflects dependence on copyright rents; they cannot easily adapt to a scaffold that actually sunsets.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, corporate_rightsholders, excluded,
    powerful, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(copyright_constitutional_mandate__public_scaffold_reading, diffuse).
narrative_ontology:fixing_cost_class(copyright_constitutional_mandate__public_scaffold_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the public goods problem of creative work production: without temporary monopoly, creators cannot recoup fixed costs of production, leading to underproduction. The scaffold coordinates by granting exclusive rights for a limited term, after which works enter the public domain for free use and further creation.
% TRANSFER_FUNCTION: Moves temporary monopoly rights (exclusion, licensing revenue) from the public to creators/authors for a limited term; then moves the work itself (full use rights, no permission needed) from monopoly control to the public domain permanently. The transfer is bidirectional and time-gated.
% ABSENT_VOICES: Corporate rightsholders who capture monopoly rents beyond incentive-calibrated terms; future generations who lose access to cultural works that would have entered public domain but for term extensions; orphan works creators whose works are locked up despite no active rightsholder. These voices are structurally excluded from the legislative process that sets terms.
% DISAPPEARANCE_RATIONALE: If copyright's temporary monopoly vanished overnight, creative production would reorganize: some works would not be produced without monopoly incentive (especially high-fixed-cost works like films), but many would shift to alternative funding (patronage, crowdfunding, first-mover advantage, services). The public domain would immediately absorb all works. The creative economy would restructure around post-monopoly models.
% FOUNDING_PROBLEM: The Constitution's Progress Clause was adopted because the Articles of Confederation lacked federal copyright power, leading to state-level protectionism and underproduction of nationally distributed creative works. The founding problem: how to incentivize creation and national distribution of 'Science and useful Arts' while ensuring eventual public access.
% FOUNDING_PROBLEM_CORROBORATION: Historical records: Madison's Federalist 43 ('The utility of this power will scarcely be questioned'); Constitutional Convention debates showing concern for both author incentive and public access. Economic analyses: Landes & Posner (1989) on optimal copyright term; Heald (2014) showing public domain availability increases with shorter terms; Buccafusco & Heald (2019) on empirical copyright term effects. These sources outside the corporate beneficiary set corroborate the contested status.
narrative_ontology:disappearance_verdict(copyright_constitutional_mandate__public_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(copyright_constitutional_mandate__public_scaffold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__public_scaffold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(copyright_constitutional_mandate__public_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(copyright_constitutional_mandate__public_scaffold_reading, 0.25, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(copyright_constitutional_mandate__public_scaffold_reading_tests).
:- end_tests(copyright_constitutional_mandate__public_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-moderate (0.25) because the monopoly is temporary and calibrated to incentive needs, not rent maximization. Suppression (0.40) reflects enforcement against infringement but fair use and public domain provide exits. Theater ratio (0.20) is low because enforcement primarily serves the coordination function (incentivizing creation), though term extensions and DMCA anti-circumvention provisions increase performative enforcement. Accessibility collapse (0.50) is moderate: alternatives (public domain, fair use, creative commons) exist but are constrained by term lengths. Resistance (0.40) reflects ongoing public domain advocacy and fair use litigation. The scaffold classification fits: has_sunset_clause (limited times), beneficiaries declared, requires_active_enforcement (infringement actions), and the founding problem (underproduction of creative works) remains contested in digital era.
 *
 * PERSPECTIVAL GAP:
 *   From the public's seat, the constraint appears as a genuine scaffold — temporary monopoly yielding permanent public enrichment. From corporate rightsholders' seat (excluded in this reading), the same constraint appears as a weakening property right they seek to strengthen. From Congress's seat, the constraint is a policy lever they extend repeatedly. The engine computes per-seat classifications from these structural positions; this reading's claim is that the scaffold classification holds from the constitutional purpose seat.
 *
 * DIRECTIONALITY LOGIC:
 *   The public and public domain are structural beneficiaries (d near 0.0) — the constraint's telos is their enrichment. Creators are instrumental beneficiaries (d ~ 0.3) — they receive monopoly rents but only as means to public end. Congress as agenda_setter has arbitrage exit (d ~ 0.1) — they can shorten terms at any time. Courts as observers have analytical exit (d = 0.5). Corporate rightsholders are excluded from this reading's beneficiary structure; their high directionality in the corporate_enclosure_reading is a different constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (creative work underproduction) is contested: digital production/distribution costs have collapsed, potentially solving the original public goods problem. If the founding problem is dead but the scaffold persists with extended terms, mandatrophy applies — the constraint becomes a piton or snare. This reading asserts the problem remains live but the scaffold has drifted (terms too long, fair use too narrow). The mandate is resolved only if terms return to incentive-calibrated lengths.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_frame_kernel_reading,
    'This constraint is the public_scaffold_reading of the copyright_constitutional_mandate kernel. How does the sibling corporate_enclosure_reading change the beneficiary structure and extractiveness profile?',
    'Comparative constraint story generation for each reading; structural delta analysis of beneficiary/victim sets and epsilon values across readings.',
    'If corporate_enclosure_reading shows high extractiveness with corporate beneficiaries, the kernel itself contains a structural ambiguity: the same constitutional text generates both a low-extraction scaffold and a high-extraction enclosure depending on reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_kernel_reading, conceptual, 'Commitment kernel decomposition: public scaffold vs corporate enclosure vs judicial ambiguity readings of the Progress Clause').

omega_variable(
    optimal_term_length_for_public_enrichment,
    'What copyright term length actually maximizes public domain enrichment versus merely transferring rents to rightsholders?',
    'Empirical analysis of creative production incentives under different term regimes; economic modeling of marginal incentive vs. public access loss.',
    'If current terms exceed the incentive-maximizing length, the constraint has drifted from scaffold into extraction; if terms are near-optimal, the scaffold reading remains descriptively accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(optimal_term_length_for_public_enrichment, empirical, 'Whether the temporary monopoly''s duration is calibrated to public enrichment or has become rent extraction').

omega_variable(
    fair_use_as_coordination_mechanism,
    'Does expanded fair use function as a genuine coordination mechanism (reducing transaction costs for transformative use) or as a judicial safety valve that legitimates an otherwise overbroad monopoly?',
    'Case law trajectory analysis: if fair use expands systematically with technology, it''s coordination; if it expands only to prevent constitutional challenge, it''s legitimation.',
    'If fair use is coordination, the scaffold has adaptive capacity; if legitimation, the scaffold''s sunset function is compromised by judicial patching.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fair_use_as_coordination_mechanism, conceptual, 'Structural role of fair use doctrine within the copyright scaffold').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__public_scaffold_reading, 0, 234).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copyright_public_scaffold_tr_t0, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(copyright_public_scaffold_tr_t41, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 41, 0.08).
narrative_ontology:measurement(copyright_public_scaffold_tr_t119, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 119, 0.12).
narrative_ontology:measurement(copyright_public_scaffold_tr_t186, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 186, 0.22).
narrative_ontology:measurement(copyright_public_scaffold_tr_t208, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 208, 0.3).
narrative_ontology:measurement(copyright_public_scaffold_tr_t234, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 234, 0.2).

% Extraction over time
narrative_ontology:measurement(copyright_public_scaffold_be_t0, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(copyright_public_scaffold_be_t41, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 41, 0.15).
narrative_ontology:measurement(copyright_public_scaffold_be_t119, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 119, 0.2).
narrative_ontology:measurement(copyright_public_scaffold_be_t186, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 186, 0.28).
narrative_ontology:measurement(copyright_public_scaffold_be_t208, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 208, 0.35).
narrative_ontology:measurement(copyright_public_scaffold_be_t234, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 234, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(copyright_public_scaffold_su_t0, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(copyright_public_scaffold_su_t41, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 41, 0.25).
narrative_ontology:measurement(copyright_public_scaffold_su_t119, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 119, 0.35).
narrative_ontology:measurement(copyright_public_scaffold_su_t186, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 186, 0.45).
narrative_ontology:measurement(copyright_public_scaffold_su_t208, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 208, 0.55).
narrative_ontology:measurement(copyright_public_scaffold_su_t234, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 234, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__public_scaffold_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(copyright_constitutional_mandate__public_scaffold_reading, 0.15).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__public_scaffold_reading, copyright_constitutional_mandate__corporate_enclosure_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__public_scaffold_reading, copyright_constitutional_mandate__judicial_ambiguity_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the Progress Clause into three structurally distinct readings with different beneficiary structures, extractiveness profiles, and constraint types. The public scaffold reading yields a Scaffold with public domain beneficiaries; the corporate enclosure reading yields a Snare/Tangled Rope with corporate beneficiaries; the judicial ambiguity reading yields a Piton/Rope with legislative discretion as the coordination function.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(copyright_constitutional_mandate__public_scaffold_reading, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
