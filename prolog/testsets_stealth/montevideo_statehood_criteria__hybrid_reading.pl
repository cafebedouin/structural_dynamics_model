% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_montevideo_statehood_criteria__hybrid_reading, []).

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
 *   constraint_id: montevideo_statehood_criteria__hybrid_reading
 *   human_readable: Hybrid Reading of Statehood: Objective Criteria Conditioned by Normative Legitimacy
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   Since the European Community's December 1991 Guidelines on Recognition of
 *   New States and the Badinter process, a substantial strand of recognition
 *   practice has held that effective control of territory and population is
 *   not enough for statehood: an aspirant must also exhibit democratic
 *   governance, respect for human and minority rights, and renunciation of
 *   territorial conquest. This file instantiates the hybrid reading of the
 *   montevideo_statehood_criteria kernel as a single epsilon-invariant
 *   constraint: the standing arrangement under assessment is recognition
 *   conditioned on liberal legitimacy, with its own stable beneficiary
 *   structure (gatekeeping discretion accruing to established liberal
 *   democracies) and victim structure (effective non-liberal authorities
 *   denied membership, the populations they govern, and compliant polities
 *   denied anyway). The declaratory and constitutive readings are separate
 *   constraints in separate files, linked through the network section; their
 *   differing structures are documented in the omega variables rather than
 *   averaged into this one. The claim/metric gap is deliberate: the reading
 *   CLAIMS tangled_rope (genuine coordination core plus asymmetric
 *   extraction) while the metrics are authored independently from its
 *   observable operation, including a rising theater trajectory.
 *
 * KEY AGENTS:
 *   - established_liberal_democracies: agenda setter and principal beneficiary (institutional/arbitrage) — administers the recognition consensus and collects the discretion it confers
 *   - nonliberal_secessionist_authorities: primary target (moderate/trapped) — effective territorial control that does not convert into membership
 *   - populations_of_unrecognized_entities: diffuse target (powerless/trapped) — bear the status costs without a seat in the decision
 *   - democratic_unrecognized_polities: compliant-but-denied witnesses (moderate/trapped) — their exclusion despite compliance exposes the selectivity
 *   - nonliberal_great_powers: countervailing institutional actors (institutional/constrained) — contest the standard and absorb its downstream costs
 *   - international_courts_and_scholars: analytical observer (analytical/analytical) — record which justifications were offered and which were honored
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__hybrid_reading, 0.58).
domain_priors:suppression_score(montevideo_statehood_criteria__hybrid_reading, 0.62).
domain_priors:theater_ratio(montevideo_statehood_criteria__hybrid_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(montevideo_statehood_criteria__hybrid_reading, "Hybrid Reading of Statehood: Objective Criteria Conditioned by Normative Legitimacy").
narrative_ontology:topic_domain(montevideo_statehood_criteria__hybrid_reading, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(montevideo_statehood_criteria__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__hybrid_reading, '256bc4db-7cd4-4586-afa8-acad2bf98973').
narrative_ontology:cs_kernel_codification('256bc4db-7cd4-4586-afa8-acad2bf98973', formalized).
narrative_ontology:cs_authority_grounding('256bc4db-7cd4-4586-afa8-acad2bf98973', distributed).
narrative_ontology:cs_reading_relation('256bc4db-7cd4-4586-afa8-acad2bf98973', montevideo_statehood_criteria__declaratory_reading, forecloses).
narrative_ontology:cs_reading_relation('256bc4db-7cd4-4586-afa8-acad2bf98973', montevideo_statehood_criteria__constitutive_reading, coexists_with).
narrative_ontology:cs_axiom('256bc4db-7cd4-4586-afa8-acad2bf98973', foundational, normative_legitimacy_gatekeeps_statehood).
narrative_ontology:cs_axiom_status(normative_legitimacy_gatekeeps_statehood, holdable).
narrative_ontology:cs_axiom_grounding('256bc4db-7cd4-4586-afa8-acad2bf98973', normative_legitimacy_gatekeeps_statehood, deontological).
narrative_ontology:cs_axiom('256bc4db-7cd4-4586-afa8-acad2bf98973', secondary, aggressive_acquisition_vetoes_recognition).
narrative_ontology:cs_axiom_status(aggressive_acquisition_vetoes_recognition, holdable).
narrative_ontology:cs_axiom_grounding('256bc4db-7cd4-4586-afa8-acad2bf98973', aggressive_acquisition_vetoes_recognition, conventional).
narrative_ontology:cs_reference_frame('256bc4db-7cd4-4586-afa8-acad2bf98973', criteria_plus_liberal_legitimacy_floor).
narrative_ontology:cs_drift_state('256bc4db-7cd4-4586-afa8-acad2bf98973', contemporary_multipolar_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('256bc4db-7cd4-4586-afa8-acad2bf98973', '').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__hybrid_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, established_liberal_democracies).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, nonliberal_secessionist_authorities).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, populations_of_unrecognized_entities).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, democratic_unrecognized_polities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, nonliberal_great_powers).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__hybrid_reading, democratic_entitlement_doctrine).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__hybrid_reading, conditional_recognition_practice).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__hybrid_reading, ex_injuria_non_oritur_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Act collectively — through the European Union's recognition guidelines, Security Council practice, and coordinated diplomacy — to decide which new entities enter the state system. They drafted the post-1991 guidelines tying recognition to democratic governance, human-rights commitments, and renunciation of conquest. They collect the discretion the standard confers: the ability to admit allies' projects and refuse adversaries' without abandoning a principled vocabulary. Their exit is easy in the relevant sense — they can revise the criteria, apply them selectively, or revert to interest-based recognition whenever the standard becomes inconvenient.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, established_liberal_democracies, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(montevideo_statehood_criteria__hybrid_reading, established_liberal_democracies, beneficiary).

% Control territory and population effectively — running administrations, collecting revenue, holding borders — but govern without liberal-democratic credentials or decline to pledge the required conduct commitments. No amount of effective state-building converts into membership while the conditions stand unmet. Their realistic paths are indefinite de facto existence, internal liberalization on terms they reject, or sponsorship by a great power willing to recognize them unilaterally — an option open to few.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, nonliberal_secessionist_authorities, payer,
    moderate, biographical, trapped, regional).

% Live under authorities whose status is withheld. They cannot access the ordinary instruments of statehood — treaty protections, development finance, travel documents accepted abroad, courts with international reach — regardless of their own conduct or preferences. Their voice in the decision reaches the recognizing states only filtered through the very authorities being judged.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, populations_of_unrecognized_entities, payer,
    powerless, biographical, trapped, regional).

% Built functioning electoral systems, maintained defensible human-rights records, and renounced conquest — satisfying every published condition — yet remain unrecognized because key gatekeeping states' interests favor the territorial status quo. Their continued exclusion despite compliance is the standing public test of whether the published criteria or the underlying interests drive decisions.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, democratic_unrecognized_polities, payer,
    moderate, biographical, trapped, regional).

% Hold permanent seats and veto power yet reject the legitimacy conditions as selectively applied gatekeeping. They absorb costs when the standard is turned against their partners — recognition bids refused, intervention justified against aligned regimes — and answer with counter-recognition, parallel institutions, and their own sphere-of-interest vocabulary. They cannot leave the system that hosts the argument.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, nonliberal_great_powers, payer,
    institutional, generational, constrained, global).

% Adjudicate and document: advisory opinions on secession and recognition, arbitration commissions, and the scholarly literature recording which justifications were offered, which were honored, and which were quietly dropped. They collect no revenue from the arrangement and bear none of its status costs.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, international_courts_and_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(montevideo_statehood_criteria__hybrid_reading, established_liberal_democracies).
narrative_ontology:fixing_cost_class(montevideo_statehood_criteria__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared admission standard that coordinates recognition decisions across dozens of states during state-creation events, reducing recognition wars and competitive client-making, and attaches baseline conduct expectations — democratic governance, human-rights compliance, non-aggression — to entry into the state system.
% TRANSFER_FUNCTION: Moves admission discretion and normative authority from aspiring entities to the gatekeeping states; moves compliance concessions (elections, minority-rights guarantees, non-aggression pledges) from aspirants to the existing club; moves the reputational and material costs of denial onto the denied and the populations they govern.
% ABSENT_VOICES: The populations whose status is decided have no seat — residents of unrecognized entities are represented only by the authorities whose recognition is contested. Compliant-but-denied polities have no forum where their compliance record counts. Non-liberal aspirant elites speak only through sponsoring states. Dissent exists in the record (General Assembly objections, counter-recognition notes) but sits outside the rooms where guidelines are drafted.
% DISAPPEARANCE_RATIONALE: If the legitimacy conditions vanished overnight, recognition decisions would revert to pure effectiveness and bilateral interest; several dozen de facto entities would immediately press claims; liberal states would lose their standard vocabulary for coordinated denial and would have to defend refusals on naked interest grounds; intervention justifications currently borrowing the legitimacy vocabulary would need new grounding; the UN membership boundary would churn as counter-recognitions met recognitions.
% FOUNDING_PROBLEM: The simultaneous dissolution of the USSR and SFR Yugoslavia (1989–1992) threatened a recognition free-for-all in which armed groups could manufacture facts on the ground and win international legitimacy by conquest. The European Community's 1991 Guidelines and the Badinter process conditioned recognition on democracy, rule of law, human and minority rights, and frontier inviolability, to stabilize the transition and deny reward to aggression.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: the Badinter Commission's own opinions, the recorded objections of non-aligned and non-liberal states in the recognition debates, and three decades of scholarship (Crawford on statehood, Franck on the democratic entitlement, Coggins on recognition politics) attest both that the dissolution crisis was real and that the conditionality's subsequent selective application is disputed. No neutral body attests that the founding problem remains live in its original form today; the gatekeeping states assert it, the excluded and their sponsors deny it.
narrative_ontology:disappearance_verdict(montevideo_statehood_criteria__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(montevideo_statehood_criteria__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(montevideo_statehood_criteria__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(montevideo_statehood_criteria__hybrid_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(montevideo_statehood_criteria__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(montevideo_statehood_criteria__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(montevideo_statehood_criteria__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58 because the legitimacy conditions impose real, uncompensated costs on those they exclude — denial of treaty protections, development finance, accepted documents, and international legal personality — while the conditions themselves are applied selectively enough that compliance does not purchase admission. Suppression is 0.62 as a raw structural property (unscaled by power or scope): collective non-recognition, sanctions exposure, and diplomatic isolation actively close the alternative path of simple effectiveness-based statehood, though de facto persistence remains possible, so alternatives are narrowed rather than eliminated. Theater is 0.46 and rising: election-monitoring rituals, democracy clauses, and legitimacy discourse increasingly decorate decisions made on interest grounds, the classic Goodhart signature of a proxy replacing the target. Accessibility collapse is moderate (0.52) because a genuine exit exists — liberalize on the published terms — but the compliant-but-denied cases show it does not reliably open. Resistance is 0.58: non-liberal great powers contest the standard openly, aspirants denounce it as gatekeeping, and counter-recognition practices blunt its universality. The temporal series run on one shared grid (t=0..35, all three metrics at every point) so no metric inherits another's end-state value; the trajectories encode extraction accumulation, theater drift, and enforcement hardening anchored to the Kosovo recognition split, the Libya-era intervention vocabulary, and the multipolar contestation of the 2020s. Coalition note: the powerless population seat is not without recourse — diaspora advocacy and reputation-building of the kind the compliant polities attempted are the available coalition levers, and their repeated failure is itself evidence registered in the theater series. Identity-lock note: parts of the international-law profession and the EU enlargement bureaucracy have fused professionally with the democratic-entitlement project; if that frame broke, the criteria would be read as one negotiable instrument among others rather than a settled acquis. Suppression mechanism: predominantly structural (roughly 85 percent — collective-action barriers, finance cutoffs, isolation), with an internalized residue in which aspirant elites adopt legitimacy language as the only admissible currency, shaping their own demands.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat should compute differently. From the gatekeeping seat the arrangement is a principled admission standard it built and maintains, with the discretion it confers experienced as stewardship; from the trapped aspirant and population seats the same structure operates as a door that opens only for those who do not need it. The compliant-but-denied seat occupies a third position: it did everything asked and was refused anyway, so it experiences the published criteria as performance. The countervailing great-power seat experiences the standard as weaponized law turned selectively against its clients. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Established liberal democracies sit near the beneficiary end: they wrote the conditions, administer the consensus, and collect the discretion, with arbitrage-grade exit (they can revise or selectively apply the standard at will). Nonliberal secessionist authorities sit near the target end: they bear the full denial burden with trapped exit — effectiveness does not convert, and the sanctioned alternative (great-power sponsorship) is available only to a few. Populations of unrecognized entities sit nearest the full-target end: powerless, trapped, and bearing costs proportional to their exclusion with zero input. Democratic unrecognized polities are the diagnostic anomaly: structurally they should derive as near-beneficiaries of a standard they satisfy, yet they are denied — which is precisely why the theater ratio matters and why the criterion-versus-interest omega is load-bearing. Nonliberal great powers derive mid-to-high: they pay (clients denied, intervention cover used against aligned regimes) but hold countervailing power (vetoes, counter-recognition) that keeps them from the trapped end. No directionality overrides are used: the beneficiary/victim declarations plus exit options produce the right ordering, and the per-power-atom granularity of overrides would misfire across the two institutional seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — stabilizing the simultaneous Soviet and Yugoslav dissolutions so that conquest could not manufacture statehood — was real, urgent, and largely specific to that wave. The arrangement built for it was never given a sunset: conditionality hardened from transitional guidance into permanent gatekeeping, and its theater share grew as the original crisis receded. Classifying as tangled_rope rather than snare preserves what is genuinely still coordinated: a shared admission standard does solve a real collective-action problem among dozens of recognizing states, and participants are not uniformly net losers. Classifying as anything purer would mislabel either the surviving coordination function (as pure extraction) or the accumulating asymmetry (as innocent overhead). The mismatch consumer should note the configuration here: a founding problem whose status is contested, attached to a world_rearranges verdict — the arrangement's disappearance would indeed reorganize recognition practice, but whether the problem it was built for still justifies the current form is exactly what the parties dispute.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_structural_delta,
    'This story is one reading of the montevideo_statehood_criteria kernel; how would the sibling readings restructure the beneficiary and victim sets?',
    'Compile the declaratory and constitutive readings as separate constraint stories and compare computed seat classifications. The disagreement between readings is located in one specific element: whether the four objective criteria are sufficient for statehood.',
    'Under the declaratory reading the normative victim set empties (only entities failing the objective criteria remain excluded) and the liberal gatekeeping benefit disappears; under the constitutive reading discretion relocates from published criteria to the recognition act itself, changing which seat holds agenda-setting power. This file''s epsilon and victim set are valid only for the hybrid reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Kernel indexicality: one of three readings, each instantiating a structurally distinct constraint.').

omega_variable(
    criterion_vs_interest_operativity,
    'Is the operative decision rule the published legitimacy criteria, or great-power interest using the criteria as cover?',
    'Code recognition outcomes against compliance scores across a full case set that includes compliant-but-denied polities and non-compliant-but-admitted allies; if outcomes track sponsor interest better than compliance records, the published criteria are performing rather than deciding.',
    'If cover, the authored theater_ratio understates the performative share, the effective victim set widens to include fully compliant polities, and the arrangement drifts toward exclusion maintained by ritual rather than standard.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(criterion_vs_interest_operativity, empirical, 'Whether published criteria or underlying interests drive recognition outcomes.').

omega_variable(
    intervention_cover_causality,
    'Does this reading actually supply legal cover for humanitarian intervention and regime change, or is that cover generated by separate Security Council practice that merely borrows the same legitimacy vocabulary?',
    'Trace citation patterns in the justification documents for Kosovo 1999, Libya 2011, and later interventions: count reliance on statehood-legitimacy reasoning (failed statehood, illegitimate authority) versus Charter-based authorization.',
    'Determines whether the network edge toward the intervention-doctrine constraint carries real contamination or only rhetorical resemblance; if the cover is separately generated, this reading''s downstream responsibility shrinks and the expected structural delta is overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intervention_cover_causality, empirical, 'Whether the legitimacy criteria causally feed intervention and regime-change justifications.').

omega_variable(
    conditionality_paternalism_boundary,
    'Is attaching liberal-legitimacy conditions to membership a defensible entry standard that all applicants face equally, or imperial gatekeeping that reserves full sovereignty for the already-liberal?',
    'Not resolvable by data alone: turns on whether the conditions are applied symmetrically to incumbents as well as aspirants and whether the governed populations assent. Comparative treatment of incumbent violators versus aspiring ones is the relevant evidence, but the verdict depends on the weight given to consent.',
    'If gatekeeping, the burden on aspirants is structural rather than transitional and the arrangement resists reform from within; if a genuine entry standard, part of the measured burden is the price of the coordination itself and the extraction estimate should be discounted accordingly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(conditionality_paternalism_boundary, preference, 'Normative framing of conditionality: equal entry standard versus asymmetric gatekeeping.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__hybrid_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mv_hybrid_tr_t0, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(mv_hybrid_tr_t0, observed).
narrative_ontology:measurement(mv_hybrid_tr_t5, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 5, 0.26).
narrative_ontology:measurement_basis(mv_hybrid_tr_t5, observed).
narrative_ontology:measurement(mv_hybrid_tr_t10, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement_basis(mv_hybrid_tr_t10, observed).
narrative_ontology:measurement(mv_hybrid_tr_t15, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement_basis(mv_hybrid_tr_t15, observed).
narrative_ontology:measurement(mv_hybrid_tr_t20, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(mv_hybrid_tr_t20, observed).
narrative_ontology:measurement(mv_hybrid_tr_t25, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 25, 0.43).
narrative_ontology:measurement_basis(mv_hybrid_tr_t25, observed).
narrative_ontology:measurement(mv_hybrid_tr_t30, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 30, 0.45).
narrative_ontology:measurement_basis(mv_hybrid_tr_t30, observed).
narrative_ontology:measurement(mv_hybrid_tr_t35, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 35, 0.46).
narrative_ontology:measurement_basis(mv_hybrid_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(mv_hybrid_be_t0, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(mv_hybrid_be_t0, observed).
narrative_ontology:measurement(mv_hybrid_be_t5, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 5, 0.44).
narrative_ontology:measurement_basis(mv_hybrid_be_t5, observed).
narrative_ontology:measurement(mv_hybrid_be_t10, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 10, 0.47).
narrative_ontology:measurement_basis(mv_hybrid_be_t10, observed).
narrative_ontology:measurement(mv_hybrid_be_t15, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 15, 0.51).
narrative_ontology:measurement_basis(mv_hybrid_be_t15, observed).
narrative_ontology:measurement(mv_hybrid_be_t20, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 20, 0.54).
narrative_ontology:measurement_basis(mv_hybrid_be_t20, observed).
narrative_ontology:measurement(mv_hybrid_be_t25, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 25, 0.56).
narrative_ontology:measurement_basis(mv_hybrid_be_t25, observed).
narrative_ontology:measurement(mv_hybrid_be_t30, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 30, 0.57).
narrative_ontology:measurement_basis(mv_hybrid_be_t30, observed).
narrative_ontology:measurement(mv_hybrid_be_t35, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 35, 0.58).
narrative_ontology:measurement_basis(mv_hybrid_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(mv_hybrid_su_t0, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(mv_hybrid_su_t0, observed).
narrative_ontology:measurement(mv_hybrid_su_t5, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement_basis(mv_hybrid_su_t5, observed).
narrative_ontology:measurement(mv_hybrid_su_t10, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement_basis(mv_hybrid_su_t10, observed).
narrative_ontology:measurement(mv_hybrid_su_t15, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 15, 0.58).
narrative_ontology:measurement_basis(mv_hybrid_su_t15, observed).
narrative_ontology:measurement(mv_hybrid_su_t20, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement_basis(mv_hybrid_su_t20, observed).
narrative_ontology:measurement(mv_hybrid_su_t25, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 25, 0.61).
narrative_ontology:measurement_basis(mv_hybrid_su_t25, observed).
narrative_ontology:measurement(mv_hybrid_su_t30, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement_basis(mv_hybrid_su_t30, observed).
narrative_ontology:measurement(mv_hybrid_su_t35, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 35, 0.62).
narrative_ontology:measurement_basis(mv_hybrid_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(montevideo_statehood_criteria__hybrid_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, montevideo_statehood_criteria__declaratory_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, montevideo_statehood_criteria__constitutive_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, humanitarian_intervention_doctrine).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the criteria for statehood' decomposes into three structurally distinct constraints sharing one kernel. The declaratory reading (upstream, highest empirical confidence in classical doctrine) treats the four Montevideo criteria as sufficient and has a near-empty victim set; the constitutive reading relocates discretion to the recognition act; the hybrid reading (this file) layers normative legitimacy on top of the objective criteria, creating the liberal-gatekeeping beneficiary structure and the non-liberal-aspirant victim set. The upstream declaratory claim is routinely cited as settled background by hybrid-reading instruments, which is why the family edges run declaratory -> hybrid. The edge to humanitarian_intervention_doctrine encodes the expected structural delta that legitimacy-conditioned statehood supplies cover for intervention against non-compliant incumbents; its causal weight is carried by the intervention_cover_causality omega rather than asserted.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
