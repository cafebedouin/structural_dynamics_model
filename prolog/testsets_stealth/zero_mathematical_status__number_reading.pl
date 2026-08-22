% ============================================================================
% CONSTRAINT STORY: zero_mathematical_status__number_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zero_mathematical_status__number_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: zero_mathematical_status__number_reading
 *   human_readable: Zero as a Number with Defined Arithmetic Operations (Number Reading)
 *   domain: history of mathematics / philosophy of mathematics / conceptual history
 *
 * SUMMARY:
 *   Since Brahmagupta's Brahmasphutasiddhanta (628 CE) codified rules
 *   treating shunya as an operable quantity — a+0=a, a*0=0, with debts and
 *   fortunes governed uniformly — mathematics has run on zero-as-number. This
 *   story authors ONE reading of the contested kernel
 *   zero_mathematical_status: the number_reading. The epsilon referent is
 *   fixed to the standing arrangement under contest —
 *   mathematics-with-zero-as-full-number — assessed by this reading's own
 *   lights, in which the arrangement is near-pure enablement. The sibling
 *   readings (parmenidean_rejection, placeholder_reading) instantiate
 *   different constraints with different epsilon values and are authored as
 *   separate files; nothing about them is averaged into this story.
 *   Beneficiaries are declared deliberately: the beneficiary set is the
 *   universal subsidy set of all practitioners, and the omega
 *   discovered_or_instituted_status documents the
 *   natural-law-versus-constructed ambiguity the schema requires for a
 *   mountain carrying beneficiaries. KEY AGENTS (by structural relationship):
 *   - working_mathematicians: Primary beneficiary (organized/constrained) —
 *   operates wholly inside the arrangement - astronomical_calculators:
 *   Historical beneficiary (organized/constrained) — the arrangement's first
 *   heavy industrial users - mercantile_accountants: Beneficiary with
 *   historical transition costs (moderate/mobile) -
 *   parmenidean_tradition_holders: Excluded dissenter
 *   (moderate/identity_locked) - positional_notation_scribes: Excluded
 *   notational traditionalist (organized/constrained) -
 *   philosophers_of_mathematics: Analytical observer (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zero_mathematical_status__number_reading, 0.03).
domain_priors:suppression_score(zero_mathematical_status__number_reading, 0.02).
domain_priors:theater_ratio(zero_mathematical_status__number_reading, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zero_mathematical_status__number_reading, extractiveness, 0.03).
narrative_ontology:constraint_metric(zero_mathematical_status__number_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(zero_mathematical_status__number_reading, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zero_mathematical_status__number_reading, accessibility_collapse, 0.87).
narrative_ontology:constraint_metric(zero_mathematical_status__number_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zero_mathematical_status__number_reading, mountain).
narrative_ontology:human_readable(zero_mathematical_status__number_reading, "Zero as a Number with Defined Arithmetic Operations (Number Reading)").
narrative_ontology:topic_domain(zero_mathematical_status__number_reading, "history of mathematics / philosophy of mathematics / conceptual history").

domain_priors:emerges_naturally(zero_mathematical_status__number_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zero_mathematical_status__number_reading, '135f8812-5b18-4113-b7cb-c07af0612fa7').
narrative_ontology:cs_kernel_codification('135f8812-5b18-4113-b7cb-c07af0612fa7', formalized).
narrative_ontology:cs_authority_grounding('135f8812-5b18-4113-b7cb-c07af0612fa7', expertise).
narrative_ontology:cs_interpretation_layer_present('135f8812-5b18-4113-b7cb-c07af0612fa7').
narrative_ontology:cs_reading_relation('135f8812-5b18-4113-b7cb-c07af0612fa7', zero_mathematical_status__parmenidean_rejection, forecloses).
narrative_ontology:cs_reading_relation('135f8812-5b18-4113-b7cb-c07af0612fa7', zero_mathematical_status__placeholder_reading, forecloses).
narrative_ontology:cs_axiom('135f8812-5b18-4113-b7cb-c07af0612fa7', foundational, zero_is_operable_quantity).
narrative_ontology:cs_axiom_status(zero_is_operable_quantity, holdable).
narrative_ontology:cs_axiom_grounding('135f8812-5b18-4113-b7cb-c07af0612fa7', zero_is_operable_quantity, instrumental).
narrative_ontology:cs_axiom('135f8812-5b18-4113-b7cb-c07af0612fa7', secondary, uniform_laws_span_zero_and_quantities).
narrative_ontology:cs_axiom_status(uniform_laws_span_zero_and_quantities, holdable).
narrative_ontology:cs_axiom_grounding('135f8812-5b18-4113-b7cb-c07af0612fa7', uniform_laws_span_zero_and_quantities, instrumental).
narrative_ontology:cs_reference_frame('135f8812-5b18-4113-b7cb-c07af0612fa7', full_arithmetic_membership_of_zero).
narrative_ontology:cs_drift_state('135f8812-5b18-4113-b7cb-c07af0612fa7', contemporary_axiomatic_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('135f8812-5b18-4113-b7cb-c07af0612fa7', '').
narrative_ontology:cs_kernel_id(zero_mathematical_status__number_reading, zero_mathematical_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zero_mathematical_status__number_reading, working_mathematicians).
narrative_ontology:constraint_beneficiary(zero_mathematical_status__number_reading, astronomical_calculators).
narrative_ontology:constraint_beneficiary(zero_mathematical_status__number_reading, mercantile_accountants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(zero_mathematical_status__number_reading, mercantile_accountants).
narrative_ontology:constraint_vindicates(zero_mathematical_status__number_reading, brahmagupta_operational_rules).
narrative_ontology:constraint_vindicates(zero_mathematical_status__number_reading, positional_notation_completeness).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Prove theorems and manipulate symbolic expressions in which zero appears constantly — as additive identity, index origin, limit target, coefficient. Every algebraic technique they use presupposes the rules a+0=a and a*0=0. Leaving the arrangement would mean leaving the discipline's shared language; no rival toolkit offers comparable reach.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__number_reading, working_mathematicians, beneficiary,
    organized, civilizational, constrained, global).

% Compute planetary positions and tables in the Indian, Islamic, and later European observatory traditions. Place-value computation with zero made long multiplications and interpolations tractable; earlier geometric and abacus methods were slower and accumulated errors. Their practice depends on the rules holding uniformly.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__number_reading, astronomical_calculators, beneficiary,
    organized, generational, constrained, global).

% Keep ledgers and settle accounts. Gained fast written arithmetic when positional numerals with zero spread along trade routes; bore the transition costs — retraining clerks, anxieties about forged digits, occasional municipal bans on the new numerals. Many kept the abacus alongside for generations before switching.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__number_reading, mercantile_accountants, beneficiary,
    moderate, immediate, mobile, regional).
narrative_ontology:stakeholder_secondary_role(zero_mathematical_status__number_reading, mercantile_accountants, payer).

% Philosophers in the lineage running from Parmenides through neoplatonic and scholastic ontology, for whom non-being cannot be an object of thought or operation. Granting nothing a place inside arithmetic strikes them as a category error rather than a discovery. They stand outside working mathematics today; adopting the rules would dissolve the position that defines them.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__number_reading, parmenidean_tradition_holders, excluded,
    moderate, civilizational, identity_locked, continental).

% Scribal and reckoning traditions that used a blank or dot purely to mark empty places in positional writing — Babylonian heirs, and later European abacists. On their view the marker does its job without needing arithmetic personality. The traditions were gradually absorbed into schools teaching the fuller rules.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__number_reading, positional_notation_scribes, excluded,
    organized, generational, constrained, regional).

% Study what it is for something to count as a number, and whether zero's status was found or instituted. They publish analyses of the historical dispute and its resolution, take no side in operating the rules, and can examine the arrangement from outside any practice that depends on it.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__number_reading, philosophers_of_mathematics, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zero_mathematical_status__number_reading, diffuse).
narrative_ontology:fixing_cost_class(zero_mathematical_status__number_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes written computation across communities and generations: one consistent treatment of the empty place, and of nothing-owed and nothing-owned, lets ledgers, tables, and proofs be read and checked by strangers. The rules make arithmetic results reproducible across hands.
% TRANSFER_FUNCTION: Moves computational capability outward to every practitioner who adopts the rules — merchants, astronomers, mathematicians — while taking nothing measurable from anyone; the flow is one-directional subsidy of technique, with only learning costs at adoption.
% ABSENT_VOICES: Parmenidean-lineage philosophers would object that numberhood for nothing smuggles non-being into arithmetic by fiat; scribal traditionalists would object that the marker never needed arithmetic personality. Both sit outside the rooms where the rules are taught and used — marginalized less by argument than by the arrangement's sheer usefulness.
% DISAPPEARANCE_RATIONALE: Revoking the license to operate on zero would unravel algebra, the calculus, place-value computation, and ultimately digital computing — every quantitative practice would need rebuilding on some non-zero footing, and no candidate footing is known.
% FOUNDING_PROBLEM: Give the empty place of positional notation and the 'nothing' of bookkeeping an operative place inside arithmetic itself, so that computations neither stall nor change character when they reach it.
% FOUNDING_PROBLEM_CORROBORATION: Historians of mathematics attest the original problem's form and its settlement (scholarship on the Brahmasphutasiddhanta and its transmission along trade and observatory networks); philosophers of mathematics attest the residual live questions (semantics of division by zero, zero's role in foundations and category theory). Neither group draws income from the arrangement's operation — corroboration comes from outside the beneficiary set.
narrative_ontology:disappearance_verdict(zero_mathematical_status__number_reading, world_rearranges).
narrative_ontology:founding_problem_status(zero_mathematical_status__number_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zero_mathematical_status__number_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(zero_mathematical_status__number_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zero_mathematical_status__number_reading, 0.03, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zero_mathematical_status__number_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(zero_mathematical_status__number_reading, ExtMetricName, E),
    domain_priors:suppression_score(zero_mathematical_status__number_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(zero_mathematical_status__number_reading),
    narrative_ontology:constraint_metric(zero_mathematical_status__number_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(zero_mathematical_status__number_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(zero_mathematical_status__number_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored at 0.03: the arrangement imposes only learning costs at adoption and care at the division-by-zero edge; it takes nothing ongoing from anyone. Suppression is authored at 0.02 as a raw, unscaled structural property — there is no enforcement machinery at all; adoption runs on conviction and demonstrated usefulness, and the engine scales only extractiveness. Theater_ratio 0.01: no ritual maintenance exists — nobody performs zero's numberhood; it is simply used. Accessibility_collapse 0.87: once the rules are grasped, algebra without them is practically unavailable — every alternative (geometric construction, abacus reckoning, placeholder-only notation) collapses on contact with symbolic manipulation. Resistance 0.05: end-state dissent is confined to philosophical ontology, not operational refusal. Claim and metrics were authored independently: the mountain claim rests on structure (self-sustaining, no administrator seat anywhere in the stakeholder set, no capture, persistence without defense), and the metric values rest on description; they happen to align, but neither was tuned toward the other or toward a predicted engine output. Both temporal series run on one shared grid (628, 850, 1202, 1545, 1687, 1889, 2026) with every tracked metric authored at every point. No suppression_requirement series is authored: the enforcement picture is static (there is no enforcement to intensify or decay), which the scalar already captures.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats and the excluded seats should compute differently from the same structure. From the practitioner seats, the arrangement is experienced as pure enablement — capability arrives, nothing is taken. From the parmenidean seat, the same arrangement is experienced as an imposed ontology: a category error that won by usefulness rather than by argument, and one their own framework cannot enter without dissolving. The scribal seat experiences it as the loss of a sufficient practice to an unnecessarily elaborate one. The observer seat sees a settled status. The engine computes these per-seat classifications from power, exit, and role data; this commentary does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Every seated party clusters near the beneficiary end of the directionality axis: working_mathematicians and astronomical_calculators receive the arrangement's entire product and bear no ongoing charge (d near 0.05-0.10); mercantile_accountants sit slightly higher (d near 0.20) because the secondary payer position records real historical transition costs — retraining, fraud exposure, municipal bans — borne at adoption rather than continuously. The excluded seats derive elevated directionality from their oppositional position but receive no transfer and mount no successful exit, so no chi concentrates on them. No seat sits near the target end; with epsilon near floor and no trapped targets, effective extraction is negligible at every seat, and global spatial scope amplifies from a near-zero base.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against three mislabels. First, rope: calling this a maintained coordination agreement would imply ongoing coordination work and participant negotiation — but no maintenance occurs; there is no agenda_setter seat because nobody administers the arrangement, and that absence is the structural signature behind the mountain claim. Second, FSM overreach: beneficiaries exist, but none captures — gain_flow is affirmatively diffuse, the subsidy spreads across every practitioner, so beneficiary presence here signals universality, not capture. Third, piton: the function is not atrophied — it is maximally load-bearing (algebra, calculus, computing all run through it), theater is near zero, and the founding problem remains live, so mandatrophy_resolved is not declared. The founding problem (giving nothing an operative place inside arithmetic) is still the problem every new practitioner encounters, and the rules are still its solution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discovered_or_instituted_status,
    'Is zero''s numberhood a discovered feature of mathematical reality, or an instituted convention that prevailed historically?',
    'Cross-cultural convergence analysis: independent algebraic traditions (Indian, Islamic, Chinese, modern formal) arriving at identical operational rules would evidence discovery; persistent divergence would evidence institution.',
    'If instituted, the arrangement resembles a maintained coordination standard and the beneficiary declaration reads as a coalition artifact; if discovered, the mountain profile stands and the beneficiaries are subsidy recipients, not capturers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discovered_or_instituted_status, conceptual, 'Natural-law versus constructed status of zero''s numberhood (required ambiguity documentation for a mountain with declared beneficiaries).').

omega_variable(
    kernel_reading_delta,
    'How would this constraint''s structure change under the sibling readings of kernel zero_mathematical_status — parmenidean_rejection (nothing cannot be an arithmetical object) or placeholder_reading (zero is only a notational marker)?',
    'Author the sibling stories as separate constraint files and compare epsilon, beneficiary/victim sets, and computed types across the family.',
    'Under placeholder_reading the arrangement becomes a notational standard with scribal institutions as its constituency; under parmenidean_rejection the arrangement is incoherent and its beneficiary set empties. This story''s epsilon is indexed to the number_reading alone and must not be averaged across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_delta, conceptual, 'Committer structure: this constraint is one reading of a three-reading kernel; sibling deltas routed here per the committer-frame rules.').

omega_variable(
    division_by_zero_residual_cost,
    'Does the rules'' silence at division by zero impose material operating costs on practitioners — a residual cost term inside an otherwise negligible profile?',
    'Measure incidence and severity of division-by-zero failures in computational practice and the pedagogical cost of teaching the boundary case.',
    'Material costs would raise epsilon above the negligible band and soften the profile toward a maintained standard; immaterial costs leave the near-zero profile intact.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(division_by_zero_residual_cost, empirical, 'Residual user cost at the rules'' edge case (Brahmagupta himself assigned 0/0 = 0; modern analysis leaves it undefined).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zero_mathematical_status__number_reading, 628, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zero_number_reading_tr_t628, zero_mathematical_status__number_reading, theater_ratio, 628, 0.04).
narrative_ontology:measurement_basis(zero_number_reading_tr_t628, observed).
narrative_ontology:measurement(zero_number_reading_tr_t850, zero_mathematical_status__number_reading, theater_ratio, 850, 0.03).
narrative_ontology:measurement_basis(zero_number_reading_tr_t850, observed).
narrative_ontology:measurement(zero_number_reading_tr_t1202, zero_mathematical_status__number_reading, theater_ratio, 1202, 0.04).
narrative_ontology:measurement_basis(zero_number_reading_tr_t1202, observed).
narrative_ontology:measurement(zero_number_reading_tr_t1545, zero_mathematical_status__number_reading, theater_ratio, 1545, 0.03).
narrative_ontology:measurement_basis(zero_number_reading_tr_t1545, observed).
narrative_ontology:measurement(zero_number_reading_tr_t1687, zero_mathematical_status__number_reading, theater_ratio, 1687, 0.02).
narrative_ontology:measurement_basis(zero_number_reading_tr_t1687, observed).
narrative_ontology:measurement(zero_number_reading_tr_t1889, zero_mathematical_status__number_reading, theater_ratio, 1889, 0.01).
narrative_ontology:measurement_basis(zero_number_reading_tr_t1889, observed).
narrative_ontology:measurement(zero_number_reading_tr_t2026, zero_mathematical_status__number_reading, theater_ratio, 2026, 0.01).
narrative_ontology:measurement_basis(zero_number_reading_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(zero_number_reading_be_t628, zero_mathematical_status__number_reading, base_extractiveness, 628, 0.09).
narrative_ontology:measurement_basis(zero_number_reading_be_t628, observed).
narrative_ontology:measurement(zero_number_reading_be_t850, zero_mathematical_status__number_reading, base_extractiveness, 850, 0.08).
narrative_ontology:measurement_basis(zero_number_reading_be_t850, observed).
narrative_ontology:measurement(zero_number_reading_be_t1202, zero_mathematical_status__number_reading, base_extractiveness, 1202, 0.07).
narrative_ontology:measurement_basis(zero_number_reading_be_t1202, observed).
narrative_ontology:measurement(zero_number_reading_be_t1545, zero_mathematical_status__number_reading, base_extractiveness, 1545, 0.06).
narrative_ontology:measurement_basis(zero_number_reading_be_t1545, observed).
narrative_ontology:measurement(zero_number_reading_be_t1687, zero_mathematical_status__number_reading, base_extractiveness, 1687, 0.05).
narrative_ontology:measurement_basis(zero_number_reading_be_t1687, observed).
narrative_ontology:measurement(zero_number_reading_be_t1889, zero_mathematical_status__number_reading, base_extractiveness, 1889, 0.04).
narrative_ontology:measurement_basis(zero_number_reading_be_t1889, observed).
narrative_ontology:measurement(zero_number_reading_be_t2026, zero_mathematical_status__number_reading, base_extractiveness, 2026, 0.03).
narrative_ontology:measurement_basis(zero_number_reading_be_t2026, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(zero_mathematical_status__number_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zero_mathematical_status__number_reading, information_standard).
narrative_ontology:affects_constraint(zero_mathematical_status__number_reading, zero_mathematical_status__parmenidean_rejection).
narrative_ontology:affects_constraint(zero_mathematical_status__number_reading, zero_mathematical_status__placeholder_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the colloquial label 'zero': the label conflates three structurally distinct claims — numberhood with defined operations (this story, epsilon near floor), mere notational function (placeholder_reading), and ontological incoherence (parmenidean_rejection). Each is authored as its own constraint with its own epsilon per the epsilon-invariance principle. Edges run from this story to the siblings because the number_reading's success set the environment both siblings now operate in: its victory is what reduced them to historical and philosophical positions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
