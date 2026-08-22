% ============================================================================
% CONSTRAINT STORY: eu_council_unanimity__sovereignty_guarantor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_council_unanimity__sovereignty_guarantor_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: eu_council_unanimity__sovereignty_guarantor_reading
 *   human_readable: Council Unanimity as Sovereignty Guarantee (Sovereignty-Guarantor Reading)
 *   domain: institutional/political/international-relations
 *
 * SUMMARY:
 *   In the domains the member states have reserved to themselves — taxation,
 *   common foreign and security policy, treaty revision, own resources — the
 *   Council of the EU acts only when every member state consents. This file
 *   instantiates ONE reading of that arrangement: the
 *   sovereignty_guarantor_reading, under which the unanimity requirement is a
 *   foundational protection against majoritarian coercion and each state's
 *   refusal is a legitimate exercise of the consent right the arrangement
 *   exists to secure. Per the epsilon-referent rule for kernel readings, the
 *   authored extractiveness describes the standing unanimity arrangement AS
 *   THIS READING assesses it: real coordination costs (delay, holdout
 *   accommodation, lowest-common-denominator outcomes) but no systematic
 *   extraction, because blocking is rights-exercise rather than rent-taking.
 *   The sibling readings — veto_trap_reading (blocking as minoritarian
 *   extraction leverage) and diplomatic_capital_reading (unanimity as
 *   consensus-building discipline) — are separate constraint files with their
 *   own epsilon, beneficiary/victim structures, and classifications; they are
 *   linked through network.affects_constraints and are neither described nor
 *   averaged into this one. KEY AGENTS (by structural relationship): -
 *   small_member_states: primary beneficiary (organized/constrained) — each
 *   holds an unconditional refusal right over sovereignty-implicating
 *   measures; the guarantee substitutes for size - mid_sized_member_states:
 *   beneficiary (organized/constrained) — intermittent reliance on the
 *   refusal right while pursuing integration goals - large_member_states:
 *   dual-positioned beneficiary and principal cost-bearer
 *   (powerful/constrained) — shielded like all members but sponsoring most
 *   initiatives and absorbing the largest share of delay and concession costs
 *   - council_presidency_broker: agenda administrator (institutional/mobile)
 *   — chairs and packages negotiations, cannot compel assent -
 *   european_parliament_majorities: excluded voice (institutional/trapped) —
 *   directly elected majoritarian representation barred from these decision
 *   tables - national_constitutional_courts: analytical observer
 *   (institutional/analytical) — ultra vires and identity review policing the
 *   consent boundary
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__sovereignty_guarantor_reading, 0.38).
domain_priors:suppression_score(eu_council_unanimity__sovereignty_guarantor_reading, 0.22).
domain_priors:theater_ratio(eu_council_unanimity__sovereignty_guarantor_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__sovereignty_guarantor_reading, rope).
narrative_ontology:human_readable(eu_council_unanimity__sovereignty_guarantor_reading, "Council Unanimity as Sovereignty Guarantee (Sovereignty-Guarantor Reading)").
narrative_ontology:topic_domain(eu_council_unanimity__sovereignty_guarantor_reading, "institutional/political/international-relations").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__sovereignty_guarantor_reading, '132715cb-f0e2-4f05-864c-ff2d06701b0b').
narrative_ontology:cs_kernel_codification('132715cb-f0e2-4f05-864c-ff2d06701b0b', formalized).
narrative_ontology:cs_authority_grounding('132715cb-f0e2-4f05-864c-ff2d06701b0b', lineage).
narrative_ontology:cs_interpretation_layer_present('132715cb-f0e2-4f05-864c-ff2d06701b0b').
narrative_ontology:cs_reading_relation('132715cb-f0e2-4f05-864c-ff2d06701b0b', eu_council_unanimity__veto_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('132715cb-f0e2-4f05-864c-ff2d06701b0b', eu_council_unanimity__diplomatic_capital_reading, coexists_with).
narrative_ontology:cs_axiom('132715cb-f0e2-4f05-864c-ff2d06701b0b', foundational, no_binding_without_consent).
narrative_ontology:cs_axiom_status(no_binding_without_consent, holdable).
narrative_ontology:cs_axiom_grounding('132715cb-f0e2-4f05-864c-ff2d06701b0b', no_binding_without_consent, deontological).
narrative_ontology:cs_axiom('132715cb-f0e2-4f05-864c-ff2d06701b0b', foundational, veto_exercise_is_legitimate_defense).
narrative_ontology:cs_axiom_status(veto_exercise_is_legitimate_defense, holdable).
narrative_ontology:cs_axiom_grounding('132715cb-f0e2-4f05-864c-ff2d06701b0b', veto_exercise_is_legitimate_defense, deontological).
narrative_ontology:cs_reference_frame('132715cb-f0e2-4f05-864c-ff2d06701b0b', sovereign_consent_founding_settlement).
narrative_ontology:cs_drift_state('132715cb-f0e2-4f05-864c-ff2d06701b0b', contemporary_post_enlargement_qmv_expansion_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('132715cb-f0e2-4f05-864c-ff2d06701b0b', '').
narrative_ontology:cs_kernel_id(eu_council_unanimity__sovereignty_guarantor_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__sovereignty_guarantor_reading, small_member_states).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__sovereignty_guarantor_reading, mid_sized_member_states).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__sovereignty_guarantor_reading, large_member_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(eu_council_unanimity__sovereignty_guarantor_reading, large_member_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Each retains a legally unconditional capacity to refuse any collective measure touching its sovereignty — taxation harmonization, common foreign and security positions, treaty change, own-resources decisions. Its vote weighs the same as the largest member's. Refusal carries diplomatic friction and occasional isolation, but no legal penalty and no loss of the right itself. Leaving the Union to escape obligations it dislikes would cost more than staying and withholding consent, so its practical protection is the refusal right the rule confers.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, small_member_states, beneficiary,
    organized, generational, constrained, continental).

% Holds the same unconditional refusal right but exercises it less often, because its integration ambitions frequently run ahead of its sovereignty anxieties. It trades continuously between supporting majority-based efficiency in newer policy domains and preserving the consent requirement for the domains it regards as core. Its protection functions as intermittent insurance: inexpensive to hold, decisive when invoked.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, mid_sized_member_states, beneficiary,
    organized, generational, constrained, continental).

% Enjoys the same shield — no coalition can bind it without its consent — but sponsors most of the collective initiatives and therefore absorbs the largest share of negotiation delay, package-dealing, and concessions whenever any member withholds consent. It cannot shed the cost side without dissolving the shield, since altering the decision rule itself requires the consent of every government the rule protects.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, large_member_states, beneficiary,
    powerful, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(eu_council_unanimity__sovereignty_guarantor_reading, large_member_states, payer).

% Chairs Council sessions, drafts compromise texts, and packages unrelated files into deals that give every government something to consent to. It cannot compel assent; its entire leverage is agenda control and text craft exercised under the standing possibility that any member refuses. Its tenure rotates, so its stake in any single file is short-lived.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, council_presidency_broker, agenda_setter,
    institutional, biographical, mobile, continental).

% Represents Union citizens by direct election and legislates alongside governments in ordinary policy domains, but in the domains reserved to unanimous government consent it is consulted or informed only. Its majorities cannot decide, and acquiring decision rights would require treaty revision that itself demands the consent of every government currently holding the exclusive seat.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, european_parliament_majorities, excluded,
    institutional, generational, trapped, continental).

% Review whether Union action exceeds the powers their states conferred, and some — most prominently the German Federal Constitutional Court — police an untouchable core of state identity. They neither fund nor receive anything from the consent requirement, but their jurisprudence articulates and enforces the boundary the consent requirement draws.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, national_constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eu_council_unanimity__sovereignty_guarantor_reading, diffuse).
narrative_ontology:fixing_cost_class(eu_council_unanimity__sovereignty_guarantor_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of pooling decision-making among sovereign states: it enables joint action in sensitive domains while guaranteeing that no state is bound by a coalition it did not join, converting potential majoritarian coercion into negotiated universal consent.
% TRANSFER_FUNCTION: Moves decision control and bargaining concessions: policy concessions and negotiation time flow from initiative sponsors (predominantly large member states and the Commission) toward whichever state withholds consent, while autonomy security flows equally to every member state regardless of size.
% ABSENT_VOICES: European Parliament majorities and, behind them, Union citizens: directly elected majoritarian voice is structurally barred from the reserved domains and could gain entry only through treaty revision requiring the very unanimity it objects to. Affected third parties — neighboring states bound by unanimously adopted foreign-policy positions — are likewise outside the chamber.
% DISAPPEARANCE_RATIONALE: If the consent requirement vanished overnight, qualified-majority coalitions would form immediately in taxation and foreign policy, small and mid-sized states would face permanent outvoted-minority status in domains they regard as existential, and several governments would initiate repatriation of powers or exit processes rather than accept binding without consent — the intergovernmental bargain underlying the whole arrangement would unravel.
% FOUNDING_PROBLEM: To give sovereignty-sensitive states a credible commitment that deeper cooperation could never coerce them: after the Empty Chair Crisis and the Luxembourg Compromise, the founding settlement held that collective action implicating state sovereignty requires each state's consent, making deep integration acceptable to governments that feared domination by larger coalitions.
% FOUNDING_PROBLEM_CORROBORATION: National constitutional court jurisprudence (ultra vires and identity review, notably the German Federal Constitutional Court) attests from outside the member-government beneficiary set that the sovereignty-sensitivity problem remains live; independent international-relations scholarship on credible commitment and the historical record of the Empty Chair Crisis corroborate the founding genealogy; eurosceptic opposition parties across member states attest the demand for the guarantee persists.
narrative_ontology:disappearance_verdict(eu_council_unanimity__sovereignty_guarantor_reading, world_rearranges).
narrative_ontology:founding_problem_status(eu_council_unanimity__sovereignty_guarantor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__sovereignty_guarantor_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(eu_council_unanimity__sovereignty_guarantor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eu_council_unanimity__sovereignty_guarantor_reading, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_council_unanimity__sovereignty_guarantor_reading_tests).
:- end_tests(eu_council_unanimity__sovereignty_guarantor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.38): under this reading the arrangement imposes genuine coordination costs — negotiation delay, concessions to holdouts, diluted ambition — but extracts nothing systematically from an identifiable paying class, because the refusal right is distributed equally and exercisable by all. Suppression is low (0.22): the rule is largely self-enforcing (a single refusal suffices by construction; no enforcement machinery is needed to make blocking stick), with modest informal pressure — isolation norms, public naming of obstructive governments — accumulating against veto USE rather than against the rule itself. Suppression is authored as a raw structural property and is left unscaled; only extractiveness is scaled by directionality and scope downstream. Theater ratio is low (0.15): consensus declarations carry some ceremony, but the consent function is performed for real in every file. Accessibility collapse is moderate (0.4): qualified majority voting exists as a worked alternative and has steadily expanded into adjacent domains, so the unanimity arrangement is a maintained choice rather than an inevitability. Resistance is moderate (0.45): the Commission, the Parliament, and several governments campaign periodically to extend qualified majority voting into the remaining reserved domains. The temporal series share one grid (eight points across t=0..68); suppression_requirement is tracked because this story specifically traces the maturation of informal anti-blocking machinery — from the Luxembourg Compromise's explicit veto tolerance, through post-enlargement isolation norms, to Article 7 proceedings and funding-conditionality debates — not because baseline suppression fluctuates; the gentle rise models enforcement capacity growing against veto exercise while the right itself remains formally intact.
 *
 * PERSPECTIVAL GAP:
 *   Seats should classify differently. From the small-state seat the arrangement computes as near-pure coordination: the refusal right is the whole value, exercised rarely, costing little. From the large-state seat the same structure mixes shield and burden — the engine should register a partial target position (hence the directionality override), yet under this reading the burden is coordination cost borne by a co-beneficiary, not extraction from a victim. The excluded parliamentary seat experiences the arrangement as denial of voice rather than as either coordination or extraction. The observer courts experience it as doctrine to be policed. These divergences are computed by the engine from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   All three member-state tiers are declared beneficiaries, driving their derived directionality toward the beneficiary end: the guarantee subsidizes every member's autonomy. Large member states additionally carry a payer secondary role — the automatic derivation from their beneficiary listing alone would undershoot their structural position, since they finance most of the arrangement's coordination cost as initiative sponsors; the override sets the powerful-seat d to 0.42, marking partial target position without asserting victimhood. The presidency broker administers but collects nothing material; the excluded parliamentary seat and the observer courts sit outside the benefit/cost flow. Gain_flow is authored 'diffuse' affirmatively: every named seat was checked and none captures the arrangement's gains — autonomy security accrues to all members roughly in proportion to their sovereignty sensitivity, and no seat converts the arrangement into concentrated private receipts.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — giving sovereignty-sensitive states a credible commitment that pooled action cannot coerce them — remains live: successive enlargements have increased the number of states relying on the guarantee, and the domains reserved to consent remain the ones governments treat as existential. Status live combined with disappearance verdict world_rearranges produces no capture/zombie mismatch. The classification discipline cuts both ways: holding extractiveness at coordination-cost level prevents the veto_trap move of manufacturing victims out of frustrated initiative sponsors, while the tracked rise in anti-blocking enforcement pressure marks the watchpoint — if qualified-majority displacement were to complete in the reserved domains, the surviving consent rituals would persist without their function and the arrangement would decay toward inertial, theatrically maintained form. Nothing in the current record supports declaring the mandate outlived.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_location,
    'This constraint is one reading of the eu_council_unanimity kernel: what would the sibling readings (veto_trap_reading, diplomatic_capital_reading) change structurally, and where exactly is the disagreement located?',
    'Cross-reading corpus comparison: compile all three readings against the same referent arrangement and compare computed per-seat classifications; the disagreement locates in the normative status of blocking (rights-exercise versus extraction-leverage versus legitimacy-building), which determines whether a victim class exists.',
    'Adopting veto_trap_reading would add victims (initiative sponsors, blocked-policy constituencies), raise epsilon substantially, and shift the computed type toward enforced hybrid or pure-extraction forms; adopting diplomatic_capital_reading would lower epsilon toward pure coordination cost and strengthen rope certification. This file''s epsilon of 0.38 is valid only under the sovereignty-guarantor reading''s lights.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Committer structure: one-of-three readings of the unanimity kernel; sibling readings instantiate different constraints with different epsilon and victim sets.').

omega_variable(
    veto_episode_size_distribution,
    'Does the consent guarantee in practice protect small states as a class, or does effective blocking power concentrate in whichever state — regardless of size — faces the highest domestic cost from a proposal?',
    'Distributional analysis of recorded veto and abstention episodes across Council history: frequency, success rate, and concession yield tabulated by state size.',
    'If large states obtain the most concessions through blocking, the small-state-protection beneficiary claim weakens and the arrangement drifts toward asymmetric extraction despite its consent framing; if small states block successfully at rates proportional to their numbers, the guarantor reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_episode_size_distribution, empirical, 'Whether the guarantee''s benefits actually distribute toward small states or toward domestically constrained blockers of any size.').

omega_variable(
    anti_blocking_suppression_source,
    'Is the rising pressure on veto use structural (Article 7 proceedings, funding conditionality, qualified-majority fallback threats) or internalized (norms that render blocking diplomatically unthinkable for mainstream governments)?',
    'Post-norm-removal trajectory: if governments blocked freely once formal pressure mechanisms were withdrawn, the suppression was internalized; if blocking remains rare only while conditionality threats persist, it is structural.',
    'Internalized suppression means the guarantee is decaying faster than formal rules show — the right survives on paper while its exercise atrophies; structural suppression means the guarantee is being actively dismantled and the reading''s reference frame is under open attack.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anti_blocking_suppression_source, empirical, 'Structural versus internalized mechanism behind the growing cost of exercising the refusal right.').

omega_variable(
    defense_extraction_episode_boundary,
    'At what point, if any, does a veto exercised as sovereignty defense become functionally indistinguishable from leverage extraction — a concession demanded as the price of consent that bears no relation to the sovereignty interest invoked?',
    'Episode-level coding: compare the invoked sovereignty interest against the concessions actually obtained; systematic divergence between the two marks ransom dynamics the guarantor reading cannot absorb.',
    'A clean boundary sustains this reading''s authored epsilon; a porous boundary means extraction rides inside rights-exercise and the true epsilon sits above the authored value regardless of which reading is adopted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(defense_extraction_episode_boundary, conceptual, 'Where legitimate sovereignty defense shades into concession-demanding holdout behavior.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__sovereignty_guarantor_reading, 0, 68).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_c_tr_t0, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(eu_c_tr_t10, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement(eu_c_tr_t20, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(eu_c_tr_t30, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 30, 0.11).
narrative_ontology:measurement(eu_c_tr_t40, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement(eu_c_tr_t50, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 50, 0.13).
narrative_ontology:measurement(eu_c_tr_t60, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 60, 0.14).
narrative_ontology:measurement(eu_c_tr_t68, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 68, 0.15).

% Extraction over time
narrative_ontology:measurement(eu_c_be_t0, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(eu_c_be_t10, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 10, 0.21).
narrative_ontology:measurement(eu_c_be_t20, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 20, 0.24).
narrative_ontology:measurement(eu_c_be_t30, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 30, 0.27).
narrative_ontology:measurement(eu_c_be_t40, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 40, 0.3).
narrative_ontology:measurement(eu_c_be_t50, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 50, 0.33).
narrative_ontology:measurement(eu_c_be_t60, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 60, 0.36).
narrative_ontology:measurement(eu_c_be_t68, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 68, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(eu_c_su_t0, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(eu_c_su_t10, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 10, 0.1).
narrative_ontology:measurement(eu_c_su_t20, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 20, 0.12).
narrative_ontology:measurement(eu_c_su_t30, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 30, 0.14).
narrative_ontology:measurement(eu_c_su_t40, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 40, 0.16).
narrative_ontology:measurement(eu_c_su_t50, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 50, 0.18).
narrative_ontology:measurement(eu_c_su_t60, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 60, 0.2).
narrative_ontology:measurement(eu_c_su_t68, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 68, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__sovereignty_guarantor_reading, resource_allocation).
narrative_ontology:affects_constraint(eu_council_unanimity__sovereignty_guarantor_reading, eu_council_unanimity__veto_trap_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__sovereignty_guarantor_reading, eu_council_unanimity__diplomatic_capital_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'EU Council unanimity' covers three structurally distinct claims that decompose per the epsilon-invariance principle. This file (sovereignty_guarantor_reading) authors epsilon for the standing unanimity arrangement as the sovereigntist reading assesses it — moderate coordination cost, no systematic extraction, no victim class. The sibling veto_trap_reading authors epsilon for the SAME referent arrangement as the integrationist critique assesses it — substantially higher, with initiative sponsors and blocked-policy constituencies as victims. The sibling diplomatic_capital_reading authors it as consensus-building discipline with legitimacy dividends. Upstream/downstream: the guarantor reading is the historically prior settlement and is cited as evidence by the diplomatic-capital reading, while the veto-trap reading cites guarantor-reading episodes (individual blocks of aid and sanctions packages) as its evidentiary base. All three files link one another through network.affects_constraints; none averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eu_council_unanimity__sovereignty_guarantor_reading, powerful, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
