% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__collective_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_arms_right__collective_right_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: second_amendment_arms_right__collective_right_reading
 *   human_readable: Second Amendment Collective-Rights Reading (State Militia Authority)
 *   domain: constitutional_law/legal_interpretation
 *
 * SUMMARY:
 *   This story instantiates the collective-right reading of the Second
 *   Amendment as an operative constitutional arrangement: from Miller (1939)
 *   until Heller (2008), the Amendment was read to protect state militia
 *   authority, leaving individual ownership outside organized militia service
 *   subject to plenary regulation with no federal constitutional recourse.
 *   The arrangement coordinated a federalism settlement (states' militia
 *   institutions shielded, legislative police power unencumbered) while the
 *   same structure placed the entire regulatory incidence on private owners,
 *   enforced actively by a judiciary that dismissed competing claims
 *   summarily. KEY AGENTS (by structural relationship): state_governments:
 *   primary beneficiary (institutional/constrained) — holds the shielded
 *   militia authority and the cleared police-power field; federal_courts:
 *   agenda setter (institutional/analytical) — administers and enforces the
 *   interpretive rule; federal_legislature: secondary beneficiary
 *   (institutional/mobile); individual_owners_outside_militia: primary target
 *   (organized/constrained) — bears regulatory incidence without recourse;
 *   organized_militia_members: protected-class beneficiary
 *   (organized/constrained); firearms_regulation_advocates: secondary
 *   beneficiary (organized/mobile); individualist_scholars_litigants:
 *   excluded voice (moderate/mobile); constitutional_analysts: analytical
 *   observer. Constraint-family note: this is one of three linked stories
 *   decomposing the colloquial label 'the Second Amendment right' per the
 *   epsilon-invariance principle — the individual-right reading authors high
 *   epsilon on prohibition measures (they take from an entitled holder), the
 *   civic-republican reading an intermediate profile, and this reading low
 *   epsilon on the same measures (nothing entitled is taken). Same kernel,
 *   different constraints, different victim sets; the stories are linked
 *   through network edges.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__collective_right_reading, 0.26).
domain_priors:suppression_score(second_amendment_arms_right__collective_right_reading, 0.59).
domain_priors:theater_ratio(second_amendment_arms_right__collective_right_reading, 0.47).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, extractiveness, 0.26).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, suppression_requirement, 0.59).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, theater_ratio, 0.47).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__collective_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_arms_right__collective_right_reading, "Second Amendment Collective-Rights Reading (State Militia Authority)").
narrative_ontology:topic_domain(second_amendment_arms_right__collective_right_reading, "constitutional_law/legal_interpretation").

domain_priors:requires_active_enforcement(second_amendment_arms_right__collective_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__collective_right_reading, '4ed2dbdf-e87d-4e0d-9056-09161d83fdd4').
narrative_ontology:cs_kernel_codification('4ed2dbdf-e87d-4e0d-9056-09161d83fdd4', fixed_text).
narrative_ontology:cs_authority_grounding('4ed2dbdf-e87d-4e0d-9056-09161d83fdd4', lineage).
narrative_ontology:cs_interpretation_layer_present('4ed2dbdf-e87d-4e0d-9056-09161d83fdd4').
narrative_ontology:cs_reading_relation('4ed2dbdf-e87d-4e0d-9056-09161d83fdd4', second_amendment_arms_right__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('4ed2dbdf-e87d-4e0d-9056-09161d83fdd4', second_amendment_arms_right__civic_republican_reading, influences).
narrative_ontology:cs_axiom('4ed2dbdf-e87d-4e0d-9056-09161d83fdd4', foundational, militia_authority_is_the_protected_object).
narrative_ontology:cs_axiom_status(militia_authority_is_the_protected_object, holdable).
narrative_ontology:cs_axiom_grounding('4ed2dbdf-e87d-4e0d-9056-09161d83fdd4', militia_authority_is_the_protected_object, conventional).
narrative_ontology:cs_axiom('4ed2dbdf-e87d-4e0d-9056-09161d83fdd4', foundational, no_individual_entitlement_outside_militia_service).
narrative_ontology:cs_axiom_status(no_individual_entitlement_outside_militia_service, holdable).
narrative_ontology:cs_axiom_grounding('4ed2dbdf-e87d-4e0d-9056-09161d83fdd4', no_individual_entitlement_outside_militia_service, conventional).
narrative_ontology:cs_reference_frame('4ed2dbdf-e87d-4e0d-9056-09161d83fdd4', militia_clause_federalism_settlement).
narrative_ontology:cs_drift_state('4ed2dbdf-e87d-4e0d-9056-09161d83fdd4', post_heller_individual_rights_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('4ed2dbdf-e87d-4e0d-9056-09161d83fdd4', '').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__collective_right_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, state_governments).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, federal_legislature).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, organized_militia_members).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, firearms_regulation_advocates).
narrative_ontology:constraint_victim(second_amendment_arms_right__collective_right_reading, individual_owners_outside_militia).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold constitutional responsibility for organizing, arming, and disciplining the organized militia under the militia clauses. Under this reading the Second Amendment shields that authority against federal interference while leaving their general police power over private firearm possession wholly unencumbered by any competing individual constitutional claim. They cannot exit the federal framework, but the arrangement runs in their favor on both margins.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, state_governments, beneficiary,
    institutional, generational, constrained, national).

% Administer the interpretive rule. From Miller (1939) until 2008 every federal appellate court treated Second Amendment claims brought by private litigants as categorically meritless, dismissing them with minimal analysis. The bench accumulates doctrinal authority by settling the arms boundary; exit is not meaningful for the institution that is the arrangement's enforcing organ.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, federal_courts, agenda_setter,
    institutional, generational, analytical, national).

% Retains plenary Article I power to organize and arm the militia and to regulate arms commerce without individual-rights litigation friction. Incidental beneficiary of the same cleared field; could reshape the arrangement by constitutional amendment but never needed to while the reading held.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, federal_legislature, beneficiary,
    institutional, biographical, mobile, national).

% Bear the full incidence of federal, state, and local firearms regulation with no federal constitutional recourse; their claims were dismissed summarily for seven decades. Partial refuge exists in roughly forty state constitutional provisions, unevenly enforced. They cannot exit the jurisdiction; their remedy ran through electoral politics and, ultimately, through persuading the Supreme Court to change the reading.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, individual_owners_outside_militia, payer,
    organized, biographical, constrained, national).

% Serve in the National Guard and similar organized forces whose arms-bearing the reading actually protects. Their position is doubly conditioned: sheltered as militia members, yet subject to military discipline and federal readiness standards that define the protected class. Membership criteria of age, fitness, and selection exclude most of the population that owns arms privately.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, organized_militia_members, beneficiary,
    organized, biographical, constrained, national).

% Public-health organizations, municipal governments, and reform coalitions whose policy program depends on open regulatory space. Under this reading, prohibition and licensing measures face no federal constitutional obstacle, so their efforts route through ordinary legislation. Their position weakens sharply whenever the reading does.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, firearms_regulation_advocates, beneficiary,
    organized, generational, mobile, national).

% Academic lawyers, advocacy litigators, and rights organizations arguing the individual-liberty reading. Before 2008 they were doctrinally voiceless: courts declined to engage their arguments on the merits, treating the collective reading as settled. They published, filed test cases, and built the scholarly record that ultimately displaced the arrangement, working from outside the authoritative conversation.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, individualist_scholars_litigants, excluded,
    moderate, biographical, mobile, national).

% Legal historians and constitutional theorists mapping the reading's twentieth-century origins, its distance from founding-era militia usage, and its dependence on a stretched citation of Miller. They neither collect nor pay; they document the structure and its drift.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, constitutional_analysts, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_arms_right__collective_right_reading, state_governments).
narrative_ontology:fixing_cost_class(second_amendment_arms_right__collective_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Settles the federalism boundary for arms governance: constitutional protection attaches to state organized-militia institutions, and every question about private possession routes through ordinary federal and state political processes rather than individual constitutional litigation.
% TRANSFER_FUNCTION: Moves constitutional shelter toward state militia authority and legislative regulatory discretion, and moves the incidence of firearms regulation onto private owners outside militia service, who receive no federal forum. Interpretive authority concentrates in the federal judiciary administering the collective rule.
% ABSENT_VOICES: Individual-right proponents were doctrinally excluded: for seven decades courts refused merits engagement, so the appearance of interpretive consensus was produced partly by keeping the individualist seat out of the authoritative conversation. The unorganized militia, the body of citizens the founding-era term covered, also had no seat; the reading quietly narrowed militia to the select organized force.
% DISAPPEARANCE_RATIONALE: It did vanish, in 2008, and the world rearranged: federal courts began striking prohibition and licensing measures, the regulatory space the reading had preserved contracted sharply, and states, legislatures, and regulation advocates lost the arrangement's protections simultaneously, confirming its load-bearing role.
% FOUNDING_PROBLEM: Consolidated after Miller (1939) to answer whether the Second Amendment shields private possession from regulation or guarantees state authority over the organized militia, preserving congressional militia and commerce power and state police power against a judicially enforceable individual veto.
% FOUNDING_PROBLEM_CORROBORATION: The Heller majority itself attests the displacement, expressly rejecting the collective reading as operative doctrine; legal-historical scholarship from across the interpretive spectrum documents that the reading's dominance was a twentieth-century judicial construction rather than an original settlement. No institution outside the reading's remaining adherents attests the founding problem as still live.
narrative_ontology:disappearance_verdict(second_amendment_arms_right__collective_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_arms_right__collective_right_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__collective_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_arms_right__collective_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_arms_right__collective_right_reading, 0.26, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__collective_right_reading_tests).
:- end_tests(second_amendment_arms_right__collective_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low and reading-indexed (0.26 at interval end): by this reading's own lights, prohibition measures take nothing from any entitled party, so the incidence falling on private owners is lawful governance, not taking — though the series rises as regulation expanded while recourse stayed closed. Suppression (0.59) is higher than extractiveness because the arrangement's persistence depended on actively closing the individual-right litigation route, not on participant preference; suppression is authored as a raw structural property and is not scaled by power or scope. Theater_ratio (0.47) reflects maintenance by stretched citation: Miller was a narrow holding about a sawed-off shotgun's militia utility, yet later decades sustained the doctrine by citing it for categorical propositions it did not decide. Accessibility_collapse (0.64): the federal constitutional alternative collapsed completely for litigants — every circuit — but roughly forty state constitutional provisions left partial refuge, so alternatives fell far but not totally. Resistance (0.60): a sustained scholarly and political counter-movement built for four decades and ultimately succeeded. The three metric series share one time grid (1939, 1955, 1971, 1987, 2000, 2008) so every metric is authored at every examined point; the small terminal dip in suppression_requirement (0.61 to 0.59) models enforcement strain as circuit unanimity fractured in the early 2000s rather than a cyclical pattern — no oscillation mechanism is claimed here.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat and the beneficiary/agenda seats should compute differently. From the state and legislative seats the arrangement is a federalism settlement they built their institutions around; from the individual-owner seat the same structure operates as regulation without recourse, and the engine should compute a high-extraction classification there from the payer role plus constrained exit. The federal courts sit administratively: they neither pay nor receive in the ordinary sense but accumulate doctrinal authority from administering the rule. The authored claim does not adjudicate this divergence — the structural data drives it.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (state_governments, federal_legislature, organized_militia_members, firearms_regulation_advocates) derive low directionality — the arrangement subsidizes them, so effective extraction damps toward or below zero at their seats. The declared victim (individual_owners_outside_militia) derives high directionality, amplified by constrained exit: they cannot leave the jurisdiction and their federal forum was closed. No directionality overrides are used: the beneficiary/victim declarations plus exit options already produce the correct per-seat relationships, and the override surface keys on power atoms too coarsely to separate the three institutional seats safely. The excluded seat (individualist_scholars_litigants) feeds the consensus-provenance question — the seven-decade unanimity was partly manufactured by excluding that voice — and is commentary-grade, not correction-grade.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preserving the federalism settlement against an individual constitutional veto — died as a live operative question when Heller displaced the reading in 2008, yet the arrangement persists as advocacy, amicus argument, and dissenting scholarship: the classic dead-problem-plus-world-rearranges mismatch that flags zombie persistence. The elevated terminal theater_ratio is the symptom: maintaining a displaced doctrine is mostly performance. Declaring mandatrophy_resolved true keeps the classification from mislabeling the residual advocacy activity as live coordination, while the tangled_rope claim records what the arrangement genuinely was during its operative life: a real coordination function (the federalism settlement) fused with asymmetric incidence (private owners paid through the same structure that shielded the states), held in place by active judicial enforcement for seven decades.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the second_amendment_arms_right kernel (reading: collective_right_reading); which rights-holder identity — state militia institution, private person, or armed citizenry — does the governing framework adopt, and how does that choice restructure victims, beneficiaries, and epsilon?',
    'Framework-adoption events resolve it for operative law (Heller 2008 adopted the individual-right reading); the scholarly and advocacy contest continues otherwise. No empirical test fixes a rights-holder identity — it is set by interpretive commitment.',
    'Under the individual_right_reading sibling, prohibition measures jump from low epsilon to high epsilon and individual owners convert from payers to rights-holders; under the civic_republican_reading an intermediate victim set appears (citizens excluded from the armed body). This story''s epsilon is reading-indexed to the collective reading and must not be averaged across siblings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one of three sibling readings of the Second Amendment kernel, distinguished by rights-holder identity.').

omega_variable(
    protected_class_atrophy,
    'Does the reading''s low epsilon survive the contraction of the organized militia into a select professional force, or does the widening gap between the protected class and the privately armed population render the low epsilon artifactual?',
    'Track organized-militia membership as a share of the adult population alongside the reading''s protective scope across the interval; compare the fraction of arms-relevant activity actually covered by the protected class in 1939 versus 2008.',
    'If the protected class atrophied below functional viability, the reading''s coordination function collapses and its late-interval persistence is inertial rather than coordinative, shifting the computed classification toward the degraded, inertia-maintained category.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protected_class_atrophy, empirical, 'Whether the reading''s protective object evaporated as the militia professionalized into the National Guard.').

omega_variable(
    incidence_valuation_disagreement,
    'Is the regulatory incidence borne by individual owners outside militia service extraction or ordinary lawful governance — a valuation that flips entirely with the antecedent rights-holder commitment?',
    'Not resolvable by data internal to this reading; resolved only by adopting a sibling framework. Recorded here so the corpus carries both valuations over the identical incidence facts.',
    'Keeps story-level epsilon reading-indexed (low) while documenting that the individual-right sibling authors high epsilon over the same facts; cross-story comparison within the family must treat the differing epsilon values as reading-indexed, not as measurement error.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(incidence_valuation_disagreement, conceptual, 'The same incidence is extraction under one sibling reading and governance under another.').

omega_variable(
    miller_doctrinal_stretch,
    'How much of the collective reading''s operative force derived from United States v. Miller''s actual narrow holding versus repeated stretched citation of it?',
    'Close reading of the circuit-court corpus citing Miller against the holding''s actual scope, coding each citation as faithful or extending.',
    'A predominantly extending citation base supports the elevated theater_ratio and indicates the doctrine was maintained performatively in its later decades rather than by its analytic force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(miller_doctrinal_stretch, empirical, 'Faithful-versus-performative maintenance of the anchoring precedent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__collective_right_reading, 1939, 2008).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1939, second_amendment_arms_right__collective_right_reading, theater_ratio, 1939, 0.18).
narrative_ontology:measurement(seco_tr_t1955, second_amendment_arms_right__collective_right_reading, theater_ratio, 1955, 0.24).
narrative_ontology:measurement(seco_tr_t1971, second_amendment_arms_right__collective_right_reading, theater_ratio, 1971, 0.31).
narrative_ontology:measurement(seco_tr_t1987, second_amendment_arms_right__collective_right_reading, theater_ratio, 1987, 0.38).
narrative_ontology:measurement(seco_tr_t2000, second_amendment_arms_right__collective_right_reading, theater_ratio, 2000, 0.44).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_arms_right__collective_right_reading, theater_ratio, 2008, 0.47).

% Extraction over time
narrative_ontology:measurement(seco_be_t1939, second_amendment_arms_right__collective_right_reading, base_extractiveness, 1939, 0.1).
narrative_ontology:measurement(seco_be_t1955, second_amendment_arms_right__collective_right_reading, base_extractiveness, 1955, 0.13).
narrative_ontology:measurement(seco_be_t1971, second_amendment_arms_right__collective_right_reading, base_extractiveness, 1971, 0.16).
narrative_ontology:measurement(seco_be_t1987, second_amendment_arms_right__collective_right_reading, base_extractiveness, 1987, 0.2).
narrative_ontology:measurement(seco_be_t2000, second_amendment_arms_right__collective_right_reading, base_extractiveness, 2000, 0.23).
narrative_ontology:measurement(seco_be_t2008, second_amendment_arms_right__collective_right_reading, base_extractiveness, 2008, 0.26).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1939, second_amendment_arms_right__collective_right_reading, suppression_requirement, 1939, 0.35).
narrative_ontology:measurement(seco_su_t1955, second_amendment_arms_right__collective_right_reading, suppression_requirement, 1955, 0.42).
narrative_ontology:measurement(seco_su_t1971, second_amendment_arms_right__collective_right_reading, suppression_requirement, 1971, 0.5).
narrative_ontology:measurement(seco_su_t1987, second_amendment_arms_right__collective_right_reading, suppression_requirement, 1987, 0.57).
narrative_ontology:measurement(seco_su_t2000, second_amendment_arms_right__collective_right_reading, suppression_requirement, 2000, 0.61).
narrative_ontology:measurement(seco_su_t2008, second_amendment_arms_right__collective_right_reading, suppression_requirement, 2008, 0.59).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__collective_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_arms_right__collective_right_reading, second_amendment_arms_right__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__collective_right_reading, second_amendment_arms_right__civic_republican_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'the Second Amendment right' per the epsilon-invariance principle. The single natural-language concept covers three structurally distinct claims distinguished by rights-holder identity: this collective-right reading (state militia authority protected; low epsilon on prohibition measures; individual owners as incidence-bearers), the individual_right_reading (pre-existing individual liberty; high epsilon on prohibition measures; owners as rights-holders), and the civic_republican_reading (armed citizenship; intermediate profile). Each story carries its own epsilon, beneficiaries, and victims; the upstream individual-right reading now influences the standing legal environment in which this reading survives only as advocacy. Family members link mutually through network edges; orphaning any member would hide the reading-indexed epsilon divergence that is the family's analytical payload.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
