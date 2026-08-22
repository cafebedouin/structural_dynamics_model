% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__civic_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_scope__civic_right_reading, []).

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
 *   constraint_id: second_amendment_scope__civic_right_reading
 *   human_readable: Second Amendment as Civic Right Conditioned on Militia Participation
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   This constraint instantiates one reading of the Second Amendment kernel:
 *   the right to firearms is protected conditioned on participation in
 *   state-recognized militia structures. Under this reading, the prefatory
 *   clause ('A well regulated Militia, being necessary to the security of a
 *   free State') is not merely ornamental but operative—it specifies the
 *   condition under which the operative clause ('the right of the people to
 *   keep and bear Arms, shall not be infringed') attaches. Citizens eligible
 *   and willing to participate in militia frameworks have constitutional
 *   warrant for firearm ownership; those outside such structures or unable to
 *   access participation have no such warrant. The constraint is a tangled
 *   rope: it coordinates a state interest (militia preparedness) with
 *   individual liberty (firearm access) while asymmetrically extracting from
 *   those unable to meet the militia participation condition. The
 *   claim/metric independence rule applies here: the reading is authored as
 *   claimed (tangled rope), and the metrics describe genuine coordination
 *   function (militia participation) combined with substantial asymmetric
 *   extraction (gating individuals through a participation condition they may
 *   not be able to satisfy). The metrics are not tuned toward the claim.
 *
 * KEY AGENTS:
 *   - militia_eligible_citizens: Structural beneficiaries; can access the constitutional right conditioned on militia participation (moderate power, mobile exit in principle)
 *   - regulatory_authority: Agenda-setter; courts and legislatures that interpret the Second Amendment and administer militia eligibility criteria (institutional power, arbitrage exit via constitutional amendment)
 *   - non_militia_eligible_populations: Structural victims; excluded from the right's protection by their inability to satisfy the militia participation condition (powerless, trapped exit)
 *   - state_militia_administrators: Agenda-setters and co-beneficiaries; administer the participation gate and channel firearm ownership through militia structures (institutional power)
 *   - constitutional scholars: Observers; produce the scholarly record courts cite to justify or critique the reading (analytical power)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__civic_right_reading, 0.48).
domain_priors:suppression_score(second_amendment_scope__civic_right_reading, 0.52).
domain_priors:theater_ratio(second_amendment_scope__civic_right_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__civic_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_scope__civic_right_reading, "Second Amendment as Civic Right Conditioned on Militia Participation").
narrative_ontology:topic_domain(second_amendment_scope__civic_right_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(second_amendment_scope__civic_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__civic_right_reading, '12a97f88-8ae4-4ef7-a182-eb3bd95a6503').
narrative_ontology:cs_kernel_codification('12a97f88-8ae4-4ef7-a182-eb3bd95a6503', fixed_text).
narrative_ontology:cs_authority_grounding('12a97f88-8ae4-4ef7-a182-eb3bd95a6503', lineage).
narrative_ontology:cs_interpretation_layer_present('12a97f88-8ae4-4ef7-a182-eb3bd95a6503').
narrative_ontology:cs_reading_relation('12a97f88-8ae4-4ef7-a182-eb3bd95a6503', second_amendment_scope__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('12a97f88-8ae4-4ef7-a182-eb3bd95a6503', second_amendment_scope__collective_right_reading, coexists_with).
narrative_ontology:cs_axiom('12a97f88-8ae4-4ef7-a182-eb3bd95a6503', foundational, prefatory_clause_operative).
narrative_ontology:cs_axiom_status(prefatory_clause_operative, holdable).
narrative_ontology:cs_axiom_grounding('12a97f88-8ae4-4ef7-a182-eb3bd95a6503', prefatory_clause_operative, empirically_contingent).
narrative_ontology:cs_axiom('12a97f88-8ae4-4ef7-a182-eb3bd95a6503', foundational, militia_participation_conditions_right).
narrative_ontology:cs_axiom_status(militia_participation_conditions_right, holdable).
narrative_ontology:cs_axiom_grounding('12a97f88-8ae4-4ef7-a182-eb3bd95a6503', militia_participation_conditions_right, deontological).
narrative_ontology:cs_reference_frame('12a97f88-8ae4-4ef7-a182-eb3bd95a6503', well_regulated_militia_necessary_to_free_state).
narrative_ontology:cs_drift_state('12a97f88-8ae4-4ef7-a182-eb3bd95a6503', contemporary_post_heller_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('12a97f88-8ae4-4ef7-a182-eb3bd95a6503', '2026-06-12T14:37:22Z').
narrative_ontology:cs_kernel_id(second_amendment_scope__civic_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__civic_right_reading, militia_eligible_citizens).
narrative_ontology:constraint_victim(second_amendment_scope__civic_right_reading, non_militia_eligible_populations).
narrative_ontology:constraint_victim(second_amendment_scope__civic_right_reading, urban_populations_distant_from_militia_infrastructure).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(second_amendment_scope__civic_right_reading, state_militia_administrators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Citizens who can participate in organized militia structures (typically rural, organized into state militia networks or reserve systems) have textual warrant for firearm ownership under this reading. They benefit from a constitutional right framed through their civic participation capacity. They access firearms through channels organized around militia preparedness narratives.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, militia_eligible_citizens, beneficiary,
    moderate, generational, mobile, national).

% Courts and legislatures administering this reading maintain the authority to define what constitutes militia eligibility, participation, and preparedness. They set the conditions under which the right attaches, administer screening for eligibility, and enforce the boundary between protected and unprotected possessors. Their power is exercised through statutory militia frameworks, licensing regimes, and judicial interpretation.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, regulatory_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Populations unable or unwilling to participate in militia structures (persons with disabilities militia service excludes, conscientious objectors, those without geographic access to militia networks, urban residents for whom militia is remote) are positioned outside the protection of this reading's right. They bear the cost of exclusion from constitutional warranty without having chosen to opt out; the condition of militia participation is not one they can satisfy.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, non_militia_eligible_populations, payer,
    powerless, biographical, trapped, national).

% City-dwellers and residents of densely settled areas where organized militia culture is sparse or absent face geographic and social barriers to militia participation. The right's conditionality becomes a practical exclusion for them—they cannot access the condition precedent to the right. They are excluded from the conversation about militia necessity because militia infrastructure is marginal in their communities.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, urban_populations_distant_from_militia_infrastructure, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_scope__civic_right_reading, urban_populations_distant_from_militia_infrastructure, excluded).

% State National Guard, organized militia boards, and weapons-preparedness bodies administer the eligibility process and gate access to participation-qualified status. They benefit from a constitutional framing that channels firearm ownership through their structures and validates their role as the mediators of the civic-right claim. They set criteria for militia service and participation.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, state_militia_administrators, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_scope__civic_right_reading, state_militia_administrators, beneficiary).

% Constitutional scholars, originalists, and legal historians analyze the historical militia premise, the prefatory clause's operative effect, and the scope of the right as this reading constructs it. They produce the scholarly record courts cite to justify or critique the reading.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, second_amendment_doctrine_scholars, observer,
    analytical, generational, analytical, national).

% Persons formally barred from militia participation by law or regulation—felons, non-citizens, persons with certain diagnoses—are structurally positioned outside the right entirely under this reading. They have no path to the condition precedent.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, excluded_categories, excluded,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_non_agent(second_amendment_scope__civic_right_reading, excluded_categories).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_scope__civic_right_reading, regulatory_authority).
narrative_ontology:fixing_cost_class(second_amendment_scope__civic_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes armed civic participation through institutional militia structures: coordinates training, equipment standards, leadership hierarchy, and accountability for citizens exercising armed capability. Connects individual gun ownership to a state-recognized collective participation framework, reducing the agency of isolated gun possession by embedding it in civic obligation and oversight.
% TRANSFER_FUNCTION: Transfers the interpretive authority to define who counts as militia-eligible from the individual claimant to the regulatory and militia-administrative authority. Transfers the constitutional warrant itself from an individual right claim to a conditional civic duty. Moves legitimacy from 'I own firearms' to 'I own firearms as a militia participant,' gating individual liberty on collective membership.
% ABSENT_VOICES: Populations excluded from militia infrastructure—urban non-participants, conscientious objectors, persons with disabilities, non-citizens—are structurally absent from this reading's protection and from the conversation about militia necessity. They would argue that the right should not be conditioned on participation in state-sanctioned structures they cannot access, and that the prefatory clause does not logically subordinate the operative clause to militia participation. Their absence shapes the reading; its authority depends on their silence.
% DISAPPEARANCE_RATIONALE: If this particular reading of the Second Amendment disappeared and were replaced by an individual-right or collective-right interpretation, the constitutional warrant structure would reorganize: firearms would be protected either as an individual liberty unattached to militia duty, or as a tool exclusively for state militia authority. The balance between individual access and state gatekeeping would shift dramatically. The practical regulatory architecture depends on which reading holds—militia participation as a condition precedent gates access fundamentally differently than an unconditioned individual right or an exclusive state monopoly.
% FOUNDING_PROBLEM: The militia system—citizen-soldiers organized to defend against tyranny and external threat—requires both armed capacity and civic obligation to maintain. Early American constitutional drafters sought to protect the institutional capacity of citizens to organize armed defense (militia) without leaving that capacity to the state's arbitrary decision to disarm the citizenry. The prefatory clause ('well regulated Militia') describes the end; the operative clause ('right...to keep and bear Arms') protects the means. This reading resolves the tension: the right is protected because and insofar as citizens participate in the militia structure.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars citing 18th-century sources and militia records (Heller dissenters, historian amici) argue the founding problem was indeed militia maintenance and that the right is conditioned on participation. Opposing scholars (including Heller majority) and state governments argue the founding problem was individual self-defense and that militia language was aspirational, not conditional. No neutral external party—neither the courts nor the historical record itself—definitively settles whether the founding problem remains live or has been superseded; the scholarly dispute is internal to constitutional interpretation.
narrative_ontology:disappearance_verdict(second_amendment_scope__civic_right_reading, contested).
narrative_ontology:founding_problem_status(second_amendment_scope__civic_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__civic_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_scope__civic_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_scope__civic_right_reading, 0.48, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_scope__civic_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_scope__civic_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_scope__civic_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48 at interval end, starting at 0.38) because the reading genuinely coordinates state militia preparedness with individual liberty—a real coordination function—but substantially conditions individual access on meeting a participation criterion that not all citizens can satisfy. The asymmetry is baked into the reading itself: the benefit of constitutional protection is gated on militia participation, and the payer class (non-militia-eligible populations) bears the cost of exclusion without having rejected the condition—they simply cannot meet it. Suppression is also moderate (0.52) because the constraint is maintained partly through judicial and scholarly interpretation (which has real force but is contestable) and partly through regulatory gatekeeping (militia eligibility criteria). The suppression is not at snare levels because militia participation, while exclusive, is formally open to those who meet criteria and choose to participate. Theater ratio rises over time (0.25 → 0.41) as the scholarly justification for the militia condition becomes more elaborate and jurisprudentially developed while the actual militia participation rate among gun owners diverges from the constitutional narrative—the gap between the theoretical militia-service justification and the practical individual-liberty use grows, suggesting theatrical maintenance of the theoretical warrant. The measurement grid is shared across all three metrics at all six time points.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (non-militia-eligible citizens) and the agenda-setter seats (regulatory authority, militia administrators) should compute fundamentally differently from this reading. From the regulatory seat, the reading is a genuine constitutional harmonization: it protects individual liberty while anchoring it to a civic duty, maintaining both the militia system and individual rights through a single doctrinal structure. From the excluded seat, the same reading operates as a constitutional wall—the right is theoretically available but practically or legally inaccessible. The excluded populations have no path to the benefit and no choice to reject the condition; they simply do not qualify. The engine computes per-seat classification from power, exit, and directional beneficiary/victim data; this divergence should be visible in the seat-level outputs.
 *
 * DIRECTIONALITY LOGIC:
 *   Militia-eligible citizens occupy the beneficiary seat (d near 0.0–0.2): they get constitutional warrant through participation, have relatively mobile exit (can participate or not; can live in jurisdictions with different militia infrastructure), and moderate power. Regulatory authority occupies a mixed seat (d near 0.4–0.5): it benefits from the authority to gate access and administer participation, but it also bears a cost if the reading's legitimacy collapses (it has invested in the interpretive framework and loses interpretive authority if the reading is rejected). Non-militia-eligible populations occupy the target seat (d near 0.85–1.0): they are structurally excluded from the right's protection by a condition they cannot satisfy, have trapped exit (no way to satisfy the condition), and powerless status. Urban populations distant from militia infrastructure occupy a similar target seat with somewhat higher d than formally excluded categories, because they face practical barriers rather than legal bars—they could theoretically migrate or participate, but the geographic/social distance makes the condition precedent inaccessible in practice.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—maintaining militia readiness and citizen armed capacity against potential tyranny—has substantially atrophied as a live driver of public policy. Modern militia participation among gun owners is negligible; firearms ownership has become primarily individual self-defense and recreation, unconnected to organized militia duty. Yet this reading persists partly as scholarly and judicial doctrine, partly as political aspiration, and partly as a narrative frame for defending firearm access. The theater ratio rising from 0.25 to 0.41 reflects this atrophy: increasingly elaborate constitutional and scholarly justification is required to maintain the militia condition even as practical militia participation recedes. This is not a false summit (the reading is not claiming natural-law status); it is a tangled rope whose coordination function (militia participation) has become largely ceremonial while its extraction function (gating individuals through the participation condition) remains structurally active. The constraint shows classic mandatrophy dynamics: the function it was built to serve (militia preparedness) has contracted, but the constraint persists through doctrinal inertia and political commitment to the reading itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prefatory_clause_operativity,
    'Is the prefatory militia clause operative—a condition precedent that limits the scope of the operative right—or merely explanatory—an aspirational statement that does not limit scope?',
    'Historical linguistic analysis of 18th-century grammar and constitutional drafting practice; comparative study of other constitutional prefatory clauses and their judicial treatment; examination of foundational documents and militia records to establish the drafters'' intent.',
    'If operative: this reading holds, and the right is conditioned on militia participation. If merely explanatory: the individual_right_reading holds, and the right is unconnected to militia duty. This is the core structural bifurcation of the kernel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(prefatory_clause_operativity, conceptual, 'Whether the militia clause limits the operative right or merely explains its rationale.').

omega_variable(
    modern_militia_participation_capacity,
    'What fraction of the population can realistically access militia participation as a condition precedent to the right, given contemporary militia infrastructure, geographic distribution, and eligibility criteria?',
    'Empirical survey of state militia rosters and participation barriers; legal analysis of eligibility criteria (age, disability, conscientious objection status, citizenship); geographic mapping of militia infrastructure relative to population distribution.',
    'If the participation condition is accessible to <50% of the population, the reading''s extraction asymmetry is severe and structural—the condition gates individuals who have no realistic path to it. If accessible to >80%, the condition is more symmetric and the reading is closer to genuine coordination than extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(modern_militia_participation_capacity, empirical, 'Practical accessibility of the militia participation condition.').

omega_variable(
    founding_problem_atrophy_status,
    'Has the founding problem—militia readiness as essential to free-state security—substantially changed status from live to dead or attenuated?',
    'Historical analysis of militia participation rates, state militia capacity and funding, public discourse on militia necessity, contemporary security threats and responses, and the de facto role of citizen militia in national defense.',
    'If atrophied: the constraint shows mandatrophy dynamics and should be reclassified as piton (persisting by inertia, defended doctrinally, but functionally obsolete). If live: the constraint remains tangled rope with genuine coordination function. If contested: the parties dispute the founding problem''s status, which is itself a significant feature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_atrophy_status, empirical, 'Whether the militia-readiness problem this reading was built to solve remains substantively live.').

omega_variable(
    suppression_mechanism_identity,
    'Is the measured suppression primarily structural (legal barriers and participation criteria) or internalized (excluded populations have accepted the reading''s legitimacy)?',
    'Post-exclusion trajectory analysis: if excluded populations organize to challenge the reading or migrate to challenge it, the suppression is primarily structural. If they accept the reading as legitimate and do not organize challenge, the suppression is internalized.',
    'If structural: the exclusion is a removable barrier and the constraint''s effective suppression can be reduced by changing criteria. If internalized: the excluded populations carry the suppression with them even after legal exclusion is removed—the constraint has psychological and identity-based persistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_identity, empirical, 'Whether suppression is structural or internalized in excluded populations.').

omega_variable(
    sibling_reading_kernel_contest,
    'Which sibling reading (individual_right_reading, collective_right_reading, or civic_right_reading) best fits the historical evidence and constitutional text?',
    'This is the core contested question of the kernel itself. Different judicial eras have given different answers (Miller, Heller, McDonald, Bruen). The resolution mechanism is the full apparatus of constitutional interpretation: originalist vs. living-constitution methodology, historical linguistics, foundational documents, precedent, and evolving case law.',
    'The answer determines which reading becomes doctrinally dominant, which shapes firearms policy across the nation and determines who has constitutional warrant for firearm ownership. This is the core legal/political contestation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_kernel_contest, conceptual, 'The fundamental disagreement about what the Second Amendment means—a property of the kernel, not just this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__civic_right_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_scope__civic_right_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(seco_tr_t8, second_amendment_scope__civic_right_reading, theater_ratio, 8, 0.29).
narrative_ontology:measurement(seco_tr_t16, second_amendment_scope__civic_right_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(seco_tr_t24, second_amendment_scope__civic_right_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement(seco_tr_t32, second_amendment_scope__civic_right_reading, theater_ratio, 32, 0.41).
narrative_ontology:measurement(seco_tr_t40, second_amendment_scope__civic_right_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_scope__civic_right_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(seco_be_t8, second_amendment_scope__civic_right_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(seco_be_t16, second_amendment_scope__civic_right_reading, base_extractiveness, 16, 0.46).
narrative_ontology:measurement(seco_be_t24, second_amendment_scope__civic_right_reading, base_extractiveness, 24, 0.48).
narrative_ontology:measurement(seco_be_t32, second_amendment_scope__civic_right_reading, base_extractiveness, 32, 0.49).
narrative_ontology:measurement(seco_be_t40, second_amendment_scope__civic_right_reading, base_extractiveness, 40, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_scope__civic_right_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(seco_su_t8, second_amendment_scope__civic_right_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(seco_su_t16, second_amendment_scope__civic_right_reading, suppression_requirement, 16, 0.51).
narrative_ontology:measurement(seco_su_t24, second_amendment_scope__civic_right_reading, suppression_requirement, 24, 0.53).
narrative_ontology:measurement(seco_su_t32, second_amendment_scope__civic_right_reading, suppression_requirement, 32, 0.54).
narrative_ontology:measurement(seco_su_t40, second_amendment_scope__civic_right_reading, suppression_requirement, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_scope__civic_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_scope__civic_right_reading, 0.12).
narrative_ontology:affects_constraint(second_amendment_scope__civic_right_reading, second_amendment_scope__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__civic_right_reading, second_amendment_scope__collective_right_reading).

% DUAL FORMULATION NOTE:
% The second_amendment_scope kernel admits three distinct constraint readings. This file (civic_right_reading) instantiates the reading where the prefatory militia clause is operative and conditions the right on militia participation. The sibling readings (individual_right_reading and collective_right_reading) instantiate alternative interpretations of the same constitutional text, each with different ε values, beneficiary/victim structures, and claim types. All three readings share the kernel (the Second Amendment text) but produce different constraints due to different interpretations. Network edges link them as a family; each reading's constraint story is structurally independent with its own metrics and stakeholder analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
