% ============================================================================
% CONSTRAINT STORY: second_amendment_boundary__militia_conditioned_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_boundary__militia_conditioned_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: second_amendment_boundary__militia_conditioned_reading
 *   human_readable: Second Amendment Boundary — Militia-Conditioned Reading (Prefatory-Clause Scope)
 *   domain: constitutional law / political theory / firearms policy
 *
 * SUMMARY:
 *   A constitutional boundary read through its prefatory clause: 'a well
 *   regulated Militia' defines the scope within which 'the right of the
 *   people to keep and bear Arms' operates. On this reading the boundary
 *   protects militia-relevant arms capacity against federal interference and
 *   leaves private possession outside the constitutional shield, so firearms
 *   regulation is a matter for democratic authority subject to means-end
 *   review. The arrangement the story is about is that militia-bounded
 *   boundary as a standing feature of the constitutional order. The
 *   claim/metric gap is deliberate: the constraint is CLAIMED as tangled_rope
 *   (genuine federalism-and-democratic-control coordination with asymmetric
 *   incidence) while the metrics describe its actual operation — extraction
 *   that accumulated as the protected institution transformed, an enforcement
 *   requirement that matured, and a protection component grown largely
 *   performative. The engine measures that divergence; the claim is not tuned
 *   to the metrics. KEY AGENTS (by structural relationship): -
 *   state_legislatures: agenda-setter and primary beneficiary
 *   (institutional/mobile) — hold the regulatory authority the boundary
 *   reserves to democratic control; the gains accrue here -
 *   restricted_gun_owners: primary target/payer (organized/constrained) —
 *   collectors and self-defense claimants in high-regulation jurisdictions;
 *   bear restriction with no constitutional recourse on this reading -
 *   organized_militia_institutions: protected beneficiary
 *   (institutional/trapped) — the clause's named protectee, now federally
 *   funded and deployable - gun_violence_exposed_communities: secondary
 *   beneficiary (moderate/constrained) — benefit from the permissibility of
 *   regulation - federal_government: bounded party (institutional/mobile) —
 *   its authority over militia arms is the constraint's object -
 *   unorganized_militia_members: excluded seat — the clause's named civic
 *   body without a modern forum - firearms_industry: secondary payer
 *   (powerful/arbitrage) — market exposed to democratic restriction -
 *   federal_courts: enforcement seat (institutional/analytical) — administer
 *   the militia-bounded scope case by case - constitutional_scholars:
 *   analytical observer
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__militia_conditioned_reading, 0.72).
domain_priors:suppression_score(second_amendment_boundary__militia_conditioned_reading, 0.64).
domain_priors:theater_ratio(second_amendment_boundary__militia_conditioned_reading, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__militia_conditioned_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_boundary__militia_conditioned_reading, "Second Amendment Boundary — Militia-Conditioned Reading (Prefatory-Clause Scope)").
narrative_ontology:topic_domain(second_amendment_boundary__militia_conditioned_reading, "constitutional law / political theory / firearms policy").

domain_priors:requires_active_enforcement(second_amendment_boundary__militia_conditioned_reading).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, state_legislatures).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, gun_violence_exposed_communities).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, organized_militia_institutions).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, restricted_gun_owners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, federal_government).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, firearms_industry).
narrative_ontology:constraint_vindicates(second_amendment_boundary__militia_conditioned_reading, prefatory_clause_scope_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_boundary__militia_conditioned_reading, civic_militia_federalism).
narrative_ontology:constraint_vindicates(second_amendment_boundary__militia_conditioned_reading, democratic_arms_policy_reservation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enact firearms regulation under their police powers — licensing, category restrictions, storage and carry rules — and organize the militia within their states. The constitutional boundary leaves their regulatory authority intact and shields their militia institutions from federal disarmament. Their policy position is fully mobile: they may regulate heavily, lightly, or not at all, and they answer to statewide electorates on an electoral horizon.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, state_legislatures, agenda_setter,
    institutional, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(second_amendment_boundary__militia_conditioned_reading, state_legislatures, beneficiary).

% Own and carry firearms for collection, sport, and self-defense. In high-regulation jurisdictions they face category bans, may-issue licensing, waiting periods, and storage mandates, and on this reading they hold no constitutional claim to set against those rules. Exit is partial and costly: moving to a lower-regulation state means leaving work, family, and community, and their possession interest is tied to where they live. They litigate, vote, and organize through advocacy organizations.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, restricted_gun_owners, payer,
    organized, biographical, constrained, national).

% The state military institutions the prefatory clause names — today the National Guard and state defense forces. They receive the clause's protection: federal authority to disarm or absorb them is bounded. Their modern form is federally funded and deployable under federal command, so the independence the protection guards has narrowed. The institution persists and cannot exit the constitutional structure that names it.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, organized_militia_institutions, beneficiary,
    institutional, generational, trapped, national).

% Urban and disproportionately affected communities where firearm violence concentrates. They benefit from the boundary leaving regulation available: their exposure is addressable through legislation their representatives can enact. Their exit is constrained — residence is tied to work, family, and housing — so their recourse is the policy process the boundary keeps open.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, gun_violence_exposed_communities, beneficiary,
    moderate, biographical, constrained, regional).

% Holds authority to organize, arm, and discipline the militia and to regulate interstate firearms commerce, but is bounded from disarming or absorbing state militia capacity — the bound is the arrangement's founding design, which the federal structure accepted. It retains commerce-based regulatory instruments and can shift among them; its exit from the bound runs through the amendment process.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, federal_government, payer,
    institutional, generational, mobile, national).

% The adult residents who constitute the citizen body the prefatory clause addresses. The organized Guard supplanted them as the militia's institutional referent; no forum represents them as the militia, and their possession is regulated as private possession. Membership follows from citizenship and residence — there is no exit from the class.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, unorganized_militia_members, excluded,
    powerless, generational, trapped, national).

% Manufactures and sells firearms and accessories. Its civilian market is exposed to democratic restriction — category bans, magazine limits, and licensing regimes shrink the addressable market — and it absorbs those losses through pricing, product redesign, and multi-state market shifts. It also supplies military and law-enforcement demand that regulation does not touch.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, firearms_industry, payer,
    powerful, biographical, arbitrage, national).

% Adjudicate the boundary's scope: which possession claims fall within militia context, which regulations survive review, and where the line between protected militia arms and regulable private possession sits. They administer the scope case by case and maintain its doctrine across generations; their seat is analytical — they collect nothing and pay nothing directly.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, federal_courts, agenda_setter,
    institutional, generational, analytical, national).

% Study the boundary's text, founding genealogy, and doctrinal evolution, publishing analyses that feed judicial opinions, legislative hearings, and public debate. They hold no stake in the arrangement's outcomes and can adopt any interpretive position.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_boundary__militia_conditioned_reading, state_legislatures).
narrative_ontology:fixing_cost_class(second_amendment_boundary__militia_conditioned_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the founding-era collective-action problem over armed force: it reserves to the states a protected militia capacity that Congress cannot neglect or disarm, and it reserves firearms-policy decisions to democratic legislative processes rather than constitutionalizing a private possession entitlement. In modern form it coordinates the federal-state division of armed-force authority and keeps the terms of private possession set by legislation subject to means-end review rather than by a constitutional floor.
% TRANSFER_FUNCTION: Moves regulatory authority over firearms from private possessors — who on this reading hold no constitutional claim to resist — to state legislatures and democratic majorities; and, in restrictive jurisdictions, moves possession itself from a default status to a conditioned, licensed status. The militia-arms guarantee moves in the opposite direction: security for state military capacity against federal absorption.
% ABSENT_VOICES: The unorganized militia — the civic body the prefatory clause names — has no seat: the organized Guard supplanted it as the militia's institutional referent, and no forum represents the people-as-militia whose protection the clause declares. Private possessors speak in electoral and legislative forums but are held constitutionally irrelevant in the judicial forum where the boundary's scope is fixed. Categorically prohibited possessors — the most heavily burdened class — have the least voice of all.
% DISAPPEARANCE_RATIONALE: State militia capacity would lose its federal shield: Congress could neglect, underfund, absorb, or disarm state military institutions without constitutional limit, collapsing the founding federal-state division of armed force toward federal control. Firearms policy would continue — states would still regulate under their police powers — but the militia guarantee, the arrangement's coordination core, would be gone, and the Guard's dual state-federal character would rest on statute alone.
% FOUNDING_PROBLEM: A standing federal army threatened republican liberty, and state defense rested on citizen militias; but the Constitution gave Congress power to organize, arm, and discipline the militia, raising the Anti-Federalist fear that Congress could neglect, underfund, or disarm the state militias and render them useless. The amendment's prefatory clause was built to secure well-regulated state militia capacity against federal neglect or disarmament.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem itself is attested outside any current beneficiary set by the ratification record: state convention proposed amendments (New Hampshire's and Virginia's militia amendments), Anti-Federalist pamphlets, and Madison's Federalist 46 — none of which are seats in the modern arrangement. Its status is contested: the Guard's federal funding and deployability are documented in the Militia Act lineage and the Montgomery Amendment case law from seats outside the dispute; militia-tradition scholars read that record as the problem transformed but live, while federalism scholars read the statutory Guard settlement as resolving it.
narrative_ontology:disappearance_verdict(second_amendment_boundary__militia_conditioned_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_boundary__militia_conditioned_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__militia_conditioned_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_boundary__militia_conditioned_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_boundary__militia_conditioned_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_boundary__militia_conditioned_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_boundary__militia_conditioned_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_boundary__militia_conditioned_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72 at interval end) is authored from the boundary's own operation: it extinguishes the constitutional claims of private possessors, and the burden accumulated as the protected militia institution transformed (1903 federalization) while the permissive scope absorbed ever-broader regulation — the series runs 0.18 to 0.72 on one shared grid. Suppression (0.64) is the boundary's active enforcement requirement: holding the militia-bounded scope requires sustained judicial administration against litigated counter-claims, and that requirement matured over the interval rather than staying static, which is why suppression_requirement is tracked rather than left to the scalar. Theater (0.71) reflects the split function: the militia-protection half has grown largely performative — it shields an institution whose independence from federal control has narrowed — while the permissive scope-definition half remains functional. Accessibility collapse is low (0.3): understanding the boundary does not collapse the alternatives to it; rival constitutional scopes remain fully available and contested. Resistance is high (0.8): the payer class litigates, mobilizes, and contests the scope continuously. Suppression is authored as a raw structural property — it is not scaled by power or scope; only extractiveness is scaled, by directionality and scope, in the engine's computation.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat and the beneficiary/agenda seats should compute differently. From restricted_gun_owners (organized power, constrained exit, national scope) the boundary operates as pure exposure: every regulatory burden lands with no constitutional floor, and their exit — interstate movement — is costly and partial. From state_legislatures (institutional power, mobile exit) the same boundary is policy space: the arrangement hands them authority they experience as democratic legitimacy. organized_militia_institutions occupy a third position: formally the protected beneficiary, they experience a protection that has drifted off its referent — the shield exists but guards a transformed institution. federal_government bears the bound as the design's intended cost, not as predation. The engine computes these per-seat divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   state_legislatures are declared beneficiaries and the agenda-setters: the boundary subsidizes them with regulatory authority, so their d sits near the beneficiary end and effective extraction damps toward subsidy. gun_violence_exposed_communities benefit through regulability — low-to-mid d. organized_militia_institutions are the clause's intended beneficiary, though the federalization of the modern militia strains the subsidy; on the declarations their d stays low. restricted_gun_owners are the declared victims with constrained exit: their d sits near the full-target end, and the national scope of the boundary amplifies their effective extraction because scope compliance is institutionally hard to verify. firearms_industry pays through market exposure but holds arbitrage-grade exit — product redesign and multi-state market shifts — pulling its effective d back from the target end. federal_government bears the bound by design; its payer role without a victim declaration keeps it mid-range. federal_courts administer without collecting or paying — near-symmetric.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — securing state militia capacity against federal neglect or disarmament — is contested: the institution it protected was transformed out of its founding form, but the structural concern (the federal-state division of armed force) persists in altered form. The classification prevents two mislabels. It is not a snare: the coordination function is genuine (a federalism settlement over armed force and a reservation of firearms policy to democratic control), and on this reading's lights the incidence on private possessors is governance rather than predation — there is no cover story; the permissive scope is the arrangement's stated content. It is not a rope: the incidence is asymmetric and accumulated, with a declared victim set bearing costs through the same structure that coordinates the beneficiaries — tangled_rope holds both facts at once. The theater trajectory (0.12 to 0.71) signals that the protection component is piton-ward: if the militia referent dissolves entirely, the constraint splits into a vestigial protection shell and a live permissive scope rule, and the story should decompose accordingly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading — the militia_conditioned_reading — of the second_amendment_boundary kernel; what structural delta would each sibling reading instantiate, and where exactly is the disagreement located?',
    'Author and compare the sibling stories (second_amendment_boundary__individual_right_reading, second_amendment_boundary__insurrectionist_reading): the disagreement is located in the prefatory clause''s interpretive function (limiting scope vs. stating purpose) and in the referent of ''well regulated Militia''.',
    'If the individual-right scope prevails, the seat structure inverts — would-be regulators and restricted possessors exchange positions and ε re-references a shield constraint; if the insurrectionist scope prevails, the beneficiary set shifts toward unorganized possessors as a deterrent check and the enforcement requirement rises.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: this story instantiates one reading of the Second Amendment boundary kernel; sibling readings are separate constraints.').

omega_variable(
    militia_referent_ambiguity,
    'What is the modern referent of ''well regulated Militia'' — the federally organized and funded National Guard, the unorganized citizen body, or a founding institution that no longer exists in its original form?',
    'Institutional-legal analysis of the militia statute lineage (Militia Acts of 1792 and 1903, Guard federalization doctrine) against founding-era usage of ''well regulated''.',
    'Guard-as-referent keeps the protection real but federally absorbed; unorganized-body-as-referent extends the protected class beyond current administration of the scope; defunct-institution-as-referent makes the militia protection purely vestigial and pushes the constraint''s protection component toward piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_referent_ambiguity, empirical, 'Which institution the militia clause protects today.').

omega_variable(
    drift_characterization_ambiguity,
    'Is the constraint''s departure from its founding reference frame best characterized as practice_drift (the militia institution transformed) or codification_collapse (the militia clause no longer performs a protective function)?',
    'Test whether any live institution performs the clause''s protective function: if state-level Guard capacity independently constrains federal action, practice_drift holds; if no institution performs it, codification_collapse holds.',
    'codification_collapse would weight the constraint toward piton (text functioning theatrically over an atrophied function); practice_drift holds tangled_rope with an atrophied protection component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drift_characterization_ambiguity, conceptual, 'Two coherent framings of the same drift produce different classifications; CS framing under-determination routed to omega rather than resolved silently.').

omega_variable(
    boundary_vs_downstream_regulation_epsilon,
    'How much of the measured extraction belongs to the constitutional boundary itself (extinguishing private-possession claims) versus the downstream regulatory regimes it permits?',
    'Decompose per the ε-invariance principle: author separate stories for the major regulatory regimes (licensing, category bans, storage mandates); attribute to the boundary the residual extraction that persists when the downstream regimes are held fixed.',
    'If the downstream regimes carry most of the extraction, the boundary''s ε falls toward coordination-cost levels and the type moves toward rope; if scope-extinction itself is load-bearing, the tangled_rope claim holds with high ε.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_vs_downstream_regulation_epsilon, conceptual, 'ε-invariance decomposition between the constitutional boundary and the regulations it authorizes.').

omega_variable(
    victim_set_boundary_ambiguity,
    'Does the victim set include only law-abiding possessors restricted by regulation (collectors, self-defense claimants), or also categorically prohibited possessors whose possession is wholly barred?',
    'Compare incidence data: restriction burdens on licensed possessors versus prohibition enforcement on prohibited classes; the declared structural delta names the former as the victim set.',
    'Including prohibited possessors raises measured ε and suppression (total bars versus conditioned possession) and changes the payer seat''s coalition potential; excluding them keeps the victim set to the declared collectors and self-defense claimants.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_boundary_ambiguity, conceptual, 'Where the victim set''s boundary sits.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__militia_conditioned_reading, 1791, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1791, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 1791, 0.12).
narrative_ontology:measurement_basis(seco_tr_t1791, observed).
narrative_ontology:measurement(seco_tr_t1830, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 1830, 0.22).
narrative_ontology:measurement_basis(seco_tr_t1830, observed).
narrative_ontology:measurement(seco_tr_t1865, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 1865, 0.38).
narrative_ontology:measurement_basis(seco_tr_t1865, observed).
narrative_ontology:measurement(seco_tr_t1903, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 1903, 0.55).
narrative_ontology:measurement_basis(seco_tr_t1903, observed).
narrative_ontology:measurement(seco_tr_t1934, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 1934, 0.6).
narrative_ontology:measurement_basis(seco_tr_t1934, observed).
narrative_ontology:measurement(seco_tr_t1968, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 1968, 0.64).
narrative_ontology:measurement_basis(seco_tr_t1968, observed).
narrative_ontology:measurement(seco_tr_t1994, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 1994, 0.68).
narrative_ontology:measurement_basis(seco_tr_t1994, observed).
narrative_ontology:measurement(seco_tr_t2026, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 2026, 0.71).
narrative_ontology:measurement_basis(seco_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 1791, 0.18).
narrative_ontology:measurement_basis(seco_be_t1791, observed).
narrative_ontology:measurement(seco_be_t1830, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 1830, 0.28).
narrative_ontology:measurement_basis(seco_be_t1830, observed).
narrative_ontology:measurement(seco_be_t1865, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 1865, 0.35).
narrative_ontology:measurement_basis(seco_be_t1865, observed).
narrative_ontology:measurement(seco_be_t1903, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 1903, 0.44).
narrative_ontology:measurement_basis(seco_be_t1903, observed).
narrative_ontology:measurement(seco_be_t1934, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 1934, 0.52).
narrative_ontology:measurement_basis(seco_be_t1934, observed).
narrative_ontology:measurement(seco_be_t1968, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 1968, 0.6).
narrative_ontology:measurement_basis(seco_be_t1968, observed).
narrative_ontology:measurement(seco_be_t1994, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 1994, 0.66).
narrative_ontology:measurement_basis(seco_be_t1994, observed).
narrative_ontology:measurement(seco_be_t2026, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 2026, 0.72).
narrative_ontology:measurement_basis(seco_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1791, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 1791, 0.15).
narrative_ontology:measurement_basis(seco_su_t1791, observed).
narrative_ontology:measurement(seco_su_t1830, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 1830, 0.2).
narrative_ontology:measurement_basis(seco_su_t1830, observed).
narrative_ontology:measurement(seco_su_t1865, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 1865, 0.28).
narrative_ontology:measurement_basis(seco_su_t1865, observed).
narrative_ontology:measurement(seco_su_t1903, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 1903, 0.34).
narrative_ontology:measurement_basis(seco_su_t1903, observed).
narrative_ontology:measurement(seco_su_t1934, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 1934, 0.42).
narrative_ontology:measurement_basis(seco_su_t1934, observed).
narrative_ontology:measurement(seco_su_t1968, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 1968, 0.5).
narrative_ontology:measurement_basis(seco_su_t1968, observed).
narrative_ontology:measurement(seco_su_t1994, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 1994, 0.58).
narrative_ontology:measurement_basis(seco_su_t1994, observed).
narrative_ontology:measurement(seco_su_t2026, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 2026, 0.64).
narrative_ontology:measurement_basis(seco_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__militia_conditioned_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_boundary__militia_conditioned_reading, second_amendment_boundary__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__militia_conditioned_reading, second_amendment_boundary__insurrectionist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the Second Amendment's two-clause text is one kernel; each reading instantiates a structurally distinct constraint with its own ε, beneficiary/victim set, and type. This story (militia_conditioned_reading) instantiates the permissive-scope constraint: beneficiaries are state legislative authority, the protected militia institution, and violence-exposed communities; victims are restricted private possessors. The sibling stories instantiate a shield constraint (individual_right_reading: possessors as beneficiaries, regulators as the bounded party) and a deterrent constraint (insurrectionist_reading). The ε values differ because the readings allocate the constitutional shield to different seats; the family is linked through affects_constraints so drift and contamination propagate across the readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
