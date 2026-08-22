% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__collective_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_scope__collective_right_reading, []).

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
 *   constraint_id: second_amendment_scope__collective_right_reading
 *   human_readable: Second Amendment — Collective Right Reading (State Militia Shield)
 *   domain: constitutional_law/political_theory/rights_jurisprudence
 *
 * SUMMARY:
 *   The collective right reading holds that the Second Amendment's prefatory
 *   militia clause limits the operative clause: the amendment secures state
 *   authority to maintain militias and confers no personal entitlement to own
 *   firearms. This was the dominant judicial reading from Cruikshank (1876)
 *   through Miller (1939) to Quilici (1982), until Heller (2008) repudiated
 *   it. The story treats the reading-as-operated as the constraint: a
 *   federalism shield around state military institutions, with regulatory
 *   authority over private arms left wholly to legislatures. The claim/metric
 *   posture is deliberate and unreconciled: claimed_type is rope (the
 *   structure is a coordination mechanism allocating military capacity
 *   between center and states), while the metrics document late-interval
 *   drift — theater rising toward 0.60 as the shielded function atrophied
 *   after the 1903 federalization of the Guard, and suppression rising toward
 *   0.62 as holding the reading against mounting resistance required ever
 *   more doctrinal force. Epsilon is authored low (0.26) because the referent
 *   is the standing arrangement assessed by this reading's own lights: from
 *   this seat the arrangement shields states and obstructs no regulation. The
 *   individual-right sibling story will author high epsilon for the same
 *   arrangement; that divergence is the kernel's designed signal, not an
 *   inconsistency.
 *
 * KEY AGENTS:
 *   - state_governments: primary beneficiary (institutional/constrained) — holds the militia shield and unrestricted regulatory power
 *   - organized_state_militias: secondary beneficiary (organized/trapped) — the shielded institution itself, unable to exit
 *   - federal_judiciary: agenda_setter (institutional/analytical) — administered the reading for a century, ended it in 2008
 *   - federal_legislature: agenda_setter + payer (institutional/constrained) — wrote the text, checked only in the militia domain
 *   - individual_firearm_owners: payer (moderate/constrained) — bears regulation and prosecution with no constitutional defense
 *   - compulsory_militia_members: historical payer (powerless/trapped) — supplied the mandated labor and personal expense the guaranteed institution ran on
 *   - gun_policy_advocacy_organizations: beneficiary (organized/mobile) — the narrow scope removes their chief legal obstacle
 *   - freedmen_communities_postbellum: excluded (powerless/trapped) — denied any seat or protection in the doctrine's construction
 *   - constitutional_scholars: observer (analytical/analytical) — produced and then dismantled the reading's intellectual warrant
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__collective_right_reading, 0.26).
domain_priors:suppression_score(second_amendment_scope__collective_right_reading, 0.62).
domain_priors:theater_ratio(second_amendment_scope__collective_right_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, extractiveness, 0.26).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__collective_right_reading, rope).
narrative_ontology:human_readable(second_amendment_scope__collective_right_reading, "Second Amendment — Collective Right Reading (State Militia Shield)").
narrative_ontology:topic_domain(second_amendment_scope__collective_right_reading, "constitutional_law/political_theory/rights_jurisprudence").

domain_priors:requires_active_enforcement(second_amendment_scope__collective_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__collective_right_reading, '92066df0-1cb3-435e-993e-e5bcdcb8689c').
narrative_ontology:cs_kernel_codification('92066df0-1cb3-435e-993e-e5bcdcb8689c', fixed_text).
narrative_ontology:cs_authority_grounding('92066df0-1cb3-435e-993e-e5bcdcb8689c', lineage).
narrative_ontology:cs_interpretation_layer_present('92066df0-1cb3-435e-993e-e5bcdcb8689c').
narrative_ontology:cs_reading_relation('92066df0-1cb3-435e-993e-e5bcdcb8689c', second_amendment_scope__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('92066df0-1cb3-435e-993e-e5bcdcb8689c', second_amendment_scope__civic_right_reading, forecloses).
narrative_ontology:cs_axiom('92066df0-1cb3-435e-993e-e5bcdcb8689c', foundational, no_personal_arms_entitlement).
narrative_ontology:cs_axiom_status(no_personal_arms_entitlement, overridden).
narrative_ontology:cs_axiom_grounding('92066df0-1cb3-435e-993e-e5bcdcb8689c', no_personal_arms_entitlement, conventional).
narrative_ontology:cs_axiom('92066df0-1cb3-435e-993e-e5bcdcb8689c', secondary, militia_shield_against_federal_disarmament).
narrative_ontology:cs_axiom_status(militia_shield_against_federal_disarmament, holdable).
narrative_ontology:cs_axiom_grounding('92066df0-1cb3-435e-993e-e5bcdcb8689c', militia_shield_against_federal_disarmament, conventional).
narrative_ontology:cs_reference_frame('92066df0-1cb3-435e-993e-e5bcdcb8689c', prefatory_clause_scope_limitation).
narrative_ontology:cs_drift_state('92066df0-1cb3-435e-993e-e5bcdcb8689c', post_heller_doctrinal_repudiation, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('92066df0-1cb3-435e-993e-e5bcdcb8689c', '').
narrative_ontology:cs_kernel_id(second_amendment_scope__collective_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__collective_right_reading, state_governments).
narrative_ontology:constraint_beneficiary(second_amendment_scope__collective_right_reading, organized_state_militias).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(second_amendment_scope__collective_right_reading, gun_policy_advocacy_organizations).
narrative_ontology:constraint_victim(second_amendment_scope__collective_right_reading, federal_legislature).
narrative_ontology:constraint_victim(second_amendment_scope__collective_right_reading, individual_firearm_owners).
narrative_ontology:constraint_victim(second_amendment_scope__collective_right_reading, compulsory_militia_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sovereign polities that organized, armed, and officered citizen militias from 1791 onward, relying on the amendment as a guarantee that Congress cannot disarm or abolish their military institutions. Each also holds plenary police power over private firearms within its borders — under this reading that power is untouched by the amendment. After 1903 their militias were folded into the federal National Guard, leaving the guarantee wrapped around an institution they no longer independently control.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, state_governments, beneficiary,
    institutional, generational, constrained, national).

% Guard units and their predecessor bodies: they receive arms, funding, and officers through cooperative state-federal administration at the intersection of Title 10 and Title 32. The amendment's guarantee is addressed to their continued existence; they cannot exit the arrangement because they are its creatures, constituted by the very statutes the guarantee overlays.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, organized_state_militias, beneficiary,
    organized, generational, trapped, national).

% Interprets and enforces the amendment's scope. Across Cruikshank (1876), Miller (1939), and Quilici (1982) it administered this reading: declining individual-rights claims, sustaining convictions and municipal bans, and treating the prefatory clause as controlling. Its docket is where the reading lived; Heller (2008) ended its administration of this reading.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Wrote the amendment in 1791 and retains Article V power to rewrite it, though that path requires two-thirds of both houses plus thirty-eight states. In the narrow domain of disarming state militias it is legally checked; everywhere else in firearms policy — commerce, taxation, manufacture — this reading leaves it unobstructed.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, federal_legislature, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_scope__collective_right_reading, federal_legislature, payer).

% Tens of millions of households keeping firearms for defense, sport, and work. Under this reading they hold no constitutional card: regulation, licensing, prohibition, and prosecution proceed without a Second Amendment defense, and their recourse runs through legislatures and elections rather than courts. Exit means changing jurisdictions or surrendering the goods; neither is realistically available to most.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, individual_firearm_owners, payer,
    moderate, biographical, constrained, national).

% Historical seat: the able-bodied men enrolled by state militia statutes from 1791 into the early twentieth century, obligated to muster, arm themselves at personal expense, and serve under state officers, with fines for absence. They supplied the labor the guaranteed institution ran on; the obligation lapsed as enrollment systems decayed and the Guard went volunteer.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, compulsory_militia_members, payer,
    powerless, biographical, trapped, national).

% Public-health and gun-control organizations pressing for registration, licensing, and bans. Under this reading the amendment poses no obstacle to any of it — their model legislation faces only ordinary political channels — so the reading's narrow scope is a standing benefit to their agenda.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, gun_policy_advocacy_organizations, beneficiary,
    organized, biographical, mobile, national).

% Historical seat: newly emancipated Black communities in the Reconstruction South who sought arms for self-defense against paramilitary terror and were systematically disarmed by state governments and white militias. The reading gave them nothing to invoke — no personal right, and hostile state authorities — and Cruikshank (1876) confirmed the amendment imposed no duty on states to secure their arms-bearing. They had no seat in the doctrine's construction.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, freedmen_communities_postbellum, excluded,
    powerless, generational, trapped, regional).

% Academic lawyers and historians producing the interpretive scholarship courts cite. This reading dominated the mid-twentieth-century literature; a revisionist wave from the 1980s, textualist and historiographic, eroded it, and by 2008 the profession had largely shifted. They observe and argue; they neither collect nor pay.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_scope__collective_right_reading, state_governments).
narrative_ontology:fixing_cost_class(second_amendment_scope__collective_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves an eighteenth-century problem of military federalism: guaranteeing that states could organize, arm, and sustain citizen militias so that organized force would not concentrate exclusively in a federal standing army. It coordinates by allocating a shield — Congress may not disarm the state military institution — while leaving everything else about arms to ordinary legislation.
% TRANSFER_FUNCTION: Moves constitutional protection away from private persons and toward state institutions: security in arms is located in the militia, not the household. Correspondingly it moves regulatory authority over private firearms wholly to legislatures, and — across its operational centuries — moved mandatory musters, personal arms expenses, and service risk onto enrolled militiamen.
% ABSENT_VOICES: Individual claimants had no seat: the reading assigned them no protected interest, so gun owners challenging bans — and, catastrophically, Reconstruction-era freedmen facing disarmament and paramilitary violence — entered the doctrine only as losing arguments. Their objections survive in dissents, failed petitions, and the later revisionist scholarship, nowhere in the doctrine's operative consensus.
% DISAPPEARANCE_RATIONALE: State and militia seats attest dependency: the guarantee is the textual anchor of federalism in military affairs, and removing it overnight would expose state military institutions to outright federal abolition. Individual-rights seats and, after 1903, most military historians attest that nothing practical depends on it — the Guard's integration into federal command already removed the independent capacity the guarantee protected, and the reading's residual daily work (defeating individual claims) is exactly what its opponents want ended. Whether the world rearranges depends on which seat's dependencies count, and the seats disagree.
% FOUNDING_PROBLEM: The ratification-era fear that a federal standing army would extinguish state autonomy: Anti-Federalists demanded a written guarantee that the states could maintain armed citizen militias as a counterweight to congressional control of the army.
% FOUNDING_PROBLEM_CORROBORATION: Military-history scholarship on the Militia Act of 1903 and successor acts attests, from outside the beneficiary set, that state militias ceased to be independent forces — dual federal command, federal equipment, federal deployment. Federal courts' own characterizations of the Guard as an integral federal reserve component corroborate the finding, as does revealed behavior: no state has acted to rebuild an independent military capability, and state governments invoke the guarantee rhetorically while accepting federal integration.
narrative_ontology:disappearance_verdict(second_amendment_scope__collective_right_reading, contested).
narrative_ontology:founding_problem_status(second_amendment_scope__collective_right_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__collective_right_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_scope__collective_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_scope__collective_right_reading, 0.26, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_scope__collective_right_reading_tests).
:- end_tests(second_amendment_scope__collective_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low and rising gently (0.10 to 0.26): under this reading's own lights the arrangement extracts little — it shields state institutions and leaves regulation unobstructed — but the series accumulates as the reading's operational work shifts from protecting militias to defeating individual claims. Suppression (0.62 at end) is a raw structural property, unscaled by power or scope: it measures the enforcement machinery — the doctrine of claim-dismissal built from Miller through Quilici — needed to hold the reading in place as resistance mounted; it is deliberately distinct from extractiveness, and the divergence between a low epsilon and a high suppression requirement is itself diagnostic. Theater rises 0.05 to 0.60: after 1903 the shielded function (independent state military capacity) was gone, and invocation of the reading became increasingly ritual — performed to dismiss claims rather than to protect anything that existed. Accessibility_collapse is moderate (0.40): alternative sources of militia protection (Article I militia powers, state constitutional provisions, statutory frameworks) persisted alongside the amendment, so understanding the constraint did not foreclose all alternatives. Resistance is high (0.72): sustained scholarly revisionism from the 1980s, five federal circuits abandoning the reading in the 1990s–2000s, and final judicial repudiation. All three tracked metrics run on one shared time grid (1791, 1876, 1939, 1982, 1999, 2008) so every metric is authored at every examined point; trajectories are monotonic, not cyclical, so no cycle-lengthening was applied.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structural data. State and militia seats experience a shield: low effective extraction, a genuine coordination benefit. Individual firearm owner seats experience a locked door: they bear the full weight of the regulatory environment with no constitutional recourse, so their per-seat extraction runs high despite the story-level low epsilon — their constrained exit and payer position place them near the target end of directionality. The judiciary seat experiences settled law to administer, neither collecting nor bearing. The sharpest divergence is cross-story rather than cross-seat: the individual-right sibling reading of the same text authors high epsilon for the identical arrangement. Both stories are correct from their seats; the engine computes each, and the gap between them is the measurement the kernel exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries drive the derivation: state_governments and organized_state_militias sit near the beneficiary end (low d, damped or inverted effective extraction) — the arrangement subsidizes them. Payers sit near the target end: individual_firearm_owners (constrained exit amplifies d toward full target) and compulsory_militia_members (trapped, historically the arrangement's direct labor source). Agenda-setters (federal_judiciary, federal_legislature) derive mid-range positions — the legislature's dual role (sets the text via Article V, bears the militia-domain check) is captured by its secondary_role. The victims array is intentionally EMPTY under this reading's own lights: the reading concedes no personal right, so it recognizes no one whose right it takes — individual cost-bearing is registered through payer roles and exits rather than a victims declaration, and the engine derives directionality from those roles. The excluded seat (freedmen_communities_postbellum) is commentary-grade per R3: it documents who was never in the room and drives no classification override. Gain_flow names state_governments because the shield's value demonstrably accrues there; fixing_cost is prohibitive because the only removal path is Article V amendment.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — state military capacity as counterweight to a federal standing army — died with the 1903 federalization of the militia, yet the arrangement persisted 105 more years, its theater ratio climbing from 0.05 to 0.60 while its protective function emptied out. The classification apparatus prevents two opposite mislabelings. Calling the constraint pure rope across the whole interval hides the late-period reality: a functionless shield maintained ritually, whose daily work was defeating individual claims — real costs borne by real litigant seats. Calling it a snare overstates: there is no concentrated capturer running the extraction — gains accrue to states as a class for a federalism function, extraction itself stayed low, and the reading's own lights sincerely register the arrangement as benign. The rope claim plus the drift series locates the transition honestly: a genuine coordination mechanism that atrophied into theatrical maintenance, resolved as mandatrophy (founding_problem_status dead, corroborated by military-history scholarship and the courts' own Guard characterizations from outside the beneficiary set). The status-dead x verdict-contested combination routes correctly: the parties genuinely dispute whether anything depends on the arrangement, so no zombie flag is asserted, but the theater trajectory flags the drift for review.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of kernel second_amendment_scope (reading: collective_right_reading). What would the sibling readings change structurally, and where exactly is the disagreement located?',
    'Comparative authoring of the sibling stories: individual_right_reading moves individual_firearm_owners into the beneficiary set, raises epsilon sharply, and narrows regulatory authority; civic_right_reading conditions a personal right on militia participation, yielding an intermediate beneficiary set. The disagreement is located in the normative force of the prefatory clause (''A well regulated Militia, being necessary to the security of a free State'') — scope-limiting versus purpose-stating.',
    'Cross-reading epsilon divergence over the identical arrangement is the designed signal: this story authors low epsilon; the individual-right sibling authors high epsilon for the same text. This story''s classification must not hedge across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame position of this story within the second_amendment_scope kernel.').

omega_variable(
    founding_problem_death_date,
    'When did independent state military capacity effectively die — the Militia Act of 1903, the 1916 National Defense Act, or post-1945 total integration?',
    'Military-historical record of the Guard federalization stages: command structure, funding dependence, and deployment authority at each stage.',
    'An earlier death date strengthens the mandatrophy account (longer period of functionless persistence); demonstrable residual state capacity would keep the shield partially functional and support the rope claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_death_date, empirical, 'Dating the obsolescence of the arrangement''s founding problem.').

omega_variable(
    shield_vs_claim_blocker_share,
    'What share of the reading''s 1939–2008 operation was protective (shielding militia arrangements from federal action) versus claim-blocking (defeating individual rights claims)?',
    'Docket analysis of Second Amendment invocations in federal courts 1939–2008, coding each invocation by whether a militia arrangement was defended or an individual claim was rejected.',
    'A high protective share supports the rope classification; a high claim-blocking share supports drift toward theatrical maintenance with real costs imposed on litigant seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(shield_vs_claim_blocker_share, empirical, 'Composition of the reading''s twentieth-century operational work.').

omega_variable(
    post_heller_persistence_mode,
    'After Heller (2008) formally repudiated the reading, does the constraint persist as a live scholarly position, an inertial academic habit, or is it extinct as an operating constraint?',
    'Track rates of scholarly defense of the collective reading and any state or lower-court adoption after 2008.',
    'Determines whether the story''s terminal state is a repudiated rope or a surviving piton maintained by professional inertia.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_heller_persistence_mode, conceptual, 'Post-repudiation survival mode of the reading.').

omega_variable(
    freedmen_protection_attribution,
    'Does the postbellum failure to protect freedmen arms-bearing count as an operation OF this constraint (the reading denying any federal individual right), or as state-action failure orthogonal to the reading?',
    'Historical analysis of Cruikshank-era litigation and counterfactual assessment of whether an individual-right reading would have altered outcomes given Reconstruction enforcement capacity.',
    'Attributing these costs to the constraint raises effective extraction and pushes toward tangled_rope; orthogonal attribution keeps epsilon low and preserves the rope reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(freedmen_protection_attribution, empirical, 'Cost-attribution question for the reading''s darkest historical episode.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__collective_right_reading, 1791, 2008).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1791, second_amendment_scope__collective_right_reading, theater_ratio, 1791, 0.05).
narrative_ontology:measurement(seco_tr_t1876, second_amendment_scope__collective_right_reading, theater_ratio, 1876, 0.1).
narrative_ontology:measurement(seco_tr_t1939, second_amendment_scope__collective_right_reading, theater_ratio, 1939, 0.25).
narrative_ontology:measurement(seco_tr_t1982, second_amendment_scope__collective_right_reading, theater_ratio, 1982, 0.45).
narrative_ontology:measurement(seco_tr_t1999, second_amendment_scope__collective_right_reading, theater_ratio, 1999, 0.55).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_scope__collective_right_reading, theater_ratio, 2008, 0.6).

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_scope__collective_right_reading, base_extractiveness, 1791, 0.1).
narrative_ontology:measurement(seco_be_t1876, second_amendment_scope__collective_right_reading, base_extractiveness, 1876, 0.14).
narrative_ontology:measurement(seco_be_t1939, second_amendment_scope__collective_right_reading, base_extractiveness, 1939, 0.18).
narrative_ontology:measurement(seco_be_t1982, second_amendment_scope__collective_right_reading, base_extractiveness, 1982, 0.23).
narrative_ontology:measurement(seco_be_t1999, second_amendment_scope__collective_right_reading, base_extractiveness, 1999, 0.25).
narrative_ontology:measurement(seco_be_t2008, second_amendment_scope__collective_right_reading, base_extractiveness, 2008, 0.26).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1791, second_amendment_scope__collective_right_reading, suppression_requirement, 1791, 0.05).
narrative_ontology:measurement(seco_su_t1876, second_amendment_scope__collective_right_reading, suppression_requirement, 1876, 0.08).
narrative_ontology:measurement(seco_su_t1939, second_amendment_scope__collective_right_reading, suppression_requirement, 1939, 0.3).
narrative_ontology:measurement(seco_su_t1982, second_amendment_scope__collective_right_reading, suppression_requirement, 1982, 0.5).
narrative_ontology:measurement(seco_su_t1999, second_amendment_scope__collective_right_reading, suppression_requirement, 1999, 0.58).
narrative_ontology:measurement(seco_su_t2008, second_amendment_scope__collective_right_reading, suppression_requirement, 2008, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_scope__collective_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_scope__collective_right_reading, second_amendment_scope__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__collective_right_reading, second_amendment_scope__civic_right_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the Second Amendment' decomposes into three structurally distinct readings of one kernel (second_amendment_scope). This member authors low epsilon for the standing arrangement as seen from the collective-right seat; the individual-right sibling authors high epsilon for the same arrangement; the civic-right sibling sits between. Historically this reading influenced its siblings — a century of precedent from Cruikshank through Miller shaped the terms each sibling had to answer — while being logically incompatible with both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
