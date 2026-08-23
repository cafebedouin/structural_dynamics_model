% ============================================================================
% CONSTRAINT STORY: second_amendment_boundary__insurrectionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_boundary__insurrectionist_reading, []).

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
 *   constraint_id: second_amendment_boundary__insurrectionist_reading
 *   human_readable: Second Amendment Boundary — Insurrectionist Reading (Resistance-Capacity Instantiation)
 *   domain: constitutional law / political theory / firearms policy
 *
 * SUMMARY:
 *   This constraint is the insurrectionist instantiation of the
 *   second_amendment_boundary kernel: the arms right exists to preserve armed
 *   counter-force capacity against tyrannical government, and individual
 *   possession is instrumentally protected as the material precondition of
 *   potential overthrow. Under this reading the protected domain extends
 *   logically to military-grade platforms (the endpoint follows from the
 *   premise: arms suited to resisting a state must resemble the state's
 *   arms), state disarmament efforts are classified as tyranny precursors
 *   rather than policy options, and the arrangement's costs fall on parties
 *   who never agreed to hold them — officers confronting military-pattern
 *   firepower, communities absorbing realized mass-casualty events, and
 *   everyone within range of a site where the retained capacity engages. The
 *   deterrent legitimacy claimed by armed holders is asserted by the
 *   arrangement's own stewards and is the story's central open question. KEY
 *   AGENTS (by structural relationship):
 *   armed_citizens_claiming_deterrent_legitimacy — primary beneficiary
 *   (organized / identity_locked), holds the protected inventory and the
 *   deterrent claim; firearms_industry_manufacturers — secondary beneficiary
 *   (powerful / arbitrage), converts the protected domain into product lines
 *   and revenue, and is the seat the arrangement's monetary receipts
 *   demonstrably accrue to; state_security_apparatus — primary target
 *   (institutional / trapped), bears degraded regulatory reach and elevated
 *   encounter lethality; civilians_in_hypothetical_conflict_zones — co-target
 *   (powerless / trapped), bear collateral exposure if the capacity
 *   activates; mass_casualty_event_communities — co-target (powerless /
 *   trapped), absorb the realized costs and are foreclosed from remedy;
 *   constitutional_doctrine_courts — agenda setter (institutional /
 *   constrained), draw the boundary line; insurrectionist_advocacy_networks —
 *   agenda setter and collector (organized / identity_locked), steward the
 *   trigger narrative and fundraise on it; gun_violence_survivor_advocates —
 *   excluded voice (moderate / constrained), would cap the protected
 *   category, no standing where the line is adjudicated;
 *   comparative_politics_scholars — analytical observer, external test of the
 *   deterrent premise. Family note: this is one of three readings of the
 *   shared text; the siblings are separate constraint files linked via
 *   network.affects_constraints, and the decomposition logic is recorded in
 *   network.dual_formulation_note.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__insurrectionist_reading, 0.34).
domain_priors:suppression_score(second_amendment_boundary__insurrectionist_reading, 0.72).
domain_priors:theater_ratio(second_amendment_boundary__insurrectionist_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, extractiveness, 0.34).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__insurrectionist_reading, snare).
narrative_ontology:human_readable(second_amendment_boundary__insurrectionist_reading, "Second Amendment Boundary — Insurrectionist Reading (Resistance-Capacity Instantiation)").
narrative_ontology:topic_domain(second_amendment_boundary__insurrectionist_reading, "constitutional law / political theory / firearms policy").

domain_priors:requires_active_enforcement(second_amendment_boundary__insurrectionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__insurrectionist_reading, '31b527cb-716f-4c66-a338-d6377e3ec82f').
narrative_ontology:cs_kernel_codification('31b527cb-716f-4c66-a338-d6377e3ec82f', fixed_text).
narrative_ontology:cs_authority_grounding('31b527cb-716f-4c66-a338-d6377e3ec82f', lineage).
narrative_ontology:cs_interpretation_layer_present('31b527cb-716f-4c66-a338-d6377e3ec82f').
narrative_ontology:cs_reading_relation('31b527cb-716f-4c66-a338-d6377e3ec82f', second_amendment_boundary__individual_right_reading, influences).
narrative_ontology:cs_reading_relation('31b527cb-716f-4c66-a338-d6377e3ec82f', second_amendment_boundary__militia_conditioned_reading, forecloses).
narrative_ontology:cs_axiom('31b527cb-716f-4c66-a338-d6377e3ec82f', foundational, resistance_capacity_constitutive_of_right).
narrative_ontology:cs_axiom_status(resistance_capacity_constitutive_of_right, holdable).
narrative_ontology:cs_axiom_grounding('31b527cb-716f-4c66-a338-d6377e3ec82f', resistance_capacity_constitutive_of_right, instrumental).
narrative_ontology:cs_axiom('31b527cb-716f-4c66-a338-d6377e3ec82f', secondary, disarmament_precursor_inference).
narrative_ontology:cs_axiom_status(disarmament_precursor_inference, holdable).
narrative_ontology:cs_axiom_grounding('31b527cb-716f-4c66-a338-d6377e3ec82f', disarmament_precursor_inference, empirically_contingent).
narrative_ontology:cs_reference_frame('31b527cb-716f-4c66-a338-d6377e3ec82f', founding_counterforce_compact).
narrative_ontology:cs_drift_state('31b527cb-716f-4c66-a338-d6377e3ec82f', contemporary_juridical_expansion, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('31b527cb-716f-4c66-a338-d6377e3ec82f', '').
narrative_ontology:cs_kernel_id(second_amendment_boundary__insurrectionist_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__insurrectionist_reading, armed_citizens_claiming_deterrent_legitimacy).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__insurrectionist_reading, firearms_industry_manufacturers).
narrative_ontology:constraint_victim(second_amendment_boundary__insurrectionist_reading, state_security_apparatus).
narrative_ontology:constraint_victim(second_amendment_boundary__insurrectionist_reading, civilians_in_hypothetical_conflict_zones).
narrative_ontology:constraint_victim(second_amendment_boundary__insurrectionist_reading, mass_casualty_event_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__insurrectionist_reading, insurrectionist_advocacy_networks).
narrative_ontology:constraint_vindicates(second_amendment_boundary__insurrectionist_reading, armed_populace_deterrence_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_boundary__insurrectionist_reading, disarmament_slippery_slope_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own and carry firearms under a constitutional shield whose stated warrant is keeping a counter-force in being against domestic tyranny. Many train, stockpile, and organize around the expectation that they may someday be called to use the arms for that purpose. Leaving the arrangement means surrendering both the equipment and the self-understanding that accompanies it — the citizen-guardian identity is not separable from the hardware. They bear readiness costs, patchwork legal ambiguity, and social stigma, and they read every new restriction as the opening step of the sequence they exist to forestall.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, armed_citizens_claiming_deterrent_legitimacy, beneficiary,
    organized, generational, identity_locked, national).

% Design, manufacture, and market rifles, receivers, ammunition, and accessories whose fastest-growing segment descends from military service rifles. The broad protected category enlarges the addressable market and insulates product lines from prohibition-style measures. Revenue tracks the intensity of the deterrent narrative — fear of impending bans reliably produces buying surges. Capital, tooling, and product lines are mobile across civilian, export, and sporting segments.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, firearms_industry_manufacturers, beneficiary,
    powerful, biographical, arbitrage, global).

% Police agencies and the federal services charged with public order face adversaries equipped with armor-piercing and high-capacity weaponry in routine encounters, and lose officers at rates tied to civilian access to military-pattern arms. Their principal tested instruments — registration, licensing, and category-ban schemes used in peer countries — are the specific measures the boundary classes as out of bounds. They cannot decline the security function; staff escalation and equipment escalation are the levers left inside the line.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, state_security_apparatus, payer,
    institutional, generational, trapped, national).

% Everyone within range of a site where the retained counter-force engages — which, under the arrangement's own logic, includes any street, school, or public gathering. They were never consulted on the trade, cannot feasibly relocate out of exposure range, and their expected losses appear in no ledger the arrangement keeps; the deterrent narrative books them as the price of liberty. Daily life also runs on an elevated armed-violence baseline the arrangement sustains.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, civilians_in_hypothetical_conflict_zones, payer,
    powerless, generational, trapped, national).

% School and town communities that have absorbed realized mass shootings carried out with the protected category of arms. After each event they organize for category limits, extreme-risk intervention orders, and registration, and watch each proposal die at the boundary line. Members cannot leave the risk environment without abandoning homes, jobs, schools, and graves.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, mass_casualty_event_communities, payer,
    powerless, biographical, trapped, local).

% Federal and state judiciaries, apex court foremost, decide what the arms text means: which weapons count as protected, which justifications legitimize possession, and which regulatory measures are categorically barred. Their rulings bind every other seat and are effectively irreversible within an officeholder's career; precedent constrains revision as tightly as statute would.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, constitutional_doctrine_courts, agenda_setter,
    institutional, generational, constrained, national).

% Membership organizations, training networks, and media operations that teach the trigger-recognition narrative — which government actions count as tyranny, what readiness requires — and mobilize members against any legislative touch of the boundary. Dues, donations, and advertising revenue scale with perceived imminence of disarmament, which their own communications sustain. Organizational solvency and leadership careers are bound to the narrative continuing.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, insurrectionist_advocacy_networks, agenda_setter,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_boundary__insurrectionist_reading, insurrectionist_advocacy_networks, beneficiary).

% People wounded in, or bereaved by, shootings who campaign for the regulatory package the boundary bars. They hold standing in legislative hearings and public opinion but none where the boundary itself is adjudicated; doctrine classes their program as categorically illegitimate before any interest-balancing occurs. Exiting would mean abandoning both the cause and the communities that produced them.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, gun_violence_survivor_advocates, excluded,
    moderate, biographical, constrained, national).

% Researchers who assemble cross-national evidence on whether widespread civilian armament correlates with democratic survival, coup resistance, or backsliding, and publish findings wherever they point. They hold no stake in the arrangement and can be disregarded by every seated party; their work is the only standing external test of the deterrent premise.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, comparative_politics_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_boundary__insurrectionist_reading, firearms_industry_manufacturers).
narrative_ontology:fixing_cost_class(second_amendment_boundary__insurrectionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a distributed reserve of military-capable arms and a shared account of when state conduct crosses into tyranny, so that counter-force capacity exists ahead of any triggering moment without centralized provisioning; sustains readiness expectations and a common trigger-recognition standard among holders.
% TRANSFER_FUNCTION: Moves lethal-risk exposure onto state security personnel and anyone near a site where the capacity activates; moves household spending toward arms designed for use against human bodies; moves veto power over firearms legislation to whichever interpreter draws the boundary most permissively; moves dues and donation revenue to organizations that steward the trigger narrative.
% ABSENT_VOICES: Survivors of realized mass-casualty events and residents of the places they occurred would cap the protected category at utility and sporting arms and exclude designs optimized against people; their testimony reaches legislatures but has no standing where the boundary is drawn, because doctrine classes their remedies as categorically illegitimate rather than weighing their interests. Generations not yet born who inherit whatever risk landscape results are likewise absent from the drafting table.
% DISAPPEARANCE_RATIONALE: If the boundary reverted overnight to a narrow utility-and-sporting line with military-pattern platforms regulable, pending category bans would become constitutionally available within a legislative cycle, manufacturers would retool toward hunting and competition segments, militia organizing would lose its constitutional shelter and contract toward hobby scale, and the trigger-recognition narrative would lose the legal backstop that currently shields it from ordinary policy disagreement. The armed-politics economy built around the broad line visibly rearranges.
% FOUNDING_PROBLEM: After a war that began with disarmament raids at Lexington and Concord, the drafters faced a standing-army problem: how to keep a centralizing government from monopolizing organized force and turning it inward. The settlement enrolled citizens in a militia system that presumed privately held arms as its materiel.
% FOUNDING_PROBLEM_CORROBORATION: Founding-era historiography, written from outside the beneficiary set, corroborates that the anti-standing-army problem was real and urgent at ratification. Against modern liveness, comparative-politics research on civil resistance and democratic survival, together with the record of Shays' Rebellion and the Whiskey Rebellion — both armed challenges to the young republic crushed by governments raising armies — attest from outside the arrangement that the strategic premise is unsupported. No corroborating source outside the benefiting parties attests that the problem remains live today; modern liveness is asserted almost exclusively by the arrangement's own stewards, and that absence is itself the signal.
narrative_ontology:disappearance_verdict(second_amendment_boundary__insurrectionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_boundary__insurrectionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__insurrectionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_boundary__insurrectionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_boundary__insurrectionist_reading, 0.34, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_boundary__insurrectionist_reading_tests).
:- end_tests(second_amendment_boundary__insurrectionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. The claimed type is snare on structural grounds: the arrangement's coordination warrant — that dispersed small arms deter state tyranny — is asserted by its stewards rather than demonstrated anywhere in the operational record; its operative transfers run to identifiable payers; and its persistence depends on doctrinal machinery that classes regulatory alternatives as categorically illegitimate rather than merely unwise. The ε scalar is deliberately reading-indexed: per the kernel-reading cutover, its referent is the arrangement this reading instantiates — the broad protected domain with the resistance warrant attached — and its value is authored from that reading's own lights, which register the collateral exposure the frame openly concedes (and prices low) but do not book outsider mortality as a debit against the design. Payer seats therefore compute far higher effective extraction than the scalar suggests; the engine owns that arithmetic, and the divergence between the reading-indexed scalar and computed per-seat extraction is precisely the datum this story contributes. Suppression is high (0.72) and, by design, unscaled by power or scope — it is the raw structural force of doctrinal immunity plus enforcement orthodoxy, roughly sixty percent structural (precedent lock, categorical bars, litigation attrition against regulation) and forty percent internalized (identity fusion: the citizen-guardian self-concept makes exit unthinkable independently of legal barriers; see the suppression-mechanism omega). Theater ratio 0.45 reflects the growing share of observable activity — drills, pageantry, tactical display, anniversary reenactment, ban-scare merchandising — that maintains the deterrent narrative rather than any deployable capacity. Accessibility collapse 0.55: inside the reading's frame alternatives collapse nearly completely (any disarmament becomes unthinkable), but across the wider polity peer-country regulatory models and narrower readings remain live, so collapse is partial in fact. Resistance 0.60: sustained survivor-led and municipal opposition, recurring ballot initiatives, and persistent scholarly refutation of the deterrent premise. The measurement series share one grid (t=0..48, step 8) so every tracked metric is authored at every examined point. Trajectories are monotone ratchets rather than cycles: episodic tragedy-reform-backlash perturbations ride on a rising baseline (each mass-casualty event produces a reform push, its failure entrenches the boundary further, and each failed push re-finances the beneficiary bloc through ban-scare purchasing and fundraising), and the net direction is what this grid resolves. The rising suppression_requirement series is authored deliberately: the story traces enforcement intensification — each apex-court expansion narrows what regulation remains available, and movement enforcement punishes deviation from the trigger narrative — which is enforcement-capacity change, not merely shifting extraction.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the armed-holder seat the arrangement is functioning insurance: a premium paid in money, training, and stigma against a catastrophe that has not arrived — experienced as protection, not loss. From the officer seat and the mass-casualty-community seat the same structure is enforced risk-placement: lethal exposure assigned to people who never agreed to hold it, with the remedy shelf legally emptied in advance. The courts sit administratively above the trade, setting the exchange rate between safety measures and protected arms while bearing neither side's exposure. Coalition prospects for the powerless payers are real but blunted: survivors, municipal governments, and policing interests have repeatedly aligned, yet the opposing bloc is identity-fused (exit costs are existential, not financial) and its collector's revenues rise on every reform scare, so each reform cycle re-finances the resistance it provokes. Nothing in the authored claim adjudicates these divergences; the engine derives per-seat classifications from power, exit, and declared direction.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. armed_citizens_claiming_deterrent_legitimacy is the declared beneficiary with identity_locked exit: derivation places it near the subsidized end (low d), with the caveat that the deterrent it claims is the story's open empirical question — if the omega resolves against deterrence, the seat's benefit is nominal and its effective position shifts. firearms_industry_manufacturers is beneficiary with arbitrage-grade exit — nearest the beneficiary pole — and additionally collects the arrangement's monetary receipts, which is why gain_flow names it. state_security_apparatus is a declared victim that cannot exit its own function: derivation places it near the full-target end. civilians_in_hypothetical_conflict_zones and mass_casualty_event_communities are trapped powerless victims at the full-target end, with the contingency caveat recorded in the victim-concretion omega (one class's losses depend on a trigger that has not fired; the other's arrive annually). Courts and advocacy networks sit outside the benefit/loss axis as administrators — courts constrained by precedent, networks identity-locked to the narrative they steward and collecting through it. No directionality overrides are used: the beneficiary/victim declarations plus exit atoms already differentiate every same-power pair (two organized, identity-locked actors — holders and advocacy networks — separate cleanly because one is declared beneficiary and the other administrator-collector).
 *
 * MANDATROPHY ANALYSIS:
 *   The snare call is what stops the deterrent story from laundering risk-placement as coordination: a genuine coordination solution leaves participants net beneficiaries and leaves alternatives unsuppressed, and this arrangement satisfies neither test on the structural record — the warrant is asserted, the payers are identifiable, and the alternatives are categorically barred. The reverse mislabel is guarded too: nothing here reads the arrangement as mere inertia, because its enforcement is active and intensifying (rising suppression_requirement), which is snare behavior rather than piton behavior. The founding-problem interview locates the live uncertainty: the founding problem — counter-force against a monopolizing center — was real at ratification and is corroborated as real-then by historiography outside the beneficiary set, but whether it remains live now is attested today almost solely by the arrangement's own stewards, so the genealogy is marked contested rather than dead. Because the status is contested rather than dead, no automatic capture/zombie mismatch flag fires against the world_rearranges disappearance verdict; but a decisive external refutation of the deterrent premise (the deterrence-support omega) would drive the status toward dead while the verdict stayed world_rearranges — exactly the mismatch signature the battery flags for zombie review, and the transition this story is positioned to witness.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_empirical_support,
    'Does dispersed civilian small-arms possession actually deter state-level tyranny or democratic backsliding, or is the deterrent effect asserted rather than demonstrated?',
    'Comparative-politics analysis correlating civilian armament rates with democratic survival, coup occurrence, and backsliding episodes across polities; natural experiments from jurisdictions that sharply restricted civilian armament and tracked subsequent institutional trajectory.',
    'If the deterrent effect is unsupported, the coordination warrant is cover and the snare classification is confirmed with the beneficiary seat''s claimed benefit exposed as nominal; if supported, the arrangement has a genuine coordination component and the classification moves toward tangled_rope with the extraction read as its price.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_empirical_support, empirical, 'Whether the deterrent premise underlying the entire arrangement has any empirical support.').

omega_variable(
    reading_partition_integrity,
    'Are the three readings of the second_amendment_boundary kernel genuinely distinct constraints with distinct victim sets, or scope disagreements over a single arrangement?',
    'Compile the sibling files and compare victim sets, ε values, and computed classifications across the family; the manifest''s declared deltas predict different victim sets per reading.',
    'If siblings reduce to one arrangement viewed three ways, this partition over-models and the family should merge; if victim sets differ as declared, the partition is confirmed and cross-reading ε comparison becomes the family''s core measurement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_partition_integrity, conceptual, 'Committer-frame partition integrity: this story instantiates one reading; siblings are other constraints, not opinions about this one.').

omega_variable(
    epsilon_referent_under_reading_lights,
    'ε here is authored from the insurrectionist reading''s own lights over the insurrectionist instantiation; harms visible only from payer seats (outsider mortality, degraded state security) enter the engine through structural declarations rather than the scalar. Does the story under-report extraction relative to an all-seats assessment?',
    'Compare computed per-seat effective extraction against the authored scalar once the engine runs; large systematic divergence flags the reading-indexed discount explicitly.',
    'If divergence is large, the corpus learns how much extraction a reading''s own frame discounts; if small, the frame''s accounting approximates the all-seats view and the scalar stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_referent_under_reading_lights, conceptual, 'Methodological omega on reading-indexed ε versus all-seats extraction visibility.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression of alternatives predominantly structural (doctrinal barriers, precedent lock, categorical bars) or internalized (identity fusion making disarmament unthinkable even where legal barriers loosen)?',
    'Post-restriction attitude and compliance trajectories in jurisdictions that executed large buybacks or category bans: if opposition and identity attachment persist after the capability is surrendered, the internalized share dominates; if attitudes normalize quickly, the structural share dominates.',
    'If internalized, effective suppression exceeds the structural measure — targets carry the barrier with them after any legal exit opens — deepening the lock and hardening the snare reading; if structural, removing the doctrinal barriers would release the constraint quickly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized mechanism behind the arrangement''s suppression profile.').

omega_variable(
    victim_class_concretion,
    'Do the declared victim classes describe real extraction, or is the civilian-conflict victim set a counterfactual artifact contingent on a trigger event that may never fire?',
    'Actuarial comparison of expected hypothetical-conflict collateral against realized annual mortality attributable to the protected military-pattern platform category; treat realized annual losses as continuous extraction and hypothetical-only losses as contingent.',
    'If extraction is continuous through realized mortality, the victim declarations describe an operating transfer and the snare structure is load-bearing; if purely contingent, part of the victim set is speculative and the arrangement''s extraction is deferred rather than running.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_class_concretion, empirical, 'Whether the victim set is realized or counterfactual, and what that does to the extraction picture.').

omega_variable(
    collateral_consent_legitimacy,
    'May the arrangement''s designers legitimately impose lethal-risk exposure on non-consenting third parties as the price of a liberty insurance they did not purchase?',
    'Normative analysis of consent boundaries in constitutional design: compare against accepted precedents for imposing non-consented risk on bystanders in exchange for collective goods, and against the arrangement''s own refusal to consult the exposed.',
    'If the imposition is illegitimate, the transfer is non-consensual by construction and the extraction reading strengthens; if the polity accepts it as social-contract pricing, a portion of the measured extraction is reclassified as the acknowledged cost of a chosen good.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(collateral_consent_legitimacy, preference, 'Whether imposing unconsulted lethal risk on bystanders is a legitimate design choice or non-consensual transfer.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__insurrectionist_reading, 0, 48).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_boundary__insurrectionist_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(seco_tr_t8, second_amendment_boundary__insurrectionist_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(seco_tr_t16, second_amendment_boundary__insurrectionist_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement(seco_tr_t24, second_amendment_boundary__insurrectionist_reading, theater_ratio, 24, 0.31).
narrative_ontology:measurement(seco_tr_t32, second_amendment_boundary__insurrectionist_reading, theater_ratio, 32, 0.37).
narrative_ontology:measurement(seco_tr_t40, second_amendment_boundary__insurrectionist_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement(seco_tr_t48, second_amendment_boundary__insurrectionist_reading, theater_ratio, 48, 0.45).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 0, 0.14).
narrative_ontology:measurement(seco_be_t8, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 8, 0.17).
narrative_ontology:measurement(seco_be_t16, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 16, 0.2).
narrative_ontology:measurement(seco_be_t24, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 24, 0.24).
narrative_ontology:measurement(seco_be_t32, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 32, 0.28).
narrative_ontology:measurement(seco_be_t40, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 40, 0.31).
narrative_ontology:measurement(seco_be_t48, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 48, 0.34).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(seco_su_t8, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 8, 0.54).
narrative_ontology:measurement(seco_su_t16, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(seco_su_t24, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 24, 0.62).
narrative_ontology:measurement(seco_su_t32, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 32, 0.66).
narrative_ontology:measurement(seco_su_t40, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 40, 0.69).
narrative_ontology:measurement(seco_su_t48, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 48, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__insurrectionist_reading, identity_coordination).
narrative_ontology:affects_constraint(second_amendment_boundary__insurrectionist_reading, second_amendment_boundary__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__insurrectionist_reading, second_amendment_boundary__militia_conditioned_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the ε-invariance principle: the colloquial label 'the Second Amendment' covers three structurally distinct claims about one stabilized text, emitted as three stories of the second_amendment_boundary kernel. This file instantiates the insurrectionist reading only; the individual_right_reading and militia_conditioned_reading are separate constraints with their own ε, beneficiary/victim structures, and classifications. Upstream/downstream: founding-lineage claims feed the insurrectionist instantiation, whose litigation and rhetoric in turn exert structural pressure on the individual-right reading's scope doctrine (military-pattern common-use claims), which is why this file declares an influences edge toward that sibling. The militia_conditioned sibling is linked as a logically exclusive alternative within any single framework. Cross-reading comparison of victim sets and ε is the corpus measurement this family exists to take.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
