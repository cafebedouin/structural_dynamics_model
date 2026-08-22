% ============================================================================
% CONSTRAINT STORY: ihl_distinction_proportionality__categorical_prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ihl_distinction_proportionality__categorical_prohibition_reading, []).

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
 *   constraint_id: ihl_distinction_proportionality__categorical_prohibition_reading
 *   human_readable: Categorical Prohibition of Autonomous Weapons (Martens Clause Reading)
 *   domain: international_humanitarian_law/military_ethics/technology_governance
 *
 * SUMMARY:
 *   This story authors the categorical prohibition reading as a clean,
 *   epsilon-invariant constraint: a proposed absolute ban on autonomous
 *   weapons systems, grounded in the Martens Clause, under which LAWS would
 *   be unlawful in all cases regardless of technical performance, because
 *   machine-decided killing violates human dignity per se. The referent for
 *   epsilon is the categorical-ban arrangement itself as this reading holds
 *   it — the ban's actual operation on the parties it governs — not the
 *   permissive status quo and not any sibling reading's alternative; the
 *   reading deems the ban justified, and justification is orthogonal to the
 *   structural burden the ban places on identifiable seats. Constraint
 *   family: the colloquial label 'IHL governs autonomous weapons' decomposes
 *   into three structurally distinct claims with different epsilon, victim
 *   sets, and enforcement paths — this categorical reading, the human-agency
 *   reading, and the outcomes-based reading — linked via
 *   network.affects_constraints and documented in each file's
 *   dual_formulation_note. KEY AGENTS (by structural relationship): -
 *   norm_entrepreneur_coalition: Agenda setter (institutional/constrained) —
 *   convenes the diplomatic track and defines the ban's terms -
 *   anti_militarist_civil_society: Primary beneficiary (organized/mobile) —
 *   mobilizes opinion; voluntary participant with full exit -
 *   states_lacking_laws_capability: Primary beneficiary (organized/mobile) —
 *   converts a capability gap into a shared legal ceiling at negligible cost
 *   - civilians_in_conflict_zones: Intended protected beneficiary
 *   (powerless/trapped) — receives the ban's protection only if it binds
 *   deployers - advanced_military_powers: Primary payer
 *   (institutional/constrained) — surrenders the largest capability stock;
 *   blocks consensus but cannot exit the normative environment -
 *   defense_autonomy_industry: Secondary payer (powerful/arbitrage) — loses a
 *   product class in signatory markets; redirects capital -
 *   combat_experienced_operators: Excluded voice (moderate/constrained) —
 *   tactical judgment on delegation limits largely absent from drafting
 *   forums - ihl_scholarly_community: Analytical observer
 *   (analytical/analytical) — supplies the doctrinal arguments all camps cite
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__categorical_prohibition_reading, 0.76).
domain_priors:suppression_score(ihl_distinction_proportionality__categorical_prohibition_reading, 0.8).
domain_priors:theater_ratio(ihl_distinction_proportionality__categorical_prohibition_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, extractiveness, 0.76).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__categorical_prohibition_reading, tangled_rope).
narrative_ontology:human_readable(ihl_distinction_proportionality__categorical_prohibition_reading, "Categorical Prohibition of Autonomous Weapons (Martens Clause Reading)").
narrative_ontology:topic_domain(ihl_distinction_proportionality__categorical_prohibition_reading, "international_humanitarian_law/military_ethics/technology_governance").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__categorical_prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__categorical_prohibition_reading, 'ab2d7666-31c3-48b5-883c-9314d429c174').
narrative_ontology:cs_kernel_codification('ab2d7666-31c3-48b5-883c-9314d429c174', fixed_text).
narrative_ontology:cs_authority_grounding('ab2d7666-31c3-48b5-883c-9314d429c174', lineage).
narrative_ontology:cs_interpretation_layer_present('ab2d7666-31c3-48b5-883c-9314d429c174').
narrative_ontology:cs_reading_relation('ab2d7666-31c3-48b5-883c-9314d429c174', ihl_distinction_proportionality__human_agency_reading, influences).
narrative_ontology:cs_reading_relation('ab2d7666-31c3-48b5-883c-9314d429c174', ihl_distinction_proportionality__outcomes_based_reading, forecloses).
narrative_ontology:cs_axiom('ab2d7666-31c3-48b5-883c-9314d429c174', foundational, machine_decided_killing_dignity_violation_per_se).
narrative_ontology:cs_axiom_status(machine_decided_killing_dignity_violation_per_se, holdable).
narrative_ontology:cs_axiom_grounding('ab2d7666-31c3-48b5-883c-9314d429c174', machine_decided_killing_dignity_violation_per_se, deontological).
narrative_ontology:cs_axiom('ab2d7666-31c3-48b5-883c-9314d429c174', secondary, technical_performance_irrelevant_to_lawfulness).
narrative_ontology:cs_axiom_status(technical_performance_irrelevant_to_lawfulness, holdable).
narrative_ontology:cs_axiom_grounding('ab2d7666-31c3-48b5-883c-9314d429c174', technical_performance_irrelevant_to_lawfulness, deontological).
narrative_ontology:cs_reference_frame('ab2d7666-31c3-48b5-883c-9314d429c174', principles_of_humanity_binding_limit).
narrative_ontology:cs_drift_state('ab2d7666-31c3-48b5-883c-9314d429c174', contemporary_ccw_stalemate, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ab2d7666-31c3-48b5-883c-9314d429c174', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, anti_militarist_civil_society).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, states_lacking_laws_capability).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, civilians_in_conflict_zones).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__categorical_prohibition_reading, advanced_military_powers).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__categorical_prohibition_reading, defense_autonomy_industry).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, norm_entrepreneur_coalition).
narrative_ontology:constraint_vindicates(ihl_distinction_proportionality__categorical_prohibition_reading, martens_clause_principles_of_humanity).
narrative_ontology:constraint_vindicates(ihl_distinction_proportionality__categorical_prohibition_reading, human_dignity_in_lethal_decision_doctrine).
narrative_ontology:constraint_vindicates(ihl_distinction_proportionality__categorical_prohibition_reading, meaningful_human_control_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A coalition of humanitarian organizations, the ICRC, and like-minded states convenes the diplomatic track, drafts treaty language, and defines what counts as compliance with the proposed absolute ban. It organizes General Assembly resolutions, funds campaign infrastructure, and builds the legal argument from the Martens Clause outward. Its authority and institutional relevance grow with each state endorsement; withdrawing from the campaign would mean abandoning a core mandate.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, norm_entrepreneur_coalition, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(ihl_distinction_proportionality__categorical_prohibition_reading, norm_entrepreneur_coalition, beneficiary).

% Campaign networks, NGOs, and grassroots groups mobilize public opinion, lobby national delegations, and document the risks of delegated targeting. They gain issue momentum, membership, and agenda influence as endorsements accumulate. Participation is voluntary and the networks work many adjacent issues, so disengagement is always available to them.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, anti_militarist_civil_society, beneficiary,
    organized, generational, mobile, global).

% Most of the world's states neither develop nor deploy lethal autonomy. An absolute ban converts a capability gap they cannot close into a shared legal ceiling, improving their security position against technologically superior adversaries at negligible cost to themselves. They vote as blocs in UN fora and bear essentially none of the adjustment burden.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, states_lacking_laws_capability, beneficiary,
    organized, generational, mobile, global).

% Populations living where drones and automated defensive systems already operate are the people the ban would shield. They cannot move out of war zones, hold no seat in negotiating rooms, and experience the technology as something decided entirely above them. Their protection arrives only if the norm actually binds deployers.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, civilians_in_conflict_zones, beneficiary,
    powerless, immediate, trapped, regional).

% States operating the most advanced autonomous and semi-autonomous programs — the United States, China, Russia, Israel, and the United Kingdom among them — would surrender the largest stock of capability under an absolute ban and submit to verification of systems they currently classify. They can block treaty-by-consensus processes and invest in workarounds, but they cannot opt out of the reputational and alliance-politics environment the campaign shapes around them.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, advanced_military_powers, payer,
    institutional, generational, constrained, global).

% Contractors building target-recognition software, loitering munitions, and command-and-control automation face the loss of an entire product class in signatory markets. Capital and engineering talent can pivot to adjacent defense lines or to non-party customers, so the loss is real but redirectable.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, defense_autonomy_industry, payer,
    powerful, biographical, arbitrage, global).

% Soldiers and officers who would operate alongside or in place of automated engagement systems hold tactical views on where delegation fails — cluttered urban terrain, degraded communications, ambiguous signatures, escalation-prone encounters. Their testimony appears rarely in the diplomatic and advocacy forums where the ban's terms are drafted.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, combat_experienced_operators, excluded,
    moderate, biographical, constrained, regional).

% Academic international lawyers and military ethicists publish the competing interpretations of the Martens Clause, test the categorical claim against state practice and case law, and supply the doctrinal arguments that every camp cites. They hold no enforcement power and bear none of the arrangement's costs.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_scholarly_community, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ihl_distinction_proportionality__categorical_prohibition_reading, states_lacking_laws_capability).
narrative_ontology:fixing_cost_class(ihl_distinction_proportionality__categorical_prohibition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared legal ceiling on lethal autonomy before deployment entrenches it: states gain mutual assurance that no adversary will field target-selection machines, negotiators gain a bright-line rule far cheaper to verify than continuous performance auditing, and the human-responsibility premise underlying IHL's accountability chain is preserved by construction rather than by case-by-case adjudication.
% TRANSFER_FUNCTION: Moves decision authority over life-and-death targeting permanently back to human institutions; moves strategic option-space and foregone capability from technologically advanced militaries and their suppliers into a common normative pool from which capability-poor states draw relative security at zero cost.
% ABSENT_VOICES: Combat-experienced operators who judge where delegation fails tactically, engineers who believe verifiable compliance architectures are achievable, and publics of states that rely on technological deterrence against conventional threats are thinly represented in the advocacy-led and consensus-blocked forums where the ban's terms take shape; their objections enter mostly as state-delegation talking points rather than as seated participants.
% DISAPPEARANCE_RATIONALE: If the categorical reading vanished overnight, the outcomes-based reading would occupy the vacuum by default: development of increasingly autonomous engagement would proceed under performance standards, the human-control norm would erode incrementally, and the advocacy-diplomatic infrastructure built around the ban would dissolve or reorganize around regulation instead.
% FOUNDING_PROBLEM: The prospect that target selection and engagement decisions migrate from accountable human agents to machines faster than law can respond: accountability gaps when no one decides, diffusion of responsibility for unlawful deaths, erosion of the human-judgment premise beneath IHL's entire architecture, and an arms-race dynamic that would foreclose later agreement.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: developer-state doctrine documents themselves (US DoD Directive 3000.09, UK joint doctrine publications) insist on appropriate levels of human judgment, attesting the underlying concern even while rejecting the categorical remedy; cross-tradition military ethics scholarship and legal analyses independent of the campaign coalition reach the same concern. No source outside the beneficiary set attests that the categorical form specifically is required — that step remains this reading's own claim.
narrative_ontology:disappearance_verdict(ihl_distinction_proportionality__categorical_prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(ihl_distinction_proportionality__categorical_prohibition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__categorical_prohibition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ihl_distinction_proportionality__categorical_prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ihl_distinction_proportionality__categorical_prohibition_reading, 0.76, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ihl_distinction_proportionality__categorical_prohibition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ihl_distinction_proportionality__categorical_prohibition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ihl_distinction_proportionality__categorical_prohibition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.76: the reading bans an entire technology class — the broadest restriction available — and its costs concentrate on the small set of capable states and their suppliers while the overwhelming majority of states pay nothing; breadth times asymmetry places base epsilon at the top of the kernel's family. Suppression 0.80: 'regardless of technical performance' functions as a suppression clause by design — it forecloses the exit of building better-performing systems, which is precisely the exit the outcomes-based sibling would open; suppression is authored as a raw structural property and is not scaled by power or scope. Theater_ratio 0.42: the diplomatic track performs real drafting and vote-building, but a growing share of activity is resolution-passing and communiqué language that changes no deployer's behavior. Accessibility_collapse 0.82: inside the reading's own logic, accepting the per-se premise collapses every performance-regulation alternative at once. Resistance 0.68: the capable states reject the reading openly and block consensus processes. The claimed type, tangled_rope, is authored independently of these numbers: the ban coordinates a genuine collective-action problem (mutual assurance against a killing-machine race, preserved human accountability) while extracting asymmetrically from capability holders and requiring active enforcement machinery to hold. The measurement series run on one shared time grid (t=0..14, eight points, all three metrics authored at every point); trajectories are monotonic rather than cyclical — extractiveness and enforcement capacity ratchet upward as the reading hardens from 'negotiate' to 'ban categorically,' and theater dips during the period of substantive GGE work before rising again as non-binding resolutions multiply.
 *
 * PERSPECTIVAL GAP:
 *   Same-level actor dynamics carry the divergence: all states are formally sovereign equals, yet the ban's seats split by capability — laggard-state seats compute a cheap insurance arrangement, capable-state seats compute confiscation of accumulated advantage, and neither seat can exit the normative environment the other shapes. Industry sits beside capable states in the payer row but holds arbitrage-grade exit (pivotal capital, non-party customers), so its computed burden discounts below the states'. The agenda-setter seat experiences the same structure as norm-building; trapped civilians experience it as distant protection that may never arrive. The engine computes these per-seat classifications from the structural data; the authored claim adjudicates none of them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries map to the low-d end: civilians_in_conflict_zones (trapped, powerless) sit nearest full subsidy; states_lacking_laws_capability (mobile, organized) draw relative security without bearing costs; anti_militarist_civil_society collects agenda influence voluntarily. Victims map high: advanced_military_powers bear the concentrated cost with constrained exit (reputational exposure, alliance politics, verification risk), and defense_autonomy_industry's arbitrage exit pulls its derived d slightly below the states'. The norm_entrepreneur_coalition administers the arrangement and collects authority as it advances — a mild self-interest registered through its beneficiary secondary role. Receipt: the extraction is foregone capability rather than a collected rent, but the seat whose position measurably improves is states_lacking_laws_capability, so gain_flow names that seat rather than 'diffuse.' Fixing: the categorical form admits no partial-compliance architecture by construction, so any repair requires abandoning the founding axiom — prohibitive for whoever could fix it.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading dresses a constructed political-legal demand in quasi-natural clothing ('principles of humanity prohibit...'), which is exactly the move the classification apparatus exists to catch: a false-summit shape would launder the ban as discovered law. Classifying it as tangled_rope keeps both truths visible — the coordination function is real (arms-race prevention, accountability preservation) and the extraction is real (asymmetric, class-wide, enforcement-dependent). Mandatrophy: the founding problem is live, so no obsolescence flag applies; the mismatch consumer reads founding_problem_status=live against disappearance_verdict=world_rearranges, a consistent pairing producing no zombie flag. If verified performance parity ever arrived, the categorical form would face piton risk — maintained theatrically after its rationale eroded — and the superhuman_performance_pressure omega tracks exactly that door.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading (categorical_prohibition_reading) of the kernel ihl_distinction_proportionality; what structural facts would the sibling readings (human_agency_reading, outcomes_based_reading) change?',
    'Side-by-side compilation of the three reading stories: compare victim sets, beneficiary sets, and epsilon over the identical referent; the deltas locate what each reading adds or removes.',
    'Under outcomes_based_reading the victim set collapses to sub-standard deployers and epsilon falls steeply (performance-compliant systems become lawful); under human_agency_reading epsilon is intermediate (lawful only with mandated human judgment loops). This story''s high epsilon is a property of the categorical form alone.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: which kernel, which reading, what siblings would change.').

omega_variable(
    dignity_harm_locus,
    'Where do the readings locate the wrong that IHL must prevent — in the means (a machine deciding), in the outcome (civilian harm), or in the missing human agent — and which loci does this reading treat as legally sufficient?',
    'Doctrinal analysis of each reading''s axioms against the same case set: defensive automation, loitering munitions, air-defense fire-control.',
    'If the disagreement reduces to means-versus-outcome, the categorical and outcomes readings cannot share a framework and foreclosure is confirmed; if it reduces to agent-presence, the categorical reading is a limiting case of human_agency rather than a true rival.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dignity_harm_locus, conceptual, 'Structural location of the inter-reading disagreement.').

omega_variable(
    martens_clause_legal_force,
    'Does the Martens Clause as received in customary law generate categorical prohibitions of weapon classes, or only interpretive guidance that fills gaps without barring means?',
    'State practice and opinio juris survey plus ICJ and ICTY treatment of the Clause; count instances where courts or states derived a flat prohibition from it.',
    'If only interpretive, this reading''s constraint operates as advocacy pressure rather than binding law — effective suppression and enforcement requirements drop until a treaty crystallizes; if categorical force is found, the constraint binds non-consenting states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(martens_clause_legal_force, empirical, 'Whether the Clause''s customary force reaches categorical prohibition.').

omega_variable(
    intrinsic_vs_advocated_dignity_bar,
    'Is the dignity bar on machine-decided killing an intrinsic feature of the moral order the reading discovers, or a construction maintained by the advocacy coalition that benefits from it?',
    'Test the reading''s own tradition: survey whether principled non-consequentialist ethicists outside the campaign coalition converge on the per-se bar independently of campaign framing.',
    'If constructed, the constraint carries a false-summit shape (presented as discovered, actually built) and its classification leans further toward enforced extraction; if intrinsic, part of its asymmetry is the unavoidable price of a genuine moral limit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intrinsic_vs_advocated_dignity_bar, conceptual, 'Naturalness of the dignity threshold versus coalition construction.').

omega_variable(
    superhuman_performance_pressure,
    'Can the categorical premise (''regardless of technical performance'') survive demonstrated superhuman discrimination performance, or does the reading''s own tradition face internal pressure to soften?',
    'Adversarial validation trials of discrimination and proportionality performance against human-operator baselines under realistic battlefield conditions.',
    'Demonstrated superhuman performance would force the reading onto purely deontological ground (abandoning any empirical shelter) or split the coalition; repeated failure of such demonstrations would stabilize the categorical form.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(superhuman_performance_pressure, empirical, 'Empirical insulation of the per-se axiom.').

omega_variable(
    asymmetric_burden_defection,
    'Does the concentration of adjustment costs on capable states create defection incentives large enough to unravel the coordination the ban provides?',
    'Track ratification and compliance patterns of capable states under any emerging instrument; compare with chemical-weapons-ban accession history.',
    'Sustained great-power abstention would leave the ban binding mainly on the non-capable — reducing it toward a paper coordination shell with the real regime set by defectors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asymmetric_burden_defection, empirical, 'Defection risk from asymmetric burden distribution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__categorical_prohibition_reading, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl_categorical_tr_t0, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement_basis(ihl_categorical_tr_t0, observed).
narrative_ontology:measurement(ihl_categorical_tr_t2, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 2, 0.4).
narrative_ontology:measurement_basis(ihl_categorical_tr_t2, observed).
narrative_ontology:measurement(ihl_categorical_tr_t4, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 4, 0.36).
narrative_ontology:measurement_basis(ihl_categorical_tr_t4, observed).
narrative_ontology:measurement(ihl_categorical_tr_t6, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 6, 0.35).
narrative_ontology:measurement_basis(ihl_categorical_tr_t6, observed).
narrative_ontology:measurement(ihl_categorical_tr_t8, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 8, 0.37).
narrative_ontology:measurement_basis(ihl_categorical_tr_t8, observed).
narrative_ontology:measurement(ihl_categorical_tr_t10, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 10, 0.39).
narrative_ontology:measurement_basis(ihl_categorical_tr_t10, observed).
narrative_ontology:measurement(ihl_categorical_tr_t12, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 12, 0.41).
narrative_ontology:measurement_basis(ihl_categorical_tr_t12, observed).
narrative_ontology:measurement(ihl_categorical_tr_t14, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 14, 0.42).
narrative_ontology:measurement_basis(ihl_categorical_tr_t14, observed).

% Extraction over time
narrative_ontology:measurement(ihl_categorical_be_t0, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(ihl_categorical_be_t0, observed).
narrative_ontology:measurement(ihl_categorical_be_t2, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 2, 0.58).
narrative_ontology:measurement_basis(ihl_categorical_be_t2, observed).
narrative_ontology:measurement(ihl_categorical_be_t4, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 4, 0.62).
narrative_ontology:measurement_basis(ihl_categorical_be_t4, observed).
narrative_ontology:measurement(ihl_categorical_be_t6, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 6, 0.65).
narrative_ontology:measurement_basis(ihl_categorical_be_t6, observed).
narrative_ontology:measurement(ihl_categorical_be_t8, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 8, 0.68).
narrative_ontology:measurement_basis(ihl_categorical_be_t8, observed).
narrative_ontology:measurement(ihl_categorical_be_t10, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 10, 0.71).
narrative_ontology:measurement_basis(ihl_categorical_be_t10, observed).
narrative_ontology:measurement(ihl_categorical_be_t12, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 12, 0.74).
narrative_ontology:measurement_basis(ihl_categorical_be_t12, observed).
narrative_ontology:measurement(ihl_categorical_be_t14, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 14, 0.76).
narrative_ontology:measurement_basis(ihl_categorical_be_t14, observed).

% Suppression requirement over time
narrative_ontology:measurement(ihl_categorical_su_t0, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(ihl_categorical_su_t0, observed).
narrative_ontology:measurement(ihl_categorical_su_t2, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 2, 0.53).
narrative_ontology:measurement_basis(ihl_categorical_su_t2, observed).
narrative_ontology:measurement(ihl_categorical_su_t4, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 4, 0.58).
narrative_ontology:measurement_basis(ihl_categorical_su_t4, observed).
narrative_ontology:measurement(ihl_categorical_su_t6, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 6, 0.62).
narrative_ontology:measurement_basis(ihl_categorical_su_t6, observed).
narrative_ontology:measurement(ihl_categorical_su_t8, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 8, 0.66).
narrative_ontology:measurement_basis(ihl_categorical_su_t8, observed).
narrative_ontology:measurement(ihl_categorical_su_t10, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement_basis(ihl_categorical_su_t10, observed).
narrative_ontology:measurement(ihl_categorical_su_t12, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 12, 0.75).
narrative_ontology:measurement_basis(ihl_categorical_su_t12, observed).
narrative_ontology:measurement(ihl_categorical_su_t14, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 14, 0.8).
narrative_ontology:measurement_basis(ihl_categorical_su_t14, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__categorical_prohibition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_distinction_proportionality__human_agency_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_distinction_proportionality__outcomes_based_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'IHL governs autonomous weapons' covers three structurally distinct claims that cannot share one story because their epsilon values, victim sets, and enforcement paths diverge. This file is the categorical_prohibition_reading (highest epsilon: bans the entire technology class; victims are capability holders). The human_agency_reading is the traditional center of gravity (intermediate epsilon: lawful only with human judgment in the loop); the outcomes_based_reading is the permissive pole (epsilon concentrated on sub-standard deployers only). Upstream/downstream: the human-agency reading supplies the doctrinal material this reading extends, and this reading's advocacy exerts structural pressure on it; the outcomes reading stands in direct contradiction to this one. Each member links the others via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
