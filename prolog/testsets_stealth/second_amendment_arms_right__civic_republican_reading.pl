% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__civic_republican_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_arms_right__civic_republican_reading, []).

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
 *   constraint_id: second_amendment_arms_right__civic_republican_reading
 *   human_readable: Armed Citizenship as Republican Safeguard (Civic-Republican Reading)
 *   domain: constitutional law/political philosophy/legal interpretation
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the Second Amendment kernel: the
 *   civic-republican reading, under which the right protects armed
 *   citizenship — the capacity of citizens to keep and bear arms as
 *   constituents of a well-regulated militia — as a prerequisite of
 *   republican self-governance. The right is neither purely individual (it is
 *   constituted by civic role and coupled to duty) nor state-centered (it is
 *   held by citizens, not by state governments as such). Operationally the
 *   arrangement does three things: it removes the disarmament option from
 *   governments, it imposes equipment/training/muster obligations on
 *   citizens, and it legitimizes qualification regulation so long as
 *   regulation preserves rather than destroys the trained civic body. The
 *   colloquial label 'the Second Amendment right' decomposes, per the
 *   epsilon-invariance principle, into three structurally distinct
 *   constraints — this reading, the individual-right reading, and the
 *   collective-right reading — with different holder sets, different victim
 *   sets, and different epsilon values; they are linked as a constraint
 *   family through network.affects_constraints. The claim/metric gap is
 *   deliberate: the reading is CLAIMED as tangled_rope (genuine coordination
 *   plus identifiable asymmetric extraction) while the metrics are authored
 *   independently from the arrangement's documented operation; the engine
 *   computes per-seat classifications from the structural data.
 *
 * KEY AGENTS:
 *   - citizen_militia_members: dual-positioned principal (organized / identity_locked) — holds the protected right AND owes the training, equipment, and muster duties; exit requires renouncing civic membership itself
 *   - state_governments: subsidized beneficiary with surrendered discretion (institutional / constrained) — receives defense capacity without financing a standing force, pays in lost disarmament options
 *   - federal_militia_authorities: agenda-setter (institutional / mobile) — writes and administers the organizing, arming, and qualification statutes
 *   - conscientious_objectors: primary victim among principled dissenters (powerless / trapped) — bear fines and civic subordination with no conscience-compatible path to full membership
 *   - indigent_militia_delinquents: primary victim among the poor (powerless / trapped) — bore the duty's costs inversely to ability to pay while wealthier peers bought commutations
 *   - judicial_interpreters: analytical observer (institutional / analytical) — adjudicates how far regulation may go before it destroys the protected capacity
 *   - professional_army_advocates: excluded faction (powerful / arbitrage) — locked out of the constitutional conversation by the anti-standing-army premise, litigates in ordinary politics instead
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__civic_republican_reading, 0.45).
domain_priors:suppression_score(second_amendment_arms_right__civic_republican_reading, 0.38).
domain_priors:theater_ratio(second_amendment_arms_right__civic_republican_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__civic_republican_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_arms_right__civic_republican_reading, "Armed Citizenship as Republican Safeguard (Civic-Republican Reading)").
narrative_ontology:topic_domain(second_amendment_arms_right__civic_republican_reading, "constitutional law/political philosophy/legal interpretation").

domain_priors:requires_active_enforcement(second_amendment_arms_right__civic_republican_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__civic_republican_reading, '34cae127-65e7-49ee-b830-500badfb04cc').
narrative_ontology:cs_kernel_codification('34cae127-65e7-49ee-b830-500badfb04cc', fixed_text).
narrative_ontology:cs_authority_grounding('34cae127-65e7-49ee-b830-500badfb04cc', lineage).
narrative_ontology:cs_interpretation_layer_present('34cae127-65e7-49ee-b830-500badfb04cc').
narrative_ontology:cs_reading_relation('34cae127-65e7-49ee-b830-500badfb04cc', second_amendment_arms_right__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('34cae127-65e7-49ee-b830-500badfb04cc', second_amendment_arms_right__collective_right_reading, coexists_with).
narrative_ontology:cs_axiom('34cae127-65e7-49ee-b830-500badfb04cc', foundational, arms_bearing_constitutes_civic_membership).
narrative_ontology:cs_axiom_status(arms_bearing_constitutes_civic_membership, holdable).
narrative_ontology:cs_axiom_grounding('34cae127-65e7-49ee-b830-500badfb04cc', arms_bearing_constitutes_civic_membership, deontological).
narrative_ontology:cs_axiom('34cae127-65e7-49ee-b830-500badfb04cc', secondary, regulation_bound_by_civic_capacity_preservation).
narrative_ontology:cs_axiom_status(regulation_bound_by_civic_capacity_preservation, holdable).
narrative_ontology:cs_axiom_grounding('34cae127-65e7-49ee-b830-500badfb04cc', regulation_bound_by_civic_capacity_preservation, instrumental).
narrative_ontology:cs_reference_frame('34cae127-65e7-49ee-b830-500badfb04cc', armed_citizen_body_republican_safeguard).
narrative_ontology:cs_drift_state('34cae127-65e7-49ee-b830-500badfb04cc', post_heller_doctrinal_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('34cae127-65e7-49ee-b830-500badfb04cc', '').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__civic_republican_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__civic_republican_reading, citizen_militia_members).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__civic_republican_reading, state_governments).
narrative_ontology:constraint_victim(second_amendment_arms_right__civic_republican_reading, conscientious_objectors).
narrative_ontology:constraint_victim(second_amendment_arms_right__civic_republican_reading, indigent_militia_delinquents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(second_amendment_arms_right__civic_republican_reading, citizen_militia_members).
narrative_ontology:constraint_victim(second_amendment_arms_right__civic_republican_reading, state_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the right to keep and bear arms as a constitutive marker of full civic standing, and owe the corresponding duties: acquiring prescribed equipment, attending training and muster, submitting to qualification standards. Their protection against disarmament exists only insofar as they remain inside the civic-militia body; leaving means renouncing civic membership itself, not cancelling a subscription. Equipment costs, training time, and historically muster fines flow out of their households; the shield against disarmament and the civic status flow back.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, citizen_militia_members, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_arms_right__civic_republican_reading, citizen_militia_members, payer).

% Receive a defense-ready citizen body without financing a professional force, and historically collected delinquency fines into local treasuries. In exchange they surrender the option of general disarmament and must tolerate an armed populace they do not fully command; their regulatory discretion is bounded by the requirement that regulation preserve the trained civic capacity rather than dismantle it.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, state_governments, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_arms_right__civic_republican_reading, state_governments, payer).

% Draft and administer the implementing statutes: organizing, arming, and disciplining the militia, setting qualification standards, apportioning arms, and enforcing or waiving muster obligations. They set the terms under which the civic right-and-duty operates and can tighten or loosen qualification regimes; their exit is institutional reshaping of the rules rather than departure from the system.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, federal_militia_authorities, agenda_setter,
    institutional, generational, mobile, national).

% Bear the demands of an armed civic order they did not consent to: compulsory muster liability, fines for nonappearance, and social marking as deficient citizens. Because the reading fuses arms-bearing with full civic standing, there is no way for them to be full members without violating conscience; exit means emigration or accepting permanent second-class civic status.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, conscientious_objectors, payer,
    powerless, biographical, trapped, national).

% Liable for purchasing prescribed arms and equipment and attending mustes they often could not equip themselves for; failure brought fines and forfeiture, while wealthier neighbors bought commutations or paid substitutes. The duty's cost fell inversely to ability to pay, and the promised protection against disarmament was nominal for men who could not acquire the qualifying arms in the first place.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, indigent_militia_delinquents, payer,
    powerless, biographical, trapped, national).

% Adjudicate what the armed-citizenship guarantee permits: how far qualification and training regimes may go before they destroy the protected capacity, whether the duty side remains enforceable, and which historical era's militia practice governs. They see the full structure across centuries and across rival readings of the same text.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, judicial_interpreters, observer,
    institutional, generational, analytical, national).

% Argue that reliable defense requires professional standing forces and that distributing arms among amateur citizens yields Shays-style disorder rather than security. The anti-standing-army premise of the civic-republican framework excludes their solution from the constitutional conversation, so they press their case through ordinary statute-making and budget politics instead.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, professional_army_advocates, excluded,
    powerful, biographical, arbitrage, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_arms_right__civic_republican_reading, state_governments).
narrative_ontology:fixing_cost_class(second_amendment_arms_right__civic_republican_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides common defense and insures against governmental monopoly of force by distributing military capacity across a trained citizen body. It solves the standing-army problem: a republic secures itself without creating the professional army that has dominated or toppled civil governments elsewhere, and it maintains a civic boundary in which full membership entails both the liberty and the obligation of arms.
% TRANSFER_FUNCTION: Moves defense-readiness costs — equipment purchase, training time, muster attendance, historically delinquency fines — from the public treasury onto individual citizens; moves disarmament discretion away from governments, which may not dismantle the armed civic body; and historically moved fine revenue from delinquent militiamen into local treasuries.
% ABSENT_VOICES: Conscientious objectors and the unpropertied had no seat at the drafting or ratifying tables; enslaved and disenfranchised people lived under the armed civic order while excluded from both its protections and its deliberations; professional-army advocates were excluded by the framework's anti-standing-army premise and routed their case into ordinary legislation. Unanimity in the founding record partly reflects who was never in the room.
% DISAPPEARANCE_RATIONALE: If the armed-citizenship guarantee vanished overnight, governments would regain the option of general disarmament or of monopolizing force in professional hands; defense arrangements would reorganize around standing institutions; the civic status of arms-bearing would dissolve into either private hobby or licensed privilege; and qualification regimes would lose their constitutional anchor and become ordinary police-power regulation.
% FOUNDING_PROBLEM: How does a republic defend itself without creating the standing army that history showed destroys republics — answered at the founding by making the citizen body itself the repository of military capacity, armed, trained, and mustered under law.
% FOUNDING_PROBLEM_CORROBORATION: Anti-Federalist essays (Brutus, Federal Farmer), state ratifying-convention proposed amendments, and the early militia statutes attest the standing-army problem from outside any current benefiting party, and historians of the early republic corroborate the genealogy. No institution outside the reading's adherents attests that the problem remains live in its original form: a professional all-volunteer force now exists, and the parties dispute whether that fact confirms the danger's persistence or dissolves it.
narrative_ontology:disappearance_verdict(second_amendment_arms_right__civic_republican_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_arms_right__civic_republican_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__civic_republican_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_arms_right__civic_republican_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_arms_right__civic_republican_reading, 0.45, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__civic_republican_reading_tests).
:- end_tests(second_amendment_arms_right__civic_republican_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45 at interval end) because the arrangement's costs are real but largely returned to the payers as right and status: equipment mandates, training time, and historical muster fines fall on citizens who simultaneously hold the shield against disarmament. Suppression (0.38) is predominantly STRUCTURAL — legal liability, fines, exclusion from civic standing — rather than internalized belief; the historical enforcement machinery (compulsory musters backed by penalty) is what the suppression_requirement series tracks. Theater ratio (0.42) reflects the mid-interval decay of the militia system into ceremony (peak 0.71 circa 1986) followed by partial revival of substantive training content under civic-republican advocacy. Accessibility collapse is moderate (0.5): alternative defense arrangements — standing professional forces, licensed-privilege regimes — remain conceivable and were in fact adopted; the reading forecloses framings more than it forecloses arrangements. Resistance is high (0.62): this reading is squeezed from both flanks, attacked by individual-right advocates as too weak and by regulatory advocates as an obstacle, and displaced from controlling doctrine after 2008. The victim seats deserve the coalition check: conscientious objectors and indigent delinquents are each powerless, but coalition formation between them is historically thin because the civic boundary that defines the beneficiary set also defines them out of full membership — their common interest existed but found no organizational vehicle, and anti-militia resistance (anti-conscription agitation, muster evasion) surfaced episodically and was defeated. All three metric series run on one shared nine-point grid (1792-2026) so the engine samples every metric at every examined time point; the arc is rise-decay-partial-revival, not monotonic drift.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute materially different types from identical structural data. From the citizen_militia_member seat the arrangement is near-symmetric coordination: a right purchased with duties, worth maintaining. From the conscientious_objector and indigent_delinquent seats the same structure operates as enforced extraction with no offsetting benefit — a snare-flavored experience. From the state_government seat it is a subsidy: defense capacity acquired at citizen expense, with the lost disarmament option a cost states accepted grudgingly and periodically litigate. From the federal_militia_authority seat it is an administrable program whose dials (qualification thresholds, muster frequency) are adjustable. The engine derives these divergences from power, exit, and directional position; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation: citizen_militia_members and state_governments sit toward the beneficiary end; conscientious_objectors and indigent_militia_delinquents sit toward the target end with trapped exits amplifying effective extraction. One override is authored: power_atom 'organized' -> d 0.4. The derivation from the beneficiary declaration alone would push citizen_militia_members toward the full-beneficiary end (d roughly 0.1-0.2), but the dual right-plus-duty structure means they also bear equipment costs, training time, and historical fines — a near-symmetric position. In this story 'organized' maps only to citizen_militia_members, so the override is effectively per-agent; the commentary records why the structural derivation alone misplaces them. Identity-lock dynamics: the exit is identity_locked because the fusion is constitutive, not contractual — in this reading civic standing IS arms-bearing capacity, an ideological-institutional fusion rather than mere career or relational dependence. If the frame broke (civic membership decoupled from arms), the constraint would migrate toward either the individual-right sibling (liberty without duty) or ordinary police-power regulation, and the dual-beneficiary structure would dissolve.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding mandate — citizen muster as the republic's defense — genuinely atrophied: by the early twentieth century the duty half was largely dead, musters were social ceremony, and the theater series peaked above 0.7 while function collapsed. That is piton-flavored drift, and a naive reading of the mid-interval data would classify the arrangement as inertial performance. Two things prevent full mandatrophy resolution: the revival pressure of civic-republican theory and state-level qualification programs restored substantive content after 2008, and the constraint's OTHER function — denying governments the disarmament option — never lapsed. The classification prevents symmetric mislabeling: calling this a pure rope ignores the documented extraction borne by objectors and the indigent (fines, forfeitures, commutation inequity); calling it a snare ignores the genuine dual-benefit core and the fact that the arrangement's chief operation runs AGAINST state power rather than extracting for it. On the R5 mismatch consumer: founding_problem_status 'contested' crossed with disappearance_verdict 'world_rearranges' raises no zombie flag — the arrangement's persistence tracks a live-but-transformed problem, not a dead mandate kept alive by inertia alone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the second_amendment_arms_right kernel; would instantiating the individual_right_reading or the collective_right_reading change the beneficiary and victim sets so completely that no metric value carries over between stories?',
    'Author the two sibling stories and compare computed classifications; divergence in party sets and epsilon locates the disagreement structurally rather than rhetorically.',
    'If sibling readings produce disjoint party sets, cross-reading metric comparisons are void and the kernel''s overall classification is tri-valued rather than singular; if they converge, the kernel admits a stable classification despite the doctrinal fight.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: this story is one of three readings of a fixed-text kernel; sibling instantiations are separate constraints.').

omega_variable(
    civic_boundary_membership_question,
    'Who counts as the armed citizen body whose right this reading protects — and therefore who sits inside the beneficiary set versus merely under the armed order?',
    'Trace how qualification and disqualification rules drew the civic boundary across eras (property, race, age, felony) and test whether exclusions track civic-capacity judgments or group subordination.',
    'A narrow civic boundary converts nominal beneficiaries into victims and raises effective extraction sharply; a broad boundary approaches universal coverage and pulls the arrangement toward pure-coordination territory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civic_boundary_membership_question, conceptual, 'The civic boundary determines both the beneficiary set and the victim set; its drawing is the reading''s deepest unresolved parameter.').

omega_variable(
    duty_component_operativity,
    'Does the duty half of the dual structure — training, muster, qualification — remain operative anywhere in the standing arrangement, or has the reading become right-only in practice with the duty surviving as rhetoric?',
    'Survey current state militia codes, qualification mandates, and enforcement records; measure actual compulsion versus voluntary participation and whether delinquency carries any sanction.',
    'If the duty is dead, the arrangement extracts little but also coordinates little, drifting toward inertial-performance dynamics; if alive, the moderate-extraction profile and the tangled classification are accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(duty_component_operativity, empirical, 'Operativity of the duty half decides whether the dual-beneficiary structure is real or vestigial.').

omega_variable(
    qualification_cost_classification,
    'Are training and qualification costs inherent coordination cost of maintaining a competent civic body, or extractive overhead levied under cover of competence?',
    'Compare qualification-regime costs against measured competency outcomes and against the identity_coordination coordination floor; examine who designs the regimes and who pays them.',
    'An inherent-cost finding supports the coordination-side reading of the metrics; an overhead finding pushes effective extraction upward and the classification toward snare-flavored asymmetry.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(qualification_cost_classification, empirical, 'Whether the moderate epsilon sits inside or outside the inherent cost of civic competence maintenance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__civic_republican_reading, 1789, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1792, second_amendment_arms_right__civic_republican_reading, theater_ratio, 1792, 0.18).
narrative_ontology:measurement(seco_tr_t1820, second_amendment_arms_right__civic_republican_reading, theater_ratio, 1820, 0.28).
narrative_ontology:measurement(seco_tr_t1860, second_amendment_arms_right__civic_republican_reading, theater_ratio, 1860, 0.44).
narrative_ontology:measurement(seco_tr_t1903, second_amendment_arms_right__civic_republican_reading, theater_ratio, 1903, 0.62).
narrative_ontology:measurement(seco_tr_t1934, second_amendment_arms_right__civic_republican_reading, theater_ratio, 1934, 0.66).
narrative_ontology:measurement(seco_tr_t1968, second_amendment_arms_right__civic_republican_reading, theater_ratio, 1968, 0.69).
narrative_ontology:measurement(seco_tr_t1986, second_amendment_arms_right__civic_republican_reading, theater_ratio, 1986, 0.71).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_arms_right__civic_republican_reading, theater_ratio, 2008, 0.55).
narrative_ontology:measurement(seco_tr_t2026, second_amendment_arms_right__civic_republican_reading, theater_ratio, 2026, 0.42).

% Extraction over time
narrative_ontology:measurement(seco_be_t1792, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 1792, 0.58).
narrative_ontology:measurement(seco_be_t1820, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 1820, 0.52).
narrative_ontology:measurement(seco_be_t1860, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 1860, 0.46).
narrative_ontology:measurement(seco_be_t1903, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 1903, 0.34).
narrative_ontology:measurement(seco_be_t1934, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 1934, 0.3).
narrative_ontology:measurement(seco_be_t1968, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 1968, 0.27).
narrative_ontology:measurement(seco_be_t1986, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 1986, 0.25).
narrative_ontology:measurement(seco_be_t2008, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 2008, 0.36).
narrative_ontology:measurement(seco_be_t2026, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 2026, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1792, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 1792, 0.52).
narrative_ontology:measurement(seco_su_t1820, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 1820, 0.47).
narrative_ontology:measurement(seco_su_t1860, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 1860, 0.38).
narrative_ontology:measurement(seco_su_t1903, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 1903, 0.22).
narrative_ontology:measurement(seco_su_t1934, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 1934, 0.19).
narrative_ontology:measurement(seco_su_t1968, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 1968, 0.16).
narrative_ontology:measurement(seco_su_t1986, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 1986, 0.14).
narrative_ontology:measurement(seco_su_t2008, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 2008, 0.26).
narrative_ontology:measurement(seco_su_t2026, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 2026, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__civic_republican_reading, identity_coordination).
narrative_ontology:affects_constraint(second_amendment_arms_right__civic_republican_reading, second_amendment_arms_right__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__civic_republican_reading, second_amendment_arms_right__collective_right_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the Second Amendment kernel per the epsilon-invariance principle: the colloquial label 'the Second Amendment right' covers three structurally distinct claims that cannot share one story because their holder sets, victim sets, and epsilon values differ. The individual-right sibling currently controls doctrine (post-2008) and exerts downstream pressure on this reading's legitimacy conditions; the collective-right sibling was the pre-2008 judicial mainstream and shares this reading's militia-context premise while differing on the holder. This reading historically mediated between the siblings — cited by courts as the scholarly middle ground — which is why the family edges run through it. Each file authors its own epsilon, beneficiaries, victims, and claimed type; no metric is averaged across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_arms_right__civic_republican_reading, organized, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
