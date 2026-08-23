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
 *   constraint_id: second_amendment_arms_right__civic_republican_reading
 *   human_readable: Armed Citizenship Guarantee — Civic Republican Reading of the Second Amendment
 *   domain: constitutional law / political philosophy / legal interpretation
 *
 * SUMMARY:
 *   This story instantiates ONE reading of a contested constitutional kernel.
 *   The kernel is the Second Amendment's arms provision; this reading holds
 *   that it protects armed citizenship — the capacity and standing of
 *   citizens to bear arms as constituents of a self-governing polity — and is
 *   therefore neither a purely individual liberty nor a state institutional
 *   prerogative. The standing arrangement under contest, which is epsilon's
 *   referent throughout: a constitutional guarantee that conditions
 *   permissible regulation on militia-fitness rationales, paired with civic
 *   duties (training, qualification, readiness) that fall on the
 *   citizen-body, with compulsory enforcement historically and hortatory
 *   enforcement now. The reading's own endorsed alternative — a revitalized,
 *   universally practiced well-regulated militia — is NOT the referent;
 *   extraction is assessed on the arrangement as it stands. Sibling readings
 *   (individual_right_reading, collective_right_reading) are separate
 *   constraint stories linked via network.affects_constraints; their
 *   structural deltas are recorded in the kernel_reading_indexation omega
 *   rather than hedged into this file's metrics.
 *
 * KEY AGENTS:
 *   - citizen_militia_members: Dual-positioned core (moderate/constrained) — holds the protection and bears the readiness burden; the reading's defining seat
 *   - abstaining_citizens: Free-riding beneficiaries (moderate/mobile) — enjoy the insurance externality, pay nothing, exit costlessly
 *   - republican_self_governing_communities: Local beneficiaries (organized/constrained) — retain distributed response capacity
 *   - arms_training_institutions: Commercial beneficiaries (organized/arbitrage) — capture the mandated-spending flow
 *   - conscientious_objectors: Pure payers (powerless/trapped) — conviction forbids the conduct the civic body expects
 *   - civically_disqualified_persons: Pure payers (powerless/trapped) — stripped of the protection by the civic-standing conditionality
 *   - professional_defense_establishment: Institutional payers (institutional/constrained) — bear the capped force-monopoly
 *   - state_legislatures: Agenda-setters (institutional/arbitrage) — write the training and qualification regime
 *   - constitutional_adjudicators: Agenda-setters (institutional/constrained) — draw the regulation/disarmament line
 *   - price_excluded_citizens: Excluded (powerless/trapped) — priced out of the civic body, unrepresented
 *   - republican_theory_scholars: Analytical observers (analytical/analytical) — trace lineage and drift
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__civic_republican_reading, 0.57).
domain_priors:suppression_score(second_amendment_arms_right__civic_republican_reading, 0.42).
domain_priors:theater_ratio(second_amendment_arms_right__civic_republican_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, extractiveness, 0.57).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__civic_republican_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_arms_right__civic_republican_reading, "Armed Citizenship Guarantee — Civic Republican Reading of the Second Amendment").
narrative_ontology:topic_domain(second_amendment_arms_right__civic_republican_reading, "constitutional law / political philosophy / legal interpretation").

domain_priors:requires_active_enforcement(second_amendment_arms_right__civic_republican_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__civic_republican_reading, '304c4639-b229-4552-b9c9-e5eef83e9856').
narrative_ontology:cs_kernel_codification('304c4639-b229-4552-b9c9-e5eef83e9856', fixed_text).
narrative_ontology:cs_authority_grounding('304c4639-b229-4552-b9c9-e5eef83e9856', lineage).
narrative_ontology:cs_interpretation_layer_present('304c4639-b229-4552-b9c9-e5eef83e9856').
narrative_ontology:cs_reading_relation('304c4639-b229-4552-b9c9-e5eef83e9856', second_amendment_arms_right__collective_right_reading, forecloses).
narrative_ontology:cs_reading_relation('304c4639-b229-4552-b9c9-e5eef83e9856', second_amendment_arms_right__individual_right_reading, coexists_with).
narrative_ontology:cs_axiom('304c4639-b229-4552-b9c9-e5eef83e9856', foundational, armed_citizenship_constitutes_self_governance).
narrative_ontology:cs_axiom_status(armed_citizenship_constitutes_self_governance, holdable).
narrative_ontology:cs_axiom_grounding('304c4639-b229-4552-b9c9-e5eef83e9856', armed_citizenship_constitutes_self_governance, deontological).
narrative_ontology:cs_axiom('304c4639-b229-4552-b9c9-e5eef83e9856', secondary, citizen_force_checks_standing_power).
narrative_ontology:cs_axiom_status(citizen_force_checks_standing_power, holdable).
narrative_ontology:cs_axiom_grounding('304c4639-b229-4552-b9c9-e5eef83e9856', citizen_force_checks_standing_power, empirically_contingent).
narrative_ontology:cs_reference_frame('304c4639-b229-4552-b9c9-e5eef83e9856', civic_body_as_well_regulated_militia).
narrative_ontology:cs_drift_state('304c4639-b229-4552-b9c9-e5eef83e9856', contemporary_mass_standing_forces_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('304c4639-b229-4552-b9c9-e5eef83e9856', '2026-06-12T09:30:00Z').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__civic_republican_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__civic_republican_reading, citizen_militia_members).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__civic_republican_reading, abstaining_citizens).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__civic_republican_reading, republican_self_governing_communities).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__civic_republican_reading, arms_training_institutions).
narrative_ontology:constraint_victim(second_amendment_arms_right__civic_republican_reading, conscientious_objectors).
narrative_ontology:constraint_victim(second_amendment_arms_right__civic_republican_reading, civically_disqualified_persons).
narrative_ontology:constraint_victim(second_amendment_arms_right__civic_republican_reading, professional_defense_establishment).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(second_amendment_arms_right__civic_republican_reading, citizen_militia_members).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__civic_republican_reading, anti_standing_army_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__civic_republican_reading, civic_virtue_through_arms_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the constitutional protection that conditions what weapons regulation may reach them, and correspondingly owe the civic duties the reading attaches to armed citizenship: maintaining equipment, keeping proficiency, answering calls to serve. Historically these duties were compulsory with fines for missed musters; today they survive as hortatory expectation and voluntary training. They cannot exit citizenship, and declining participation carries a civic-standing cost even where no legal penalty remains.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, citizen_militia_members, beneficiary,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_arms_right__civic_republican_reading, citizen_militia_members, payer).

% Decline to keep, train with, or identify with arms, and face no legal penalty for abstention in the contemporary arrangement. They enjoy whatever deterrent or insurance value the distributed capacity provides without paying any of the equipment, time, or risk costs. Abstention itself is their exit, and it is effectively free.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, abstaining_citizens, beneficiary,
    moderate, biographical, mobile, national).

% Localities and state-level bodies that rely on resident capacity for emergency response and internal order, host training infrastructure, and historically administered the muster system. They prefer a defense posture not solely dependent on distant professional institutions, and they absorb part of the arrangement's benefit as locally retained response capacity.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, republican_self_governing_communities, beneficiary,
    organized, generational, constrained, regional).

% Instructors, ranges, qualification providers, and affiliated manufacturers who sell the instruction, facilities, and equipment the arrangement's readiness expectations call forth. Their revenue scales with the breadth of training and qualification expectations, and they advocate curricula and standards that widen demand. Because they serve sporting and security markets too, they can redirect effort if the mandate-side demand shrinks.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, arms_training_institutions, beneficiary,
    organized, immediate, arbitrage, national).

% Hold convictions that forbid bearing arms. Under the compulsory era they faced muster fines and imprisonment; under civic-standing conditionality they face second-class recognition as incomplete citizens. No lawful path within the polity reconciles compliance with conviction short of emigration, and emigration abandons everything else in their lives.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, conscientious_objectors, payer,
    powerless, biographical, trapped, national).

% Persons judged outside full civic membership — by felony conviction, historical racial exclusion, or other status lines — lose the protection precisely because the arrangement ties it to civic standing, while remaining subject to its coercions. Historically this seat included enslaved and disenfranchised people disarmed by the same militia statutes that demanded armed service of members. Discharge from the disqualified class is slow, discretionary, and frequently unavailable.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, civically_disqualified_persons, payer,
    powerless, biographical, trapped, national).

% Career military, police, and allied institutions that absorb the defense and order functions the citizen-body otherwise would. The arrangement caps their consolidation: militia clauses and the civic-participation norm deny them exclusive domestic force jurisdiction. They bear the opportunity cost of the forgone monopoly while retaining vast budgets, and they encounter the arrangement chiefly as a legal boundary condition on planning rather than a daily burden.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, professional_defense_establishment, payer,
    institutional, generational, constrained, national).

% Enact militia codes, training mandates, and qualification standards, and historically ran the muster and fine system directly. Today they calibrate regulation inside the bounds the courts draw, and federalism lets them vary intensity — shifting restrictive or permissive settings without exiting the system.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, state_legislatures, agenda_setter,
    institutional, biographical, arbitrage, national).

% Courts draw the operative line between militia-fitness regulation and disarmament, striking laws that cross it. Their interpretive choices define the reading's working content, and precedent binds their own movement — they administer a boundary they did not choose and cannot lightly redraw.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, constitutional_adjudicators, agenda_setter,
    institutional, generational, constrained, national).

% Would participate in principle but cannot afford arms, ammunition, range time, and instruction. The arrangement's civic conditionality marks them as falling short of full citizenship while no forum represents their specific situation — advocacy organs speak for holders, prohibition movements speak against arms altogether, and the priced-out are addressed by neither.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, price_excluded_citizens, excluded,
    powerless, biographical, trapped, national).

% Trace the reading's lineage from classical civic-republican thought through ratification-era debate to present jurisprudence, assess its internal coherence, and document its drift. They collect nothing from the arrangement and bear none of its burdens.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, republican_theory_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_arms_right__civic_republican_reading, arms_training_institutions).
narrative_ontology:fixing_cost_class(second_amendment_arms_right__civic_republican_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a distributed, trained, and equipped body of citizens capable of collective defense and of checking centralized professional force — solving the free-rider problem that individual citizens will not spontaneously sustain proficiency and equipment, without creating the standing professional force the founding generation treated as the classic instrument of tyranny.
% TRANSFER_FUNCTION: Transfers time, labor, and equipment costs from militia-eligible citizens into the collective defense capacity; transfers security-provision responsibility away from exclusive professional institutions toward the civic body; and transfers civic standing itself, granted or withheld according to participation in armed citizenship.
% ABSENT_VOICES: Price-excluded citizens — willing but unable to afford participation — have no organized seat and are addressed by neither the protection's defenders nor its opponents. Historically the enslaved and disenfranchised sat outside every deliberation that wrote militia statutes binding them. Conscientious objectors now hold a payer seat but held none when the compulsory apparatus was designed.
% DISAPPEARANCE_RATIONALE: If the protection and its conditioning apparatus vanished overnight, legislatures could prohibit possession outright, the legal status of tens of millions of privately held arms would flip, the training and qualification economy would collapse, civic-identity organizations would lose their constitutive constitutional claim, and the federal division of militia powers would rebalance toward centralized professional control.
% FOUNDING_PROBLEM: The new republic needed collective defense and internal order but regarded the standing professional army as the historical instrument by which English liberty had been overturned. The founding problem was how to secure defense capacity without creating that standing force — answered by a citizenry that armed and trained itself under civic obligation.
% FOUNDING_PROBLEM_CORROBORATION: The original problem is corroborated from outside any benefiting party by ratification-era state convention debates and anti-Federalist writing, which pressed the standing-army fear explicitly. That the defense-gap problem is now operationally dead is corroborated by the existence of the mass standing military the arrangement coexists with, and by military historians documenting the militia system's functional supersession. The civic-norm half — that armed citizenship remains a necessary feature of self-governance — is attested as live only by the reading's adherents; no source outside the dispute attests it, and the dormancy of the muster system is documentary corroboration against it.
narrative_ontology:disappearance_verdict(second_amendment_arms_right__civic_republican_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_arms_right__civic_republican_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__civic_republican_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_arms_right__civic_republican_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_arms_right__civic_republican_reading, 0.57, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__civic_republican_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_arms_right__civic_republican_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_arms_right__civic_republican_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is moderate (0.57) per the reading's own structural delta: the protection side delivers genuine value to its holders, while training and qualification expectations impose real, unevenly distributed burdens — compulsory in the founding era with fines, monetized and voluntary today. Suppression (0.42, unscaled raw structural property) reflects the residue of the compulsory apparatus plus civic-standing conditionality; only extractiveness is scaled by directionality and scope downstream, and the commentary treats suppression accordingly. Theater_ratio (0.44) sits just under the substitution threshold: substantive training survives (organized marksmanship programs, state defense forces, qualification courses) but a large share of 'well-regulated militia' invocation is rhetorical maintenance of a muster system that no longer operates. Accessibility_collapse is low (0.30): the professional-defense alternative did not merely survive, it became the dominant security provider — the arrangement coexists with its replacement rather than foreclosing it. Resistance (0.60) is substantial and persistent: gun-control politics contests the protection's reach, conscientious objectors historically absorbed fines and imprisonment rather than comply, and the scholarly field disputes the reading's coherence. The temporal series run on one shared seven-point grid (t=0,40,80,120,160,200,240, roughly 1789-2029) with all three metrics authored at every point; the suppression_requirement series is authored deliberately because this arrangement's enforcement machinery visibly transformed — muster fines built up and decayed, were perversely weaponized in the post-Civil War disarmament campaigns, were repealed, and were replaced by judicial enforcement infrastructure — which is enforcement-capacity change, not merely extraction drift. The 1869 spike in all three series records that weaponization: extraction and suppression rose together as hollowed militia statutes were turned to disarming the newly freed, and theater rose as the statutes' defensive function had already emptied while their coercive function intensified. Endpoint values equal the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   The seats should classify differently and the engine computes that divergence from the structural data. From the agenda-setter seats (courts, legislatures) the arrangement presents as boundary management — a line to administer between fitness regulation and disarmament. From the dual seat (citizen_militia_members) it presents as an entitlement with fair-share dues. From the pure payer seats it splits again: conscientious objectors and civically disqualified persons experience compulsion and exclusion, while the professional defense establishment experiences merely a planning constraint, softened by its institutional power and budgetary insulation. Abstaining citizens experience the mildest version — free insurance — because their exit is costless. Same nominal polity, radically different constraint depending on seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive d toward zero and victim declarations toward one, with exit options modulating. Abstaining_citizens sit nearest the beneficiary pole: full benefit, mobile exit, no burden. Arms_training_institutions likewise derive low d — and they are additionally the seat that materially RECEIVES the arrangement's mandated spending (recorded on the receipt surface), though receipt-of-payment-for-services is distinct from their low structural extraction exposure. Republican_self_governing_communities derive low-moderate d. Citizen_militia_members are the deliberate dual case: declared beneficiary (the protection) with secondary payer position (the burdens); their net d lands mid-low rather than at either pole, and no override is authored because the derivation from dual declaration plus constrained exit already captures it. Among payers, conscientious_objectors and civically_disqualified_persons derive near-full-target d — trapped exit amplifies their exposure, and their powerlessness removes damping. Professional_defense_establishment is a victim by declaration but its institutional power and the indirect character of its cost (forgone monopoly rather than extracted resources) temper effective extraction below what a trapped powerless payer would show. The agenda-setter seats derive near-symmetric administrative positions. No directionality_overrides are authored: the derivation chain from the declared structure produces the correct relationships, and the commentary records the qualitative picture instead.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem had two halves with divergent statuses. The operational half — defense without a standing army — is dead: the mass professional military and police exist, universal musters ended, and the compulsory apparatus was formally dismantled a century ago; the arrangement's original mandate demonstrably outlived that function. The normative half — armed citizenship as constitutive of self-governance and insurance against usurpation — is asserted as live by the reading's adherents and dismissed as obsolete by opponents; the honest status is contested, and no corroborating source outside the dispute attests liveness. The classification discipline prevents both symmetrical errors: reading the arrangement as a pure snare ignores the genuine coordination function (distributed capacity, anti-tyranny insurance, the free-rider problem it solves), while reading it as a pure rope ignores the asymmetric extraction (compulsion of dissenters, civic-standing conditionality that manufactures a disqualified class, burdens borne by participants for benefits enjoyed diffusely). The receipt surface records that the mandated spending concentrates commercially even while the deterrent benefit diffuses — capture of receipts without a single capturer of the arrangement's purpose. On the R5 mismatch wiring, this story lands in contested-status times world-rearranges, not the dead-times-rearranges zombie cell; the compulsory-muster mandate specifically is resolved, and the residual contest is recorded rather than papered over.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexation,
    'This constraint is one reading (civic_republican_reading) of the kernel second_amendment_arms_right — what structurally changes under the sibling readings, and where exactly is the disagreement located?',
    'Comparative structural analysis across the sibling story files: individual_right_reading relocates the bearer to the pre-political individual (duty-side burdens vanish; regulation-facing victims emerge instead), while collective_right_reading relocates the bearer to the state militia institution (citizens become instruments rather than right-holders, raising extraction on citizen autonomy). The disagreement is located in the determination of the right''s BEARER — civic member versus isolated individual versus state institution — and that single element determines the entire beneficiary/victim topology of each reading.',
    'If a sibling reading were adopted as the operative constraint, the beneficiary set, victim set, and epsilon profile all change: the individual reading removes mandate-side extraction but creates regulation-side victims; the collective reading removes the citizen-beneficiary entirely. Cross-reading epsilon comparisons over the shared kernel text are invalid — each reading is its own constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indexation, conceptual, 'Committer structure: one of three readings of the Second Amendment kernel; bearer-determination is the pivot on which the readings diverge.').

omega_variable(
    civic_norm_liveness,
    'Does armed citizenship remain a live constitutive civic practice under this reading, or does the arrangement persist as inherited form maintained rhetorically?',
    'Participation and organizational vitality data: marksmanship program enrollment trajectories, state defense force strength, organized qualification-course throughput, and survey data on self-understood civic duty to maintain readiness. Declining substantive participation alongside stable rhetorical invocation indicates inherited-form persistence.',
    'If the practice is dead-form, the constraint''s remaining function is mostly theatrical maintenance and the classification drifts toward inertial persistence; if live, the coordination function is genuine and the hybrid classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civic_norm_liveness, conceptual, 'Whether the civic-participation substrate of the arrangement is operative or vestigial.').

omega_variable(
    deterrent_thesis_efficacy,
    'Does distributed citizen force actually deliver the anti-tyranny insurance and security capacity the coordination story promises?',
    'Comparative historical and institutional analysis: outcomes in periods and jurisdictions relying on citizen capacity versus professional monopoly, case studies of attempted usurpation, and criminological evidence on armed-civilian effects on public safety.',
    'If the deterrent and capacity claims fail empirically, the coordination story loses its load-bearing premise and a larger share of the measured burden counts as uncompensated extraction, pushing the classification toward the extractive pole.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrent_thesis_efficacy, empirical, 'Empirical standing of the citizen-force deterrent and capacity thesis.').

omega_variable(
    civic_inclusion_boundary,
    'Who counts as the civic body entitled to the protection and bound by its duties? The victim set tracks whatever boundary is drawn.',
    'Normative and legal analysis of membership criteria: the boundary has been drawn by race, sex, felony status, and disability across the arrangement''s history, and each redrawing relocates who bears conditional-civic costs. No empirical test settles a membership criterion; the choice rests on commitments about political membership.',
    'Each boundary choice changes the victim set and therefore the measured extraction: narrower boundaries concentrate conditional-civic harm on excluded classes; wider boundaries extend duty-burdens to populations likely to experience them as pure imposition (for example, committed pacifists enrolled involuntarily).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civic_inclusion_boundary, preference, 'Membership-boundary contingency determining who is inside the protection and who bears its conditionality.').

omega_variable(
    suppression_mechanism_split,
    'How much of the measured suppression is structural (statutory penalties, historic muster fines, conditional civic standing) versus internalized (stigma attaching to nonbearing, self-imposed second-class citizenship)?',
    'Post-statute attitude persistence studies: in jurisdictions and cohorts where compulsory-militia penalties were repealed generations ago, measure whether avoidance of nonparticipation still carries felt civic shame; compare cohorts socialized before and after repeal.',
    'If a large share is internalized, effective suppression exceeds the structural statutory measure — targets carry the pressure after the legal machinery is dismantled — and the arrangement''s coercive footprint is understated by statute-count alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_split, empirical, 'Structural versus internalized components of the arrangement''s suppressive force.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__civic_republican_reading, 0, 240).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sa_civic_republican_tr_t0, second_amendment_arms_right__civic_republican_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(sa_civic_republican_tr_t0, observed).
narrative_ontology:measurement(sa_civic_republican_tr_t40, second_amendment_arms_right__civic_republican_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement_basis(sa_civic_republican_tr_t40, observed).
narrative_ontology:measurement(sa_civic_republican_tr_t80, second_amendment_arms_right__civic_republican_reading, theater_ratio, 80, 0.38).
narrative_ontology:measurement_basis(sa_civic_republican_tr_t80, observed).
narrative_ontology:measurement(sa_civic_republican_tr_t120, second_amendment_arms_right__civic_republican_reading, theater_ratio, 120, 0.25).
narrative_ontology:measurement_basis(sa_civic_republican_tr_t120, observed).
narrative_ontology:measurement(sa_civic_republican_tr_t160, second_amendment_arms_right__civic_republican_reading, theater_ratio, 160, 0.35).
narrative_ontology:measurement_basis(sa_civic_republican_tr_t160, observed).
narrative_ontology:measurement(sa_civic_republican_tr_t200, second_amendment_arms_right__civic_republican_reading, theater_ratio, 200, 0.45).
narrative_ontology:measurement_basis(sa_civic_republican_tr_t200, observed).
narrative_ontology:measurement(sa_civic_republican_tr_t240, second_amendment_arms_right__civic_republican_reading, theater_ratio, 240, 0.44).
narrative_ontology:measurement_basis(sa_civic_republican_tr_t240, projected).

% Extraction over time
narrative_ontology:measurement(sa_civic_republican_be_t0, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement_basis(sa_civic_republican_be_t0, observed).
narrative_ontology:measurement(sa_civic_republican_be_t40, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement_basis(sa_civic_republican_be_t40, observed).
narrative_ontology:measurement(sa_civic_republican_be_t80, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 80, 0.63).
narrative_ontology:measurement_basis(sa_civic_republican_be_t80, observed).
narrative_ontology:measurement(sa_civic_republican_be_t120, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 120, 0.46).
narrative_ontology:measurement_basis(sa_civic_republican_be_t120, observed).
narrative_ontology:measurement(sa_civic_republican_be_t160, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 160, 0.5).
narrative_ontology:measurement_basis(sa_civic_republican_be_t160, observed).
narrative_ontology:measurement(sa_civic_republican_be_t200, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 200, 0.54).
narrative_ontology:measurement_basis(sa_civic_republican_be_t200, observed).
narrative_ontology:measurement(sa_civic_republican_be_t240, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 240, 0.57).
narrative_ontology:measurement_basis(sa_civic_republican_be_t240, projected).

% Suppression requirement over time
narrative_ontology:measurement(sa_civic_republican_su_t0, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(sa_civic_republican_su_t0, observed).
narrative_ontology:measurement(sa_civic_republican_su_t40, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 40, 0.48).
narrative_ontology:measurement_basis(sa_civic_republican_su_t40, observed).
narrative_ontology:measurement(sa_civic_republican_su_t80, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 80, 0.62).
narrative_ontology:measurement_basis(sa_civic_republican_su_t80, observed).
narrative_ontology:measurement(sa_civic_republican_su_t120, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 120, 0.35).
narrative_ontology:measurement_basis(sa_civic_republican_su_t120, observed).
narrative_ontology:measurement(sa_civic_republican_su_t160, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 160, 0.33).
narrative_ontology:measurement_basis(sa_civic_republican_su_t160, observed).
narrative_ontology:measurement(sa_civic_republican_su_t200, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 200, 0.4).
narrative_ontology:measurement_basis(sa_civic_republican_su_t200, observed).
narrative_ontology:measurement(sa_civic_republican_su_t240, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 240, 0.42).
narrative_ontology:measurement_basis(sa_civic_republican_su_t240, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__civic_republican_reading, identity_coordination).
narrative_ontology:affects_constraint(second_amendment_arms_right__civic_republican_reading, individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__civic_republican_reading, collective_right_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the kernel second_amendment_arms_right: the colloquial label 'the Second Amendment right' covers three structurally distinct arrangements with distinct epsilon values, beneficiary sets, and failure modes. This file authors the civic_republican_reading only (bearer: the citizen-body as militia; dual right-plus-duty structure; moderate extraction concentrated in training/qualification burdens). The individual_right_reading (bearer: pre-political individual) eliminates duty-side extraction but generates regulation-facing victims; the collective_right_reading (bearer: state militia institution) converts citizens from right-holders into instruments. The readings are separate stories linked here; epsilon is invariant within each and incomparable across them. Jurisprudential ascendancy currently runs individual-ward, making this reading and the collective reading downstream of shifting interpretive conditions rather than of each other's truth.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
