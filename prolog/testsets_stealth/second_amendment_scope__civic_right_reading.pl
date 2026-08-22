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
 *   constraint_id: second_amendment_scope__civic_right_reading
 *   human_readable: Second Amendment Civic-Militia Conditioned Right (Civic Right Reading)
 *   domain: constitutional law/political theory/rights jurisprudence
 *
 * SUMMARY:
 *   This story instantiates the civic right reading of the Second Amendment:
 *   an individual right to keep and bear arms that exists only through, and
 *   is measured by, service in a well-regulated militia. The arrangement
 *   modeled is the founding-era settlement as it actually operated —
 *   compulsory enrollment of able-bodied citizens, privately supplied arms to
 *   a prescribed standard, periodic musters, fine-backed enforcement, and
 *   constitutional protection of arms extending only to participants. The
 *   interval indexes years after ratification (t=0 is approximately 1791;
 *   t=70 approximately 1861), tracing the bargain from rough reciprocity
 *   through functional decay: as frontier threats receded and the
 *   anti-standing-army premise eroded, musters degenerated into ceremony,
 *   enforcement lapsed, and the operative residue became fine collection,
 *   before federalization of the militia replaced the system outright. The
 *   claim and the metrics are authored independently: the claimed type states
 *   the structure I believe true of the arrangement; the metrics describe its
 *   operation, including its decay-phase drift.
 *
 * KEY AGENTS:
 *   - militia_eligible_citizens: conditioned-rights holders who also bear service costs (moderate/constrained)
 *   - compelled_militia_enrollees: statute-bound obligors, the arrangement's primary burden-bearers (moderate/trapped)
 *   - state_militia_authorities: administrators who set terms, collect fines, and rewrite the bargain when compliance fails (institutional/arbitrage)
 *   - non_participating_arms_seekers: denied protected status entirely, with only political recourse (moderate/constrained)
 *   - federal_government: recipient of defense capacity at private expense, holder of organizing and preemption power (institutional/arbitrage)
 *   - volunteer_militia_companies: late-period honor-holders who absorbed the militia identity without the compulsory burden (organized/mobile)
 *   - professional_officer_corps: excluded advocates of the rival professional-force posture (powerful/mobile)
 *   - constitutional_scholars: analytical observers of the settlement and its decay (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__civic_right_reading, 0.58).
domain_priors:suppression_score(second_amendment_scope__civic_right_reading, 0.38).
domain_priors:theater_ratio(second_amendment_scope__civic_right_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__civic_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_scope__civic_right_reading, "Second Amendment Civic-Militia Conditioned Right (Civic Right Reading)").
narrative_ontology:topic_domain(second_amendment_scope__civic_right_reading, "constitutional law/political theory/rights jurisprudence").

domain_priors:requires_active_enforcement(second_amendment_scope__civic_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__civic_right_reading, '737cbca1-699a-42b5-8888-f0c49cb9d052').
narrative_ontology:cs_kernel_codification('737cbca1-699a-42b5-8888-f0c49cb9d052', fixed_text).
narrative_ontology:cs_authority_grounding('737cbca1-699a-42b5-8888-f0c49cb9d052', lineage).
narrative_ontology:cs_interpretation_layer_present('737cbca1-699a-42b5-8888-f0c49cb9d052').
narrative_ontology:cs_reading_relation('737cbca1-699a-42b5-8888-f0c49cb9d052', second_amendment_scope__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('737cbca1-699a-42b5-8888-f0c49cb9d052', second_amendment_scope__collective_right_reading, forecloses).
narrative_ontology:cs_axiom('737cbca1-699a-42b5-8888-f0c49cb9d052', foundational, right_conditioned_on_militia_service).
narrative_ontology:cs_axiom_status(right_conditioned_on_militia_service, holdable).
narrative_ontology:cs_axiom_grounding('737cbca1-699a-42b5-8888-f0c49cb9d052', right_conditioned_on_militia_service, conventional).
narrative_ontology:cs_axiom('737cbca1-699a-42b5-8888-f0c49cb9d052', foundational, arms_bearing_constitutes_citizenship).
narrative_ontology:cs_axiom_status(arms_bearing_constitutes_citizenship, holdable).
narrative_ontology:cs_axiom_grounding('737cbca1-699a-42b5-8888-f0c49cb9d052', arms_bearing_constitutes_citizenship, deontological).
narrative_ontology:cs_axiom('737cbca1-699a-42b5-8888-f0c49cb9d052', secondary, standing_armies_inimical_to_liberty).
narrative_ontology:cs_axiom_status(standing_armies_inimical_to_liberty, overridden).
narrative_ontology:cs_axiom_grounding('737cbca1-699a-42b5-8888-f0c49cb9d052', standing_armies_inimical_to_liberty, empirically_contingent).
narrative_ontology:cs_reference_frame('737cbca1-699a-42b5-8888-f0c49cb9d052', civic_militia_reciprocity_compact).
narrative_ontology:cs_drift_state('737cbca1-699a-42b5-8888-f0c49cb9d052', post_federalization_operational_collapse, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('737cbca1-699a-42b5-8888-f0c49cb9d052', '').
narrative_ontology:cs_kernel_id(second_amendment_scope__civic_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__civic_right_reading, militia_eligible_citizens).
narrative_ontology:constraint_beneficiary(second_amendment_scope__civic_right_reading, state_militia_authorities).
narrative_ontology:constraint_victim(second_amendment_scope__civic_right_reading, compelled_militia_enrollees).
narrative_ontology:constraint_victim(second_amendment_scope__civic_right_reading, non_participating_arms_seekers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(second_amendment_scope__civic_right_reading, compelled_militia_enrollees).
narrative_ontology:constraint_beneficiary(second_amendment_scope__civic_right_reading, federal_government).
narrative_ontology:constraint_beneficiary(second_amendment_scope__civic_right_reading, volunteer_militia_companies).
narrative_ontology:constraint_victim(second_amendment_scope__civic_right_reading, militia_eligible_citizens).
narrative_ontology:constraint_vindicates(second_amendment_scope__civic_right_reading, anti_standing_army_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_scope__civic_right_reading, civic_republican_citizenship_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Able-bodied citizens within the statutory age window hold a constitutionally protected right to keep and bear arms suitable for militia use — protection that attaches only through enrollment and service. Enrollment brings muster obligations, a requirement to equip oneself with musket and accoutrements at private expense, and exposure to fines for non-attendance; it also carries civic standing, a recognized political role, and immunity of one's arms from ordinary regulation. Leaving the arrangement means forfeiting the protected status of one's arms and accepting whatever regulation ordinary law imposes.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, militia_eligible_citizens, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_scope__civic_right_reading, militia_eligible_citizens, payer).

% Enrolled by statute upon reaching service age, without a consent step: the obligation to appear at musters, train, and answer calls-up attaches automatically and runs to age forty-five. The burden falls unevenly — those with money purchase exemptions or hire substitutes, while those without pay fines or serve. In return they hold the same conditioned arms right as other participants. Evasion is possible but carries legal risk, and relocation to frontier districts was the common escape.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, compelled_militia_enrollees, payer,
    moderate, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_scope__civic_right_reading, compelled_militia_enrollees, beneficiary).

% Governors, legislatures, and adjutants general write the muster calendar, prescribe acceptable arms, levy and collect fines, and command the force when called. They obtain a defense-ready citizenry without appropriating funds for a standing army, and the fine rolls supplement state revenue. When compliance collapsed they rewrote the terms — cutting muster days, commuting fines, eventually repealing compulsory service — rather than enforcing harder.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, state_militia_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% People who wish to keep firearms but do not enroll — the infirm, the disinclined, and members of groups barred from militia eligibility. Under this arrangement their arms receive no constitutional protection: whatever they own is regulable, registrable, or confiscable by ordinary law. Their recourse is argument in the political process, where their claim has no textual foothold so long as the conditioning holds.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, non_participating_arms_seekers, payer,
    moderate, biographical, constrained, national).

% Congress holds the enumerated powers to organize, arm, and discipline the militia and to call it forth; the presidency commands it when called. The arrangement delivers national defense capacity financed by household armament rather than treasury expenditure. Congress can preempt state administration — as it ultimately did by federalizing the militia into the National Guard — and until then collects the defense output while the costs remain with the states' enrollees.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, federal_government, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_scope__civic_right_reading, federal_government, agenda_setter).

% Self-formed uniformed companies of the 1830s-1850s — predominantly middle-class men who drilled for prestige and sociability rather than under compulsion. As compulsory musters collapsed, these companies absorbed the militia's official identity, received state arms issues and public subsidies, and enjoyed the civic honor the arrangement conferred while bearing none of the compulsory burden carried by the enrolled poor.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, volunteer_militia_companies, beneficiary,
    organized, biographical, mobile, regional).

% Career soldiers and advocates of a regular army, from the founding era onward, who argued that citizen musters produced untrained troops and that defense required a professional force. The arrangement was designed in opposition to their preference; they sat outside its administration and could influence it only by persuasion, which prevailed only after the militia system had decayed on its own.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, professional_officer_corps, excluded,
    powerful, generational, mobile, national).

% Legal historians and constitutional theorists who reconstruct the founding settlement, trace the militia system's operational decay, and weigh competing readings of the Amendment's text. They hold no position inside the arrangement; their seat is analytic, and their conclusions feed courts and legislatures rather than musters.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_scope__civic_right_reading, state_militia_authorities).
narrative_ontology:fixing_cost_class(second_amendment_scope__civic_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides common defense through a regulated citizen militia: households arm and train themselves to a state-prescribed standard, periodic musters maintain readiness, and the resulting force answers calls-up without the expense — and, to the founding generation, the danger — of a standing army.
% TRANSFER_FUNCTION: Moves defense labor and equipment costs from enrolled households to the public defense function (private armament and training supplying a collectively consumed good); moves constitutional protection of arms from non-participants to participants; moves fine revenue and regulatory authority over arms to state militia administrations.
% ABSENT_VOICES: Professional-officer and standing-army advocates, whose preferred defense posture the arrangement was built to foreclose; enslaved and free Black Americans, barred from militia eligibility in most states while arms laws were enforced hardest against them; and non-participating arms seekers, whose counterclaim the conditioning leaves without textual footing. Several of these seats were outside the room when the settlement was fixed, and their absence shaped whose interests the bargain encoded.
% DISAPPEARANCE_RATIONALE: If the conditioned-right settlement vanished overnight, enrolled households would shed muster duties, equipment costs, and fine exposure immediately; states would face an unfunded defense gap or turn to paid forces; the protected status of arms would redistribute — universalizing under one successor reading, vesting in state institutions under another; and the political settlement trading arms rights for defense service would require renegotiation from scratch.
% FOUNDING_PROBLEM: Secure common defense without a standing army: the founding generation treated professional standing armies as engines of tyranny and built defense on an armed, trained citizenry, making arms-bearing simultaneously a personal safeguard and a public duty.
% FOUNDING_PROBLEM_CORROBORATION: Military historians of the early republic, documenting muster collapse and mass delinquency, and the legislative record of the 1903 federalization of the militia into the National Guard attest — from outside the arrangement's beneficiary set — that the citizen-militia defense problem was dissolved rather than solved; the permanent professional military every branch now maintains confirms the anti-standing-army premise is no longer operative. Adherents of the civic reading dispute obsolescence at the level of constitutional meaning while conceding the operational system is gone.
narrative_ontology:disappearance_verdict(second_amendment_scope__civic_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_scope__civic_right_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__civic_right_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_scope__civic_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_scope__civic_right_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.58 is moderate: the service gate means much of the burden is the coordination cost itself — household armament and training literally constitute the defense force — but two components sit beyond inherent cost: the exclusion of non-participants from any protected status, and the late-period conversion of the arrangement into fine collection once training value collapsed. Suppression 0.38 is authored as a raw structural property, unscaled by power or scope: enrollment was compulsory by statute with fine and court-martial exposure, yet genuine exits existed — forfeiture of the right, exemption purchase, geographic evasion — keeping it below arrangements that must police exits to survive. Theater 0.68 reflects the end-state: by the 1840s-1850s musters were carnivals, training content had vanished, and the operative residue was fine rolls and paperwork. Accessibility_collapse 0.45: alternatives never collapsed — volunteer companies, professional-force advocacy, and unprotected arms-bearing remained live throughout. Resistance 0.55: mass delinquency, a flourishing market in exemptions and hired substitutes, and successive legislative repeals constituted sustained and largely successful resistance. Coordination type is resource_allocation: the arrangement's dominant function is allocating the defense burden across households against a collectively consumed good; its characteristic failure is burden maldistribution, not membership drift. The measurement series share one eight-point grid (decade steps) so every metric is authored at every examined time point, and the series converge on the base_properties end-state values.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats compute differently from the same structure. From state militia authorities the arrangement is an instrument they administer: they set terms, collect fines, receive readiness, and retained the latitude to rewrite the bargain when compliance failed. From enrolled households the same structure operated first as a reciprocal civic bargain and later as a compulsory burden whose promised benefits — trained corps, civic honor, protected arms — eroded faster than the obligations did. Non-participating arms seekers experience flat denial: no seat in the bargain and no protected status. The dual-positioned citizen seat is the hinge: the same population holds the conditioned right on the benefit side and the compulsory obligation on the burden side, and which face dominates varies by era and by income, since exemption purchasing shifted the burden down the wealth distribution.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for militia_eligible_citizens (hold the conditioned right and the civic standing attached to it), state_militia_authorities (collect fines and readiness and control the terms), federal_government (receives defense output financed at private expense), and volunteer_militia_companies (late-period honor and subsidies without obligation). Victim declarations drive high directionality for compelled_militia_enrollees (obligation attaches without consent; exit is blocked by statute and fine exposure) and non_participating_arms_seekers (bear total exclusion from protected status with only political recourse). The overlap between the first two seats — the same citizens appear on both sides — is the intended signature of a structure that coordinates and burdens through the single act of enrollment; the engine's per-seat computation should place the enrolled poor nearer the target end than exempt-purchasing participants, a differentiation the coarse group labels capture only partially and which the eligibility-boundary omega documents.
 *
 * MANDATROPHY ANALYSIS:
 *   Classification discipline cuts both ways here. Reading the arrangement as pure coordination ignores the trapped enrollee and the excluded seeker — the seats through which the bargain billed its costs. Reading it as pure extraction ignores the real defense function that made the bargain rational at founding and that volunteers and professionals eventually had to replace deliberately. The temporal record arbitrates: theater_ratio crosses 0.5 between t=40 and t=50 (the 1830s-40s), marking the point where muster attendance — the proxy — replaced defense readiness as the arrangement's operative content, the classic substitution drift. The founding problem is dead: the anti-standing-army defense problem was dissolved by the National Guard federalization and the accepted permanence of professional forces, while the arrangement's obligations persisted for decades past the function's death. The mismatch between a dead founding problem and a world that rearranges around the settlement flags the zombie configuration: the civic reading survives as interpretive doctrine atop an operational substrate that no longer exists, which is precisely what the genealogy interview exists to catch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_delta,
    'This constraint is one reading of the second_amendment_scope kernel (reading: civic_right_reading); what structural deltas would the sibling readings (individual_right_reading, collective_right_reading) introduce if instantiated?',
    'Author each sibling as its own constraint story and compare beneficiary sets, regulatory authority, and epsilon: the individual reading deletes the service gate (universalizes the beneficiary set, cuts regulatory authority, lowers epsilon); the collective reading deletes individual beneficiaries entirely (vests the protection in state militia institutions).',
    'Seat classifications and effective extraction shift wholesale across readings; policy conclusions drawn from this reading''s moderate, service-gated profile do not transfer to either sibling.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_delta, conceptual, 'Committer structure of the kernel contest: this file is one of three mutually exclusive readings of one constitutional text.').

omega_variable(
    conditioning_reciprocity_vs_gate,
    'Is the militia-service condition a genuine reciprocal civic bargain (service constitutes the right) or a gating mechanism whose principal effects are excluding non-servers and shifting defense costs onto households?',
    'Compare realized benefits and burdens across participant and non-participant seats over the interval: if participants receive proportionate civic benefit (protected arms, standing, political weight) the bargain is reciprocal; if obligations persist while benefits erode (late-period fines without training), the gate is operating extractively.',
    'Genuine reciprocity supports a coordination-dominant classification; extraction-dominant gating raises effective epsilon for trapped enrollees and pushes the arrangement toward the extraction pole.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditioning_reciprocity_vs_gate, conceptual, 'Whether the service condition is constitutive reciprocity or exclusionary gating.').

omega_variable(
    founding_vs_decay_constraint_identity,
    'Are the founding-era civic bargain (roughly t=0 to t=30) and the late-period fine-and-ceremony regime (roughly t=40 to t=70) one constraint or two — does measuring epsilon across the whole interval conflate structurally distinct arrangements?',
    'Decompose: author the founding bargain and the decayed shell as separate stories with separate epsilon values; if the decayed shell classifies as inertial-theatrical while the bargain classifies as hybrid coordination, the single label covered two constraints.',
    'Single-story treatment dates any type transition late; decomposition reveals the burden-collecting shell persisted for decades after the coordination function died.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_vs_decay_constraint_identity, empirical, 'Epsilon-invariance check on the civic-militia arrangement across its operational lifetime.').

omega_variable(
    eligibility_boundary_authorship,
    'The militia-eligible beneficiary set was statutorily bounded (able-bodied free men within an age window, with racial exclusions in most states); is that boundary part of this constraint''s structure or an external overlay, and how does it redirect the arrangement''s costs?',
    'Historical analysis of state militia statutes and arms-law enforcement against excluded populations (enslaved and free Black Americans); test whether denial of eligibility correlated with heightened arms prohibition against the excluded.',
    'If the boundary is internal, the arrangement''s costs concentrated on populations denied both the right and its civic path — widening the effective victim set beyond the authored seats and raising effective epsilon.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(eligibility_boundary_authorship, empirical, 'Whether the eligibility boundary is structural to the conditioned right or incidental to it.').

omega_variable(
    decay_mode_attrition_vs_substitution,
    'Did the compulsory militia system die by enforcement attrition (capacity decay) or by deliberate substitution (volunteer companies, then federalized guard)?',
    'Legislative histories of state repeal acts and the 1903 federalization statute; compare the timing of enforcement collapse against the organization of replacements.',
    'Attrition supports an inertia-shaped lifecycle for the drift series; deliberate substitution supports a transition-completed reading — changing the lifecycle verdict attached to the same measurements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decay_mode_attrition_vs_substitution, empirical, 'Mode of the arrangement''s operational collapse.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__civic_right_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_scope__civic_right_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(seco_tr_t0, observed).
narrative_ontology:measurement(seco_tr_t10, second_amendment_scope__civic_right_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement_basis(seco_tr_t10, observed).
narrative_ontology:measurement(seco_tr_t20, second_amendment_scope__civic_right_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement_basis(seco_tr_t20, observed).
narrative_ontology:measurement(seco_tr_t30, second_amendment_scope__civic_right_reading, theater_ratio, 30, 0.36).
narrative_ontology:measurement_basis(seco_tr_t30, observed).
narrative_ontology:measurement(seco_tr_t40, second_amendment_scope__civic_right_reading, theater_ratio, 40, 0.47).
narrative_ontology:measurement_basis(seco_tr_t40, observed).
narrative_ontology:measurement(seco_tr_t50, second_amendment_scope__civic_right_reading, theater_ratio, 50, 0.56).
narrative_ontology:measurement_basis(seco_tr_t50, observed).
narrative_ontology:measurement(seco_tr_t60, second_amendment_scope__civic_right_reading, theater_ratio, 60, 0.63).
narrative_ontology:measurement_basis(seco_tr_t60, observed).
narrative_ontology:measurement(seco_tr_t70, second_amendment_scope__civic_right_reading, theater_ratio, 70, 0.68).
narrative_ontology:measurement_basis(seco_tr_t70, observed).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_scope__civic_right_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(seco_be_t0, observed).
narrative_ontology:measurement(seco_be_t10, second_amendment_scope__civic_right_reading, base_extractiveness, 10, 0.41).
narrative_ontology:measurement_basis(seco_be_t10, observed).
narrative_ontology:measurement(seco_be_t20, second_amendment_scope__civic_right_reading, base_extractiveness, 20, 0.44).
narrative_ontology:measurement_basis(seco_be_t20, observed).
narrative_ontology:measurement(seco_be_t30, second_amendment_scope__civic_right_reading, base_extractiveness, 30, 0.48).
narrative_ontology:measurement_basis(seco_be_t30, observed).
narrative_ontology:measurement(seco_be_t40, second_amendment_scope__civic_right_reading, base_extractiveness, 40, 0.51).
narrative_ontology:measurement_basis(seco_be_t40, observed).
narrative_ontology:measurement(seco_be_t50, second_amendment_scope__civic_right_reading, base_extractiveness, 50, 0.54).
narrative_ontology:measurement_basis(seco_be_t50, observed).
narrative_ontology:measurement(seco_be_t60, second_amendment_scope__civic_right_reading, base_extractiveness, 60, 0.56).
narrative_ontology:measurement_basis(seco_be_t60, observed).
narrative_ontology:measurement(seco_be_t70, second_amendment_scope__civic_right_reading, base_extractiveness, 70, 0.58).
narrative_ontology:measurement_basis(seco_be_t70, observed).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_scope__civic_right_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(seco_su_t0, observed).
narrative_ontology:measurement(seco_su_t10, second_amendment_scope__civic_right_reading, suppression_requirement, 10, 0.57).
narrative_ontology:measurement_basis(seco_su_t10, observed).
narrative_ontology:measurement(seco_su_t20, second_amendment_scope__civic_right_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement_basis(seco_su_t20, observed).
narrative_ontology:measurement(seco_su_t30, second_amendment_scope__civic_right_reading, suppression_requirement, 30, 0.47).
narrative_ontology:measurement_basis(seco_su_t30, observed).
narrative_ontology:measurement(seco_su_t40, second_amendment_scope__civic_right_reading, suppression_requirement, 40, 0.44).
narrative_ontology:measurement_basis(seco_su_t40, observed).
narrative_ontology:measurement(seco_su_t50, second_amendment_scope__civic_right_reading, suppression_requirement, 50, 0.41).
narrative_ontology:measurement_basis(seco_su_t50, observed).
narrative_ontology:measurement(seco_su_t60, second_amendment_scope__civic_right_reading, suppression_requirement, 60, 0.39).
narrative_ontology:measurement_basis(seco_su_t60, observed).
narrative_ontology:measurement(seco_su_t70, second_amendment_scope__civic_right_reading, suppression_requirement, 70, 0.38).
narrative_ontology:measurement_basis(seco_su_t70, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_scope__civic_right_reading, resource_allocation).
narrative_ontology:affects_constraint(second_amendment_scope__civic_right_reading, second_amendment_scope__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__civic_right_reading, second_amendment_scope__collective_right_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Second Amendment' covers three structurally distinct constraints corresponding to the kernel's three readings. This file authors the civic_right_reading alone: an individual right whose existence is conditioned on militia participation — hence a beneficiary set of militia-eligible citizens, moderate regulatory authority, and service-gated moderate epsilon. The individual_right_reading (separate file) deletes the service gate: universal beneficiary set, minimal regulatory authority, lower epsilon. The collective_right_reading (separate file) deletes the individual bearer: state institutions become the sole rights-holders. The readings are linked as a family through affects_constraints; each file's epsilon is stable within its own reading, and cross-reading comparison happens at the network layer, not inside any single story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
