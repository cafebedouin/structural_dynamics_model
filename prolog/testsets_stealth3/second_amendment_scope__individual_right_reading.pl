% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_scope__individual_right_reading, []).

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
 *   constraint_id: second_amendment_scope__individual_right_reading
 *   human_readable: Individual-Right Reading of the Second Amendment: Constitutional Shield for Private Firearms Ownership Unconnected to Militia Service
 *   domain: constitutional_law/political_theory/rights_jurisprudence
 *
 * SUMMARY:
 *   The ratified Second Amendment text is a contested kernel; this story
 *   instantiates ONE reading of it — the individual-right reading, under
 *   which the amendment guarantees every person an entitlement to acquire and
 *   possess firearms unconnected to any militia service, enforced by federal
 *   courts against regulation at every level of government. The standing
 *   arrangement under contest (and the sole epsilon referent here) is that
 *   enforced arrangement: a constitutional shield that removes regulatory
 *   designs from ordinary politics, concentrates enforceable liberty in
 *   owners and commercial demand in the industry, and transfers the security
 *   costs of widespread civilian armament onto violence-exposed communities,
 *   constrained governments, and persons whose abusers retain firearm access.
 *   Sibling readings (collective_right_reading, civic_right_reading) are
 *   separate constraint files with their own beneficiary sets and epsilon
 *   values; nothing in this file averages across them. Claim and metrics are
 *   authored independently: the reading's adherents present the arrangement
 *   as settled constitutional meaning approaching natural fact, while the
 *   authored metrics describe an actively enforced, substantially costly,
 *   heavily resisted constructed arrangement — the engine measures that
 *   divergence rather than this file reconciling it. KEY AGENTS (by
 *   structural relationship): - law_abiding_firearm_owners: Primary
 *   beneficiary (organized/identity_locked) — holds the protected liberty,
 *   supplies the political weight - firearm_industry: Concentrated commercial
 *   beneficiary (institutional/arbitrage) — receives the arrangement's
 *   monetizable gains - gun_rights_advocacy_organizations: Beneficiary and
 *   litigation agenda-shaper (organized/arbitrage) - federal_judiciary:
 *   Agenda setter (institutional/analytical) — its doctrine defines the
 *   arrangement's operative force - urban_gun_violence_communities: Primary
 *   payer (moderate/trapped) — bears the mortality and security burden in
 *   place - state_local_governments: Institutional payer
 *   (institutional/constrained) — regulatory authority progressively narrowed
 *   - intimate_partner_violence_exposed_persons: Trapped payer
 *   (powerless/trapped) — disarmament protections hardest to sustain -
 *   non_owning_citizens: Nominal beneficiary, effective payer
 *   (moderate/mobile) — holds the guarantee without exercising it -
 *   gun_control_advocacy_movements: Organized payer (organized/mobile) —
 *   policy objectives repeatedly invalidated
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__individual_right_reading, 0.71).
domain_priors:suppression_score(second_amendment_scope__individual_right_reading, 0.62).
domain_priors:theater_ratio(second_amendment_scope__individual_right_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_scope__individual_right_reading, "Individual-Right Reading of the Second Amendment: Constitutional Shield for Private Firearms Ownership Unconnected to Militia Service").
narrative_ontology:topic_domain(second_amendment_scope__individual_right_reading, "constitutional_law/political_theory/rights_jurisprudence").

domain_priors:requires_active_enforcement(second_amendment_scope__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__individual_right_reading, 'da3bb956-d50d-458c-95d4-b46352cc24b8').
narrative_ontology:cs_kernel_codification('da3bb956-d50d-458c-95d4-b46352cc24b8', fixed_text).
narrative_ontology:cs_authority_grounding('da3bb956-d50d-458c-95d4-b46352cc24b8', lineage).
narrative_ontology:cs_interpretation_layer_present('da3bb956-d50d-458c-95d4-b46352cc24b8').
narrative_ontology:cs_reading_relation('da3bb956-d50d-458c-95d4-b46352cc24b8', second_amendment_scope__collective_right_reading, forecloses).
narrative_ontology:cs_reading_relation('da3bb956-d50d-458c-95d4-b46352cc24b8', second_amendment_scope__civic_right_reading, forecloses).
narrative_ontology:cs_axiom('da3bb956-d50d-458c-95d4-b46352cc24b8', foundational, the_people_means_individuals_not_states).
narrative_ontology:cs_axiom_status(the_people_means_individuals_not_states, holdable).
narrative_ontology:cs_axiom_grounding('da3bb956-d50d-458c-95d4-b46352cc24b8', the_people_means_individuals_not_states, conventional).
narrative_ontology:cs_axiom('da3bb956-d50d-458c-95d4-b46352cc24b8', secondary, self_defense_is_pre_political_natural_right).
narrative_ontology:cs_axiom_status(self_defense_is_pre_political_natural_right, holdable).
narrative_ontology:cs_axiom_grounding('da3bb956-d50d-458c-95d4-b46352cc24b8', self_defense_is_pre_political_natural_right, deontological).
narrative_ontology:cs_reference_frame('da3bb956-d50d-458c-95d4-b46352cc24b8', enumerated_individual_liberty_guarantee).
narrative_ontology:cs_drift_state('da3bb956-d50d-458c-95d4-b46352cc24b8', post_heller_bruen_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('da3bb956-d50d-458c-95d4-b46352cc24b8', '').
narrative_ontology:cs_kernel_id(second_amendment_scope__individual_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, law_abiding_firearm_owners).
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, firearm_industry).
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, gun_rights_advocacy_organizations).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, urban_gun_violence_communities).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, state_local_governments).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, intimate_partner_violence_exposed_persons).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, non_owning_citizens).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, non_owning_citizens).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, gun_control_advocacy_movements).
narrative_ontology:constraint_vindicates(second_amendment_scope__individual_right_reading, heller_individual_right_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_scope__individual_right_reading, text_history_tradition_review).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own firearms for self-defense, hunting, and sport under a constitutional guarantee that courts enforce against regulation. A substantial subset treats ownership as central to personal and regional identity, which makes surrendering arms culturally costly even where legally permitted. Their votes and memberships supply the political weight that defends the guarantee.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, law_abiding_firearm_owners, beneficiary,
    organized, biographical, identity_locked, national).

% Manufactures and sells into a market whose demand is stabilized and widened by the constitutional guarantee; funds trade associations and litigation that defend it. Exit is easy in commercial terms — product lines, exports, diversification — so its attachment to the arrangement is strategic rather than existential.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, firearm_industry, beneficiary,
    institutional, generational, arbitrage, global).

% Draw membership, dues, and relevance from defending and expanding the guarantee; designed and funded the multi-decade litigation campaign that produced the modern judicial doctrine. Collect organizational benefits while simultaneously shaping the interpretive agenda through strategic case selection.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, gun_rights_advocacy_organizations, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_scope__individual_right_reading, gun_rights_advocacy_organizations, agenda_setter).

% Interprets the ratified text and strikes laws that fail its tests; its doctrine — individual right, text-history-tradition review — defines what the guarantee actually does in practice. Holds the agenda-setting seat: no legislature can move the arrangement's boundary without passing through its rulings.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Live with the highest homicide and injury burdens of widespread civilian armament; their preferred regulations are repeatedly invalidated in court. Political voice exists but loses at the state and federal level; leaving means abandoning homes, jobs, and family networks, so the costs are borne in place.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, urban_gun_violence_communities, payer,
    moderate, biographical, trapped, local).

% Hold police-power responsibility for public safety while their regulatory toolkit narrows with each ruling; spend heavily drafting, defending, and redrafting laws that courts then invalidate. Cannot exit the constitutional system and must operate inside whatever interpretive space remains open.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, state_local_governments, payer,
    institutional, generational, constrained, national).

% Depend on disarmament rules — restraining-order surrender requirements, abuser-disarmament statutes — that face due-process and scope challenges under the strengthened guarantee; an armed abuser raises the lethal stakes of leaving. Exit from the relationship is already dangerous and slower where the abuser retains firearm access.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, intimate_partner_violence_exposed_persons, payer,
    powerless, immediate, trapped, local).

% Hold the same nominal guarantee but do not exercise it; they bear the ambient security costs of widespread armament and the fiscal costs of responding to it, while the owner minority supplies the issue's electoral intensity. Acquiring arms remains cheaply available to them, so their position is chosen rather than locked.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, non_owning_citizens, payer,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_scope__individual_right_reading, non_owning_citizens, beneficiary).

% Organize for regulation the guarantee repeatedly blocks; absorb the costs of defeated legislation, lost litigation, and repeated constitutional redesign of their proposals. Strategy stays mobile — state workarounds, ballot measures, culture change — because no single venue settles the dispute.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, gun_control_advocacy_movements, payer,
    organized, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_scope__individual_right_reading, firearm_industry).
narrative_ontology:fixing_cost_class(second_amendment_scope__individual_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a uniform, judicially enforceable national baseline guaranteeing individual arms acquisition and possession against regulatory fluctuation — solving the collective problem of securing a minority-preference liberty against majoritarian reversal and interstate patchwork, and fixing stable expectations for owners, dealers, and manufacturers.
% TRANSFER_FUNCTION: Moves regulatory discretion away from state and local governments toward individual owners; moves the security costs of widespread armament onto violence-exposed communities, partners of armed abusers, and public budgets; moves litigation expenditure from rights-defending organizations to governmental defendants.
% ABSENT_VOICES: Survivors of gun violence and communities with elevated homicide rates have no seat in constitutional adjudication — the text-history-tradition method admits historical materials, not victim-burden evidence; public-health researchers supply the strongest cost data yet occupy no formal role. They stand outside the courtroom, in legislatures and city councils whose products the arrangement then overrules.
% DISAPPEARANCE_RATIONALE: Overnight disappearance would restore unrestricted regulatory authority to every state and municipality; carry regimes, purchase controls, and storage mandates would proliferate within a single legislative session; the industry's domestic demand assumptions, the advocacy sector's litigation portfolio, and millions of owners' legal expectations would all be invalidated simultaneously — the American firearms order would reorganize around state police power.
% FOUNDING_PROBLEM: Securing the people's arms against federal confiscation: the ratifying generation tied the amendment to the militia system — the states' defense rested on citizen-soldiers bringing privately kept arms, and the amendment guaranteed that Congress could neither disarm that material basis nor destroy the states' militia institution.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians and militia-system scholarship outside the gun-rights beneficiary coalition attest both the founding problem (militia preservation against federal standing-army ambition) and its death: the 1903 Dick Act and the 1916 National Defense Act absorbed the state militias into a federally controlled National Guard, eliminating the citizen-soldier system the amendment was written to protect. No serious historical account disputes the sequence; this reading's own severance of right from militia service concedes the obsolescence from inside.
narrative_ontology:disappearance_verdict(second_amendment_scope__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_scope__individual_right_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__individual_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_scope__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_scope__individual_right_reading, 0.71, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_scope__individual_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_scope__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_scope__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.71: the arrangement's costs fall on parties who never consented to bear them — communities carrying the nation's highest firearm mortality, governments stripped of regulatory instruments they were elected to use, and partners of armed abusers — while enforceable benefits concentrate on owners and industry; broad universal coverage plus strict judicial review is exactly the configuration the manifest's expected delta identifies as scaling burden upward. Suppression 0.62: persistence runs through judicial veto and the chilling effect of anticipated invalidation rather than physical coercion; citizens retain voice channels (elections, amendment), which caps suppression below personally enforced arrangements. Theater 0.18: in the operative era the arrangement's activity is substantively functional — rulings really strike laws, permits really issue or vanish — leaving only a residual ceremonial layer of constitutional veneration. Accessibility_collapse 0.62: once the review standard is understood, whole families of regulatory design (may-issue licensing, common semi-automatic-platform prohibitions, broad carry restrictions) collapse as presumptively unavailable, though a residual design space survives. Resistance 0.78: continuous, organized, expensive opposition — re-passing of invalidated laws, litigation, ballot measures, public-health campaigning — among the highest sustained resistance profiles of any constitutional arrangement. All three tracked series share one eleven-point grid (1939-2025) with every metric authored at every point. The dormant era (1939-1999) shows near-zero extractiveness and suppression with a HIGH theater share, because the reading then survived almost entirely as rhetoric — scholarly argument, platform planks, advocacy literature — while courts enforced the rival collective reading; the operative era (2008 forward) inverts the profile as function dominates and theater falls, while suppression_requirement ratchets upward with the review regime's hardening and steps down slightly after the recent appellate tempering.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. From the owner seat the arrangement is a liberty guarantee — the same structure that constrains governments delivers enforceable freedom, and identity fusion makes the guarantee feel like selfhood rather than policy. From the industry seat it is a stable demand floor defended as principle. From the judiciary seat it is a doctrine to administer faithfully. From the community, government, and intimate-partner-vulnerability seats the identical structure operates as the removal of protection: every ruling that widens the guarantee narrows somebody's capacity to be safe or to govern. The engine computes these per-seat classifications from the declared roles, power atoms, and exit options; the divergence between the beneficiary seats' experienced coordination and the payer seats' experienced burden is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (law_abiding_firearm_owners, firearm_industry, gun_rights_advocacy_organizations) derive low directionality — the arrangement subsidizes them; identity_locked exit on the owner seat pins that seat nearest the full-beneficiary end, since surrendering arms means surrendering a fused identity, while the industry's arbitrage-grade exit makes its attachment strategic rather than existential. Declared victims (urban_gun_violence_communities, state_local_governments, intimate_partner_violence_exposed_persons) derive high directionality; trapped exit on the community and intimate-partner seats places them near the full-target end — they bear the costs in place, without mobility. Dual-positioned seats sit mid-range: non_owning_citizens hold the nominal guarantee (beneficiary half) while bearing ambient risk (payer half); gun_control_advocacy_movements pay in perpetually defeated objectives. The federal judiciary holds the agenda-setting seat without collecting rents — it administers the boundary rather than profiting from it. No directionality overrides are authored: the derivation from declared roles and exit options reproduces the true relationships, and the override mechanism keys on power atoms, which cannot separate same-atom agents here (the industry and the governments are both institutional with opposite directionalities) — an override would blur precisely the structure this story distinguishes.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preserving the citizen-soldier militia system against federal disarmament — died when the militia was absorbed into the federally controlled National Guard (1903/1916), and this reading itself concedes the death by severing the right from militia service entirely. What persists is a transferred mandate: individual self-defense liberty detached from its civic-military origin. That is mandate transfer, not atrophy — the operative function is live, the theater ratio is low, and enforcement is energetic, so the degraded-inertia signature does not fit. The R5 mismatch consumer will nonetheless flag founding_problem_status=dead combined with disappearance_verdict=world_rearranges; the flag correctly registers genealogical discontinuity — the arrangement no longer solves what it was built to solve — while the computed classification reflects the live transferred function. The classification guards against both mislabels: a pure-coordination verdict would conceal the asymmetric transfer of security costs onto non-consenting payers; a pure-extraction verdict would erase the genuine liberty-stabilization function that millions of owners actually consume and that gives the arrangement its real coordination content.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This file instantiates one reading (individual_right_reading) of the second_amendment_scope kernel; if a sibling reading — collective_right_reading or civic_right_reading — displaced it as the operative interpretation, how would the constraint''s beneficiary set and classification restructure?',
    'Doctrinal displacement tracked through Supreme Court composition, reversal candidates, and Article V amendment politics; each sibling is authored as its own constraint file with its own epsilon, beneficiary set, and classification.',
    'Under collective_right_reading the beneficiary set collapses to state institutions, individual coverage vanishes, and the arrangement moves toward federalism coordination with low individual-facing burden; under civic_right_reading the beneficiary set narrows to militia participants and a conditioning gate reappears that this reading structurally lacks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one-of-three readings of the Second Amendment scope kernel; sibling readings are separate constraints, not hedged positions inside this one.').

omega_variable(
    defensive_use_net_effect,
    'Does widespread civilian armament produce net protective or net harmful effects — do defensive firearm uses outweigh or undercount the deaths, injuries, and coercions the arrangement''s cost-bearers experience?',
    'Incident-level data linkage joining defensive-use reporting, crime records, and injury surveillance, replacing the contested survey extrapolations whose annual defensive-use estimates range from roughly sixty thousand to two and a half million.',
    'A demonstrated net-protective profile would pull effective burden down toward genuine coordination of self-protection; a net-harm profile pushes the arrangement toward the pure-extraction boundary and strengthens the payer seats'' computed position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(defensive_use_net_effect, empirical, 'Net safety effect of civilian armament — the pivotal empirical unknown beneath the extractiveness score.').

omega_variable(
    strict_scrutiny_trajectory,
    'Will the text-history-tradition review regime continue hardening (invalidating ever more regulatory designs) or soften back toward interest-balancing after the recent appellate tempering?',
    'Track lower-court invalidation rates, remand outcomes, and granted-review dockets across successive seasons following the tempering ruling.',
    'Hardening raises suppression and accessibility_collapse together and pushes the arrangement toward the pure-extraction boundary; softening lowers both and stabilizes the hybrid coordination-plus-burden profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strict_scrutiny_trajectory, empirical, 'Trajectory of judicial review intensity, which governs the arrangement''s suppressive force going forward.').

omega_variable(
    owner_identity_fusion_durability,
    'Is the identity fusion binding a substantial subset of firearm owners to possession durable across generational turnover, or is it cohort-specific?',
    'Longitudinal cohort surveys tracking ownership motivation, identity salience, and willingness to surrender arms across successive generations.',
    'If fusion fades, resistance declines and the arrangement drifts toward inertial maintenance with weakening functional attachment; if it deepens, the payer coalition''s prospects weaken further and the beneficiary side locks nearer the subsidy end.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(owner_identity_fusion_durability, empirical, 'Durability of the identity-lock mechanism on the beneficiary side of the arrangement.').

omega_variable(
    amendment_pathway_feasibility,
    'Is the Article V repeal-or-amend pathway realistically available, or is the prohibitive cost of fixing the arrangement through formal channels a permanent feature?',
    'Track state legislative composition against the thirty-eight-state ratification threshold alongside historical amendment success rates.',
    'A credibly opening pathway would convert the arrangement''s persistence basis from entrenchment to ongoing consent and revise the fixing-cost judgment; permanent closure entrenches the current cost asymmetry between those who could fix it and those who bear it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(amendment_pathway_feasibility, empirical, 'Feasibility of the formal exit route anchoring the prohibitive fixing-cost assessment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__individual_right_reading, 1939, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1939, second_amendment_scope__individual_right_reading, theater_ratio, 1939, 0.55).
narrative_ontology:measurement_basis(seco_tr_t1939, observed).
narrative_ontology:measurement(seco_tr_t1950, second_amendment_scope__individual_right_reading, theater_ratio, 1950, 0.6).
narrative_ontology:measurement_basis(seco_tr_t1950, observed).
narrative_ontology:measurement(seco_tr_t1965, second_amendment_scope__individual_right_reading, theater_ratio, 1965, 0.62).
narrative_ontology:measurement_basis(seco_tr_t1965, observed).
narrative_ontology:measurement(seco_tr_t1980, second_amendment_scope__individual_right_reading, theater_ratio, 1980, 0.58).
narrative_ontology:measurement_basis(seco_tr_t1980, observed).
narrative_ontology:measurement(seco_tr_t1991, second_amendment_scope__individual_right_reading, theater_ratio, 1991, 0.5).
narrative_ontology:measurement_basis(seco_tr_t1991, observed).
narrative_ontology:measurement(seco_tr_t1999, second_amendment_scope__individual_right_reading, theater_ratio, 1999, 0.42).
narrative_ontology:measurement_basis(seco_tr_t1999, observed).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_scope__individual_right_reading, theater_ratio, 2008, 0.3).
narrative_ontology:measurement_basis(seco_tr_t2008, observed).
narrative_ontology:measurement(seco_tr_t2010, second_amendment_scope__individual_right_reading, theater_ratio, 2010, 0.26).
narrative_ontology:measurement_basis(seco_tr_t2010, observed).
narrative_ontology:measurement(seco_tr_t2016, second_amendment_scope__individual_right_reading, theater_ratio, 2016, 0.24).
narrative_ontology:measurement_basis(seco_tr_t2016, observed).
narrative_ontology:measurement(seco_tr_t2022, second_amendment_scope__individual_right_reading, theater_ratio, 2022, 0.2).
narrative_ontology:measurement_basis(seco_tr_t2022, observed).
narrative_ontology:measurement(seco_tr_t2025, second_amendment_scope__individual_right_reading, theater_ratio, 2025, 0.18).
narrative_ontology:measurement_basis(seco_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(seco_be_t1939, second_amendment_scope__individual_right_reading, base_extractiveness, 1939, 0.04).
narrative_ontology:measurement_basis(seco_be_t1939, observed).
narrative_ontology:measurement(seco_be_t1950, second_amendment_scope__individual_right_reading, base_extractiveness, 1950, 0.05).
narrative_ontology:measurement_basis(seco_be_t1950, observed).
narrative_ontology:measurement(seco_be_t1965, second_amendment_scope__individual_right_reading, base_extractiveness, 1965, 0.07).
narrative_ontology:measurement_basis(seco_be_t1965, observed).
narrative_ontology:measurement(seco_be_t1980, second_amendment_scope__individual_right_reading, base_extractiveness, 1980, 0.09).
narrative_ontology:measurement_basis(seco_be_t1980, observed).
narrative_ontology:measurement(seco_be_t1991, second_amendment_scope__individual_right_reading, base_extractiveness, 1991, 0.12).
narrative_ontology:measurement_basis(seco_be_t1991, observed).
narrative_ontology:measurement(seco_be_t1999, second_amendment_scope__individual_right_reading, base_extractiveness, 1999, 0.22).
narrative_ontology:measurement_basis(seco_be_t1999, observed).
narrative_ontology:measurement(seco_be_t2008, second_amendment_scope__individual_right_reading, base_extractiveness, 2008, 0.45).
narrative_ontology:measurement_basis(seco_be_t2008, observed).
narrative_ontology:measurement(seco_be_t2010, second_amendment_scope__individual_right_reading, base_extractiveness, 2010, 0.55).
narrative_ontology:measurement_basis(seco_be_t2010, observed).
narrative_ontology:measurement(seco_be_t2016, second_amendment_scope__individual_right_reading, base_extractiveness, 2016, 0.6).
narrative_ontology:measurement_basis(seco_be_t2016, observed).
narrative_ontology:measurement(seco_be_t2022, second_amendment_scope__individual_right_reading, base_extractiveness, 2022, 0.68).
narrative_ontology:measurement_basis(seco_be_t2022, observed).
narrative_ontology:measurement(seco_be_t2025, second_amendment_scope__individual_right_reading, base_extractiveness, 2025, 0.71).
narrative_ontology:measurement_basis(seco_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1939, second_amendment_scope__individual_right_reading, suppression_requirement, 1939, 0.08).
narrative_ontology:measurement_basis(seco_su_t1939, observed).
narrative_ontology:measurement(seco_su_t1950, second_amendment_scope__individual_right_reading, suppression_requirement, 1950, 0.09).
narrative_ontology:measurement_basis(seco_su_t1950, observed).
narrative_ontology:measurement(seco_su_t1965, second_amendment_scope__individual_right_reading, suppression_requirement, 1965, 0.11).
narrative_ontology:measurement_basis(seco_su_t1965, observed).
narrative_ontology:measurement(seco_su_t1980, second_amendment_scope__individual_right_reading, suppression_requirement, 1980, 0.13).
narrative_ontology:measurement_basis(seco_su_t1980, observed).
narrative_ontology:measurement(seco_su_t1991, second_amendment_scope__individual_right_reading, suppression_requirement, 1991, 0.16).
narrative_ontology:measurement_basis(seco_su_t1991, observed).
narrative_ontology:measurement(seco_su_t1999, second_amendment_scope__individual_right_reading, suppression_requirement, 1999, 0.24).
narrative_ontology:measurement_basis(seco_su_t1999, observed).
narrative_ontology:measurement(seco_su_t2008, second_amendment_scope__individual_right_reading, suppression_requirement, 2008, 0.4).
narrative_ontology:measurement_basis(seco_su_t2008, observed).
narrative_ontology:measurement(seco_su_t2010, second_amendment_scope__individual_right_reading, suppression_requirement, 2010, 0.48).
narrative_ontology:measurement_basis(seco_su_t2010, observed).
narrative_ontology:measurement(seco_su_t2016, second_amendment_scope__individual_right_reading, suppression_requirement, 2016, 0.55).
narrative_ontology:measurement_basis(seco_su_t2016, observed).
narrative_ontology:measurement(seco_su_t2022, second_amendment_scope__individual_right_reading, suppression_requirement, 2022, 0.66).
narrative_ontology:measurement_basis(seco_su_t2022, observed).
narrative_ontology:measurement(seco_su_t2025, second_amendment_scope__individual_right_reading, suppression_requirement, 2025, 0.62).
narrative_ontology:measurement_basis(seco_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_scope__individual_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_scope__individual_right_reading, collective_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__individual_right_reading, civic_right_reading).

% DUAL FORMULATION NOTE:
% Kernel decomposition note: the colloquial label 'the Second Amendment' covers three structurally distinct constraints — one per reading of the second_amendment_scope kernel. This file authors the individual_right_reading (universal individual beneficiary set, heavily constrained regulatory authority, high epsilon). collective_right_reading (state-institutional beneficiary set, federalism-coordination function, no individual coverage) and civic_right_reading (militia-participant beneficiary set, conditioned right) are separate files. The readings' epsilon values differ because their beneficiary/victim structures differ — not because one text is measured inconsistently; each reading is a different constraint. Family members link through affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
