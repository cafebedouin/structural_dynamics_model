% ============================================================================
% CONSTRAINT STORY: legal_personhood_boundary__developmental_potentiality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legal_personhood_boundary__developmental_potentiality_reading, []).

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
 *   constraint_id: legal_personhood_boundary__developmental_potentiality_reading
 *   human_readable: Conception-Anchored Personhood Regime (Developmental-Potentiality Reading)
 *   domain: legal philosophy / constitutional law / rights theory
 *
 * SUMMARY:
 *   In jurisdictions where the developmental-potentiality reading of the
 *   personhood boundary is operative, legal rights-bearing status attaches at
 *   fertilization: every human organism is a rights-bearer from conception,
 *   and the state acquires both duty and authority to protect prenatal life
 *   against choices made by the person gestating it. The arrangement's
 *   operation is concrete: criminalized termination, interrogation of
 *   pharmacy and clinic records, investigation of miscarriage,
 *   mandated-reporting chains running through physicians, and liability
 *   exposure radiating outward to logistics volunteers and fertility
 *   laboratories. This file is one reading of the legal_personhood_boundary
 *   kernel (see kernel_context and the committer omega); the sibling readings
 *   are separate constraint files linked through the network block, not
 *   alternatives folded into this one. The claimed type and the metrics below
 *   are independently authored: the claim records the structure I believe
 *   true of this arrangement; the metrics record its operation as I believe
 *   descriptively accurate; the engine computes per-seat classifications from
 *   the structural data and owns any divergence between claim and
 *   computation.
 *
 * KEY AGENTS:
 *   - pregnant_persons: Primary target (powerless/trapped) - bears the full physical and legal burden of the protected gestation
 *   - conceived_human_organisms: Declared primary beneficiary (powerless/trapped) - protected status exercised entirely through adult proxies
 *   - state_enforcement_apparatus: Agenda-setter and receipt-of-gain seat (institutional/arbitrage) - converts the doctrine into jurisdiction, dockets, and data infrastructure
 *   - anti_abortion_advocacy_movement: Secondary beneficiary (organized/mobile) - supplies doctrine, litigation, and electoral force
 *   - religious_doctrinal_institutions: Doctrinal beneficiary (institutional/identity_locked) - teaching authority validated by legal alignment
 *   - obstetric_care_physicians: Enforced intermediary target (organized/constrained) - clinical judgment subordinated near criminal thresholds
 *   - fertility_clinics: Collateral target (organized/mobile) - cryopreserved embryos become stationary liabilities
 *   - abortion_logistics_volunteers: Network-node target (powerless/mobile) - prosecuted through the most exposed members
 *   - majority_pro_access_electorate: Excluded seat (organized/constrained) - majoritarian preference filtered from the operative agenda
 *   - reproductive_jurisprudence_scholars: Analytical observer (analytical/analytical) - maps the structure no combatant camp cites whole
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_personhood_boundary__developmental_potentiality_reading, 0.78).
domain_priors:suppression_score(legal_personhood_boundary__developmental_potentiality_reading, 0.85).
domain_priors:theater_ratio(legal_personhood_boundary__developmental_potentiality_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__developmental_potentiality_reading, snare).
narrative_ontology:human_readable(legal_personhood_boundary__developmental_potentiality_reading, "Conception-Anchored Personhood Regime (Developmental-Potentiality Reading)").
narrative_ontology:topic_domain(legal_personhood_boundary__developmental_potentiality_reading, "legal philosophy / constitutional law / rights theory").

domain_priors:requires_active_enforcement(legal_personhood_boundary__developmental_potentiality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__developmental_potentiality_reading, '5ae0e8cb-9b21-463e-a149-b55b6959725f').
narrative_ontology:cs_kernel_codification('5ae0e8cb-9b21-463e-a149-b55b6959725f', distributed).
narrative_ontology:cs_authority_grounding('5ae0e8cb-9b21-463e-a149-b55b6959725f', distributed).
narrative_ontology:cs_reading_relation('5ae0e8cb-9b21-463e-a149-b55b6959725f', legal_personhood_boundary__restrictive_anthropocentric_reading, forecloses).
narrative_ontology:cs_reading_relation('5ae0e8cb-9b21-463e-a149-b55b6959725f', legal_personhood_boundary__functional_capacity_reading, forecloses).
narrative_ontology:cs_axiom('5ae0e8cb-9b21-463e-a149-b55b6959725f', foundational, immediate_personhood_at_conception).
narrative_ontology:cs_axiom_status(immediate_personhood_at_conception, holdable).
narrative_ontology:cs_axiom_grounding('5ae0e8cb-9b21-463e-a149-b55b6959725f', immediate_personhood_at_conception, deontological).
narrative_ontology:cs_axiom('5ae0e8cb-9b21-463e-a149-b55b6959725f', secondary, state_duty_to_protect_prenatal_rights_bearers).
narrative_ontology:cs_axiom_status(state_duty_to_protect_prenatal_rights_bearers, holdable).
narrative_ontology:cs_axiom_grounding('5ae0e8cb-9b21-463e-a149-b55b6959725f', state_duty_to_protect_prenatal_rights_bearers, deontological).
narrative_ontology:cs_reference_frame('5ae0e8cb-9b21-463e-a149-b55b6959725f', conception_anchored_full_status).
narrative_ontology:cs_drift_state('5ae0e8cb-9b21-463e-a149-b55b6959725f', contemporary_post_floor_removal_pluralism, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('5ae0e8cb-9b21-463e-a149-b55b6959725f', '').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__developmental_potentiality_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__developmental_potentiality_reading, conceived_human_organisms).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__developmental_potentiality_reading, anti_abortion_advocacy_movement).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__developmental_potentiality_reading, religious_doctrinal_institutions).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__developmental_potentiality_reading, state_enforcement_apparatus).
narrative_ontology:constraint_victim(legal_personhood_boundary__developmental_potentiality_reading, pregnant_persons).
narrative_ontology:constraint_victim(legal_personhood_boundary__developmental_potentiality_reading, obstetric_care_physicians).
narrative_ontology:constraint_victim(legal_personhood_boundary__developmental_potentiality_reading, fertility_clinics).
narrative_ontology:constraint_victim(legal_personhood_boundary__developmental_potentiality_reading, abortion_logistics_volunteers).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__developmental_potentiality_reading, sanctity_of_life_from_conception).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__developmental_potentiality_reading, potentiality_principle_of_moral_status).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__developmental_potentiality_reading, fetal_right_to_life_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% From fertilization onward, under this reading each human organism holds full rights-bearing status. At the stages where enforcement operates they exercise no agency of their own: every claim made on their behalf - continued gestation, legal protection, burial requirements for embryonic remains - is voiced by adult proxies such as the state, advocacy organizations, and doctrinal institutions. What flows to them is continued biological development secured by law; nothing flows from them by their own choice.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, conceived_human_organisms, beneficiary,
    powerless, biographical, trapped, national).

% Carry the entire physical burden of the protected gestation. Under an operative conception-personhood regime their decisions about continuation, termination, medication, and travel are subordinated to the embryo's legally recognized status: procedures are criminalized, pharmacies interrogate prescriptions, miscarriages prompt investigation, and crossing state lines for care carries planning costs and, in some proposals, legal exposure. Biologically they cannot exit the condition the law binds them to; geographically, those with money and documents can travel, and others cannot.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, pregnant_persons, payer,
    powerless, biographical, trapped, national).

% Legislatures draft the personhood statutes, attorneys general defend them, prosecutors bring cases against providers and helpers, and health agencies build reporting systems that log pregnancies from the first prenatal visit. The apparatus gains jurisdiction, budget lines, dockets, and data infrastructure it did not previously hold; individual offices retain discretion over how aggressively to deploy it, and some jurisdictions decline enforcement entirely.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, state_enforcement_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(legal_personhood_boundary__developmental_potentiality_reading, state_enforcement_apparatus, beneficiary).

% Supplies the doctrinal argument, drafts legislation, litigates, and mobilizes voters. Each enacted personhood statute converts decades of persuasion into standing law, channels funding to member organizations through service contracts and litigation activity, and gives the movement a concrete objective whose pursuit sustains membership and donations. Its strategy can shift between states and between statutory, ballot, and constitutional routes.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, anti_abortion_advocacy_movement, beneficiary,
    organized, generational, mobile, national).

% Hold the theological anthropology (ensoulment at conception, the image-of-God teaching) from which the reading descends. Alignment between civil law and doctrine validates the institution's teaching authority; retreat would unravel positions across a wider lattice of sexual, marital, and end-of-life questions. Departure from the doctrine is not a menu option - the institution's identity is constituted by it.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, religious_doctrinal_institutions, beneficiary,
    institutional, civilizational, identity_locked, global).

% Practice medicine inside the enforcement perimeter: ectopic and molar pregnancies, previable membrane rupture, and lethal fetal anomalies now sit near criminal thresholds, delaying standard care until documentation establishes nonviability or the patient deteriorates. Mandated-reporting duties conscript them into the surveillance chain. Relocation to permissive states is possible but costs licensure recency, hospital privileges, patient panels, and family roots.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, obstetric_care_physicians, payer,
    organized, biographical, constrained, national).

% Store hundreds of thousands of cryopreserved embryos whose legal status the reading upgrades to persons-in-being. Disposal, donation-to-research, and abandonment protocols become potential homicide or neglect exposures; liability pricing and insurance harden accordingly. Clinics can, and some do, relocate operations or suspend new embryo creation to permissive jurisdictions, but stored embryos cannot be moved out of the law's reach by moving the lab.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, fertility_clinics, payer,
    organized, biographical, mobile, national).

% Drive patients across state lines, host them overnight, crowdfund procedure costs, and transport medication. Several jurisdictions have drafted or enacted offenses targeting exactly this assistance. Any individual volunteer can stop volunteering tomorrow; the network's function depends on enough people not doing so, and prosecution reaches the network through its most exposed members.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, abortion_logistics_volunteers, payer,
    powerless, biographical, mobile, national).

% In several enactment jurisdictions, persistent polling and referendum results show majorities favoring legal abortion access, yet the personhood statutes stand: districted legislatures, supermajority thresholds, and preemption keep that preference out of the operative agenda. Available channels - initiative, election turnover, litigation - exist but run on multi-year clocks against entrenched enforcement.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, majority_pro_access_electorate, excluded,
    organized, generational, constrained, national).

% Map the doctrine's structure across the competing readings, trace which enforcement mechanisms follow from which anchoring premise, and publish the comparisons that combatant camps cite selectively. They hold no vote; their product is the shared map.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, reproductive_jurisprudence_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legal_personhood_boundary__developmental_potentiality_reading, state_enforcement_apparatus).
narrative_ontology:fixing_cost_class(legal_personhood_boundary__developmental_potentiality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the adherent political community a single determinate answer to the personhood-boundary question - who counts as a rights-bearer - aligned with sanctity-of-life doctrine, and organizes enforcement of that boundary consistently across jurisdictions and instruments.
% TRANSFER_FUNCTION: Transfers bodily autonomy, medical decision authority, and life-planning control from pregnant persons to state enforcement institutions and the doctrinal coalition; transfers legally protected status (within the reading's own frame) to conceived organisms, exercised on their behalf by adult proxies.
% ABSENT_VOICES: Pregnant persons in enactment jurisdictions whose majoritarian pro-access preference is structurally filtered out of legislative agendas; and the conceived organisms themselves, who cannot speak - their 'voice' is constructed entirely by adult proxies, which is precisely the epistemic seam along which the sibling readings diverge.
% DISAPPEARANCE_RATIONALE: If the conception-anchored regime vanished overnight, criminal prosecutions of providers and helpers would cease, interstate care flows would normalize, mandated-reporting chains and pregnancy data systems would dismantle, fertility clinics would resume standard disposition protocols, and the advocacy coalition would lose its central legal objective - the reproductive-care economy and the enforcement bureaucracy built around it would both reorganize.
% FOUNDING_PROBLEM: Law must draw a rights-bearing boundary somewhere between gamete and born infant for purposes of protection, homicide law, and moral status; the developmental-potentiality reading was built to end arbitrary line-drawing by anchoring status at the earliest observable beginning of an individual human organism.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: bioethics and legal literature across all three readings acknowledge the line-drawing problem as real (the dispute is over the answer, not the question), and judicial opinions across jurisdictions repeatedly confront it. No corroborating source outside the beneficiary set attests that conception specifically is the correct anchor - that claim is attested only by the reading's own adherents, which is itself signal.
narrative_ontology:disappearance_verdict(legal_personhood_boundary__developmental_potentiality_reading, world_rearranges).
narrative_ontology:founding_problem_status(legal_personhood_boundary__developmental_potentiality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legal_personhood_boundary__developmental_potentiality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legal_personhood_boundary__developmental_potentiality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legal_personhood_boundary__developmental_potentiality_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legal_personhood_boundary__developmental_potentiality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legal_personhood_boundary__developmental_potentiality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legal_personhood_boundary__developmental_potentiality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.78: the arrangement's governing operation transfers decision authority over gestation - continuation, medication, travel, medical management of failed pregnancies - from the person gestating to enforcement institutions, near-totally and without the governed party's consent; assessed by the reading's own lights the transfer is a duty discharged, but descriptively it is a near-total transfer of bodily self-government. Suppression 0.85 is a raw structural property, deliberately unscaled by power or scope: criminalization of the exit act itself, interdiction of assistance networks, pharmacy and clinic record demands, and proposed travel penalties. Theater_ratio 0.25: the enforcement core is functional rather than performative; the theatrical fraction comprises mandatory-viewing scripts, gestational-age certification rituals, and crisis-pregnancy-center placement that simulates care provision. Accessibility_collapse 0.55: alternatives partly survive - interstate travel, telehealth medication, self-managed termination - characteristic of a constructed arrangement that must be defended rather than a natural limit. Resistance 0.75: sustained mass protest, recurring referendum majorities against enactment coalitions, provider-network litigation, and civil disobedience by logistics volunteers. All three tracked series share one time grid (points 0, 2, 4, 6, 8, 10); suppression_requirement is authored because the story specifically traces enforcement-capacity build-out after the federal-floor removal at t=6, not merely extraction drift. Coalition note: the payer seats overlap heavily (pregnant persons, volunteers, clinicians, a pro-access majority) and have demonstrated coalition capacity at the ballot box; the enforcement design answers this by targeting network nodes and insulating statutes behind supermajorities, which is why high latent coalition power has not yet converted into removal.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences the arrangement as the administration of a rights-protection duty it legitimately holds; the trapped payer seat experiences the identical statutes as dispossession of bodily self-government; the excluded-majority seat experiences them as an override of its documented preference. The deepest gap sits at the fetal seat: it is the arrangement's declared primary beneficiary yet exercises no perception or agency at the stages where enforcement operates - its 'experience' of the arrangement is wholly authored by adult proxies, which is precisely the seam along which the sibling readings diverge. The engine computes these per-seat classifications from the structural data; the divergence between the enforcement seat's computed type and the payer seats' computed type is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the low end: conceived_human_organisms (beneficiary, trapped) sit nearest the full-subsidy end - the arrangement exists to secure their continued development; anti_abortion_advocacy_movement and religious_doctrinal_institutions collect standing law, funding, and doctrinal validation; state_enforcement_apparatus collects jurisdiction, dockets, and data infrastructure while bearing enforcement cost and legitimacy exposure, placing it low but not at zero. Victim declarations drive the high end: pregnant_persons (trapped) sit nearest the full-target end - biology plus criminalization closes every exit; obstetric_care_physicians (constrained) and abortion_logistics_volunteers (individually mobile inside a targeted network) sit high; fertility_clinics hold genuine mobile exit and therefore sit measurably lower than their fellow payers despite comparable nominal exposure - the clearest same-power, exit-differentiated pair in the story. The excluded-majority seat carries diffuse cost (an overridden preference) with no declared beneficiary/victim position; the analytical seat is directionally neutral by construction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - law must draw the rights-bearing boundary somewhere between gamete and infant - remains live under every reading, so this arrangement cannot be dismissed as a mandate outliving its function; founding_problem_status is live and the mismatch consumer finds no dead-mandate flag. The mandatrophy risk here runs opposite to the usual case: the arrangement's sincere moral framing invites classification as shared-value coordination, and the structural tests are what prevent that mislabel - the governed did not opt in, the payer seats receive no compensating good, persistence depends on criminal enforcement and exit interdiction, and the gains concentrate in identifiable seats (receipt surface: state_enforcement_apparatus). Conversely, the analysis resists the opposite error: because the doctrine's adherent community does derive real identity-ordering value, the story carries the coordination_genuine_or_cover omega rather than asserting the cover conclusion as settled. Fixing is politically prohibitive for the seats that could fix it (supermajority insulation, electoral punishment, entrenched appointments) - a cost-class fact recorded on the receipt surface, not a classification claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_location,
    'This file instantiates only the developmental_potentiality_reading of the legal_personhood_boundary kernel; which structural element separates it from the restrictive_anthropocentric_reading and the functional_capacity_reading, and what would each sibling change?',
    'Comparative compilation of the three readings'' victim sets, beneficiary sets, and enforcement surfaces: the restrictive sibling restores the birth threshold (early embryos leave the protected class; enforcement collapses to post-birth instruments); the functional sibling keys status to demonstrated cognitive capacity regardless of species (early embryos leave the protected class; novel non-human and artificial candidates enter).',
    'If the disagreement reduces to the anchor premise alone, the readings are mutually foreclosing within any single legal framework and cannot be merged or averaged; if it reduces to weighting, hybrid framings become available and this file''s epsilon would need re-authoring under the hybrid.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_location, conceptual, 'Committer structure: one reading of the personhood-boundary kernel; sibling deltas and the located disagreement.').

omega_variable(
    fetal_seat_proxy_construction,
    'Is the conceived-organism seat a genuine agent seat whose interests the enforcement tracks, or a proxy construction through which adult stakeholders pursue their own objectives?',
    'Observe enforcement behavior where fetal interests and proxy interests diverge: continuation mandates for lethal-anomaly pregnancies, refusal of health exceptions, treatment of multifetal reduction. If enforcement consistently proceeds against the gestating person''s welfare where no independently survivable fetal interest exists, the seat is proxy-operated.',
    'If proxy-operated, the protected-class beneficiary is better modeled as the proxy coalition itself, the arrangement loses its remaining coordination-side justification, and effective extraction concentrates further on pregnant persons.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fetal_seat_proxy_construction, empirical, 'Whether the fetal rights-bearer seat is agent-genuine or proxy-constructed.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression structural (criminal exposure, distance, cost, surveillance) or internalized (shame norms, fatalism, self-monitoring that persists where enforcement is absent)?',
    'Cross-jurisdiction and post-repeal trajectory: compare care-seeking and disclosure behavior in enactment jurisdictions versus demographically matched permissive neighbors; if demand and disclosure remain depressed where no enforcement operates, a substantial internalized component exists.',
    'If largely internalized, effective suppression exceeds the structural measure and outlives any statutory reversal; the arrangement''s coercive footprint persists past the enforcement apparatus itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Split of suppression between material barriers and internalized norms.').

omega_variable(
    coordination_genuine_or_cover,
    'Is the identity-coordination function genuine (a sincere shared-value ordering its adherents would maintain even at lower imposed burden) or warrant covering the transfer the arrangement performs?',
    'Exception-acceptance test: whether enactment coalitions accept carve-outs (lethal fetal anomaly, rape, serious health of the carrier) that reduce the imposed burden without moving the conception anchor. Acceptance indicates the anchor, not the burden, is the doctrinal core; systematic rejection indicates the burden is load-bearing.',
    'Genuine coordination would support a hybrid coordination-plus-extraction reading of the structure; systematic rejection confirms the coordination story as warrant rather than function and consolidates the pure-extraction classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_genuine_or_cover, conceptual, 'Whether the arrangement''s coordination function is real or warrant-only.').

omega_variable(
    doctrine_scope_generalization,
    'Does the conception anchor remain confined to abortion, or does personhood logic generalize to emergency contraception and intrauterine devices, to embryo disposition in fertility medicine, and onward to end-of-life questions?',
    'Track enacted and proposed statutes beyond abortion: contraceptive reclassification bills, embryo-status legislation, advance-directive conflicts. Extension events are observable legislative facts.',
    'Generalization enlarges the victim set (contraceptive users, fertility patients, terminally ill persons), raises base extractiveness further, and converts a regional patchwork into a comprehensive status regime.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_scope_generalization, empirical, 'Scope trajectory of the conception-anchored status assignment.').

omega_variable(
    patchwork_vs_federal_resolution,
    'Will the jurisdictional enforcement patchwork persist, or does federal consolidation (national prohibition or a restored national access floor) collapse it in one direction or the other?',
    'Congressional and court trajectories: national legislation attempts, federal court composition, interstate-commerce and privacy jurisprudence bearing on cross-border care.',
    'Federal prohibition raises spatial scope to continental scale, amplifying effective extraction through verification difficulty; a restored federal floor reverses the measured trajectory and returns the arrangement to minority-enclave status.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(patchwork_vs_federal_resolution, empirical, 'Durability of the jurisdictional patchwork carrying the arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__developmental_potentiality_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lega_tr_t0, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(lega_tr_t2, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 2, 0.16).
narrative_ontology:measurement(lega_tr_t4, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement(lega_tr_t6, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 6, 0.2).
narrative_ontology:measurement(lega_tr_t8, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 8, 0.23).
narrative_ontology:measurement(lega_tr_t10, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 10, 0.25).

% Extraction over time
narrative_ontology:measurement(lega_be_t0, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(lega_be_t2, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(lega_be_t4, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 4, 0.52).
narrative_ontology:measurement(lega_be_t6, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 6, 0.62).
narrative_ontology:measurement(lega_be_t8, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 8, 0.72).
narrative_ontology:measurement(lega_be_t10, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 10, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(lega_su_t0, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(lega_su_t2, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 2, 0.4).
narrative_ontology:measurement(lega_su_t4, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 4, 0.5).
narrative_ontology:measurement(lega_su_t6, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 6, 0.65).
narrative_ontology:measurement(lega_su_t8, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 8, 0.76).
narrative_ontology:measurement(lega_su_t10, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 10, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_personhood_boundary__developmental_potentiality_reading, identity_coordination).
narrative_ontology:affects_constraint(legal_personhood_boundary__developmental_potentiality_reading, legal_personhood_boundary__restrictive_anthropocentric_reading).
narrative_ontology:affects_constraint(legal_personhood_boundary__developmental_potentiality_reading, legal_personhood_boundary__functional_capacity_reading).

% DUAL FORMULATION NOTE:
% Decomposition of the colloquial label 'when does personhood begin': the label conflates three structurally distinct claims about the rights-bearing anchor. This file authors the conception-anchored claim alone, with its own epsilon (high - the arrangement transfers gestational self-government to enforcement institutions), its own victim set (pregnant persons, clinicians, logistics volunteers, fertility medicine), and its own enforcement surface. The birth-anchored and capacity-anchored siblings carry different epsilons, different victim sets, and different enforcement mechanics; influence between family members runs through shared litigation infrastructure and doctrinal argument, recorded via network edges. No single file can hold all three without violating epsilon-invariance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
