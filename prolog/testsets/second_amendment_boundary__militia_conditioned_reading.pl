% ============================================================================
% CONSTRAINT STORY: second_amendment_boundary__militia_conditioned_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:boltzmann_floor_override/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: second_amendment_boundary__militia_conditioned_reading
 *   human_readable: Second Amendment Militia-Conditioned Reading: Regulatory Authority Over Firearms
 *   domain: constitutional/political/regulatory
 *
 * SUMMARY:
 *   This constraint embodies one reading of the contested kernel
 *   'second_amendment_boundary': the interpretation that the prefatory clause
 *   'A well regulated Militia, being necessary to the security of a free
 *   State' binds and defines the scope of the operative clause 'the right of
 *   the people to keep and bear Arms, shall not be infringed.' Under this
 *   reading, the right exists in order to preserve militia capability for
 *   collective defense; private firearm possession is not a pre-existing
 *   natural right but a derived right whose legitimacy flows from the militia
 *   context. This reading presumes State regulatory authority over firearms
 *   is presumptively legitimate and subject to means-end scrutiny rather than
 *   strict scrutiny. Victims of this constraint are gun owners whose
 *   possession is restricted by State law (collectors, self-defense claimants
 *   in high-regulation jurisdictions) and firearms dealers in regulated
 *   markets. Beneficiaries are State regulators and public safety
 *   constituencies who gain regulatory authority and scope. The kernel_id
 *   'second_amendment_boundary' contains three structurally distinct
 *   readings: this one (militia_conditioned_reading), the
 *   individual_right_reading (operative clause stands independently), and the
 *   insurrectionist_reading (the right preserves armed resistance capacity).
 *   Each generates different ε, beneficiary/victim sets, and classification.
 *   This constraint is authored as Tangled Rope: it coordinates State
 *   regulation (a genuine collective-action problem solved by uniform
 *   standards) AND asymmetrically extracts from gun owners (who bear the
 *   restriction costs). The measurement series tracks how extractiveness and
 *   theater have drifted since the militia-conditioned framing gained
 *   prominence in constitutional interpretation circa early 2000s.
 *
 * KEY AGENTS:
 *   - State legislatures and regulatory bodies: Agenda-setters. Set firearm policy, conduct means-end justification, enforce restrictions, collect regulatory authority as a benefit.
 *   - Gun owners subject to restriction: Victims. Bear the costs of possession restrictions, ownership bans, licensing requirements, red-flag laws in militia-conditioned jurisdictions.
 *   - Firearms dealers: Secondary victims. Face regulatory compliance, licensing, reporting requirements, market compression from restrictions.
 *   - Public safety constituencies: Beneficiaries. Gain regulatory authority, lower-violence outcomes (if regulation is effective), reduced access to regulated weapons in high-density areas.
 *   - Individual-right litigants and organizations: Challengers. Would reject this reading's premise and assert operative clause independence; their exclusion is the enforcement mechanism.
 *   - Comparative constitutional democracies (Canada, Australia, UK): Analytical observers. Their regulation-friendly militia readings show the constraint's viability in other systems.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__militia_conditioned_reading, 0.68).
domain_priors:suppression_score(second_amendment_boundary__militia_conditioned_reading, 0.45).
domain_priors:theater_ratio(second_amendment_boundary__militia_conditioned_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__militia_conditioned_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_boundary__militia_conditioned_reading, "Second Amendment Militia-Conditioned Reading: Regulatory Authority Over Firearms").
narrative_ontology:topic_domain(second_amendment_boundary__militia_conditioned_reading, "constitutional/political/regulatory").

domain_priors:requires_active_enforcement(second_amendment_boundary__militia_conditioned_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__militia_conditioned_reading, 'a9cae229-cc88-42ad-ab5b-272a1b60cbee').
narrative_ontology:cs_kernel_codification('a9cae229-cc88-42ad-ab5b-272a1b60cbee', fixed_text).
narrative_ontology:cs_authority_grounding('a9cae229-cc88-42ad-ab5b-272a1b60cbee', lineage).
narrative_ontology:cs_interpretation_layer_present('a9cae229-cc88-42ad-ab5b-272a1b60cbee').
narrative_ontology:cs_reading_relation('a9cae229-cc88-42ad-ab5b-272a1b60cbee', second_amendment_boundary__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('a9cae229-cc88-42ad-ab5b-272a1b60cbee', second_amendment_boundary__insurrectionist_reading, coexists_with).
narrative_ontology:cs_axiom('a9cae229-cc88-42ad-ab5b-272a1b60cbee', foundational, prefatory_clause_defines_operative_scope).
narrative_ontology:cs_axiom_status(prefatory_clause_defines_operative_scope, holdable).
narrative_ontology:cs_axiom_grounding('a9cae229-cc88-42ad-ab5b-272a1b60cbee', prefatory_clause_defines_operative_scope, deontological).
narrative_ontology:cs_axiom('a9cae229-cc88-42ad-ab5b-272a1b60cbee', foundational, collective_defense_grounds_individual_right).
narrative_ontology:cs_axiom_status(collective_defense_grounds_individual_right, holdable).
narrative_ontology:cs_axiom_grounding('a9cae229-cc88-42ad-ab5b-272a1b60cbee', collective_defense_grounds_individual_right, conventional).
narrative_ontology:cs_reference_frame('a9cae229-cc88-42ad-ab5b-272a1b60cbee', militia_regulation_framework).
narrative_ontology:cs_drift_state('a9cae229-cc88-42ad-ab5b-272a1b60cbee', contemporary_public_safety_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a9cae229-cc88-42ad-ab5b-272a1b60cbee', '2026-06-12T14:23:18Z').
narrative_ontology:cs_kernel_id(second_amendment_boundary__militia_conditioned_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, state_regulators).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, public_safety_constituencies).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, gun_owners_subject_to_restriction).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, firearms_dealers_in_regulated_markets).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, state_regulatory_bodies).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, firearms_dealers).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, firearms_dealers).
narrative_ontology:constraint_vindicates(second_amendment_boundary__militia_conditioned_reading, state_police_power_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_boundary__militia_conditioned_reading, collective_security_framing).
narrative_ontology:constraint_vindicates(second_amendment_boundary__militia_conditioned_reading, means_end_scrutiny_framework).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set firearm policy within the militia-conditioned constitutional framework. Conduct legislative fact-finding to justify restrictions as means-end rational or intermediate scrutiny level. Enact bans on categories (automatic weapons, high-capacity magazines), licensing requirements, background checks, red-flag statutes. Collect regulatory authority and public-safety legitimacy as benefits. Experience the militia-conditioned reading as enabling their legislative agenda.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, state_legislatures, agenda_setter,
    institutional, generational, analytical, national).

% Administer firearm licensing, permitting, and enforcement of restrictions. Document public-safety justifications for regulations. Operate within the militia-conditioned framework which presumes their authority is legitimate. Benefit from the expanded regulatory scope and reduced litigation burden compared to strict-scrutiny frameworks.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, state_regulatory_bodies, agenda_setter,
    institutional, biographical, analytical, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_boundary__militia_conditioned_reading, state_regulatory_bodies, beneficiary).

% Urban centers, gun-violence researchers, public-health advocates, anti-gun-violence organizations. Gain from the militia-conditioned reading which allows comprehensive restriction of civilian firearm access. Experience regulations as coordination in service of collective safety. Can mobilize politically to expand restrictions and defend them against individual-right constitutional challenges.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, public_safety_constituencies, beneficiary,
    organized, biographical, mobile, national).

% Collectors, hunters, rural self-defense practitioners, sport shooters. Bear the direct costs of possession restrictions, licensing requirements, permit fees, ownership bans, mandatory reporting, red-flag law exposure. Their exit is constrained: gun ownership is often tied to livelihood (rural living, wildlife management), identity (community membership, self-conception as gun owner), or genuine necessity (predator defense in rural areas). Cannot relocate easily due to economic and family ties. Experience the militia-conditioned reading as a post-hoc justification for restrictions disconnected from militia capability.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, gun_owners_subject_to_restriction, payer,
    powerless, biographical, identity_locked, national).

% Licensed firearms sellers, gunsmiths, ammunition dealers. Bear regulatory compliance costs (licensing, background-check administration, record-keeping, training requirements, liability exposure from red-flag laws). In high-regulation states, face market compression and dealer-to-consumer friction. Some dealers benefit from the constraint's legitimacy (it prevents black-market alternatives from gaining political legitimacy). Can exit by relocating to lower-regulation states but face capital costs and established customer bases.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, firearms_dealers, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_boundary__militia_conditioned_reading, firearms_dealers, beneficiary).

% Second Amendment advocacy organizations, constitutional scholars, individual-right litigators. Argue that the operative clause 'keep and bear Arms' establishes a pre-existing individual natural right unbound by the prefatory militia clause. Would be present in the conversation if the military-conditioned reading did not dominate academic and judicial interpretation. Structurally excluded by the constraint's authority (they cannot set the interpretive baseline; they must litigate reactively). Experience the militia-conditioned reading as a suppressed alternative whose adoption would dissolve this constraint.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, individual_right_advocates, excluded,
    organized, generational, trapped, national).

% Apply constitutional interpretation to firearm-restriction statutes. Under the militia-conditioned reading, courts use means-end scrutiny (rational basis or intermediate) rather than strict scrutiny. They adjudicate whether specific restrictions are constitutionally sound by asking whether they preserve militia access while serving public safety. Their role is to confirm the militia-conditioned reading's framework or shift to alternative readings. Currently, most federal courts (outside the 5th and 2nd Circuits) apply the militia-conditioned framework.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, federal_courts, observer,
    institutional, generational, analytical, universal).

% Argue that the Second Amendment exists to preserve armed resistance capacity against tyrannical government; individual possession is instrumentally necessary for potential overthrow. Structurally excluded from mainstream constitutional interpretation. Would assert that any regulation reducing private firearm availability violates the right's founding purpose. Are treated as fringe rather than serious constitutional voices in most legal contexts.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, insurrectionist_reading_proponents, excluded,
    powerless, biographical, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_boundary__militia_conditioned_reading, state_legislatures).
narrative_ontology:fixing_cost_class(second_amendment_boundary__militia_conditioned_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The militia-conditioned reading coordinates State regulatory authority over firearms by establishing a principled framework (means-end scrutiny) within which legislation can proceed without triggering strict constitutional scrutiny. It solves the collective-action problem of how to regulate access to weapons that enable mass harm (suicide, mass shooting, criminal acquisition) while preserving a constitutional foundation for regulation rather than fighting each statute individually. It coordinates public-safety constituencies around a shared understanding that democratic regulation is constitutionally legitimate.
% TRANSFER_FUNCTION: Moves regulatory authority from individual firearm owners (who would otherwise hold a constitutionally protected individual right) to State legislatures and regulatory bodies. The transfer is asymmetric: owners lose possession rights (access, categories of weapons, carry permits), States gain regulatory scope and policy discretion. The value transferred is authority over the civilian firearm market and the ability to shape public-safety policy without strict constitutional constraint.
% ABSENT_VOICES: Individual-right advocates and insurrectionist-reading proponents. They would argue the operative clause stands independently and prefatory-clause limits are impermissible. They are structurally excluded from mainstream constitutional conversation because the militia-conditioned reading dominates academic interpretation and federal court doctrine. Their presence would reframe the constraint entirely: from a legitimate coordination mechanism (State regulation) to an extraction mechanism (constitutional usurpation of a pre-existing right). Legislative testimony, lower-court dissents, and advocacy organizations represent them at the margins but without authority to set the interpretive baseline.
% DISAPPEARANCE_RATIONALE: If the militia-conditioned reading vanished overnight and the individual-right reading became dominant, the entire regulatory regime around firearms would be subject to strict scrutiny challenges. Existing bans (automatic weapons, high-capacity magazines), licensing schemes, and red-flag laws would face constitutional invalidation. States would have to justify regulations through means narrowly tailored to compelling interests rather than through general police power. The civilian firearm market would expand; regulations would compress. Public-safety policy would reorganize around different mechanisms (policing, mental-health intervention, ammunition tax) rather than access restriction. The constraint's disappearance would represent a fundamental shift in constitutional authority.
% FOUNDING_PROBLEM: The founding problem, in the militia-conditioned reading, is twofold: (1) How can States regulate militia access and ensure its responsible use without destroying militia capability? (2) How can democratic governance regulate civilian firearm access for public safety while remaining consistent with a constitutional protection for arms? The reading was developed (or revived) to solve this: by making the militia context definitional rather than merely exemplary, it vests States with authority to regulate while preserving the constitutional legitimacy of that authority.
% FOUNDING_PROBLEM_CORROBORATION: State regulators, public-health constituencies, and academic constitutional scholars attest the founding problem is live: they argue that regulating firearm access is essential for public safety and that the militia-conditioned reading provides the constitutional legitimacy to do so without treating the right as preexisting and absolute. Individual-right advocates and some jurisdictions (Texas, Florida) attest the founding problem is dead or misframed: they argue that militia regulation is a solved problem (modern militia structures are government-managed National Guard) and the real problem is preventing government tyranny through civilian access to arms, which requires an unbound individual right. Comparative constitutional democracies (Canada, Australia, UK) with militia-conditioned or similar readings show that comprehensive firearm regulation is politically viable under such readings, providing external corroboration that the reading enables regulation in practice. No single corroborating source outside the benefiting parties exists — the contest itself is the evidence that the founding problem's status is genuinely disputed.
narrative_ontology:disappearance_verdict(second_amendment_boundary__militia_conditioned_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_boundary__militia_conditioned_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__militia_conditioned_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(second_amendment_boundary__militia_conditioned_reading, 'none', 1).

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
 *   Extractiveness is measured at 0.68 because the militia-conditioned reading vests regulatory authority in States to restrict firearms substantially, subject only to the condition that militia access is not completely eliminated. This is not compensation for coordination cost — the coordination benefit (uniform standards, democratic process) is real but modest in scope. The extraction comes from the asymmetry: gun owners subject to restriction have little exit (constrained or identity_locked — gun ownership is often tied to identity, rural livelihood, or self-defense necessity) while States have high power and can shape the regulatory landscape. Suppression is measured at 0.45 (moderate) because the constraint persists through both structural enforcement (constitutional holdings, statutory authority) and internalized acceptance. The prefatory-clause reading has gained significant ground in constitutional law since the early 2000s, but it remains contested — the individual_right_reading still holds substantial legitimacy in certain jurisdictions and constituencies, so suppression of alternatives is real but not overwhelming. Theater ratio at 0.22 reflects that the militia-conditioned framing does perform a genuine coordination function (uniform standards, democratic legitimacy for policy) but also performs a theater function (invoking 'well regulated Militia' to justify regulations that have nothing to do with militia capability, e.g., background checks for collectors). The measurement series shows both extractiveness and theater rising over the 25-year interval as the militia-conditioned reading consolidated in academic and judicial interpretation while actual regulation expanded beyond militia-relevant measures. The temporal drift is best understood as a constraint that began as genuine coordination (regulating militia access to ensure responsible use) and drifted toward extraction (regulating private possession using the militia framing as a cover story). The one shared time grid ensures every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   From the State-regulator seat, this is genuine coordination: the militia-conditioned reading provides a principled framework for democratically restricting access to weapons that pose collective-action problems (suicide, mass shooting, criminal access). State regulators experience the constraint as legitimacy-granting and power-enhancing — it solves the problem of how to regulate firearms without triggering strict scrutiny. From the gun-owner seat (especially collectors and rural self-defense claimants), the same structure operates as extraction: the militia-conditioned reading is experienced as a post-hoc justification for restrictions that have nothing to do with militia effectiveness, and the ability to exit by litigating or relocating to individual-right jurisdictions is constrained by legal barriers and identity-lock. The engine should compute different types from each seat: from the State seat, a Rope or lower-Tangled-Rope (coordination with modest extraction); from the gun-owner seat, a Snare or high-Tangled-Rope (extraction with a coordination cover story). The directionality values will differ sharply because the two seats have opposite interests in the constraint's persistence.
 *
 * DIRECTIONALITY LOGIC:
 *   State regulators and public-safety constituencies have directionality near 0.0 (beneficiaries): they gain regulatory authority, they set the agenda, they experience the constraint as enabling rather than extractive. Gun owners subject to restriction have directionality near 1.0 (full targets): they bear costs (restricted access, compliance, relocation to alternative jurisdictions), they have constrained exit (legal barriers, identity-lock to gun ownership), they cannot exit by collective action at the national level. Firearms dealers sit intermediate: they are secondary victims (regulatory compliance costs) but not primary targets (they can relocate or exit the market). The beneficiary group (state regulators, public safety) is smaller and more powerful than the victim group (dispersed gun owners), which amplifies effective extraction from the victim seats' perspective. No directionality override is needed; the derivation from beneficiary/victim + power + exit should produce the correct d values automatically.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids false mandatrophy classification by declaring both beneficiaries (State regulators gain authority) and victims (gun owners bear costs). If mandatrophy were declared, it would assert the militia-conditioned reading has lost its founding function — that States no longer need to regulate militia access (the founding problem is dead) but the constraint persists anyway. That is not plausible: the founding problem (ensuring militia access is not abused while preserving collective defense capability) is contested but live; different constituencies read its status differently. The constraint is not inert; it actively shapes policy in high-regulation jurisdictions. Mandatrophy would be a misclassification here. The true uncertainty is whether the constraint's operation has drifted from genuine coordination (militia-preserving regulation) to pure extraction (using the militia framing to justify restrictions unrelated to militia capability) — that is the mandate-drift question captured in the theater_ratio measurements, not a mandatrophy question.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prefatory_clause_binding_force,
    'Does the prefatory clause ''well regulated Militia'' logically constrain the operative clause ''keep and bear Arms,'' or does it merely state a purpose without limiting scope?',
    'Comparative constitutional interpretation across jurisdictions with similar two-clause structures; historical drafting records and ratification-era usage patterns; precedent from prior courts treating prefatory vs. operative clause relationships.',
    'If binding: this reading''s regulatory authority framework holds; victims = gun owners subject to restriction; State presumption of legitimacy applies. If non-binding: the operative clause''s scope expands; victims = regulators and public safety constituencies facing constitutional challenges; private possession becomes presumptively protected.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(prefatory_clause_binding_force, conceptual, 'Whether prefatory clause logically binds the operative clause''s scope or merely states purpose.').

omega_variable(
    collective_vs_individual_antecedent,
    'Does ''the right of the people to keep and bear Arms'' refer to an antecedent individual natural right (as the individual_right_reading claims) or to a right that exists only in connection with militia service (as this reading claims)?',
    'Eighteenth-century usage of ''the right of the people'' across other constitutional provisions; militia participation records; contemporary writings on arms bearing and self-defense; the framing generation''s own constitutional language in other contexts.',
    'If individual antecedent: the operative clause stands independently; regulation requires strict scrutiny. If militia-conditioned: the right derives its legitimacy from the militia context; regulation receives rational-basis or intermediate scrutiny; extraction of restriction power is coordination, not overreach.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_vs_individual_antecedent, conceptual, 'What the framing generation understood ''the right of the people'' to antecede.').

omega_variable(
    regulation_mechanism_legitimacy,
    'Does the militia-conditioned framing authorize comprehensive regulation, or only regulation that preserves militia effectiveness?',
    'Comparative militia law in other constitutional democracies; historical patterns of state militia regulation; what makes a militia ''well regulated'' in the founding era''s understanding; contemporary militia functionality tests in constitutional litigation.',
    'If comprehensive: all firearm restrictions pass constitutional muster so long as they don''t eliminate militia access entirely; extractiveness is low (regulation is coordination cost). If militia-preserving only: restrictions that degrade militia capability violate the right; extractiveness rises sharply (the constraint becomes a binding limitation on State authority); victims shift toward regulators.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulation_mechanism_legitimacy, empirical, 'What scope of regulation is authorized by the militia condition.').

omega_variable(
    kernel_reading_intercommunicability,
    'Can a single legal framework coherently hold both the militia-conditioned reading (this constraint) and the individual_right_reading (its sibling) as competing interpretations within the same system, or does commitment to one framework foreclose the other?',
    'Examination of how Courts (US and comparative) handle structural interpretive conflicts in the same provision; whether frameworks can coexist at different doctrinal levels (e.g., strict scrutiny applied to individual rights while rational-basis applied to militia conditions); political-economy data on how different constituencies deploy each reading in litigation strategy.',
    'If coexistent: both readings remain live as competing positions; the kernel contest is structural to constitutional interpretation and resolution lies in political process, not logical foreclosure. If foreclosed: one reading''s acceptance requires the other''s rejection; classification of the entire kernel shifts; legitimacy claims of the losing reading become indefensible within the system.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_intercommunicability, conceptual, 'Whether the militia-conditioned and individual-right readings can coexist within a single legal framework.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.45) structural (external barriers: constitutional holdings, enforcement mechanisms preventing alternative market operations) or internalized (gun owners'' acceptance of regulation as legitimate because they accept the militia-conditioned framing)?',
    'Survey data on gun-owner attitudes toward regulation in militia-conditioned vs. individual-right jurisdictions; compliance patterns post-regulation; exit data (relocation from high-regulation to low-regulation states); litigation initiation rates; media framing analysis of whether suppression is presented as legitimate constraint or illegitimate intrusion.',
    'If structural: suppression persists through enforcement and collective action; alternative regulatory frameworks cannot gain traction without structural intervention. If internalized: suppression is cognitive and framing-dependent; it dissolves if the militia-conditioned reading loses authority or if gun owners shift to individual-right framing; the constraint''s persistence is theater-dependent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Whether measured suppression is structural or internalized in gun-owner acceptance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__militia_conditioned_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(seco_tr_t0, observed).
narrative_ontology:measurement(seco_tr_t5, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement_basis(seco_tr_t5, observed).
narrative_ontology:measurement(seco_tr_t10, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement_basis(seco_tr_t10, observed).
narrative_ontology:measurement(seco_tr_t15, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement_basis(seco_tr_t15, observed).
narrative_ontology:measurement(seco_tr_t20, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement_basis(seco_tr_t20, observed).
narrative_ontology:measurement(seco_tr_t25, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 25, 0.22).
narrative_ontology:measurement_basis(seco_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(seco_be_t0, observed).
narrative_ontology:measurement(seco_be_t5, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement_basis(seco_be_t5, observed).
narrative_ontology:measurement(seco_be_t10, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement_basis(seco_be_t10, observed).
narrative_ontology:measurement(seco_be_t15, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 15, 0.61).
narrative_ontology:measurement_basis(seco_be_t15, observed).
narrative_ontology:measurement(seco_be_t20, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(seco_be_t20, observed).
narrative_ontology:measurement(seco_be_t25, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(seco_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement_basis(seco_su_t0, observed).
narrative_ontology:measurement(seco_su_t5, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 5, 0.36).
narrative_ontology:measurement_basis(seco_su_t5, observed).
narrative_ontology:measurement(seco_su_t10, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 10, 0.39).
narrative_ontology:measurement_basis(seco_su_t10, observed).
narrative_ontology:measurement(seco_su_t15, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 15, 0.42).
narrative_ontology:measurement_basis(seco_su_t15, observed).
narrative_ontology:measurement(seco_su_t20, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 20, 0.44).
narrative_ontology:measurement_basis(seco_su_t20, observed).
narrative_ontology:measurement(seco_su_t25, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 25, 0.45).
narrative_ontology:measurement_basis(seco_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__militia_conditioned_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_boundary__militia_conditioned_reading, 0.12).
narrative_ontology:affects_constraint(second_amendment_boundary__militia_conditioned_reading, second_amendment_boundary__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__militia_conditioned_reading, second_amendment_boundary__insurrectionist_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__militia_conditioned_reading, firearms_regulation__shall_issue_doctrine).
narrative_ontology:affects_constraint(second_amendment_boundary__militia_conditioned_reading, state_police_power__public_safety_authority).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel second_amendment_boundary. The other readings (individual_right_reading, insurrectionist_reading) are separate constraint stories with different ε values, beneficiary/victim sets, and classifications. All three are linked via affects_constraints to signal the kernel contest. No reading logically forecloses the others in contemporary US law — they coexist as competing interpretations held by different parties. This story assumes the militia-conditioned reading is operative and shows its structural effects on gun owners and State regulators. Sibling stories assume different readings are operative and will show different structural configurations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
