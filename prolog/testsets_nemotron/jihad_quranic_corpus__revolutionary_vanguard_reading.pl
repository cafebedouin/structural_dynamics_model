% ============================================================================
% CONSTRAINT STORY: jihad_quranic_corpus__revolutionary_vanguard_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jihad_quranic_corpus__revolutionary_vanguard_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: jihad_quranic_corpus__revolutionary_vanguard_reading
 *   human_readable: Jihad as Immediate Individual Obligation (Fard 'Ayn) Against Apostate Rulers and Occupiers
 *   domain: religious/political/theological
 *
 * SUMMARY:
 *   This constraint story instantiates the revolutionary_vanguard_reading of
 *   the jihad_quranic_corpus kernel. It treats jihad not as a regulated state
 *   function (expansionist_legalist_reading) or primarily spiritual/defensive
 *   obligation (defensive_spiritual_reading), but as an immediate, individual
 *   duty (fard 'ayn) triggered by the apostasy of rulers and presence of
 *   occupiers. The reading deploys takfir (excommunication) to declare Muslim
 *   rulers apostates and emergency jurisprudence (fiqh al-awlawiyat / fiqh
 *   al-haraj) to bypass classical safeguards: no imam required, no invitation
 *   to Islam (da'wa) prerequisite, proportionality overridden by necessity,
 *   non-combatant immunity dissolved via collective guilt (man 'aana /
 *   tatarus). The constraint extracts life, resources, and compliance from
 *   target populations while providing coordination and status to vanguard
 *   militants and interpretive authority to revolutionary scholars. It is
 *   maintained by active enforcement (violent imposition, suppression of
 *   dissenting scholarship) and has no sunset mechanism.
 *
 * KEY AGENTS:
 *   - vanguard_militants: Primary target/beneficiary hybrid (powerful/identity_locked) — bears lethal risk, gains status and theological certainty
 *   - revolutionary_scholars: Primary beneficiary (organized/identity_locked) — supplies doctrinal cover, gains interpretive monopoly
 *   - apostate_rulers: Primary victim (institutional/trapped) — targeted for removal, no exit
 *   - occupying_forces: Primary victim (institutional/trapped) — targeted for expulsion, no exit
 *   - civilian_populations_in_target_zones: Victim (powerless/trapped) — collective guilt assigns combatant status, extraction of life and resources
 *   - moderate_muslim_scholars: Victim (organized/constrained) — delegitimized, suppressed, co-opted or eliminated
 *   - religious_minorities: Victim (powerless/trapped) — disproportionate targeting, no protection
 *   - security_services: Agenda setter/enforcer (institutional/mobile) — suppresses the constraint militarily
 *   - analytical_observer: Observer (analytical/analytical) — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.92).
domain_priors:suppression_score(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.88).
domain_priors:theater_ratio(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__revolutionary_vanguard_reading, snare).
narrative_ontology:human_readable(jihad_quranic_corpus__revolutionary_vanguard_reading, "Jihad as Immediate Individual Obligation (Fard 'Ayn) Against Apostate Rulers and Occupiers").
narrative_ontology:topic_domain(jihad_quranic_corpus__revolutionary_vanguard_reading, "religious/political/theological").

domain_priors:requires_active_enforcement(jihad_quranic_corpus__revolutionary_vanguard_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__revolutionary_vanguard_reading, 'a5fc57c9-2426-452a-a57a-f6ea60bb2c66').
narrative_ontology:cs_kernel_codification('a5fc57c9-2426-452a-a57a-f6ea60bb2c66', fixed_text).
narrative_ontology:cs_authority_grounding('a5fc57c9-2426-452a-a57a-f6ea60bb2c66', lineage).
narrative_ontology:cs_interpretation_layer_present('a5fc57c9-2426-452a-a57a-f6ea60bb2c66').
narrative_ontology:cs_reading_relation('a5fc57c9-2426-452a-a57a-f6ea60bb2c66', jihad_quranic_corpus__defensive_spiritual_reading, forecloses).
narrative_ontology:cs_reading_relation('a5fc57c9-2426-452a-a57a-f6ea60bb2c66', jihad_quranic_corpus__expansionist_legalist_reading, influences).
narrative_ontology:cs_axiom('a5fc57c9-2426-452a-a57a-f6ea60bb2c66', foundational, fard_ayn_triggered_by_emergency).
narrative_ontology:cs_axiom_status(fard_ayn_triggered_by_emergency, holdable).
narrative_ontology:cs_axiom_grounding('a5fc57c9-2426-452a-a57a-f6ea60bb2c66', fard_ayn_triggered_by_emergency, deontological).
narrative_ontology:cs_axiom('a5fc57c9-2426-452a-a57a-f6ea60bb2c66', foundational, takfir_legitimizes_bypass_of_imam).
narrative_ontology:cs_axiom_status(takfir_legitimizes_bypass_of_imam, holdable).
narrative_ontology:cs_axiom_grounding('a5fc57c9-2426-452a-a57a-f6ea60bb2c66', takfir_legitimizes_bypass_of_imam, deontological).
narrative_ontology:cs_axiom('a5fc57c9-2426-452a-a57a-f6ea60bb2c66', secondary, collective_guilt_dissolves_noncombatant_immunity).
narrative_ontology:cs_axiom_status(collective_guilt_dissolves_noncombatant_immunity, holdable).
narrative_ontology:cs_axiom_grounding('a5fc57c9-2426-452a-a57a-f6ea60bb2c66', collective_guilt_dissolves_noncombatant_immunity, instrumental).
narrative_ontology:cs_reference_frame('a5fc57c9-2426-452a-a57a-f6ea60bb2c66', classical_jihad_fiqh_imam_monopoly).
narrative_ontology:cs_drift_state('a5fc57c9-2426-452a-a57a-f6ea60bb2c66', post_afghan_jihad_1980s, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a5fc57c9-2426-452a-a57a-f6ea60bb2c66', '').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__revolutionary_vanguard_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__revolutionary_vanguard_reading, vanguard_militants).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__revolutionary_vanguard_reading, revolutionary_scholars).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, apostate_rulers).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, occupying_forces).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, civilian_populations_in_target_zones).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, moderate_muslim_scholars).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, religious_minorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, vanguard_militants).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__revolutionary_vanguard_reading, emergency_doctrine_overrides_classical_safeguards).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__revolutionary_vanguard_reading, takfir_legitimizes_bypass_of_state_authority).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__revolutionary_vanguard_reading, collective_guilt_assigns_combatant_status_to_civilians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individual militants who answer the fard 'ayn call. They bear lethal risk, forego normal livelihoods, and submit to vanguard discipline. In return they gain theological certainty (guaranteed martyrdom reward), brotherhood, status within the vanguard, and purpose. Exit is identity-locked: leaving constitutes apostasy in the vanguard's framework; professional reintegration is near-impossible; relational bonds are fused to the struggle. They operate globally but are concentrated in conflict zones.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, vanguard_militants, payer,
    powerful, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__revolutionary_vanguard_reading, vanguard_militants, beneficiary).

% Scholars who provide doctrinal cover (fatwas, manuals, theological innovation) for the vanguard reading. They gain interpretive monopoly over the 'true' meaning of jihad, status as heirs of the prophets, recruitment pipeline control, and resource flows (donations, bay'ah). Exit is identity-locked: their scholarly identity is constituted by this reading; recantation destroys authority. Some maintain ambiguous ties to mainstream institutions (constrained exit), but the core cadre is fused.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, revolutionary_scholars, beneficiary,
    organized, generational, identity_locked, global).

% Muslim rulers declared apostates (takfir) for ruling by non-sharia law, allying with non-Muslim powers, or suppressing vanguard groups. They are targeted for removal (assassination, overthrow). No theological exit: the takfir verdict is retroactive and total. Physical exit (exile) is possible but politically fatal. They bear the full extraction of the constraint's violence and the cost of counter-vanguard repression.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, apostate_rulers, payer,
    institutional, immediate, trapped, national).

% Non-Muslim military forces occupying Muslim lands (per vanguard definition). Targeted for expulsion through attrition warfare. No theological exit (defined as enemies of Islam). Physical exit (withdrawal) is the vanguard's objective but the constraint extracts maximum cost before withdrawal. They bear combat losses, occupation costs, and political attrition.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, occupying_forces, payer,
    institutional, immediate, trapped, regional).

% Civilians living in territories controlled or contested by vanguard groups. Assigned combatant status via collective guilt doctrines (man 'aana — those who assist/remain; tatarus — collateral damage permitted). Extraction: forced taxation (zakat/jizya at gunpoint), forced labor, child recruitment, movement restrictions, summary justice, prohibition of exit (human shields). No protection under classical rules; no exit without vanguard permission or external liberation.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, civilian_populations_in_target_zones, payer,
    powerless, immediate, trapped, local).

% Mainstream scholars who reject the vanguard reading (uphold defensive jihad, state authority, non-combatant immunity). They face delegitimization (labeled 'palace scholars', 'government muftis'), co-optation pressure, physical threat, and loss of platform/audience to vanguard scholars. Exit options: conform (issue supportive fatwas), go silent, emigrate, or persist at high risk. Some institutional protection exists (state patronage, al-Azhar-type institutions) but is eroding.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, moderate_muslim_scholars, payer,
    organized, biographical, constrained, global).

% Non-Muslim communities (Christians, Yazidis, Shia in Sunni vanguard zones, etc.) in vanguard-controlled areas. Face disproportionate targeting: expulsion, enslavement, jizya at extortionate rates, destruction of heritage, forced conversion. Classical dhimmi protections are voided by the emergency/takfir framework. No theological exit; physical exit (flight) is often the only survival path but carries asset loss and displacement trauma.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, religious_minorities, payer,
    powerless, immediate, trapped, local).

% State intelligence, police, and military apparatus tasked with suppressing vanguard groups. They set the enforcement agenda (counter-terrorism laws, surveillance, kinetic operations, deradicalization programs). They bear operational costs and political blowback but have institutional mandate, resources, and career mobility. Exit is mobile: rotation, promotion, transfer. They are not victims of the constraint in the same sense — they are its active suppressors.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, security_services, agenda_setter,
    institutional, biographical, mobile, national).

% The analytical seat: sees the full structural asymmetry across all seats. Bears no extraction, collects no benefit, has full exit. Provides the classification frame.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jihad_quranic_corpus__revolutionary_vanguard_reading, vanguard_militants).
narrative_ontology:fixing_cost_class(jihad_quranic_corpus__revolutionary_vanguard_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates decentralized violent action across dispersed cells without central command: provides theological certainty (this is fard 'ayn, not optional), target prioritization (apostate rulers first, then occupiers, then collaborators), and legitimacy for resource extraction from populations. Solves the collective action problem of insurgency by making participation individually obligatory and religiously salvific.
% TRANSFER_FUNCTION: Moves lives, wealth, obedience, and territorial control from civilian populations, minority communities, and state institutions to vanguard militants and revolutionary scholars. The vanguard extracts taxation (ghanima/zakat), labor, recruits, and safe haven; scholars extract interpretive authority and donation flows. The transfer is enforced by violence and theological coercion.
% ABSENT_VOICES: The vast majority of Muslim scholars and populations who reject takfir methodology and emergency jurisprudence as deviations. They are structurally excluded by the vanguard's epistemic closure: dissent is ridda (apostasy). Also absent: international legal frameworks (Geneva Conventions, UN Charter) which the reading explicitly rejects as taghut (idolatry). They are in the global scholarly mainstream, in refugee camps, in diaspora — anywhere the vanguard's epistemic authority does not reach.
% DISAPPEARANCE_RATIONALE: If this constraint vanished overnight, vanguard groups would lose their primary recruitment and legitimacy engine; civilian populations in target zones would regain protected status under classical rules; moderate scholars would reclaim interpretive space; state monopoly on force would be reinforced. The insurgency ecosystem would lose its theological fuel — though grievances (occupation, authoritarianism) would persist, the fard 'ayn / takfir / emergency framework would no longer convert them into individual theological obligation.
% FOUNDING_PROBLEM: The perceived failure of Muslim states to defend Muslim lands (Palestine, Afghanistan, Kashmir, Chechnya, etc.) and the perceived apostasy of rulers who made peace with Israel, hosted US bases, or suppressed Islamist movements. Classical jihad doctrine required an imam and state apparatus — which were absent or complicit. The vanguard reading solved this by making jihad an individual duty triggered by emergency, bypassing the missing imam.
% FOUNDING_PROBLEM_CORROBORATION: Vanguard scholars (al-Qaradawi's early work, Azzam, Maqdisi, Suri) attest the problem is live: occupations persist, rulers remain apostate. Mainstream scholars (al-Azhar, al-Qaradawi's later work, major fiqh academies) attest the problem is substantially addressed by state-based defense and that the vanguard reading creates worse fitna (civil strife) than it solves. Independent analysts (Hegghammer, Wagemakers, Lacroix) document the genealogy: the reading emerges from Sayyid Qutb's hakimiyya + Afghan jihad praxis + Iraqi post-2003takfir expansion — not from a continuous scholarly tradition. No disinterested body corroborates the vanguard's claim that classical doctrine is 'suspended'.
narrative_ontology:disappearance_verdict(jihad_quranic_corpus__revolutionary_vanguard_reading, world_rearranges).
narrative_ontology:founding_problem_status(jihad_quranic_corpus__revolutionary_vanguard_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__revolutionary_vanguard_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(jihad_quranic_corpus__revolutionary_vanguard_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.92, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jihad_quranic_corpus__revolutionary_vanguard_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jihad_quranic_corpus__revolutionary_vanguard_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jihad_quranic_corpus__revolutionary_vanguard_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.92) reflects totalizing demand: the constraint claims the life, wealth, and obedience of entire populations in target zones. Suppression (0.88) is high because the constraint's persistence depends on violently suppressing alternative authorities (state, moderate scholars, rival groups) and eliminating exit (territorial control, ideological closure). Theater ratio (0.15) is low — the violence is functionally central, not performative. Accessibility collapse (0.78) is high: once the emergency doctrine is accepted, classical alternatives (defensive jihad, state monopoly, scholarly consensus) are structurally inaccessible. Resistance (0.35) is moderate: state counter-terrorism and moderate scholarly pushback exist but are fragmented and often reactive. The measurement series shows extractiveness rising from 0.65 (1979, Iranian Revolution / Afghan jihad inception) to 0.92 (2014, ISIS caliphate peak) as the victim set expanded and safeguards eroded. Theater ratio dipped as operational tempo increased (less performative, more functional), then rose slightly post-2014 as territorial control collapsed but ideological enforcement persisted. Suppression requirement tracks state/military counter-pressure.
 *
 * PERSPECTIVAL GAP:
 *   The vanguard_militant seat experiences the constraint as genuine coordination (theological certainty, brotherhood, purpose) with high personal cost — the engine should compute a mixed type for this seat. The revolutionary_scholar seat experiences it as coordination with status benefit and lower physical risk — likely computed as rope or tangled_rope. The civilian_populations_in_target_zones seat experiences pure extraction with no coordination benefit and no exit — computed as snare. The apostate_rulers and occupying_forces seats experience targeted elimination — snare. The moderate_muslim_scholars seat experiences delegitimization and suppression — snare or tangled_rope depending on whether they retain any platform. The analytical_observer seat sees the full structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Vanguard militants are identity_locked: professional identity (career path dependence — no post-vanguard career), relational identity (self constituted through the brotherhood), ideological identity (worldview makes exit apostasy). Revolutionary scholars are similarly identity_locked but with institutional exit options (could join mainstream institutions at status cost). Apostate rulers and occupying forces are trapped: no theological exit (declared apostates/enemies), physical exit possible but politically costly. Civilians in target zones are trapped: territorial enclosure, ideological framing as combatants removes protected status. Moderate scholars are constrained: can conform, go silent, or face delegitimization — some institutional mobility remains. Security services are mobile: institutional mandate, resources, exit via rotation. The analytical observer is analytical by definition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (defending Muslim lands from occupation and apostasy) is contested — some attest it remains live (ongoing occupations, authoritarian rule), others attest it has been superseded by the constraint's own expansion (victim set now includes the populations it claimed to defend). The constraint prevents mislabeling by making the extraction visible: the coordination function (defense of Islam) is the cover; the transfer function (life/resources from populations to vanguard) is the structure. The emergency doctrine is the ratchet: each crisis expands the victim set and never contracts it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading (revolutionary_vanguard_reading) of the contested kernel jihad_quranic_corpus. What does the sibling reading defensive_spiritual_reading change structurally?',
    'Compare victim sets, authority structures, and jurisdictional scope across readings. The defensive reading restricts victims to active aggressors, retains state monopoly on force, and preserves classical safeguards.',
    'If the defensive reading''s structural profile computes as a different constraint type (e.g., rope or tangled_rope), the kernel itself is not a single constraint but a family of structurally distinct claims — validating the ε-invariance decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Structural delta between revolutionary_vanguard_reading and defensive_spiritual_reading').

omega_variable(
    kernel_reading_committer_structure_2,
    'This constraint is one reading (revolutionary_vanguard_reading) of the contested kernel jihad_quranic_corpus. What does the sibling reading expansionist_legalist_reading change structurally?',
    'Compare authority conditions (imam requirement, invitation to Islam first), victim set restrictions (combatants only, proportionality), and jurisdictional calculus. The expansionist reading retains classical procedural safeguards while permitting offensive campaigns.',
    'If the expansionist reading''s structural profile computes as a different constraint type (e.g., tangled_rope), it confirms the kernel decomposes into multiple constraints with different ε values and stakeholder structures — not one constraint with observer-dependent metrics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure_2, conceptual, 'Structural delta between revolutionary_vanguard_reading and expansionist_legalist_reading').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (state counter-terrorism, military occupation) or internalized (ideological identity-fusion, theological necessity) for the vanguard militants themselves?',
    'Post-engagement trajectory analysis: if militants experience suppression as externally imposed (prison, drone strikes, state crackdown), it is structural. If they experience the constraint as self-imposed theological necessity that persists even when structural pressure eases, it is partially internalized.',
    'If internalized, the constraint''s effective suppression for the militant seat is higher than the structural measure suggests — the militant carries the suppression as identity. This affects computed per-seat type for vanguard_militants.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for vanguard militants').

omega_variable(
    civilian_victim_set_boundary,
    'Where exactly does the ''collective guilt'' logic draw the line between combatant and non-combatant civilians in target zones? Is it territorial (all residents of occupied/apostate territory), affiliational (families of security personnel), or doctrinal (non-adherents to the vanguard''s specific creed)?',
    'Analyze operational fatwas and targeting directives from vanguard groups across theaters. Track whether the boundary is stable or expands under pressure (mission creep).',
    'A wider boundary increases the victim set and extraction magnitude. If the boundary is doctrinally unstable (expands to absorb setbacks), the constraint exhibits extraction accumulation — a snare signature.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(civilian_victim_set_boundary, empirical, 'Boundary ambiguity in collective guilt victim assignment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__revolutionary_vanguard_reading, 1979, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jiha_tr_t1979, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 1979, 0.25).
narrative_ontology:measurement(jiha_tr_t1988, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 1988, 0.2).
narrative_ontology:measurement(jiha_tr_t1996, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 1996, 0.18).
narrative_ontology:measurement(jiha_tr_t2001, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 2001, 0.15).
narrative_ontology:measurement(jiha_tr_t2011, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 2011, 0.12).
narrative_ontology:measurement(jiha_tr_t2014, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 2014, 0.15).
narrative_ontology:measurement(jiha_tr_t2020, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 2020, 0.18).

% Extraction over time
narrative_ontology:measurement(jiha_be_t1979, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 1979, 0.65).
narrative_ontology:measurement(jiha_be_t1988, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 1988, 0.72).
narrative_ontology:measurement(jiha_be_t1996, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 1996, 0.78).
narrative_ontology:measurement(jiha_be_t2001, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 2001, 0.85).
narrative_ontology:measurement(jiha_be_t2011, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 2011, 0.89).
narrative_ontology:measurement(jiha_be_t2014, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 2014, 0.92).
narrative_ontology:measurement(jiha_be_t2020, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 2020, 0.91).

% Suppression requirement over time
narrative_ontology:measurement(jiha_su_t1979, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 1979, 0.55).
narrative_ontology:measurement(jiha_su_t1988, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 1988, 0.65).
narrative_ontology:measurement(jiha_su_t1996, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 1996, 0.72).
narrative_ontology:measurement(jiha_su_t2001, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 2001, 0.82).
narrative_ontology:measurement(jiha_su_t2011, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 2011, 0.88).
narrative_ontology:measurement(jiha_su_t2014, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 2014, 0.9).
narrative_ontology:measurement(jiha_su_t2020, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 2020, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jihad_quranic_corpus__revolutionary_vanguard_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.1).
narrative_ontology:affects_constraint(jihad_quranic_corpus__revolutionary_vanguard_reading, jihad_quranic_corpus__defensive_spiritual_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__revolutionary_vanguard_reading, jihad_quranic_corpus__expansionist_legalist_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__revolutionary_vanguard_reading, takfir_mechanism).
narrative_ontology:affects_constraint(jihad_quranic_corpus__revolutionary_vanguard_reading, emergency_jurisprudence_fiqh_al_awlawiyat).
narrative_ontology:affects_constraint(jihad_quranic_corpus__revolutionary_vanguard_reading, caliphate_declaration_constraint).

% DUAL FORMULATION NOTE:
% The jihad_quranic_corpus kernel decomposes into three constraint stories with distinct ε values and stakeholder structures. This reading (revolutionary_vanguard) has ε ≈ 0.92 (snare). The defensive_spiritual_reading likely has ε ≈ 0.15 (rope or mountain). The expansionist_legalist_reading likely has ε ≈ 0.45 (tangled_rope). They are linked via affects_constraints. The emergency jurisprudence and takfir mechanism are upstream enabling constraints; the caliphate declaration is a downstream institutionalization.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jihad_quranic_corpus__revolutionary_vanguard_reading, powerful, 0.35).
constraint_indexing:directionality_override(jihad_quranic_corpus__revolutionary_vanguard_reading, organized, 0.25).
constraint_indexing:directionality_override(jihad_quranic_corpus__revolutionary_vanguard_reading, institutional, 0.9).
constraint_indexing:directionality_override(jihad_quranic_corpus__revolutionary_vanguard_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
