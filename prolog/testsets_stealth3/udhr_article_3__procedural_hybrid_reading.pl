% ============================================================================
% CONSTRAINT STORY: udhr_article_3__procedural_hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_article_3__procedural_hybrid_reading, []).

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
 *   constraint_id: udhr_article_3__procedural_hybrid_reading
 *   human_readable: UDHR Article 3 - Procedural Hybrid Reading (Due-Process Floor Beneath Unresolved Substance)
 *   domain: constitutional_law/human_rights/political_philosophy
 *
 * SUMMARY:
 *   A near-universally ratified procedural floor governs how states may
 *   coerce persons: no torture, no custody without a judicially reviewable
 *   legal basis, emergency detention bounded and subject to review. Built in
 *   the aftermath of mid-century state atrocities, the arrangement is
 *   administered by domestic courts, UN treaty bodies, and regional courts,
 *   with a documentation-and-litigation sector auditing delivery. Its
 *   defining structural move is what it does not do: it settles the
 *   procedural question and deliberately leaves the substantive politics of
 *   liberty and welfare above it unresolved, which keeps accession cheap for
 *   states with opposed substantive commitments and keeps the floor itself
 *   the one element all parties defend. Delivery, however, is concentrated:
 *   where courts are strong the floor bites, and where they are weak it
 *   persists largely as form. The claim/metric gap is deliberate:
 *   claimed_type is authored from what is structurally true (genuine
 *   coordination plus real, seat-localized extraction, actively enforced)
 *   while the metrics are authored from descriptive operation; the engine
 *   computes per-seat verdicts from the structural data and neither
 *   reconciles to the claim. KEY AGENTS (by structural relationship): -
 *   detained_persons_and_suspects: primary protected class
 *   (powerless/trapped) - receives habeas review and torture prohibition
 *   directly - persons_under_state_jurisdiction: diffuse beneficiary base
 *   (moderate/constrained) - carries the floor as background condition of
 *   legitimate policing - state_security_establishments: primary cost-bearing
 *   seat (powerful/constrained) - surrenders coercive tools, retains budgets,
 *   personnel, and implementation pace - domestic_judiciaries:
 *   jurisdiction-collecting enforcer (institutional/identity_locked) -
 *   administers review, absorbs political attack -
 *   un_treaty_bodies_and_regional_courts: transnational administrator
 *   (institutional/identity_locked) - review, visits, findings; no coercive
 *   power of its own - ratifying_states_legislatures: agenda-setting
 *   implementer (institutional/constrained) - enacted and funds the machinery
 *   - human_rights_ngo_sector: mobilized monitor-beneficiary
 *   (organized/mobile) - documents the gap its relevance tracks -
 *   populations_under_nominal_protection: excluded voice (powerless/trapped)
 *   - holds a guarantee it cannot invoke - analytical_observers: analytical
 *   seat - measures delivery against ratification
 *
 * KEY AGENTS:
 *   - detained_persons_and_suspects: primary protected class (powerless/trapped) - direct recipient of habeas review and torture prohibition; protection arrives through others' petitions when it arrives at all
 *   - persons_under_state_jurisdiction: diffuse beneficiary base (moderate/constrained) - carries the floor as a background condition and pays for it indirectly through taxation
 *   - state_security_establishments: primary cost-bearing seat (powerful/constrained) - surrenders specific coercive tools, retains everything else, presses hardest against the limits in emergencies
 *   - domestic_judiciaries: jurisdiction-collecting enforcer (institutional/identity_locked) - administers detention review, absorbs caseload and political retaliation
 *   - un_treaty_bodies_and_regional_courts: transnational administrator (institutional/identity_locked) - periodic review, country visits, individual communications; publicity is their only lever
 *   - ratifying_states_legislatures: agenda-setting implementer (institutional/constrained) - enacted the implementing statutes and funds the machinery; denunciation is priced out
 *   - human_rights_ngo_sector: mobilized monitor-beneficiary (organized/mobile) - shadow-reporting, litigation, campaigning; funding tracks the arrangement's activity cycle
 *   - populations_under_nominal_protection: excluded voice (powerless/trapped) - formal guarantee without functioning delivery; the arrangement's failure surface
 *   - analytical_observers: analytical seat (analytical/analytical) - comparative scholars measuring delivery against ratification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_article_3__procedural_hybrid_reading, 0.4).
domain_priors:suppression_score(udhr_article_3__procedural_hybrid_reading, 0.35).
domain_priors:theater_ratio(udhr_article_3__procedural_hybrid_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__procedural_hybrid_reading, tangled_rope).
narrative_ontology:human_readable(udhr_article_3__procedural_hybrid_reading, "UDHR Article 3 - Procedural Hybrid Reading (Due-Process Floor Beneath Unresolved Substance)").
narrative_ontology:topic_domain(udhr_article_3__procedural_hybrid_reading, "constitutional_law/human_rights/political_philosophy").

domain_priors:requires_active_enforcement(udhr_article_3__procedural_hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__procedural_hybrid_reading, '1d881332-e579-4479-9201-7ae8553d727d').
narrative_ontology:cs_kernel_codification('1d881332-e579-4479-9201-7ae8553d727d', fixed_text).
narrative_ontology:cs_authority_grounding('1d881332-e579-4479-9201-7ae8553d727d', lineage).
narrative_ontology:cs_interpretation_layer_present('1d881332-e579-4479-9201-7ae8553d727d').
narrative_ontology:cs_reading_relation('1d881332-e579-4479-9201-7ae8553d727d', udhr_article_3__negative_liberty_reading, coexists_with).
narrative_ontology:cs_reading_relation('1d881332-e579-4479-9201-7ae8553d727d', udhr_article_3__positive_entitlement_reading, coexists_with).
narrative_ontology:cs_axiom('1d881332-e579-4479-9201-7ae8553d727d', foundational, procedural_legitimacy_separable_from_substance).
narrative_ontology:cs_axiom_status(procedural_legitimacy_separable_from_substance, holdable).
narrative_ontology:cs_axiom_grounding('1d881332-e579-4479-9201-7ae8553d727d', procedural_legitimacy_separable_from_substance, conventional).
narrative_ontology:cs_axiom('1d881332-e579-4479-9201-7ae8553d727d', secondary, accountability_duty_over_emergency_prerogative).
narrative_ontology:cs_axiom_status(accountability_duty_over_emergency_prerogative, holdable).
narrative_ontology:cs_axiom_grounding('1d881332-e579-4479-9201-7ae8553d727d', accountability_duty_over_emergency_prerogative, deontological).
narrative_ontology:cs_reference_frame('1d881332-e579-4479-9201-7ae8553d727d', settled_procedural_floor_open_substance).
narrative_ontology:cs_drift_state('1d881332-e579-4479-9201-7ae8553d727d', contemporary_backsliding_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1d881332-e579-4479-9201-7ae8553d727d', '2026-08-05T12:00:00Z').
narrative_ontology:cs_kernel_id(udhr_article_3__procedural_hybrid_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, persons_under_state_jurisdiction).
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, detained_persons_and_suspects).
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, domestic_judiciaries).
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, un_treaty_bodies_and_regional_courts).
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, human_rights_ngo_sector).
narrative_ontology:constraint_victim(udhr_article_3__procedural_hybrid_reading, state_security_establishments).
narrative_ontology:constraint_vindicates(udhr_article_3__procedural_hybrid_reading, procedural_rule_of_law_doctrine).
narrative_ontology:constraint_vindicates(udhr_article_3__procedural_hybrid_reading, habeas_jurisdiction_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% People in custody or facing state prosecution. When the floor functions, they receive prompt presentation before a judge, access to counsel, and freedom from interrogation abuse; when it fails, they are the ones beaten in cells no inspector visits. They cannot leave the jurisdiction that holds them and rarely have resources to litigate; their protection arrives through others' petitions or inspectors' visits, not their own effort.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, detained_persons_and_suspects, beneficiary,
    powerless, biographical, trapped, global).

% Everyone living under a ratifying state's authority. They carry the arrangement as a background condition: policing, arrest, and imprisonment proceed inside known procedural limits, and the state's legitimacy partly rests on keeping them. They pay indirectly through taxation that funds courts and reporting obligations, and they bear the risk of the floor's erosion in emergencies they did not choose.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, persons_under_state_jurisdiction, beneficiary,
    moderate, generational, constrained, global).

% National courts and judges. Habeas-type review gives them jurisdiction over executive detention - authority, caseload, and institutional standing flow from administering the floor. They also absorb the costs: political attacks when they release security suspects, backlog strain, and, in backsliding states, packing and purges aimed at exactly this jurisdiction. Exiting the role would mean ceasing to be a constitutional court at all.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, domestic_judiciaries, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(udhr_article_3__procedural_hybrid_reading, domestic_judiciaries, agenda_setter).

% Police, intelligence services, and military commands. They surrender specific tools - torture, unreviewable custody, open-ended emergency detention - and absorb compliance costs of documentation, counsel access, and inspection. They keep everything else: budgets, personnel, operational initiative, and the pace of implementation, which they slow wherever review threatens operations. During declared emergencies they press hardest against the limits, and they retain the substantive capacity the floor merely regulates.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, state_security_establishments, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(udhr_article_3__procedural_hybrid_reading, state_security_establishments, agenda_setter).

% Parliaments and ratifying governments. They enacted the implementing statutes, fund the courts and reporting machinery, and answer diplomatically for violations. Their exit - denouncing the covenants - carries reputational and treaty-network costs few are willing to pay, so adjustment happens inside the framework they set.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, ratifying_states_legislatures, agenda_setter,
    institutional, generational, constrained, national).

% UN Human Rights Committee, Committee Against Torture, subcommittee on prevention, regional courts and commissions. They administer periodic review, hear individual communications, conduct country visits, and publish findings. Their mandates, budgets, and professional authority exist only through the arrangement; they have no coercive power of their own and rely on publicity, dialogue, and domestic courts to convert findings into change.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, un_treaty_bodies_and_regional_courts, agenda_setter,
    institutional, generational, identity_locked, global).

% Documentation and litigation organizations. They monitor custodial conditions, shadow-report to treaty bodies, litigate landmark cases, and campaign on findings. Funding, staffing, and institutional relevance track the arrangement's activity cycle; they could redirect missions elsewhere if it dissolved, at the cost of accumulated expertise.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, human_rights_ngo_sector, beneficiary,
    organized, biographical, mobile, global).

% People in ratifying states where the floor exists on paper only - no working habeas, inspectors bribed or barred, courts packed. The arrangement gives them a formal guarantee they cannot invoke and a reporting record that documents abuse without stopping it. They are not seated in any review process; their cases enter the record only when smuggled out by relatives, journalists, or exiles.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, populations_under_nominal_protection, excluded,
    powerless, biographical, trapped, global).

% Comparative constitutional scholars, historians of human rights, and legal philosophers. They trace the floor's genealogy, measure delivery against ratification, and map where procedural form and substantive practice diverge. They collect nothing and pay nothing; their seat is the record itself.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, analytical_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(udhr_article_3__procedural_hybrid_reading, un_treaty_bodies_and_regional_courts).
narrative_ontology:fixing_cost_class(udhr_article_3__procedural_hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a uniform procedural floor governing how states may coerce persons: no torture, no detention without a judicially reviewable legal basis, emergency detentions bounded and reviewable. Each ratifying state forgoes arbitrary-coercion tools in exchange for reciprocal protection of its nationals everywhere and domestic legitimacy; the floor converts an unstable mutual fear - each society vulnerable to its own and other states' arbitrariness - into settled restraint administered by courts.
% TRANSFER_FUNCTION: Moves coercive discretion from executive and security organs to independent judiciaries (jurisdiction over detention), moves compliance costs onto state budgets (courts, documentation, inspection, reporting), and moves assurance to all persons under jurisdiction. Secondarily, moves documentary rents - mandates, budgets, careers - to the monitoring complex that administers and audits the floor.
% ABSENT_VOICES: Persons held in jurisdictions where the floor is nominal - tortured or disappeared despite ratification, pre-trial detainees without counsel or working habeas access - would object that the guarantee protects them only on paper. They are outside the reviewing rooms, often unable to petition at all; their objection enters the record only when their cases surface as individual communications or smuggled testimony, years late.
% DISAPPEARANCE_RATIONALE: If the procedural floor vanished overnight, detention regimes worldwide would revert toward unreviewable executive custody; the torture prohibition would lose its adjudicative anchor; judiciaries would lose detention jurisdiction; the monitoring architecture would dissolve; and the interstate trust assumptions embedded in extradition, asylum, and diplomatic protection would unwind. The substantive liberty/welfare politics the floor sits beneath would continue, but without the shared procedural ground they currently presuppose.
% FOUNDING_PROBLEM: The mid-twentieth-century record of state-run torture, disappearance, and arbitrary detention showed that domestic law could not reliably restrain states that captured it; the arrangement was built to create a transnational, judicially administrable floor against arbitrary state violence against persons.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: UN Special Rapporteur on Torture and Subcommittee on Prevention findings document continuing custodial torture in ratifying states; investigative journalism on custodial deaths and the forensic-pathology literature independently attest ongoing arbitrary detention and abuse; comparative criminal-justice scholarship documents habeas non-function in nominal-ratification jurisdictions. Monitoring NGOs corroborate as well but sit partially inside the beneficiary set, so evidentiary weight rests on the state-practice and scholarly record.
narrative_ontology:disappearance_verdict(udhr_article_3__procedural_hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_article_3__procedural_hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_article_3__procedural_hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(udhr_article_3__procedural_hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_article_3__procedural_hybrid_reading, 0.4, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_article_3__procedural_hybrid_reading_tests).
:- end_tests(udhr_article_3__procedural_hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.40: by this reading's own lights the floor delivers most of what it promises where institutions function, and the residual extraction concentrates in three places - procedural compliance that masks substantive deprivation where review is weak, compliance-cost asymmetry between compliant and violating states, and the documentary rents the monitoring complex collects from the persistent gap. Suppression (0.35) reflects enforcement machinery that coerces states - treaty review, court compulsion, reputational and legal sanction - while leaving the substantive politics above the floor entirely unsuppressed; the suppression_requirement series is authored because the story traces a real enforcement-capacity arc: build-out from the declaration through the covenant and convention era (rising to 0.48 at t=44), then decay under great-power defiance and democratic backsliding (0.35 at t=77). Theater (0.32) is real but not dominant: ratification ceremony and periodic reporting in weak-delivery states are substantially performative, while habeas review in consolidated systems is functional. Accessibility collapse is low (0.20): alternative rights framings and the substantive politics above the floor remain fully live - that openness is this reading's defining move, not an accident. Resistance (0.55) is sustained and seat-specific: security establishments press against the limits in every emergency, and backslidding governments attack the reviewing courts themselves. The measurement series show two crisis-driven oscillations on one shared grid (instrument-building convergence, war-on-terror divergence, partial reconvergence, backsliding drift); the oscillation is partly mechanism - each emergency strains the floor and the post-crisis settlement decides how much slack the payer seat retains - and the end-state scalars were read at the post-reconvergence phase, sitting between trough and spike values. The receipt surface is authored from the situations: gains from the arrangement's residual extraction demonstrably accrue to the transnational administrators whose mandates exist only through it (receipt is not benefit - the protected classes benefit enormously and receive none of the extraction), and closing the delivery gap is prohibitive for every seat positioned to attempt it: violating executives face internal political costs exceeding any benefit they perceive, and the monitoring seats that could escalate lack coercive power and partly subsist on the gap.
 *
 * PERSPECTIVAL GAP:
 *   The same structure presents four different faces. To the security establishment it is a confiscation of tools and a brake on operations - the payer seat should compute the most extractive per-seat verdict. To detained persons and the general population it is protection - near-beneficiary seats. To domestic judiciaries it is jurisdiction and standing, purchased with political exposure. To populations under nominal protection it is paper - a guarantee they cannot invoke, which is the arrangement's failure surface rather than either coordination or extraction. Inter-institutionally, domestic courts and treaty bodies administer the same floor with different leverage: courts can compel, treaty bodies can only publicize, so the same violation costs one government a lost case and another a diplomatic embarrassment. Same-level divergence appears twice: two institutional administrator seats (courts, treaty bodies) differ in coercive leverage, and two powerless seats (detained persons in functioning systems, nominal-protection populations) differ in delivery, not power. Identity-lock binds both administrator seats: a constitutional court that abandoned detention review would cease to be what it is, and a treaty body has no existence outside its mandate; if that frame broke - courts declining review, bodies refusing findings - the floor's enforcement would collapse faster than any metric series anticipates. The engine computes these divergences from the structural data; the authored claim adjudicates none of them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low d: detained persons (direct protection, no offsetting gain, trapped exit) sit nearest the beneficiary end; the general population is near-beneficiary with a diffuse indirect tax cost; judiciaries collect jurisdiction but absorb caseload and political backlash, holding them slightly off the floor; treaty bodies are constituted by the arrangement and sit near-full beneficiary; NGOs benefit with mobile exit damping further. The single victim declaration - state security establishments - drives the derivation toward the full-target end, and the story overrides the 'powerful' atom to 0.65 because the derivation cannot see retained capacity: they keep budgets, personnel, operational initiative, and implementation pace, and they harvest legitimacy spillovers, so they are partial targets rather than full ones. Populations under nominal protection are deliberately left out of both arrays: their position is neither paid extraction nor received protection but failed delivery, and forcing them into either array would misstate the structure; their seat is carried instead through the nominal_vs_delivered_protection omega and the excluded stakeholder role.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - restraining state violence against persons - is live, so no mandatrophy resolution is declared. Lifecycle risk runs in both directions and the temporal series watch both. Piton-drift: if delivery decays while ratification ceremony continues, theater_ratio climbs and the arrangement becomes performance; the post-2001 theater elevation is the early signature. Snare-drift: if emergency ratchets accumulate and the monitoring complex entrenches rents from the gap, base_extractiveness trends upward and the payer seats darken toward snare. Mislabeling guards: calling the whole arrangement a snare would erase the genuine coordination that keeps ratification near-universal and torture normatively condemned even by violators; calling it a rope would erase the documented capturer seat and the failed-delivery population. The tangled-rope claim holds both halves. The receipt surface records the extraction side without reclassifying the coordination side: gains accrue to a named administrator seat, fixing is prohibitive for every seat that could attempt it, and the arrangement nonetheless persists because its coordination value exceeds what it extracts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of kernel udhr_article_3 - the procedural_hybrid_reading. What would change structurally if a sibling reading were instantiated instead?',
    'Not resolvable by data within this story: the choice of reading is a commitment-system fact. Resolution occurs at the kernel level if a jurisdiction or tribunal formally adopts a sibling reading (for example, a constitutional court holding that ''security of person'' entails material provision obligations), which would activate the sibling constraint file and re-route this reading''s victim and beneficiary structure.',
    'Under the negative-liberty sibling the guarantee narrows to freedom from state coercion and the welfare dimension drops out entirely; under the positive-entitlement sibling the victim set expands to the materially deprived and the state becomes the primary payer seat. This reading''s moderate epsilon, its beneficiary/payer structure, and its coexistence relations are all indexed to the hybrid choice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer-frame routing: one reading of the Article 3 kernel; sibling readings would restructure victim sets and payer seats.').

omega_variable(
    nominal_vs_delivered_protection,
    'Does the procedural floor deliver actual habeas access and torture protection across ratifying states, or is delivery concentrated in consolidated democracies while remaining nominal elsewhere?',
    'Cross-reference ratification status with custodial-death statistics, habeas petition filing and grant rates, national preventive-mechanism visit coverage, and treaty-body individual-communication outcomes per capita.',
    'If delivery is thin outside consolidated democracies, effective extraction for the powerless seats is far higher than the authored epsilon suggests and the payer-side picture darkens toward snare for those seats; if delivery is broad, the current epsilon stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nominal_vs_delivered_protection, empirical, 'Gap between formal guarantee and delivered protection across jurisdictions.').

omega_variable(
    emergency_ratchet_permanence,
    'Do declared emergencies permanently ratchet detention practice past the procedural floor, or does practice snap back when the emergency ends?',
    'Longitudinal comparison of detention rates, review backlogs, and interrogation-practice indicators across pre-emergency, emergency, and post-emergency windows for states with declared emergencies since 2001.',
    'A permanent ratchet means each crisis cycle raises the floor''s steady-state epsilon and the oscillating measurement series understates trend extraction; snap-back means the oscillation is strain, not accumulation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergency_ratchet_permanence, empirical, 'Whether emergency-period detention practices normalize permanently or revert.').

omega_variable(
    proceduralism_masking_boundary,
    'Is lawful-but-oppressive detention - procedurally clean custody with unjust substance - a failure of this reading''s guarantee, or a boundary this reading correctly declines to police, leaving substance to the sibling readings'' domain?',
    'Conceptual: test whether the reading''s own axioms (procedural legitimacy separable from substance) generate the observed gap as a consequence rather than a defect; empirical supplement: compare jurisdictions where procedural delivery is strong but substantive detention policy is harsh.',
    'If failure, this reading''s epsilon is understated and its coherence as a distinct reading weakens; if boundary, the residual extraction belongs to the substantive readings'' referent and this story''s epsilon is if anything overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proceduralism_masking_boundary, conceptual, 'Whether proceduralism''s substantive blind spot is defect or design.').

omega_variable(
    enforcement_decay_vs_maturation,
    'Does the post-2001 decline in the suppression_requirement series represent enforcement decay (machinery losing grip on great-power and backsliding violators) or enforcement maturation (norms internalized, less active force needed)?',
    'Compare the trajectory of treaty-body finding acceptance, domestic-court deference rates in security cases, and new-ratification velocity before and after 2001; decay predicts rising defiance with stable ratification, maturation predicts falling violation with stable machinery.',
    'Decay implies the floor is weakening and measured epsilon will rise ahead of the series; maturation implies consolidation and the series overstates fragility.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_decay_vs_maturation, empirical, 'Interpretation of the enforcement-capacity trajectory after 2001.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_article_3__procedural_hybrid_reading, 0, 77).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_tr_t0, udhr_article_3__procedural_hybrid_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement_basis(udhr_tr_t0, observed).
narrative_ontology:measurement(udhr_tr_t11, udhr_article_3__procedural_hybrid_reading, theater_ratio, 11, 0.4).
narrative_ontology:measurement_basis(udhr_tr_t11, observed).
narrative_ontology:measurement(udhr_tr_t22, udhr_article_3__procedural_hybrid_reading, theater_ratio, 22, 0.36).
narrative_ontology:measurement_basis(udhr_tr_t22, observed).
narrative_ontology:measurement(udhr_tr_t33, udhr_article_3__procedural_hybrid_reading, theater_ratio, 33, 0.28).
narrative_ontology:measurement_basis(udhr_tr_t33, observed).
narrative_ontology:measurement(udhr_tr_t44, udhr_article_3__procedural_hybrid_reading, theater_ratio, 44, 0.24).
narrative_ontology:measurement_basis(udhr_tr_t44, observed).
narrative_ontology:measurement(udhr_tr_t55, udhr_article_3__procedural_hybrid_reading, theater_ratio, 55, 0.37).
narrative_ontology:measurement_basis(udhr_tr_t55, observed).
narrative_ontology:measurement(udhr_tr_t66, udhr_article_3__procedural_hybrid_reading, theater_ratio, 66, 0.35).
narrative_ontology:measurement_basis(udhr_tr_t66, observed).
narrative_ontology:measurement(udhr_tr_t77, udhr_article_3__procedural_hybrid_reading, theater_ratio, 77, 0.32).
narrative_ontology:measurement_basis(udhr_tr_t77, observed).

% Extraction over time
narrative_ontology:measurement(udhr_be_t0, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement_basis(udhr_be_t0, observed).
narrative_ontology:measurement(udhr_be_t11, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 11, 0.46).
narrative_ontology:measurement_basis(udhr_be_t11, observed).
narrative_ontology:measurement(udhr_be_t22, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 22, 0.38).
narrative_ontology:measurement_basis(udhr_be_t22, observed).
narrative_ontology:measurement(udhr_be_t33, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 33, 0.33).
narrative_ontology:measurement_basis(udhr_be_t33, observed).
narrative_ontology:measurement(udhr_be_t44, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 44, 0.31).
narrative_ontology:measurement_basis(udhr_be_t44, observed).
narrative_ontology:measurement(udhr_be_t55, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 55, 0.41).
narrative_ontology:measurement_basis(udhr_be_t55, observed).
narrative_ontology:measurement(udhr_be_t66, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 66, 0.43).
narrative_ontology:measurement_basis(udhr_be_t66, observed).
narrative_ontology:measurement(udhr_be_t77, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 77, 0.4).
narrative_ontology:measurement_basis(udhr_be_t77, observed).

% Suppression requirement over time
narrative_ontology:measurement(udhr_su_t0, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement_basis(udhr_su_t0, observed).
narrative_ontology:measurement(udhr_su_t11, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 11, 0.18).
narrative_ontology:measurement_basis(udhr_su_t11, observed).
narrative_ontology:measurement(udhr_su_t22, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 22, 0.3).
narrative_ontology:measurement_basis(udhr_su_t22, observed).
narrative_ontology:measurement(udhr_su_t33, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 33, 0.42).
narrative_ontology:measurement_basis(udhr_su_t33, observed).
narrative_ontology:measurement(udhr_su_t44, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 44, 0.48).
narrative_ontology:measurement_basis(udhr_su_t44, observed).
narrative_ontology:measurement(udhr_su_t55, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 55, 0.46).
narrative_ontology:measurement_basis(udhr_su_t55, observed).
narrative_ontology:measurement(udhr_su_t66, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 66, 0.4).
narrative_ontology:measurement_basis(udhr_su_t66, observed).
narrative_ontology:measurement(udhr_su_t77, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 77, 0.35).
narrative_ontology:measurement_basis(udhr_su_t77, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_article_3__procedural_hybrid_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(udhr_article_3__procedural_hybrid_reading, udhr_article_3__negative_liberty_reading).
narrative_ontology:affects_constraint(udhr_article_3__procedural_hybrid_reading, udhr_article_3__positive_entitlement_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Article 3' conflates three structurally distinct claims over one kernel text; per the epsilon-invariance principle each is authored as a separate story with its own epsilon, beneficiaries, and victims, linked through network.affects_constraints. This file instantiates the procedural-hybrid reading (moderate epsilon: the floor delivers where institutions function and persists as form where they do not). The negative-liberty sibling authors a narrower guarantee centered on freedom from state coercion; the positive-entitlement sibling authors an arrangement with the state as primary payer seat and the materially deprived as victims. This reading sits upstream of both in delivery terms - each sibling's protections or provisions are administered through the procedural machinery this reading constitutes - while coexisting with both at the level of political commitment. Sibling files carry reciprocal notes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(udhr_article_3__procedural_hybrid_reading, powerful, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
