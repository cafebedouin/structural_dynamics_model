% ============================================================================
% CONSTRAINT STORY: combatant_status_definition__functional_protection_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-13
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_combatant_status_definition__functional_protection_reading, []).

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
 *   constraint_id: combatant_status_definition__functional_protection_reading
 *   human_readable: Common Article 3 Status-Independent Minimum Protections (Functional Protection Reading)
 *   domain: international_humanitarian_law/law_of_armed_conflict
 *
 * SUMMARY:
 *   This story instantiates the functional protection reading of the
 *   combatant-status kernel: the Common Article 3 minimum protections attach
 *   to the fact of detention or helplessness, not to any combatant
 *   classification, so no status determination stands between capture and
 *   humane treatment. The arrangement is a treaty-born coordination floor
 *   that has consolidated into customary law, binding state forces and
 *   organized non-state groups alike in every internal conflict. KEY AGENTS
 *   (by structural relationship): - detained_persons_in_niac: Primary
 *   beneficiary (powerless/trapped) — receives the unconditional floor at the
 *   moment of capture - civilian_internees: Primary beneficiary
 *   (powerless/trapped) — long-duration protection without any category to
 *   invoke - hors_de_combat_wounded_sick: Primary beneficiary
 *   (powerless/trapped) — acute-window collection and care obligations -
 *   non_state_armed_groups: Cost-bearing participant with reciprocal coverage
 *   (organized/constrained) — bound without consent, covered when captured -
 *   state_detention_authorities: Cost-bearing participant
 *   (powerful/constrained) — carries interrogation, judicial-process, and
 *   condition costs that reclassification cannot reduce -
 *   geneva_convention_high_contracting_parties: Agenda setter
 *   (institutional/generational) — owns the text and the amendment path -
 *   icrc_protection_mandate: Agenda setter with incidental institutional gain
 *   (institutional/identity-locked) — administers monitoring, access, and
 *   interpretation - international_war_crimes_tribunals: Observer enforcing
 *   through prosecution (institutional/identity-locked) -
 *   international_human_rights_ngos: Observer applying public pressure
 *   (organized/mobile) - military_status_determination_officers: Excluded
 *   voice (powerful/identity-locked) — their classification function is
 *   outside this floor's operation. Sibling readings of the same kernel are
 *   separate constraint files linked through the network section; this file
 *   authors only the functional reading's structure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__functional_protection_reading, 0.16).
domain_priors:suppression_score(combatant_status_definition__functional_protection_reading, 0.42).
domain_priors:theater_ratio(combatant_status_definition__functional_protection_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, extractiveness, 0.16).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__functional_protection_reading, rope).
narrative_ontology:human_readable(combatant_status_definition__functional_protection_reading, "Common Article 3 Status-Independent Minimum Protections (Functional Protection Reading)").
narrative_ontology:topic_domain(combatant_status_definition__functional_protection_reading, "international_humanitarian_law/law_of_armed_conflict").

domain_priors:requires_active_enforcement(combatant_status_definition__functional_protection_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__functional_protection_reading, 'b8994ab8-b36e-418f-a32f-b6533f34e38d').
narrative_ontology:cs_kernel_codification('b8994ab8-b36e-418f-a32f-b6533f34e38d', fixed_text).
narrative_ontology:cs_authority_grounding('b8994ab8-b36e-418f-a32f-b6533f34e38d', lineage).
narrative_ontology:cs_interpretation_layer_present('b8994ab8-b36e-418f-a32f-b6533f34e38d').
narrative_ontology:cs_reading_relation('b8994ab8-b36e-418f-a32f-b6533f34e38d', combatant_status_definition__state_centric_reading, influences).
narrative_ontology:cs_reading_relation('b8994ab8-b36e-418f-a32f-b6533f34e38d', combatant_status_definition__national_liberation_reading, coexists_with).
narrative_ontology:cs_axiom('b8994ab8-b36e-418f-a32f-b6533f34e38d', foundational, humane_treatment_floor_is_status_independent).
narrative_ontology:cs_axiom_status(humane_treatment_floor_is_status_independent, holdable).
narrative_ontology:cs_axiom_grounding('b8994ab8-b36e-418f-a32f-b6533f34e38d', humane_treatment_floor_is_status_independent, deontological).
narrative_ontology:cs_axiom('b8994ab8-b36e-418f-a32f-b6533f34e38d', secondary, judicial_process_precedes_penalty_execution).
narrative_ontology:cs_axiom_status(judicial_process_precedes_penalty_execution, holdable).
narrative_ontology:cs_axiom_grounding('b8994ab8-b36e-418f-a32f-b6533f34e38d', judicial_process_precedes_penalty_execution, conventional).
narrative_ontology:cs_reference_frame('b8994ab8-b36e-418f-a32f-b6533f34e38d', status_independent_humanitarian_floor).
narrative_ontology:cs_drift_state('b8994ab8-b36e-418f-a32f-b6533f34e38d', post_tadic_post_hamdan_jurisprudence, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('b8994ab8-b36e-418f-a32f-b6533f34e38d', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__functional_protection_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, detained_persons_in_niac).
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, civilian_internees).
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, hors_de_combat_wounded_sick).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, non_state_armed_groups).
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, icrc_protection_mandate).
narrative_ontology:constraint_victim(combatant_status_definition__functional_protection_reading, non_state_armed_groups).
narrative_ontology:constraint_victim(combatant_status_definition__functional_protection_reading, state_detention_authorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Held in custody by state forces or organized armed groups during internal conflicts. Their treatment — freedom from torture, execution without trial, hostage-taking, and indignity; access to care when wounded — attaches to the fact of detention itself, with no classification hearing standing between capture and protection. Exit from the arrangement is physically unavailable: they leave it only through release, transfer, or death.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, detained_persons_in_niac, beneficiary,
    powerless, immediate, trapped, global).

% Civilians deprived of liberty for security or administrative reasons in internal armed conflicts. They hold no combatant category to invoke and need none: internment conditions, contact with families, and procedural safeguards flow from the same unconditional baseline. Their duration in custody is typically measured in years of a biographical span.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, civilian_internees, beneficiary,
    powerless, biographical, trapped, global).

% Fighters and civilians rendered helpless by wounds, sickness, shipwreck, or capture. The arrangement obliges each side to collect and care for them without adverse distinction founded on which force they served. Their window of vulnerability is immediate and acute; the protection operates at the moment of rescue or capture.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, hors_de_combat_wounded_sick, beneficiary,
    powerless, immediate, trapped, global).

% Organized armed parties to internal conflicts. They bear binding obligations — interrogation limits, humane conditions, judicial process before any execution — without having signed anything, through customary application. Their own captured members are covered by the same floor when taken by governments or rival groups. They cannot exit the obligation short of demobilization, and their compliance is monitored by delegations they can admit or refuse.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, non_state_armed_groups, payer,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(combatant_status_definition__functional_protection_reading, non_state_armed_groups, beneficiary).

% Military and intelligence units operating detention facilities in internal conflicts. They carry the operational costs: restricted interrogation technique menus, courts or regular procedure required before any penal execution, record-keeping and access for visiting delegations. Reclassifying a detainee's status does not relieve these duties under this arrangement, which closes the reinterpretation route that would otherwise lower their costs.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, state_detention_authorities, payer,
    powerful, biographical, constrained, national).

% The states that drafted, adopted, and periodically reaffirm the 1949 Conventions, and that convene diplomatic conferences when the text needs development. They administer the regime collectively — amendment requires a new conference, denunciation is formally available but practically unprecedented — and they fund and staff the enforcement ecosystem their diplomacy created.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, geneva_convention_high_contracting_parties, agenda_setter,
    institutional, generational, constrained, global).

% The organization that visits detention sites, transmits confidential findings to detaining authorities, negotiates access in every internal conflict, and maintains the authoritative commentaries on the minimum protections. Its visiting mandate and access privileges derive from the regime it polices; its institutional identity is fused with the humanitarian mission, making withdrawal from the arrangement unthinkable for it. It accrues operational relevance and funding from the regime's continuation.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, icrc_protection_mandate, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(combatant_status_definition__functional_protection_reading, icrc_protection_mandate, beneficiary).

% Ad hoc chambers and permanent courts that prosecute mistreatment of detained persons as war crimes, and whose jurisprudence has affirmed that the minimum protections bind in every armed conflict regardless of classification. Their mandates are created case-by-case and expire; while seated, their function and the arrangement's enforcement are the same activity.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, international_war_crimes_tribunals, observer,
    institutional, biographical, identity_locked, continental).

% Monitoring organizations that document detention conditions, publish violation reports, and campaign for access and accountability. They operate outside the confidential channel, can disengage from any given conflict file without institutional loss, and shift attention between crises as their advocacy priorities move.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, international_human_rights_ngos, observer,
    organized, biographical, mobile, global).

% Service legal advisors and review-board personnel who screen detainees for combatant classification and administer the distinction architecture inside national defense establishments. Under this arrangement their determinations have no bearing on the treatment floor, so their professional function sits outside the protection regime's operation; their objection — that classification remains necessary for the fuller privilege regime above the floor — is voiced in ministry offices and service schools, not in the humanitarian forums where the floor is interpreted and monitored.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, military_status_determination_officers, excluded,
    powerful, biographical, identity_locked, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(combatant_status_definition__functional_protection_reading, diffuse).
narrative_ontology:fixing_cost_class(combatant_status_definition__functional_protection_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a guaranteed minimum-treatment baseline in conflicts where no prisoner-of-war regime applies: every party to an internal conflict, state or organized non-state, knows in advance what treatment its captured opponents and detained civilians must receive, taking treatment standards off the battlefield status dispute and giving humanitarian access negotiation a common reference line.
% TRANSFER_FUNCTION: Moves restraint obligations onto detaining authorities — interrogation limits, judicial process before any death sentence, humane conditions, collection and care of the wounded — and moves physical security, dignity, and due-process goods to detained persons. The compliance costs concentrate on the detaining side at the moment of detention; the protective goods concentrate on the detained side for its duration.
% ABSENT_VOICES: Military status-determination officers would object that the reading dissolves the classification architecture they administer; intelligence services seeking interrogation latitude would object to the unconditional technique restrictions. Both objections are housed inside national security bureaucracies and defense educational institutions rather than in the diplomatic conferences, tribunal dockets, or monitoring channels where the floor's content is settled.
% DISAPPEARANCE_RATIONALE: If the status-independent floor vanished overnight, every detainee's protections in internal conflicts would reattach to contested classification hearings; the unlawful-combatant gap would reopen in each new conflict; visiting delegations would lose the common baseline their access negotiations cite; and persons held by non-state armed groups would hold no protection claim at all, since no status category reaches them.
% FOUNDING_PROBLEM: After the Spanish Civil War and the Second World War, the 1949 drafters confronted the likelihood that most future conflicts would be internal. Persons detained in civil wars fell outside every existing category — neither prisoners of war nor civilian internees under the occupying-power regime — leaving them wholly unprotected. Common Article 3 was adopted in the conference's final days to guarantee a minimum floor precisely where the status categories fail.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the ICTY Tadić jurisprudence — an independent judicial body — affirming both the minimum content and the customary reach of the floor; the military manuals of states that never ratified Additional Protocol I nonetheless acknowledging the floor's applicability in internal conflicts; and United Nations commission-of-inquiry findings documenting that unprotected detainees recur in every contemporary internal conflict. The ICRC also attests the problem's persistence, but as custodian it sits inside the regime; the judicial, doctrinal, and UN sources carry the external provenance.
narrative_ontology:disappearance_verdict(combatant_status_definition__functional_protection_reading, world_rearranges).
narrative_ontology:founding_problem_status(combatant_status_definition__functional_protection_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(combatant_status_definition__functional_protection_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(combatant_status_definition__functional_protection_reading, 'none', 1).
narrative_ontology:epsilon_provenance(combatant_status_definition__functional_protection_reading, 0.16, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(combatant_status_definition__functional_protection_reading_tests).
:- end_tests(combatant_status_definition__functional_protection_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.16 at interval end) because the floor's costs fall on detaining authorities as the price of the protective function itself — forbidding torture and summary execution is the point of the arrangement, not a rent taken from its subjects — and because the residual extraction vector (compliance and prosecution exposure concentrating on weaker or losing parties) has narrowed as customary consolidation closed the status loophole. Suppression (0.42) reflects real but incomplete enforcement machinery: tribunals, universal jurisdiction, and referral regimes coerce defection-prone parties, while much compliance still rides on normative force; suppression is authored as a raw structural property and is deliberately not scaled by scope or directionality. Theater (0.30) is rising but sub-dominant: proclamation, manual-writing, and ratification ceremony increasingly decouple from practice in some capitals, yet the visiting, reporting, and prosecutorial functions remain load-bearing. Accessibility collapse is moderate (0.40): once the functional reading is understood, the status-gated alternative remains legally accessible and live — that residual accessibility is precisely why the kernel stays contested. Resistance (0.52) is real: unlawful-combatant arguments, refused access negotiations, and non-ratification of the protocols by several major powers. The temporal series share one grid (t=0..76 at decade steps plus endpoint); base_extractiveness falls as jurisprudence closed the classification gap, suppression_requirement climbs with the enforcement build-up (ad hoc tribunals, permanent court, referral practice), and theater_ratio climbs with compliance ceremony — the joint signature of a maturing coordination norm acquiring bureaucratic accretion, not of a dying one.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats compute differently from the same structure. From state_detention_authorities and non_state_armed_groups, the floor arrives as unbudgeted operational constraint — interrogation menus narrowed, executions gated behind courts, facilities opened to visitors — with no classification escape hatch; from the detained seats, the same structure is survival infrastructure that requires nothing of them. The ICRC and tribunal seats are identity-locked: their institutional existence is fused with the arrangement, so they experience it as constitutive rather than costly. The excluded seat experiences the arrangement as an erasure of its professional function. The engine derives these per-seat classifications from the structural data; the authored rope claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: the three detained classes sit near the full-beneficiary end (d near 0.05–0.10) — the floor subsidizes them entirely and they are trapped inside it. The cost-bearing seats sit toward the target end but not at it: state_detention_authorities bear real compliance costs (d elevated) yet also draw reciprocity and legitimacy returns, and their exit is constrained rather than open, so they are targets of the arrangement's demands without being its victims; non_state_armed_groups land near symmetric (obligations out, coverage of their own captured members in). The ICRC derives low d from its beneficiary-side position but its identity lock keeps its relationship constitutive. No directionality overrides are authored: the role declarations, power atoms, and exit options already produce the correct qualitative ordering, and the coarse per-power-atom override surface would smear the ICRC's distinct position onto the state seats sharing its power level. Note the deliberate absence of a victims array: the cost-bearing seats are coordination-cost payers, not victims — what the arrangement takes from them is the protective function operating as designed, which is the rope structure, and listing them as victims would misread compliance costs as extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — detainees in internal conflicts falling outside every protection category — is live: internal conflicts remain the dominant conflict form and every new one relitigates classification at the point of capture. The R5 mismatch consumer therefore reads founding_problem_status=live against disappearance_verdict=world_rearranges: consistent, no zombie flag, no mandatrophy declaration. The rising theater series is the watch item: if proclamation activity continued decoupling from visiting and prosecutorial function, the arrangement would drift toward theatrical maintenance — but at 0.30 the functional core dominates, the beneficiaries are abundant and captive, and no seat profits enough from the theater alone to sustain it if the substance failed. On the receipt surface: gain_flow is authored as diffuse after checking every named seat — the protective goods flow to the detained classes as the arrangement's designed output rather than captured rent, the ICRC accrues modest institutional capital strictly subordinate to its mandate function, and the states' reciprocity and legitimacy returns are unassignable to any single seat; no seat captures the extraction, which is itself small. fixing_cost is prohibitive: unwinding the floor would require reversing three-quarters of a century of customary consolidation across treaty text, jurisprudence, and military manuals — no seat could do it at acceptable cost. That prohibitive-plus-diffuse pairing is the shape the piton cell flags, and this story is the counterexample worth having in the corpus: the cost-asymmetry test fails for piton because the administrators bear almost nothing, the function is intact, and removal serves no one — entrenched coordination, not inertial drag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing,
    'This constraint is the functional_protection_reading of the combatant_status_definition kernel; the state_centric and national_liberation readings instantiate structurally different constraints — what changes if a sibling governs instead?',
    'Track which reading governs floor application in each active conflict: whether detaining authorities establish status-review procedures before treatment decisions (state-centric practice) or extend the floor unconditionally at capture (functional practice), and whether liberation-movement status grants appear in peace processes (national-liberation practice).',
    'Under the state-centric reading, extractiveness for detainees held by non-state groups or held in unprivileged categories rises sharply — the floor becomes contingent on a favorable determination, recreating the victim class this reading abolishes. Under the national-liberation reading, additional status grants extend the privilege layer above the floor without altering it. The disagreement is located in whether status determination gates protections.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Kernel-level framing choice; sibling readings emit different constraints with different beneficiary and victim structures.').

omega_variable(
    floor_privilege_layer_boundary,
    'Does the status-independent floor exhaust the protection regime, or does it presuppose a status-gated privilege layer above it (combatant immunity, repatriation rights) that status determination continues to govern?',
    'Doctrinal analysis of whether the Common Article 3 floor and the Additional Protocol privilege regimes form one integrated structure or two separable layers, tested against how tribunals actually allocate protections between the layers.',
    'If the floor is complete, the state-centric reading is confined to describing a superseded structure; if the layers are separable, this reading structurally coexists with status-gated governance of everything above the minimum, and the kernel contest relocates to the boundary rather than the floor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(floor_privilege_layer_boundary, conceptual, 'Whether this reading''s constraint is the whole regime or its base layer.').

omega_variable(
    enforcement_asymmetry_extraction,
    'Does selective enforcement — prosecution exposure concentrating on weaker or defeated parties while powerful states shield their own — constitute extraction inside this reading''s constraint, or an implementation defect external to it?',
    'Compare prosecution and sanction rates for mistreatment-of-detainee violations across state power and alliance position, controlling for violation frequency.',
    'If the asymmetry is intrinsic to the arrangement''s operation, effective extraction for weak-party adherents rises and the reading drifts toward a hybrid coordination/extraction structure; if it is external, the pure-coordination classification stands and the asymmetry belongs to the enforcement ecosystem''s own story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_asymmetry_extraction, empirical, 'Whether victor''s-justice asymmetry is part of the constraint or noise around it.').

omega_variable(
    nonstate_compliance_mechanism,
    'Do organized non-state armed groups restrain treatment because the normative floor has been internalized, or only where deterrence or reciprocity operates?',
    'Comparative study of armed-group detention practice contrasting groups exposed to sustained norm socialization and delegation dialogue against groups facing purely deterrent pressure.',
    'If normative internalization carries compliance, the suppression requirement can plateau or decline as enforcement consolidates; if only deterrence works, suppression keeps climbing and the enforcement build-up becomes load-bearing rather than supplementary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nonstate_compliance_mechanism, empirical, 'Mechanism behind non-state compliance, determining the suppression trajectory''s ceiling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__functional_protection_reading, 0, 76).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ca3_functional_floor_tr_t0, combatant_status_definition__functional_protection_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(ca3_functional_floor_tr_t10, combatant_status_definition__functional_protection_reading, theater_ratio, 10, 0.11).
narrative_ontology:measurement(ca3_functional_floor_tr_t20, combatant_status_definition__functional_protection_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement(ca3_functional_floor_tr_t30, combatant_status_definition__functional_protection_reading, theater_ratio, 30, 0.17).
narrative_ontology:measurement(ca3_functional_floor_tr_t40, combatant_status_definition__functional_protection_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(ca3_functional_floor_tr_t50, combatant_status_definition__functional_protection_reading, theater_ratio, 50, 0.23).
narrative_ontology:measurement(ca3_functional_floor_tr_t60, combatant_status_definition__functional_protection_reading, theater_ratio, 60, 0.26).
narrative_ontology:measurement(ca3_functional_floor_tr_t70, combatant_status_definition__functional_protection_reading, theater_ratio, 70, 0.28).
narrative_ontology:measurement(ca3_functional_floor_tr_t76, combatant_status_definition__functional_protection_reading, theater_ratio, 76, 0.3).

% Extraction over time
narrative_ontology:measurement(ca3_functional_floor_be_t0, combatant_status_definition__functional_protection_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(ca3_functional_floor_be_t10, combatant_status_definition__functional_protection_reading, base_extractiveness, 10, 0.26).
narrative_ontology:measurement(ca3_functional_floor_be_t20, combatant_status_definition__functional_protection_reading, base_extractiveness, 20, 0.24).
narrative_ontology:measurement(ca3_functional_floor_be_t30, combatant_status_definition__functional_protection_reading, base_extractiveness, 30, 0.22).
narrative_ontology:measurement(ca3_functional_floor_be_t40, combatant_status_definition__functional_protection_reading, base_extractiveness, 40, 0.21).
narrative_ontology:measurement(ca3_functional_floor_be_t50, combatant_status_definition__functional_protection_reading, base_extractiveness, 50, 0.19).
narrative_ontology:measurement(ca3_functional_floor_be_t60, combatant_status_definition__functional_protection_reading, base_extractiveness, 60, 0.18).
narrative_ontology:measurement(ca3_functional_floor_be_t70, combatant_status_definition__functional_protection_reading, base_extractiveness, 70, 0.17).
narrative_ontology:measurement(ca3_functional_floor_be_t76, combatant_status_definition__functional_protection_reading, base_extractiveness, 76, 0.16).

% Suppression requirement over time
narrative_ontology:measurement(ca3_functional_floor_su_t0, combatant_status_definition__functional_protection_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(ca3_functional_floor_su_t10, combatant_status_definition__functional_protection_reading, suppression_requirement, 10, 0.19).
narrative_ontology:measurement(ca3_functional_floor_su_t20, combatant_status_definition__functional_protection_reading, suppression_requirement, 20, 0.23).
narrative_ontology:measurement(ca3_functional_floor_su_t30, combatant_status_definition__functional_protection_reading, suppression_requirement, 30, 0.27).
narrative_ontology:measurement(ca3_functional_floor_su_t40, combatant_status_definition__functional_protection_reading, suppression_requirement, 40, 0.31).
narrative_ontology:measurement(ca3_functional_floor_su_t50, combatant_status_definition__functional_protection_reading, suppression_requirement, 50, 0.34).
narrative_ontology:measurement(ca3_functional_floor_su_t60, combatant_status_definition__functional_protection_reading, suppression_requirement, 60, 0.37).
narrative_ontology:measurement(ca3_functional_floor_su_t70, combatant_status_definition__functional_protection_reading, suppression_requirement, 70, 0.4).
narrative_ontology:measurement(ca3_functional_floor_su_t76, combatant_status_definition__functional_protection_reading, suppression_requirement, 76, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(combatant_status_definition__functional_protection_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(combatant_status_definition__functional_protection_reading, combatant_status_definition__state_centric_reading).
narrative_ontology:affects_constraint(combatant_status_definition__functional_protection_reading, combatant_status_definition__national_liberation_reading).

% DUAL FORMULATION NOTE:
% Constraint family from one kernel: combatant_status_definition decomposes into three readings, each a separate story with its own epsilon, beneficiaries, and victims. This file is the functional_protection_reading (status-independent floor; low extraction for all detainees; status determination removed as a precondition for humane treatment). The state_centric_reading gates protections behind formal state military organization and categorically excludes non-state actors from prisoner-of-war protections — a different victim set (anyone held outside the gate) and a higher epsilon for the excluded. The national_liberation_reading widens the gate to organized liberation movements under Additional Protocol I Article 1(4) — a different beneficiary set above the floor. Linkage direction: this reading exerts structural pressure on the state-centric sibling (once the floor is unconditional, categorical exclusion loses its practical payoff and retreats to the privilege layer) while running parallel to the national-liberation sibling (different protection layers, complementary projects).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
