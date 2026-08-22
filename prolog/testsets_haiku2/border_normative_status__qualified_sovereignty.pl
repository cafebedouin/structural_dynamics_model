% ============================================================================
% CONSTRAINT STORY: border_normative_status__qualified_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_normative_status__qualified_sovereignty, []).

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
 *   constraint_id: border_normative_status__qualified_sovereignty
 *   human_readable: Border Control with Proportionality and Rights Constraints
 *   domain: political_philosophy/international_law
 *
 * SUMMARY:
 *   This constraint instantiates the 'qualified sovereignty' reading of the
 *   contested border-normative-status kernel. It asserts that states retain
 *   legitimate authority to control borders, but that authority is qualified
 *   by three structural constraints: (1) exclusion decisions must serve a
 *   legitimate state interest (not arbitrary preference); (2) the means
 *   chosen must be necessary and proportionate to that interest; (3)
 *   application must respect human rights obligations (non-refoulement, due
 *   process, family unity where applicable). This reading sits between the
 *   sovereignty_primary reading (which treats state border authority as
 *   foundational and unqualified) and the freedom_primary reading (which
 *   treats freedom of movement as foundational and borders as presumptively
 *   unjust). The qualified_sovereignty reading attempts to hold both: states
 *   have legitimate exclusion power, but its exercise is constrained by
 *   reasoned justification and proportionality review. The measurement
 *   trajectory shows extraction rising from 0.58 to 0.68 (states' burden of
 *   justification increases as international oversight expands), theater
 *   stabilizing at 0.48 (proportionality review is neither purely functional
 *   nor purely performative), and suppression stabilizing at 0.72
 *   (maintaining the constraint requires sustained enforcement of
 *   adjudication procedures and exclusion of workarounds like categorical
 *   bans).
 *
 * KEY AGENTS:
 *   - territorial_state_apparatus: agenda_setter holding institutional power; bears the cost of justification and adjudication
 *   - citizen_population: beneficiary at organized power level; retains membership closure and labor-market control
 *   - excluded_migrants: payer at powerless level; barred from entry unless proportionality is satisfied
 *   - displaced_persons & asylum_seekers: payers at powerless level; vulnerability locked to refugee/displaced status; gain procedural protection from proportionality constraint but remain excludable
 *   - receiving_states_judiciary: observer at institutional level; interprets proportionality standard and determines whether it functions as constraint or theater
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__qualified_sovereignty, 0.68).
domain_priors:suppression_score(border_normative_status__qualified_sovereignty, 0.72).
domain_priors:theater_ratio(border_normative_status__qualified_sovereignty, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, extractiveness, 0.68).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__qualified_sovereignty, tangled_rope).
narrative_ontology:human_readable(border_normative_status__qualified_sovereignty, "Border Control with Proportionality and Rights Constraints").
narrative_ontology:topic_domain(border_normative_status__qualified_sovereignty, "political_philosophy/international_law").

domain_priors:requires_active_enforcement(border_normative_status__qualified_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__qualified_sovereignty, 'a60e2aa8-a635-4b98-b8ef-72112b6b7679').
narrative_ontology:cs_kernel_codification('a60e2aa8-a635-4b98-b8ef-72112b6b7679', fixed_text).
narrative_ontology:cs_authority_grounding('a60e2aa8-a635-4b98-b8ef-72112b6b7679', lineage).
narrative_ontology:cs_interpretation_layer_present('a60e2aa8-a635-4b98-b8ef-72112b6b7679').
narrative_ontology:cs_reading_relation('a60e2aa8-a635-4b98-b8ef-72112b6b7679', border_normative_status__sovereignty_primary, coexists_with).
narrative_ontology:cs_reading_relation('a60e2aa8-a635-4b98-b8ef-72112b6b7679', border_normative_status__freedom_primary, coexists_with).
narrative_ontology:cs_axiom('a60e2aa8-a635-4b98-b8ef-72112b6b7679', foundational, proportionality_constrains_exclusion).
narrative_ontology:cs_axiom_status(proportionality_constrains_exclusion, holdable).
narrative_ontology:cs_axiom_grounding('a60e2aa8-a635-4b98-b8ef-72112b6b7679', proportionality_constrains_exclusion, deontological).
narrative_ontology:cs_axiom('a60e2aa8-a635-4b98-b8ef-72112b6b7679', secondary, legitimate_state_interest_standard_applicable).
narrative_ontology:cs_axiom_status(legitimate_state_interest_standard_applicable, holdable).
narrative_ontology:cs_axiom_grounding('a60e2aa8-a635-4b98-b8ef-72112b6b7679', legitimate_state_interest_standard_applicable, conventional).
narrative_ontology:cs_reference_frame('a60e2aa8-a635-4b98-b8ef-72112b6b7679', bounded_community_with_rights_constraints).
narrative_ontology:cs_drift_state('a60e2aa8-a635-4b98-b8ef-72112b6b7679', contemporary_rights_expansion, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a60e2aa8-a635-4b98-b8ef-72112b6b7679', '').
narrative_ontology:cs_kernel_id(border_normative_status__qualified_sovereignty, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__qualified_sovereignty, territorial_state_apparatus).
narrative_ontology:constraint_beneficiary(border_normative_status__qualified_sovereignty, citizen_population).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, excluded_migrants).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, displaced_persons).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, asylum_seekers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(border_normative_status__qualified_sovereignty, asylum_seekers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets border policy, determines admission criteria, enforces exclusion rules. Claims authority derives from territorial sovereignty and collective self-determination. Administers the adjudication machinery required by the proportionality constraint. Bears the administrative cost of demonstrating legitimate interest and proportional means for each exclusion decision.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, territorial_state_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Retains the right to constitute themselves as a bounded political community and control entry. Benefit is collective: membership closure and the ability to shape labor markets, social policy, and cultural reproduction through admission control. Individual citizens often benefit economically from labor market protection, though some sectors depend on migrant labor.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, citizen_population, beneficiary,
    organized, biographical, arbitrage, national).

% Barred from entry despite economic or family incentive to migrate. The proportionality constraint means their exclusion must be justified by a legitimate state interest (public health, security, labor market impact, resource constraint) and the means must be necessary and least-restrictive available. They have no formal voice in admission criteria or adjudication but are the subjects of exclusion decisions. Exit option is effectively absent: they cannot exit the global status of unaccepted migrants.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, excluded_migrants, payer,
    powerless, biographical, trapped, global).

% Flee persecution, conflict, or violence and seek refuge. The proportionality constraint recognizes certain international obligations (non-refoulement, asylum protection) that qualify state exclusion authority. However, the state retains the right to limit numbers or designate safe third countries, constrained only by the requirement to justify these limits as proportionate to legitimate interests. Identity is locked to their refugee or internally displaced status; they cannot simply relocate without the legal recognition states control.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, displaced_persons, payer,
    powerless, immediate, identity_locked, regional).

% Claim asylum rights under international law. The proportionality constraint creates an adjudication burden: the state must assess claims individually rather than excluding entire classes categorically. They benefit from the procedural protection the proportionality requirement creates (right to hearing, right to appeal, due process) but remain vulnerable to exclusion if the state can demonstrate legitimate interest and proportional necessity for denial.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, asylum_seekers, payer,
    powerless, immediate, constrained, regional).
narrative_ontology:stakeholder_secondary_role(border_normative_status__qualified_sovereignty, asylum_seekers, beneficiary).

% Interprets and applies the proportionality standard in contested admissions cases. Balances state sovereignty against human rights obligations. Their interpretation trajectory determines whether 'proportionate' remains a meaningful constraint or becomes theater.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, receiving_states_judiciary, observer,
    institutional, generational, analytical, national).

% Monitor state compliance with non-refoulement, family unity, and due process obligations. Can issue rulings and recommendations that clarify the proportionality standard, but enforcement depends on domestic political will.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_normative_status__qualified_sovereignty, territorial_state_apparatus).
narrative_ontology:fixing_cost_class(border_normative_status__qualified_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Recognizes both state authority to control borders (solving the collective-action problem of political self-determination and the ability to shape membership conditions) and human rights constraints that prevent that authority from becoming arbitrary or absolutist. Coordinates the legitimate interests of bounded political communities with protection of vulnerable persons crossing borders.
% TRANSFER_FUNCTION: Moves the burden of proof and adjudication cost to states (they must justify and demonstrate proportionality), while redirecting discretionary power away from purely interest-based exclusion toward reasoned, constrained decisions. Transfers some admission-access outcomes from categorical-exclusion regimes to individual-assessment regimes.
% ABSENT_VOICES: Non-citizens in third countries with no migration aspiration are absent; they do not benefit or pay within this constraint (the constraint applies only to those seeking entry). Stateless persons, whose very claim to membership anywhere is contested, are structurally hard to include in the adjudication frame. Some nationalist and sovereignty-primary factions would object that proportionality requirements constitute unjust constraints on collective self-determination, but they are excluded from design of the constraint itself.
% DISAPPEARANCE_RATIONALE: If the proportionality requirement vanished and states retained unqualified border control, admission would revert to categorical exclusion without adjudication burden. Migrants and asylum seekers would lose procedural protection and access to review. States would regain the discretion to exclude entire nationalities, religions, or classes without justifying the exclusion as necessary or proportionate. The constraint's disappearance would eliminate a major source of legal leverage for advocates challenging discriminatory or arbitrary admissions policy.
% FOUNDING_PROBLEM: States need legitimate authority to control borders (the founding problem of any bounded political community), but unconstrained border discretion enables arbitrary exclusion, discriminatory application, and violations of human dignity. The proportionality principle was developed to reconcile these: border control is legitimate only when exercised as means proportionate to justified ends, not as an absolute prerogative.
% FOUNDING_PROBLEM_CORROBORATION: International human rights courts (European Court of Human Rights, Inter-American Court of Human Rights, African Court on Human and Peoples' Rights) affirm the founding problem status in rulings requiring proportionality review. States' own judicial systems increasingly apply proportionality tests to border decisions. Refugee advocacy organizations and humanitarian bodies attest the founding problem is live because state discretion without constraint continues to produce exclusions harming vulnerable persons. Sovereignty-primary legal scholars dispute this framing and attest the problem is overstated; their position is recorded in dissenting jurisprudence and legislative debate.
narrative_ontology:disappearance_verdict(border_normative_status__qualified_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(border_normative_status__qualified_sovereignty, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__qualified_sovereignty, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(border_normative_status__qualified_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(border_normative_status__qualified_sovereignty, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_normative_status__qualified_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_normative_status__qualified_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_normative_status__qualified_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.68 reflects the burden the constraint places on migrants and asylum seekers: their access to territory is conditioned on state demonstration of justified interest in exclusion, a burden that persists even when individual claims meet humanitarian criteria. Suppression at 0.72 reflects the enforcement machinery required: states must maintain adjudication capacity, resist categorical-exclusion shortcuts, and sustain procedural consistency despite political pressure to tighten borders. Theater at 0.48 is moderate because proportionality review is genuinely consequential in some jurisdictions (courts genuinely overturn arbitrary denials) and purely performative in others (proportionality language masks discretionary exclusion). Accessibility collapse at 0.61 reflects that alternatives (uncontrolled movement, pure discretion) remain intellectually available and are advocated by freedom_primary and sovereignty_primary readings, but the proportionality frame has become the dominant institutional language for border disputes. Resistance at 0.74 reflects the sustained challenge from both sovereignty_primary proponents (who resist the proportionality constraint as unjust limitation on state authority) and freedom_primary proponents (who resist it as inadequate protection of movement rights). The constraint persists because neither wing has achieved hegemonic institutional authority; proportionality review sits as the negotiated middle ground enforced by courts and monitored by rights bodies.
 *
 * PERSPECTIVAL GAP:
 *   From the state's position, proportionality is a legitimate framework: it preserves border authority while ensuring arbitrary power is constrained. From the migrant/asylum-seeker position, proportionality is a costlier constraint than no border at all (freedom_primary reading) but a necessary improvement over purely discretionary exclusion (sovereignty_primary baseline). The perspective gap shows up in measured divergence: state institutions report compliance and proportional application; migrant advocacy reports persistent exclusion justified by performative proportionality language. The engine computes per-seat type from these same structural data: from the state's seat, the constraint is tangled_rope (coordination + asymmetric extraction held in balance by adjudication). From the migrant's seat, the constraint approaches snare (justified extraction that persists by suppressing alternatives—freedom of movement—and by requiring migrants to accept procedural theater in place of actual mobility). The gap itself is the measurement the corpus takes.
 *
 * DIRECTIONALITY LOGIC:
 *   The agenda-setter (state apparatus) experiences the constraint as costly coordination: proportionality requirements increase administrative burden and reduce discretion, but the state retains foundational authority and the benefit of membership closure. Its directionality is moderate-to-beneficiary (d ≈ 0.35): the constraint qualifies power but doesn't remove it. Citizens benefit from membership closure without bearing the adjudication cost; directionality is beneficiary (d ≈ 0.15). Migrants and asylum seekers bear the extraction cost directly: they must either meet the state's justified-interest standard or be excluded, with no formal voice in how that standard is applied. Their directionality is target (d ≈ 0.85): the constraint theoretically protects them from arbitrary exclusion, but in practice operates as a gating mechanism that persists in excluding them while wrapping that exclusion in reasoned justification. The judiciary sits at analytical directionality: their interpretation trajectory determines whether the constraint functions as coordination or as rationalized extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: political communities do need mechanisms for membership closure and collective self-determination, and unqualified border discretion does enable arbitrary and harmful exclusion. The proportionality constraint does not resolve the problem; it manages it by creating an adjudication layer that requires states to justify exclusion as necessary and proportionate. Mandatrophy risk is moderate because the adjudication machinery can atrophy (courts deferring to executive border policy, proportionality review becoming theater) while the extraction persists. Theater-ratio rise from 0.35 to 0.48 signals this drift: proportionality language increasingly appears in border decisions without substantively constraining exclusion. If theater_ratio crosses 0.55 and stays there while extraction holds at 0.68+, the constraint would shift toward piton—a vestigial proportionality requirement maintained as institutional window-dressing while discretion operates in substance. The measurement series does not yet show this shift, but it is the mandatrophy risk to monitor.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_operationalization_gap,
    'What concrete criteria determine whether a state''s border exclusion is ''proportionate'' to its legitimate interest, and how do these criteria vary across jurisdictions?',
    'Comparative analysis of proportionality standards applied by different human rights courts and domestic judiciaries; examination of case law where exclusion was found disproportionate versus where it was upheld.',
    'If proportionality criteria are concrete and consistently applied, the constraint functions as a meaningful limitation on state discretion. If criteria remain vague or jurisdiction-dependent, proportionality becomes theater and the constraint approaches snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_operationalization_gap, empirical, 'The operationality and consistency of proportionality review across jurisdictions.').

omega_variable(
    freedom_vs_qualified_sovereignty_foreclosure,
    'Is the qualified_sovereignty reading logically compatible with the freedom_primary reading, or does accepting state border authority necessarily foreclose the right to freedom of movement?',
    'Philosophical analysis of whether a framework can recognize both bounded political communities (with membership closure) and individual freedom of movement as foundational rights without one subordinating the other.',
    'If the readings foreclose each other, only one can be true and the kernel admits a genuine contradiction at the foundations. If they coexist, the qualified_sovereignty reading represents a negotiated middle ground between incommensurable values rather than a principled resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(freedom_vs_qualified_sovereignty_foreclosure, conceptual, 'Whether qualified_sovereignty and freedom_primary readings are logically compatible or mutually exclusive.').

omega_variable(
    adjudication_burden_sustainability,
    'As migration volumes increase and border pressure intensifies, can receiving states sustain the adjudication machinery that proportionality review requires, or does the burden collapse into categorical decision-making and theater?',
    'Long-term observation of adjudication delays, reversal rates, and state-level compliance with proportionality standards as migration pressures rise. Post-crisis analysis of whether states maintain review processes or adopt emergency categorical exclusions.',
    'If the burden proves unsustainable, proportionality review becomes theater (theater_ratio rises, suppression increases to maintain the fiction). The constraint would approach piton: vestigial requirement maintained as institutional legitimacy cover while discretion operates in practice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(adjudication_burden_sustainability, empirical, 'The sustainability of proportionality adjudication under scaling migration pressure.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'To what extent is the measured suppression structural (legal barriers, enforcement machinery, institutional cost-imposition) versus internalized (migrants'' internalized acceptance of border authority as legitimate)?',
    'Comparative ethnography and interview studies with migrant populations in different legal frameworks; observation of migrant behavior when enforcement machinery is degraded or absent.',
    'If suppression is primarily internalized, the constraint''s effective suppression is higher than measured—it travels with migrants who accept border authority as legitimate even after structural enforcement is removed. If primarily structural, suppression is tied to enforcement capacity and degrades when enforcement capacity erodes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether measured suppression (0.72) is structural enforcement or internalized legitimacy acceptance.').

omega_variable(
    kernel_boundary_displacement,
    'Does the qualified_sovereignty reading resolve the border-normative-status kernel, or does it displace the fundamental disagreement onto a new locus (e.g., from ''do states have authority to exclude'' to ''what counts as a legitimate interest for exclusion'')?',
    'Historical analysis of whether jurisdictions adopting the proportionality framework resolve disputes, or whether disputes simply re-emerge at the level of legitimate-interest determination.',
    'If the reading displaces rather than resolves the kernel, the framework may be a procedural agreement to disagree rather than a substantive resolution. The three readings (sovereignty_primary, qualified_sovereignty, freedom_primary) would remain live, just fighting at different levels of the decision tree.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_boundary_displacement, conceptual, 'Whether qualified_sovereignty resolves the kernel or merely displaces its locus of contestation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__qualified_sovereignty, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_normative_status__qualified_sovereignty, theater_ratio, 0, 0.35).
narrative_ontology:measurement(bord_tr_t5, border_normative_status__qualified_sovereignty, theater_ratio, 5, 0.38).
narrative_ontology:measurement(bord_tr_t10, border_normative_status__qualified_sovereignty, theater_ratio, 10, 0.42).
narrative_ontology:measurement(bord_tr_t15, border_normative_status__qualified_sovereignty, theater_ratio, 15, 0.45).
narrative_ontology:measurement(bord_tr_t20, border_normative_status__qualified_sovereignty, theater_ratio, 20, 0.48).
narrative_ontology:measurement(bord_tr_t25, border_normative_status__qualified_sovereignty, theater_ratio, 25, 0.47).
narrative_ontology:measurement(bord_tr_t30, border_normative_status__qualified_sovereignty, theater_ratio, 30, 0.48).
narrative_ontology:measurement(bord_tr_t35, border_normative_status__qualified_sovereignty, theater_ratio, 35, 0.48).
narrative_ontology:measurement(bord_tr_t40, border_normative_status__qualified_sovereignty, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_normative_status__qualified_sovereignty, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(bord_be_t5, border_normative_status__qualified_sovereignty, base_extractiveness, 5, 0.61).
narrative_ontology:measurement(bord_be_t10, border_normative_status__qualified_sovereignty, base_extractiveness, 10, 0.64).
narrative_ontology:measurement(bord_be_t15, border_normative_status__qualified_sovereignty, base_extractiveness, 15, 0.66).
narrative_ontology:measurement(bord_be_t20, border_normative_status__qualified_sovereignty, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(bord_be_t25, border_normative_status__qualified_sovereignty, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(bord_be_t30, border_normative_status__qualified_sovereignty, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(bord_be_t35, border_normative_status__qualified_sovereignty, base_extractiveness, 35, 0.68).
narrative_ontology:measurement(bord_be_t40, border_normative_status__qualified_sovereignty, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_normative_status__qualified_sovereignty, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(bord_su_t5, border_normative_status__qualified_sovereignty, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(bord_su_t10, border_normative_status__qualified_sovereignty, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(bord_su_t15, border_normative_status__qualified_sovereignty, suppression_requirement, 15, 0.71).
narrative_ontology:measurement(bord_su_t20, border_normative_status__qualified_sovereignty, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(bord_su_t25, border_normative_status__qualified_sovereignty, suppression_requirement, 25, 0.72).
narrative_ontology:measurement(bord_su_t30, border_normative_status__qualified_sovereignty, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(bord_su_t35, border_normative_status__qualified_sovereignty, suppression_requirement, 35, 0.72).
narrative_ontology:measurement(bord_su_t40, border_normative_status__qualified_sovereignty, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_normative_status__qualified_sovereignty, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(border_normative_status__qualified_sovereignty, 0.18).
narrative_ontology:affects_constraint(border_normative_status__qualified_sovereignty, border_normative_status__sovereignty_primary).
narrative_ontology:affects_constraint(border_normative_status__qualified_sovereignty, border_normative_status__freedom_primary).
narrative_ontology:affects_constraint(border_normative_status__qualified_sovereignty, asylum_non_refoulement_obligation).
narrative_ontology:affects_constraint(border_normative_status__qualified_sovereignty, state_legitimate_interest_doctrine).

% DUAL FORMULATION NOTE:
% The border_normative_status kernel admits three structurally distinct constraint readings: sovereignty_primary (unqualified state authority), qualified_sovereignty (this file—authority constrained by proportionality and rights), and freedom_primary (fundamental right to movement). Each reading has different beneficiaries, victims, and ε values. The three stories form a kernel family linked by affects_constraints. This reading (qualified_sovereignty) influences the other two: it is the institutional settlement most receiving states have adopted, and it shapes the debate by framing subsequent challenges as proportionality disputes rather than categorical authority questions. The family shares the kernel (contested state authority over borders) but produces three distinct constraint stories with different classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(border_normative_status__qualified_sovereignty, powerless, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
