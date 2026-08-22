% ============================================================================
% CONSTRAINT STORY: second_amendment_text__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__individual_right_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: second_amendment_text__individual_right_reading
 *   human_readable: Second Amendment Individual Right Reading: Personal Self-Defense Protected
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This constraint instantiates the individual-right reading of the Second
 *   Amendment operative clause ('the right of the people to keep and bear
 *   Arms, shall not be infringed'). Under this reading, the operative clause
 *   guarantees an individual right to firearm ownership independent of
 *   militia service or state licensing discretion. Personal self-defense is
 *   recognized as a core protected activity. The reading benefits individual
 *   gun owners (who gain constitutional protection against disarmament) and
 *   costs disqualified populations — felons, domestic abusers, involuntarily
 *   committed persons — who are foreclosed from the right's protection. The
 *   constraint coordinates a constitutional standard across jurisdictions
 *   while extracting by concentrating decisional authority in courts and
 *   creating asymmetric protection (broad access for the permitted, total
 *   exclusion for the disqualified). This is the operative constitutional law
 *   post-Heller (2008) and McDonald (2010), though contested by adherents of
 *   the collective-security reading.
 *
 * KEY AGENTS:
 *   - individual_gun_owners: primary beneficiary (claim constitutional protection, gain mobility against permit denial)
 *   - felons_with_prior_convictions: primary victim (foreclosed from right, trapped by federal disqualification)
 *   - domestic_abusers_subject_to_restraint: primary victim (foreclosed by Lautenberg Amendment, powerless to appeal)
 *   - persons_involuntarily_committed: victim (identity-locked by involuntary commitment, disqualified by statute)
 *   - courts_enforcing_the_reading: agenda-setter (interpret operative clause, adjudicate boundaries, strike down conflicting regulations)
 *   - legislative_gun_rights_advocates: beneficiary (mobilize the reading for deregulation, gain constitutional leverage)
 *   - victims_of_gun_violence: excluded (present in policy contest but absent from constitutional reading)
 *   - originalist_constitutional_scholars: observer (ground the reading in historical evidence, validate its legitimacy)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__individual_right_reading, 0.62).
domain_priors:suppression_score(second_amendment_text__individual_right_reading, 0.71).
domain_priors:theater_ratio(second_amendment_text__individual_right_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_text__individual_right_reading, "Second Amendment Individual Right Reading: Personal Self-Defense Protected").
narrative_ontology:topic_domain(second_amendment_text__individual_right_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_text__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__individual_right_reading, '292d08dc-e74a-4c2e-ae5a-1f75e3060380').
narrative_ontology:cs_kernel_codification('292d08dc-e74a-4c2e-ae5a-1f75e3060380', fixed_text).
narrative_ontology:cs_authority_grounding('292d08dc-e74a-4c2e-ae5a-1f75e3060380', lineage).
narrative_ontology:cs_interpretation_layer_present('292d08dc-e74a-4c2e-ae5a-1f75e3060380').
narrative_ontology:cs_reading_relation('292d08dc-e74a-4c2e-ae5a-1f75e3060380', second_amendment_text__collective_security_reading, forecloses).
narrative_ontology:cs_reading_relation('292d08dc-e74a-4c2e-ae5a-1f75e3060380', second_amendment_text__originalist_civic_virtue_reading, coexists_with).
narrative_ontology:cs_axiom('292d08dc-e74a-4c2e-ae5a-1f75e3060380', foundational, operative_clause_independent).
narrative_ontology:cs_axiom_status(operative_clause_independent, holdable).
narrative_ontology:cs_axiom_grounding('292d08dc-e74a-4c2e-ae5a-1f75e3060380', operative_clause_independent, deontological).
narrative_ontology:cs_axiom('292d08dc-e74a-4c2e-ae5a-1f75e3060380', secondary, individual_self_defense_core_protected).
narrative_ontology:cs_axiom_status(individual_self_defense_core_protected, holdable).
narrative_ontology:cs_axiom_grounding('292d08dc-e74a-4c2e-ae5a-1f75e3060380', individual_self_defense_core_protected, empirically_contingent).
narrative_ontology:cs_reference_frame('292d08dc-e74a-4c2e-ae5a-1f75e3060380', operative_clause_grants_individual_liberty).
narrative_ontology:cs_drift_state('292d08dc-e74a-4c2e-ae5a-1f75e3060380', contemporary_post_heller_enforcement, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('292d08dc-e74a-4c2e-ae5a-1f75e3060380', '').
narrative_ontology:cs_kernel_id(second_amendment_text__individual_right_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__individual_right_reading, individual_gun_owners).
narrative_ontology:constraint_beneficiary(second_amendment_text__individual_right_reading, self_defense_practitioners).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, felons_with_prior_convictions).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, domestic_abusers_subject_to_restraint).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, persons_involuntarily_committed_to_mental_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(second_amendment_text__individual_right_reading, legislative_gun_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claim a constitutional right to possess firearms for lawful purposes including personal self-defense, independent of militia service or state licensing discretion. They argue the operative clause ('the right of the people to keep and bear Arms, shall not be infringed') stands alone and protects an individual liberty. They face permitting requirements, permit delays, and discretionary denial in some jurisdictions, and seek recognition that the right exists prior to and independent of regulatory permission.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, individual_gun_owners, beneficiary,
    moderate, biographical, mobile, national).

% Are disqualified from firearm possession under federal law (18 U.S.C. § 922(g)) and corresponding state laws. Under this reading's framework, they represent the identifiable class for whom the individual right is foreclosed by criminal conviction. They lack political voice in Second Amendment doctrine formation and cannot appeal to the right as a defense against firearm possession prohibitions.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, felons_with_prior_convictions, payer,
    powerless, biographical, trapped, national).

% Are disqualified from firearm possession under laws implementing the Lautenberg Amendment (18 U.S.C. § 922(g)(8)) when subject to domestic violence restraining orders or convicted of misdemeanor domestic violence. They bear the cost of the reading's instantiation: the individual right is recognized for the majority, but firearm access is foreclosed for them as a public-safety measure. They are absent from the political reading process itself.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, domestic_abusers_subject_to_restraint, payer,
    powerless, biographical, trapped, national).

% Are disqualified from firearm possession under federal law (18 U.S.C. § 922(d)(4)) and corresponding state laws when adjudicated as dangerous or lacking capacity. The reading's framework recognizes an individual right for the non-committed, but the committed are foreclosed from exercising it. Their exclusion is presented as a public-safety necessity, not a limitation on the right itself.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, persons_involuntarily_committed_to_mental_institutions, payer,
    powerless, biographical, identity_locked, national).

% Interpret and enforce the operative clause as guaranteeing an individual right independent of militia service. Post-District of Columbia v. Heller (2008) and McDonald v. City of Chicago (2010), this reading became the operative constitutional law. Courts apply it to strike down permit regimes, magazine capacity limits, and categorical firearm bans, while sustaining felon disqualifications and certain licensing requirements as consistent with the right's historical scope. They administrate the boundary between protected and prohibited.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, courts_enforcing_the_reading, agenda_setter,
    institutional, generational, analytical, national).

% Mobilize the individual-right reading to argue for broader firearm access, permit-less carry, and protection against new restrictions. They fund litigation establishing and defending the reading's boundaries and mount political campaigns against permitting regimes. They benefit from the reading's existence by having a constitutional anchor for their deregulation agenda. Their exit from this reading would require political realignment to the collective-security reading.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, legislative_gun_rights_advocates, beneficiary,
    organized, generational, constrained, national).

% Are not parties to the constitutional reading but experience its downstream effects. They argue the individual-right reading forecloses regulations (universal background checks, red-flag laws, assault-weapon restrictions) that would reduce harm. They are excluded from the reading's framing but present in the political contest over implementation; their voice is structural advocacy for rival readings or for narrowing the right's scope.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, victims_of_gun_violence, excluded,
    organized, biographical, constrained, national).

% Analyze the historical original public meaning of the Second Amendment text. Originalist scholars in the individual-right camp attest that the operative clause was understood at ratification to protect an individual capacity (as demonstrated by founding-era militia practice, state constitutions, and foundational texts). Their interpretive work grounds the reading's legitimacy in historical evidence rather than contemporary policy preference.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, originalist_constitutional_scholars, observer,
    institutional, generational, analytical, national).

% Generate data on firearm injury, suicide, and homicide outcomes. They sit outside the constitutional reading but feed the empirical debate over whether the reading's scope (broad firearm access) produces net public health benefit or harm. They provide contested evidence about the tradeoff between self-defense capability and injury risk.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, public_health_researchers, observer,
    institutional, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_text__individual_right_reading, legislative_gun_rights_advocates).
narrative_ontology:fixing_cost_class(second_amendment_text__individual_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a constitutional standard for when firearm ownership is a protected individual liberty rather than a privilege conditionally granted by the state. Coordinates interpretation across federal and state judiciaries so that gun owners have a baseline of protected access independent of where they reside, and state legislatures know which regulatory structures violate the standard.
% TRANSFER_FUNCTION: Transfers constitutional authority over firearm access from state legislatures (which retain some power to regulate) to individual rights-bearers (who gain a claim against permit denial) and courts (which acquire the role of adjudicating the boundary). The reading also transfers political capital toward gun-rights advocates (who gain a constitutional argument) and away from gun-control advocates (who must now justify restrictions as consistent with an individual right, not merely as police-power choices).
% ABSENT_VOICES: Victims of gun violence and their advocates are structurally excluded from the constitutional reading process itself — the reading is about what the text means, not about gun policy outcomes. They appear only as a policy-competing voice outside the constitutional frame. Disqualified populations (felons, domestic abusers, involuntarily committed) are absent from the reading's formation and from the political process that defends or contests it; they are subjects of the reading's consequences, not participants in its meaning-making.
% DISAPPEARANCE_RATIONALE: If this reading of the operative clause disappeared overnight — if courts reverted to the collective-security reading or treated the militia clause as conditioning the entire right — the constitutional protection for individual firearm ownership would evaporate or radically narrow. Federal law would no longer constrain state permitting regimes; states could impose licensing and discretionary denial without constitutional pushback; the Second Amendment would cease to be a justiciable individual right. Gun-rights litigation strategy would collapse; gun-owner constituencies would reorganize around state-level defenses and political contestation rather than federal constitutional claims. The constraint's disappearance would trigger large-scale reorganization.
% FOUNDING_PROBLEM: The original drafters sought to protect the capacity of individual citizens to keep and bear arms for lawful purposes, including resistance to governmental tyranny and personal self-defense, against disarmament by a centralized authority. The operative clause was designed to ensure that gun ownership remained an individual matter, not conditional on state permission or militia enrollment.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars and the Supreme Court majority in Heller attest that the founding problem remains live — preventing governmental disarmament of the citizenry is an enduring concern. Gun-control advocates and scholars in the collective-security tradition contest this framing, arguing the founding problem was specifically about state militia capability, not individual gun ownership for self-defense. Legislative testimony from both camps disputes whether the original concern still applies to modern conditions. Independent historical scholarship (e.g., Saul Cornell's work on militia vs. individual-rights framing) documents genuine contestation among historians and legal scholars about what the founding generation intended.
narrative_ontology:disappearance_verdict(second_amendment_text__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_text__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__individual_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_text__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_text__individual_right_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__individual_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_text__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_text__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises over the interval (0.15 to 0.62) because the reading's scope has expanded and its enforcement has strengthened. Pre-Heller (1791–2007), the reading existed primarily in academic argument and lower-court dissents with minimal enforcement power; post-Heller, it became binding constitutional law with courts striking down entire regulatory regimes (e.g., DC's handgun ban, Chicago's permit-denial regime). Suppression is high (0.71) because the constraint's enforcement requires actively excluding disqualified populations from exercising the right — suppression is structural (felons are legally disarmed) and institutional (courts enforce the boundary). Theater ratio is moderate (0.28): the constraint coordinates genuine protection for the permitted, but a growing share of interpretive activity goes to defending disqualifications (public-safety theater) rather than clarifying the core right. Accessibility collapse is low (0.48) because the right's opponents (collective-security advocates, gun-control supporters) remain politically organized and continue to mount constitutional and legislative challenges; alternatives (the collective-security reading, regulatory permitting) are not yet collapsed as intelligible positions.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seat (individual gun owners, courts), the reading is a genuine protection of an enduring constitutional liberty — a Rope coordinating what counts as lawful firearm ownership across jurisdictions. From the victim seat (disqualified populations), the reading operates as enforced exclusion — a Snare that forecloses their access without their consent or participation. From the excluded seat (gun-violence victims and their advocates), the reading extracts by pre-empting regulatory alternatives they favor. The engine computes per-seat divergence from the structural data: beneficiary/victim declarations and the power/exit asymmetry between seats. The claimed_type (tangled_rope) reflects the reading's genuine coordination function (establishing a constitutional standard) combined with asymmetric extraction (disqualification without participation).
 *
 * DIRECTIONALITY LOGIC:
 *   Individual gun owners occupy the beneficiary seat: they claim and gain a constitutional right the reading protects, with moderate power (organized interest groups, voting constituents) and mobile exit options (they can advocate for the collective-security reading or focus on state-level policy). Disqualified populations occupy the target seat: they are foreclosed by the reading's operation, powerless (no organized political voice in Second Amendment doctrine), and identity-locked or trapped (criminal conviction or involuntary commitment are status features, not choices). Courts occupy the agenda-setter seat: they interpret the reading, set its boundaries, and enforce it against contrary state regulation. Legislative gun-rights advocates occupy a secondary beneficiary seat: they gain political capital from having a constitutional anchor for their deregulation agenda. Victims of gun violence are excluded: their voice is present in the political arena but absent from the constitutional reading's formation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is contested: originalist scholars and Heller's majority attest the reading preserves an enduring concern (protecting the citizenry against disarmament); gun-control advocates and collective-security scholars attest the founding problem is solved (state capacity for organized militia is no longer under threat) or was never about individual self-defense. The measurement series shows base extractiveness rising sharply post-Heller (from 0.48 to 0.62) because the reading's constitutional enforcement machinery became operational; before Heller, the reading existed as doctrine with minimal enforcement power. If extractiveness continues rising while founding_problem_status remains contested, the constraint becomes a candidate for mandatrophy (the founding problem is dead, but the extraction persists). The current state is tangled_rope because the reading still coordinates a genuine constitutional standard; if the problem dies while the disqualifications and exclusions persist, the classification would shift toward piton (inertial maintenance of a mechanism whose founding justification has atrophied).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    militia_clause_conditional_status,
    'Does the militia clause (''A well regulated Militia, being necessary to the security of a free State'') condition or limit the operative clause''s scope, or does the operative clause stand alone independent of militia service?',
    'Historical linguistic analysis of founding-era drafting records, contemporaneous state constitutions, and foundational texts. Originalist scholarship on the grammatical relationship between prefatory and operative clauses in 18th-century American legal writing.',
    'If the militia clause is genuinely conditional, the collective-security reading forecloses the individual-right reading within a single originalist framework. If the operative clause stands alone, the individual-right reading is structurally sound and the collective-security reading requires a non-originalist reading method. This resolves which reading''s legitimacy claim is foundational vs. derivative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_clause_conditional_status, conceptual, 'Whether the prefatory militia clause conditions the operative clause or stands separately.').

omega_variable(
    self_defense_scope_historical,
    'Was personal self-defense recognized as a core protected activity in founding-era understandings of the right to bear arms, or was the right understood primarily as militia-related?',
    'Originalist historical evidence: founding-era legal treatises (Blackstone), state constitutions, militia statutes, founding-era political writings. Comparative analysis of how the right was understood in England vs. America.',
    'If self-defense was understood as core, the individual-right reading''s instantiation of personal self-defense as protected is historically grounded. If militia service was primary, the originalist_civic_virtue reading (emphasizing citizen-soldier capacity) is more historically accurate. This affects whether originalism sustains or competes with the individual-right reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_defense_scope_historical, empirical, 'Historical scope of ''bear arms'' to include personal self-defense vs. militia service only.').

omega_variable(
    disqualification_victims_voice,
    'Are disqualified populations (felons, domestic abusers, involuntarily committed) genuinely unable to participate in the constitutional reading process, or do they have available means of advocacy that remain unexercised?',
    'Analysis of legal processes for challenging disqualifications, political organizing capacity among disqualified populations, representation in constitutional litigation and legislative testimony, and barriers to entry (legal standing, resources, stigma).',
    'If disqualification is genuinely structurally silencing, the reading''s beneficiary/victim asymmetry is compounded by absence-from-the-room (Pattern-5 violation). If disqualified populations have advocacy channels but remain politically inert, the silence is contingent rather than structural. This affects whether the reading''s legitimacy can withstand the R5 corroboration requirement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(disqualification_victims_voice, empirical, 'Whether disqualified populations'' absence from constitutional reading is structural or contingent.').

omega_variable(
    extraction_vs_necessary_boundary,
    'Is the measured suppression (0.71) attributable to necessary boundary-setting on a genuine individual right, or does it reflect extraction that extends beyond what the original right required?',
    'Originalist scope analysis: what disqualifications were understood as necessary at ratification? Comparative study of how disqualifications have expanded since founding (felony disqualifications vastly broader now; involuntary commitment statutes are recent). Analysis of whether modern disqualifications track public-safety necessity or administrative convenience.',
    'If disqualifications are originalist-justified, suppression is the cost of coherent boundary-setting. If modern disqualifications exceed what originalism justifies, the reading is overlaying extraction onto coordination. This affects whether the tangled_rope classification is the terminal assessment or whether correction would shift it toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_necessary_boundary, empirical, 'Whether measured suppression reflects necessary boundary-setting or extraction beyond original scope.').

omega_variable(
    reading_codification_authority,
    'Is the individual-right reading''s authority grounded in the founding-era text''s meaning (textual constraint on all readings), or in contemporary constitutional interpretation (reading chosen by current interpreters)?',
    'Meta-constitutional analysis: does the framework treat the text as having a fixed original meaning that constrains readings, or as a vessel for evolving interpretation? Comparison with how other textual constraints (First Amendment, Fifth Amendment) are treated in the same framework.',
    'If authority is textual/originalist, the reading''s legitimacy is independent and stable. If authority is interpretive, the reading''s codification is contingent on the interpreter''s power and choice. This affects whether the reading is a discovered constraint or an authoritatively declared one.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_codification_authority, conceptual, 'Whether the reading''s authority is textually grounded or interpretively chosen.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__individual_right_reading, 1791, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1791, second_amendment_text__individual_right_reading, theater_ratio, 1791, 0.08).
narrative_ontology:measurement_basis(seco_tr_t1791, projected).
narrative_ontology:measurement(seco_tr_t1900, second_amendment_text__individual_right_reading, theater_ratio, 1900, 0.12).
narrative_ontology:measurement_basis(seco_tr_t1900, projected).
narrative_ontology:measurement(seco_tr_t1968, second_amendment_text__individual_right_reading, theater_ratio, 1968, 0.18).
narrative_ontology:measurement_basis(seco_tr_t1968, observed).
narrative_ontology:measurement(seco_tr_t1994, second_amendment_text__individual_right_reading, theater_ratio, 1994, 0.22).
narrative_ontology:measurement_basis(seco_tr_t1994, observed).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_text__individual_right_reading, theater_ratio, 2008, 0.25).
narrative_ontology:measurement_basis(seco_tr_t2008, observed).
narrative_ontology:measurement(seco_tr_t2026, second_amendment_text__individual_right_reading, theater_ratio, 2026, 0.28).
narrative_ontology:measurement_basis(seco_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_text__individual_right_reading, base_extractiveness, 1791, 0.15).
narrative_ontology:measurement_basis(seco_be_t1791, projected).
narrative_ontology:measurement(seco_be_t1900, second_amendment_text__individual_right_reading, base_extractiveness, 1900, 0.25).
narrative_ontology:measurement_basis(seco_be_t1900, projected).
narrative_ontology:measurement(seco_be_t1968, second_amendment_text__individual_right_reading, base_extractiveness, 1968, 0.45).
narrative_ontology:measurement_basis(seco_be_t1968, observed).
narrative_ontology:measurement(seco_be_t1994, second_amendment_text__individual_right_reading, base_extractiveness, 1994, 0.52).
narrative_ontology:measurement_basis(seco_be_t1994, observed).
narrative_ontology:measurement(seco_be_t2008, second_amendment_text__individual_right_reading, base_extractiveness, 2008, 0.48).
narrative_ontology:measurement_basis(seco_be_t2008, observed).
narrative_ontology:measurement(seco_be_t2026, second_amendment_text__individual_right_reading, base_extractiveness, 2026, 0.62).
narrative_ontology:measurement_basis(seco_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1791, second_amendment_text__individual_right_reading, suppression_requirement, 1791, 0.35).
narrative_ontology:measurement_basis(seco_su_t1791, projected).
narrative_ontology:measurement(seco_su_t1900, second_amendment_text__individual_right_reading, suppression_requirement, 1900, 0.42).
narrative_ontology:measurement_basis(seco_su_t1900, projected).
narrative_ontology:measurement(seco_su_t1968, second_amendment_text__individual_right_reading, suppression_requirement, 1968, 0.55).
narrative_ontology:measurement_basis(seco_su_t1968, observed).
narrative_ontology:measurement(seco_su_t1994, second_amendment_text__individual_right_reading, suppression_requirement, 1994, 0.63).
narrative_ontology:measurement_basis(seco_su_t1994, observed).
narrative_ontology:measurement(seco_su_t2008, second_amendment_text__individual_right_reading, suppression_requirement, 2008, 0.59).
narrative_ontology:measurement_basis(seco_su_t2008, observed).
narrative_ontology:measurement(seco_su_t2026, second_amendment_text__individual_right_reading, suppression_requirement, 2026, 0.71).
narrative_ontology:measurement_basis(seco_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__individual_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_text__individual_right_reading, 0.12).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, second_amendment_text__collective_security_reading).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, second_amendment_text__originalist_civic_virtue_reading).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, permit_regimes_and_discretionary_denial).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, felon_disqualification_statutory_framework).

% DUAL FORMULATION NOTE:
% The second_amendment_text kernel admits three structurally distinct readings, each instantiating a different constraint with different ε, beneficiary/victim structures, and types. The individual_right_reading treats the operative clause as independent; the collective_security_reading treats the militia clause as conditioning or limiting it; the originalist_civic_virtue_reading emphasizes the citizen-soldier understanding. Each reading is a separate constraint file linked via this network field. The divergence in ε and type is not measurement error — it reflects the genuine structural difference in what the readings protect and what they cost.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_text__individual_right_reading, powerless, 1.0).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
