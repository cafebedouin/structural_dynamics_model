% ============================================================================
% CONSTRAINT STORY: salic_prohibition__sovereign_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_salic_prohibition__sovereign_override_reading, []).

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
 *   constraint_id: salic_prohibition__sovereign_override_reading
 *   human_readable: Salic Prohibition as Sovereign-Revocable Positive Law
 *   domain: constitutional/dynastic/political
 *
 * SUMMARY:
 *   Under the sovereign_override_reading, Salic Law (the prohibition on
 *   female royal succession) is understood as revocable positive law grounded
 *   entirely in sovereign legislative prerogative. A reigning monarch may
 *   override the prohibition through Pragmatic Sanction or decree to permit
 *   female succession, legitimize cognatic descent, or name a female heir.
 *   Challengers to a female successor appointed by sovereign act are
 *   construed as rebels against legitimate authority, not defenders of a
 *   higher law. The male-line monopoly is maintained by law only as long as
 *   the sovereign chooses to enforce it; a successor may abandon it. This
 *   reading contrasts sharply with the immutable_mandate_reading (which
 *   treats Salic Law as irrevocable constitutional or divine mandate) and the
 *   cognatic_reversion_reading (which treats Salic Law as an anachronistic
 *   Frankish custom never properly binding on non-Frankish territories). The
 *   sovereign_override_reading coherently explains cases where monarchs grant
 *   female succession (Charles VII's Pragmatic Sanction in favor of Maria
 *   Theresa's inheritance of Austrian lands; female regencies and eventual
 *   succession in Iberian kingdoms) as lawful sovereign acts, not violations
 *   of a higher law.
 *
 * KEY AGENTS:
 *   - Reigning monarch: agenda-setter, holds prerogative to override through sovereign act
 *   - Male royal line: beneficiary, monopolizes succession under standing rule
 *   - Female royal claimants: payer, trapped by identity-lock, may petition for override
 *   - Nobility and Estates: organized beneficiary-payers, benefit from predictability but constrained by sovereign will
 *   - Foreign powers: institutional payers, face uncertainty costs from rule revocability
 *   - Church authority: observer-beneficiary, validates sovereign legitimacy
 *   - Alternative succession advocates: excluded, frame sovereignty constraints as rebellion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__sovereign_override_reading, 0.62).
domain_priors:suppression_score(salic_prohibition__sovereign_override_reading, 0.71).
domain_priors:theater_ratio(salic_prohibition__sovereign_override_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__sovereign_override_reading, tangled_rope).
narrative_ontology:human_readable(salic_prohibition__sovereign_override_reading, "Salic Prohibition as Sovereign-Revocable Positive Law").
narrative_ontology:topic_domain(salic_prohibition__sovereign_override_reading, "constitutional/dynastic/political").

domain_priors:requires_active_enforcement(salic_prohibition__sovereign_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__sovereign_override_reading, '71339786-0cd5-400d-b8fd-882fa73ac6f1').
narrative_ontology:cs_kernel_codification('71339786-0cd5-400d-b8fd-882fa73ac6f1', fixed_text).
narrative_ontology:cs_authority_grounding('71339786-0cd5-400d-b8fd-882fa73ac6f1', extraction).
narrative_ontology:cs_interpretation_layer_present('71339786-0cd5-400d-b8fd-882fa73ac6f1').
narrative_ontology:cs_reading_relation('71339786-0cd5-400d-b8fd-882fa73ac6f1', salic_prohibition__immutable_mandate_reading, coexists_with).
narrative_ontology:cs_reading_relation('71339786-0cd5-400d-b8fd-882fa73ac6f1', salic_prohibition__cognatic_reversion_reading, influences).
narrative_ontology:cs_axiom('71339786-0cd5-400d-b8fd-882fa73ac6f1', foundational, sovereign_prerogative_supreme_succession_authority).
narrative_ontology:cs_axiom_status(sovereign_prerogative_supreme_succession_authority, holdable).
narrative_ontology:cs_axiom_grounding('71339786-0cd5-400d-b8fd-882fa73ac6f1', sovereign_prerogative_supreme_succession_authority, conventional).
narrative_ontology:cs_axiom('71339786-0cd5-400d-b8fd-882fa73ac6f1', foundational, salic_law_revocable_by_sovereign_decree).
narrative_ontology:cs_axiom_status(salic_law_revocable_by_sovereign_decree, holdable).
narrative_ontology:cs_axiom_grounding('71339786-0cd5-400d-b8fd-882fa73ac6f1', salic_law_revocable_by_sovereign_decree, conventional).
narrative_ontology:cs_axiom('71339786-0cd5-400d-b8fd-882fa73ac6f1', secondary, female_succession_via_sovereign_grace_legitimate).
narrative_ontology:cs_axiom_status(female_succession_via_sovereign_grace_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('71339786-0cd5-400d-b8fd-882fa73ac6f1', female_succession_via_sovereign_grace_legitimate, deontological).
narrative_ontology:cs_reference_frame('71339786-0cd5-400d-b8fd-882fa73ac6f1', sovereign_legislative_authority_framework).
narrative_ontology:cs_drift_state('71339786-0cd5-400d-b8fd-882fa73ac6f1', late_medieval_succession_crises, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('71339786-0cd5-400d-b8fd-882fa73ac6f1', '').
narrative_ontology:cs_kernel_id(salic_prohibition__sovereign_override_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__sovereign_override_reading, reigning_monarch).
narrative_ontology:constraint_beneficiary(salic_prohibition__sovereign_override_reading, male_royal_line).
narrative_ontology:constraint_victim(salic_prohibition__sovereign_override_reading, female_royal_claimants).
narrative_ontology:constraint_victim(salic_prohibition__sovereign_override_reading, cognatic_succession_partisans).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(salic_prohibition__sovereign_override_reading, nobility_and_estates).
narrative_ontology:constraint_beneficiary(salic_prohibition__sovereign_override_reading, church_authority).
narrative_ontology:constraint_victim(salic_prohibition__sovereign_override_reading, nobility_and_estates).
narrative_ontology:constraint_victim(salic_prohibition__sovereign_override_reading, foreign_powers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the prerogative to declare the succession law through sovereign act (Pragmatic Sanction, edict, or legislative decree). Under this reading, the monarch may unilaterally override Salic prohibition to name a female heir or legitimize cognatic descent. The constraint operates as long as the reigning sovereign chooses to enforce it; a successor may revoke it at will. The monarch benefits by maintaining male-line control when it serves dynastic interest, but retains the legal flexibility to deviate if circumstances demand (marital failure, lack of male heirs, political alliance).
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, reigning_monarch, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(salic_prohibition__sovereign_override_reading, reigning_monarch, beneficiary).

% Monopolizes succession under the standing rule; no female heir can compete. The constraint protects their exclusive claim by law. However, this protection is contingent on sovereign will — a reigning monarch can dissolve it, and male-line members cannot exit the constraint without abdication or armed rebellion.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, male_royal_line, beneficiary,
    powerful, generational, trapped, national).

% Barred from succession by positive law, despite possessing royal blood and often equal capability. They may petition the sovereign for a Pragmatic Sanction, but cannot demand it by right — their succession depends on the reigning monarch's discretionary choice. Exit would require renouncing royal identity and claim, which dissolves the agent's structural position entirely. Female claimants may organize opposition (salique protests, advocacy for law change) but remain trapped by the law's standing operation until the sovereign acts.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, female_royal_claimants, payer,
    powerful, biographical, identity_locked, national).

% Benefit from a clear, predictable male-line succession rule (reduces succession disputes and civil war risk). They also pay by being bound by whatever succession law the sovereign declares, and may bear costs if the sovereign revokes Salic prohibition (new claimants, civil war between factions supporting competing heirs). Their exit option is limited to collective resistance (Estates General assembly, refusal to recognize a female successor chosen by the sovereign) but such resistance is costly and may be suppressed.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, nobility_and_estates, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(salic_prohibition__sovereign_override_reading, nobility_and_estates, payer).

% Bear uncertainty costs: the constraint's revocability means foreign powers cannot rely on a stable, universally understood succession law. When a sovereign overrides Salic prohibition (as France did with the Pragmatic Sanction under Charles VII), rivals and neighbors must adjust their dynastic calculations and alliance strategies. They may support challengers, contest the legitimacy of the female heir, or prepare for war to enforce an alternative reading.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, foreign_powers, payer,
    institutional, biographical, constrained, global).

% Observes and sometimes validates the sovereign's authority to override the law through papal dispensation, legitimacy doctrine, or canon-law precedent. The Church's role is supportive (blessing the sovereign's choice) rather than originating; the Church benefits by maintaining leverage as the validator of dynastic legitimacy, but does not set the succession law itself under this reading.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, church_authority, observer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(salic_prohibition__sovereign_override_reading, church_authority, beneficiary).

% Argue that Salic Law should be reformable by counsel, Estates, or legal precedent—not by sovereign will alone. Under the sovereign_override_reading, these voices are structurally excluded: the law's legitimacy rests entirely on sovereign prerogative, and attempts to constitutionalize it or subject it to collective approval are treated as rebellion against royal authority. They remain marginalized until a future reading gains purchase (or until a sovereign weakened by war or succession crisis recognizes their claims).
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, alternative_succession_advocates, excluded,
    moderate, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(salic_prohibition__sovereign_override_reading, reigning_monarch).
narrative_ontology:fixing_cost_class(salic_prohibition__sovereign_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a transparent, predictable male-line succession rule that reduces uncertainty about the royal heir and minimizes civil war over contested claims. The clear exclusion of females eliminates a class of potential claimants and their factions, simplifying dynastic calculation for nobility and foreign powers.
% TRANSFER_FUNCTION: Transfers succession entitlement from potentially all royal blood (cognatic) to the male line alone (agnatic). The constraint moves authority over succession disputes away from collective bodies (Estates, Church assembly) to the sovereign's personal prerogative. Females of royal blood transfer their dynastic expectation to male relatives; their own claim is extinguished by positive law.
% ABSENT_VOICES: Female royal claimants are present but powerless (trapped by identity-lock). Advocates for reform through law, Estates consent, or Church hierarchy are excluded by the sovereign_override reading—they would argue for constitutionalization of succession rules and collective validation, but remain outside the authorized conversation under a regime that treats such claims as seditious.
% DISAPPEARANCE_RATIONALE: If Salic prohibition as a positive law vanished overnight, the entire succession field would restructure: female claimants would immediately assert cognatic rights, foreign powers would recalculate dynastic alliances, the nobility would fragment into factions supporting different heirs (male and female), and competing claims would likely trigger civil war or demand constitutional reform. The constraint's existence stabilizes expectations; its removal destabilizes succession entirely.
% FOUNDING_PROBLEM: Early medieval Frankish inheritance law excluded women from land and office to prevent fragmentation of fiefs and the emergence of foreign-held dynastic claims through female marriage. Applied to royal succession, Salic Law simplified succession and prevented the kingdom from passing to foreign hands via a queen's marriage to a foreign prince.
% FOUNDING_PROBLEM_CORROBORATION: Historians and legal scholars outside the benefiting parties (male-line supporters) dispute whether the founding problem persists: some argue that by the late medieval period, inheritance strategies and marriage contracts had evolved to prevent unwanted foreign claim (female succession no longer necessarily meant foreign rule); others argue the problem remains live wherever a queen-regnant might marry a foreign prince and subordinate the kingdom to his authority. Contemporary advocacy literature from female claimants and reform advocates contests the founding problem's reality without external corroboration in official records—the sovereign itself is the only authority that can declare the problem solved or ongoing.
narrative_ontology:disappearance_verdict(salic_prohibition__sovereign_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(salic_prohibition__sovereign_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__sovereign_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(salic_prohibition__sovereign_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(salic_prohibition__sovereign_override_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(salic_prohibition__sovereign_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(salic_prohibition__sovereign_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(salic_prohibition__sovereign_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high-moderate (0.62) because the constraint redistributes succession entitlement from cognatic (all royal blood) to agnatic (male line only), and the sovereign's discretionary override power means female claimants must sue for grace rather than claim right. Suppression is high (0.71) because the constraint's enforcement depends on active legal machinery (court refusal of female claims, treaties recognizing male heirs, military defense against female claimants supported by foreign powers) and the identity-lock of female claimants prevents exit without losing all social position. Theater is moderate (0.38): the constraint serves a real coordination function (clarity in succession, reduced civil war risk) but an increasing share of enforcement machinery defends male-line monopoly rather than the coordination goal itself. The measurement series tracks the constraint from early stability (lower extraction when male heirs are available, lower suppression when the rule is rarely tested) through periods of succession crisis (rising extraction as female claimants press claims, rising suppression as the sovereign enforces the male-line rule against pressure). All metrics are authored on the same time grid so the drift pattern is coherent.
 *
 * PERSPECTIVAL GAP:
 *   The seated gap between reigning sovereign and female claimant should produce different types: from the sovereign's perspective, Salic Law is a coordination device (clear succession) that the sovereign may revoke strategically—essentially a Rope or Tangled Rope the sovereign can dissolve. From the female claimant's perspective, Salic Law is a Snare—she is trapped by positive law, cannot exit, and must petition a potentially hostile sovereign for grace. The engine computes per-seat classification from power/exit/directionality: sovereign has institutional power and arbitrage exit (can revoke), so d is low and the constraint registers as coordination or coordin-with-extraction. Female claimant has powerful/biographical/identity_locked, so d is high and the constraint registers as extraction. This divergence is structurally true—the same law is coordination from one seat and extraction from another. The authored claim (Tangled Rope) captures the mixed structure from the system level; the per-seat computations will show the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The reigning monarch is the structural beneficiary under this reading: the sovereign holds the prerogative to enforce or override at will, and benefits from the discretionary power to resolve succession crises through either male-line enforcement or female-succession override. The male-line members benefit passively (monopoly protection) but are trapped by their own beneficiary status—they cannot exit without losing their exclusive claim. Female royal claimants are the primary targets (d near 1.0): they are barred by law, trapped by identity-lock, and their only exit is petition to a discretionary sovereign—no right, only grace. The nobility and Estates are near-symmetric (d near 0.5): they benefit from the rule's clarity and predictability, but are also bound by whatever the sovereign chooses and may bear costs if the rule is overridden (civil faction, disputed succession). Foreign powers are payers (d moderate-high): they face uncertainty costs from the rule's revocability and may have to support competing claimants or prepare for war. Church authority is observer-beneficiary: it validates the sovereign's choice and benefits from remaining the arbiter of legitimacy, but does not originate the law.
 *
 * MANDATROPHY ANALYSIS:
 *   Under the sovereign_override_reading, mandatrophy is prevented by the sovereign's retained prerogative to override. However, if successive sovereigns choose to enforce the male-line rule indefinitely despite succession crises that could be resolved through female succession, the constraint exhibits signs of inertial maintenance: the coordination function (clarity, reduced civil war) might be achieved equally well through a transparent, revocable female-succession rule, but enforcement persists because the male-line beneficiaries have institutional weight and no single sovereign bears enough cost to change it unilaterally. If the founding problem (preventing foreign claim through female marriage) is solved by marriage law or treaty rather than succession exclusion, but Salic Law persists theatrically, the constraint approaches Piton status. The theater_ratio rising from 0.20 to 0.38 over the interval suggests increasing performance: early enforcement is functional (real succession disputes prevented), later enforcement is more theatrical (defending male-line monopoly even when female succession would resolve succession crisis). This is not full mandatrophy (the constraint is not yet purely performative), but it tracks the pathway toward it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereign_will_vs_law_distinction,
    'Is Salic prohibition a positive law subject to sovereign revision, or an embedded constitutional limit that even the sovereign cannot override without delegitimizing royal authority?',
    'Historical record of sovereign attempts to override and the outcomes (acceptance vs. civil war). Did successful overrides (Pragmatic Sanction, female succession) strengthen or weaken the sovereign''s legitimacy? Did failed overrides provoke rebellion construed as defense of law against tyranny?',
    'If sovereigns who override face delegitimization and rebellion, Salic prohibition functions as a constitutional limit despite the rhetoric of sovereign prerogative—classification shifts toward immutable_mandate_reading. If overrides are accepted as the sovereign''s lawful prerogative, the sovereign_override_reading holds and the constraint is Tangled Rope (coordination + extraction via differential access to sovereign grace).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereign_will_vs_law_distinction, empirical, 'Whether sovereign override is legally legitimate or a constitutional violation.').

omega_variable(
    female_succession_extraction_asymmetry,
    'Does a female heir who inherits via Pragmatic Sanction benefit the reigning sovereign (by resolving succession crisis) or extract from the sovereign (by challenging the male-line monopoly)?',
    'Analysis of historical cases (Margaret I of Denmark, Isabella of Castile, Louis XV and the Pragmatic Sanction for Maria Theresa): did the sovereign grant succession to shore up legitimacy, or did the female claimant compel it through force, political pressure, or marriage alliance?',
    'If the sovereign grants succession strategically, the arrangement is Tangled Rope from the reigning sovereign''s perspective (benefits from resolving succession crisis). If female claimants extract succession by pressure, the arrangement is Snare from the female claimant''s perspective (trapped, must pressure a reluctant sovereign). The same constraint may be different types from different seats depending on the directionality of the override grant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(female_succession_extraction_asymmetry, empirical, 'Whether female succession is a sovereign''s strategic choice or a female claimant''s forced extraction.').

omega_variable(
    identity_lock_mechanism_cognatic_vs_dynastic,
    'Is the female claimant''s identity-lock rooted in biological/dynastic identity (always royal, always has a claim to assert) or in institutional identity fusion (her entire social position, authority, property, and legitimacy depend on royal status)?',
    'Post-exclusion behavior: if a female claimant exits royal status (renounces title, marries commoner, founds new identity), does she retain claim to succession? If she cannot exit without forfeiting everything, identity-lock is institutional. If she can exit and retain some autonomous status, lock is biological only.',
    'If lock is institutional, the constraint is more extractive (she loses her entire world by exit); if biological only, she retains an exit option and extraction is lower. Directionality and effective extraction both depend on lock mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_cognatic_vs_dynastic, conceptual, 'Whether identity-lock is dynastic/biological or institutional/social.').

omega_variable(
    reading_kernel_contest_committer,
    'Is this constraint the sovereign_override_reading, or does the immutable_mandate_reading better describe the actual structural persistence of Salic Law across sovereign attempts to override?',
    'Empirical test: count overrides that succeeded (Pragmatic Sanction, female succession cases) vs. overrides that failed (rebellions, civil war, throne contested). If overrides succeed with acceptance, sovereign_override_reading holds. If overrides trigger civil war framed as defense of law, immutable_mandate_reading holds. Examine rhetoric: do rebels frame opposition as defense of Salic Law as immutable, or as defense against tyrannical sovereign abuse?',
    'This omega routes to the fundamental reading-choice itself. If immutable_mandate_reading computes as more coherent with the empirical record, the constraint is a Mountain (Salic Law as embedded law of succession nature), not Tangled Rope. The engine does not resolve this — it is the committer-frame omega that documents the reading-choice deliberation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_contest_committer, conceptual, 'Whether the constraint is actually a sovereign-revocable positive law or an immutable dynastic constitutional limit.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__sovereign_override_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sali_tr_t0, salic_prohibition__sovereign_override_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(sali_tr_t3, salic_prohibition__sovereign_override_reading, theater_ratio, 3, 0.26).
narrative_ontology:measurement(sali_tr_t6, salic_prohibition__sovereign_override_reading, theater_ratio, 6, 0.32).
narrative_ontology:measurement(sali_tr_t9, salic_prohibition__sovereign_override_reading, theater_ratio, 9, 0.36).
narrative_ontology:measurement(sali_tr_t12, salic_prohibition__sovereign_override_reading, theater_ratio, 12, 0.38).

% Extraction over time
narrative_ontology:measurement(sali_be_t0, salic_prohibition__sovereign_override_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(sali_be_t3, salic_prohibition__sovereign_override_reading, base_extractiveness, 3, 0.54).
narrative_ontology:measurement(sali_be_t6, salic_prohibition__sovereign_override_reading, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(sali_be_t9, salic_prohibition__sovereign_override_reading, base_extractiveness, 9, 0.61).
narrative_ontology:measurement(sali_be_t12, salic_prohibition__sovereign_override_reading, base_extractiveness, 12, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(sali_su_t0, salic_prohibition__sovereign_override_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(sali_su_t3, salic_prohibition__sovereign_override_reading, suppression_requirement, 3, 0.62).
narrative_ontology:measurement(sali_su_t6, salic_prohibition__sovereign_override_reading, suppression_requirement, 6, 0.67).
narrative_ontology:measurement(sali_su_t9, salic_prohibition__sovereign_override_reading, suppression_requirement, 9, 0.7).
narrative_ontology:measurement(sali_su_t12, salic_prohibition__sovereign_override_reading, suppression_requirement, 12, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(salic_prohibition__sovereign_override_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(salic_prohibition__sovereign_override_reading, 0.12).
narrative_ontology:affects_constraint(salic_prohibition__sovereign_override_reading, salic_prohibition__immutable_mandate_reading).
narrative_ontology:affects_constraint(salic_prohibition__sovereign_override_reading, salic_prohibition__cognatic_reversion_reading).

% DUAL FORMULATION NOTE:
% The salic_prohibition kernel decomposes into three structurally distinct readings: sovereign_override_reading (this story), immutable_mandate_reading, and cognatic_reversion_reading. Each reading has a different epsilon, different beneficiary/victim structure, and different classification. The sovereign_override_reading treats Salic Law as positive law revocable by sovereign will; immutable_mandate_reading treats it as irrevocable constitutional/divine law; cognatic_reversion_reading treats it as Frankish anachronism inapplicable to non-Frankish lands. The three readings coexist in historical dispute—different factions and dynasties held each reading simultaneously. They are linked via network.affects_constraints because an argument that succeeds in one reading (e.g., that the founding problem is solved) may undermine another (by showing that agnatic exclusion is no longer necessary for the coordination goal). Each story's epsilon is fixed to its own reading's reference point (the standing arrangement under contest, assessed by that reading's lights), not averaged or hedged across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(salic_prohibition__sovereign_override_reading, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
