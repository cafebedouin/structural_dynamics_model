% ============================================================================
% CONSTRAINT STORY: remonstrance_authority__crown_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-25
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_remonstrance_authority__crown_reading, []).

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
 *   constraint_id: remonstrance_authority__crown_reading
 *   human_readable: Parlementary Remonstrance as Minoritarian Fiscal Veto (Crown Reading)
 *   domain: constitutional_history/political_economy/legal_authority
 *
 * SUMMARY:
 *   This story instantiates the crown reading of the remonstrance-authority
 *   kernel: the parlements' right to remonstrate against and delay
 *   registration of royal edicts is read, from the crown's structural
 *   position, as an illegitimate minoritarian veto exercised by a
 *   self-perpetuating, venal office-holding class to protect its own fiscal
 *   exemptions and those of allied privileged estates, at the expense of the
 *   treasury and of unprivileged taxpayers who absorb the residual burden.
 *   The sibling magistrate reading treats the same formal right as a
 *   fundamental constitutional check against arbitrary royal innovation; that
 *   reading is a separate constraint (magistrate_reading) with its own ε and
 *   beneficiary/victim structure, not a different observable of this one.
 *   Under the crown reading, the standing arrangement under contest is the
 *   parlements' exercise of remonstrance as it actually operated in the
 *   eighteenth century, not the reformed unitary-registration system the
 *   crown sought to install.
 *
 * KEY AGENTS:
 *   - royal_treasury: primary target — bears fiscal cost of delayed and blocked registration
 *   - crown_fiscal_administration: agenda-setter frustrated in its own function — must negotiate around the veto it formally administers
 *   - parlement_magistrates: primary beneficiary — venal office holders whose remonstrance right entrenches class and regional exemption
 *   - unprivileged_taxpayers: secondary target — absorbs the residual fiscal burden the exemptions displace
 *   - constitutional_historians: analytical observer — evaluates the function historically
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(remonstrance_authority__crown_reading, 0.78).
domain_priors:suppression_score(remonstrance_authority__crown_reading, 0.52).
domain_priors:theater_ratio(remonstrance_authority__crown_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(remonstrance_authority__crown_reading, snare).
narrative_ontology:human_readable(remonstrance_authority__crown_reading, "Parlementary Remonstrance as Minoritarian Fiscal Veto (Crown Reading)").
narrative_ontology:topic_domain(remonstrance_authority__crown_reading, "constitutional_history/political_economy/legal_authority").

domain_priors:requires_active_enforcement(remonstrance_authority__crown_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(remonstrance_authority__crown_reading, 'c47d8df8-370a-4cf6-b3e1-20f64152d589').
narrative_ontology:cs_kernel_codification('c47d8df8-370a-4cf6-b3e1-20f64152d589', distributed).
narrative_ontology:cs_authority_grounding('c47d8df8-370a-4cf6-b3e1-20f64152d589', practice).
narrative_ontology:cs_interpretation_layer_present('c47d8df8-370a-4cf6-b3e1-20f64152d589').
narrative_ontology:cs_reading_relation('c47d8df8-370a-4cf6-b3e1-20f64152d589', remonstrance_authority__magistrate_reading, coexists_with).
narrative_ontology:cs_axiom('c47d8df8-370a-4cf6-b3e1-20f64152d589', foundational, unitary_royal_legislative_sovereignty).
narrative_ontology:cs_axiom_status(unitary_royal_legislative_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('c47d8df8-370a-4cf6-b3e1-20f64152d589', unitary_royal_legislative_sovereignty, conventional).
narrative_ontology:cs_axiom('c47d8df8-370a-4cf6-b3e1-20f64152d589', secondary, venal_office_privilege_is_illegitimate_particularism).
narrative_ontology:cs_axiom_status(venal_office_privilege_is_illegitimate_particularism, holdable).
narrative_ontology:cs_axiom_grounding('c47d8df8-370a-4cf6-b3e1-20f64152d589', venal_office_privilege_is_illegitimate_particularism, instrumental).
narrative_ontology:cs_reference_frame('c47d8df8-370a-4cf6-b3e1-20f64152d589', absolute_royal_fiscal_prerogative).
narrative_ontology:cs_drift_state('c47d8df8-370a-4cf6-b3e1-20f64152d589', eve_of_1789, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('c47d8df8-370a-4cf6-b3e1-20f64152d589', '').
narrative_ontology:cs_kernel_id(remonstrance_authority__crown_reading, remonstrance_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(remonstrance_authority__crown_reading, parlement_magistrates).
narrative_ontology:constraint_beneficiary(remonstrance_authority__crown_reading, venal_office_holders).
narrative_ontology:constraint_beneficiary(remonstrance_authority__crown_reading, regional_privileged_estates).
narrative_ontology:constraint_victim(remonstrance_authority__crown_reading, royal_treasury).
narrative_ontology:constraint_victim(remonstrance_authority__crown_reading, crown_fiscal_administration).
narrative_ontology:constraint_victim(remonstrance_authority__crown_reading, unprivileged_taxpayers).
narrative_ontology:constraint_victim(remonstrance_authority__crown_reading, royal_reform_ministers).
narrative_ontology:constraint_vindicates(remonstrance_authority__crown_reading, sovereign_fiscal_supremacy_doctrine).
narrative_ontology:constraint_vindicates(remonstrance_authority__crown_reading, unitary_royal_legislative_will).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Depends on registered edicts to raise loans and levy taxes; when a parlement remonstrates and delays registration, the treasury's borrowing costs rise and revenue collection stalls at the moment it is most needed, typically during war finance emergencies. It cannot bypass registration without a lit de justice, which itself invites further resistance and non-compliance in the provinces.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, royal_treasury, payer,
    institutional, generational, trapped, national).

% Ministers and intendants draft fiscal edicts meant to apply uniformly across the realm. They must negotiate, cajole, exile magistrates, or force registration through royal presence, spending political capital and administrative bandwidth on a body whose formal task was to verify legal form, not to adjudicate the substance of fiscal policy.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, crown_fiscal_administration, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(remonstrance_authority__crown_reading, crown_fiscal_administration, agenda_setter).

% Hold venal, heritable offices that give them the exclusive standing to register or remonstrate against royal edicts within their jurisdiction. They collect prestige, patronage, and the ability to shield their own class and region from taxation by delaying or reshaping fiscal measures, while bearing almost none of the fiscal consequences of the delays they cause.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, parlement_magistrates, beneficiary,
    organized, civilizational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(remonstrance_authority__crown_reading, parlement_magistrates, agenda_setter).

% Purchased their offices as durable, transmissible property and use remonstrance to entrench the privileges attached to those offices — including tax exemptions — against any uniform fiscal reform that would erode the value of what they bought.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, venal_office_holders, beneficiary,
    organized, civilizational, arbitrage, regional).

% Nobility and clergy within a parlement's jurisdiction benefit when the local court remonstrates against measures that would tax their lands or income at parity with commoners; they lobby magistrates, many of whom are drawn from their own ranks, to use the remonstrance right on their behalf.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, regional_privileged_estates, beneficiary,
    organized, generational, mobile, regional).

% Bear a disproportionate share of whatever taxes ultimately survive the remonstrance-and-negotiation process, since the exemptions the magistrates defend fall on the privileged orders, shifting the residual fiscal burden onto commoners without any comparable body to remonstrate on their behalf.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, unprivileged_taxpayers, payer,
    powerless, biographical, trapped, regional).

% Individual ministers who attempt uniform fiscal reform (equalized land taxes, stamp duties, reduced venality) find their edicts blocked or watered down by remonstrance, and their careers and reputations suffer for proposing measures the parlements can portray as tyrannical innovation.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, royal_reform_ministers, payer,
    powerful, biographical, constrained, national).

% Royal administrators in the field who see firsthand how remonstrance delays translate into unequal, ad hoc enforcement across provinces, but have no formal standing within the remonstrance process itself — they report the effects upward without a seat at the registration dispute.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, provincial_intendants, excluded,
    moderate, biographical, constrained, regional).

% Assess, after the fact, whether the parlements' remonstrance right functioned as a genuine check on arbitrary power or as a mechanism by which a narrow, self-perpetuating class entrenched its own exemptions against necessary and popularly beneficial reform.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(remonstrance_authority__crown_reading, parlement_magistrates).
narrative_ontology:fixing_cost_class(remonstrance_authority__crown_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In its formal design, remonstrance lets a court of registration flag legal or procedural defects in a royal edict before it takes force, in principle protecting subjects from poorly drafted or contradictory law.
% TRANSFER_FUNCTION: The right moves fiscal risk and delay cost from the privileged office-holding and landed classes onto the royal treasury and onto unprivileged taxpayers, who absorb both the shortfall from unregistered measures and the residual burden once exemptions are preserved.
% ABSENT_VOICES: Unprivileged taxpayers and provincial intendants have no formal standing in the registration dispute; the negotiation happens entirely between crown ministers and magistrates drawn from or allied with the privileged orders whose exemptions are at stake.
% DISAPPEARANCE_RATIONALE: If remonstrance vanished overnight, royal edicts would register immediately and uniformly, fiscal reform could proceed without provincial veto points, treasury borrowing costs tied to registration delay would fall, and the parlements would lose their principal lever for defending regional and class-based tax exemptions — a substantial rearrangement of who bears the fiscal burden.
% FOUNDING_PROBLEM: Registration of royal edicts by sovereign courts was originally meant to verify that new law was consistent with existing law and properly recorded, guarding against clerical error and outright forgery in an age without centralized legislative drafting.
% FOUNDING_PROBLEM_CORROBORATION: Royal ministers across multiple reigns (Turgot, Calonne) attest that by the eighteenth century remonstrance no longer functioned as verification but as a device by which magistrates substituted their own policy judgment, and largely their own class interest, for the crown's; contemporary intendant correspondence outside the parlements corroborates that the delays tracked exemption-defense rather than any detected legal defect.
narrative_ontology:disappearance_verdict(remonstrance_authority__crown_reading, world_rearranges).
narrative_ontology:founding_problem_status(remonstrance_authority__crown_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(remonstrance_authority__crown_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(remonstrance_authority__crown_reading, 'none', 1).
narrative_ontology:epsilon_provenance(remonstrance_authority__crown_reading, 0.78, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(remonstrance_authority__crown_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(remonstrance_authority__crown_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(remonstrance_authority__crown_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.78 at interval end) because, under the crown reading, remonstrance functions as leverage that magistrates use to preserve fiscal exemptions unrelated to the edicts' legal form, imposing borrowing-cost and delay costs on the treasury and shifting the ultimate tax burden onto commoners. Suppression is authored moderate (0.52) rather than extreme because the crown retained the lit de justice and exile as counter-levers throughout the period — the veto was contested, not absolute — but its rising trajectory reflects the entrenchment of remonstrance practice across the century as magistrates elaborated increasingly elaborate constitutional arguments for their standing. Theater ratio rises to 0.42 because an increasing share of remonstrance activity, by the crown's lights, took the form of performative appeals to 'ancient constitution' rhetoric rather than substantive legal-defect review — the original verification function had by the 1770s-80s become largely pretextual cover for interest defense. Accessibility collapse is moderate (0.4): alternative fiscal paths (direct taxation without registration, provincial estates, financial edicts by simple declaration) existed but were costly and politically fraught, so alternatives were narrowed but not eliminated. Resistance is authored high (0.71) because the crown, treasury, and reform ministers repeatedly and forcefully contested the right through lits de justice, exiles of magistrates, and outright suppression attempts (1771 Maupeou coup), which is itself evidence of how much friction the arrangement generated rather than how settled it was.
 *
 * DIRECTIONALITY LOGIC:
 *   Parlement magistrates and the estates allied with them sit near the full-beneficiary end: they collect prestige, patronage, and durable fiscal exemption from the arrangement while bearing almost none of the cost of the delays they cause. The royal treasury and reform ministers sit near the full-target end: they bear the fiscal and political cost of an obstruction they cannot simply route around, given magistrates' arbitrage-grade exit (venal offices could be sold, held, or defended across reigns) versus the treasury's structural entrapment in the registration requirement. Unprivileged taxpayers are also targets, though indirectly — they do not negotiate with magistrates at all; the exemptions magistrates defend are simply subtracted from the tax base before the remaining burden falls on them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (verification of edicts against existing law and correct recording) is authored dead — by the eighteenth century the function had been substituted by policy review keyed to class and regional interest, per the corroboration of reform ministers and intendant correspondence outside the parlements. The disappearance_verdict of world_rearranges combined with founding_problem_status of dead is exactly the capture/zombie signature the R5 mismatch consumer is built to catch: an arrangement whose stated function is gone but whose removal would still visibly rearrange who bears fiscal costs, because a different, unstated function (class and regional exemption defense) has been substituted underneath the same formal right. This is why the crown reading claims snare rather than tangled_rope: verification-of-form is not read, from this seat, as a live coordination function still operating alongside the extraction — it is read as dead cover.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verification_function_genuinely_dead_or_merely_captured,
    'Did the legal-verification function of remonstrance actually atrophy to nothing by the eighteenth century, or did it persist alongside the exemption-defense function, making this a tangled rope rather than a pure snare from a neutral observer''s seat?',
    'Systematic content analysis of remonstrance texts across the century, coding each for genuine legal-defect argumentation versus policy/interest argumentation, compared against the rate at which crown edicts were found to contain actual drafting errors flagged by parlements.',
    'If a substantial share of remonstrances still performed genuine legal review, the crown reading''s snare classification overstates the case and a tangled_rope reading (coordination function still partly live, extraction layered on top) would be more defensible even from a crown-sympathetic seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_function_genuinely_dead_or_merely_captured, empirical, 'Whether the founding verification function was genuinely dead or partially persisting alongside captured exemption-defense.').

omega_variable(
    crown_reading_vs_magistrate_reading_locus_of_disagreement,
    'The two readings of the remonstrance_authority kernel disagree principally on where illegitimate extraction is located — in the crown''s fiscal demands (magistrate reading) or in the magistrates'' exemption defense (crown reading). Is this disagreement resolvable by evidence, or is it an irreducible framing choice about which baseline (royal sovereignty vs. ancient constitutional liberty) is the legitimate reference point?',
    'This is not resolvable by additional historical fact alone; it depends on which theory of sovereign legitimacy (unitary royal will vs. customary/estates-based constitutionalism) is taken as the evaluative baseline — an irreducibly conceptual/preference-laden choice, though the distributional facts (who paid, who was exempted) are separately empirically checkable.',
    'Adopting the crown reading''s baseline routes ε and victim assignment as authored here; adopting the magistrate reading''s baseline would invert beneficiary and victim roles entirely for the same formal institution, producing a structurally different constraint (magistrate_reading), not a different measurement of this one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(crown_reading_vs_magistrate_reading_locus_of_disagreement, conceptual, 'The kernel-level framing choice between royal-sovereignty and ancient-liberty baselines, routed here rather than folded into this reading''s classification.').

omega_variable(
    coalition_capacity_of_unprivileged_taxpayers,
    'Could unprivileged taxpayers, as a powerless but numerous class, have exercised any coalition leverage against the exemption-defense function of remonstrance, or were they structurally without any comparable institutional lever throughout the period?',
    'Comparative study of provincial estates and municipal bodies that did have some standing to petition against tax burdens, assessing whether any such channel provided even partial counter-leverage prior to 1789.',
    'If some coalition capacity existed (via provincial estates, tax revolts, or municipal petitions) the powerless-payer characterization should be qualified; if none existed, the trapped/powerless characterization for unprivileged_taxpayers stands as authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coalition_capacity_of_unprivileged_taxpayers, empirical, 'Whether unprivileged taxpayers had any coalition-based counter-leverage against the exemptions remonstrance defended.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(remonstrance_authority__crown_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(remo_tr_t0, remonstrance_authority__crown_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(remo_tr_t10, remonstrance_authority__crown_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(remo_tr_t20, remonstrance_authority__crown_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(remo_tr_t30, remonstrance_authority__crown_reading, theater_ratio, 30, 0.31).
narrative_ontology:measurement(remo_tr_t40, remonstrance_authority__crown_reading, theater_ratio, 40, 0.36).
narrative_ontology:measurement(remo_tr_t50, remonstrance_authority__crown_reading, theater_ratio, 50, 0.4).
narrative_ontology:measurement(remo_tr_t60, remonstrance_authority__crown_reading, theater_ratio, 60, 0.42).

% Extraction over time
narrative_ontology:measurement(remo_be_t0, remonstrance_authority__crown_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(remo_be_t10, remonstrance_authority__crown_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(remo_be_t20, remonstrance_authority__crown_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(remo_be_t30, remonstrance_authority__crown_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(remo_be_t40, remonstrance_authority__crown_reading, base_extractiveness, 40, 0.71).
narrative_ontology:measurement(remo_be_t50, remonstrance_authority__crown_reading, base_extractiveness, 50, 0.75).
narrative_ontology:measurement(remo_be_t60, remonstrance_authority__crown_reading, base_extractiveness, 60, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(remo_su_t0, remonstrance_authority__crown_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(remo_su_t10, remonstrance_authority__crown_reading, suppression_requirement, 10, 0.35).
narrative_ontology:measurement(remo_su_t20, remonstrance_authority__crown_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(remo_su_t30, remonstrance_authority__crown_reading, suppression_requirement, 30, 0.44).
narrative_ontology:measurement(remo_su_t40, remonstrance_authority__crown_reading, suppression_requirement, 40, 0.47).
narrative_ontology:measurement(remo_su_t50, remonstrance_authority__crown_reading, suppression_requirement, 50, 0.5).
narrative_ontology:measurement(remo_su_t60, remonstrance_authority__crown_reading, suppression_requirement, 60, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(remonstrance_authority__crown_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(remonstrance_authority__crown_reading, 0.08).
narrative_ontology:affects_constraint(remonstrance_authority__crown_reading, magistrate_reading).
narrative_ontology:affects_constraint(remonstrance_authority__crown_reading, venal_office_market).
narrative_ontology:affects_constraint(remonstrance_authority__crown_reading, ancien_regime_fiscal_reform_attempts).

% DUAL FORMULATION NOTE:
% remonstrance_authority__crown_reading and remonstrance_authority__magistrate_reading decompose a single natural-language kernel (the parlements' remonstrance right) into two structurally distinct constraints per the ε-invariance principle: the crown reading authors high ε for royal fiscal authority and places the crown/treasury among the victims of a minoritarian veto; the magistrate reading authors high ε for royal power and places the magistrates/subjects as beneficiaries of a constitutional check against arbitrary innovation. Both readings share the same kernel (the stabilized institutional practice of registration-with-remonstrance) but are not two measurements of one constraint — they are two constraints linked here and in the sibling file via affects_constraints, each with its own claimed_type, beneficiary/victim structure, and metrics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(remonstrance_authority__crown_reading, institutional, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
