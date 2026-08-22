% ============================================================================
% CONSTRAINT STORY: remonstrance_authority__crown_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Parlementary Remonstrance as Illegitimate Minoritarian Veto
 *   domain: constitutional_history/political_economy/legal_authority
 *
 * SUMMARY:
 *   Under the Ancien Régime, the Parlement of Paris (and provincial
 *   parlements) claimed a 'right of remonstrance' — the power to refuse
 *   registration of royal edicts, forcing the King to either withdraw the
 *   measure or impose it via a lit de justice. The crown_reading frames this
 *   as an illegitimate minoritarian veto: venal office-holders (who purchased
 *   their positions) used remonstrance to block fiscal reforms that
 *   threatened their tax exemptions and office values, while claiming to
 *   defend 'ancient liberties.' The constraint persisted because the Crown
 *   lacked the administrative capacity to bypass parlementary registration
 *   without political crisis, and because the parlements successfully framed
 *   their particularist interests as constitutional principle. Extraction
 *   falls on the royal fisc (blocked reforms) and on unrepresented taxpayers
 *   (who bore the burden of unreformed taxation). The magistrate_reading
 *   inverts this: remonstrance is the fundamental constitutional mechanism
 *   preserving liberty against arbitrary innovation. These are not the same
 *   constraint viewed differently — they have different ε, different
 *   beneficiary/victim structures, different enforcement logics. They are
 *   linked as a constraint family.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(remonstrance_authority__crown_reading, 0.78).
domain_priors:suppression_score(remonstrance_authority__crown_reading, 0.65).
domain_priors:theater_ratio(remonstrance_authority__crown_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(remonstrance_authority__crown_reading, snare).
narrative_ontology:human_readable(remonstrance_authority__crown_reading, "Parlementary Remonstrance as Illegitimate Minoritarian Veto").
narrative_ontology:topic_domain(remonstrance_authority__crown_reading, "constitutional_history/political_economy/legal_authority").

domain_priors:requires_active_enforcement(remonstrance_authority__crown_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(remonstrance_authority__crown_reading, '800f805d-3f99-4239-9d89-8ed2e534f9e6').
narrative_ontology:cs_kernel_codification('800f805d-3f99-4239-9d89-8ed2e534f9e6', implicit).
narrative_ontology:cs_authority_grounding('800f805d-3f99-4239-9d89-8ed2e534f9e6', lineage).
narrative_ontology:cs_interpretation_layer_present('800f805d-3f99-4239-9d89-8ed2e534f9e6').
narrative_ontology:cs_reading_relation('800f805d-3f99-4239-9d89-8ed2e534f9e6', remonstrance_authority__magistrate_reading, forecloses).
narrative_ontology:cs_axiom('800f805d-3f99-4239-9d89-8ed2e534f9e6', foundational, remonstrance_as_venal_privilege_protection).
narrative_ontology:cs_axiom_status(remonstrance_as_venal_privilege_protection, holdable).
narrative_ontology:cs_axiom_grounding('800f805d-3f99-4239-9d89-8ed2e534f9e6', remonstrance_as_venal_privilege_protection, empirically_contingent).
narrative_ontology:cs_axiom('800f805d-3f99-4239-9d89-8ed2e534f9e6', foundational, sovereign_fiscal_autonomy_as_constitutional_necessity).
narrative_ontology:cs_axiom_status(sovereign_fiscal_autonomy_as_constitutional_necessity, holdable).
narrative_ontology:cs_axiom_grounding('800f805d-3f99-4239-9d89-8ed2e534f9e6', sovereign_fiscal_autonomy_as_constitutional_necessity, deontological).
narrative_ontology:cs_reference_frame('800f805d-3f99-4239-9d89-8ed2e534f9e6', fronde_era_remonstrance_as_emergency_brake).
narrative_ontology:cs_drift_state('800f805d-3f99-4239-9d89-8ed2e534f9e6', pre_revolutionary_parlementary_obstruction, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('800f805d-3f99-4239-9d89-8ed2e534f9e6', '').
narrative_ontology:cs_kernel_id(remonstrance_authority__crown_reading, remonstrance_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(remonstrance_authority__crown_reading, parlementary_officers).
narrative_ontology:constraint_beneficiary(remonstrance_authority__crown_reading, venal_office_holders).
narrative_ontology:constraint_beneficiary(remonstrance_authority__crown_reading, provincial_estates_allied).
narrative_ontology:constraint_victim(remonstrance_authority__crown_reading, crown_fiscal_authority).
narrative_ontology:constraint_victim(remonstrance_authority__crown_reading, royal_administration).
narrative_ontology:constraint_victim(remonstrance_authority__crown_reading, taxpaying_subjects_without_representation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Venal office-holders who purchased their positions; remonstrance protects the value of their offices and their tax exemptions. They control the registration gate and frame their corporatist interest as constitutional duty. Exit means selling office (capital loss) or resigning (status loss); constrained by sunk investment and professional identity.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, parlementary_officers, beneficiary,
    organized, biographical, constrained, national).

% The broader corps of office-holders (not just parlementaires) whose office values depend on the fiscal status quo. They mobilize in solidarity with parlementary remonstrances because royal fiscal reform threatens the entire venal system. Their agenda-setting role is collective: they define what counts as 'attack on property' to include any tax on office value.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, venal_office_holders, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(remonstrance_authority__crown_reading, venal_office_holders, agenda_setter).

% Provincial estates (Pays d'états) that negotiated tax privileges with the Crown; they ally with parlements to block uniform taxation that would erode their negotiated advantages. They benefit from remonstrance as a veto on royal fiscal unity but are subordinate to parlementary leadership in the obstruction coalition.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, provincial_estates_allied, beneficiary,
    organized, biographical, constrained, regional).

% The royal treasury and its ministers; remonstrance blocks edicts needed for revenue, forcing reliance on expensive loans (rentes) and regressive taxes. The Crown has the lit de justice override but using it incurs political crisis and legitimacy costs. Exit from the constraint means either surrendering fiscal autonomy (unthinkable for a sovereign) or destroying the parlementary institution (Maupeou 1771 — temporarily successful but politically catastrophic).
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, crown_fiscal_authority, payer,
    institutional, generational, constrained, national).

% Intendants and financial officers who execute royal policy; their reform initiatives are routinely blocked by remonstrance, undermining administrative competence and creating perverse incentives (e.g., farming taxes to private financiers at high cost). They are victims of the constraint's obstruction but also its enforcers when lit de justice is used — a dual position captured by secondary_role in other stories, here rendered as institutional victimhood.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, royal_administration, payer,
    institutional, biographical, constrained, national).

% The Third Estate — peasants, urban workers, bourgeois not in venal offices — who bear the fiscal burden of blocked reforms (taille, gabelle, vingtièmes) but have no voice in remonstrance. They cannot exit the fiscal system; emigration is legally restricted and economically impossible. They are the silent payers of the extraction that remonstrance enables.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, taxpaying_subjects_without_representation, payer,
    powerless, biographical, trapped, national).

% Enlightenment writers (Voltaire, Montesquieu, Turgot, Necker) who analyzed remonstrance as either constitutional safeguard or aristocratic obstruction. They shaped the interpretive frame but held no formal power in the registration process. Their exit is analytical: they observe from outside the constraint's operational mechanics.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, philosophes_and_public_opinion, observer,
    moderate, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Originally: checking arbitrary royal innovation during regencies and minorities by requiring parlementary registration of edicts — a procedural brake on executive overreach. By 1661, this had mutated into a routine veto on fiscal policy.
% TRANSFER_FUNCTION: Moves fiscal authority from the Crown (which needs revenue for state functions) to the parlementary corps (which extracts registration compliance as rent) and to venal office-holders (who preserve tax exemptions). The transfer is not monetary but institutional: the Crown pays in political capital and borrowing costs; officers collect in protected privilege.
% ABSENT_VOICES: The taxpaying Third Estate — peasants, workers, unrepresented bourgeois — who would object to the fiscal paralysis if they had a seat. They are structurally excluded: the remonstrance process has no popular representation, and the parlements actively claimed to speak for 'the nation' while blocking reforms that would have benefited the actual nation.
% DISAPPEARANCE_RATIONALE: If remonstrance vanished overnight (as it briefly did under Maupeou 1771-1774), the Crown could register fiscal edicts without parlementary consent, enabling tax reform and reducing borrowing costs. The venal office system would lose its primary political protection. The state's fiscal-military capacity would increase. The world rearranged — which is why the Crown eventually destroyed the constraint (1789) and why the parlements fought to restore it (1774).
% FOUNDING_PROBLEM: During the Fronde (1648-1652), the minority of Louis XIV created a vacuum where royal authority could be exercised arbitrarily by regents and ministers. The Parlement of Paris claimed the right to remonstrate as a check on this arbitrariness, protecting subjects from unauthorized taxation and jurisdictional innovation.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (checking arbitrary power during a minority) is attested as dead by the Crown's own ministers (Colbert, Richelieu's legacy) and by the parlements' own practice: after 1661, Louis XIV ruled personally and remonstrance continued against his deliberate policies, not regency arbitrariness. No corroborating source outside the parlementary corps claims the founding problem remained live after 1715. The magistrate_reading's corroboration (that arbitrary innovation remains a permanent threat) is the sibling's self-assertion, not external corroboration.
narrative_ontology:disappearance_verdict(remonstrance_authority__crown_reading, world_rearranges).
narrative_ontology:founding_problem_status(remonstrance_authority__crown_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(remonstrance_authority__crown_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(remonstrance_authority__crown_reading, 'none', 1).
narrative_ontology:epsilon_provenance(remonstrance_authority__crown_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extraction is high (0.78 by 1789) because remonstrance systematically blocked fiscal rationalization, forcing the Crown into expensive loans and regressive taxation that fell on those without parlementary protection. Suppression is substantial (0.65) because the Crown's only counter-move was the lit de justice — a performative, costly, and politically dangerous override that parlements treated as tyranny, enabling them to mobilize public opinion. Theater ratio rises from 0.15 to 0.42: early remonstrances (Fronde era) addressed genuine governance crises; later remonstrances became ritualized obstruction of any reform threatening officer privileges. Resistance is high (0.72) because the Crown repeatedly attempted to suppress or circumvent remonstrance (Maupeou reform 1771, various lit de justice sessions), but the parlementary corps survived through institutional solidarity and public opinion management.
 *
 * PERSPECTIVAL GAP:
 *   The magistrate_reading computes a different type from the same structural facts because its declared beneficiaries (the 'nation' represented by magistrates) and victims (subjects of arbitrary power) invert the crown_reading's map. The engine will compute per-seat classifications from each story's declared beneficiary/victim arrays — the crown_reading produces snare for the taxpaying subject seat; the magistrate_reading will produce rope or mountain for the same seat. This divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Parlementary officers are beneficiaries (d ≈ 0.15): they collect rents from venal offices, control registration, and frame their interest as constitutional duty. The Crown is a victim (d ≈ 0.85) when thwarted: its fiscal authority is extracted from, but it retains the nuclear option (lit de justice) — a constrained victim with high power but high exit cost. Taxpaying subjects without representation are full victims (d ≈ 0.95): they bear the fiscal consequences of blocked reform with zero voice in the remonstrance process. Provincial estates allied with parlements are secondary beneficiaries (d ≈ 0.3): they gain leverage over royal taxation but remain subordinate to parlementary leadership.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting subjects from arbitrary royal innovation during minorities and regencies) was live in 1648 but dead by 1715 — the Crown had stabilized, fiscal needs were chronic, and remonstrance had become a tool of officer corporatism. The arrangement persisted 74 years past its founding problem's death because the officer corps controlled the registration gate and the Crown lacked a substitute administrative pathway. This is mandatrophy resolved as snare: a coordination mechanism (checking arbitrary power) that became pure extraction (blocking necessary reform for private benefit).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the crown_reading of remonstrance_authority a distinct constraint from the magistrate_reading, or are they observer perspectives on one constraint?',
    'Apply ε-invariance test: if the crown reading''s extraction (0.78) and the magistrate reading''s extraction differ structurally — different beneficiary/victim sets, different enforcement logic — they are distinct constraints linked by network.affects_constraints.',
    'Confirms this story instantiates one reading of a contested kernel; sibling reading is a separate constraint file.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment to ε-invariance across kernel readings').

omega_variable(
    naturalness_of_remonstrance_claim,
    'Does the parlementary claim that remonstrance protects ''ancient liberties'' reflect a genuine historical continuity, or is it a constructed privilege serving officer interests?',
    'Comparative institutional analysis: trace the expansion of remonstrance from exceptional political crises (1648-1652) to routine fiscal obstruction (1661-1789); assess whether the ''ancient liberties'' cited correspond to verifiable pre-1648 practice or were retroactively invented.',
    'If constructed, the constraint is a snare using historical framing as cover; if genuine continuity with mutated function, it is a tangled_rope with degraded coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_of_remonstrance_claim, empirical, 'Natural-law vs constructed privilege ambiguity in parlementary self-justification').

omega_variable(
    crown_victimhood_authenticity,
    'Is the Crown''s fiscal paralysis under remonstrance genuine extraction from a sovereign authority, or is the Crown a powerful actor strategically portraying itself as victim to centralize power?',
    'Counterfactual fiscal modeling: compare actual revenue shortfalls during remonstrance standoffs (1652, 1673, 1718, 1763, 1787-88) against the Crown''s alternative revenue capacity (domains, loans, extraordinary taxes). If shortfalls were survivable, Crown victimhood is strategic framing.',
    'If Crown victimhood is strategic, the snare classification still holds (extraction from taxpaying subjects persists) but the beneficiary/victim map shifts: Crown becomes agenda_setter using remonstrance as pretext for absolutism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crown_victimhood_authenticity, empirical, 'Whether Crown fiscal victimhood is structural or performative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(remonstrance_authority__crown_reading, 1648, 1789).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(remonstrance_crown_tr_t1648, remonstrance_authority__crown_reading, theater_ratio, 1648, 0.15).
narrative_ontology:measurement(remonstrance_crown_tr_t1661, remonstrance_authority__crown_reading, theater_ratio, 1661, 0.22).
narrative_ontology:measurement(remonstrance_crown_tr_t1715, remonstrance_authority__crown_reading, theater_ratio, 1715, 0.31).
narrative_ontology:measurement(remonstrance_crown_tr_t1763, remonstrance_authority__crown_reading, theater_ratio, 1763, 0.38).
narrative_ontology:measurement(remonstrance_crown_tr_t1787, remonstrance_authority__crown_reading, theater_ratio, 1787, 0.42).

% Extraction over time
narrative_ontology:measurement(remonstrance_crown_be_t1648, remonstrance_authority__crown_reading, base_extractiveness, 1648, 0.35).
narrative_ontology:measurement(remonstrance_crown_be_t1661, remonstrance_authority__crown_reading, base_extractiveness, 1661, 0.48).
narrative_ontology:measurement(remonstrance_crown_be_t1715, remonstrance_authority__crown_reading, base_extractiveness, 1715, 0.62).
narrative_ontology:measurement(remonstrance_crown_be_t1763, remonstrance_authority__crown_reading, base_extractiveness, 1763, 0.71).
narrative_ontology:measurement(remonstrance_crown_be_t1787, remonstrance_authority__crown_reading, base_extractiveness, 1787, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(remonstrance_crown_su_t1648, remonstrance_authority__crown_reading, suppression_requirement, 1648, 0.4).
narrative_ontology:measurement(remonstrance_crown_su_t1661, remonstrance_authority__crown_reading, suppression_requirement, 1661, 0.52).
narrative_ontology:measurement(remonstrance_crown_su_t1715, remonstrance_authority__crown_reading, suppression_requirement, 1715, 0.58).
narrative_ontology:measurement(remonstrance_crown_su_t1763, remonstrance_authority__crown_reading, suppression_requirement, 1763, 0.62).
narrative_ontology:measurement(remonstrance_crown_su_t1787, remonstrance_authority__crown_reading, suppression_requirement, 1787, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(remonstrance_authority__crown_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(remonstrance_authority__crown_reading, remonstrance_authority__magistrate_reading).
narrative_ontology:affects_constraint(remonstrance_authority__crown_reading, royal_fiscal_authority).
narrative_ontology:affects_constraint(remonstrance_authority__crown_reading, venal_office_system).
narrative_ontology:affects_constraint(remonstrance_authority__crown_reading, lit_de_justice_mechanism).

% DUAL FORMULATION NOTE:
% Constraint family decomposition: remonstrance_authority kernel splits into crown_reading (high ε, Crown victim, snare) and magistrate_reading (low ε, magistrates as guardians, likely rope/tangled_rope). The crown_reading's ε (0.78) and the magistrate_reading's ε will differ structurally — different beneficiary/victim sets, different enforcement logic. Not observable-dependent measurement of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(remonstrance_authority__crown_reading, institutional, 0.85).
constraint_indexing:directionality_override(remonstrance_authority__crown_reading, organized, 0.3).
constraint_indexing:directionality_override(remonstrance_authority__crown_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
