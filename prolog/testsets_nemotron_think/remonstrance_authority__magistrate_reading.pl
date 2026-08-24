% ============================================================================
% CONSTRAINT STORY: remonstrance_authority__magistrate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_remonstrance_authority__magistrate_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: remonstrance_authority__magistrate_reading
 *   human_readable: Parlementary Remonstrance Right as Fundamental Constitutional Check
 *   domain: constitutional_history/political_economy/legal_authority
 *
 * SUMMARY:
 *   The magistrate reading of remonstrance authority presents the parlement's
 *   right to refuse registration of royal edicts as a fundamental
 *   constitutional mechanism — a Mountain of ancient law that preserves
 *   liberty against arbitrary innovation. From this reading's seat, the
 *   remonstrance is not extraction but protection: the fundamental laws (lois
 *   fondamentales) are treated as natural limits on sovereignty, and the
 *   parlement's verification role is the constitutional guarantee that the
 *   king governs as a legitimate monarch rather than a despot. The structural
 *   reality, however, shows high and rising extractiveness (0.45→0.72)
 *   concentrated on fiscal reform edicts: each major reform ministry
 *   (Colbert, Law, Turgot, Necker, Calonne) faced remonstrance blocking tax
 *   reforms that would have touched magistrate exemptions. The constraint
 *   requires active enforcement (refusal to register, remonstrance drafting,
 *   resistance to lit de justice) and creates a beneficiary class (tax-exempt
 *   magistracy, noblesse de robe) whose identity is locked to the
 *   constraint's persistence. The claimed Mountain type diverges from the
 *   metric profile — the engine will detect this as a false summit candidate.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(remonstrance_authority__magistrate_reading, 0.72).
domain_priors:suppression_score(remonstrance_authority__magistrate_reading, 0.65).
domain_priors:theater_ratio(remonstrance_authority__magistrate_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(remonstrance_authority__magistrate_reading, mountain).
narrative_ontology:human_readable(remonstrance_authority__magistrate_reading, "Parlementary Remonstrance Right as Fundamental Constitutional Check").
narrative_ontology:topic_domain(remonstrance_authority__magistrate_reading, "constitutional_history/political_economy/legal_authority").

domain_priors:requires_active_enforcement(remonstrance_authority__magistrate_reading).
domain_priors:emerges_naturally(remonstrance_authority__magistrate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(remonstrance_authority__magistrate_reading, '64852305-f4db-4568-b36a-e05abe28b61b').
narrative_ontology:cs_kernel_codification('64852305-f4db-4568-b36a-e05abe28b61b', fixed_text).
narrative_ontology:cs_authority_grounding('64852305-f4db-4568-b36a-e05abe28b61b', lineage).
narrative_ontology:cs_interpretation_layer_present('64852305-f4db-4568-b36a-e05abe28b61b').
narrative_ontology:cs_reading_relation('64852305-f4db-4568-b36a-e05abe28b61b', remonstrance_authority__crown_reading, forecloses).
narrative_ontology:cs_axiom('64852305-f4db-4568-b36a-e05abe28b61b', foundational, remonstrance_as_fundamental_law).
narrative_ontology:cs_axiom_status(remonstrance_as_fundamental_law, holdable).
narrative_ontology:cs_axiom_grounding('64852305-f4db-4568-b36a-e05abe28b61b', remonstrance_as_fundamental_law, deontological).
narrative_ontology:cs_axiom('64852305-f4db-4568-b36a-e05abe28b61b', foundational, parlement_guardian_of_liberties).
narrative_ontology:cs_axiom_status(parlement_guardian_of_liberties, holdable).
narrative_ontology:cs_axiom_grounding('64852305-f4db-4568-b36a-e05abe28b61b', parlement_guardian_of_liberties, deontological).
narrative_ontology:cs_reference_frame('64852305-f4db-4568-b36a-e05abe28b61b', ancien_regime_constitutional_order).
narrative_ontology:cs_drift_state('64852305-f4db-4568-b36a-e05abe28b61b', pre_revolutionary_crisis, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('64852305-f4db-4568-b36a-e05abe28b61b', '').
narrative_ontology:cs_kernel_id(remonstrance_authority__magistrate_reading, remonstrance_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(remonstrance_authority__magistrate_reading, tax_exempt_magistracy).
narrative_ontology:constraint_beneficiary(remonstrance_authority__magistrate_reading, parlement_office_holders).
narrative_ontology:constraint_beneficiary(remonstrance_authority__magistrate_reading, noblesse_de_robe).
narrative_ontology:constraint_victim(remonstrance_authority__magistrate_reading, crown_fiscal_authority).
narrative_ontology:constraint_victim(remonstrance_authority__magistrate_reading, reforming_ministers).
narrative_ontology:constraint_victim(remonstrance_authority__magistrate_reading, tax_burdened_subjects).
narrative_ontology:constraint_vindicates(remonstrance_authority__magistrate_reading, fundamental_laws_of_kingdom).
narrative_ontology:constraint_vindicates(remonstrance_authority__magistrate_reading, ancient_liberties_doctrine).
narrative_ontology:constraint_vindicates(remonstrance_authority__magistrate_reading, parlement_as_guardian_of_constitution).
narrative_ontology:constraint_vindicates(remonstrance_authority__magistrate_reading, registration_as_constitutional_necessity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold venal offices in sovereign courts (parlements) that confer nobility, tax exemption, and the right to remonstrate against royal edicts. Their institutional identity fuses with the remonstrance function — to abandon remonstrance is to abandon their reason for existing as a corps. They set the agenda by refusing registration of fiscal edicts and drafting remonstrances invoking fundamental laws.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, tax_exempt_magistracy, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(remonstrance_authority__magistrate_reading, tax_exempt_magistracy, beneficiary).

% Lower-ranking magistrates and officers whose office value depends on the parlement's institutional prestige and tax privileges. They benefit from the remonstrance right's protection of their corps' autonomy but lack individual agenda-setting power. Exit means selling office at a loss or abandoning a career built on venal office tenure.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, parlement_office_holders, beneficiary,
    organized, biographical, constrained, regional).

% The broader noblesse de robe — families whose status and wealth derive from generations of judicial office-holding. They benefit from the tax exemption and social precedence secured by the parlement's constitutional claims. Their identity is bound to the 'ancient constitution' narrative; exit means social declension and loss of fiscal privilege.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, noblesse_de_robe, beneficiary,
    organized, generational, constrained, national).

% The king's fiscal apparatus (contrôleurs généraux, intendants) that must register edicts to raise revenue. When parlements refuse registration, the crown must either lit de justice (forced registration), exile magistrates, or negotiate — each costly in legitimacy and administrative capacity. The constraint extracts by blocking or delaying fiscal reform edicts, forcing the crown to bear higher borrowing costs or abandon reforms.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, crown_fiscal_authority, payer,
    institutional, biographical, constrained, national).

% Ministers (Turgot, Necker, Calonne) who attempt structural fiscal reform. They bear the political cost of parlement opposition — public remonstrances delegitimize their edicts, forcing resignation or compromise. Their exit is mobile (dismissal, exile) but the constraint extracts their reform agenda and political capital.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, reforming_ministers, payer,
    powerful, biographical, mobile, national).

% Third Estate taxpayers (peasants, urban workers, bourgeois) who bear the taille, gabelle, and vingtièmes while the noblesse de robe is exempt. When remonstrance blocks fiscal reform that would broaden the tax base, the extraction falls on them. They have no voice in the remonstrance process and no exit from the fiscal burden.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, tax_burdened_subjects, payer,
    powerless, immediate, trapped, national).

% Pays d'états (Brittany, Languedoc, Burgundy) with their own tax-negotiating privileges. They would object to parlement claims to speak for 'the nation' while defending particularist exemptions. Their voice is excluded from the remonstrance dialogue, which is framed as parlement vs. crown.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, provincial_estates, excluded,
    organized, biographical, constrained, regional).

% The body of constitutional jurisprudence (arrêts de règlement, fundamental laws, Loisel, Loyseau) that the parlement invokes. It does not act or collect rents but provides the interpretive framework. Its 'situation' is the accumulated textual authority the magistrate reading treats as binding.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, juridical_doctrine, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(remonstrance_authority__magistrate_reading, juridical_doctrine).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a constitutional check on arbitrary royal power by requiring royal edicts (especially fiscal) to be registered and reviewed by sovereign courts before taking force, ensuring conformity with fundamental laws.
% TRANSFER_FUNCTION: Moves fiscal reform agenda and legislative initiative from the crown to the parlements; moves tax burden from tax-exempt magistracy to Third Estate subjects; moves political legitimacy from ministers to magistrates when remonstrances force royal retreat.
% ABSENT_VOICES: Provincial estates (pays d'états) whose negotiated fiscal privileges are undermined by parlement's claim to universal guardianship; Third Estate taxpayers who bear the cost of blocked reforms but have no standing to remonstrate; Enlightenment publicists (Voltaire, Linguet) who criticized parlement as a privileged corps but were excluded from the official constitutional dialogue.
% DISAPPEARANCE_RATIONALE: If the remonstrance right vanished overnight, the crown could register fiscal edicts without delay, Turgot's or Calonne's reforms might have succeeded, the tax burden would have shifted toward the privileged orders earlier, and the pre-revolutionary fiscal crisis would have resolved differently — the Ancien Régime's collapse was partly caused by the constraint's extraction blocking reform.
% FOUNDING_PROBLEM: After the Wars of Religion, the crown's legislative authority became unmoored from customary limits; parlements claimed the right to verify edicts against fundamental laws (Salic law, inalienability of domain, Catholic unity) to prevent tyrannical innovation.
% FOUNDING_PROBLEM_CORROBORATION: The magistrate reading's founding narrative is attested by parlement's own arrêts de règlement and juridical treatises (Loisel, Loyseau). Counter-corroboration: royal jurists (Boucher d'Argis, Séguier) and ministers (Richelieu, Colbert) attested the founding problem was settled by royal sovereignty; the Estates General of 1614 and 1789 were summoned precisely because the remonstrance mechanism failed to resolve the fiscal crisis — outside the beneficiary set, the founding problem is read as dead or transformed.
narrative_ontology:disappearance_verdict(remonstrance_authority__magistrate_reading, world_rearranges).
narrative_ontology:founding_problem_status(remonstrance_authority__magistrate_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(remonstrance_authority__magistrate_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(remonstrance_authority__magistrate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(remonstrance_authority__magistrate_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(remonstrance_authority__magistrate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(remonstrance_authority__magistrate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, ExtMetricName, E),
    domain_priors:suppression_score(remonstrance_authority__magistrate_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(remonstrance_authority__magistrate_reading),
    narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(remonstrance_authority__magistrate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72) because the constraint's operation systematically blocks fiscal reforms that would tax the beneficiaries, forcing the crown into debt and the Third Estate into heavier burdens. Suppression (0.65) is substantial: the crown must use lit de justice, exile, or Maupeou's 1771 suppression to overcome remonstrance — active coercion, not passive compliance. Theater ratio (0.30) reflects genuine constitutional discourse mixed with performative defense of privilege; the ratio rises as reform pressure mounts (1750-1787). Accessibility collapse (0.75) is high: once the fundamental laws doctrine is accepted, alternatives (royal absolutism, Estates General, ministerial responsibility) appear illegitimate to the magistrate seat. Resistance (0.70) is high from both crown (lit de justice, exile) and reformers (public opinion, physiocrats). The claimed_type 'mountain' is the magistrate reading's self-understanding; the metrics describe the constraint's operation as an analytical observer sees it.
 *
 * PERSPECTIVAL GAP:
 *   From the tax_exempt_magistracy seat (agenda_setter, identity_locked), the constraint appears as Mountain: it is the constitution itself, natural and unchangeable. From the crown_fiscal_authority seat (payer, institutional, constrained), it appears as Snare: an illegitimate veto extracting reform capacity. From the tax_burdened_subjects seat (payer, powerless, trapped), it appears as Tangled Rope: the coordination story (protection against tyranny) is real but the extraction (their disproportionate tax burden) is asymmetric. The engine computes these per-seat divergences from the structural data — the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The magistrate corps (tax_exempt_magistracy, parlement_office_holders, noblesse_de_robe) are structural beneficiaries: they collect tax exemption, office value, and constitutional authority from the constraint (d → 0.1-0.2). The crown_fiscal_authority and reforming_ministers are targets: they bear the cost of blocked reforms and legitimacy loss (d → 0.8-0.9). The tax_burdened_subjects are also targets but powerless and trapped (d → 0.95). Provincial_estates are excluded — they would challenge the parlement's claim to speak for the nation but have no standing. The juridical_doctrine is an analytical observer (d = 0.5). The identity_locked exit for magistrates reflects professional identity fusion: the remonstrance right IS the corps' identity.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (checking post-Wars of Religion arbitrary power) was live in 1600 but became contested by 1750: the constraint persisted while the original tyranny threat receded, and the magistrate corps became the primary obstacle to fiscal reform needed to sustain the state. The mandate atrophied into privilege protection. The constraint prevents mislabeling by maintaining a genuine coordination function (registration does catch some arbitrary edicts) while the extraction asymmetry (tax exemption for magistrates, burden on Third Estate) is structural. The mandatrophy is unresolved — the constraint was abolished in 1790, not reformed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_privilege,
    'Is the remonstrance right a genuine fundamental law of the French kingdom (natural/immemorial) or a constructed privilege of the noblesse de robe that acquired constitutional veneer over time?',
    'Comparative analysis of pre-1600 Parlement registers: does the remonstrance practice predate the Wars of Religion as a constitutional doctrine, or does it emerge as a fiscal resistance tool later elevated to fundamental law? Historiographical consensus (Ranum, Doyle, Swann) vs. magistrate''s own arrêts de règlement.',
    'If constructed privilege, the Mountain claim is a false summit — FSM triggers reclassification to Tangled Rope (coordination + extraction). If genuine fundamental law, the Mountain claim holds and extraction metrics reflect crown''s violation of natural limits, not constraint''s operation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_privilege, conceptual, 'Whether the constraint''s natural-law self-presentation is historically founded or a retrospective constitutionalization of privilege.').

omega_variable(
    coordination_extraction_separability,
    'Can the genuine coordination function (checking arbitrary edicts) be separated from the extraction function (protecting magistrate tax exemption), or are they structurally fused such that any reform preserving coordination also preserves extraction?',
    'Counterfactual: if a reform edict expanded taxation to noblesse de robe but preserved registration for non-fiscal edicts (religious, judicial), would parlements still remonstrate? Historical test: 1787-88 Assembly of Notables and Estates General debates — magistrates resisted any fiscal equality even with preserved remonstrance.',
    'If separable, the constraint is a Rope with extractive overlay (removable). If fused, it is a Tangled Rope where the coordination story is inseparable from the extraction — the fundamental laws doctrine itself encodes the privilege.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the constraint''s coordination and extraction components are structurally separable or fused in the fundamental laws doctrine.').

omega_variable(
    suppression_mechanism_historical_vs_institutional,
    'Is the measured suppression (0.65) primarily the crown''s active coercion (lit de justice, exile, Maupeou) or the parlement''s institutional inertia (refusal to register as routine, venal office interest)?',
    'Decompose suppression events: count crown-initiated forced registrations vs. parlement-initiated refusals per decade. If crown-initiated dominates, suppression is crown''s reaction to constraint; if parlement-initiated dominates, suppression is constraint''s active enforcement.',
    'If crown-driven, the constraint''s suppression is lower than measured — the crown chooses confrontation. If parlement-driven, the constraint actively suppresses reform, supporting higher effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_historical_vs_institutional, empirical, 'Attribution of suppression agency between crown and parlement in the remonstrance confrontation dynamic.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(remonstrance_authority__magistrate_reading, 1600, 1789).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(remonstrance_magistrate_tr_t1600, remonstrance_authority__magistrate_reading, theater_ratio, 1600, 0.15).
narrative_ontology:measurement(remonstrance_magistrate_tr_t1648, remonstrance_authority__magistrate_reading, theater_ratio, 1648, 0.2).
narrative_ontology:measurement(remonstrance_magistrate_tr_t1661, remonstrance_authority__magistrate_reading, theater_ratio, 1661, 0.22).
narrative_ontology:measurement(remonstrance_magistrate_tr_t1715, remonstrance_authority__magistrate_reading, theater_ratio, 1715, 0.25).
narrative_ontology:measurement(remonstrance_magistrate_tr_t1750, remonstrance_authority__magistrate_reading, theater_ratio, 1750, 0.28).
narrative_ontology:measurement(remonstrance_magistrate_tr_t1774, remonstrance_authority__magistrate_reading, theater_ratio, 1774, 0.3).
narrative_ontology:measurement(remonstrance_magistrate_tr_t1787, remonstrance_authority__magistrate_reading, theater_ratio, 1787, 0.32).
narrative_ontology:measurement(remonstrance_magistrate_tr_t1789, remonstrance_authority__magistrate_reading, theater_ratio, 1789, 0.3).

% Extraction over time
narrative_ontology:measurement(remonstrance_magistrate_be_t1600, remonstrance_authority__magistrate_reading, base_extractiveness, 1600, 0.45).
narrative_ontology:measurement(remonstrance_magistrate_be_t1648, remonstrance_authority__magistrate_reading, base_extractiveness, 1648, 0.55).
narrative_ontology:measurement(remonstrance_magistrate_be_t1661, remonstrance_authority__magistrate_reading, base_extractiveness, 1661, 0.6).
narrative_ontology:measurement(remonstrance_magistrate_be_t1715, remonstrance_authority__magistrate_reading, base_extractiveness, 1715, 0.65).
narrative_ontology:measurement(remonstrance_magistrate_be_t1750, remonstrance_authority__magistrate_reading, base_extractiveness, 1750, 0.68).
narrative_ontology:measurement(remonstrance_magistrate_be_t1774, remonstrance_authority__magistrate_reading, base_extractiveness, 1774, 0.7).
narrative_ontology:measurement(remonstrance_magistrate_be_t1787, remonstrance_authority__magistrate_reading, base_extractiveness, 1787, 0.72).
narrative_ontology:measurement(remonstrance_magistrate_be_t1789, remonstrance_authority__magistrate_reading, base_extractiveness, 1789, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(remonstrance_magistrate_su_t1600, remonstrance_authority__magistrate_reading, suppression_requirement, 1600, 0.4).
narrative_ontology:measurement(remonstrance_magistrate_su_t1648, remonstrance_authority__magistrate_reading, suppression_requirement, 1648, 0.55).
narrative_ontology:measurement(remonstrance_magistrate_su_t1661, remonstrance_authority__magistrate_reading, suppression_requirement, 1661, 0.6).
narrative_ontology:measurement(remonstrance_magistrate_su_t1715, remonstrance_authority__magistrate_reading, suppression_requirement, 1715, 0.55).
narrative_ontology:measurement(remonstrance_magistrate_su_t1750, remonstrance_authority__magistrate_reading, suppression_requirement, 1750, 0.6).
narrative_ontology:measurement(remonstrance_magistrate_su_t1774, remonstrance_authority__magistrate_reading, suppression_requirement, 1774, 0.65).
narrative_ontology:measurement(remonstrance_magistrate_su_t1787, remonstrance_authority__magistrate_reading, suppression_requirement, 1787, 0.7).
narrative_ontology:measurement(remonstrance_magistrate_su_t1789, remonstrance_authority__magistrate_reading, suppression_requirement, 1789, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(remonstrance_authority__magistrate_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(remonstrance_authority__magistrate_reading, 0.12).
narrative_ontology:affects_constraint(remonstrance_authority__magistrate_reading, remonstrance_authority__crown_reading).
narrative_ontology:affects_constraint(remonstrance_authority__magistrate_reading, fiscal_reform_blockage__ancien_regime).
narrative_ontology:affects_constraint(remonstrance_authority__magistrate_reading, venal_office_system__france).

% DUAL FORMULATION NOTE:
% This magistrate_reading and the crown_reading form a constraint family decomposing the kernel 'remonstrance_authority'. The magistrate_reading claims Mountain with high ε for fiscal edicts; the crown_reading claims Snare with low ε. They differ on beneficiary/victim assignment and the naturalness of the fundamental laws doctrine. Linked via affects_constraints for contamination propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(remonstrance_authority__magistrate_reading, institutional, 0.85).
constraint_indexing:directionality_override(remonstrance_authority__magistrate_reading, powerful, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
