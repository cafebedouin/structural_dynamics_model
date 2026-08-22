% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_arms_right__individual_right_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: second_amendment_arms_right__individual_right_reading
 *   human_readable: Second Amendment — Individual Pre-Political Right Reading
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the individual-right reading of the Second
 *   Amendment kernel: the claim that 'the right of the people to keep and
 *   bear arms' names a pre-political liberty held by individuals, enforceable
 *   against government restriction independent of organized militia
 *   membership. This is the reading substantially adopted by the Supreme
 *   Court in District of Columbia v. Heller (2008) and extended in McDonald
 *   v. Chicago (2010) and New York State Rifle & Pistol Association v. Bruen
 *   (2022). Under this reading, individual gun owners, the firearms industry,
 *   and gun-rights organizations become structural beneficiaries of an
 *   enforceable constitutional floor, while municipal and state regulatory
 *   authority becomes the constrained party whose discretion over firearm
 *   possession is narrowed. This is ONE of three readings of the same kernel
 *   text; the collective-right reading (militia-centered) and the
 *   civic-republican reading (armed citizenship as prerequisite for
 *   self-governance) are separate constraint stories with their own ε and
 *   stakeholder sets — this file does not average across them or describe
 *   their contest internally.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__individual_right_reading, 0.42).
domain_priors:suppression_score(second_amendment_arms_right__individual_right_reading, 0.35).
domain_priors:theater_ratio(second_amendment_arms_right__individual_right_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__individual_right_reading, rope).
narrative_ontology:human_readable(second_amendment_arms_right__individual_right_reading, "Second Amendment — Individual Pre-Political Right Reading").
narrative_ontology:topic_domain(second_amendment_arms_right__individual_right_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(second_amendment_arms_right__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__individual_right_reading, 'dd707cfe-34ed-43e2-9e91-197e64451b19').
narrative_ontology:cs_kernel_codification('dd707cfe-34ed-43e2-9e91-197e64451b19', fixed_text).
narrative_ontology:cs_authority_grounding('dd707cfe-34ed-43e2-9e91-197e64451b19', lineage).
narrative_ontology:cs_interpretation_layer_present('dd707cfe-34ed-43e2-9e91-197e64451b19').
narrative_ontology:cs_reading_relation('dd707cfe-34ed-43e2-9e91-197e64451b19', second_amendment_arms_right__collective_right_reading, forecloses).
narrative_ontology:cs_reading_relation('dd707cfe-34ed-43e2-9e91-197e64451b19', second_amendment_arms_right__civic_republican_reading, coexists_with).
narrative_ontology:cs_axiom('dd707cfe-34ed-43e2-9e91-197e64451b19', foundational, arms_right_preexists_and_is_individual).
narrative_ontology:cs_axiom_status(arms_right_preexists_and_is_individual, holdable).
narrative_ontology:cs_axiom_grounding('dd707cfe-34ed-43e2-9e91-197e64451b19', arms_right_preexists_and_is_individual, deontological).
narrative_ontology:cs_axiom('dd707cfe-34ed-43e2-9e91-197e64451b19', foundational, militia_clause_is_prefatory_not_operative).
narrative_ontology:cs_axiom_status(militia_clause_is_prefatory_not_operative, holdable).
narrative_ontology:cs_axiom_grounding('dd707cfe-34ed-43e2-9e91-197e64451b19', militia_clause_is_prefatory_not_operative, conventional).
narrative_ontology:cs_reference_frame('dd707cfe-34ed-43e2-9e91-197e64451b19', founding_era_natural_rights_understanding).
narrative_ontology:cs_drift_state('dd707cfe-34ed-43e2-9e91-197e64451b19', post_heller_doctrinal_consolidation, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('dd707cfe-34ed-43e2-9e91-197e64451b19', '').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__individual_right_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__individual_right_reading, individual_gun_owners).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__individual_right_reading, firearms_industry).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__individual_right_reading, gun_rights_organizations).
narrative_ontology:constraint_victim(second_amendment_arms_right__individual_right_reading, gun_violence_prevention_advocates).
narrative_ontology:constraint_victim(second_amendment_arms_right__individual_right_reading, municipal_regulators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(second_amendment_arms_right__individual_right_reading, federal_government).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__individual_right_reading, natural_rights_pre_existing_government_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__individual_right_reading, individual_self_defense_primacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold an enforceable constitutional claim to keep and bear arms for self-defense, hunting, and other lawful purposes, independent of militia service. Can invoke this right against most federal, state, and local restrictions through litigation. Their exit option is largely moot — the right itself is the thing they hold, not something they need to escape.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, individual_gun_owners, beneficiary,
    moderate, biographical, mobile, national).

% Manufacturers and retailers benefit directly from a constitutional floor that constrains the regulatory reach of legislatures at every level, stabilizing a large consumer market and limiting product-line restrictions (e.g., on modern sporting rifles or magazine capacity). Can relocate manufacturing across state lines to exploit favorable regulatory climates.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, firearms_industry, beneficiary,
    organized, generational, arbitrage, national).

% Litigate strategically to expand and entrench the individual-right doctrine, select test cases, fund amicus efforts, and shape which regulations reach appellate review. Set the doctrinal agenda by choosing which restrictions to challenge and building the case law that other seats must live within.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, gun_rights_organizations, agenda_setter,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_arms_right__individual_right_reading, gun_rights_organizations, beneficiary).

% Cities and states seeking to regulate carry permits, assault-style weapons, or high-capacity magazines must design regulations to survive strict or intermediate scrutiny under the individual-right framework, and often see local ordinances struck down. Cannot exit the constitutional structure; can only try to draft within its narrowing bounds.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, municipal_regulators, payer,
    institutional, biographical, constrained, regional).

% Advocate for restrictions (waiting periods, red-flag laws, assault weapons bans) that must now be litigated against a doctrinal presumption favoring individual ownership. Bear the cost of the doctrine in the form of policy options foreclosed or subjected to heightened judicial scrutiny; cannot exit the constitutional order, only work within or around it.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, gun_violence_prevention_advocates, payer,
    organized, biographical, constrained, national).

% Retains authority to regulate firearms (background checks, interstate commerce, certain weapon classes) but must draft and defend regulation within the individual-right frame, which functions as a constraint on its own legislative and enforcement discretion.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, federal_government, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_arms_right__individual_right_reading, federal_government, agenda_setter).

% Analyze originalist, textualist, and historical evidence bearing on the meaning of 'the right of the people' and 'bear arms' at ratification; produce competing historical accounts that inform, but do not resolve, the doctrinal contest between readings.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable, judicially enforceable baseline entitlement that individuals can rely on for self-defense and lawful firearm ownership without needing to justify possession by reference to militia service, giving a predictable floor beneath which legislatures cannot regulate.
% TRANSFER_FUNCTION: Moves regulatory discretion away from federal, state, and municipal legislatures and toward individual rights-holders and the courts that adjudicate the doctrine's boundaries; shifts the burden of justification onto governments seeking to restrict firearm possession rather than onto individuals seeking to possess them.
% ABSENT_VOICES: Communities disproportionately affected by gun violence, and legislators who represent them, participate in the political process but face a doctrinal presumption weighted against certain regulatory tools before debate even begins; their preferred remedies are litigated against a constitutional floor set independently of local conditions.
% DISAPPEARANCE_RATIONALE: If the individual-right reading were abandoned tomorrow in favor of a collective-right framework, municipal and state legislatures would regain far greater latitude to enact categorical bans, licensing regimes, and possession restrictions without triggering heightened constitutional scrutiny; the firearms industry and gun rights organizations would lose their primary structural leverage in litigation, and gun control advocates would gain significant policy room.
% FOUNDING_PROBLEM: The right was invoked historically to secure a check against disarmament by a potentially tyrannical government and to guarantee individuals means of self-defense, reflecting Founding-era anxieties about standing armies and centralized power over an unarmed populace.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the ratification debates and colonial militia practice are divided: some corroborate a broadly individual self-defense understanding predating the Constitution (cited approvingly in Heller), while other historians and legal scholars outside the gun-rights advocacy network argue the historical record centers militia utility and find the individual-right reading to be a late-20th-century doctrinal innovation rather than a recovered original meaning.
narrative_ontology:disappearance_verdict(second_amendment_arms_right__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_arms_right__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__individual_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_arms_right__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_arms_right__individual_right_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__individual_right_reading_tests).
:- end_tests(second_amendment_arms_right__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (ε=0.42) reflects moderate-to-substantial cost imposed on regulatory authorities and on the policy space available to gun-violence-prevention advocates, rising over the temporal series as the doctrine hardened from a background historical claim into an actively litigated, judicially enforced standard after 2008. Suppression (0.35) is lower than extraction because the mechanism operates primarily through litigation and judicial review rather than direct coercive enforcement — though the suppression_requirement series shows this climbing sharply post-Heller/Bruen as courts began actively striking down a widening range of state and local statutes, indicating the doctrine now requires active judicial enforcement to maintain its scope. Theater ratio remains low (0.2) because the coordination function (a stable possession entitlement) and the extraction function (narrowing legislative discretion) are both genuinely operative, not performative. Accessibility collapse (0.4) is moderate: legislatures retain some regulatory tools (background checks, felon-in-possession bans, commercial sale regulation) that survive scrutiny, so alternatives have not fully collapsed. Resistance (0.75) is high and organized, reflecting the sustained, well-resourced political and legal opposition from gun-violence-prevention advocates and many municipal governments.
 *
 * PERSPECTIVAL GAP:
 *   From the individual gun owner's seat, this operates as a rope: a stable, low-suppression entitlement everyone benefits from equally regardless of political power. From the municipal regulator's seat, the same doctrine operates as a substantial and increasing constraint on democratically enacted policy — closer to a tangled rope, since a genuine coordination function (predictable possession rights) coexists with asymmetric extraction (regulatory discretion transferred away from the governments answerable to gun-violence-affected communities). The engine should compute these divergently from the same structural data; the claimed_type of rope reflects the beneficiary-seat framing, and the metrics are authored to let the divergence surface rather than to pre-resolve it.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual gun owners and the firearms industry sit near the beneficiary end: the doctrine directly expands their protected possession space and constrains the actors who would restrict it. Gun rights organizations occupy a dual agenda-setter/beneficiary position — they actively litigate to expand the doctrine's reach (agenda-setting) while also benefiting from the resulting legal landscape. Municipal regulators and gun-violence-prevention advocates sit toward the target end: their policy tools are narrowed and their exit from the constitutional order is not available — they can only draft within the doctrine's shrinking bounds or seek to overturn it through future litigation or constitutional change.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem inquiry (R5) surfaces genuine contest rather than settling it: the historical anxiety about disarmament by a tyrannical central government is largely dormant as a live practical concern in most Second Amendment litigation today (there is no standing federal army threatening disarmament), yet the doctrine persists and has hardened rather than atrophied — this is NOT mandatrophy in the classic sense (a dead mandate propped up by inertia) because the doctrine has migrated to serve a different but genuinely live function: individual self-defense against ordinary crime, a problem that remains empirically present. This is a case where the founding problem shifted rather than died, which is why founding_problem_status is authored as contested rather than dead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    originalist_historical_record_contest,
    'Does the historical record at ratification (1791) support an individual self-defense understanding of ''keep and bear arms'' independent of militia context, or does it center collective militia utility as the operative meaning?',
    'Comprehensive historical linguistics analysis of period usage of ''bear arms'' across legal, military, and civilian sources; review of state constitutional analogues and ratification debate records; adjudicated definitively only by consensus among historians outside constitutional litigation incentives, which does not currently exist.',
    'If the historical record decisively supports the militia-centered reading, the individual-right reading''s foundational axiom (pre-political individual liberty) would be substantially undermined as an originalist claim, though it could persist as a living-constitutionalism or common-law-rights argument on different grounds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalist_historical_record_contest, empirical, 'Contested historical basis for the individual-right reading versus the collective-right and civic-republican readings.').

omega_variable(
    kernel_reading_selection_stakes,
    'Which of the three declared kernel readings (individual, collective, civic-republican) the Supreme Court adopts is not fixed by the constitutional text alone — it is a live doctrinal choice with enormous downstream regulatory consequences. Is the current individual-right doctrinal dominance a stable equilibrium or vulnerable to future reversal?',
    'Track stare decisis strength of Heller/McDonald/Bruen line versus historical precedent of Second Amendment doctrinal instability (the individual-right reading was a minority academic position before the 1970s); monitor future Court composition and case law trajectory.',
    'A reversal toward the collective-right reading would eliminate this constraint''s beneficiary set''s structural advantage and shift ε for municipal regulators and gun-violence-prevention advocates toward near-zero; a hardening of the individual-right reading (e.g., through explicit rejection of the collective-right and civic-republican alternatives) would further concentrate extraction on regulatory authority.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_stakes, conceptual, 'Whether the individual-right reading''s current dominance represents a stable resolution of the kernel contest or a contingent, reversible doctrinal moment.').

omega_variable(
    founding_problem_migration_ambiguity,
    'Has the founding problem (protection against government disarmament/tyranny) genuinely migrated to a live self-defense-against-crime function, or is the self-defense justification a post-hoc rationalization sustaining a doctrine whose original animating concern is functionally dead?',
    'Survey doctrinal and rhetorical emphasis across Heller-line opinions and gun-rights advocacy literature over time; compare stated justifications in 1791-era sources versus post-2008 judicial and advocacy discourse.',
    'If the self-defense justification is primarily rationalization for a doctrine whose real function is now market protection for the firearms industry and political mobilization for gun-rights organizations, this shifts the classification analysis toward tangled_rope or snare rather than rope, since the coordination story would be functioning as cover for concentrated benefit capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_migration_ambiguity, conceptual, 'Whether the founding problem has genuinely migrated to a live function or is being used as post-hoc justification for the doctrine''s persistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__individual_right_reading, 1791, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1791, second_amendment_arms_right__individual_right_reading, theater_ratio, 1791, 0.1).
narrative_ontology:measurement_basis(seco_tr_t1791, observed).
narrative_ontology:measurement(seco_tr_t1900, second_amendment_arms_right__individual_right_reading, theater_ratio, 1900, 0.1).
narrative_ontology:measurement_basis(seco_tr_t1900, observed).
narrative_ontology:measurement(seco_tr_t1939, second_amendment_arms_right__individual_right_reading, theater_ratio, 1939, 0.15).
narrative_ontology:measurement_basis(seco_tr_t1939, observed).
narrative_ontology:measurement(seco_tr_t1980, second_amendment_arms_right__individual_right_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement_basis(seco_tr_t1980, observed).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_arms_right__individual_right_reading, theater_ratio, 2008, 0.18).
narrative_ontology:measurement_basis(seco_tr_t2008, observed).
narrative_ontology:measurement(seco_tr_t2024, second_amendment_arms_right__individual_right_reading, theater_ratio, 2024, 0.2).
narrative_ontology:measurement_basis(seco_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_arms_right__individual_right_reading, base_extractiveness, 1791, 0.15).
narrative_ontology:measurement_basis(seco_be_t1791, observed).
narrative_ontology:measurement(seco_be_t1900, second_amendment_arms_right__individual_right_reading, base_extractiveness, 1900, 0.2).
narrative_ontology:measurement_basis(seco_be_t1900, observed).
narrative_ontology:measurement(seco_be_t1939, second_amendment_arms_right__individual_right_reading, base_extractiveness, 1939, 0.28).
narrative_ontology:measurement_basis(seco_be_t1939, observed).
narrative_ontology:measurement(seco_be_t1980, second_amendment_arms_right__individual_right_reading, base_extractiveness, 1980, 0.3).
narrative_ontology:measurement_basis(seco_be_t1980, observed).
narrative_ontology:measurement(seco_be_t2008, second_amendment_arms_right__individual_right_reading, base_extractiveness, 2008, 0.38).
narrative_ontology:measurement_basis(seco_be_t2008, observed).
narrative_ontology:measurement(seco_be_t2024, second_amendment_arms_right__individual_right_reading, base_extractiveness, 2024, 0.42).
narrative_ontology:measurement_basis(seco_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1791, second_amendment_arms_right__individual_right_reading, suppression_requirement, 1791, 0.1).
narrative_ontology:measurement_basis(seco_su_t1791, observed).
narrative_ontology:measurement(seco_su_t1900, second_amendment_arms_right__individual_right_reading, suppression_requirement, 1900, 0.12).
narrative_ontology:measurement_basis(seco_su_t1900, observed).
narrative_ontology:measurement(seco_su_t1939, second_amendment_arms_right__individual_right_reading, suppression_requirement, 1939, 0.15).
narrative_ontology:measurement_basis(seco_su_t1939, observed).
narrative_ontology:measurement(seco_su_t1980, second_amendment_arms_right__individual_right_reading, suppression_requirement, 1980, 0.18).
narrative_ontology:measurement_basis(seco_su_t1980, observed).
narrative_ontology:measurement(seco_su_t2008, second_amendment_arms_right__individual_right_reading, suppression_requirement, 2008, 0.28).
narrative_ontology:measurement_basis(seco_su_t2008, observed).
narrative_ontology:measurement(seco_su_t2024, second_amendment_arms_right__individual_right_reading, suppression_requirement, 2024, 0.35).
narrative_ontology:measurement_basis(seco_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(second_amendment_arms_right__individual_right_reading, collective_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__individual_right_reading, civic_republican_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the second_amendment_arms_right kernel, each authored as a separate ε-invariant constraint story per the ε-invariance principle. individual_right_reading places individual gun owners and the firearms industry as structural beneficiaries and regulatory authorities as constrained payers, with high ε on prohibition/categorical-ban measures. collective_right_reading would place state militia authority as the protected interest and treat individual ownership claims outside militia context as unprotected, inverting the beneficiary/victim structure for civilian possession claims. civic_republican_reading occupies an intermediate position, treating armed citizenship as instrumental to republican self-governance rather than either purely individual or purely state-centered, and would show different ε for measures that burden civic-militia participation versus pure self-defense possession. The three files do not share ε, beneficiaries, or victims; they are linked here to preserve the kernel-family structure for contamination and coupling analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
