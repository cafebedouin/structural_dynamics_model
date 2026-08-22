% ============================================================================
% CONSTRAINT STORY: honor_settlement_legitimacy__drop_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_settlement_legitimacy__drop_reading, []).

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
 *   constraint_id: honor_settlement_legitimacy__drop_reading
 *   human_readable: Honor-Settlement Legitimacy — Residual Dueling Persistence (Drop Reading)
 *   domain: historical_sociology/legal_history/cultural_anthropology
 *
 * SUMMARY:
 *   This story instantiates the 'drop reading' of the
 *   honor-settlement-legitimacy kernel: the claim that dueling did not become
 *   cognitively unthinkable (contraction_reading) nor decline through a fully
 *   overdetermined multi-mechanism process with a contraction edge
 *   (composite_reading), but rather persisted as a live, if fringe, normative
 *   option within specific residual honor-culture pockets — rural gentry
 *   networks, certain military subcultures, some immigrant enclaves
 *   preserving inherited codes. The constraint under contest is the residual
 *   legitimacy claim itself: that within these niches, answering a challenge
 *   remains a socially adjudicable path to restoring honor, rather than an
 *   unthinkable relic. ε is authored for this standing residual arrangement,
 *   as this reading sees it — not for the vanished mainstream practice and
 *   not for either sibling reading's alternative account of the decline.
 *
 * KEY AGENTS:
 *   - residual_honor_culture_elites: Primary beneficiary (moderate/constrained) — retains status through continued code legitimacy
 *   - dueling_code_custodians: Agenda-setter (moderate/identity_locked) — administers the procedural apparatus, has no role outside it
 *   - dueling_participants_and_families: Primary target (powerless/trapped) — bears the mortal and financial risk
 *   - peripheral_community_members_pressured_to_defer: Secondary target (powerless/constrained) — pays diffuse deference costs
 *   - state_legal_authorities: Excluded institutional actor (institutional/analytical) — formally settled the matter but structurally absent from the niche
 *   - mainstream_society_observers: Analytical observer (organized/analytical) — sees the practice as anachronistic curiosity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__drop_reading, 0.58).
domain_priors:suppression_score(honor_settlement_legitimacy__drop_reading, 0.42).
domain_priors:theater_ratio(honor_settlement_legitimacy__drop_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__drop_reading, piton).
narrative_ontology:human_readable(honor_settlement_legitimacy__drop_reading, "Honor-Settlement Legitimacy — Residual Dueling Persistence (Drop Reading)").
narrative_ontology:topic_domain(honor_settlement_legitimacy__drop_reading, "historical_sociology/legal_history/cultural_anthropology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__drop_reading, '5c3006cb-c01f-4757-9a4f-08a06b153e1d').
narrative_ontology:cs_kernel_codification('5c3006cb-c01f-4757-9a4f-08a06b153e1d', distributed).
narrative_ontology:cs_authority_grounding('5c3006cb-c01f-4757-9a4f-08a06b153e1d', practice).
narrative_ontology:cs_interpretation_layer_present('5c3006cb-c01f-4757-9a4f-08a06b153e1d').
narrative_ontology:cs_reading_relation('5c3006cb-c01f-4757-9a4f-08a06b153e1d', honor_settlement_legitimacy__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('5c3006cb-c01f-4757-9a4f-08a06b153e1d', honor_settlement_legitimacy__composite_reading, influences).
narrative_ontology:cs_axiom('5c3006cb-c01f-4757-9a4f-08a06b153e1d', foundational, honor_code_retains_local_normative_force).
narrative_ontology:cs_axiom_status(honor_code_retains_local_normative_force, holdable).
narrative_ontology:cs_axiom_grounding('5c3006cb-c01f-4757-9a4f-08a06b153e1d', honor_code_retains_local_normative_force, conventional).
narrative_ontology:cs_axiom('5c3006cb-c01f-4757-9a4f-08a06b153e1d', secondary, niche_persistence_falsifies_total_extinction_claim).
narrative_ontology:cs_axiom_status(niche_persistence_falsifies_total_extinction_claim, holdable).
narrative_ontology:cs_axiom_grounding('5c3006cb-c01f-4757-9a4f-08a06b153e1d', niche_persistence_falsifies_total_extinction_claim, empirically_contingent).
narrative_ontology:cs_reference_frame('5c3006cb-c01f-4757-9a4f-08a06b153e1d', gentry_code_duello_ascendancy).
narrative_ontology:cs_drift_state('5c3006cb-c01f-4757-9a4f-08a06b153e1d', contemporary_residual_niche_period, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5c3006cb-c01f-4757-9a4f-08a06b153e1d', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__drop_reading, residual_honor_culture_elites).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__drop_reading, dueling_code_custodians).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__drop_reading, dueling_participants_and_families).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__drop_reading, peripheral_community_members_pressured_to_defer).
narrative_ontology:constraint_vindicates(honor_settlement_legitimacy__drop_reading, personal_honor_as_adjudicable_property).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Small networks of landed gentry, military officers, and professional-class men in specific regions (rural south, military academies, certain immigrant enclaves) who continue to treat personal insult as adjudicable through the code duello. They retain social status by being seen as men who would still answer a challenge, even as the wider legal and cultural apparatus has moved on. Their standing depends on the practice's continued, if narrow, legitimacy in their circle.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, residual_honor_culture_elites, beneficiary,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(honor_settlement_legitimacy__drop_reading, residual_honor_culture_elites, agenda_setter).

% Seconds, code-of-honor authors, and informal arbiters who maintain the procedural apparatus (challenge protocols, satisfaction terms, adjudication of insult severity). They administer a shrinking but real institution and derive their entire social function from its continuation; they have no comparable role if the practice fully disappears.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, dueling_code_custodians, agenda_setter,
    moderate, generational, identity_locked, local).

% Individual men (and their dependents) who are drawn into a specific challenge because refusing would mean social death within their remaining honor-culture niche. They bear the mortal, legal, and financial risk of the duel itself, and widows/orphans bear the aftermath. Exit is nearly impossible once challenged without total exile from their social world.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, dueling_participants_and_families, payer,
    powerless, biographical, trapped, local).

% Neighbors, employees, and social subordinates within these niches who must publicly defer to the honor logic even if they reject it privately, because open contempt for the code marks them as targets or outcasts. They pay a diffuse cost in constrained speech and behavior without ever dueling themselves.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, peripheral_community_members_pressured_to_defer, payer,
    powerless, biographical, constrained, local).

% Courts and legislatures that formally criminalized dueling generations earlier and consider the matter settled. They are structurally absent from the niches where the practice persists — enforcement is sporadic, jurisdictionally thin, or socially unwelcome, so their formal position has little bearing on the residual practice's actual operation.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, state_legal_authorities, excluded,
    institutional, generational, analytical, national).

% The broader public and press who regard dueling as an anachronism, occasionally reporting on it as a curiosity or scandal when a case surfaces. They exert reputational pressure on the practice's adherents but have no direct administrative role in suppressing it.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, mainstream_society_observers, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_settlement_legitimacy__drop_reading, residual_honor_culture_elites).
narrative_ontology:fixing_cost_class(honor_settlement_legitimacy__drop_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Within the residual niche, the duel still solves a genuine local coordination problem: it provides a bounded, rule-governed procedure for resolving otherwise unresolvable claims of insult to reputation, substituting for open-ended feuding or unregulated violence.
% TRANSFER_FUNCTION: Moves physical risk, and sometimes life, from the abstract claim of 'honor' onto the bodies of the specific men who duel, while transferring social status and continued relevance to the elites and custodians who keep the code alive; deference costs are extracted diffusely from bystanders who must perform respect for a logic they may not endorse.
% ABSENT_VOICES: Widows, children, and dependents of duelists have essentially no voice in whether a challenge proceeds; state legal authorities who have already ruled the practice illegal are excluded from the niche's actual decision-making; women and social subordinates whose deference is compelled are not parties to the code's authorship.
% DISAPPEARANCE_RATIONALE: Within the residual niches themselves, the practice's disappearance would genuinely rearrange local status hierarchies — the elites and custodians who derive standing from it would lose a defining marker of identity. From the standpoint of the wider society, the practice is already so marginal that its full disappearance would be imperceptible; mainstream institutions already treat it as extinct in all but a few holdout communities.
% FOUNDING_PROBLEM: The original code duello was built to provide a formalized, rule-bound alternative to escalating blood feuds among the gentry — an alternative to unregulated retaliatory violence over perceived insult.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and criminologists studying the decline of dueling attest that the underlying disorder problem (unregulated feud violence) was resolved by the rise of state monopoly on legal violence and formal defamation law generations before the residual practice persisted; this corroboration comes from outside the honor-culture niches themselves — the custodians and elites who maintain the practice today largely do not claim the original disorder problem is still live, but instead defend the practice on grounds of tradition and identity, which is a different and newer justification than the founding one.
narrative_ontology:disappearance_verdict(honor_settlement_legitimacy__drop_reading, contested).
narrative_ontology:founding_problem_status(honor_settlement_legitimacy__drop_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__drop_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_settlement_legitimacy__drop_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_settlement_legitimacy__drop_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_settlement_legitimacy__drop_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_settlement_legitimacy__drop_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_settlement_legitimacy__drop_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects a moderate-to-substantial toll: within the niche, the practice extracts mortal risk from a narrow set of participants and diffuse deference costs from a wider set of bystanders, but this is bounded by the practice's small and shrinking scope — it does not touch the vast majority of society at all, which caps the effective extraction relative to a mainstream institution. Suppression (0.42) is moderate: adherence is sustained less by external coercion (the state has criminalized it and does not enforce participation) and more by internal social sanction within the niche — refusing a challenge risks ostracism, not legal punishment, which is a softer but real suppressive force. Theater ratio (0.28) is low-to-moderate: the practice retains a genuine (if archaic) coordination function for its remaining adherents rather than being purely performative, though the ratio's rising trajectory reflects the practice becoming increasingly self-conscious and ritualized as a marker of identity rather than a live dispute-resolution necessity. Accessibility collapse (0.35) is low — alternatives to dueling (courts, informal apology, social withdrawal) are widely available and used by the vast majority; the honor-culture niche is a self-selected holdout, not a closed trap for the whole society. Resistance (0.55) is moderate-to-high: mainstream legal and cultural institutions actively resist the practice's legitimacy claims, and even within niches younger generations increasingly push back against its demands.
 *
 * DIRECTIONALITY LOGIC:
 *   Residual honor-culture elites and code custodians sit near the beneficiary end: the practice's continuation is what sustains their distinct social standing, and they bear none of the participants' physical risk. Dueling participants and their families sit at the full-target end: trapped exit options within the niche's own social logic, they absorb the mortal and financial cost directly. Peripheral community members occupy an intermediate position — constrained rather than trapped, since they can often avoid direct involvement, but they still pay a diffuse deference tax. State legal authorities are excluded rather than positioned on the beneficiary/victim axis at all — the practice operates largely outside their effective jurisdiction in these niches, which is why suppression trends downward over the interval (state enforcement capacity/interest erodes further as the practice becomes a legal curiosity rather than a live problem).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — unregulated feud violence among the gentry — is genuinely dead (formal law and state violence monopoly resolved it generations ago), yet the arrangement persists in specific niches under a substituted justification: tradition and identity rather than dispute-resolution necessity. This is a classic mandatrophy signature, but the classification must resist two errors symmetrically: treating the residual practice as still solving its original problem (it is not — hence founding_problem_status: dead) OR treating its complete absence of function as license to call the phenomenon nonexistent (the sibling contraction_reading's error, from this reading's perspective). The drop reading insists the practice is real, bounded, and continuing to extract from a small population under a piton-like inertial logic — administered by custodians with no alternative function, tolerated by mainstream institutions because its scope is too narrow to justify further enforcement cost.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    residual_practice_scale_ambiguity,
    'How large and how socially consequential are the residual honor-culture niches in which dueling-adjacent practices persist — large enough to constitute a genuine ongoing constraint, or so vanishingly rare that ''persistence'' overstates a handful of anecdotal cases?',
    'Systematic historical/sociological census of documented duels or duel-adjacent honor confrontations in the post-mainstream-decline period, cross-referenced against population size of self-identified honor-culture communities.',
    'If residual scale is genuinely negligible, this reading collapses toward the contraction_reading''s account (the practice is functionally extinct, and ''persistence'' is a rounding error); if residual scale is substantial and self-sustaining, the drop_reading''s claim of live niche legitimacy is strongly corroborated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_practice_scale_ambiguity, empirical, 'Whether the residual dueling niche is large enough to be a genuine ongoing constraint or negligible noise.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly do the three sibling readings of the honor_settlement_legitimacy kernel disagree — is it about the causal mechanism of decline (composite vs. contraction), or about whether the decline is total vs. partial (drop vs. the other two)?',
    'Comparative structural analysis across all three constraint stories: composite_reading and contraction_reading may actually agree that decline is total and differ only on mechanism, while drop_reading''s distinguishing claim is that decline is incomplete. Documenting this in each story''s commentary and cross-checking beneficiary/victim structures (drop_reading uniquely names live beneficiaries/victims; the others may not) would confirm the disagreement locus.',
    'If disagreement is purely about completeness of decline, drop_reading''s ε and stakeholder structure is the load-bearing distinguishing feature of the family, and the sibling readings should show ε near zero or omit beneficiaries/victims for the historical practice, since by their account no one currently benefits or suffers from a defunct norm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Locating the precise structural axis of disagreement among the three kernel readings.').

omega_variable(
    custodian_function_genuineness,
    'Do dueling-code custodians provide a genuine (if archaic) coordination service to niche adherents, or has their function become purely ceremonial/performative even within the niche?',
    'Ethnographic or historical case study of actual challenge-and-resolution episodes in the residual period: does the custodian''s mediation actually reduce violence/escalation relative to no mediation, or is the entire apparatus theatrical scaffolding around outcomes already predetermined by social pressure?',
    'If genuinely functional, supports the piton-with-residual-coordination-value classification; if purely ceremonial, the theater_ratio should be revised sharply upward and the classification would drift toward snare (pure extraction dressed as tradition) rather than piton (inertial residue of a once-functional coordination mechanism).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(custodian_function_genuineness, empirical, 'Whether the custodian apparatus still performs real dispute-mediation work or has become fully theatrical.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__drop_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_settlement_legitimacy__drop_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(hono_tr_t20, honor_settlement_legitimacy__drop_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement(hono_tr_t40, honor_settlement_legitimacy__drop_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement(hono_tr_t60, honor_settlement_legitimacy__drop_reading, theater_ratio, 60, 0.22).
narrative_ontology:measurement(hono_tr_t80, honor_settlement_legitimacy__drop_reading, theater_ratio, 80, 0.25).
narrative_ontology:measurement(hono_tr_t100, honor_settlement_legitimacy__drop_reading, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_settlement_legitimacy__drop_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hono_be_t20, honor_settlement_legitimacy__drop_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(hono_be_t40, honor_settlement_legitimacy__drop_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement(hono_be_t60, honor_settlement_legitimacy__drop_reading, base_extractiveness, 60, 0.53).
narrative_ontology:measurement(hono_be_t80, honor_settlement_legitimacy__drop_reading, base_extractiveness, 80, 0.56).
narrative_ontology:measurement(hono_be_t100, honor_settlement_legitimacy__drop_reading, base_extractiveness, 100, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_settlement_legitimacy__drop_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(hono_su_t20, honor_settlement_legitimacy__drop_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(hono_su_t40, honor_settlement_legitimacy__drop_reading, suppression_requirement, 40, 0.46).
narrative_ontology:measurement(hono_su_t60, honor_settlement_legitimacy__drop_reading, suppression_requirement, 60, 0.44).
narrative_ontology:measurement(hono_su_t80, honor_settlement_legitimacy__drop_reading, suppression_requirement, 80, 0.43).
narrative_ontology:measurement(hono_su_t100, honor_settlement_legitimacy__drop_reading, suppression_requirement, 100, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_settlement_legitimacy__drop_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_settlement_legitimacy__drop_reading, 0.1).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy__contraction_reading).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% This story is the 'drop_reading' member of the honor_settlement_legitimacy kernel family, alongside contraction_reading (cognitive unthinkability account) and composite_reading (overdetermined multi-mechanism account with contraction edge). All three share the same underlying historical kernel — the documented decline of dueling as a normative practice — but instantiate structurally distinct constraints with different ε values, different beneficiary/victim structures, and different classifications, because they disagree about whether the decline was total (contraction, composite) or partial (drop). Per the ε-invariance principle, these are linked via network edges rather than merged into one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
